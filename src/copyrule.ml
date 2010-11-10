(*******************************************************************************
 * Copyright (c) 2010 AT&T.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Trevor Jim and Yitzhak Mandelbaum
 *******************************************************************************)

open Gul
open Attr

let union x y = PSet.fold PSet.add x y
let intersect x y = PSet.filter (fun a -> PSet.mem a x) y
let difference x y = PSet.filter (fun a -> not(PSet.mem a y)) x
let list_of_set set = PSet.fold (fun x l -> x::l) set []
let set_of_list l = List.fold_right PSet.add l PSet.empty
let sort = List.sort (fun (x,_) (y,_) -> compare x y)

(* Try 3 *)
let mk_needs gr =
  let attributes_and_types =
    list_of_set       (* remove duplicates *)
      (set_of_list
         (List.concat (* all attributes and types in a list with duplicates *)
            (List.map
               (function
                   RuleDef(_,_,a) -> a.input_attributes
                 | _ -> [])
               gr.ds))) in
  let all_nonterminals =
    List.concat
      (List.map
         (function
             RuleDef(n,_,_) -> [n]
           | _ -> [])
         gr.ds) in
  let all_attributes = (* actually, all input attributes *)
    set_of_list(List.map fst attributes_and_types) in
  (if List.length attributes_and_types > PSet.cardinal all_attributes then
    Printf.eprintf "Warning: Copyrule says that some attribute has multiple types\n%!");

  (* build a graph, if a (nonterminal,attribute) reaches (attr_target,attribute)
     then the attribute should be a parameter of the nonterminal *)
  let attr_target = "**needs_parameters**" in
  let rec get_depend (g:(string*string) Tgraph.graph) n r =
    match r.r with
    | Symb(x,_,input_attributes,_) ->
        let present_attributes = set_of_list(List.map fst input_attributes) in
        let possibly_missing_attributes = difference all_attributes present_attributes in
        PSet.fold
          (fun a g2 -> Tgraph.add_edge (Tgraph.add_node g2 (x,a)) (n,a) (x,a))
          possibly_missing_attributes
          g
    | Position _
    | Prose _
    | When _ | Action _ | Box _ | Delay _
    | CharRange _
    | Lit _ -> g
    | Minus(r2,r3)
    | Seq(r2,_,_,r3)
    | Alt(r2,r3) ->
        get_depend (get_depend g n r2) n r3
    | Opt(r2)
    | Assign(r2,_,_)
    | Rcount(_,r2)
    | Star(_,r2)
    | Hash(_,r2)
    | Lookahead(_,r2) ->
        get_depend g n r2 in
  let all_nodes =
    List.concat(List.map (fun n -> List.map (fun a -> (n,a)) (list_of_set all_attributes))
                  (attr_target::all_nonterminals)) in
  let g0 =
    List.fold_left
      Tgraph.add_node
      Tgraph.empty
      all_nodes in
  let g1 =
    List.fold_left
      (fun result -> function
          RuleDef(n,r,_) ->
            get_depend result n r
        | _ -> result)
      g0
      gr.ds in
  let g2 =
    List.fold_left
      (fun g ->
        (function
            RuleDef(n,_,a) ->
              let input_attributes = set_of_list(List.map fst a.input_attributes) in
              PSet.fold
                (fun a g2 -> Tgraph.add_edge g2 (n,a) (attr_target,a))
                input_attributes
                g
          | _ -> g))
      g1
      gr.ds in
  (* calculate transitive closure for least fixpoint *)
  let g3 = Tgraph.tc g2 in
  let needs_attributes n =
    PSet.filter
      (fun a -> PSet.mem (attr_target,a) (Tgraph.get_targets g3 (n,a)))
      all_attributes in
  let needs_attributes_and_types n =
    sort(list_of_set
           (PSet.map
              (fun a -> (a,List.assoc a attributes_and_types))
              (needs_attributes n))) in
  needs_attributes_and_types

let add_actuals needs =
  let rec loop r =
    match r.r with
    | Symb(x,e,actuals,l) ->
        let formals = needs x in
        let missing =
          difference formals (set_of_list(List.map fst actuals)) in
        let actuals =
          sort (List.append
                  (List.map (fun a -> (a,a)) (list_of_set(missing)))
                  actuals) in
        r.r <- Symb(x,e,actuals,l);
        (* inductive cases *)
    | Minus(r2,r3)
    | Seq(r2,_,_,r3)
    | Alt(r2,r3) ->
        (loop r2; loop r3)
    | Opt(r2)
    | Assign(r2,_,_)
    | Rcount(_,r2)
    | Star(_,r2)
    | Hash(_,r2) ->
        loop r2
          (* trivial base cases *)
    | Lookahead _
    | Position _
    | When _
    | Action _
    | Box _
    | Delay _
    | Prose _
    | CharRange _
    | Lit _ -> () in
  loop

let transform gr =
  let needs_attributes_and_types = mk_needs gr in
  let tbl = Hashtbl.create 11 in
  List.iter
    (function
        RuleDef(n,_,a) ->
          let attributes_and_types = needs_attributes_and_types n in
          a.input_attributes <- attributes_and_types;
          Hashtbl.add tbl n (set_of_list (List.map fst attributes_and_types))
      | _ -> ())
    gr.ds;
  let needs n =
    try
      Hashtbl.find tbl n
    with Not_found ->
      (Printf.eprintf "Warning: Copyrule can't find %s\n%!" n; PSet.empty)
  in
  List.iter
    (function
        RuleDef(_,r,_) ->
          add_actuals needs r
      | _ -> ())
    gr.ds

(* THIS WAS TRY 2
(* Try 2

Copy rule

When should a missing input attribute be propagated (become an input
attribute of the caller?)

When should a missing output attribute be propagated?

1. For each position in a right-hand side compute the set of
"available" attributes.  In A B, the output attributes of A are
available to B.
2. Every missing input attribute is made explicit.  In A B if B has an
input attribute a, we would rewrite as A B(;a=a).
3. If there is ever a missing input attribute where the attribute is not
"available", we propagate.
4. Sometimes it is not clear whether an attribute is "available".  In
*A where A has an input and output attribute a, on the first iteration
of the loop, a is unavailable, but for the second it is available.
Here for the purposes of the copy rule we should say it is
unavailable.
5. Propagate an output attribute if it is also an input attribute that
is propagated.  (NB we need the type of the output attribute, and we
can get it from the input attribute.)

*)

let transform gr =
  (* Gather mapping from nonterminal -> input_attributes * output_attributes *)
  let tbl = Hashtbl.create 11 in
  List.iter
    (function
        RuleDef(n,r,a) ->
          Hashtbl.add tbl n (a.input_attributes,a.output_attributes)
      | _ -> ())
    gr.ds;
  let outputs n =
    try
      snd(Hashtbl.find tbl n)
    with Not_found ->
      (Printf.eprintf "Internal error: Copyrule cannot find output attributes of %s\n%!" n; [])
  in
  let inputs n =
    try
      fst(Hashtbl.find tbl n)
    with Not_found ->
      (Printf.eprintf "Internal error: Copyrule cannot find input attributes of %s\n%!" n; [])
  in

  let proc r available =
    let missing_inputs = ref PSet.empty in
    let rec loop r available = match r.r with
    | Symb(n,eopt,input_attributes,lopt) ->
        let actuals = sort input_attributes in
        let formals = inputs n in
        (let extra =
          difference (set_of_list(List.map fst actuals)) (set_of_list(List.map fst formals)) in
        PSet.iter (Printf.eprintf "Warning: %s called with extra attribute %s\n%!" n) extra);
        let missing =
          difference (set_of_list(List.map fst formals)) (set_of_list(List.map fst actuals)) in
        let actuals =
          sort (List.append
                  (List.map (fun a -> (a,a)) (list_of_set(missing)))
                  actuals) in
        r.r <- Symb(n,eopt,actuals,lopt);
        let missing_formals = List.filter (fun (var,typ) -> PSet.mem var missing) formals in
        missing_inputs := union !missing_inputs (set_of_list missing_formals);
        let available = union available (set_of_list(List.map fst (outputs n))) in
        available
    | Assign(r2,None,_) -> available
    | Assign(r2,Some x,_) -> PSet.add x available
    | Position _ | Lit(_,_) | CharRange(_,_) | Prose _
    | Action _ | Delay _ -> available
    | When _ | Box _ -> available (* NB When and Box can fail so results may pass on available *)
    | Seq(r2,_,_,r3)
    | Minus(r2,r3) ->
        loop r3 (loop r2 available)
    | Alt(r2,r3)->
        let a2 = loop r2 available in
        let a3 = loop r3 available in
        intersect a2 a3           (* NB intersection not union *)
    | Lookahead (_,r2) | Opt r2 | Rcount(_,r2) | Star(_,r2) | Hash(_,r2) -> loop r2 available
    in
    loop r available,!missing_inputs
  in
  List.iter
    (function
        RuleDef(n,r,a) ->
          let (available,missing_inputs) = proc r (set_of_list(List.map fst a.input_attributes)) in
          (*TODO: error if missing_inputs includes the same variable at different types*)
          (*TODO: verbose warn for each attribute added to inputs*)
          let missing_inputs = list_of_set missing_inputs in
          List.iter
            (fun (var,typ) ->
              Printf.eprintf "Adding input attribute %s:%s to %s\n%!" var typ n)
            missing_inputs;
          a.input_attributes <- sort (List.append a.input_attributes missing_inputs);
          let missing_outputs =
            intersect
              (set_of_list (List.map fst missing_inputs))
              (difference available (set_of_list (List.map fst a.output_attributes))) in
          let missing_outputs = List.filter (fun (var,typ) -> PSet.mem var missing_outputs) missing_inputs in
          List.iter
            (fun (var,typ) ->
              Printf.eprintf "Adding output attribute %s:%s to %s\n%!" var typ n)
            missing_outputs;
          a.output_attributes <- sort (List.append a.output_attributes missing_outputs);
          ()
      | _ -> ())
    gr.ds;
  ()
*)

(* THIS WAS THE FIRST IMPLEMENTATION, COPY RULE FOR PARAMETERS
(*
   Transform a grammar to make attribute/parameter copies explicit.

Current implementation: added a directive to the yakker grammar:

    @implicit-parameters(i:istate)(i)

which declares @(i:istate) to be the implicit formal parameters of
nonterminals, and @(i) to be the implicit actual parameters of their
occurrences on right-hand sides.

This is implemented by side-effecting a global and marking the Attr.t
of subsequent rule definitions.

This has a drawback: it is difficult to print out the @implicit-parameters
directive, if we wanted to print out the grammar before doing the
transformation.  I think the box implementation has the same
drawback.

The implicit parameters can be cancelled with

    @implicit-parameters()()

The transform is implemented by first analysing the grammar to see
what nonterminals "need" parameters.  We say a nonterminal needs
parameters if (1) it has explicit formal parameters, or (2) its RHS
contains an occurrence of nonterminal without explicit actual
parameters that needs parameters.

An example.

    @implicit-parameters(n:int)(n)
    foo@(n:int) = bar@(n+1)

Here foo needs parameters and uses the parameter n on the RHS.  This
can be abbreviated by

    foo@() = bar@(n+1)

and yakker will insert the formal parameters @(n:int).  This of course
requires that foo use "n" as the formal parameter.  You can use a
different variable as long as it is explicitly declared:

    foo@(x:int) = bar@(x+1)

Now consider

    baz = foo@(3)

Here baz does not need parameters, even though its RHS uses a
nonterminal, bar, that needs parameters, because the RHS supplies the
needed parameters explicitly.  Yakker will not transform this rule.

    fib = foo bar baz

Here fib needs parameters because the RHS uses foo and bar without
parameters and they need parameters.  Yakker will transform this to

    fib@(n:int) = foo@(n) bar@(n) baz

Now consider

    fob = foo bar@(n+1) baz

Here fob needs parameters because the RHS uses foo without parameters
and it needs parameters.  Yakker will transform this to

    fob@(n:int) = foo@(n) bar@(n+1) baz

It is perhaps disturbing that the use of the parameter n in
"bar@(n+1)" is "captured" when we add the formal parameter @(n:int).
As a matter of style I think you should use the "@()" form to make
this more explicit:

    fob@() = foo bar@(n+1) baz

In this case you get the same results.  But consider:

    fab = bar@(n+1) baz

This would be an error, our analysis thinks that fab does not need
parameters, because baz does not need parameters, and bar is
explicitly supplied with parameters.  This is really the best we can
do if we want to treat the actual parameters "@(n+1)" opaquely.  You must write

    fab@() = bar@(n+1) baz

or

    fab@(n:int) = bar@(n+1) baz

in this case.

Here's another error:

    fib@(x:int) = foo bar@(x+1) baz

fib is explicitly using a different name for the parameter, but also
uses foo on the RHS without parameters.  Yakker transforms this to the
erroneous

    fib@(x:int) = foo@(n) bar@(x+1) baz

It would be nice if we could issue a warning here, but again, this
would require us to look inside of parameters, e.g., the following is
not an error:

    fib@(n:int) = foo bar@(n+1) baz

We could consider printing a warning when we see explicit parameters
that do not use the form "@()", and we insert actuals; at the moment
we don't do this.

*)

(** return a predicate for the set of symbols that need parameters *)
(*  TODO: almost identical to mk_needs_crawl in crawler.ml, needs refactoring? *)
let mk_needs_parameters gr =
  (* build a graph, if a symbol reaches attr_target then it needs parameters *)
  let attr_target = "**needs_parameters**" in
  let rec get_depend g n r =
    match r.r with
    | Symb(x,None,_,_) -> (* TODO: attributes *)
        Tgraph.add_edge (Tgraph.add_node g x) n x
    | Symb(_,Some _,_,_) -> g (* here we differ from Gul.dependency_graph *) (* TODO: attributes *)
    | Position _
    | Prose _
    | When _ | Action _ | Box _ | Delay _
    | CharRange _
    | Lit _ -> g
    | Minus(r2,r3)
    | Seq(r2,_,_,r3)
    | Alt(r2,r3) ->
        get_depend (get_depend g n r2) n r3
    | Opt(r2)
    | Assign(r2,_,_)
    | Rcount(_,r2)
    | Star(_,r2)
    | Hash(_,r2)
    | Lookahead(_,r2) ->
        get_depend g n r2 in
  let g0 =
    List.fold_left
      (fun result -> function
          RuleDef(n,r, _)->
            get_depend (Tgraph.add_node result n) n r
        | _ -> result)
      Tgraph.empty
      gr.ds in
  let g1 =
    List.fold_left
      (fun g ->
        (function
            RuleDef(n,_,a) ->
              if a.early_params=None then g
              else Tgraph.add_edge g n attr_target
          | _ -> g))
      (Tgraph.add_node g0 attr_target)
      gr.ds in
  (* calculate transitive closure for least fixpoint *)
  let g2 = Tgraph.tc g1 in
  (* return set of nodes that reach attr_target *)
  let needs =
    List.fold_left
      (fun s ->
        (function
            RuleDef(n,_,_) ->
              if PSet.mem attr_target (Tgraph.get_targets g2 n) then PSet.add n s else s
          | _ -> s))
      PSet.empty
      gr.ds in
  (fun n -> PSet.mem n needs)

let add_actuals needs actuals =
  let rec loop r =
    match r.r with
    | Symb(x,None,a,b) when needs x -> r.r <- Symb(x,Some actuals,a,b) (* TODO: attributes *)
    | Symb _ -> ()
          (* inductive cases *)
    | Minus(r2,r3)
    | Seq(r2,_,_,r3)
    | Alt(r2,r3) ->
        (loop r2; loop r3)
    | Opt(r2)
    | Assign(r2,_,_)
    | Rcount(_,r2)
    | Star(_,r2)
    | Hash(_,r2) ->
        loop r2
          (* trivial base cases *)
    | Lookahead _
    | Position _
    | When _
    | Action _
    | Box _
    | Delay _
    | Prose _
    | CharRange _
    | Lit _ -> () in
  loop

let transform gr =
  let needs = mk_needs_parameters gr in
  List.iter
    (function
        RuleDef(n,r,a) ->
          (match a.copy, a.early_params with
          | None, Some "" ->
              (* TODO: this should be an error because the result won't compile *)
              Util.warn Util.Sys_warn (Printf.sprintf "%s requires implicit parameters but none are given" n)
          | None, _ -> ()
          | Some(formals,actuals), Some "" (* formal parameters are @() *)
          | Some(formals,actuals), None when needs n ->
              a.early_params <- Some formals;
              add_actuals needs actuals r
          | _, None (* when not(needs n) *)
          | _, Some _ -> ())
      | _ -> ())
    gr.ds
*)
