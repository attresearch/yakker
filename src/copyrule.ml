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

(* The "copy rule" fills in attributes that grammar writers omit.

For each possible missing input attribute, we analyze the grammar to
see what nonterminals "need" the attribute.  We say that a nonterminal
needs an input attribute if (1) its definition explicitly lists the
attribute as an input, or (2) its RHS contains an occurrence of a
nonterminal that needs the attribute, and the occurrence does not
supply a value for the attribute.  For example:

    A@(;i:int;j:int) = ...
    B@(;k:int) = ... A(;j=3) ...

Here A needs attributes i and j, and B needs attribute k, according to
their LHSs.  Furthermore, B needs i because A needs i and A occurs on
the RHS of B without a value for i.  B does not need j because the
occurrence of A in its RHS explicitly supplies a value for j.

The analysis requires a fixed-point computation.

Once we determine what attributes a nonterminal needs, we add them to
the LHS and RHS:

    A@(;i:int;j:int) = ...
    B@(;i:int;k:int) = ... A(;i=i;j=3) ...

Here we have modified the LHS of B to add the missing needed
attribute, i, and we have modified the RHS of B to add the missing
needed attribute i to the inputs of the occurrence of A.

We also add missing input-output attributes.  We say an attribute is
an input-output attribute if it is explicitly declared as an input and
output attribute by a single nonterminal, at the same type.

If by the input attribute copyrule analysis above we add an input
attribute to a nonterminal, and that attribute is also an input-output
attribute, we ensure that the attribute is also declared as an output
attribute of the nonterminal.

Our analysis throughout compares types by string value for now.  This
is error prone, but it is much easier to implement than a complete
ocaml type analysis.

*)

open Gul
open Attr

let union x y = PSet.fold PSet.add x y
let intersect x y = PSet.filter (fun a -> PSet.mem a x) y
let difference x y = PSet.filter (fun a -> not(PSet.mem a y)) x
let list_of_set set = PSet.fold (fun x l -> x::l) set []
let set_of_list l = List.fold_right PSet.add l PSet.empty
let sort = List.sort (fun (x,_) (y,_) -> compare x y)

let mk_needs gr =
  let input_attributes_and_types =
    list_of_set       (* remove duplicates *)
      (set_of_list
         (List.concat (* all attributes and types in a list with duplicates *)
            (List.map
               (function
                   RuleDef(_,_,a) -> a.input_attributes
                 | _ -> [])
               gr.ds))) in
  let input_output_attributes_and_types =
    list_of_set       (* remove duplicates *)
      (set_of_list
         (List.concat (* all attributes and types in a list with duplicates *)
            (List.map
               (function
                   RuleDef(_,_,a) ->
                     List.filter
                       (fun x -> List.exists (fun y -> x=y) a.input_attributes)
                       a.output_attributes
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
    set_of_list(List.map fst input_attributes_and_types) in
  (if List.length input_attributes_and_types > PSet.cardinal all_attributes then
    Printf.eprintf "Warning: Copyrule says that some attribute has multiple types\n%!");

  (* build a graph, if a (nonterminal,attribute) reaches (attr_target,attribute)
     then the attribute should be an input of the nonterminal *)
  let attr_target = "**needs_attribute**" in
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
    let inputs_and_types =
      sort(list_of_set
             (PSet.map
                (fun a -> (a,List.assoc a input_attributes_and_types))
                (needs_attributes n))) in
    let input_outputs_and_types =
      List.filter
        (fun x -> List.exists (fun y -> x=y) input_output_attributes_and_types)
        inputs_and_types in
    (inputs_and_types,input_outputs_and_types) in
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
  (* Add missing declarations of input attributes and output attributes to their nonterminals *)
  List.iter
    (function
        RuleDef(n,_,a) ->
          let (attributes_and_types,input_outputs_attributes_and_types) = needs_attributes_and_types n in
          if n=gr.start_symbol && List.length(a.input_attributes)<>List.length(attributes_and_types)
          then Printf.eprintf "Error: the start symbol, %s, uses some nonterminal without supplying all attributes\n%!" n;
          a.input_attributes <- attributes_and_types;
          let missing_input_outputs_attributes_and_types =
            List.filter
              (fun x -> not(List.exists (fun y -> x=y) a.output_attributes))
              input_outputs_attributes_and_types in
          a.output_attributes <- sort(List.append missing_input_outputs_attributes_and_types a.output_attributes);
          Hashtbl.add tbl n (set_of_list (List.map fst attributes_and_types))
      | _ -> ())
    gr.ds;
  let needs n =
    try
      Hashtbl.find tbl n
    with Not_found ->
      (Printf.eprintf "Warning: Copyrule can't find %s\n%!" n; PSet.empty)
  in
  (* Add missing actual input attributes *)
  List.iter
    (function
        RuleDef(_,r,_) ->
          add_actuals needs r
      | _ -> ())
    gr.ds
