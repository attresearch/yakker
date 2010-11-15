(*******************************************************************************
 * Copyright (c) 2010 AT&T.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Trevor Jim and Yitzhak Mandelbaum and Qian Xi
 *******************************************************************************)

open Yak
open Gul

(*
  
   Some token-related utility functions.

*)

(** Used to short-circuit token search in case of success. *)
exception Found_tok_info of Gul.expr * (nonterminal * expr option)

(** Search the list that stores the token info, given a constructor/type name, 
    return the tokenizer, constructor, and carried_type.  *)
let search_tokmap tokmap n =
  let search_tokenizer (tokenizer, lit_env) =
    try raise (Found_tok_info (tokenizer, List.assoc n lit_env))
    with Not_found -> () in
  try
    List.iter search_tokenizer tokmap;
    None
  with Found_tok_info (tokenizer, (ocaml_constructor, carried_type) ) ->
    Some (tokenizer, ocaml_constructor, carried_type)

let is_token tokmap n = 
  match search_tokmap tokmap n with None -> false | Some _ -> true

let pr_tokmap tokmap =
  let printone (tokenizer, lit_env) =
    Printf.sprintf "%s: %s\n" tokenizer (String.concat "," 
					   (List.map (fun (s1,s2) -> s1^" "^s2) lit_env))
  in
    String.concat "\n" (List.map printone tokmap)

(******************************************************************************)

(* Figure out what nonterminals are producers *)
let producers gr =
  let direct_producers gr =
    gr.early_producers <- PSet.empty;
    gr.late_producers <- PSet.empty;
    List.iter
      (function RuleDef(n,r0, _) ->
        let early_producer() = gr.early_producers <- PSet.add n gr.early_producers in
        let late_producer()  = gr.late_producers <- PSet.add n gr.late_producers in
        let rec loop r =
          match r.r with
          | Symb _
          | Assign _ ->
              ()
          | Action(early,late) ->
              if early<>None then early_producer();
              if late<>None then late_producer()
          | Position true -> early_producer()
          | Position false -> late_producer()
          | Box(_,Some _,_) -> early_producer()
          | Box(_,None,_) ->
              ()
          | Delay _ ->
              late_producer()
          | Lit _ | CharRange _ | Prose _ | When _
          | Lookahead _ -> (* NB Lookahead can't be a producer *)
              ()
          | Seq(r1,_,_,r2) ->
              loop r2
          | Minus(r1,r2) (* TODO, we should desugar this *)
          | Alt(r1,r2) ->
              loop r1; loop r2
          | Opt(r1)
          | Rcount(_,r1)
          | Star(_,r1)
          | Hash(_,r1) ->
              loop r1 in
        loop r0
        | _ -> ())
      gr.ds in
  let tail_graph gr =
    let rec loop g n r = match r.r with
      (* x is in tail position in the rhs of n: add edge n->x *)
    | Symb(x,_,_,_) ->
        Tgraph.add_edge (Tgraph.add_node g x) n x
    | Action _
    | Position _
    | Box _
    | Delay _
    | Lit _
    | CharRange _
    | Prose _
    | When _
    | Lookahead _ -> (* NB Lookahead can't contribute a tail *)
        g
    | Seq(r1,_,_,r2) ->
        loop g n r2
    | Minus(r1,r2) (* we should desugar this *)
    | Alt(r1,r2) ->
        loop (loop g n r1) n r2
    | Opt(r1)
    | Assign(r1,_,_)
    | Rcount(_,r1)
    | Star(_,r1)
    | Hash(_,r1) ->
        loop g n r1
    in
    Tgraph.tc
      (List.fold_left
         (fun result ->
           (function
             RuleDef(n,r, _)->
               loop (Tgraph.add_node result n) n r
             | _ -> result))
         Tgraph.empty
         gr.ds)
  in
  direct_producers gr;
  let direct_early_producers = gr.early_producers in
  let direct_late_producers = gr.late_producers in
  let g = tail_graph gr in
  let intersect x y = PSet.filter (fun a -> PSet.mem a x) y in
  List.iter
    (function
        RuleDef(n,r,a) ->
          let tails = Tgraph.get_targets g n in
          if not(PSet.is_empty(intersect tails direct_early_producers))
          then gr.early_producers <- PSet.add n gr.early_producers;
          if not(PSet.is_empty(intersect tails direct_late_producers))
          then gr.late_producers <- PSet.add n gr.late_producers;
          (match PSet.mem n gr.early_producers,a.Attr.early_rettype with
          | true,None ->
              Printf.eprintf "Error: %s returns a value but its type is not declared\n%!" n
          | false,Some _ ->
              Printf.eprintf "Error: %s declares a return type but does not return a value\n%!" n
          | _,_ -> ())
      | _ -> ())
    gr.ds

(* Calculate early and late relevance for a grammar *)
(* first pass, calculates early relevance and direct late relevance *)
let rec relevance0 is_early_producer r =
  r.a.early_relevant <- false;
  r.a.late_relevant <- false;
  let add r' =
    relevance0 is_early_producer r';
    r.a.early_relevant <- (r.a.early_relevant || r'.a.early_relevant);
    r.a.late_relevant <- (r.a.late_relevant || r'.a.late_relevant);
  in
  match r.r with
  | Position true -> r.a.early_relevant <- true
  | Position false -> r.a.late_relevant <- true
  | Action(early,late) ->
      if early<>None then r.a.early_relevant <- true;
      if late<>None then r.a.late_relevant <- true
  | Box _ ->
      r.a.early_relevant <- true
  | Delay _ ->
      r.a.early_relevant <- true;
      r.a.late_relevant <- true;
  | When _ ->
      r.a.early_relevant <- true
  | Symb(n,early,early_attributes,late) ->
      (* NB early_attributes may be modified by the copy rule *)
      if early<>None || early_attributes<>[] || is_early_producer n then r.a.early_relevant <- true;
      if late<>None then r.a.late_relevant <- true
  | Seq(r1,early,late,r2) ->
      add r1;
      add r2;
      (match early with None -> ()
      | Some x ->
          if not(r2.a.early_relevant) then
            (* If r2 is not early-relevant, it has no actions which could use the binding *)
            (* NB r1 might not be relevant; if so, it will be made relevant by the lift transform *)
            Util.warn Util.User_warn (Printf.sprintf "%s is bound but not used" x));
      (match late with None -> ()
      | Some x ->
          if not(r2.a.late_relevant) then
            (* If r2 is not late-relevant, it has no actions which could use the binding *)
            (* NB r1 might not be relevant; if so, it will be made relevant by the lift transform *)
            Util.warn Util.User_warn (Printf.sprintf "%s is bound but not used" x));
  | Assign(r1,early,late) ->
      (* TODO: if r1 is not an early/late producer we should be assigning unit *)
      if early<>None then r.a.early_relevant <- true;
      if late<>None then r.a.late_relevant <- true;
      add r1
  | Rcount(_,r1) ->
      add r1;
      r.a.early_relevant <- true
  | Alt(r1,r2) | Minus(r1,r2) ->
      add r1;
      add r2
  | Opt r1 | Star(_,r1) | Hash(_,r1) ->
      add r1
  | Lookahead (_,r1) ->
      (* Complete relevance0 analysis for r1 but don't propagate relevance to the Lookahead node itself *)
      relevance0 is_early_producer r1
  | Lit _ | CharRange _ | Prose _ ->
      ()

(* second pass, calculates late relevance for all subterms given late relevance for nonterminals *)
let rec late_relevance late_relevant r =
  r.a.late_relevant <- false; (* careful: we start from scratch *)
  let add r' =
    late_relevance late_relevant r';
    r.a.late_relevant <- (r.a.late_relevant || r'.a.late_relevant);
  in
  match r.r with
  | Position false -> r.a.late_relevant <- true
  | Position true -> ()
  | Action(early,late) ->
      if late<>None then r.a.late_relevant <- true
  | Delay _ ->
      r.a.late_relevant <- true
  | Symb(n,_,_,_) ->
      r.a.late_relevant <- late_relevant n
  | Seq(r1,early,late,r2) ->
      if late<>None then r.a.late_relevant <- true;
      add r1;
      add r2
  | Assign(r1,early,late) ->
      if late<>None then r.a.late_relevant <- true;
      add r1
  | Alt(r1,r2) | Minus(r1,r2) ->
      add r1;
      add r2
  | Opt r1 | Rcount(_,r1) | Star(_,r1) | Hash(_,r1) ->
      add r1
  | Lookahead (_,r1) ->
      (* Complete late relevance analysis for r1 but don't propagate relevance to the Lookahead node itself *)
      late_relevance late_relevant r1
  | Box _ | When _ | Lit _ | CharRange _ | Prose _ ->
      ()

(* main function *)
let relevance gr =
  gr.grammar_early_relevant <- false;
  gr.grammar_late_relevant <- false;
  let is_early_producer n = PSet.mem n gr.early_producers in
  List.iter
    (function
        RuleDef(n,r,a) ->
          relevance0 is_early_producer r;
          if r.a.early_relevant then gr.grammar_early_relevant <- true;
          if r.a.late_relevant then gr.grammar_late_relevant <- true
      | _ -> ())
    gr.ds;
  let g = Tgraph.tc(dependency_graph gr.ds) in (* transitive but NOT reflexive closure *)
  let directly_late_relevant =
    List.fold_right PSet.add                                (* convert to set *)
      (List.concat
         (List.map                                             (* directly late relevant nonterminals *)
            (function RuleDef(n,r,_) -> [n] | _ -> [])
            (List.filter                                       (* directly late relevant ruledefs *)
               (function RuleDef(n,r,_) -> r.a.late_relevant | _ -> false)
               gr.ds)))
      PSet.empty in
  let intersect x y = PSet.filter (fun a -> PSet.mem a x) y in
  let late_relevant x =
    try
      let reachable_from_x = PSet.add x (Tgraph.get_targets g x) in
      not(PSet.is_empty (intersect reachable_from_x directly_late_relevant))
    with Not_found -> false in
  List.iter
    (function RuleDef(n,r,a) -> late_relevance late_relevant r | _ -> ())
    gr.ds;
  ()

let assignments gr =
  let rec loop r =
    r.a.early_assignments <- PSet.empty;
    r.a.late_assignments <- PSet.empty;
    let add r' =
      loop r';
      r.a.early_assignments <- (PSet.fold PSet.add r.a.early_assignments r'.a.early_assignments);
      r.a.late_assignments <- (PSet.fold PSet.add r.a.late_assignments r'.a.late_assignments)
    in
    match r.r with
    | Assign(r1,early,late) ->
        add r1;
        (match early with None -> () | Some x -> r.a.early_assignments <- PSet.add x r.a.early_assignments);
        (match late with None -> () | Some x -> r.a.late_assignments <- PSet.add x r.a.late_assignments)
    | Seq(r1,_,_,r2) | Alt(r1,r2) | Minus(r1,r2) ->
        add r1;
        add r2
    | Rcount(_,r1) | Opt r1 | Star(_,r1) | Hash(_,r1) | Lookahead (_,r1) ->
        add r1
    | Position _
    | Action _
    | Box _
    | Delay _
    | When _
    | Symb _
    | Lit _
    | CharRange _
    | Prose _ ->
        () in
  List.iter
    (function RuleDef(n,r,a) -> loop r | _ -> ())
    gr.ds


(******************************************************************************
 *
 *               PRECEDENCE-TRANSFORM ANALYSES
 *
 ******************************************************************************)

module Stringset = Set.Make(struct type t = string let compare = String.compare end)

let filter_map f l = List.rev (List.fold_left (fun v x -> match f x with Some y -> y::v | None -> v) [] l)

(* Map precedence levels to integers. 0 is reserved for "no precedence". *)
let create_prec_table gr =
  let tbl = Hashtbl.create 51 in
  let ps = gr.precs in
  for p = 0 to Array.length ps - 1 do
    List.iter (fun s -> Hashtbl.add tbl s (p+1)) (snd ps.(p));
  done;
  tbl

exception Found_prec of int

let invalid_prec_dir n = invalid_arg ("get_prec: precedence annotation \'" ^ n ^ "\' encountered, \
                                     but not found in precedence table.")

(** Find the precedence of the rule [r]. Default is precedence of
    rightmost terminal. If the rightmost terminal has no precedence,
    then 0 is returned. If there is no terminal and no
    prec. annotation, then None is returned. *)
let get_prec ptbl tokmap r = 
  let rec loop r = 
      match r.a.precedence with 
	| Some n -> 
	    let p = try Hashtbl.find ptbl n with Not_found -> invalid_prec_dir n in
	    raise (Found_prec p)
	| None -> loop0 r.r
  and loop0 = function      
    | Symb (x, _, _, _) -> 
	if is_token tokmap x then
	  let p = match Util.find_option ptbl x with None -> 0 | Some p -> p in
	  raise (Found_prec p)
	else ()
    | Assign _ 
    | Action _ 
    | Position _
    | Box _
    | Prose _ 
    | When _
    | Delay _ 
    | Lit _ 
	
    | Rcount _
    | Star _
    | Hash _
    | Alt _ | Lookahead _ 
    | Opt _
    | Minus _ (* TODO, we should desugar this *)
    | CharRange _ -> () 

    | Seq(r1,_,_,r2) ->
	loop r2; loop r1 in
  try loop r; None with Found_prec p -> Some p

type gpos = Right_of | Left_of

let left_pos = "__yk__left"
let right_pos = "__yk__right"

let string_of_gpos = function
  | Right_of -> right_pos
  | Left_of -> left_pos

let iter_with_prec f ptbl prec r =
  let rec loop v r = 
    match r.a.precedence with
      | None -> loop0 v r
      | Some x ->
	  (* We update pos to Left_of, regardless of current pos
	     because either way we're not to the left of a relevant
	     terminal. *)
	  let p = try Hashtbl.find ptbl x with Not_found -> invalid_prec_dir x in 
	  loop0 (p, Left_of) r
  and loop0 v r = match r.r with
    | Symb (x, _, _, _) ->
	(* Check to see whether this is the rightmost terminal. 
	   If it is, change pos. *)
	(try 
	   let p = Hashtbl.find ptbl x in 
	   let v2 = (p, Left_of) in
	   f v2 r; v2
	 with Not_found -> f v r; v)
    | Assign _
    | Action _
    | Position _
    | Box _
    | Prose _
    | When _
    | Delay _
    | Lit _
    | Rcount _
    | Star _
    | Hash _
    | Alt _ | Lookahead _
    | Opt _
    | Minus _ (* TODO, we should desugar this *)
    | CharRange _ ->  f v r; v

    | Seq(r1,_,_,r2) ->
	let v2 = loop v r2 in
	let v1 = loop v2 r1 in
	f v r; v1 in
  ignore (loop (prec, Right_of) r)

let prec_dependency_graph ptbl tokmap ds =
  let rec get_depend g n r = match r.r with
    (* Add dependencies for n to a graph given definition r *)
  | Symb(x,_,_,_) ->
      Tgraph.add_edge (Tgraph.add_node g x) n x
  | Position _
  | Prose _
  | When _ | Action _ | Box _ | Delay _
  | CharRange _
  | Lit _ -> g
  | Minus(r2,r3)
  | Seq(r2,_,_,r3)
  | Alt(r2,r3) ->
      get_depend (get_depend g n r2) n r3
  | Assign(r2,_,_)
  | Opt(r2)
  | Rcount(_,r2)
  | Star(_,r2)
  | Hash(_,r2)
  | Lookahead(_,r2) ->
      get_depend g n r2
  in
  List.fold_left
    (fun g_result -> function
        RuleDef(n,r, _)->
	  let g = Tgraph.add_node g_result n in
	  let rs = alt2rules r in
	  List.fold_left (fun g r_b ->

(* TODO: refine further so that a dependency is not drawn for calls when they are not to the left/right of relevant nonterminal. 
get_depend should be like iter_with_prec. could generalize to fold_right_with_prec. not sure this is right, though. e.g 

LPAREN expr STAR expr RPAREN. ?
*)

			    match get_prec ptbl tokmap r_b with
			      | Some 0 -> g
			      | None | Some _ -> get_depend g n r_b) g rs

      | _ -> g_result)
    Tgraph.empty
    ds

let build_prec_sets gr =
  let ptbl = create_prec_table gr in
  let has_prec r = match get_prec ptbl gr.tokmap r with 
      None | Some 0 -> false 
    | _ -> true in
  let is_primary = function 
    | RuleDef (n, r, _) -> 
	let rules = alt2rules r in
	if List.exists has_prec rules then Some n else None
    | _ -> None in	       
  let primary = List.fold_left (fun s x -> Stringset.add x s) 
    Stringset.empty (filter_map is_primary gr.ds) in
  let g = prec_dependency_graph ptbl gr.tokmap gr.ds in
  let tc_g = Tgraph.tc g in
  let update n1 n2 (ic, cr) =
    ((if Stringset.mem n1 primary then Stringset.add n2 ic else ic),
     (if Stringset.mem n2 primary then Stringset.add n1 cr else cr)) in
  let in_context, calls_relevant = Tgraph.fold_edges update tc_g (Stringset.empty, Stringset.empty) in
  let secondary = Stringset.inter in_context (Stringset.union calls_relevant primary) in
  let relevant = Stringset.union primary secondary in
  let both = Stringset.inter primary secondary in
  let primary_only = Stringset.diff primary both in
  let secondary_only = Stringset.diff secondary both in
  ptbl, primary, secondary, relevant, both, primary_only, secondary_only

let print_prec_sets (_, primary, secondary, relevant, both, primary_only, secondary_only) =
  let pr = Printf.printf in
  let prset = Stringset.iter (pr "  %s\n") in
  pr "Primary:\n";
  prset primary;
  pr "Secondary:\n";
  prset secondary;
  pr "Relevant:\n";
  prset relevant;
  pr "Both:\n";
  prset both;
  pr "Primary-only:\n";
  prset primary_only;
  pr "Secondary-only:\n";
  prset secondary_only

(** Compute whether a given associativity is permitted (not necessary).
    It is a conservative estimate of the desired associativity. As long as
    [x] is not other-assoc. (where other should be either left or right)
    or non-assoc., it counts. This way,
    if [x] is not declared with any associativity, the given associativity 
    is permitted. *)
let mk_assoc_array other_a n_a =
  let len = Array.length other_a in
  if Array.length n_a <> len then 
    invalid_arg "mk_assoc_array: array arguments must be of the same length.";
  Array.init len (fun p -> not ( other_a.(p) or n_a.(p) ))

(** Rewrite the grammar based on the precedence annotations. *)
let prec_rewrite gr =
  if gr.precs = [||] then () else
  let (ptbl, primary, secondary, relevant, both, primary_only, secondary_only) = 
    build_prec_sets gr in
  
  let prec_var = "prec" in
  let prec_type = "Pami.prec" in
  let pos_var = "gpos" in 
  let pos_type = "Pami.assoc_pred" in
  let mk_pguard p = Printf.sprintf "Pami.prec_case_guard %d %s %s" p prec_var pos_var in

  (* Where to place the guard: base on where precedence comes from.
     - From annotation, place at end of rule case.
     - From terminal, place immediately following terminal.

    Note that we should be able to further analyze the grammar to further filter guard placement. 
    Here's the principle: a production of lower precedence should never appear lower in a tree 
     than a production of higher precedence, without passing through some reset.
    We can compare current precedence against max precedence of parents. If it is higher, than 
    there is no need to guard. Correspondingly, can filter attribute placement by comparing against max
    precedence of children. For each child, if their precedence is higher, than no need to pass parameters.
    Note, of course, that for both of these cases, since symbols can be called from multiple sights, you 
     might not be able to take advantage of these analyses without duplicating the symbol.
  *) 

  let instantiate_attrs =
    iter_with_prec (fun (prec, pos) r -> match r.r with
		      | Symb (n, e1, attrs, e2) -> 
			  if Stringset.mem n secondary then
			    r.r <- Symb(n, e1, (prec_var, string_of_int prec)
					  ::(pos_var, string_of_gpos pos)
					  :: attrs, e2)
		      | _ -> ()) in

  let instantiate_attrs_irrel =
    iter_rule_postorder (fun r -> match r.r with
			   | Symb (n, e1, attrs, e2) -> 
			       if Stringset.mem n relevant then
				 r.r <- Symb(n, e1, (prec_var, "0")::(pos_var, left_pos)::attrs, e2)
			   | _ -> ()) in

  (** Rewrite a rule to include handle precedence. 

     Step 1: Add attribute declarations to nonterminals.
     Step 2: Instantiate attributes. 
     Step 3: Add guards to branches. 

      Almost there: can still have a secondary nonterm where all branches are "capped" 
      resulting in copyrule not add params, yet it is still given attributes by others.

      TODO - assign attributes according to *MOST RECENT* rightmost
      terminal. Seems like it should be insensitive to
      factoring. Precedence of the rule itself remains that of the rightmost terminal.

  *)
  let add_prec_attrs tokmap = function
    | RuleDef (n, r, a) ->
	if Stringset.mem n primary then begin
	  a.Attr.input_attributes <- (prec_var, prec_type)::(pos_var, pos_type)::a.Attr.input_attributes;

	  let rs = alt2rules r in
	  List.iter (fun r_b ->
		       match get_prec ptbl tokmap r_b with
			 | None -> () (* leave it to copyrule. *)
			 | Some 0 -> instantiate_attrs_irrel r_b
			 | Some p ->
			     instantiate_attrs ptbl p r_b;
			     let guard = mkWHEN (mk_pguard p) in
			     r_b.r <- Seq(guard, None, None, mkRHS r_b.r)) rs
	end else if Stringset.mem n secondary then begin
	  (* "Cap" attributes in 0-precedence branches of secondary nodes. *)
	  let rs = alt2rules r in
	  List.iter (fun r_b ->
		       match get_prec ptbl tokmap r_b with
			 | Some 0 -> instantiate_attrs_irrel r_b
			 | Some _ -> 
			     Util.impossible ("branch with non-zero precedence found in secondary symbol " ^ n)
			 | None -> ()) rs
	end else (* not relevant *) begin
	  instantiate_attrs_irrel r
	end;
	(* Clear any precedence annotations, now that they've been processed. *)
	iter_rule_postorder (fun r -> r.a.precedence <- None) r
    | LexerDef _ | LexerDecl _ -> () in
  List.iter (add_prec_attrs gr.tokmap) gr.ds;

  (* Compute associativity predicates (as arrays l_a, n_a,
     r_a). Element 0 is reserved for "everything else".  *)
  let n = Array.length gr.precs in
  let sz = n + 1 in
  let l_a = Array.make sz false in
  let r_a = Array.make sz false in
  let n_a = Array.make sz false in
  l_a.(0) <- true;
  for i = 1 to n do 
    match gr.precs.(i-1) with 
      | (Left_assoc, _) -> l_a.(i) <- true
      | (Right_assoc, _) -> r_a.(i) <- true
      | (Non_assoc, _) -> n_a.(i) <- true
  done;

  (* Generate [left] and [right] from l_a, n_a and r_a. *)
  let left = mk_assoc_array r_a n_a in
  let right = mk_assoc_array l_a n_a in

  (* Print [left] and [right]. *)
  let string_of_bool_array name a =
    let b = Buffer.create 11 in
    Printf.bprintf b "\nlet %s = [|" name;
    Array.iter (Printf.bprintf b "%B;") a;
    Printf.bprintf b "|]\n";
    Buffer.contents b in

  add_to_prologue gr (string_of_bool_array left_pos left); 
  add_to_prologue gr (string_of_bool_array right_pos right);

  (* Clear the precedence attributes, now that they've been folded in. *)
  gr.precs <- [||]


(******

Simpler version:

   1) infer left-right correctly
   2) for middle, always supply precedence 0. this will guarantee not to prune things.

Simple version of precedence transformation.

       1) secondary = set of nonterminals called *directly* from primary
       production.  

       2) for every secondary, add attributes.

       3) for every primary production, fill in attributes, where right is
       for rightmost and left is for leftmost. this is a conservative
       approach, because nullable nonterminals can hide left/right-ness.



*)  


(***************

This unroll function:

Gets the info of whether the body of an Kleene-closure is an early producer, late producer or not a producer;

Then
if late producer then
 unrolls the body for n(=lower bound) times and binds the return value to x1; goes through the rest and binds the return value to x2; constructs the new return value List.append x1 x2
if not a producer then
 simply unrolls the body for n times and then goes through the rest
else
 do nothing

Example 1: a = 2*4(b) => a = (b b) *2b
Example 2: a = 2*4({1}) => a = a = (((({1}$_x6 {1} {_x4}) {List.append [_x6] [_x4]}) {_x5}) *2{1})$_x2 {List.append _x5 _x2}.

****************)

let unroll_analyze gr =
  if !Compileopt.unroll_star_n > 0 then begin
    let unroll_n = !Compileopt.unroll_star_n in
    Logging.log Logging.Features.verbose "Unrolling (<%d)\n%!" unroll_n;
    producers gr;
    let is_early_producer n = PSet.mem n gr.early_producers in
    let is_late_producer n = PSet.mem n gr.late_producers in

    (* a helper function for unrolling *)
    let valOf = function
        Some x1 -> x1
      | None -> failwith "Option value not found" in
    let rec unroll n r1 fresh =
      if fresh then
        if n == 1 then
          let x = Variables.fresh() in
          (mkSEQ2(r1, None, None, mkACTION2(None, Some x)), Some x, true)
        else
          let (scd, name, mklistflag) = unroll (n-1) r1 fresh in
          let name  = valOf name in
          let x,y = Variables.fresh(),Variables.fresh() in
          if mklistflag then
            (mkSEQ2(mkSEQ2(mkSEQ2(r1, None, Some x, scd), None, None,
                           mkACTION2(None, Some ("List.append ["^x^"] ["^name^"]"))),
                    None, None, mkACTION2(None, Some y)), Some y, false)
          else
            (mkSEQ2(mkSEQ2(mkSEQ2(r1, None, Some x, scd), None, None,
                           mkACTION2(None, Some ("List.append "^x^" ["^name^"]"))),
                    None, None, mkACTION2(None, Some y)), Some y, false)
      else
        if n == 1 then (r1, None, false)
        else
          let (scd, _, _) = unroll (n-1) r1 fresh in
          (mkSEQ2(r1, None, None, scd), None, false) in
    let rec loop r =
      match r.r with
        | Action(early,late) ->
            (early<>None,late<>None)
        | Position true -> (true,false)
        | Position false -> (false,true)
        | Symb (n, _, _, _) -> (is_early_producer n,is_late_producer n) (* TODO: attributes *)
        | Delay _ -> (false,true)
        | Box(_,Some _,_) -> (true,false)
        | Box(_,None,_) -> (false,true)
        | Minus(r1,r2) -> (false,false)
        | Lit _ | CharRange _ | Prose _ | When _ | Lookahead _ -> (false,false)
        | Star(Accumulate(_,_),r1) -> loop r1
        | Hash(Accumulate(_,_),r1) -> loop r1
        | Seq(r1,early,late,r2) -> ignore (loop r1); loop r2
        | Assign(r1,early,late) -> ignore (loop r1); (false,false)
        | Opt _
        | Alt _ ->
            let alt2rules = (* differs from bnf.ml b/c need to desugar Opt *)
              let rec loop l r = match r.r with
                | Alt(r1,r2) -> loop (loop l r2) r1
                | Opt(r1) ->
                    r.r <- (mkALT[r1;mkLIT ""]).r; loop l r
                | _ -> r::l in
              loop [] in
            let alts = alt2rules r in
            let lift_p = List.map (fun r -> loop r) alts in
            let early_all_true  = List.for_all (fun (x,_) -> x)     lift_p in
            let early_all_false = List.for_all (fun (x,_) -> not x) lift_p in
            let late_all_true   = List.for_all (fun (_,x) -> x)     lift_p in
            let late_all_false  = List.for_all (fun (_,x) -> not x) lift_p in
            (match early_all_true,early_all_false,late_all_true,late_all_false with
                 true,_,true,_ -> (true,true)
               | true,_,_,true -> (true,false)
               | _,true,true,_ -> (false,true)
               | _,true,_,true -> (false,false)
               | _ ->
                   let is_early_producer = early_all_true || not(early_all_false) in
                   let is_late_producer = late_all_true || not(late_all_false) in
                   (is_early_producer,is_late_producer))

        (* no unrolling for Rcount and Hash *)

        | Rcount(e,r1) ->
            let early_producer,late_producer = loop r1 in
            (early_producer,late_producer)
        | Star(Bounds(m,Num n),r1) ->
            let early_producer,late_producer = loop r1 in

            (* FIX: looks like m = n case should be special-cased, not
               ignored. ? YHM *)

            if 0<m && m <= unroll_n && m<>n then
              (* note that 2"a" shouldn't be unrolled to "a" "a" 0"a", in which case m=n *)
              begin
                if late_producer then
                  let x,y = Variables.fresh(),Variables.fresh() in
                  let (rhs, name, mklistflag) = unroll m r1 true in
                  let name = valOf(name) in
                  if mklistflag then
                    r.r <-
                      (mkSEQ2((mkSEQ2(rhs, None, None, mkSTAR2(Bounds(0, Num (n-m)), r1))),
                              None, Some y, mkACTION2(None, Some ("List.append ["^name^"] "^y)))).r
                  else
                    r.r <-
                      (mkSEQ2((mkSEQ2(rhs, None, None, mkSTAR2(Bounds(0, Num (n-m)), r1))),
                              None, Some y, mkACTION2(None, Some ("List.append "^name^" "^y)))).r
                else if (not(late_producer) && not(early_producer)) then
                  let (rhs, _, _) = unroll m r1 false in
                  r.r <-
                    (mkSEQ2(rhs, None, None, mkSTAR2(Bounds(0, Num (n-m)), r1))).r
                else ()
              end;
            (early_producer, late_producer)
        | Star(Bounds(0,Infinity),r1) ->
            let early_producer,late_producer = loop r1 in
            (early_producer,late_producer)
        | Star(Bounds(m,Infinity),r1) ->
            Logging.log Logging.Features.verbose "Unrolling star %d (<%d)\n%!" m unroll_n;
            let early_producer,late_producer = loop r1 in
            if 0<m && m <= unroll_n then
              begin
                if late_producer then
                  let x,y = Variables.fresh(),Variables.fresh() in
                  let (rhs, name, mklistflag) = unroll m r1 true in
                  let name = valOf(name) in
                  if mklistflag then
                    r.r <-
                      (mkSEQ2((mkSEQ2(rhs, None, None, mkSTAR2(Bounds(0, Infinity), r1))),
                              None, Some y, mkACTION2(None, Some ("List.append ["^name^"] "^y)))).r
                  else
                    r.r <-
                      (mkSEQ2((mkSEQ2(rhs, None, None, mkSTAR2(Bounds(0, Infinity), r1))),
                              None, Some y, mkACTION2(None, Some ("List.append "^name^" "^y)))).r
                else if (not(late_producer) && not(early_producer)) then
                  let (rhs, _, _) = unroll m r1 false in
                  r.r <-
                    (mkSEQ2(rhs, None, None, mkSTAR2(Bounds(0, Infinity), r1))).r
                else ()
              end;
            (early_producer,late_producer)
        | Hash(Bounds(m,Num n),r1) ->
            let early_producer,late_producer = loop r1 in
            (early_producer,late_producer)
        | Hash(Bounds(0,Infinity),r1) ->
            let early_producer,late_producer = loop r1 in
            (early_producer,late_producer)
        | Hash(Bounds(m,Infinity),r1) ->
            let early_producer,late_producer = loop r1 in
            (early_producer,late_producer) in
    List.iter
      (function RuleDef(n,r,a) -> ignore(loop r) | _ -> ())
      gr.ds
  end


(******************
This function recognizes parts in the grammar that belong to the regular family, and inlines those parts for potential speedup.

Example 1:
B = *"a"
D = "b"
C = D
A = B C
=>:
B = *"a".
D = "b".
C = "b".
A = *"a" "b".

Example 2:
B = *"a" {1}
A = B B
=>: The transformation is not performed because B produces a returned value.
*******************)

let regular_inline gr trans =
  relevance gr;

  let valOf x =
    match x with
        Some x1 -> x1
      | None -> failwith "Option value not found"
  in

  let rec loop inline_map r = begin
    if not(r.a.is_regular) then
      if not(r.a.early_relevant) && not(r.a.late_relevant) then begin
        match r.r with
          | Action _
          | Position _
          | Delay _
          | Box _ -> (None, false, false)
          | Minus (r1, r2) ->
              let (myr1, has_change1, is_regular1) = loop inline_map r1 in
              let (myr2, has_change2, is_regular2) = loop inline_map r2 in
              let newr1 = if (has_change1 && is_regular1) then valOf(myr1) else r1 in
              let newr2 = if (has_change2 && is_regular2) then valOf(myr2) else r2 in
              let newrdotr = Minus(newr1, newr2) in
                if (is_regular1 && is_regular2) then begin
                  if trans then
                    r.r <- newrdotr;
                  r.a.is_regular <- true;
                  let newr = dupRule r in
                    (Some newr, true, true)
                end
                else if ((is_regular1 || is_regular2) && (has_change1 || has_change2)) then begin
                  if trans then r.r <- newrdotr;
                  (None, true, false)
                end
                else if (has_change1 || has_change2) then
                  (None, true, false)
                else (None, false, false)
          | Symb (n1, _, _, _) -> (* TODO: attributes *)
              if PMap.mem n1 inline_map then
                let r1 = PMap.find n1 inline_map in
                let myr = dupRule r1 in
                  (
                    if trans then begin
                      r.a <- r1.a;
                      r.r <- r1.r;
                    end;
                      r.a.is_regular <- true;
                    (Some myr, true, true)
                  )
              else
                (None, false, false)
          | Lit _ | CharRange _  ->
              r.a.is_regular <- true;
              let myr = dupRule r in
                (Some myr, true, true)
          | Prose _ | When _ | Lookahead _ -> (None, false, false)
          | Star(Accumulate(_,_),r1)
          | Hash(Accumulate(_,_),r1) -> (None, false, false)
          | Assign(r1,early,late) ->
              if (early<>None || late<>None) then (None, false, false) else
              (*TODO: basically the same as Opt, refactor*)
              let (myr1, has_change, is_regular) = loop inline_map r1 in
              if (has_change && is_regular) then
                let newr1 = valOf myr1 in begin
                  if trans then r.r <- Assign(newr1,early,late);
                  r.a.is_regular <- true;
                  let newr = dupRule r in
                  (Some newr, true, true)
                end
              else if has_change then (None, true, false)
              else (None, false, false)
          | Seq(r1,early,late,r2) ->
              if (early==None && late==None) then (* note that if there's binding of the first part, then the entire sequence won't be a regular language *)
                let (myr1, has_change1, is_regular1) = loop inline_map r1 in
                let (myr2, has_change2, is_regular2) = loop inline_map r2 in
                let newr1 = if (has_change1 && is_regular1) then valOf(myr1) else r1 in
                let newr2 = if (has_change2 && is_regular2) then valOf(myr2) else r2 in
                let newrdotr = Seq(newr1, early, late, newr2) in
                  if (is_regular1 && is_regular2) then begin
                    if trans then r.r <- newrdotr;
                    r.a.is_regular <- true;
                    let newr = dupRule r in
                      (Some newr, true, true)
                  end
                  else if ((is_regular1 || is_regular2) && (has_change1 || has_change2)) then begin
                    if trans then r.r <- newrdotr;
                    (None, true, false)
                  end
                  else if (has_change1 || has_change2) then
                    (None, true, false)
                  else (None, false, false)
              else (None, false, false)
          | Opt r1 ->
              let (myr1, has_change, is_regular) = loop inline_map r1 in
                if (has_change && is_regular) then
                  let newr1 = valOf myr1 in begin
                      if trans then r.r <- Opt newr1;
                      r.a.is_regular <- true;
                      let newr = dupRule r in
                        (Some newr, true, true)
                    end
                else if has_change then (None, true, false)
                else (None, false, false)
          | Alt (r1, r2) ->
              let (myr1, has_change1, is_regular1) = loop inline_map r1 in
              let (myr2, has_change2, is_regular2) = loop inline_map r2 in
              let newr1 = if (has_change1 && is_regular1) then valOf(myr1) else r1 in
              let newr2 = if (has_change2 && is_regular2) then valOf(myr2) else r2 in
              let newrdotr = Alt(newr1, newr2) in
                if (is_regular1 && is_regular2) then begin
                  if trans then r.r <- newrdotr;
                  r.a.is_regular <- true;
                  let newr = dupRule r in
                    (Some newr, true, true)
                end
                else if ((is_regular1 || is_regular2) && (has_change1 || has_change2)) then begin
                  if trans then r.r <- newrdotr;
                  (None, true, false)
                end
                else if (has_change1 || has_change2) then
                  (None, true, false)
                else (None, false, false)
          | Rcount(e,r1) -> (None, false, false)
          | Star(e,r1) ->
              let (myr1, has_change, is_regular) = loop inline_map r1 in
                if (has_change && is_regular) then
                  let newr1 = valOf myr1 in begin
                      if trans then r.r <- Star(e, newr1);
                      r.a.is_regular <- true;
                      let newr = dupRule r in
                        (Some newr, true, true)
                    end
                else if has_change then (None, true, false)
                else (None, false, false)
          | Hash(e,r1) ->
              let (myr1, has_change, is_regular) = loop inline_map r1 in
                if (has_change && is_regular) then
                  let newr1 = valOf myr1 in begin
                      if trans then r.r <- Hash(e, newr1);
                      r.a.is_regular <- true;
                      let newr = dupRule r in
                        (Some newr, true, true)
                    end
                else if has_change then (None, true, false)
                else (None, false, false)
      end
      else (None, false, false)
    else (None, false, true)
  end
  in
  let rec iter map gr =
    let loopfunc (inline_map, flag) rd =
      match rd with
        | RuleDef(n, r, a) ->
            let (myr, has_change, is_regular) = loop inline_map r in (* has_change indicates if there's any inlining action taken during this pass and do_inline indicates whether the returned rhs should be added to inline_map *)
            let newflag = flag || has_change in
              if (is_regular && has_change) then let newmap = PMap.add n (valOf(myr)) inline_map in (newmap, newflag)
              else (inline_map, newflag)
        | _ -> (inline_map, flag)
    in
    let (inline_map, flag) = List.fold_left loopfunc (map, false) gr.ds in
      if flag then iter inline_map gr
  in
    iter PMap.empty gr


module First_set_lex = struct

  type tokens = string * (string * (string option)) list
      (** tokenizer, (ocaml_constructor,carried_type) list: store all tokens from the same tokenizer *)

  type t = {
    nonempty: Cs.t * tokens list;
    (** first set (cs,ts), including single characters from default tokenizer (cs)
        and all tokens from ocaml lexer tokenizers (ts) *)

    maybe_nonempty: (rhs * Cs.t) list;
    (** guarded first set*)

    maybe_empty: rhs list;
    (** guarded nullable *)

    epsilon: bool;
    (** nullable *)
  }

  let empty () = {nonempty = Cs.empty (),[];
                  epsilon = false;
                  maybe_empty = [];
                  maybe_nonempty = []}

  let non_singleton (l, u) = {nonempty = Cs.range l (u+1),[];
                              epsilon = false;
                              maybe_empty = [];
                              maybe_nonempty = []}

  let non_singleton_tok tokenizer ocaml_constructor carried_type =
    { nonempty = Cs.empty (), [(tokenizer, [(ocaml_constructor,carried_type)])];
      epsilon = false;
      maybe_empty = [];
      maybe_nonempty = []}

  let maybe_singleton r = {nonempty = Cs.empty (),[];
                           epsilon = true;
                           maybe_empty = [r];
                           maybe_nonempty = []}

  let epsilon_singleton () =  {nonempty = Cs.empty (),[];
                               epsilon = true;
                               maybe_empty = [];
                               maybe_nonempty = []}

  let to_rule = function
    | {nonempty = cs,[];
       epsilon = eps;
       maybe_empty = [];
       maybe_nonempty = []} ->
         let r = Minus.cs2rule cs in
         if eps then mkOPT(r) else r
    | _ -> Util.impossible "Analyze.to_rule"

(* function to get the string of token list field if t.nonempty *)
let pr_ts ts =
  let printone (tokenizer, ntl) =
    Printf.sprintf "%s: %s" tokenizer (String.concat "," (List.map (fun (oc,ct) -> (Printf.sprintf "%s" oc)) ntl))
  in
    String.concat "\n" (List.map printone ts)

(* function to get the string of the first set of one nonterminal *)
let print_fs fs =
  let nonemptystr = let cs,ts=fs.nonempty in Printf.sprintf "cs: %s, ts: %s" (Cs.to_nice_string cs) (pr_ts ts) in
  let epsilonstr = if fs.epsilon then "nullable" else "not nullable" in
  let maybe_nonemptystr = if fs.maybe_nonempty == [] then "empty" else "not empty" in
  let maybe_emptystr = if fs.maybe_empty == [] then "empty" else "not empty" in
    ("nonempty: "^nonemptystr^"\nepsilon: "^epsilonstr^"\nmaybe_nonempty: "^maybe_nonemptystr^"\nmaybe_empty: "^maybe_emptystr^"\n")


(* helper functions that computes the union or concatenation of two first sets *)

let union_cs cs1 cs2 =
  let cs = Cs.dup cs1 in
    Cs.union cs cs2; cs

let union_ts ts1 ts2 =
  match ts1,ts2 with
    | [],[] -> []
    | [],s -> s
    | s,[] -> s
    | _ ->
  let merge_one (tokenizer, nontlist) =
    if List.mem_assoc tokenizer ts2 then
      let nontlist2 = List.assoc tokenizer ts2 in
        (tokenizer, nontlist@(List.filter (fun t2 -> not(List.mem t2 nontlist)) nontlist2))
    else
      (tokenizer, nontlist)
  in
    List.map merge_one ts1

let union fs1 fs2 =
  let fs1cs,fs1ts = fs1.nonempty in
  let fs2cs,fs2ts = fs2.nonempty in
  let cs = Cs.dup fs1cs in
    Cs.union cs fs2cs;
    {nonempty = cs,union_ts fs1ts fs2ts;
     epsilon = fs1.epsilon || fs2.epsilon;
    maybe_empty = fs1.maybe_empty @ (List.filter (fun r -> not(List.mem r fs1.maybe_empty)) fs2.maybe_empty);
    maybe_nonempty = fs1.maybe_nonempty @ (List.filter (fun r -> not(List.mem r fs1.maybe_nonempty)) fs2.maybe_nonempty)}

let fs_notempty fs =
  let fscs,fsts = fs in
    Cs.count fscs <> 0 || fsts <> []

let concat fs1 fs2 early late =
  let fs1cs,fs1ts = fs1.nonempty in
  let fs2cs,fs2ts = fs2.nonempty in
  {nonempty = if not(fs_notempty fs1.nonempty) || fs1.epsilon || fs1.maybe_empty <> [] then
     (let cs = Cs.dup fs1cs in
        Cs.union cs fs2cs;
        cs,union_ts fs1ts fs2ts)
   else fs1.nonempty;
   epsilon = fs1.epsilon && fs2.epsilon;
     maybe_nonempty =
        if Cs.count fs2cs > 0 || fs2ts != [] then
              fs1.maybe_nonempty @
                List.map (fun r -> r, fs2cs) fs1.maybe_empty
        else fs1.maybe_nonempty;
     maybe_empty =
        if fs2.maybe_empty = [] then fs1.maybe_empty
        else
          let cross_comb l r =
            l@(List.map (fun r1 -> mkSEQ2(r,early,late,r1)) fs2.maybe_empty)
          in
            List.fold_left cross_comb [] fs1.maybe_empty
    }

let is_nonempty fs = (not fs.epsilon) && fs.maybe_empty = [] && fs.maybe_nonempty = []

(* Can only be invoked on strictly nonempty first sets. *)
let difference fs1 fs2 =
  let fs1cs,fs1ts = fs1.nonempty in
  let fs2cs,fs2ts = fs2.nonempty in
  if is_nonempty fs1 then
    let cs = Cs.dup fs1cs in
    let _ = Cs.difference cs fs2cs in
    let doone ret (tokenizer, lit_env) =
      if List.mem_assoc tokenizer fs2ts then
        let lit_env2 = List.assoc tokenizer fs2ts in
        let lit_env = List.filter (fun constructor -> not(List.mem constructor lit_env2)) lit_env in
          if lit_env = [] then ret
          else ret@[(tokenizer, lit_env)]
      else ret@[(tokenizer, lit_env)]
    in
    let ts = List.fold_left doone [] fs1ts in
      {nonempty = cs,ts; epsilon = not(fs_notempty (cs,ts)); maybe_empty = []; maybe_nonempty = []}
  else epsilon_singleton()

let first_gr gr tokmap =
  let rec loop first_map r fv =
    match r.r with
      | Symb (n1, _, _, _) -> (* TODO: attributes *)
          if PMap.mem n1 first_map then
            let fs1 = PMap.find n1 first_map in
              (fs1, true)
          else (empty(), false)
          | When _
      | Lookahead _ (* -> if fv then (maybe_singleton r, false) else (maybe_singleton r, true) *) (* This is the hack for recursive parts. *)
      | Box _
      | Action _
      | Position _
      | Delay _ | Prose _ | Lit (_, "") ->
          if fv then
            (epsilon_singleton (), false)
          else
          (epsilon_singleton (), true)
      | Lit (b, s) ->
          let c = s.[0] in
          if b then
            let c = Char.code c in
            if fv then
              (non_singleton (c,c), false)
            else
              (non_singleton (c,c), true)
          else
            let lower = Char.lowercase c in
            let lower = Char.code lower in
            let upper = Char.uppercase c in
            let upper = Char.code upper in
            let fs = union (non_singleton (lower, lower)) (non_singleton (upper, upper)) in
            if fv then (fs, false) else (fs, true)
      | CharRange (x,y) -> if fv then (non_singleton (x,y), false) else (non_singleton (x,y), true)
      | Seq(r1, early, late, r2) ->
          let (fs1, has_change1) = loop first_map r1 fv in
          let (fs2, has_change2) = loop first_map r2 fv in
            if has_change1 || has_change2 then
              (concat fs1 fs2 early late, true)
            else
              (concat fs1 fs2 early late, false)
      | Minus (r1,r2) ->
          let (fs1, has_change1) = loop first_map r1 fv in
          let (fs2, has_change2) = loop first_map r2 fv in
            if has_change1 || has_change2 then
              (difference fs1 fs2, true)
            else
              (difference fs1 fs2, false)
      | Alt(r1,r2) ->
          let (fs1, has_change1) = loop first_map r1 fv in
          let (fs2, has_change2) = loop first_map r2 fv in
            if has_change1 || has_change2 then
              (union fs1 fs2, true)
            else
              (union fs1 fs2, false)
      | Star (Bounds(m,_), r1) ->
          let (fs1, has_change1) = loop first_map r1 fv in
            if m > 0 then
              (fs1, has_change1)
            else
              let retfs = {nonempty = fs1.nonempty; epsilon = true; maybe_empty = fs1.maybe_empty; maybe_nonempty = fs1.maybe_nonempty} in
                (retfs, has_change1)
      | Assign(r1,_,_) | Opt(r1) | Star (_,r1) ->
          let (fs1, has_change1) = loop first_map r1 fv in
            ({nonempty = fs1.nonempty; epsilon = true; maybe_empty = fs1.maybe_empty; maybe_nonempty = fs1.maybe_nonempty}, has_change1)
      | Rcount(_,r1)
      | Hash(_,r1) ->
          loop first_map r1 fv
  in
  let flag = ref false in
  let iter gr =
    let map = ref PMap.empty in
    let loopfunc first_map rd =
      match rd with
        | RuleDef(n, r, a) ->
            let fv = PMap.mem n first_map in
            let (fs1, has_change) =
              match search_tokmap tokmap n with
                | None -> loop first_map r fv
                | Some (tokenizer,ocamlconstructor,carried_type) -> (non_singleton_tok tokenizer ocamlconstructor carried_type, true)
            in
            let newmap =
              if has_change then
                if fv then
                  let fs = PMap.find n first_map in
                    if compare fs fs1 <> 0 then
                      let newmap = PMap.remove n first_map in
                      let newmap = PMap.add n fs1 newmap in
                      let _ = flag := true in
                        newmap
                    else
                      first_map
                else
                  (let newmap = PMap.add n fs1 first_map in let _ = flag := true in newmap)
              else first_map
            in
              newmap
        | _ -> first_map
    in
    let _ = flag := true in
      while !flag do
        let _ = flag := false in
        let first_map = List.fold_left loopfunc !map gr.ds in
          map := first_map;
      done;
      !map
  in
    iter gr

let first r first_map =
  let rec loop r =
    match r.r with
      | Symb (n1, _, _, _) ->  (* TODO: attributes *)
          (try PMap.find n1 first_map with Not_found -> Printf.eprintf "Warning: Unable to compute %s's FIRST set." n1; empty ())
          | When _
      | Lookahead _ -> maybe_singleton r
      | Box _
      | Action _
      | Position _
      | Delay _ | Prose _ | Lit (_, "") -> epsilon_singleton ()
          | Lit (b, s) ->
              let c = s.[0] in
            if b then
              let c = Char.code c in
                      non_singleton (c,c)
            else
              let lower = Char.lowercase c in
              let lower = Char.code lower in
              let upper = Char.uppercase c in
              let upper = Char.code upper in
                union (non_singleton (lower, lower)) (non_singleton (upper, upper))
          | CharRange (x,y) -> non_singleton (x,y)
      | Seq(r1, e, l, r2) -> concat (loop r1) (loop r2) e l
      | Minus (r1,r2) -> difference (loop r1) (loop r2)
      | Alt(r1,r2) -> union (loop r1) (loop r2)
      | Star (Bounds(m,_), r1) ->
          let fs1 = loop r1 in
            if m > 0 then fs1
            else
              {nonempty = fs1.nonempty; epsilon = true; maybe_empty = fs1.maybe_empty; maybe_nonempty = fs1.maybe_nonempty}
      | Assign(r1,_,_) | Opt(r1) | Star (_,r1) ->
          let fs1 = loop r1 in
            {nonempty = fs1.nonempty; epsilon = true; maybe_empty = fs1.maybe_empty; maybe_nonempty = fs1.maybe_nonempty}
      | Rcount(_,r1)
      | Hash(_,r1) ->
          loop r1
  in
    loop r

(* function that determines if a nonterminal is recursive *)
let rec_graph gr first_map =
  let rec loop g n r = match r.r with
    | Symb(x,_,_,_) ->
        Tgraph.add_edge (Tgraph.add_node g x) n x
    | Action _
    | Position _
    | Box _
    | Delay _
    | Lit _
    | CharRange _
    | Prose _
    | When _
    | Lookahead _ ->
        g
    | Seq(r1,_,_,r2)
    | Minus(r1,r2)
    | Alt(r1,r2) ->
        loop (loop g n r1) n r2
    | Assign(r1,_,_)
    | Opt(r1)
    | Rcount(_,r1)
    | Star(_,r1)
    | Hash(_,r1) ->
        loop g n r1
  in
    Tgraph.tc
      (List.fold_left
         (fun result ->
            (function
                     RuleDef(n,r, _)->
                   loop (Tgraph.add_node result n) n r
               | _ -> result))
         Tgraph.empty
         gr.ds)

(* function that determines if a nonterminal is left recursive *)
let lrec_graph gr first_map =
  let rec loop g n r = match r.r with
    | Symb(x,_,_,_) ->
        Tgraph.add_edge (Tgraph.add_node g x) n x
    | Action _
    | Position _
    | Box _
    | Delay _
    | Lit _
    | CharRange _
    | Prose _
    | When _
    | Lookahead _ ->
        g
    | Seq(r1,_,_,r2) ->
        let fs = first r1 first_map in
          if not(fs_notempty fs.nonempty) || fs.epsilon || fs.maybe_empty <> []
          then loop (loop g n r1) n r2
          else
            loop g n r1
    | Minus(r1,r2)
    | Alt(r1,r2) ->
        loop (loop g n r1) n r2
    | Assign(r1,_,_)
    | Opt(r1)
    | Rcount(_,r1)
    | Star(_,r1)
    | Hash(_,r1) ->
        loop g n r1
  in
    Tgraph.tc
      (List.fold_left
         (fun result ->
            (function
                     RuleDef(n,r, _)->
                   loop (Tgraph.add_node result n) n r
               | _ -> result))
         Tgraph.empty
         gr.ds)

let is_rec n g = Tgraph.is_edge g n n

let print_fsmap first_map outc =
  let print_one name fs str =
    (str^name^":\n"^(print_fs fs))
  in
  let str = PMap.foldi print_one first_map "" in
    Printf.fprintf outc "\nFIRST sets:\n%s" str

let union_fls fls1 fls2 =
  let fls1cs,fls1ts = fls1 in
  let fls2cs,fls2ts = fls2 in
    union_cs fls1cs fls2cs, union_ts fls1ts fls2ts

let empty_fls () = Cs.empty (), []

let pr_fls fls =
  let flscs,flsts = fls in
    Printf.sprintf "char set: %s, token set: %s\n" (Cs.to_nice_string flscs) (pr_ts flsts)

let print_flsmap follow_map outc =
  let print_one name fls str =
    (str^name^":\n"^(pr_fls fls))
  in
  let str = PMap.foldi print_one follow_map "" in
    Printf.fprintf outc "\nFOLLOW sets:\n%s" str

let follow_gr gr first_map =
  let flag = ref false in
  let rec loop follow_map r cur_fls =
    match r.r with
      | Symb (n1, _, _, _) ->
          if PMap.mem n1 follow_map then
            let flsr = PMap.find n1 follow_map in
            let nfls = union_fls flsr cur_fls in
              if compare nfls flsr <> 0 then
                let _ = flag := true in
                let newmap = PMap.remove n1 follow_map in
                let newmap = PMap.add n1 nfls newmap in
                  newmap
              else
                follow_map
          else
            PMap.add n1 cur_fls follow_map
      | Box _
          | When _
      | Lookahead _
      | Action _
      | Position _
      | Delay _
      | Prose _
      | Lit _
          | CharRange _ -> follow_map
      | Alt (r1,r2) ->
          let newmap = loop follow_map r1 cur_fls in
          let newmap = loop newmap r2 cur_fls in
            newmap
      | Assign(r1,_,_)
      | Opt(r1)
      | Rcount(_,r1)
      | Hash(_,r1)
      | Star (_,r1) ->
          let newmap = loop follow_map r1 cur_fls in
            newmap
      | Seq(r1,_,_,r2) ->
          let newmap = loop follow_map r2 cur_fls in
          let fs2 = first r2 first_map in
          let newmap =
            if not(fs_notempty fs2.nonempty) || fs2.epsilon || fs2.maybe_empty <> [] then
              let nfls = union_fls cur_fls fs2.nonempty in
                loop newmap r1 nfls
            else loop newmap r1 fs2.nonempty
          in
            newmap
      | Minus(r1,r2) -> follow_map
  in
  let iter grdefs =
    let map = ref PMap.empty in
    let loopfunc follow_map rd =
      match rd with
        | RuleDef(n,r,a) ->
            let cur_fls = try PMap.find n follow_map with Not_found -> empty_fls () in
            let newmap = loop follow_map r cur_fls in
              if PMap.mem n newmap then
                newmap
              else (flag := true; PMap.add n cur_fls newmap)
        | _ -> follow_map
    in
    let _ = flag := true in
      while !flag do
        let _ = flag := false in
        let follow_map = List.fold_left loopfunc !map gr.ds in
          map := follow_map
      done;
      !map
  in
    iter gr

let tks_dstct ntl1 ntl2 =
  if ntl1 = [] || ntl2 = [] then
    false
  else
  let l = List.filter (fun nt -> List.mem nt ntl1) ntl2 in
    l = []

let fs_fs_dstct fs1 fs2 = (* in the token case, if one of fs is empty, the function will return false *)
  let fs1cs,fs1ts = fs1 in
  let fs2cs,fs2ts = fs2 in
    if Cs.count fs1cs = 0 && Cs.count fs2cs = 0 then (* only has ocamllex tokenizer *)
      match fs1ts,fs2ts with
          [(tknz1,ntl1)],[(tknz2,ntl2)] ->
            if String.compare tknz1 tknz2 = 0 then
              tks_dstct ntl1 ntl2,true
            else
              false,false
        | _ -> false,false
    else if fs1ts = [] && fs2ts = [] then (* no ocamllex tokenizer *)
      let x = Cs.dup fs1cs in
      let _ = Cs.intersect x fs2cs in
        Cs.count x = 0,false
    else
      false,false

let fs3dstct fs1 fs2 fs3 =
  let fs1cs,fs1ts = fs1 in
  let fs2cs,fs2ts = fs2 in
  let fs3cs,fs3ts = fs3 in
    if Cs.count fs1cs = 0 && Cs.count fs2cs = 0 && Cs.count fs3cs = 0 then (* only has ocamllex tokenizer *)
      match fs1ts,fs2ts,fs3ts with
          [(tknz1,ntl1)],[(tknz2,ntl2)],[(tknz3,ntl3)] ->
            if String.compare tknz1 tknz2 = 0 && String.compare tknz2 tknz3 = 0 && tks_dstct ntl1 ntl2 && tks_dstct ntl2 ntl3 && tks_dstct ntl3 ntl1 then
              true,true
            else
              false,false
        | _ -> false,false
    else if fs1ts = [] && fs2ts = [] && fs3ts = [] then (* no ocamllex tokenizer *)
      let x = Cs.dup fs1cs in
      let y = Cs.dup fs2cs in
      let z = Cs.dup fs3cs in
      let _ = Cs.intersect x fs2cs;Cs.intersect y fs3cs;Cs.intersect z fs1cs in
        Cs.count x = 0 && Cs.count y = 0 && Cs.count z = 0,false
    else
      false,false

let is_ll1 gr first_map follow_map n tokmap =
  let rec loop r cur_fls =
    match r.r with
      | Action _
      | When _
      | Box _
      | Symb _
      | CharRange _
      | Lit _
      | Delay _
      | Position _
      | Prose _
      | Lookahead _ -> true
      | Assign(r2,_,_)
      | Opt(r2)
      | Rcount(_,r2)
      | Hash(_,r2)
      | Star(_,r2) ->
          let fs = first r2 first_map in
          let fls = cur_fls in
          let is,flag = fs_fs_dstct fs.nonempty fls in
            if is && fs_notempty fs.nonempty && fs_notempty fls && fs.maybe_nonempty = [] && fs.maybe_empty = [] then
              true && (loop r2 cur_fls)
            else false
      | Seq (r2,_,_,r3) ->
          let fs = first r3 first_map in
          let new_fls =
            if not(fs_notempty fs.nonempty) || fs.epsilon || fs.maybe_empty <> [] then
              union_fls fs.nonempty cur_fls
            else fs.nonempty
          in
                (loop r2 new_fls) && (loop r3 cur_fls)
      | Alt (r2,r3) ->
          let fs2,fs3 = first r2 first_map, first r3 first_map in
            if fs2.maybe_empty <> [] || fs3.maybe_empty <> [] || fs2.maybe_nonempty <> [] || fs3.maybe_nonempty <> [] then
              false
            else begin
              match (fs2.epsilon,fs3.epsilon) with
                | (false,false) ->
                    let is,flag = fs_fs_dstct fs2.nonempty fs3.nonempty in
                      if is && fs_notempty fs2.nonempty && fs_notempty fs3.nonempty then
                        true && (loop r2 cur_fls) && (loop r3 cur_fls)
                      else false
                | (true,false) ->
                    let fls = cur_fls in
                    let fs2cs,fs2ts = fs2.nonempty in
                    let flscs,flsts = fls in
                    let fs2u = union_cs fs2cs flscs,union_ts fs2ts flsts in
                      if fs_notempty fls && fs_notempty fs3.nonempty then begin
                        let is,flag = fs_fs_dstct fs2u fs3.nonempty in
                          if is then true && (loop r2 cur_fls) && (loop r3 cur_fls)
                          else false
                      end
                      else false
                | (false,true) ->
                    let fls = cur_fls in
                    let fs3cs,fs3ts = fs3.nonempty in
                    let flscs,flsts = fls in
                    let fs3u = union_cs fs3cs flscs,union_ts fs3ts flsts in
                      if fs_notempty fls && fs_notempty fs2.nonempty then begin
                        let is,flag = fs_fs_dstct fs3u fs2.nonempty in
                          if is then true && (loop r2 cur_fls) && (loop r3 cur_fls)
                          else false
                      end
                      else false
                | (true,true) ->
                    let fls = cur_fls in
                    let is,flag = fs3dstct fs2.nonempty fs3.nonempty fls in
                      if is && fs_notempty fs2.nonempty && fs_notempty fs3.nonempty && fs_notempty fls then
                        true && (loop r2 cur_fls) && (loop r3 cur_fls)
                      else false
            end
      | Minus(r1,r2) -> true (* assume minus is in LL(1) *)
  in
  let init_fls = try PMap.find n follow_map with Not_found -> (Printf.eprintf "Warning: Unable to compute %s's FOLLOW set\n" n; Cs.empty (), []) in
    loop gr init_fls

let print_fs_simple fs =
  let nonemptystr = let cs,ts=fs.nonempty in Printf.sprintf "char set: %s, token set: %s" (Cs.to_nice_string cs) (pr_ts ts) in
  let epsilonstr = if fs.epsilon then "nullable" else "not nullable" in
    ("nonempty: "^nonemptystr^"\nepsilon: "^epsilonstr^"\n")

let report gr outc tokmap =
  let first_map = first_gr gr tokmap in
  let follow_map = follow_gr gr first_map in
  let recg = rec_graph gr first_map in
  let lrecg = lrec_graph gr first_map in
  let total = ref 0 in
  let n_ll1 = ref 0 in
  let n_rec = ref 0 in
  let n_lrec = ref 0 in
  let ll1list = ref [] in
  let reclist = ref [] in
  let lreclist = ref [] in
  let _ = Printf.fprintf outc "%s" "\n" in
  let print_one rd =
    match rd with
      | RuleDef(n,r,a) ->
          let first = try PMap.find n first_map with Not_found -> (Printf.eprintf "Warning: No record of %s's FIRST set\n" n; epsilon_singleton ()) in
          let follow = try PMap.find n follow_map with Not_found -> (Printf.eprintf "Warning: No record of %s's FOLLOW set\n" n; Cs.empty (), []) in
            total := !total + 1;
            Printf.fprintf outc "NONTERMINAL NAME: %s\n" n;
            if is_rec n recg then (n_rec := !n_rec + 1; reclist := !reclist@[n]; Printf.fprintf outc "%s, " "is recursive") else Printf.fprintf outc "%s, " "is not recursive";
            if is_rec n lrecg then (n_lrec := !n_lrec + 1; lreclist := !lreclist@[n]; Printf.fprintf outc "%s, " "is left recursive") else Printf.fprintf outc "%s, " "is not left recursive";
            if is_ll1 r first_map follow_map n tokmap then (n_ll1 := !n_ll1 + 1; ll1list := !ll1list@[n]; Printf.fprintf outc "%s\n" "is locally in LL(1)") else Printf.fprintf outc "%s\n" "is not locally in LL(1)";
            Printf.fprintf outc "FIRST:\n%s" (print_fs_simple first);
            Printf.fprintf outc "FOLLOW:\n%s" (pr_fls follow);
      | _ ->
          ()
  in
  let _ =  List.iter print_one gr.ds in
    Printf.fprintf outc "ANALYSIS STAT:\n%d/%d(%f%%) nonterminals are recursive\n" !n_rec !total (float_of_int(!n_rec)/.float_of_int(!total)*.100.00);
    Printf.fprintf outc "%d/%d(%f%%) nonterminals are left recursive\n" !n_lrec !total (float_of_int(!n_lrec)/.float_of_int(!total)*.100.00);
    Printf.fprintf outc "%d/%d(%f%%) nonterminals are locally in LL(1)\n" !n_ll1 !total (float_of_int(!n_ll1)/.float_of_int(!total)*.100.00);


end


module First_set_gil_lex = struct

type tokens = string * (string * (string option)) list  (* tokenizer, (ocaml_constructor,carried_type) list: store all tokens from the same tokenizer *)

type t = {nonempty: Cs.t * tokens list; (* first set (cs,ts), including single characters from default tokenizer (cs) and all tokens from ocaml lexer tokenizers (ts) *)
              maybe_nonempty: (Gil.rhs * Cs.t) list; (* guarded first set*)
              maybe_empty: Gil.rhs list; (* guarded nullable *)
              epsilon: bool; (* nullable *)
         }

let empty () = {nonempty = Cs.empty (),[];
                        epsilon = false;
                        maybe_empty = [];
                        maybe_nonempty = []}

let non_singleton (l, u) = {nonempty = Cs.range l (u+1),[];
                                        epsilon = false;
                                        maybe_empty = [];
                                        maybe_nonempty = []}

let non_singleton_tok tokenizer ocaml_constructor carried_type = {nonempty = Cs.empty (), [(tokenizer, [(ocaml_constructor,carried_type)])];
                                        epsilon = false;
                                        maybe_empty = [];
                                        maybe_nonempty = []}

let maybe_singleton r = {nonempty = Cs.empty (),[];
                                     epsilon = true;
                                     maybe_empty = [r];
                                     maybe_nonempty = []}

let epsilon_singleton () =  {nonempty = Cs.empty (),[];
                             epsilon = true;
                             maybe_empty = [];
                             maybe_nonempty = []}


(* function to get the sting of token list field if t.nonempty *)
let pr_ts ts =
  let printone (tokenizer, ntl) =
    Printf.sprintf "%s: %s\n" tokenizer (String.concat "," (List.map (fun (oc,ct) -> (Printf.sprintf "%s" oc)) ntl))
  in
    String.concat "\n" (List.map printone ts)

(* function to get the string of the first set of one nonterminal *)
let print_fs fs =
  let nonemptystr = let cs,ts=fs.nonempty in Printf.sprintf "cs: %s, ts: %s" (Cs.to_nice_string cs) (pr_ts ts) in
  let epsilonstr = if fs.epsilon then "nullable" else "not nullable" in
  let maybe_nonemptystr = if fs.maybe_nonempty == [] then "empty" else "not empty" in
  let maybe_emptystr = if fs.maybe_empty == [] then "empty" else "not empty" in
    ("nonempty: "^nonemptystr^"\nepsilon: "^epsilonstr^"\nmaybe_nonempty: "^maybe_nonemptystr^"\nmaybe_empty: "^maybe_emptystr^"\n")


(* helper functions that computes the union or concatenation of two first sets *)

let union_cs cs1 cs2 =
  let cs = Cs.dup cs1 in
    Cs.union cs cs2; cs

let union_ts ts1 ts2 =
  match ts1,ts2 with
    | [],[] -> []
    | [],s -> s
    | s,[] -> s
    | _ ->
  let merge_one (tokenizer, nontlist) =
    if List.mem_assoc tokenizer ts2 then
      let nontlist2 = List.assoc tokenizer ts2 in
        (tokenizer, nontlist@(List.filter (fun t2 -> not(List.mem t2 nontlist)) nontlist2))
    else
      (tokenizer, nontlist)
  in
    List.map merge_one ts1

let union fs1 fs2 =
  let fs1cs,fs1ts = fs1.nonempty in
  let fs2cs,fs2ts = fs2.nonempty in
  let cs = Cs.dup fs1cs in
    Cs.union cs fs2cs;
    {nonempty = cs,union_ts fs1ts fs2ts;
     epsilon = fs1.epsilon || fs2.epsilon;
    maybe_empty = fs1.maybe_empty @ fs2.maybe_empty;
    maybe_nonempty = fs1.maybe_nonempty @ fs2.maybe_nonempty}

let fs_notempty fs =
  let fscs,fsts = fs in
    Cs.count fscs <> 0 || fsts <> []

let concat fs1 fs2 =
  let fs1cs,fs1ts = fs1.nonempty in
  let fs2cs,fs2ts = fs2.nonempty in
  {nonempty = if not(fs_notempty fs1.nonempty) || fs1.epsilon || fs1.maybe_empty <> [] then
     (let cs = Cs.dup fs1cs in
        Cs.union cs fs2cs;
        cs,union_ts fs1ts fs2ts)
   else fs1.nonempty;
   epsilon = fs1.epsilon && fs2.epsilon;
     maybe_nonempty =
        if Cs.count fs2cs > 0 || fs2ts != [] then
              fs1.maybe_nonempty @
                List.map (fun r -> r, fs2cs) fs1.maybe_empty
        else fs1.maybe_nonempty;
     maybe_empty =
        if fs2.maybe_empty = [] then fs1.maybe_empty
        else
          let cross_comb l r =
            l@(List.map (fun r1 -> Gil.Seq(r,r1)) fs2.maybe_empty)
          in
            List.fold_left cross_comb [] fs1.maybe_empty
    }

let is_nonempty fs = (not fs.epsilon) && fs.maybe_empty = [] && fs.maybe_nonempty = []

(* search the list that stores the token info, given a constructor/type name, return the tokenizer, constructor(this is redundant, should be the same as the argument), and carried_type.  *)
let search_tokmap tokmap n =
  let search_tokenizer ret (tokenizer, lit_env) =
    match ret with
      | None ->
          if List.mem_assoc n lit_env then let (ocaml_constructor, carried_type) = List.assoc n lit_env in Some (tokenizer,ocaml_constructor,carried_type)
          else None
      | Some _ -> ret
  in
    List.fold_left search_tokenizer None tokmap

let pr_tokmap tokmap =
  let printone (tokenizer, lit_env) =
    Printf.sprintf "%s: %s\n" tokenizer (String.concat "," (List.map (fun (s1,s2) -> s1^" "^s2) lit_env))
  in
    String.concat "\n" (List.map printone tokmap)

let print_fsmap first_map =
  let print_one name fs str =
    (str^name^":\n"^(print_fs fs))
  in
  let str = PMap.foldi print_one first_map "" in
    str

(* the main function that computes the first sets for all nonterminal in the grammar gr. assume tokens are non-nullable and all tokens from the same tokenizer are non-ambiguous, which I think is true in ocamllex *)
let first_gr_gil_lex gr tokmap =
  let rec loop first_map r fv = (* fv stands for first visit. it is used to help determine if there's any change in the current pass *)
    match r with
      | Gil.Symb (n1, _, _) ->
          if PMap.mem n1 first_map then
            let fs1 = PMap.find n1 first_map in
              (fs1, true)
          else  (empty(), false)
          | Gil.When _
      | Gil.Lookahead _ -> if fv then (maybe_singleton r, false) else (maybe_singleton r, true)
      | Gil.Box _
      | Gil.Action _
      | Gil.Lit (_, "") ->
          if fv then
            (epsilon_singleton (), false)
          else
          (epsilon_singleton (), true)
          | Gil.Lit (b, s) ->
              let c = s.[0] in
            if b then
              let c = Char.code c in
                if fv then
                      (non_singleton (c,c), false)
                else
                  (non_singleton (c,c), true)
            else
              let lower = Char.lowercase c in
              let lower = Char.code lower in
              let upper = Char.uppercase c in
              let upper = Char.code upper in
              let fs = union (non_singleton (lower, lower)) (non_singleton (upper, upper)) in
                if fv then (fs, false) else (fs, true)
          | Gil.CharRange (x,y) -> if fv then (non_singleton (x,y), false) else (non_singleton (x,y), true)
      | Gil.Seq(r1,r2) ->
          let (fs1, has_change1) = loop first_map r1 fv in
          let (fs2, has_change2) = loop first_map r2 fv in
            if has_change1 || has_change2 then
              (concat fs1 fs2, true)
            else
              (concat fs1 fs2, false)
      | Gil.Alt(r1,r2) ->
          let (fs1, has_change1) = loop first_map r1 fv in
          let (fs2, has_change2) = loop first_map r2 fv in
            if has_change1 || has_change2 then
              (union fs1 fs2, true)
            else
              (union fs1 fs2, false)
      | Gil.Star r1 ->
          let (fs1, has_change1) = loop first_map r1 fv in
            ({nonempty = fs1.nonempty; epsilon = true; maybe_empty = fs1.maybe_empty; maybe_nonempty = fs1.maybe_nonempty}, has_change1)
  in
  let flag = ref false in
  let iter grdefs =
    let map = ref PMap.empty in
    let loopfunc first_map (n,r_gil) =
      let fv = PMap.mem n first_map in
      let (fs1, has_change) =
        match search_tokmap tokmap n with
          | None -> loop first_map r_gil fv
          | Some (tokenizer,ocamlconstructor,carried_type) -> (non_singleton_tok tokenizer ocamlconstructor carried_type, true)
      in
      let newmap =
        if has_change then
          if fv then
            let fs = PMap.find n first_map in
              if compare fs fs1 <> 0 then
                let newmap = PMap.remove n first_map in
                let newmap = PMap.add n fs1 newmap in
                let _ = flag := true in
                  newmap
              else
                first_map
          else
            (let newmap = PMap.add n fs1 first_map in let _ = flag := true in newmap)
        else first_map
      in
        newmap
    in
    let _ = flag := true in
      while !flag do
        let _ = flag := false in
        let first_map = List.fold_left loopfunc !map grdefs in
          map := first_map
      done;
      !map
  in
    iter gr

let pr_fls fls =
  let flscs,flsts = fls in
    Printf.sprintf "cs: %s, ts: %s\n" (Cs.to_nice_string flscs) (pr_ts flsts)

(* given the first_map that stores the pre-calculated first sets for all nonterminals in the grammar, return the first set of a particular rhs r. *)
let first_gil_lex r first_map =
  let rec loop r =
    match r with
      | Gil.Symb (n1, _, _) ->
          (try PMap.find n1 first_map with Not_found -> Printf.eprintf "Warning: Unable to compute %s's FIRST set." n1; empty ())
      | Gil.Box _
          | Gil.When _
      | Gil.Lookahead _ -> maybe_singleton r
      | Gil.Action _
      | Gil.Lit (_, "") -> epsilon_singleton ()
          | Gil.Lit (b, s) ->
              let c = s.[0] in
            if b then
              let c = Char.code c in
                      non_singleton (c,c)
            else
              let lower = Char.lowercase c in
              let lower = Char.code lower in
              let upper = Char.uppercase c in
              let upper = Char.code upper in
                union (non_singleton (lower, lower)) (non_singleton (upper, upper))
          | Gil.CharRange (x,y) -> non_singleton (x,y)
      | Gil.Seq(r1,r2) -> concat (loop r1) (loop r2)
      | Gil.Alt(r1,r2) -> union (loop r1) (loop r2)
      | Gil.Star r1 ->
          let fs1 = loop r1 in
            {nonempty = fs1.nonempty; epsilon = true; maybe_empty = fs1.maybe_empty; maybe_nonempty = fs1.maybe_nonempty}
  in
    loop r

(*
   the following part is all about follow set computing:
   a follow set of a nonterminal contains only the nonempty field in a first set, which includes a character set (cs) and a token list (ts), and which means there's no nullability info.
   the naming rule: fs usually means first set, while fls means follow set, but there're exceptions which is a little confusing...
*)


(* function that determines if a nonterminal is recursive *)
let rec_graph gr first_map =
  let rec loop g n r = match r with
    | Gil.Symb(x,_,_) ->
        Tgraph.add_edge (Tgraph.add_node g x) n x
    | Gil.Action _
    | Gil.Box _
    | Gil.Lit _
    | Gil.CharRange _
    | Gil.When _
    | Gil.Lookahead _ ->
        g
    | Gil.Seq(r1,r2)
    | Gil.Alt(r1,r2) ->
        loop (loop g n r1) n r2
    | Gil.Star r1 ->
        loop g n r1
  in
    Tgraph.tc
      (List.fold_left
         (fun result (n,r_gil) ->
            loop (Tgraph.add_node result n) n r_gil
         )
         Tgraph.empty
         gr)

let lrec_graph gr first_map =
  let rec loop g n r = match r with
    | Gil.Symb(x,_,_) ->
        Tgraph.add_edge (Tgraph.add_node g x) n x
    | Gil.Action _
    | Gil.Box _
    | Gil.Lit _
    | Gil.CharRange _
    | Gil.When _
    | Gil.Lookahead _ ->
        g
    | Gil.Seq(r1,r2) ->
        let fs = first_gil_lex r1 first_map in
          if not(fs_notempty fs.nonempty) || fs.epsilon || fs.maybe_empty <> []
          then loop (loop g n r1) n r2
          else
            loop g n r1
    | Gil.Alt(r1,r2) ->
        loop (loop g n r1) n r2
    | Gil.Star r1 ->
        loop g n r1
  in
    Tgraph.tc
      (List.fold_left
         (fun result (n,r_gil) ->
            loop (Tgraph.add_node result n) n r_gil
         )
         Tgraph.empty
         gr)

let is_rec n g = Tgraph.is_edge g n n

let union_fls fls1 fls2 =
  let fls1cs,fls1ts = fls1 in
  let fls2cs,fls2ts = fls2 in
    union_cs fls1cs fls2cs, union_ts fls1ts fls2ts

let empty_fls () = Cs.empty (), []

let print_flsmap follow_map =
  let print_one name fls str =
    (str^name^":\n"^(pr_fls fls))
  in
  let str = PMap.foldi print_one follow_map "" in
    str

(*
   the main function that computes the follow sets for all nonterminals in the grammar gr:
   the framework is the same as in the first set computing, it iterates until a fixed point is reached. the flags are all used to help determine if there's any change in the current pass.
   while computing follow set, there're two directions of propagation: assume r -> alpha r1
   follow set of r needs to be propagated to follow set of r1 and vice versa
*)
let follow_gr_gil_lex gr first_map =
  let flag = ref false in
  let rec loop follow_map r cur_fls =
    match r with
      | Gil.Symb (n1, _, _) ->
          if PMap.mem n1 follow_map then
            let flsr = PMap.find n1 follow_map in
            let nfls = union_fls flsr cur_fls in
              if compare nfls flsr <> 0 then
                let _ = flag := true in
                let newmap = PMap.remove n1 follow_map in
                let newmap = PMap.add n1 nfls newmap in
                  newmap
              else
                follow_map
          else
            PMap.add n1 cur_fls follow_map
      | Gil.Box _
          | Gil.When _
      | Gil.Lookahead _
      | Gil.Action _
      | Gil.Lit _
          | Gil.CharRange _ -> follow_map
      | Gil.Alt (r1,r2) ->
          let newmap = loop follow_map r1 cur_fls in
          let newmap = loop newmap r2 cur_fls in
            newmap
      | Gil.Star r1 ->
          let newmap = loop follow_map r1 cur_fls in
            newmap
      | Gil.Seq(r1,r2) ->
          let newmap = loop follow_map r2 cur_fls in
          let fs2 = first_gil_lex r2 first_map in
          let newmap =
            if not(fs_notempty fs2.nonempty) || fs2.epsilon || fs2.maybe_empty <> [] then
              let nfls = union_fls cur_fls fs2.nonempty in
                loop newmap r1 nfls
            else loop newmap r1 fs2.nonempty
          in
            newmap
  in
  let iter grdefs =
    let map = ref PMap.empty in
    let loopfunc follow_map (n,r_gil) =
      let cur_fls = try PMap.find n follow_map with Not_found -> empty_fls () in
      let newmap = loop follow_map r_gil cur_fls in
        if PMap.mem n newmap then
          newmap
        else (flag := true; PMap.add n cur_fls newmap)
    in
    let _ = flag := true in
      while !flag do
        let _ = flag := false in
        let follow_map = List.fold_left loopfunc !map grdefs in
          map := follow_map
      done;
      !map
  in
    iter gr

let tks_dstct ntl1 ntl2 =
  if ntl1 = [] || ntl2 = [] then
    false
  else
  let l = List.filter (fun nt -> List.mem nt ntl1) ntl2 in
    l = []

let fs_fs_dstct fs1 fs2 = (* in the token case, if one of fs is empty, the function will return false *)
  let fs1cs,fs1ts = fs1 in
  let fs2cs,fs2ts = fs2 in
    if Cs.count fs1cs = 0 && Cs.count fs2cs = 0 then (* only has ocamllex tokenizer *)
      match fs1ts,fs2ts with
          [(tknz1,ntl1)],[(tknz2,ntl2)] ->
            if String.compare tknz1 tknz2 = 0 then
              tks_dstct ntl1 ntl2,true
            else
              false,false
        | _ -> false,false
    else if fs1ts = [] && fs2ts = [] then (* no ocamllex tokenizer *)
      let x = Cs.dup fs1cs in
      let _ = Cs.intersect x fs2cs in
        Cs.count x = 0,false
    else
      false,false

let fs3dstct fs1 fs2 fs3 =
  let fs1cs,fs1ts = fs1 in
  let fs2cs,fs2ts = fs2 in
  let fs3cs,fs3ts = fs3 in
    if Cs.count fs1cs = 0 && Cs.count fs2cs = 0 && Cs.count fs3cs = 0 then (* only has ocamllex tokenizer *)
      match fs1ts,fs2ts,fs3ts with
          [(tknz1,ntl1)],[(tknz2,ntl2)],[(tknz3,ntl3)] ->
            if String.compare tknz1 tknz2 = 0 && String.compare tknz2 tknz3 = 0 && tks_dstct ntl1 ntl2 && tks_dstct ntl2 ntl3 && tks_dstct ntl3 ntl1 then
              true,true
            else
              false,false
        | _ -> false,false
    else if fs1ts = [] && fs2ts = [] && fs3ts = [] then (* no ocamllex tokenizer *)
      let x = Cs.dup fs1cs in
      let y = Cs.dup fs2cs in
      let z = Cs.dup fs3cs in
      let _ = Cs.intersect x fs2cs;Cs.intersect y fs3cs;Cs.intersect z fs1cs in
        Cs.count x = 0 && Cs.count y = 0 && Cs.count z = 0,false
    else
      false,false

let fssdstct fss =
  if List.for_all (fun (fscs,fsts) -> Cs.count fscs = 0) fss then
    let compareone (tag,(unioncs,unionts)) (fscs,fsts) =
      if tag then
        match unionts,fsts with
            [(tknz1,ntl1)],[(tknz2,ntl2)] ->
              if String.compare tknz1 tknz2 = 0 && tks_dstct ntl1 ntl2 then
                let newunion = union_cs unioncs fscs,union_ts unionts fsts in
                  (tag,newunion)
              else (false,(unioncs,unionts))
          | _ -> (false,(unioncs,unionts))
      else (tag,(unioncs,unionts))
    in
    let tag,_ = List.fold_left compareone (true,(List.hd fss)) (List.tl fss) in
      if tag then true,true
      else false,false
  else if List.for_all (fun (fscs,fsts) -> fsts = []) fss then
    let compareone (tag,(unioncs,unionts)) (fscs,fsts) =
      if tag then
        let x = Cs.dup unioncs in
        let _ = Cs.intersect x fscs in
        let newunion = union_cs unioncs fscs, union_ts unionts fsts in
          if Cs.count x = 0 then (tag,newunion)
          else (false,(unioncs,unionts))
      else (tag,(unioncs,unionts))
    in
    let tag,_ = List.fold_left compareone (true,List.hd fss) (List.tl fss) in
      if tag then true,false
      else false,false
  else false,false


let one_tokenizer fss =
  if List.for_all (fun (fscs,fsts) -> Cs.count fscs = 0) fss then
    let (_,ts) = List.hd fss in
    let (tokenizer,_) = List.hd ts in
      List.for_all (fun (_,unionts) -> match unionts with [(tokenizer1,ntl1)] -> String.compare tokenizer tokenizer1 = 0 | _ -> false) fss
  else false

let all_char fss = List.for_all (fun (fscs,fsts) -> fsts = []) fss

let is_ll1 r first_map follow_map n tokmap =
  let rec loop r cur_fls =
    match r with
      | Gil.Action _
      | Gil.When _
      | Gil.Box _
      | Gil.Symb _
      | Gil.CharRange _
      | Gil.Lit _
      | Gil.Lookahead _ -> true
      | Gil.Star(r2) ->
          let fs = first_gil_lex r2 first_map in
          let fls = cur_fls in
          let is,flag = fs_fs_dstct fs.nonempty fls in
            if is && fs_notempty fs.nonempty && fs_notempty fls && fs.maybe_nonempty = [] && fs.maybe_empty = [] then
              true && (loop r2 cur_fls)
            else false
      | Gil.Seq (r2, r3) ->
          let fs = first_gil_lex r3 first_map in
          let new_fls =
            if not(fs_notempty fs.nonempty) || fs.epsilon || fs.maybe_empty <> [] then
              union_fls fs.nonempty cur_fls
            else fs.nonempty
          in
                (loop r2 new_fls) && (loop r3 cur_fls)
      | Gil.Alt (r2, r3) ->
          let fs2,fs3 = first_gil_lex r2 first_map, first_gil_lex r3 first_map in
            if fs2.maybe_empty <> [] || fs3.maybe_empty <> [] || fs2.maybe_nonempty <> [] || fs3.maybe_nonempty <> [] then
              false
            else
              match (fs2.epsilon,fs3.epsilon) with
                | (false,false) ->
                    let is,flag = fs_fs_dstct fs2.nonempty fs3.nonempty in
                      if is && fs_notempty fs2.nonempty && fs_notempty fs3.nonempty then
                        true && (loop r2 cur_fls) && (loop r3 cur_fls)
                      else false
                | (true,false) ->
                    let fls = cur_fls in
                    let fs2cs,fs2ts = fs2.nonempty in
                    let flscs,flsts = fls in
                    let fs2u = union_cs fs2cs flscs,union_ts fs2ts flsts in
                      if fs_notempty fls && fs_notempty fs3.nonempty then begin
                        let is,flag = fs_fs_dstct fs2u fs3.nonempty in
                          if is then true && (loop r2 cur_fls) && (loop r3 cur_fls)
                          else false
                      end
                      else false
                | (false,true) ->
                    let fls = cur_fls in
                    let fs3cs,fs3ts = fs3.nonempty in
                    let flscs,flsts = fls in
                    let fs3u = union_cs fs3cs flscs,union_ts fs3ts flsts in
                      if fs_notempty fls && fs_notempty fs2.nonempty then begin
                        let is,flag = fs_fs_dstct fs3u fs2.nonempty in
                          if is then true && (loop r2 cur_fls) && (loop r3 cur_fls)
                          else false
                      end
                      else false
                | (true,true) ->
                    let fls = cur_fls in
                    let is,flag = fs3dstct fs2.nonempty fs3.nonempty fls in
                      if is && fs_notempty fs2.nonempty && fs_notempty fs3.nonempty && fs_notempty fls then
                        true && (loop r2 cur_fls) && (loop r3 cur_fls)
                      else false
  in
  let init_fls = try PMap.find n follow_map with Not_found -> (Printf.eprintf "Warning: Unable to compute %s's FOLLOW set\n" n; Cs.empty (), []) in
    loop r init_fls

end
