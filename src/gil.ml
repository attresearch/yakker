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

open Yak
type nonterminal = string
type expr = string

type boxnull =
    Always_null          (* box always accepts null, e.g., position *)
  | Never_null           (* box never accepts null *)
  | Runbox_null          (* run box to determine if it accepts null *)
  | Runpred_null of expr (* run separate predicate to determine if box accepts null *)

type rhs =
    Symb of nonterminal * expr option * expr option
  | Lit of bool * string              (* true iff case sensitive *)
  | CharRange of int * int
  | Action of expr
  | Box of expr * boxnull
  | When of expr * expr
  | Seq of rhs * rhs
  | Alt of rhs * rhs
  | Star of rhs
  | Lookahead of bool * rhs

let charrange2alt low high =
  let rec loop i alt =
    if i <= high then loop (i+1) (Alt (alt, CharRange(i,i)))
    else alt in
  loop (low + 1) (CharRange(low,low))

(** Convert binary Alt representation to list of alternatives *)
let alt2rules =
  let rec loop l = function
  | Alt(r2,r3) -> loop (loop l r3) r2
  | r -> r::l in
  loop []

(** Convert binary Seq representation to a list *)
let seq2rules =
  let rec loop l = function
  | Seq(r2,r3) -> loop (loop l r3) r2
  | r -> r::l in
  loop []

(** Turn a list into a sequence. Not tail-recursive. *)
let rec mkSEQ = function
  | [] -> Lit (true, "")
  | [r] -> r
  | r::rs -> Seq(r, mkSEQ rs)

(** Turn a list into a sequence, reversing the elements in the process. Tail-recursive. *)
let rec mkSEQ_rev = function
  | [] -> Lit (true, "")
  | [r] -> r
  | r::rs -> List.fold_left (fun r_acc r -> Seq(r, r_acc)) r rs

(** [ends_in_sem r] indicates whether [r] ends in a semantically relevant
    element: predicate, action, box, or symbol with binder. *)
let rec ends_in_sem = function
  | Action _
  | Symb (_, _, Some _)
  | Box _
  | When _ -> true
  | Symb (_, _, None) | Lit _ | CharRange _ | Alt _ | Star _ | Lookahead _ -> false
  | Seq (_, r) -> ends_in_sem r

let dependency_graph ds =
  let rec get_depend g n = function
    (* Add dependencies for n to a graph given definition r *)
  | Symb(x,_,_) ->
      Tgraph.add_edge (Tgraph.add_node g x) n x
  | When _ | Action _ | Box _ | CharRange _ | Lit _ -> g
  | Seq(r2,r3)
  | Alt(r2,r3) ->
      get_depend (get_depend g n r2) n r3
  | Star(r2) | Lookahead(_,r2) ->
      get_depend g n r2
  in
  List.fold_left
    (fun result (n,r) -> get_depend (Tgraph.add_node result n) n r)
    Tgraph.empty
    ds

let sort_definitions ds =
  let cmp =
    let (index_map,_) = (* Maps nonterminals to indices 0..n in topological sort order *)
      let g = dependency_graph ds in
      let in_order = List.rev(Tgraph.tsort g) in
      List.fold_left
        (fun (m,i) n -> (PMap.add n i m, i+1))
        (PMap.empty,0)
        in_order in
    (fun (n1,_) (n2,_) ->
             compare (PMap.find n1 index_map) (PMap.find n2 index_map))
  in
  List.sort cmp ds


(*********************************************************************
 * character-set analysis
 *********************************************************************)

exception Not_character_set

let rec to_cs_rule = function
  | Lit(case_sensitive,s) ->
      let len = String.length s in
      if len <> 1 then raise Not_character_set else
      let c = String.get s 0 in
      if case_sensitive then
        let result = Cs.empty() in
        Cs.insert result (Char.code c);
        result
      else
        let lower = Char.code(Char.lowercase c) in
        let upper = Char.code(Char.uppercase c) in
        let result = Cs.empty() in
        Cs.insert result lower;
        Cs.insert result upper;
	result
  | CharRange(min,max) ->
      Cs.range min (max+1)
  | Alt(r2,r3) ->
      let x = to_cs_rule r2
      and y = to_cs_rule r3 in
      let result = Cs.dup x in
      Cs.union result y;
      result
  | Symb _ | Action _ | Box _ | When _ | Seq _
  | Star _ | Lookahead _
      -> raise Not_character_set

let to_cs r =
  try Some (to_cs_rule r) with
      Not_character_set -> None
