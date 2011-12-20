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
type constr = {cname: string; arity:int; cty: string}
type nonterminal = string
type expr = string

type 'expr boxnull =
    Always_null          (* box never accepts a non-empty string
                            (i.e. it accepts, at most, the empty-string) e.g., EOF, position *)
  | Never_null           (* box never accepts the empty string *)
  | Runbox_null          (* run box to determine if it accepts the empty string. Default. *)
  | Runpred_null of 'expr (* run separate predicate to determine if box accepts the empty string. *)

(* N.B. if the call field of Symbol = None, then the targeted
   nonterminal must not make any assumptions about its inflowing
   value; for example, that it is sv0. If you want to call a
   nonterminal that requires sv0, then pass sv0 as an explicit
   parameter. The correctness of call collapsing relies on this
   condition being met. *)
type ('expr,'nt) rhs =
    Symb of 'nt
      * 'expr option                 (* call function.  *)
         * 'expr option                 (* merge functiono *)
  | Lit of bool * string              (* true iff case sensitive *)
  | CharRange of int * int
  | Action of 'expr
  | Box of 'expr * 'expr boxnull
  | DBranch of 'expr                    (** generating expression *)
      * constr                          (** constructor information (means different things
                                            depending on whether late_branch_only is on) *)
      * 'expr                           (** branch function (unused when late_branch_only is on). *)
      (** A deterministic branch on a datatype value,
          annotated with its type's wrap constructor. *)
  | When of 'expr * 'expr
  | When_special of 'expr   (* Used for inlining nullability predicates. *)
  | Seq of ('expr, 'nt) rhs * ('expr, 'nt) rhs
  | Alt of ('expr, 'nt) rhs * ('expr, 'nt) rhs
  | Star of ('expr, 'nt) rhs
  | Lookahead of bool          (** flag indicating whether or not the rhs is
                                   expected to succeed. *)
      * ('expr, 'nt) rhs              (** Currently limited to character sets
                                   or argument-free symbols. *)

(** The standard [rhs] will have strings for both type parameters, so we abbreviate it here. *)
type s_rhs = (string, string) rhs

let charrange2alt low high =
  let rec loop i alt =
    if i <= high then loop (i+1) (Alt (alt, CharRange(i,i)))
    else alt in
  loop (low + 1) (CharRange(low,low))

(** Convert binary Alt representation to list of alternatives *)
let alts_of_rhs r =
  let rec loop l = function
  | Alt(r2,r3) -> loop (loop l r3) r2
  | r -> r::l in
  loop [] r

(** Convert binary Seq representation to a list *)
let seq2rules r =
  let rec loop l = function
  | Seq(r2,r3) -> loop (loop l r3) r2
  | r -> r::l in
  loop [] r

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

(** Rebuild elements that do not influence the ['nt] type parameter
    of the [rhs] so as to change the instantiation of the ['nt] parameter
    of the [rhs].

    Handles everything but Symb, Lookahead, Seq, Alt and Star.
 *)
let rebuild : 'a 'b. ('e,'a) rhs -> ('e,'b) rhs = function
  | Action f             -> Action f
  | Box (f, bn)          -> Box (f, bn)
  | When (f, g)          -> When (f, g)
  | When_special p       -> When_special p
  | Lit (b, s)           -> Lit (b, s)
  | CharRange (m, n)     -> CharRange (m, n)
  | DBranch (f1, c, f2)  -> DBranch (f1, c, f2)
  | (Symb _ | Lookahead _ | Seq _ | Alt _ | Star _) ->
      invalid_arg "Only always-polymorphic constructors are handled by rebuild"

let add_called_symbs =
  let rec recur s = function
  | Symb (nt, _, _) -> PSet.add nt s
  | ( Action _ | Box _ | When _ | When_special _ | Lit _
    | CharRange _ | Lookahead _ | DBranch _) -> s
  | ( Alt (r1, r2) | Seq (r1, r2) ) -> recur (recur s r1) r2
  | Star r1 -> recur s r1 in
  recur

let get_called_symbs r = add_called_symbs (PSet.create compare) r

let rec map f = function
  | ( Action _ | Symb _ | Box _
    | When _ | When_special _ | Lit _ | CharRange _ | Lookahead _ | DBranch _)
      as r -> f r
  | Alt (r1, r2) -> Alt (map f r1, map f r2)
  | Star r1 -> Star (map f r1)
  | Seq (r1, r2) -> Seq (map f r1, map f r2)

(** [ends_in_sem r] indicates whether [r] ends in a semantically relevant
    element: predicate, action, box, or symbol with binder. *)
let rec ends_in_sem = function
  | Action _
  | Symb (_, _, Some _)
  | Box _ | DBranch _
  | When _
  | When_special _ -> true
  | Symb (_, _, None) | Lit _ | CharRange _ | Alt _ | Star _ | Lookahead _ -> false
  | Seq (_, r) -> ends_in_sem r

let dependency_graph ds =
  let rec get_depend g n = function
    (* Add dependencies for n to a graph given definition r *)
  | Symb(x,_,_) ->
      Tgraph.add_edge (Tgraph.add_node g x) n x
  | When _ | When_special _ | Action _ | Box _ | DBranch _ | CharRange _ | Lit _ -> g
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

(** A special-version of [dependency_graph] in which negative
    lookahead is not counted, because it forms an unusual dependency. This
    can be relevant in checking whether a nonterminal's language is
    empty. *)
let dependency_graph_nnla ds =
  let rec get_depend g n = function
      (* Add dependencies for n to a graph given definition r *)
  | Symb(x,_,_) ->
      Tgraph.add_edge (Tgraph.add_node g x) n x
  | When _ | When_special _ | Action _ | Box _
  | Lookahead (false,_) | DBranch _ | CharRange _ | Lit _ -> g
  | Seq(r2,r3)
  | Alt(r2,r3) ->
      get_depend (get_depend g n r2) n r3
  | Star(r2) | Lookahead(true, r2) ->
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
      let in_order = List.rev (Tgraph.tsort g) in
      List.fold_left
        (fun (m, i) nt -> (PMap.add nt i m, i+1))
        (PMap.empty, 0)
        in_order in
    (fun (n1,_) (n2,_) ->
             compare (PMap.find n1 index_map) (PMap.find n2 index_map))
  in
  List.sort cmp ds

let remove_definition n ds =
  let rec loop p = function
    | ((n2, r) as x) :: ds -> if n = n2 then r, List.rev_append p ds else loop (x :: p) ds
    | [] -> raise Not_found in
  loop [] ds

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
  | Symb _ | Action _ | Box _ | When _ | When_special _ | Seq _
  | Star _ | Lookahead _ | DBranch _
      -> raise Not_character_set

let to_cs r =
  try Some (to_cs_rule r) with
      Not_character_set -> None

(*********************************************************************
 * Under construction.
 *********************************************************************)
module Meta = struct

  module PHOAS = Meta_prog.PHOAS
  module DBL = Meta_prog.DBL

  type 'nt rhs_phoas = (PHOAS.hoas_exp, 'nt) rhs
  type 'nt rhs_dbl = (DBL.exp, 'nt) rhs

  let boxnull_map f = function
    | Always_null -> Always_null
    | Never_null -> Never_null
    | Runbox_null -> Runbox_null
    | Runpred_null e -> Runpred_null (f e)

  let boxnull_s2p = boxnull_map PHOAS.inject_string
  let boxnull_p2s = boxnull_map PHOAS.to_string

  let rec map_expr f r =
    let g = function
      | Action e -> Action (f e)
      | Symb (nt, x, y) ->
          Symb (nt, Util.option_map f x,  Util.option_map f y)
      | Box (e, bn) -> Box (f e, boxnull_map f bn)
      | When (e1, e2) -> When (f e1, f e2)
      | When_special e -> When_special (f e)
      | Lit (x,y) -> Lit (x,y)
      | CharRange (x,y) -> CharRange (x,y)
      | Lookahead (x, r) -> Lookahead (x, map_expr f r)
      | DBranch (f1, c, f2) -> DBranch (f f1, c, f f2)
      | (Alt _ | Star _ | Seq _) ->
          invalid_arg "structural recursion should be handled by Gil.map" in
    map g r

  open Util.Operators

  let phoas_to_string r = map_expr PHOAS.to_string r
  let string_to_phoas r = map_expr PHOAS.inject_string r
  let phoas_simplify_to_string r = map_expr (PHOAS.to_string $ PHOAS.simplify) r

end

(*****************************************)
(* DeBruijn-levels based abstraction of Gil.

   Used specifically in nullability predicate generation.
   Included here because of dependencies issues.
*)

module DB_levels =
struct

  type exp =
    Var of int
  | App of exp * exp
  | Lam of exp
  | NoneE
  | SomeE of exp
  | CallE of string * exp * exp
  | AndE of exp * exp
  | StarE of exp
  | OrE of exp * exp
  | InjectE of string
  | CfgLookaheadE of bool * string (** context-free lookahead*)
  | CsLookaheadE of bool * Cs.t
  | DBranchE of string * constr * string

  let app2 f x y = App (App (f,x), y)
  let app3 f x y z = App (App (App (f, x), y), z)

  let exp_map f e =
    let rec walk c = function
      | Var i -> f c i
      | Lam f -> Lam (walk (c+1) f)
      | App (e1, e2) -> App (walk c e1, walk c e2)
      | NoneE -> NoneE
      | SomeE e -> SomeE (walk c e)
      | StarE e -> StarE (walk c e)
      | CallE (nt,e1,e2) -> CallE (nt, walk c e1, walk c e2)
      | AndE (e1, e2) -> AndE (walk c e1, walk c e2)
      | OrE (e1, e2) -> OrE (walk c e1, walk c e2)
      | InjectE s -> InjectE s
      | DBranchE (f1, c, f2) -> DBranchE (f1, c, f2)
      | CfgLookaheadE (b,n) -> CfgLookaheadE (b,n)
      | CsLookaheadE (b,cs) -> CsLookaheadE (b,cs)
    in
    walk 0 e

  (** Adjust the numbering of bound variables so that the expression
      can be placed in a new context. Free variables remain
      unchanged. Note that we abstract over the distinction between
      free and bound by taking it as an argument, [mf]. So, what we
      call "free" is actually relatively free, with respect to [mf].

      mf = the maximum free variable.
      n = the amount by which to shift.

      For example, if we want to place [e] in a context with two more
      outer lambdas than the current context, we need to increase the
      value of all bound variables by two, because their binders are about to
      be forced deeper into the tree.

      The result expression will have minimum bound variable of mf + 1 + n
      (which is equivalent to maximum free variable of mf + n).

      (A note regarding the wildcard on the first argument of the
      function supplied to [exp_map]. This argument, [c], is the count
      of binders traversed during the map process.
      Now, every variable should be in 0..c + mf, because free variables should only
      be taken from 0..mf and variables i, mf < i <= c + mf, are bound variables.
      So, we can ignore c.)
  *)
  let shift mf n e = exp_map (fun _ i_var -> if mf < i_var then Var (i_var + n) else Var i_var) e

  (**
      subst m e_s e ==  [m -> e_s]e

      e_s and e must be numbered relative to the same maximum free variable m.  That is,
      variables <= m are free for both e_s and e.
      We simplify to assume that we are substituting for the greatest free variable in e,
      namely m.
  *)
  let subst mf e_s e = exp_map (fun c i -> if i = mf then (shift mf c e_s) else Var i) e

  (* e1 has an mf of k, while e2 has an mf of k-1. So, we shift e2 up to equalize the mf,
     and then shift down the result, because we've eliminated free variable k.
  *)
  let beta_reduce k e1 e2 = shift k (-1) (subst k (shift (k-1) 1 e2) e1)

  (** Beta-reduce where possible *)
  let simplify e =
    let rec simplify' c = function  (* c is the count of binders *)
      | App (e1, e2) ->
          let e1 = simplify' c e1 in
          let e2 = simplify' c e2 in
          (match e1 with
             | Lam e -> simplify' c (beta_reduce c e e2)
             | _ -> App (e1, e2))
      | Var i -> Var i
      | Lam f -> Lam (simplify' (c+1) f)
      | NoneE -> NoneE
      | SomeE e -> SomeE (simplify' c e)
      | StarE e -> StarE (simplify' c e)
      | AndE (e1, e2) -> AndE (simplify' c e1, simplify' c e2)
      | OrE (e1, e2) ->  OrE (simplify' c e1, simplify' c e2)
      | InjectE s -> InjectE s
      | DBranchE (f1, c, f2) -> DBranchE (f1, c, f2)
      | CfgLookaheadE (b,n) -> CfgLookaheadE (b,n)
      | CsLookaheadE (b,cs) -> CsLookaheadE (b,cs)
      | CallE (nt,e1,e2) -> CallE (nt, simplify' c e1, simplify' c e2)
    in simplify' 0 e

  (** check that the expression has at most [n] free variables.*)
  let rec check_free_at_most n = function
    | Var i -> i < n
    | Lam f -> check_free_at_most (n+1) f
    | App (e1, e2) -> check_free_at_most n e1 && check_free_at_most n e2
    | NoneE -> true
    | SomeE e | StarE e -> check_free_at_most n e
    | CallE (nt,e1,e2) -> check_free_at_most n e1 && check_free_at_most n e2
    | AndE (e1, e2) -> check_free_at_most n e1 && check_free_at_most n e2
    | OrE (e1, e2) -> check_free_at_most n e1 && check_free_at_most n e2
    | InjectE _ -> true
    | DBranchE _ -> true
    | CfgLookaheadE _ -> true
    | CsLookaheadE _ -> true

  let is_closed = check_free_at_most 0

  (** Note any called nonterminals *)
  let rec note_called s = function
    | InjectE _
    | DBranchE _
    | CfgLookaheadE _
    | CsLookaheadE _
    | Var _
    | NoneE
    | SomeE _ -> s
    | Lam f -> note_called s f
    | AndE (e1, e2)
    | OrE (e1, e2)
    | App (e1, e2) -> note_called (note_called s e1) e2
    | StarE e -> note_called s e
    | CallE (nt,_,_) -> PSet.add nt s

end

