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

let lookahead_name = "__lookahead"
let nullable_pred_prefix = "nullable_"

let mk_var c = Printf.sprintf "_x%d_" c
let mk_pvar c = Printf.sprintf "_p%d_" c
let mk_npname n = Util.bnf2ocaml (nullable_pred_prefix ^ n)

let callc symb_pred action binder =
  (* The generated code binds expression to avoid capture. There are
     no (possibly) open expressions in the body of the generated
     code. *)
  Printf.sprintf "(let symb_pred = %s
       and action = %s
       and binder = %s
    in
    fun la v ->
     match symb_pred la (action v) with
	None -> None
      | Some v2 -> Some (binder v v2))" symb_pred action binder

let gil_callc symb_pred action binder =
  (* The generated code binds expression to avoid capture. There are
     no (possibly) open expressions in the body of the generated
     code. *)
  Printf.sprintf "(let symb_pred = %s
       and f_call = %s
       and f_ret = %s
    in
    fun la ykb v ->
     let p = YkBuf.get_offset ykb in
     match symb_pred la ykb (f_call p v) with
	None -> None
      | Some v2 -> Some (f_ret p v v2))" symb_pred action binder


(*****************************************)
(* DeBruijn Levels *)

module DB_levels =
struct

  (**
      General DeBruin-levels machinery
  *)

  type exp =
    Var of int
  | App of exp * exp
  | Lam of exp
  | NoneE
  | SomeE of exp
  | CallE of string * exp * exp
  | AndE of exp * exp
  | OrE of exp * exp
  | InjectE of string
  | CfgLookaheadE of bool * string (** context-free lookahead*)
  | CsLookaheadE of bool * Cs.t

  let app2 f x y = App (App (f,x), y)
  let app3 f x y z = App (App (App (f, x), y), z)

  let exp_map f e =
    let rec walk c = function
      | Var i -> f c i
      | Lam f -> Lam (walk (c+1) f)
      | App (e1, e2) -> App (walk c e1, walk c e2)
      | NoneE -> NoneE
      | SomeE e -> SomeE (walk c e)
      | CallE (nt,e1,e2) -> CallE (nt, walk c e1, walk c e2)
      | AndE (e1, e2) -> AndE (walk c e1, walk c e2)
      | OrE (e1, e2) -> OrE (walk c e1, walk c e2)
      | InjectE s -> InjectE s
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
      | AndE (e1, e2) -> AndE (simplify' c e1, simplify' c e2)
      | OrE (e1, e2) ->  OrE (simplify' c e1, simplify' c e2)
      | InjectE s -> InjectE s
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
    | SomeE e -> check_free_at_most n e
    | CallE (nt,e1,e2) -> check_free_at_most n e1 && check_free_at_most n e2
    | AndE (e1, e2) -> check_free_at_most n e1 && check_free_at_most n e2
    | OrE (e1, e2) -> check_free_at_most n e1 && check_free_at_most n e2
    | InjectE _ -> true
    | CfgLookaheadE _ -> true
    | CsLookaheadE _ -> true

  let is_closed = check_free_at_most 0

  let rec to_string_raw c = function
    | Lam f ->
	Printf.sprintf "(lam. %s)" (to_string_raw (c+1) f)
    | Var x -> if x < c then Printf.sprintf "%d" x else Printf.sprintf "'%d" x
    | App (e1,e2) -> Printf.sprintf "(%s %s)" (to_string_raw c e1) (to_string_raw c e2)
    | NoneE -> "None"
    | SomeE e -> Printf.sprintf "(Some %s)" (to_string_raw c e)
    | AndE (e1,e2) -> Printf.sprintf "(Pred.andc %s %s)" (to_string_raw c e1) (to_string_raw c e2)
    | OrE (e1,e2) -> Printf.sprintf "(Pred.orc %s %s)" (to_string_raw c e1) (to_string_raw c e2)
    | CallE (nt,e1,e2) -> callc (mk_npname nt) (to_string_raw c e1) (to_string_raw c e2)
    | InjectE s -> Printf.sprintf "(%s)" s
    | CfgLookaheadE (b,n) -> Printf.sprintf "(Pred.lookaheadc %B 0 %s)" b n
    | CsLookaheadE (b,cs) -> Printf.sprintf "(Pred.lookaheadc %B 0 %s)" b (Cs.to_nice_string cs)


  (** Note any called nonterminals *)
  let rec note_called s = function
    | InjectE _
    | CfgLookaheadE _
    | CsLookaheadE _
    | Var _
    | NoneE
    | SomeE _ -> s
    | Lam f -> note_called s f
    | AndE (e1, e2)
    | OrE (e1, e2)
    | App (e1, e2) -> note_called (note_called s e1) e2
    | CallE (nt,_,_) -> PSet.add nt s

  (**
      Predicate-specific machinery
  *)

  let false_e = Lam (Lam NoneE)

  let is_bool = function
      Lam Lam NoneE | Lam Lam SomeE Var 1 -> true
    | _ -> false

  let rewrite preds e =
    let rec rewrite' c = function  (* c is the count of binders *)
      | AndE (e1, e2) ->
	  let e1 = rewrite' c e1 in
	  let e2 = rewrite' c e2 in
	  (match (e1, e2) with
	     | (Lam (Lam NoneE), _) -> false_e
	     | (_, Lam (Lam NoneE)) -> false_e
	     | (Lam (Lam (SomeE e)), _) ->
		 let e2_s = shift (c-1) 2 e2 in
		 Lam (Lam (app2 e2_s (Var c) e))
	     | _ -> AndE (e1, e2))
      | OrE (e1, e2) ->
	  let e1 = rewrite' c e1 in
	  let e2 = rewrite' c e2 in
	  (match (e1,e2) with
	       (Lam (Lam (SomeE _)), _) -> e1
	     | (_, Lam (Lam (SomeE _))) -> e2
	     | (Lam (Lam NoneE), _) -> e2
	     | _ -> OrE (e1, e2))
      | CallE (nt,e1,e2) as e_orig ->
	  let nt_pred = preds nt in
	  (match nt_pred with
	       Lam (Lam NoneE) -> false_e
	     | Lam (Lam (SomeE e)) ->
		 (* the "current" variable is c (which corresponds to the first binder),
		    so we use c+1 for v, which is the second binder.. *)
		 let v_var = Var (c+1) in
		 (* shift e1 and e2 by 2 b/c we are putting them under two lambdas.
		    We don't shift e b/c it comes from under a lambda. *)
		 let e1_s = shift (c-1) 2 e1 in
		 let e2_s = shift (c-1) 2 e2 in
		 Lam (Lam (SomeE( app2 e2_s v_var
			       (subst (c+1) (App (e1_s, v_var)) e)
			   )
		     ))
	     | _ -> e_orig)


       (* This analysis is run to completion at which point we (hope to) have a guarantee
	  that anything undetermined is

	  a) empty, or
	  b) must be checked dynamically (still possibly empty).

	  If it is labeled dynamic, then we know that it is not empty, but its actual contents must
	  still be calculated dynamically. What about lookahead? Lookahead should, by this definition,
	  be labeled as undetermined. However, I'm not sure that is what we're going for. Because, we *really*
	  just want to know which elements need to be *memoized* dynamically, to avoid nontermination,
	  and which do not. Lookahead should fall into the latter category. So, it should really be
	  categorized as "requires_memo".

	  Important point: this analysis is checking symbols S, "for all parameters of S". That is,
	  it is checking what (if any) conclusions can be drawn about S in a general way.

	  ******

	  We are starting with a least-fix point definition of nonterminal languages. We are then asking
	  the question "is epsilon in the language of A(e)?". We should be able to define an algorithm for
	  answering that question based on the definition of the language of A(e). The division between
	  static computation and dynamic computation is unimportant at this point. We will ignore, for the
	  moment, the issue of lookahead.

	  I think that the first question I have is what is the definition of a sound & complete algorithm
	  for computing said fixpoint? Or, for that matter, does a terminating algorithm exist? Of course not,
	  because the languages can be infinite. However, we *might* be able to find a terminating algorithm which
	  checks whether epsilon is in the language. Indeed, our Earley algorithm is just that (under the constraint
	  that nonterminals have finite domains).

	  But, we don't *want* to run a specialized Earley. The whole point is to avoid just that. So, we'd like
	  to find a "better" algorithm which is is still sound/complete w.r.t. our nondeterministic semantics.
	  Cool. Furthermore, that algorithm is specialized to the epsilon input. That said, it might still be
	  constructive to consider how Earley computes the least sets satisfying the nondeterministic rules.

	  We can start by reformulating the rules as judgments specific to epsilon. Perhaps, then, the answer
	  will just fall out in an obvious way.

	  I think that our first priority should be to find a terminating algorithm (assuming one can be found).
	  Then we can worry about the static/dynamic split and where that comes into play. It might be helpful to
	  rewrite my rewrite rules as judgments. Then, we could prove them sound and complete w.r.t. to the
	  grammar semantics and derive an algorithm for them (which I imagine will be an iterate-to-fixpoint
	  algorithm).


	  C |- r1 = \v. Some e1    C |- r2 = \v. Some e2
	  --------------------------------------
          C |- r1 r2 = \v. Some (...)

          C (A(v)) = e    e' -->* v
          -------------------------
          C |- A(e') = e


	  Start with C |- A(v) = false, for all A in dom(G) and v in dom(A). Notice that we are assuming all
	  A to have finite domain.

	  ### Version 2: Lazy tabling.

	  While the above approach works, it requires us to populate the table with every possible A(v), which could
	  be awfully expensive, not to mention wasteful. It also means that we'll need to run the analysis for every
	  such A(v). Instead, we could table lazily -- that is, perform the analysis for a particular A(v) only upon
	  demand.  By "upon demand," we refer to runtime demand. We start the computation with a specific A,v pair
	  and perform the rules, adding other A,v pairs to the table only when we encounter them, and iterating to
	  a fix point would be over the domain of the table (as opposed to the domain of the grammar). We would only
	  have to record a value for every A,v pair in the worst case. Moreover, in
	  circumstances for which not all A have finite domain, a lazy approach could work where an eager
	  one cannot.

	  ### Version 3: Static/dynamic split

	  In version 2, we have a working algorithm, but we might worry about its performance. We are running a
	  relatively expensive analysis at run time. Can we precompute some reasonable amount of entries eagerly,
	  or perform some other precomputation that would potentially speed up the run time computation? Here
	  are three points to consider:

	  1) We can precompute as many entries as we like. Indeed, when none of the nonterminals of parameterized,
	     solving every nonterminal is like tabling every nonterminal at the unit-value argument.

	  2) For some parameterized nonterminals, we can precompute their answer *independently* of their parameter.
	     We can consider this a compact way of precomputing the values of such nonterminals at every parameter.

	  3) Even for nonterminals whose status depends on their parameters, we can compile a more compact
	     representation of the computation which needs to take place at run time. Notice that if we consider
	     the analysis as a function F having two arguments, a RHS r and an argument v, then this compilation
	     is like the partial evaluation of `F r`.

	  The key point from the perspective of efficiency is just that we ensure that anything computed statically
	  not be involved in the iteration to fixpoint dynamically. I believe we do this in the same way as the
	  original code: evaluate to an expression. If the expression is a value -- great; otherwise, it will be
	  executed dynamically. The key update to the old version is that we must now memoize nonterminal calls when
	  we can't determine their value statically.

	  Yet, if we're saving some things for dynamic evaluation how do we know when our answer is the correct one?
	  How do we know when we are done? I think that a conservative approximation is to mark values as sure/unsure.
	  A value is sure iff it depends on only sure values. Otherwise, it is unsure. Then, we initialize all
	  nonterminals to `unsure (false)`.  As sure values are computed, they flip to sure. Anything remaining unsure
	  once fixpoint is reached will be computed dynamically.

	  ### Version 4: Recursion instead of iteration

	  An alternative to the sure/unsure approach is to guarantee that one pass is enough. That is, find an
	  evaluation sequence which will return the fixpoint after a single evaluation.  I believe there are two
	  (essentially identical) approaches which can accomplish this goal. First, precompute dependencies between
	  nonterminals and then evaluate from leaves to root, prepopulating all values with `false` to handle recursive
	  nonterminals. Second, replace iteration to fixpoint with a memoizing, recursive algorithm.

	  Here's why I think these approaches are succesful after one pass: the result values
	  can only increase and there are only two values. So, if we start with "false" and end up with "true" after
	  one recursive evaluation, we must have reached a fixpoint.

	  That said, some care must be taken in dealing with semantic values, because then we have None and Some v,
	  which represent more than two values. However, I think that the constraint that alts must have the same
	  semantic value is enough to ensure that are is only one possible `v` for which the function evaluates to
	  `Some v`. My reasoning is that in order for a recursive nonterminal to include epsilon, epsilon must appear
	   on some branch which *doesn't* include a recursive case. So, the fixpoint value shouldn't affect the final
	  outcome.

	  ### Memoization

	  Regarding memoization, we have proposed its use to avoid nontermination. however, it can also be useful
	  for sharing. Do we want to only use it for the former purpose, or just use it everywhere?

          ### Conclusion

	  In order to minimize code change, will take the dependency + iteration approach for static computations
	  and the recursion + memo-everywhere approach for generated predicates (where "everywhere" means all predicates
	  which are called by other predicates (after the analysis)). Hopefully, memoization will not be used often. Still,
	  I'm concerned about performance (time + memory) implications.  We will have to revisit this at some point to test affect.

	  Worth testing because we correctness only requires we memoize recursive predicates. So, if performance is an issue we
	  could see what happens if we only memoize recursive predicates, instead of all called predicates.
       *)

      | Lam f -> Lam (rewrite' (c+1) f)
      | App (e1, e2) -> App (rewrite' c e1, rewrite' c e2)
      | InjectE s -> InjectE s
      | Var i -> Var i
      | CfgLookaheadE (b,n) -> CfgLookaheadE (b,n)
      | CsLookaheadE (b,cs) -> CsLookaheadE (b,cs)
      | NoneE -> NoneE
      | SomeE e -> SomeE e

  in
    rewrite' 0 e

end


module DBL = DB_levels

(*****************************************)
(* PHOAS *)
module PHOAS = struct
  type 'a exp =
    Var of 'a
  | App of 'a exp * 'a exp
  | Lam of ('a -> 'a exp)
  | NoneE
  | SomeE of 'a exp
  | CallE of string * 'a exp * 'a exp
  | AndE of 'a exp * 'a exp
  | OrE of 'a exp * 'a exp
  | InjectE of string
  | CfgLookaheadE of bool * string
  | CsLookaheadE of bool * Cs.t

  type hoas_exp = {e:'a. unit -> 'a exp}
  type open1_hoas_exp = {e_open1:'a. 'a -> 'a exp}
  type open2_hoas_exp = {e_open2:'a. 'a -> 'a -> 'a exp}
  type open3_hoas_exp = {e_open3:'a. 'a -> 'a -> 'a -> 'a exp}

  (** Sugar for a let expression. An alternative to actually applying. 
      Useful for promoting sharing of results (given the CBV semantics. *)
  let let_e e1 e2 = App (e2, e1)

  let rec to_dB level = function
    | Var i -> DBL.Var i
    | Lam f -> DBL.Lam (to_dB (level+1) (f level))
    | App (e1, e2) -> DBL.App (to_dB level e1, to_dB level e2)
    | NoneE -> DBL.NoneE
    | SomeE e -> DBL.SomeE (to_dB level e)
    | CallE (nt,e1,e2) -> DBL.CallE (nt, to_dB level e1, to_dB level e2)
    | AndE (e1, e2) -> DBL.AndE (to_dB level e1, to_dB level e2)
    | OrE (e1, e2) -> DBL.OrE (to_dB level e1, to_dB level e2)
    | InjectE s -> DBL.InjectE s
    | CfgLookaheadE (b,n) -> DBL.CfgLookaheadE (b,n)
    | CsLookaheadE (b,cs) -> DBL.CsLookaheadE (b,cs)

  (* val subst' : 'a exp exp -> 'a exp *)
  let rec subst' = function
    | Var e -> e
    | Lam f -> Lam (fun x -> subst' (f (Var x)))
    | App (e1, e2) -> App (subst' e1, subst' e2)
    | NoneE -> NoneE
    | SomeE e -> SomeE (subst' e)
    | CallE (nt,e1,e2) -> CallE (nt, subst' e1, subst' e2)
    | AndE (e1, e2) -> AndE (subst' e1, subst' e2)
    | OrE (e1, e2) -> OrE (subst' e1, subst' e2)
    | InjectE s -> InjectE s
    | CfgLookaheadE (b,n) -> CfgLookaheadE (b, n)
    | CsLookaheadE (b,cs) -> CsLookaheadE (b, cs)

  let subst e1 e2 = {e = fun () -> subst' (e1.e_open1 (e2.e ()))}
  let subst2 e1 e2 e3 = {e = fun () -> subst' ( e1.e_open2 (e2.e ()) (e3.e ()) )}

  (** Close an open expression with a variable. Useful for lifting the 
      "openness" of an expression out into a surrounding expression. *)
  let vsub e v = subst' (e.e_open1 (Var v))
  let vsub2' e v1 v2 = subst' (e (Var v1) (Var v2))
  let vsub2 e v1 v2 = vsub2' e.e_open2 v1 v2

  let convert_to_dB {e=f} = to_dB 0 (f())

  let get_var (n,elts) i = List.nth elts (n - i - 1)
  let extend_ctxt (n,elts) x = (n+1,x::elts)
  let empty_ctxt () = (0,[])

  let rec from_dB ctxt = function
    | DBL.Var i -> Var (get_var ctxt i)
    | DBL.Lam f -> Lam (fun x -> from_dB (extend_ctxt ctxt x) f)
    | DBL.App (e1, e2) -> App (from_dB ctxt e1, from_dB ctxt e2)
    | DBL.NoneE -> NoneE
    | DBL.SomeE e -> SomeE (from_dB ctxt e)
    | DBL.CallE (nt,e1,e2) -> CallE (nt, from_dB ctxt e1, from_dB ctxt e2)
    | DBL.AndE (e1, e2) -> AndE (from_dB ctxt e1, from_dB ctxt e2)
    | DBL.OrE (e1, e2) -> OrE (from_dB ctxt e1, from_dB ctxt e2)
    | DBL.InjectE s -> InjectE s
    | DBL.CfgLookaheadE (b,n) -> CfgLookaheadE (b,n)
    | DBL.CsLookaheadE (b,cs) -> CsLookaheadE (b,cs)

  (** Convert a closed expression in DeBruijn levels 
      representation to PHOAS. *)
  let convert_from_dB e_db =
    {e = fun () -> from_dB (empty_ctxt ()) e_db}

  let close e = {e = fun () -> Lam e.e_open1}

  let open1' = function 
    | Lam f -> f
    | _ -> invalid_arg "PHOAS.open1: expression is not function."

  (** Convert a lambda to an open expression. *)
  let open1 e = {e_open1 = fun x -> open1' (e.e ()) x}

  let open2' e x = 
    match e with
      | Lam f -> 
	  (match f x with Lam f -> f
	     | _ -> invalid_arg "PHOAS.open3': expression is not function.")
      | _ -> invalid_arg "PHOAS.open3': expression is not function."

  (** Convert a double-lambda to a 2-variable open expression. *)
  let open2 e = {e_open2 = fun x y -> open2' (e.e ()) x y}

  let open3' e x y = 
    match e with
      | Lam f -> 
	  (match f x with Lam f ->
	     (match f y with Lam f -> f
		| _ -> invalid_arg "PHOAS.open3': expression is not function.")
	     | _ -> invalid_arg "PHOAS.open3': expression is not function.")
      | _ -> invalid_arg "PHOAS.open3': expression is not function."

  (** Convert a triple-lambda to an 3-variable open expression. *)
  let open3 e = {e_open3 = fun x y z -> open3' (e.e ()) x y z}

  let apply_exp' e1 e2 = 
    match e1 with
      | Lam f -> subst' (f e2)
      | InjectE s -> App (InjectE s, e2)
      | _ -> invalid_arg "PHOAS.apply: first argument is not function."

  let apply_exp (e1 : hoas_exp) (e2 : hoas_exp) = 
    {e = fun () -> apply_exp' (e1.e ()) (e2.e())}

end

(******************************************************************************)
module Expr_gil = struct 

  open DBL
  module P = PHOAS

  (** Convert to a string for reading/debugging. Does not generate real code. *)
  let rec to_string_raw c = function
    | Lam f ->
	Printf.sprintf "(lam. %s)" (to_string_raw (c+1) f)
    | Var x -> if x < c then Printf.sprintf "%d" x else Printf.sprintf "'%d" x
    | App (e1,e2) -> Printf.sprintf "(%s %s)" (to_string_raw c e1) (to_string_raw c e2)
    | NoneE -> "None"
    | SomeE e -> Printf.sprintf "(Some %s)" (to_string_raw c e)
    | AndE (e1,e2) -> Printf.sprintf "(Pred2.andc %s %s)" (to_string_raw c e1) (to_string_raw c e2)
    | OrE (e1,e2) -> Printf.sprintf "(Pred2.orc %s %s)" (to_string_raw c e1) (to_string_raw c e2)
    | CallE (nt,e1,e2) -> gil_callc (mk_npname nt) (to_string_raw c e1) (to_string_raw c e2)
    | InjectE s -> Printf.sprintf "(%s)" s
    | CfgLookaheadE (b,n) -> Printf.sprintf "(Pred2.lookaheadc %B 0 %s)" b n
    | CsLookaheadE (b,cs) -> Printf.sprintf "(Pred2.lookaheadc %B 0 %s)" b (Cs.to_nice_string cs)

  let false_e = Lam (Lam (Lam NoneE))

  let is_bool = function
    | Lam Lam Lam NoneE 
    | Lam Lam Lam SomeE Var 2 -> true
    | _ -> false

  let rewrite preds e =
    let rec rewrite' c = function  (* c is the count of binders *)
      | AndE (e1, e2) ->
	  let e1 = rewrite' c e1 in
	  let e2 = rewrite' c e2 in
	  (match (e1, e2) with
	     | (Lam Lam Lam NoneE, _) -> false_e
	     | (_, Lam Lam Lam NoneE) -> false_e
	     | (Lam Lam Lam SomeE e, _) ->
		 (* Since c is the count of binders, c - 1 is
		    the maximum possible free variable. We shift
		    by 3 since we will be surrounding [e2] by 3
		    lambdas. *)
		 let e2_s = shift (c - 1) 3 e2 in
		 let la_var = Var c in
		 let p_var = Var (c + 1) in
		 (* [Var c] is the outermost lambda, which, in this case,
		    is the lookahead variable; [Var (c + 1)] corresponds
		    to the position variable. *)
		 Lam (Lam (Lam (app3 e2_s la_var p_var e)))
	     | _ -> AndE (e1, e2))
      | OrE (e1, e2) ->
	  let e1 = rewrite' c e1 in
	  let e2 = rewrite' c e2 in
	  (match (e1,e2) with
	       (Lam (Lam (Lam (SomeE _))), _) -> e1
	     | (_, Lam (Lam (Lam (SomeE _)))) -> e2
	     | (Lam (Lam (Lam NoneE)), _) -> e2
	     | _ -> OrE (e1, e2))
      | CallE (nt, e1, e2) as e_orig ->
	  let nt_pred = preds nt in
	  (match nt_pred with
	       Lam (Lam (Lam NoneE)) -> false_e
	     | Lam (Lam (Lam (SomeE e_nt))) ->
(* 		 let e_nt_pred_ph = P.convert_from_dB nt_pred in *)
(*                  let e1_ph = P.convert_from_dB e1 in *)
(* 		 let e1_open = P.open2 e1_ph in *)

(*                  let e2_ph = P.convert_from_dB e2 in *)
(* 		 let e_nt_open =  *)
(* 		   {P.e_open3 = fun v la ykb -> *)
(* 		      let P.Lam f1 = e_nt_pred_ph.P.e () in *)
(* 		      let P.Lam f2 = f1 la in *)
(* 		      let P.Lam f3 = f2 ykb in *)
(* 		      let P.SomeE x = f3 v in  *)
(* 		      x))} in *)

(*                  let ntpred_apply = {P.e_open2 = fun pos v1 ->  *)
(*                                    P.subst' (e_nt_open.P.e_open1  *)
(* 					       (P.vsub2 e1_open pos v1))} in *)

(*                  let e2_open = P.open3 e2_ph in *)
(*                  let body = {P.e_open2 = fun pos v1 ->  *)
(* 			       let e2 v2 p v = e2_open.P.e_open3 p v v2 in *)
(*                                P.vsub2' (e2 (P.vsub2 ntpred_apply pos v1)) pos v1} in *)

(* 		 let result_ph = {P.e = fun () -> P.Lam (fun la -> P.Lam (fun ykb -> P.Lam (fun v1 -> *)
(*                    P.SomeE (P.let_e (P.App (P.InjectE "YkBuf.get_offset", ykb))  *)
(* 		     (P.Lam (fun pos -> body.P.e_open2 pos v1))))))} in *)

(* 		 P.to_dB result_ph 0 *)
 
		 (* the "current" level is c (which corresponds to the first binder),
		    so we use c + 1 for ykb, which is the second binder, and c + 2 for v,
		    which is the third binder.

		    We add another binder [p] for the position, as a let.
		 *)
		 let ykb_var = Var (c + 1) in
		 let v_var_id = c + 2 in
		 let v_var = Var v_var_id in
		 let p_var = Var (c + 3) in
		 (* shift e1 and e2 by 4 b/c we are putting them under four lambdas.
		    We shift e_nt by only 1 b/c it comes from under 3 lambdas. Notice 
		    that its maximum free variable is larger. *)
		 let e1_s = shift (c-1) 4 e1 in
		 let e2_s = shift (c-1) 4 e2 in
		 let e_nt_s = shift (c+2) 1 e_nt in
                 let calling_arg = app2 e1_s p_var v_var in
		 let v_nt = subst v_var_id calling_arg e_nt_s in
		 (* TODO: why not rewrite the app3 in the SomeE? *)
		 Lam (Lam (Lam
			     (SomeE (App (Lam (app3 e2_s p_var v_var v_nt),
					  App (InjectE "YkBuf.get_offset", ykb_var))
				    ))))
	     | _ -> e_orig)
      | Lam f -> Lam (rewrite' (c+1) f)
      | App (e1, e2) -> App (rewrite' c e1, rewrite' c e2)
      | (InjectE _
	| Var _
	| CfgLookaheadE _
	| CsLookaheadE _
	| NoneE
	| SomeE _) as r -> r
    in
    rewrite' 0 e

end

open PHOAS

(******************************************************************************)

let to_string' callts get_action get_start =
  let rec recur c = function
    | Lam f ->
	let x = mk_var c in
	(match f x with
	  | Lam g ->
	      	let y = mk_var (c+1) in
		(match g y with
		  | Lam h ->
	      	      let z = mk_var (c+2) in
		      Printf.sprintf "(fun %s %s %s -> %s)" x y z (recur (c+3) (h z))
		  | t -> Printf.sprintf "(fun %s %s -> %s)" x y (recur (c+2) t))
	  | t -> Printf.sprintf "(fun %s -> %s)" x (recur (c+1) t))
    | Var x -> x
    | App (e1,e2) -> Printf.sprintf "(%s %s)" (recur c e1) (recur c e2)
    | NoneE -> "None"
    | SomeE e -> Printf.sprintf "(Some %s)" (recur c e)
    | AndE (e1,e2) -> Printf.sprintf "(Pred.andc %s %s)" (recur c e1) (recur c e2)
    | OrE (e1,e2) -> Printf.sprintf "(Pred.orc %s %s)" (recur c e1) (recur c e2)
    | CallE (nt,e1,e2) -> callts (mk_npname nt) (recur c e1) (recur c e2)
    | InjectE s -> Printf.sprintf "(%s)" s
    | CfgLookaheadE (b,n) ->
	let n_act = get_action n in
	Printf.sprintf "(Pred.full_lookaheadc %B %d %d)" b n_act (get_start n_act)
    | CsLookaheadE (b,la_cs) -> 
	(* Complement w.r.t. 257 to account for EOF, which we represent as character 256. *)
	let cs = if b then la_cs else Cs.complement 257 la_cs in	
	Printf.sprintf "(Pred.cs_lookaheadc (%s))" (Cs.to_code cs)
  in 
  recur

let to_string callts get_action get_start {e=f} = to_string' callts get_action get_start 0 (f())

(******************************************************************************)

(** Convenience constructors *)

let app2 x y z = App (App (x,y), z)
let app3 f x y z = App (App (App (f, x), y), z)
let lam2 f = Lam (fun x -> Lam (f x))
let lam3 f = Lam (fun x -> lam2 (f x))

let ignore_call_e = Lam (fun p -> Lam (fun v -> InjectE "sv0"))
(* For reasons of type-var generalization, we use Lam constructors directly,
   rather than using lam2. *)
let ignore_binder_e = Lam (fun x -> Lam (fun y -> Var x))

(******************************************************************************)

type nullability = Yes_n | No_n | Maybe_n

let get_symbol_nullability preds_tbl nt =
(*   Printf.eprintf "Looking up nullability of symbol %s.\n" nt; *)
  let e_n = Hashtbl.find preds_tbl nt in
  (*  For the case of
          Lam (SomeE e)
      , while we know statically that it is nullable,
      we still need to dynamically compute e, so we
      categorize it as maybe. *)
  match e_n with
    | DBL.Lam (DBL.Lam DBL.NoneE) -> No_n
    | DBL.Lam (DBL.Lam (DBL.SomeE (DBL.Var 1))) -> Yes_n
    | _ -> Maybe_n

(******************************************************************************)

let true_e () = lam2 (fun la v -> SomeE (Var v))
let false_e () = lam2 (fun la v -> NoneE)

(* invariant: branches return an npred, which now includes lookahead arg. *)
let rec trans' r = match r.Gul.r with
| Gul.When e -> Lam (fun la -> InjectE e)
| Gul.Action(Some e,_) ->
    lam2 (fun la v -> SomeE (App (InjectE e, Var v)))
| Gul.Action _ -> true_e ()
| Gul.Symb (nt, None, _, None) -> (* TODO: attributes *)
    CallE (nt, ignore_call_e, ignore_binder_e)
| Gul.Symb (nt, Some action, _, None) -> (* TODO: attributes *)
    CallE (nt, InjectE action, ignore_binder_e)
| Gul.Symb (nt, None, _, Some binder) -> (* TODO: attributes *)
    CallE (nt, ignore_call_e, InjectE binder)
| Gul.Symb (nt, Some call_action, _, Some binder) -> (* TODO: attributes *)
    CallE (nt, InjectE call_action, InjectE binder)
| Gul.CharRange _ -> false_e ()
| Gul.Lit (_,"") | Gul.Opt _ -> true_e ()
| Gul.Lit (_,_) -> false_e ()
| Gul.Alt (r1,r2) -> OrE (trans' r1, trans' r2)
| Gul.Seq (r1,_,_,r2) -> AndE (trans' r1, trans' r2)
| Gul.Assign (r1,_,_) -> trans' r1
| Gul.Star (Gul.Bounds (0,_),_) -> true_e ()
| Gul.Star (Gul.Bounds (_,_),r1) -> trans' r1  (* TODO: this will be problematic
                                                  if r1 is nullable and modifies the semval.*)
    (* should instead be some recursive function *)
| Gul.Lookahead (b, {Gul.r=Gul.Symb(nt,None,_,None)}) -> CfgLookaheadE(b,nt) (* TODO: attributes *)
| Gul.Box (_, _, Gil.Always_null) -> true_e ()
| Gul.Box (_, _, Gil.Never_null) -> false_e ()
| Gul.Box (e, _, Gil.Runbox_null) -> InjectE("Pred3.boxc (" ^ e ^ ")")
| Gul.Box (_, _, Gil.Runpred_null e) -> InjectE ("let p = " ^ e ^ " in fun _ _ -> p")
| Gul.Lookahead _
| Gul.Star _ | Gul.Rcount _ | Gul.Hash _
| Gul.Delay _
| Gul.Prose _ | Gul.Position _ | Gul.Minus _ ->
    Printf.eprintf "Should have desugared %s\n" (Pr.rule2string r); false_e()

let trans r = {e = fun () -> trans' r}

let compile_rule nt r =
  let e = trans r in
  let e_simp = DBL.simplify (convert_to_dB e) in
  if false then
    begin
      (* Sanity check *)
      if not (DBL.is_closed e_simp) then
	Printf.eprintf "Nonterminal %s has open predicate: %s\n" nt (DBL.to_string_raw 0 e_simp)
    end;
  e_simp

(* None = The empty set
   Some false = a non-empty set, which does not include epsilon
   Some true = a non-empty set, which does include epsilon
   Some e  = needs to be determined dynamically.
*)


let preds_from_grammar grm =
  let preds_tbl = Hashtbl.create 11 in
  let preds nt =
    try Hashtbl.find preds_tbl nt with
	Not_found ->
	  Printf.eprintf "Warning: symbol %s not defined. Assuming not nullable.\n" nt;
	  DBL.false_e
  in
  let init_nt = function Gul.RuleDef (n,_,_) -> Hashtbl.add preds_tbl n DBL.false_e | _ -> () in
  let try_rewrite_nt = function
      Gul.RuleDef (n,r,_) ->
	if Logging.activated then
	  Logging.log Logging.Features.nullpred
	    "Attempting rewrite of symbol %s.\n" n;
	let e_n = compile_rule n r in
	let e_n_rw = DBL.rewrite preds e_n in
	let e_n' = DBL.simplify e_n_rw in
	if false then
	  begin
	    (* Sanity check *)
	    if not (DBL.is_closed e_n') then
	      Printf.eprintf "Nonterminal %s has open simpl-rewr predicate: \nbefore:   %s\nafter:  %s\n"
		n (DBL.to_string_raw 0 e_n_rw) (DBL.to_string_raw 0 e_n');
	  end;
	Hashtbl.replace preds_tbl n e_n'
    | _ -> ()
  in
  (* Sort by dependency order so that we need only traverse the grammar rules once to get the
     correct answers. *)
  let ds_sorted = Gul.sort_definitions grm.Gul.ds in
  List.iter init_nt ds_sorted;
  List.iter try_rewrite_nt ds_sorted;
  preds_tbl

let process_grammar ch get_action get_start grm =
  let preds =  preds_from_grammar grm in

  (* Record which nonterminals are called from other nonterminals (including themselves). *)
  let called_set = Hashtbl.fold (fun _ e s -> DBL.note_called s e) preds (PSet.create compare) in

  let print_pred nt e_p first =
    if first then
      Printf.fprintf ch "let rec "
    else
      Printf.fprintf ch "and ";
    (* To ensure "let rec" compatibility, we force everything to be a syntactic function.
       We can special case functions, b/c they already meet the criterion, and eta-expand
       everything else.
    *)
    let x = mk_var 0 in
    let p = convert_from_dB e_p in
    let body = match e_p with
	DBL.Lam (DBL.Lam _) ->
	  (match p.e () with
	       Lam f ->
		 (match f lookahead_name with
		      Lam f2 -> f2 x
		    | _ -> failwith "Internal error in module Nullable_pred: De Bruin and PHOAS representations out-of-sync.")
	     | _ -> failwith "Internal error in module Nullable_pred: De Bruin and PHOAS representations out-of-sync.")
      | _ ->  app2 (p.e()) (Var lookahead_name) (Var x)
    in
    if PSet.mem nt called_set && not (DBL.is_bool e_p) then
      Printf.fprintf ch "%s = let __tbl = SV_hashtbl.create 11 in
                       fun %s %s ->
                          let __k = (%s,%s) in
                          try SV_hashtbl.find __tbl __k
                          with Not_found ->
                            let x = %s in SV_hashtbl.add __tbl __k x; x\n\n"
	(mk_npname nt) lookahead_name x lookahead_name x
	(to_string' callc get_action get_start 1 body)
    else
      Printf.fprintf ch "%s %s %s = %s\n\n" (mk_npname nt) lookahead_name x (to_string' callc get_action get_start 1 body);
    false
  in
  Printf.fprintf ch "module SV_hashtbl = Hashtbl.Make(struct
                        type t = lookahead * sv
                        let equal (la1,a) (la2,b) = (la1 == la2) && (sv_compare a b = 0)
                        let hash = Hashtbl.hash end)\n";
  ignore (Hashtbl.fold print_pred preds true)

(******************************************************************************)
module Gil = struct

  let true_e () = lam3 (fun la p v -> SomeE (Var v))
  let false_e () = lam3 (fun la p v -> NoneE)
  (* For reasons of type-var generalization, we use Lam constructors directly,
     rather than using lam3. *)
  let ignore_binder_e = Lam (fun p -> Lam (fun x -> Lam (fun y -> Var x)))
  let app_iv f ykb v = app2 (InjectE f) (App (InjectE "YkBuf.get_offset", Var ykb)) (Var v)

  (* invariant: branches return an npred, which now includes lookahead arg. *)
  let rec trans' = function
  | Gil.Symb (nt, None, None) ->
      CallE (nt, ignore_call_e, ignore_binder_e)
  | Gil.Symb (nt, Some action, None) ->
      CallE (nt, InjectE action, ignore_binder_e)
  | Gil.Symb (nt, None, Some binder) ->
      CallE (nt, ignore_call_e, InjectE binder)
  | Gil.Symb (nt, Some call_action, Some binder) ->
      CallE (nt, InjectE call_action, InjectE binder)
  | Gil.Lit (_,"") -> true_e ()
  | Gil.Lit (_,_) -> false_e ()
  | Gil.CharRange _ -> false_e ()
  | Gil.Action e ->
      lam3 (fun la ykb v -> SomeE (app_iv e ykb v))
  | Gil.Box (_, Gil.Always_null) -> true_e ()
  | Gil.Box (_, Gil.Never_null) -> false_e ()
  | Gil.Box (e, Gil.Runbox_null) -> InjectE("Pred3.boxc (" ^ e ^ ")")
  | Gil.Box (_, Gil.Runpred_null e) -> InjectE ("let p = " ^ e ^ " in fun _ _ -> p")
  | Gil.When (f_pred, f_next) -> 
      InjectE ("let p = " ^ f_pred ^ " and n = " ^ f_next ^ " in " ^ 
	       "fun _ ykb v -> let pos = YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None")
  | Gil.Seq (r1,r2) -> AndE (trans' r1, trans' r2)
  | Gil.Alt (r1,r2) -> OrE (trans' r1, trans' r2)
  | Gil.Star _ -> true_e ()
  | Gil.Lookahead (b, Gil.Symb(nt,None,None)) -> CfgLookaheadE(b,nt)
  | Gil.Lookahead (b, r) ->
	(match Gil.to_cs r with
	   | Some cs -> CsLookaheadE(b,cs)
	   | None ->
	       Util.error Util.Sys_warn 
		 "lookahead limited to character sets and argument-free symbols.\n"; 
	       false_e()
	)

  let trans r = {e = fun () -> trans' r}

  let compile_rule nt r =
    let e = trans r in
    let e_simp = DBL.simplify (convert_to_dB e) in
    if false then
      begin
	(* Sanity check *)
	if not (DBL.is_closed e_simp) then
	  Printf.eprintf "Nonterminal %s has open predicate: %s\n" nt (Expr_gil.to_string_raw 0 e_simp)
      end;
    e_simp

  let preds_from_grammar grm =
    let preds_tbl = Hashtbl.create 11 in
    let preds nt =
      try Hashtbl.find preds_tbl nt with
	  Not_found ->
	    Printf.eprintf "Warning: symbol %s not defined. Assuming not nullable.\n" nt;
	    Expr_gil.false_e
    in
    let init_nt = fun (n,_) -> Hashtbl.add preds_tbl n Expr_gil.false_e in
    let try_rewrite_nt (n,r) =
      if Logging.activated then
	Logging.log Logging.Features.nullpred 
	  "Attempting rewrite of symbol %s.\n" n;
      let e_n = compile_rule n r in	
      let e_n_rw = Expr_gil.rewrite preds e_n in
      let e_n' = DBL.simplify e_n_rw in
      if false then
	begin
	  (* Sanity check *)
	  if not (DBL.is_closed e_n') then
	    Printf.eprintf "Nonterminal %s has open simpl-rewr predicate: \nbefore:   %s\nafter:  %s\n"
	      n (Expr_gil.to_string_raw 0 e_n_rw) (Expr_gil.to_string_raw 0 e_n');
	end;
      Hashtbl.replace preds_tbl n e_n'
    in
    (* Sort by dependency order so that we need only traverse the grammar rules once to get the 
       correct answers. *)
    let ds_sorted = Gil.sort_definitions grm in
    List.iter init_nt ds_sorted;
    List.iter try_rewrite_nt ds_sorted;
    preds_tbl

  let process_grammar ch get_action get_start grm =
    let preds =  preds_from_grammar grm in

    (* Record which nonterminals are called from other nonterminals (including themselves). *)
    let called_set = Hashtbl.fold (fun _ e s -> DBL.note_called s e) preds (PSet.create compare) in

    let print_pred nt e_p first =
      if first then
	Printf.fprintf ch "let rec "
      else
	Printf.fprintf ch "and ";
      (* To ensure "let rec" compatibility, we force everything to be a syntactic function.
	 We can special case functions, b/c they already meet the criterion, and eta-expand
	 everything else.
      *)
      let ykb = mk_pvar 0 in
      let v = mk_var 0 in
      let p = convert_from_dB e_p in
      let exc = Failure "Internal error in module Nullable_pred: De Bruin and PHOAS representations out-of-sync." in
      let body = match e_p with
	  DBL.Lam DBL.Lam DBL.Lam _ ->
	    (match p.e () with 
	       | Lam f -> 
		   (match f lookahead_name with 
		      | Lam f2 -> 
			  (match f2 ykb with
			     | Lam f3 -> f3 v
			     | _ -> raise exc)
		      | _ -> raise exc)
	       | _ -> raise exc)
	| _ ->  app3 (p.e()) (Var lookahead_name) (Var ykb) (Var v)
      in
      if PSet.mem nt called_set && not (Expr_gil.is_bool e_p) then begin
	(* COMMENT HERE *)
	Printf.fprintf ch "%s = let __tbl = SV_hashtbl.create 11 in\n\
			 fun %s %s %s -> \n"
	  (mk_npname nt) lookahead_name ykb v;
	let body_code = to_string' gil_callc get_action get_start 1 body in
	Printf.fprintf ch 
	  "let __p1 = YkBuf.get_offset %s in\n\
          try\n\
	    let (r, __p2)  = SV_hashtbl.find __tbl %s in\n\
	    if __p1 = __p2 then r else\n\
	    let x = %s in SV_hashtbl.replace __tbl %s (x, __p1); x\n\
	  with Not_found ->\n"
	  ykb v body_code v;
	Printf.fprintf ch 
          "  let x = %s in SV_hashtbl.add __tbl %s (x, __p1); x\n\n" 
	  body_code v
      end
      else
	Printf.fprintf ch "%s %s %s %s = %s\n\n" (mk_npname nt) lookahead_name ykb v 
	  (to_string' gil_callc get_action get_start 1 body);
      false
    in
    Printf.fprintf ch "module SV_hashtbl = Hashtbl.Make(struct 
			  type t = sv 
			  let equal a b = sv_compare a b = 0
			  let hash = Hashtbl.hash end)\n";
    Printf.fprintf ch "module Pred = Pred3\n"; 
    ignore (Hashtbl.fold print_pred preds true)

  let get_symbol_nullability preds_tbl nt =
    let e_n = Hashtbl.find preds_tbl nt in
    (*  For the case of
	Lam (Lam (Lam (SomeE e)))
	, while we know statically that it is nullable,
	we still need to dynamically compute e, so we
	categorize it as maybe. *)
    match e_n with
      | DBL.Lam DBL.Lam DBL.Lam DBL.NoneE -> No_n
      | DBL.Lam DBL.Lam DBL.Lam DBL.SomeE DBL.Var 2 -> Yes_n
      | _ -> Maybe_n

end
