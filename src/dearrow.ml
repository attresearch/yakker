(*******************************************************************************
 * Copyright (c) 2011 AT&T.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Trevor Jim and Yitzhak Mandelbaum
 *******************************************************************************)

(** The arrow-notation transform.
    Prerequisites:
      lifting. provides valuable invariants for Opt, Star.

    For detailed notes on the design of the code in this module please see:

       https://yakker.research.att.com/wiki/Notes-on-the-arrow-notation-transformation

*)

open Yak
open Gul

module Label = struct
(* The labeling transform *)
open Variables

(* Label convention: 0 means unlabeled *)
(* Assumes that the input grammar has been marked relevant,
   and all pre/post labels start out at 0 *)

let start_labels_at = 1000

let transform gr =
  let current = ref start_labels_at in
  let rec loop r =
    if not(r.a.early_relevant || r.a.late_relevant) then () else
    let prelabel() = r.a.pre <- postincr current in
    let postlabel() = r.a.post <- postincr current in
    match r.r with
    | When _
    | Symb _
    | DBranch _
      -> prelabel(); postlabel()
    | Lookahead(_,r2)
    | Star(_,r2) ->
        prelabel(); postlabel();
        loop r2
    | Box _
    | Position _
    | Action _
    | Delay _ ->
        prelabel()
    | Seq(r2,early,late,r3) ->
        prelabel();
        loop r2;
        loop r3
    | Assign(r2,early,late) ->
        prelabel();
        loop r2
    | Alt(r2,r3) ->
        loop r2;
        loop r3
    | Opt r2 ->
        loop r2
    (* cases below should have been desugared *)
    | Rcount _    -> Util.impossible "Label.transform.loop.Rcount"
    | Hash _      -> Util.impossible "Label.transform.loop.Hash"
    | Minus _     -> Util.impossible "Label.transform.loop.Minus"
    (* cases below should not be relevant *)
    | Lit _       -> Util.impossible "Label.transform.loop.Lit"
    | CharRange _ -> Util.impossible "Label.transform.loop.CharRange"
    | Prose _     -> Util.impossible "Label.transform.loop.Prose"
  in
  List.iter
    (function RuleDef(n,r,a) -> loop r | _ -> ())
    gr.ds
end

open Gul.Curried_constructors

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
open Util.Operators

(* TODO:
   x fix assumption about shape of context on return from calls. Does not take extraneous attributes into account.
   x handle output attributes.
   x handle input attributes.
   x handle input attributes at Symb.
   x handle output attributes at Symb.
   x optimization: avoid match/build when in/out are identical.
   - tag environments, just like we do coroutines, to avoid full-blown comparison.
      * step 1: ensure all match/builds are hidden behind an abstraction layer.
   - Consider changing type inference to not produce a type if the nonterminal is not a producer. Seems more accurate than marking as unit.
   - Optimize non-producer returns so that even if the environment is non-empty, not action is produced.
     I've begun on this, but am not handling all cases. Specifically, do_base and when filter for this, while other invocations of updateCtxt do not. Have not checked merge yet.
   - We seem to be producing too many context constructors -- some are never used in the
     output code. Figure out why, and fix as need be.
   x Modify Return_bind semantics to only conditionally bind -- if it is a semantically relevant binding. In general, we give semantic irrelevance the unit type, but for these purposes we should avoid binding altogether. Aside from optimization, doesn't interact well with relevance analysis used to determine late/early relevance. Once we change late actions to be desugared into early, this issue won't come up, but still will be wasteful.
   - TESTING
     + case of Symb at end of RHS, for both relevant and irrelevant symbs.
   x Modify history-only args codegen to just use "_e" rather than its eta-expansion. Same for _m.
   x Modify codegen templates to avoid using "function" when pattern is irrefutable.
     Or, at least, when it is a variable.
   x Use late-relevance information to only place merge functions on nonterminals
     that need them.
   - Add support for input/output attributes to type inference.
   - Maintain attributes lists in sorted order. Then, optimize gen_ret in updateEnvMerge
     for special case of return simply propogating the result of the ending nonterminal.
   - Consider special-casing environemnt updates when env is empty or singleton.
   x support open delays.
   - Add support to type inference for inferring types of return attributes; moreover,
     when types are declared, be sure to unify them.
   - Move attribute-related checks to type inference, or even before.
*)



(* TODO: improve error reporting *)
let dearrow_error m =
  Util.sys_error m;
  raise Exit

(* Internal name for env type *)
let env_type_name = "_ev"

(* TODO: - move to Variables if deemed appropriate. *)

let fresh_prefix = "yk_x"

(** [freshen_wrt x seed xs]
    "freshen" a prefix [x] with respect to all previous combinations
    of [x] with numbers [n < seed] and strings in [xs]. *)
let freshen_wrt x seed xs =
  (* Easy to make more efficient version, but i'm expecting very small sets. *)
  let rec try_unique i =
    let y = "__" ^ x ^ (string_of_int i) in (* Create a new variable name *)
    if List.mem y xs then try_unique (i + 1) else y,i (* Check whether it is taken *)
  in try_unique seed

let fresh_wrt = fst $ freshen_wrt fresh_prefix 1




(**

  Missing distinction between contexts (static) and environments
  (dynamic).  leads to abuse of notation. We are using contexts to
  guide us in the correct generation of code for manipulating
  environments. We should be careful not to confuse the two.

  context = ordered list of variable-type pairs
  environment = ordered list of values.

*)
module type CONTEXT = sig
  type pl                               (** "plain" - normal context, might have shadowed variables. *)
  type sf                               (** shadowing-free *)

  type binding_class = Var | Attr

  type 'a ctxt

  val ctxt_size : 'a ctxt -> int
  val is_empty : 'a ctxt -> bool

  val force_pl : 'a ctxt -> pl ctxt
    (** [force_pl g] coerces a any context to a plain one. *)

  val force_sf : 'a ctxt -> sf ctxt
    (** [force_sf g] coerces any context to shadowing-free
        one. Use should be accompanied by explanation validating its
        use. *)

  val empty : sf ctxt
  val singleton_var : var -> ty -> sf ctxt
  val singleton_attr : var -> ty -> sf ctxt
  val ext_var : 'a ctxt -> var -> ty -> pl ctxt
  val ext_attr : 'a ctxt -> var -> ty -> pl ctxt

  val lookup : 'a ctxt -> var -> binding_class * ty

  (** If context is already shadow-free, guaranteed not to change. *)
  val deshadow: 'a ctxt -> sf ctxt

  (** [drop_n g n] drops the last [n] members of the environment g.
      If [n] is greater than the size of [g], then the empty environment
      is returned. *)
  val drop_n : 'a ctxt -> int -> 'a ctxt

  (** [drop_these g xs]. [g] is a context and [xs] a list of variables.
      Drop first binding of all variables in [xs] from [g]. Works correctly even if [xs]
      has repeated elements *)
  val drop_these : 'a ctxt -> var list -> 'a ctxt

  val tys_of_ctxt : 'a ctxt -> ty list
    (** [tys_of_ctxt g] provides the types of variables in [g]
        as an ordered list (including duplicates)  *)

  val names_of_ctxt : 'a ctxt -> var list
    (** [names_of_ctxt g] provides the bound names in [g]
        as an ordered list (including duplicates)  *)

  val vars_of_ctxt : 'a ctxt -> var list
    (** [names_of_ctxt g] provides the variables in [g]
        as an ordered list (including duplicates)  *)

  val attrs_of_ctxt : 'a ctxt -> var list
    (** [names_of_ctxt g] provides the attributes in [g]
        as an ordered list *)
end

module Context : CONTEXT = struct

  (** Contexts are encoded *in reverse*.  *)


  type binding_class = Var | Attr

  (* We leave the first two types abstract -- they are only used for
     static enforcement of invariants, and have no real meaning. *)
  type pl                               (** "plain" - normal context, might have shadowed variables. *)
  type sf                               (** shadowing-free *)

  type 'a ctxt = (var * binding_class * ty) list

  let empty = []
  let singleton_var v t = [(v, Var, t)]
  let singleton_attr v t = [(v, Attr, t)]
  let ext_var g v t = (v,Var,t)::g
  let ext_attr g v t = (v,Attr,t)::g

  let lookup g v =
    let (_,c,t) = List.find (fun (v1,_,_) ->  v1 = v) g in
    c,t

  let ctxt_size = List.length
  let is_empty = function [] -> true | _ -> false

  let force_pl g = g
  let force_sf g = g

  let drop_n g n = Util.list_drop n g

  let drop_these g xs =
    let g', _ = List.fold_left (fun (g2,xs) ((x,_,_) as b) ->
                                  let is_mem, xs = Util.list_mem_and_remove x xs in
                                  (if is_mem then g2 else b :: g2), xs)
      ([], xs) g in
    List.rev g'

  let names_of_ctxt g =
    List.rev_map (fun (x, _, _) -> x) g

  let tys_of_ctxt g =
    List.rev_map (fun (_, _, ty) -> ty) g

  let attrs_of_ctxt g =
    let f = function (x, Attr, _) -> [x] | _ -> [] in
    (List.flatten $ List.rev_map f) g

  let vars_of_ctxt g =
    let f = function (x, Var, _) -> [x] | _ -> [] in
    (List.flatten $ List.rev_map f) g

  (* Replaces all shadowed bindings with fresh names.
     If context is already shadow-free, guaranteed not to change.
     Deshadowing is deterministic. *)
  let deshadow g =
    let names_g = names_of_ctxt g in
    let maybe_rename (revg, vars, seed) (x, cl, t) =
      let x_image, seed =
        if StringSet.mem x vars then
          let x, used_seed = freshen_wrt fresh_prefix seed names_g in
          x, used_seed + 1
        else
          x, seed in
      (x_image, cl, t)::revg, StringSet.add x vars, seed in
    let revg, _, _ = List.fold_left maybe_rename ([], StringSet.empty, 1) g in
    List.rev revg

end

open Context

let ext_attrs g attrs = List.fold_left (fun g (x,ty) -> ext_attr g x ty) (force_pl g) attrs

(** "Attribute-union". Ordered union of the attributes sets in [g] and
    the attribute * type list [attrs]. [g] is extended with any
    attributes in [attrs] not already in [g].

    Precondition: vars(g) /\ attr_names = {} *)
let union_attrs g attrs =
  let attrs_g = attrs_of_ctxt g in
  let new_attrs = List.filter (fun (x,_) -> not $| List.mem x attrs_g) attrs in
  ext_attrs g new_attrs

module Meta_prog = struct
  type constructor = string
  type pat = string
  type exp = string

  let unit_ty : ty  = "unit"
  let int_ty : ty  = "int"
  let bool_ty : ty = "bool"

  let unit_val : exp = "()"
  let true_val : exp = "true"
  let false_val : exp = "false"
  let gen = Printf.sprintf
end

open Meta_prog


(** Binding qualifier. *)
type bindq = No_bind | Bind | Var_bind of string
             | Return_bind of
                 bool                   (* flag: returns value *)
                 * (var * string) list  (* (out-attribute, type) pairs. *)

(* TODO: improve error reporting for this check. *)
let check_ctxt_eq g1 g2 =
  if not (g1 = g2) then Util.sys_error "Contexts not equal."

(** [check_attrs_mem attrs g missing_message]
    Check that attributes [attrs] are included in [g]. We do not check
    for type compatability.
    Report [missing_message] (applied to the missing variable) if any
    of the names specified in [attrs] do not appear in [g]. *)
let check_attrs_mem attrs g missing_message =
  List.iter begin fun (v,_) ->
    try
      (match lookup g v with
        | (Attr, _) -> ()
        | (Var, _) -> dearrow_error ("Expected " ^ v ^ " to be attribute but found variable instead."))
    with Not_found -> dearrow_error ((missing_message v) ^ gen "\nContext: %s\n" (String.concat "," $| names_of_ctxt g))
  end
    attrs

let check_out_attrs_mem attrs g = check_attrs_mem attrs g
  (fun v -> v ^ " specified as output attribute, but not found in context.")

(** check that [out_attrs] are present in [g1] and then copy to [g2]. *)
let ext_out_attrs out_attrs g1 g2 =
  check_out_attrs_mem out_attrs g1;
  ext_attrs g2 out_attrs

(** Copy attributes [attrs] from [g1] to [g2]. Report
    [missing_message] (applied to the missing variable) if any of the
    names specified in [attrs] do not appear in [g1]. *)
let copy_attrs g1 g2 attrs missing_message =
  List.fold_left (fun g2 v ->
                    try
                      match lookup g1 v with
                        | (Attr, ty) -> ext_attr g2 v ty
                        | (Var, _) -> dearrow_error ("Expected " ^ v ^ " to be attribute but found variable instead.")
                    with Not_found -> dearrow_error (missing_message v))
    (force_pl g2) attrs

let copy_out_attrs g1 g2 attrs = copy_attrs g1 g2 attrs
  (fun v -> v ^ " specified as output attribute, but not found in context.")

let reserved_pos_var = "p"

(** the template for box code. *)
let box_template env_pat box_exp some_pat some_exp =
  gen "function
| %s ->
  let f = %s in
  fun input pos ->
    begin match f input pos with
    | None -> None
    | Some (n, %s) -> Some (n, %s)
    end
| _ -> raise (Failure  \"Expected %s\")" env_pat box_exp some_pat some_exp env_pat

(** irrefutable-pattern and closed box-expression version of box template. *)
let box_template_ip_c pat_in box_exp some_pat some_exp =
  gen "let f = %s in
  fun %s input pos ->
    begin match f input pos with
    | None -> None
    | Some (n, %s) -> Some (n, %s)
    end" box_exp pat_in some_pat some_exp

let action_template pos_pat env_pat result_exp =
  gen "fun %s -> function %s -> %s | _ -> raise (Failure  \"Expected %s\")" pos_pat env_pat result_exp env_pat

(** irrefutable-pattern version of action template. *)
let action_template_ip pos_pat env_pat result_exp =
  gen "fun %s %s -> %s" pos_pat env_pat result_exp

let merge_template pos_pat env_pat child_pat result_exp : string =
  gen "fun %s v1 v2 -> match (v1,v2) with
| (%s, %s) -> %s
| _ -> raise (Failure  \"Expected %s and %s\")" pos_pat env_pat child_pat result_exp env_pat child_pat

(** irrefutable-pattern version of merge template. *)
let merge_template_ip pos_pat env_pat child_pat result_exp : string =
  gen "fun %s %s %s -> %s" pos_pat env_pat child_pat result_exp

(* The following [mk_]-prefixed functions provide an abstraction
   from the code generation into which we insert handling of
   histories. *)

module EL_combs = struct

  (* TODO: how do we know that input patterns don't overlap with "h"? *)

  let hist_in_pat = gen "(%s,h)"
  let hist_in_pat1 = gen "(%s,h1)"
  let hist_in_pat2 = gen "(%s,h2)"
  let hist_in_pat_wild = gen "(%s,_)"
  let hist_out_exp = gen "(%s,h)"
  let hist_new_exp s = gen "(%s,_e %s h)" s reserved_pos_var
  let hist_merge_exp s i = gen "(%s,_m %d %s h1 h2)" s i reserved_pos_var
  let hist_prop_exp = gen "(%s,h1)"

  let mk_box env_pat box_exp some_pat some_exp =
    match env_pat with
      | "_" ->
          box_template_ip_c (hist_in_pat "_") box_exp some_pat (hist_out_exp some_exp)
      | _ ->
          box_template (hist_in_pat env_pat) box_exp some_pat (hist_out_exp some_exp)

  let mk_args_empty = function
    | true -> Some "_e2"
    | false -> None

  let mk_args env_pat result_exp =
    Some (action_template reserved_pos_var (hist_in_pat env_pat) (hist_new_exp result_exp))
  let mk_when env_pat result_exp =
    action_template "_" (hist_in_pat_wild env_pat) result_exp
  let mk_action pos_pat env_pat result_exp =
    match env_pat with
      | "_" ->
          action_template_ip pos_pat (hist_in_pat "_") (hist_out_exp result_exp)
      | _ ->
          action_template pos_pat (hist_in_pat env_pat) (hist_out_exp result_exp)

  let mk_merge is_late_rel lbl env_pat child_pat result_exp =
    if env_pat = result_exp && child_pat = "_" then
      if is_late_rel then gen "_m2 %d" lbl
      else Fsm.default_binder_tx
    else if child_pat = result_exp then
      let v = Variables.freshn "v" in
      let res_exp = if is_late_rel then hist_merge_exp v lbl else hist_prop_exp v in
      (* We can ignore [env_pat] because it can't be relevant to [result_exp]. *)
      merge_template_ip reserved_pos_var (hist_in_pat1 "_") (hist_in_pat2 v) res_exp
    else
      merge_template reserved_pos_var (hist_in_pat1 env_pat) (hist_in_pat2 child_pat)
        (if is_late_rel then hist_merge_exp result_exp lbl else hist_prop_exp result_exp)

  let mk_push l env_pat e =
    match env_pat with
      | "_" ->
          let v = Variables.freshn "v" in
          let h = Variables.freshn "h" in
          action_template_ip
            reserved_pos_var
            (gen "(%s,%s)" v h)
            (gen "(%s,_p %d (%s) %s %s)" v l e reserved_pos_var h)
      | _ ->
          let v = Variables.freshn "v" in
          let env_pat = gen "(%s as %s)" env_pat v in
          let h = Variables.freshn "h" in
          action_template
            reserved_pos_var
            (gen "(%s,%s)" env_pat h)
            (gen "(%s,_p %d (%s) %s %s)" v l e reserved_pos_var h)

  let init () =
    mk_box, mk_args_empty, mk_args, mk_when, mk_action, mk_merge, mk_push

end

module E_combs = struct

  let mk_box env_pat box_exp some_pat some_exp =
    box_template env_pat box_exp some_pat some_exp

  let mk_args_empty _ = None

  let mk_args env_pat result_exp =
    Some (action_template "_" env_pat result_exp)

  let mk_when env_pat result_exp =
    action_template "_" env_pat result_exp

  let mk_action pos_pat env_pat result_exp =
    action_template pos_pat env_pat result_exp

  let mk_merge _ _ env_pat child_pat result_exp =
    merge_template "_" env_pat child_pat result_exp

  let mk_push _ _ _ = Util.impossible "Dearrow.ECombs.mk_push: grammar not late-relevant."

  let init () = mk_box, mk_args_empty, mk_args, mk_when, mk_action, mk_merge, mk_push

end

module L_combs = struct

  let hist_in_pat  = "h"
  let hist_in_pat1 = "h1"
  let hist_in_pat2 = "h2"
  let hist_in_pat_wild = "_"
  let hist_out_exp = "h"
  let hist_new_exp = "_e"
  let hist_merge_exp = gen "_m %d"

  let mk_box _ _ _ _ =
    Util.impossible "Dearrow.LCombs.mk_box: grammar not early-relevant."

  let mk_args_empty = function
    | true -> Some hist_new_exp
    | false -> None

  let mk_args _ _ =
    Util.impossible "Dearrow.LCombs.mk_args: grammar not early-relevant."

  let mk_when _ _  =
    Util.impossible "Dearrow.LCombs.mk_when: grammar not early-relevant."

  let mk_action _ _ _ =
    Util.impossible "Dearrow.LCombs.mk_action: grammar not early-relevant."

  let mk_merge is_late_rel lbl _ _ _ =
    if is_late_rel then hist_merge_exp lbl else Fsm.default_binder_tx

  let mk_push l pat e =
    match pat with
      | "_" -> gen "_p %d (%s)" l e
      | _ -> Util.impossible "Dearrow.LCombs.mk_action: grammar not early-relevant."

  let init () =
    mk_box, mk_args_empty, mk_args, mk_when, mk_action, mk_merge, mk_push
end

module Neither_combs = struct

  let mk_box _ _ _ _ =
    Util.impossible "Dearrow.Neither_combs.mk_box: grammar not relevant."

  let mk_args_empty _ = None

  let mk_args _ _ =
    Util.impossible "Dearrow.Neither_combs.mk_args: grammar not relevant."

  let mk_when _ _  =
    Util.impossible "Dearrow.Neither_combs.mk_when: grammar not relevant."

  let mk_action _ _ _ =
    Util.impossible "Dearrow.Neither_combs.mk_action: grammar not relevant."

  let mk_merge _ _ _ _ _ =
    Util.warn Util.Sys_warn "Dearrow.Neither_combs.mk_merge: grammar not relevant.";
    Fsm.default_binder_tx

  let mk_push _ _ _ =
    Util.impossible "Dearrow.Neither_combs.mk_push: grammar not relevant."

  let init () = mk_box, mk_args_empty, mk_args, mk_when, mk_action, mk_merge, mk_push

end


let exp_empty_env = "Ykenv_empty"
let pat_empty_env = "_"
  (* We use wildcard as an optimization:

     1. When environment is empty, we have nothing to benefit from the
     pattern matching (other than error checking), so skip it.

     2. Call-collapsing assumes that if a nonterminal has no
     arguments, then its call can be collapsed. But that won't be
     true, if we match against the empty environment, because then we'll
     have a match failure if a caller has a non-empty environment and it
     calls a callee without arguments and the call is collapsed.
  *)



(* On the proper treatment of symbols:

   When dealing with symbols, we must take care for the result of
   the symbol to align with any merge code used for that symbol. The
   invariant regarding symbols must take into account a number of
   factors: relevance, producerness and out attributes. Only
   producerness and out attributes require a non-trivial merge function.

   The issues under consideration are primarily relevant, then, to
   two pieces of code: the choice of merge function in the Symb case
   below, and the handling of Return_bind. In considering the
   latter, we face a design choice: do we encode the proper course
   of action in the Return_bind constrruction itself (perhaps
   splitting it into two different constructors), or do we derive it
   from the context of the code? The latter choice seems natural in
   some sense, just because we can, but it runs into the issue of
   the code falling out of sync with the relevance analysis. On the
   other hand, if we take the former approach, then will be
   implicitly relying on the relevance analysis aligning with our
   expectations in this code. That might be the right way to go, but
   I'd be more comfortable with it if I knew we had a way to quickly
   place "blame" if something goes wrong, which I don't. Or, if I
   could be sure that if we consistently rely on it, that it won't
   result in any bugs. In so far as the concern for us is
   consistency between merge code and return code, as long as they
   both rely on the same analysis, I think we'll be fine.

   Note: I chose the latter path, and made the code more general
   than necessary at present so that if relevance analysis changes
   it will still work. more later...


   Done:

   1. We guide the returning of a value based on the judged
   "producerness" of a nonterminal.  We mark returns a priori with
   need to bind or not based on the early_producer set, and
   correspondingly modifying code that handles returns.

   2. In generating merge code, we take into account the "producerness"
   of nonterminal.

   3. Special-case handling of empty context not to pattern match
   against it (which is fine because it carries nothing), which will
   allow call-collapsing to work in cases where caller does *not* have
   an empty context but callee has no arguments.

   4. We can't just skip irrelevant rhs because they might have
   out-attribute producing symbols. Our code considers both
   "producerness" and output attributes, for considerations of what
   and when to add elements to the environment.

*)
let transform gr =
  Label.transform gr;
  let mk_box, mk_args_empty, mk_args, mk_when, mk_action, mk_merge, mk_push =
    match gr.grammar_early_relevant,gr.grammar_late_relevant with
      | true,true -> EL_combs.init ()
      | true,false -> E_combs.init ()
      | false,true -> L_combs.init ()
      | false,false -> Neither_combs.init () in

  (** a global map from ordered lists of types to
      constructors. *)
  let con_table : (ty list, constructor) Hashtbl.t = Hashtbl.create 11 in
  Hashtbl.add con_table [] exp_empty_env;
  let con_map  (l : ty list) : constructor =
    match Util.find_option con_table l with
      | None ->
          let c = "Ykenv" ^ Variables.fresh () in
          Hashtbl.add con_table l c;
          c
      | Some c -> c in

  let nt_attrs = Gul.attribute_table_of_definitions gr.ds in


  (** An abbreviation for con_map . tys_of_ctxt *)
  let mt = con_map $ tys_of_ctxt in

  let nm = names_of_ctxt in

  (*
    WARNING: tricky, because if we use a wildcard in the pattern, then we can run into
    trouble when trying to reconstruct later.

    Solution: provide "deshadowing" of contexts.
  *)

  let tuple_pat : pat list -> string =
    String.concat "," in

  let tuple_exp : exp list -> string = tuple_pat in

  (** Convert a context to a string encoding the pattern of variables in the context.
      Variables must be deshadowed first so that the pattern remains linear. First argument
      is a list of variables to be converted to wildcards in the pattern. *)
  let tuple_pat_of_ctxt_with_wild (xs : var list) : sf ctxt -> string =
    tuple_pat $ List.map (fun x -> if List.mem x xs then "_" else x) $ nm in

  (** Composes a number of the above operations. *)
  let named_pat_of_ctxt_with_wild (xs : var list) : sf ctxt -> pat =
    fun g ->
      if is_empty g then pat_empty_env
      else mt g ^ "(" ^ tuple_pat_of_ctxt_with_wild xs g ^ ")" in

  (** Convert a context to a string encoding the pattern of variables in the context.
      Variables must be deshadowed first so that the pattern remains linear. *)
  let tuple_pat_of_ctxt : sf ctxt -> string = tuple_pat $ nm in

  (** /G/ composes a number of the above operations. *)
  let named_pat_of_ctxt : sf ctxt -> pat =
    fun g ->
      if is_empty g then pat_empty_env
      else mt g ^ "(" ^ tuple_pat_of_ctxt g ^ ")" in

  let named_exp_of_ctxt : sf ctxt -> exp =
    fun g ->
      if is_empty g then exp_empty_env
      else mt g ^ "(" ^ tuple_pat_of_ctxt g ^ ")" in

  (** [ds_lookup g1 g2 x]
      Consider [g1] and [g2] together as a map from original names to
      deshadowed names. Lookup the right-most [x] of [g1] in this map. *)
  let ds_lookup g1 g2 x =
    (* great target for deforestation. *)
    List.assoc x (List.rev (List.combine (nm g1) (nm g2))) in

  let updateCtxt g xs a ty =
    let g1 = drop_these g xs in
    match a with
      | Var_bind x when not $| List.mem x (attrs_of_ctxt g1) -> ext_attr g1 x ty
      | Var_bind _ | No_bind | Bind -> g1
      | Return_bind (_, out_attrs) -> ext_out_attrs out_attrs g1 (force_pl empty) in

  (** Wrap expressions [args] so that their free variables from [g] are properly bound.
      pre-condition: [args] is non-empty. *)
  let wrap_args g args args_tys =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let c = con_map args_tys in
    let env_out = tuple_exp args in
    mk_args pat_in (gen "%s(%s)" c env_out) in

  let wrapWhen g e =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    mk_when pat_in ("("^e^")") in

  (** wrap a box e so that its free variables from G are properly bound.
      result is dropped and environment is propogated. *)
  let boxWrapProp g e =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let x = fresh_wrt $| nm g in
    mk_box (gen "(%s as %s)" pat_in x) e "_" x in

  let boxWrapUE g xs a e ty =
    let g1 = drop_these g xs in
    let g_ds = deshadow g in
    let g1_ds = drop_these g_ds xs in

    let pat_in = named_pat_of_ctxt g_ds in

    let _gen some_pat some_exp =
      mk_box pat_in e some_pat some_exp in

    let gen_upd p =
      let p = match p with | "_" -> p | _ -> ds_lookup g1 g1_ds p in
      let env_out = named_exp_of_ctxt g1_ds in
      _gen p env_out in

    let gen_ext =
      let xvar = fresh_wrt $| nm g1_ds in (*  Make sure [xvar] doesn't
                                                         shadow anything that will
                                                         appear in the return value
                                                         (which is based on [g1]). *)
      let g_out = ext_var g1_ds xvar ty in
      let env_out = named_exp_of_ctxt (force_sf g_out) in (* Promotion is safe b/c [xvar] is generated
                                                            fresh w.r.t. to [out_attrs]. *)
      _gen xvar env_out in

    let gen_ret bind_ret out_attrs =
      let p, g_out =
        if bind_ret then
          let xvar = fresh_wrt $| List.map fst out_attrs in
          xvar, ext_out_attrs out_attrs g1 (singleton_var xvar ty)
        else
          "_", ext_out_attrs out_attrs g1 (force_pl empty) in
      let env_out = named_exp_of_ctxt (force_sf g_out) in (* Promotion is safe b/c [p] is
                                                             fresh w.r.t. to [out_attrs]. *)
      _gen p env_out in

    match a with
      | No_bind -> gen_upd "_"
      | Var_bind x -> if List.mem x (attrs_of_ctxt g1) then gen_upd x else gen_ext
      | Bind -> gen_ext
      | Return_bind (b, out_attrs) -> gen_ret b out_attrs in


  (** [updateEnvP ppat g_ds g xs a e ty]
      pre: [g_ds = deshadow g]. *)
  let updateEnvP ppat g_ds g xs a e ty =
    let g1 = drop_these g xs in
    let g1_ds = drop_these g_ds xs in

    let pat_in = named_pat_of_ctxt g_ds in

    let _gen exp_result = mk_action ppat pat_in exp_result in

    let gen_upd p =
      let p = match p with | "_" -> p | _ -> ds_lookup g1 g1_ds p in
      let env_out = named_exp_of_ctxt g1_ds in
      _gen (gen "let %s = %s in %s" p e env_out) in

    let gen_ext =
      let xvar = fresh_wrt $| nm g1_ds in
      let g_out = ext_var g1_ds xvar ty in
      let env_out = named_exp_of_ctxt (force_sf g_out) in (* Promotion is safe b/c [xvar] is generated
                                                            fresh w.r.t. to [out_attrs]. *)
      _gen (gen "let %s = %s in %s" xvar e env_out) in

    let gen_ret bind_ret out_attrs =
      let p, g_out =
        if bind_ret then
          let xvar = fresh_wrt $| List.map fst out_attrs in
          xvar, ext_out_attrs out_attrs g1 (singleton_var xvar ty)
        else
          "_", ext_out_attrs out_attrs g1 (force_pl empty) in
      let env_out = named_exp_of_ctxt (force_sf g_out) in (* Promotion is safe b/c [xvar] is generated
                                                             fresh w.r.t. to [out_attrs]. *)
      _gen (gen "let %s = %s in %s" p e env_out) in

    match a with
      | No_bind -> gen_upd "_"
      | Var_bind x -> if List.mem x (attrs_of_ctxt g1) then gen_upd x else gen_ext
      | Bind -> gen_ext
      | Return_bind (b, out_attrs) -> gen_ret b out_attrs in

  let updateEnv g xs a e ty =
    let g_ds = deshadow g in
    updateEnvP "_" g_ds g xs a e ty in

  (*  PRE: vars(g) /\ attrs = {} *)
  let updateEnvMerge g xs attrs a ty_opt is_late_rel lbl : string option =
    let g_ds = deshadow g in
    let attr_names, attr_tys = List.split attrs in
    (* Convert any overwritten attributes from original env. to
       wildcards. We have no need them b/c their value is being
       replaced by the new version. *)
    let pat_in = named_pat_of_ctxt_with_wild attr_names g_ds in

    (* Extend [g] with attributes from [attrs] not already in
       [g]. Results hold for [g_ds] as well by preconditions of this
       function. *)
    let attrs_g = attrs_of_ctxt g in
    let new_attrs = List.filter (fun (x,_) -> not $| List.mem x attrs_g) attrs in
    let g = ext_attrs g new_attrs in
    let g_ds = force_sf (ext_attrs g_ds new_attrs) in

    let g1 = drop_these g xs in
    let g1_ds = drop_these g_ds xs in

    (* code for simply propogating current environment. Useful in
       conjunction with histories, when history needs to be merged. *)
    let _gen_prop = if is_late_rel then Some (mk_merge true lbl "x" "_" "x") else None in

    let _gen pat2 exp_result = mk_merge is_late_rel lbl pat_in pat2 exp_result in

    let mk_result_pat p_result =
      match p_result, attrs with
        | "_", [] -> "_"                (* optimization: avoid any pattern match at all. *)
        | _ ->
            let names, tys = match ty_opt with
              | None -> attr_names, attr_tys
              | Some ty -> p_result :: attr_names, ty :: attr_tys in
            let c = con_map tys in
            gen "%s(%s)" c $| tuple_pat names in

    (* code for update when we're ignoring the
       second (non-position) parameter. *)
    let gen_upd_2wild = _gen $| mk_result_pat "_" $| named_exp_of_ctxt g1_ds in

    let gen_upd x =
      let pat_result = mk_result_pat $| ds_lookup g1 g1_ds x in
      let env_out = named_exp_of_ctxt g1_ds in
      _gen pat_result env_out in

    (* We suspend the computation because [from_some] is only
       guaranteed to succeed under specific circumstances, so [gen_ext]
       should only be invoked under those circumstances. *)
    let gen_ext () =
      let xvar = fresh_wrt $| nm g1_ds in
      let pat_result = mk_result_pat xvar in
      let g_out = ext_var g1_ds xvar (Util.from_some ty_opt) in
      let env_out = named_exp_of_ctxt (force_sf g_out) in (* Promotion is safe b/c [xvar] is generated
                                                             fresh w.r.t. to [out_attrs]. *)
      _gen pat_result env_out in

    (* In this function, we distinguish various cases of return:
       return nothing, return just attributes, return at least a
       value. We don't need to specially consider just a value,
       because ext_out_attrs won't have any effect if there are no
       out_attrs. *)
    let gen_ret bind_ret out_attrs =
      if not bind_ret && out_attrs = [] then
        _gen_prop
      else
        let p, g_out =
          if bind_ret then
            let xvar = fresh_wrt (attr_names @ List.map fst out_attrs) in
            (* Needs to be fresh w.r.t.
               - [attr_names], so that the generated pattern for the result is valid;
               - [out_attrs], so that it doesn't shadow any of the attribute values that
               need to be returned. *)
            xvar, ext_out_attrs out_attrs g1 (singleton_var xvar $| Util.from_some ty_opt)
          else
            "_", ext_out_attrs out_attrs g1 (force_pl empty) in
        let pat_result = mk_result_pat p in
        let env_out = named_exp_of_ctxt (force_sf g_out) in (* Promotion is safe b/c [pat_result] is
                                                               fresh w.r.t. to [out_attrs]. *)
        Some (_gen pat_result env_out) in
    match a with
      | No_bind -> (match xs, attrs with [],[] -> _gen_prop | _ -> Some gen_upd_2wild)
      | Var_bind x -> Some (if List.mem x (attrs_of_ctxt g1) then gen_upd x else gen_ext ())
      | Bind -> Some (gen_ext ())
      | Return_bind (b, out_attrs) -> gen_ret b out_attrs in

  (** Translate IRRELEVANT Gul right-parts to Gil. *)
  let rec gul2gil r = (* should only be called by dispatch, so invariants are satisfied *)
    match r.r with
    | CharRange(x, y) -> Gil.CharRange(x, y)
    | Lit(x, y)  -> Gil.Lit(x, y)
    | Opt(r1) -> Gil.Alt(Gil.Lit(false, ""), gul2gil r1)
    | Alt(r1, r2) -> Gil.Alt(gul2gil r1, gul2gil r2)
    | Symb(n, None, [], None) -> Gil.Symb(n, None, None)
    | Action(None, None) -> Gil.Lit(false, "")
    | Seq(r1, None, None, r2) -> Gil.Seq(gul2gil r1, gul2gil r2)
    | Star(Bounds(0, Infinity), r1) -> Gil.Star(gul2gil r1)
    | Lookahead(b, r1)  -> Gil.Lookahead(b, gul2gil r1)
          (* The cases below are relevant, gul2gil should not be called on relevant rhs *)
    | Star _      -> Util.impossible "Dearrow.transform.gul2gil.Star"
    | Delay _     -> Util.impossible "Dearrow.transform.gul2gil.Delay"
    | Box _       -> Util.impossible "Dearrow.transform.gul2gil.Box"
    | Seq _       -> Util.impossible "Dearrow.transform.gul2gil.Seq"
    | Assign _    -> Util.impossible "Dearrow.transform.gul2gil.Assign"
    | Action _    -> Util.impossible "Dearrow.transform.gul2gil.Action"
    | When _      -> Util.impossible "Dearrow.transform.gul2gil.When"
    | DBranch (e,c)   -> (* TODO-dbranch Util.impossible "Dearrow.transform.gul2gil.DBranch". i.e., allow for relevant dbranches *)
        (* note: presently, this is the only real use of dbranch. *)
        Gil.DBranch(e, c, "")
    | Symb(n,Some _,   _,     _) -> Util.impossible (Printf.sprintf "Dearrow.transform.gul2gil.Symb(%s) with early arguments" n)
    | Symb(n,     _,_::_,     _) -> Util.impossible (Printf.sprintf "Dearrow.transform.gul2gil.Symb(%s) with attributes" n)
    | Symb(n,     _,   _,Some _) -> Util.impossible (Printf.sprintf "Dearrow.transform.gul2gil.Symb(%s) with late arguments" n)
          (* The cases below should have been desugared *)
    | Position _  -> Util.impossible "Dearrow.transform.gul2gil.Position"
    | Hash _      -> Util.impossible "Dearrow.transform.gul2gil.Hash"
    | Rcount _    -> Util.impossible "Dearrow.transform.gul2gil.Rcount"
    | Minus _     -> Util.impossible "Dearrow.transform.gul2gil.Minus"
    | Prose _     -> Util.impossible "Dearrow.transform.gul2gil.Prose" in

  let rec _tr (g : pl ctxt) xs bind_q r : 'a ctxt * 'b Gil.rhs=

    (** Should only be used for Gul rules which are *not* considered
        early producers. *)
    let _do_base g xs a r =
      (* Optimize special cases of xs, bind_q *)
      let r_gil = match xs, a with
        | [], No_bind -> r
        | _, Return_bind (false, []) -> r
            (* Does not return a value and has not output attributes,
               so we don't need to take any action now. *)

        | _, Return_bind (true, _) ->
            Util.impossible
              (Printf.sprintf "Dearrow.transform._tr._do_base: producer rhs passed to _do_base: %s" $|
                   Pr.Gil.Pretty.rule2string r)
              (* Violates pre-condition of [_do_base] *)

(*         | [], Return_bind [] -> r       (\* Semantically irrelevant, so we bind nothing. *\) *)
            (* BUG: the above code and comment are flawed in two ways. 1) still need to drop attributes
               which shouldn't be propogated. 2) calling code is expecting
               the return environment to contain unit.

               The previous bug claim is no longer true. We used to
               merge for all nonterminals, which resulted in the two
               problems below. However, we now use relevance/producer
               status to determine applicability of merge. Therefore,
               non-producing nonterminals with no out attributes don't
               need to return anything. That, together with the
               precondition on [_do_base] that [r] not be derived from
               an early producer, allows us to infer that if the rule
               is marked as [Return_bind], then it is safe to ignore
               all but the out attributes in deciding whether to treat
               as "no-bind."

               There's one catch: if a symbol has an argument or other
               binding (so is relevant) but is not a producer, then
               the above does not apply: the symbol will be judged
               relevant and, hence, references to it will be processed
               by the normal Symb case, rather than shortcut. In this
               case, we will once again have the situation wherein
               references to the symbol will merge and check for a
               unit return value, while the above code will not
               generate the action needed to ensure that the symbol in
               fact returns the unit value.

               The proper fix is to take producerness into account
               when handling symbols, but, for now, we'll just
               distinguish by looking at the environment, which will
               be non-empty if there's an argument. Not quite correct,
               because the context can be empty while r is still
               relevant and a non-producer, but good enough for
               regressions for now.
            *)
        | _ -> Gil.Seq (r, Gil.Action (updateEnv g xs a unit_val unit_ty)) in
      updateCtxt g xs a unit_ty, r_gil in
    let do_base r = _do_base g xs bind_q r in
    match r.r with
      | Lit (b,s) -> do_base $| Gil.Lit (b, s)
      | DBranch (e, c) -> do_base $| Gil.DBranch (e, c, "")
      | CharRange (m,n) -> do_base $| Gil.CharRange (m,n)
      | Lookahead (b, r1) -> do_base $| Gil.Lookahead (b, gul2gil r1)

      (* Late actions were already handled by replay.  If present, was
         left intact so as not to effect relevance. So, we can
         ignore. *)
      | Action (Some e, _) ->
          let ty = Util.from_some r.a.inf_type in
          let f = updateEnv g xs bind_q e ty in
          updateCtxt g xs bind_q ty, Gil.Action f

      (* Late actions were already handled by replay.  If present, was
         left intact so as not to effect relevance. So, we can
         ignore -- treat as epsilon. *)
      | Action (None, Some _) ->
          do_base $| Gil.Lit(true, "")

      | When e ->
          let f = match xs, bind_q with
            | [], No_bind
            | _, Return_bind (false, [])
                -> "prop2"
            | _ -> updateEnv g xs bind_q unit_val unit_ty in
          updateCtxt g xs bind_q unit_ty, Gil.When (wrapWhen g e, f)

      | Box (e, Some ty, bn) ->
          let f = match xs, bind_q with
            | [], No_bind -> boxWrapProp g e
            | _ -> boxWrapUE g xs bind_q e ty in
          updateCtxt g xs bind_q ty, Gil.Box(f, bn)

      | Box (_, None, _) ->
          Util.impossible "Dearrow.transform._tr.Box"

      (* We ignore any late arguments, because they will have already
         been handled by replay.  However, Replay leaves them intat so
         as not to effect relevance, so we can't insist that the late
         args be set to None. *)
      | Symb (nt, e_opt, attrs, _) ->
          begin
            let a_nt = try Hashtbl.find nt_attrs nt with
                Not_found ->
                  Util.impossible (Printf.sprintf "Dearrow.transform._tr.Symb: Symbol %s has unspecified properties." nt) in
            let gil_arg =
              let attr_names, attr_tys = List.split a_nt.Attr.input_attributes in
              (* List the input attribute expressions according to their order in the format attributes. *)
              let attr_exprs = List.map (fun x -> try List.assoc x attrs
                                         with Not_found ->
                                           dearrow_error (gen "Dearrow.transform._tr.Symb: Symbol %s missing attribute %s at invocation." nt x))
                attr_names in
              let args, args_tys = match e_opt with
                | None -> attr_exprs, attr_tys
                | Some e ->
                    let arg_ty = try Util.from_some a_nt.Attr.early_param_type with
                        Not_found -> dearrow_error (gen "Dearrow.transform._tr.Symb: Symbol %s called with paramater, \
                                                         but definition does not include a parameter type." nt) in
                    e :: attr_exprs, arg_ty :: attr_tys in
              match args with
                | [] -> mk_args_empty r.a.late_relevant
                | _ -> wrap_args g args args_tys in
            let ty = try Util.from_some r.a.inf_type with
                Not_found -> Util.impossible "Dearrow.transform._tr.Symb: Missing type annotation on Symb." in
            let attrs_o = a_nt.Attr.output_attributes in

            (* check: vars(g) /\ attr_names = {} *)
            begin match Util.list_intersect String.compare (vars_of_ctxt g) (List.map fst attrs_o) with
              | [] -> ()
              | names -> dearrow_error (Printf.sprintf "Dearrow.transform._tr.Symb: The names of symbol %s's \
                                                        output attributes overlap with bound variable names: %s." nt (tuple_exp names))
            end;

            let ty_opt = if PSet.mem nt gr.early_producers then Some ty else None in
            let merge = updateEnvMerge g xs attrs_o bind_q ty_opt r.a.late_relevant r.a.post in
            let r1 = Gil.Symb (nt, gil_arg, merge) in
            let g = updateCtxt (union_attrs g attrs_o) xs bind_q ty in
(*             Printf.eprintf "\nArrow: Context after %s: %s\n%!" nt (String.concat "," $| names_of_ctxt g); *)
            g, r1
          end

      | Assign (r, Some x, None) ->
          if List.mem x $| vars_of_ctxt g then
            dearrow_error ("Cannot assign " ^ x ^ " as attribute, because the name is already bound as a lexically-scoped variable.");
          let g', r1 = _tr g xs (Var_bind x) r in
          let r_gil = match bind_q with
            | No_bind -> r1
            | _ -> Gil.Seq (r1, Gil.Action (updateEnv g' [] bind_q unit_val unit_ty)) in
          updateCtxt g' [] bind_q unit_ty, r_gil

      (* As with Symb, for both cases of Seq, we ignore any late
         binders, because they will have already been handled by
         replay.  However, Replay leaves them intact so as not to
         affect relevance, so we can't insist that the late binders be
         set to None. *)
      | Seq (r1, None, _, r2) ->
          let g1, r1 = _tr g [] No_bind r1 in
          let g2, r2 = _tr g1 xs bind_q r2 in
          g2, Gil.Seq (r1,r2)

      | Seq (r1, Some x, _, r2) ->
          let ty = Util.from_some r.a.inf_type in
          let g1, r1 = _tr g [] Bind r1 in
          let g2, r2 = _tr (ext_var g1 x ty) (x::xs) bind_q r2 in
          g2, Gil.Seq (r1,r2)

      | Alt (r1, r2) ->
          let g1,r1 = _tr g xs bind_q r1 in
          let g2,r2 = _tr g xs bind_q r2 in
          check_ctxt_eq g1 g2;
          g1, Gil.Alt (r1,r2)

      (* Given lifting, Opt and Star(Bounds...) both must carry irrelevant subterms.
         Hence, we treat them like base cases. *)
      | Opt r1 ->
          let _, r1 = _tr g [] No_bind r1 in
          do_base (Gil.Alt(Gil.Lit(false,""), r1))

      | Star (Bounds (0, Infinity), r1) | Star (Accumulate (None, _), r1)->
          let _, r1 = _tr g [] No_bind r1 in
          do_base (Gil.Star r1)

      | Star (Accumulate (Some (x,e),_), r1) ->
          let ty = Util.from_some r.a.inf_type in
          let g', r_e =
            let xs = [] in
            let f = updateEnv g xs Bind e ty in
            updateCtxt g xs Bind ty, Gil.Action f in
          let g2 = ext_var g' x ty in
          let g2', r = _tr g2 [x] Bind r1 in
          check_ctxt_eq g' g2';
          let g3, r_x =
            let xs = x :: xs in         (* add [x] now so as to delete it from context.  *)
            let f = updateEnv g2 xs bind_q x ty in
            updateCtxt g2 xs bind_q ty, Gil.Action f in
          g3, Gil.Seq (r_e, Gil.Seq(Gil.Star r, r_x))

      | Position true ->
          let r_gil = match xs, bind_q with
            | [], No_bind -> Gil.Lit (false, "")
            | _ ->
                let g_ds = deshadow g in
                let p = fst $ freshen_wrt "p" 1 $| nm g in
                let f = updateEnvP p g_ds g xs bind_q p int_ty in
                Gil.Action f in
          updateCtxt g xs bind_q unit_ty, r_gil

      (* Essentially: @delay(e) => h = {h # push p (e, p)}
         assuming a history attribute h and position attribute p.
         In current framework, it is a bit more hackish. *)
      | Delay (false, e, _) ->
          do_base $| Gil.Action(mk_push r.a.pre "_" e)
      | Delay (true, e, _) ->
          let g = deshadow g in
          let pat_in = named_pat_of_ctxt g in
          do_base $| Gil.Action(mk_push r.a.pre pat_in e)
      | Star (Bounds _, _) ->
          Util.todo "Dearrow.transform._tr.Star: star with early closed bounds not yet supported."
      | Assign (_, _, Some _) ->
          Util.impossible "Dearrow.transform._tr.Assign: late assign not yet supported."

      (* These late cases should have been desugared: *)
      | Position false ->
          Util.impossible "Dearrow.transform._tr.Position: Late positions should have been desugared by Replay."
      | Action (None, None) ->
          Util.impossible "Dearrow.transform._tr.Action: Action(None,None) should never occur."
      | Assign (_, None, None) ->
          Util.impossible "Dearrow.transform._tr.Assing: Assign(_,None,None) should never occur."

      (* cases below should have been desugared *)
      | Rcount _    -> Util.impossible "Dearrow.transform._tr.Rcount"
      | Hash _      -> Util.impossible "Dearrow.transform._tr.Hash"
      | Minus _     -> Util.impossible "Dearrow.transform._tr.Minus"
          (* cases below should not be relevant *)
      | Prose _     -> Util.impossible "Dearrow.transform._tr.Prose"
  in
  gr.gildefs <-
    List.concat $|
        List.map
          (function
             | RuleDef(n,r,a) ->
                 let initial_ctxt, drop_set = match a.Attr.early_params with
                   | None -> empty, []
                   | Some s ->
                       let x = get_param s in
                       let ty = Util.from_some a.Attr.early_param_type in
                       singleton_var x ty, [x] in
                 let initial_ctxt = List.fold_left (fun g (v,ty) -> ext_attr g v ty)
                   (force_pl initial_ctxt) a.Attr.input_attributes in

                 let r = snd $| _tr initial_ctxt drop_set
                     (Return_bind (PSet.mem n gr.early_producers, a.Attr.output_attributes)) r in
                 [(n, r)]
             | _ -> [])
          gr.ds;
  let free_tyvars = Util.remove_dups $| Hashtbl.fold begin fun tys _ ft ->
    List.rev_append (List.filter Ty_infer.is_tyvar tys) ft
  end con_table [] in
  begin match free_tyvars with
    | [] -> add_to_prologue gr $| Printf.sprintf "type %s = \n" env_type_name
    | [x] -> add_to_prologue gr $| Printf.sprintf "type %s %s = \n" x env_type_name
    | _ -> add_to_prologue gr $| Printf.sprintf "type (%s) %s = \n" (String.concat " , " free_tyvars) env_type_name
  end;
  let cs = Hashtbl.fold (fun tys c xs -> (c,tys) :: xs) con_table [] in
  let cs = List.sort (fun (c1,_) (c2,_) -> String.compare c1 c2) cs in
  List.iter (fun (c,tys) ->
               match tys with
                 | [] -> add_to_prologue gr $| Printf.sprintf "| %s\n" c
                 | _ -> add_to_prologue gr $| Printf.sprintf "| %s of %s\n" c (String.concat " * " tys))
    cs;
  add_to_prologue gr $| Printf.sprintf "let ev0 = %s
let ev_compare = compare
let prop2 _ x = x
" exp_empty_env

let early_late_prologue = "
(*EARLY-LATE PROLOGUE*)
type _pos = int (* input positions *)

let hv_compare = Yk_History.compare

type sv = ev * (int * hv * _pos, Yak.History.label) Yak.History.history
let sv0 = (ev0, Yk_History.new_history())
let sv_compare (x1,x2) (y1,y2) =
  (match ev_compare x1 y1 with
  | 0 -> hv_compare x2 y2
  | z -> z)

let sv_hash (x,h) =
  let hash_h = Yk_History.hash h in
  (Hashtbl.hash x) lxor hash_h

let _m2 l p (x,h1) (_,h2) = x, _m l p h1 h2
let _e2 p (_,h) = ev0, _e p h
"

let early_prologue = "
(*EARLY PROLOGUE*)
type sv = ev
let sv0 = ev0
let sv_compare = ev_compare
let sv_hash = Hashtbl.hash
"

(** @raise [Failure], if failure occurs in attempting to retrieve type information or parse is ambiguous. *)
let get_type_info filename =
  if not (Sys.file_exists "yak.cmi") then raise (Failure "Attempted to execute 'ocamlc -i' for type inference \
                                                          without access to Yak interface. Move to directory \
                                                          containing yak.cmi and try again.")
  else
  let res =
    let hide_std_err = true in
    try
      Util.pipe_in_out_result
        (if hide_std_err then
           (Printf.sprintf "ocamlc -i %s 2> /dev/null" filename)
         else
           (Printf.sprintf "ocamlc -i %s" filename))
        (fun _ -> ())
        (* There is a space before the let, because, for some reason, the first byte of the file is eaten in this
           process. not connected to Util.pipe_in_out_result -- happens if I
           do the same using echo to send the command to ocamlc -i. *)
        Tyspec.parse_channel
    with _ -> raise (Failure "Failure occurred while attempting to retrieve type information.") in
  match res with
    | [] -> Util.impossible "Tyspec.parse_channel must return at least one result (or raise an exception)."
    | [x] -> x
    | _ -> Util.error Util.Sys_warn "Ambiguous parse of type information."; raise (Failure  "Ambigous parse of input.")

(* must be early relevant. argument [is_late_rel] tells us whether it is late relevant as well. *)
let extend_prologue pcompile gr =
  (** [set_env_type partial_compile gr], where [partial_compile]
      compiles the grammar w/o outputing the epilogue. Adds the [ev] type
      and related definitions to the prologue. *)
  let set_env_type is_late_rel gr =
    (* redirect output to a temporary file *)
    let (temp_file_name, temp_chan) = Filename.open_temp_file "yakker" ".ml" in

    (* Compile without printing the epilogue. We save that
       for later. First, duplicate the grammar to avoid any
       changes to original copy. *)
    let gr2 = {gr with ds = gr.ds} in
    if is_late_rel then
      begin
        add_to_prologue gr2 "let sv0 = (ev0, Yk_History.new_history())
let _m2 l p (x,h1) (_,h2) = x, _m l p h1 h2
let _e2 p (_,h) = ev0, _e p h
";
        pcompile temp_chan gr2;
        Printf.fprintf temp_chan "\nlet __yk_get_type_info_ = match List.hd (snd (List.hd program)) with ACallInstr3 (f,_) -> fst (f 0 (failwith \"\"));;\n";
      end
    else
      begin
        add_to_prologue gr2 "let sv0 = ev0\n";
        pcompile temp_chan gr2;
        Printf.fprintf temp_chan "\nlet __yk_get_type_info_ = match List.hd (snd (List.hd program)) with ACallInstr3 (f,_) -> f 0 (failwith \"\");;\n"
      end;

    (* make sure compiled output is flushed *)
    close_out temp_chan;

    (* Generate ev-related type definitions. *)
    let (n, tyargs) = get_type_info temp_file_name in
    let abstract_ty_defs = Util.list_make n (Printf.sprintf "type %s%d\n" Tyspec.tyvar_prefix) in
    add_many_to_prologue gr abstract_ty_defs;
    let ev_ty_def = "type ev = " ^ tyargs ^ " " ^ env_type_name ^ "\n" in
    add_to_prologue gr ev_ty_def;

    (* Clean up temp file *)
    Sys.remove temp_file_name
  in

    (match gr.grammar_early_relevant,gr.grammar_late_relevant with
       | true, true ->
           set_env_type gr.grammar_late_relevant gr;
           add_to_prologue gr early_late_prologue
       | true, false ->
           set_env_type gr.grammar_late_relevant gr;
           add_to_prologue gr early_prologue
       | false,true -> Coroutine.add_late_prologue gr
       | false,false -> Coroutine.add_no_early_or_late_prologue gr);
    add_to_prologue gr Coroutine.all_prologue

