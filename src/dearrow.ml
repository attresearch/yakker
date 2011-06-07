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
open Gul.Curried_constructors

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
open Util.Operators

(* TODO:
   x fix assumption about shape of context on return from calls. Does not take extraneous attributes into account.
   - handle input and output attributes.
   - Consider special-casing environemnt updates when env is empty or singleton. *)

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
  type 'a ctxt

  val ctxt_size : 'a ctxt -> int
  val is_empty : 'a ctxt -> bool

  val demote : sf ctxt -> pl ctxt
    (** [demote g] coerces a shadowing-free context to a normal one. *)

  val empty : sf ctxt
  val singleton_var : var -> ty -> sf ctxt
  val singleton_attr : var -> ty -> sf ctxt
  val ext_var : 'a ctxt -> var -> ty -> pl ctxt
  val ext_attr : 'a ctxt -> var -> ty -> pl ctxt

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

  let ctxt_size = List.length
  let is_empty = function [] -> true | _ -> false

  let demote g = g

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
type bindq = No_bind | Bind | Var_bind of string | Return_bind

(* TODO: improve error reporting for this check. *)
let check_ctxt_eq g1 g2 =
  if not (g1 = g2) then Util.sys_error "Contexts not equal."

let transform gr =

  (** a global map from ordered lists of types to
      constructors. *)
  let con_table : (ty list, constructor) Hashtbl.t = Hashtbl.create 11 in
  Hashtbl.add con_table [] "Ykctxt_empty";
  let con_map  (l : ty list) : constructor =
    match Util.find_option con_table l with
      | None ->
          let c = "Ykctxt" ^ Variables.fresh () in
          Hashtbl.add con_table l c;
          c
      | Some c -> c in

  let nt_attrs = Gul.attribute_table_of_definitions gr.ds in


  (** An abbreviation for con_map . tys_of_ctxt *)
  let mt g = (con_map (tys_of_ctxt g)) in

  let nm = names_of_ctxt in

  (*
    WARNING: tricky, because if we use a wildcard in the pattern, then we can run into
    trouble when trying to reconstruct later. We should distinguish between generating
    patterns and expressions.

    Solution: provide "deshadowing" of contexts.
  *)

  let tuple_pat : pat list -> string =
    String.concat "," in

  let tuple_exp : exp list -> string  =
    tuple_pat in

  (** Convert a context to a string encoding the pattern of variables in the context.
      Variables must be deshadowed first so that the pattern remains linear. *)

  let tuple_pat_of_ctxt : sf ctxt -> string =
    tuple_pat $ nm in

  (** /G/ composes a number of the above operations. *)
  let named_pat_of_ctxt : sf ctxt -> pat =
    fun g ->
      if is_empty g then mt g
      else
        mt g ^ "(" ^ tuple_pat_of_ctxt g ^ ")" in

  let named_exp_of_ctxt : sf ctxt -> exp = named_pat_of_ctxt in


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
  | _ -> failwith \"Expected %s\"" env_pat box_exp some_pat some_exp env_pat in

  let action_template pos_pat env_pat result_exp =
    gen "fun %s -> function %s -> %s | _ -> failwith \"Expected %s\"" pos_pat env_pat result_exp env_pat in

  let merge_template pos_pat env_pat child_pat result_exp = gen "fun %s v1 v2 -> match (v1,v2) with
  | (%s, %s) -> %s
  | _ -> failwith \"Expected %s and %s\"" pos_pat env_pat child_pat result_exp env_pat child_pat in

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
      | Return_bind -> demote empty in

  (* Wrap an expression [e] so that its free variables from [g] are properly bound. *)
  let wrap g e ty =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let c = mt (singleton_var "z" ty) in       (* [z] is arbitrary; we never make use
                                                  of the variable name *)
    action_template "_" pat_in (gen "%s(%s)" c e) in

  let wrapWhen g e =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    action_template "_" pat_in ("("^e^")") in

  (** wrap a box e so that its free variables from G are properly bound.
      result is dropped and environment is propogated. *)
  let boxWrapProp g e =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let x = fresh_wrt $| nm g in
    box_template (gen "(%s as %s)" pat_in x) e "_" x in

  let boxWrapUE g xs a e ty =
    let g1 = drop_these g xs in
    let g_ds = deshadow g in
    let g1_ds = drop_these g_ds xs in

    let pat_in = named_pat_of_ctxt g_ds in

    let _gen some_pat some_exp = box_template pat_in e some_pat some_exp in

    let gen_upd p =
      let p = match p with | "_" -> p | _ -> ds_lookup g1 g1_ds p in
      let env_out = named_exp_of_ctxt g1_ds in
      _gen p env_out in

    let gen_ext =
      let xvar = fresh_wrt $| nm g1_ds in (*  Make sure [xvar] doesn't
                                                         shadow anything that will
                                                         appear in the return value
                                                         (which is based on [g1]). *)
      let c = mt (ext_var g1 "z" ty) in       (* [z] is arbitrary; we never make use
                                                 of the variable name *)

      let env_out = tuple_exp (nm g1_ds @ [xvar]) in
      _gen xvar (gen "%s(%s)" c env_out) in

    let gen_ret =
      let xvar = "x" in
      let c = mt (singleton_var "x" ty) in
      _gen xvar (gen "%s(%s)" c xvar) in

    match a with
      | No_bind -> gen_upd "_"
      | Var_bind x -> if List.mem x (attrs_of_ctxt g1) then gen_upd x else gen_ext
      | Bind -> gen_ext
      | Return_bind -> gen_ret in


  (** [updateEnvP ppat g_ds g xs a e ty]
      pre: [g_ds = deshadow g]. *)
  let updateEnvP ppat g_ds g xs a e ty =
    let g1 = drop_these g xs in
    let g1_ds = drop_these g_ds xs in

    let pat_in = named_pat_of_ctxt g_ds in

    let _gen exp_result = action_template ppat pat_in exp_result in

    let gen_upd p =
      let p = match p with | "_" -> p | _ -> ds_lookup g1 g1_ds p in
      let env_out = named_exp_of_ctxt g1_ds in
      _gen (gen "let %s = %s in %s" p e env_out) in

    let gen_ext =
      let c = mt (ext_var g1 "z" ty) in       (* [z] is arbitrary; we never make use
                                                 of the variable name *)
      let env_out = tuple_exp (nm g1_ds @ [e]) in
      _gen (gen "%s(%s)" c env_out) in

    let gen_ret =
      let c = mt (singleton_var "z" ty) in       (* [z] is arbitrary; we never make use
                                                       of the variable name *)
      let env_out = e in
      _gen (gen "%s(%s)" c env_out) in

    match a with
      | No_bind -> gen_upd "_"
      | Var_bind x -> if List.mem x (attrs_of_ctxt g1) then gen_upd x else gen_ext
      | Bind -> gen_ext
      | Return_bind -> gen_ret in

  let updateEnv g xs a e ty =
    let g_ds = deshadow g in
    updateEnvP "_" g_ds g xs a e ty in

  let updateEnvMerge g xs a ty =
    let ppat = "_" in
    let g_ds = deshadow g in
    let g1 = drop_these g xs in
    let g1_ds = drop_these g_ds xs in

    let pat_in = named_pat_of_ctxt g_ds in

    let _gen pat2 exp_result = merge_template ppat pat_in pat2 exp_result in

    (* code for update when we're ignoring the
       second (non-position) parameter. *)
    let gen_upd_2wild = _gen "_" (named_exp_of_ctxt g1_ds) in

    let gen_upd x =
      let pat_x = match x with
        | "_" -> "_"
        | _ -> named_pat_of_ctxt (singleton_var (ds_lookup g1 g1_ds x) ty) in
      let env_out = named_exp_of_ctxt g1_ds in
      _gen pat_x env_out in

    let gen_ext =
      let xvar = fresh_wrt $| nm g1_ds in
      let pat_x = named_pat_of_ctxt (singleton_var xvar ty) in
      let c = mt (ext_var g1 "z" ty) in       (* [z] is arbitrary; we never make use
                                             of the variable name *)
      let env_out = tuple_exp (nm g1_ds @ [xvar]) in
      _gen pat_x (gen "%s(%s)" c env_out) in

    (* Simply need to propogate the merged value. *)
    let gen_ret =
      let xvar = "x" in
      _gen xvar xvar in

    match a with
      | No_bind -> gen_upd_2wild
      | Var_bind x -> if List.mem x (attrs_of_ctxt g1) then gen_upd x else gen_ext
      | Bind -> gen_ext
      | Return_bind -> gen_ret in

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
    let _do_base g xs a r =
      (* Optimize special case of xs, bind_q *)
      let r_gil = match xs, a with
        | [], No_bind -> r
        | _ -> Gil.Seq (r, Gil.Action (updateEnv g xs a unit_val unit_ty)) in
      updateCtxt g xs a unit_ty, r_gil in
    let do_base r = _do_base g xs bind_q r in
    match r.r with
      | Lit (b,s) -> do_base $| Gil.Lit (b, s)
      | DBranch (e, c) -> do_base $| Gil.DBranch (e, c, "")
      | CharRange (m,n) -> do_base $| Gil.CharRange (m,n)
      | Lookahead (b, r1) -> do_base $| Gil.Lookahead (b, gul2gil r1)

      | Action (Some e, None) ->
          let ty = Util.from_some r.a.inf_type in
          let f = updateEnv g xs bind_q e ty in
          updateCtxt g xs bind_q ty, Gil.Action f

      | When e ->
          let f = match xs, bind_q with
            | [], No_bind -> "fun _ x -> x"
            | _ -> updateEnv g xs bind_q true_val bool_ty in
          updateCtxt g xs bind_q bool_ty, Gil.When (wrapWhen g e, f)

      | Box (e, Some ty, bn) ->
          let f = match xs, bind_q with
            | [], No_bind -> boxWrapProp g e
            | _ -> boxWrapUE g xs bind_q e ty in
          updateCtxt g xs bind_q ty, Gil.Box(f, bn)

      | Box (_, None, _) ->
          Util.impossible "Dearrow.transform._tr.Box"

      (* Version 1 of symbol rules *)
(*       | Symb (nt, e_opt, [], None) -> *)
(*           let ty = Util.from_some r.a.inf_type in *)
(*           let merge = match xs, bind_q with *)
(*             | [], No_bind -> None *)
(*             | _ -> Some (updateEnvMerge g xs bind_q ty) in *)
(*           let wrap_arg e = *)
(*             try *)
(*               let a_nt = Hashtbl.find nt_attrs nt in *)
(*               let arg_ty = Util.from_some a_nt.Attr.early_param_type in *)
(*               wrap g e arg_ty *)
(*             with Not_found -> Util.sys_error *)
(*               (Printf.sprintf "Dearrow.transform._tr.Symb: Symbol %s has parameter but %s has unspecified argument type." *)
(*                  nt nt); "ty_ERROR" in *)
(*           updateCtxt g xs bind_q ty, Gil.Symb (nt, Util.option_map wrap_arg e_opt, merge) *)

      (* Version 2 of symbol rules *)
      | Symb (nt, e_opt, [], None) ->
          let ty = Util.from_some r.a.inf_type in
          let merge = Some (updateEnvMerge g [] Bind ty) in
          let wrap_arg e =
            try
              let a_nt = Hashtbl.find nt_attrs nt in
              let arg_ty = Util.from_some a_nt.Attr.early_param_type in
              wrap g e arg_ty
            with Not_found -> Util.sys_error
              (Printf.sprintf "Dearrow.transform._tr.Symb: Symbol %s has parameter but %s has unspecified argument type."
                 nt nt); "ty_ERROR" in
          let g = updateCtxt g [] Bind ty in
          let xvar = "x" in
          let g, r = _tr (ext_var g xvar ty) (xvar :: xs) bind_q ({r = Action(Some xvar, None);
                                                                   a = {(mkAnnot None) with inf_type = Some ty}}) in
          g, Gil.Seq (Gil.Symb (nt, Util.option_map wrap_arg e_opt, merge), r)

      | Assign (r, Some x, None) ->
          if List.mem x $| vars_of_ctxt g then
            begin
              Util.sys_error ("Cannot assign " ^ x ^ " as attribute, because the name is already bound as a lexically-scoped variable.");
              raise Exit
            end;
          let g', r1 = _tr g xs (Var_bind x) r in
          let r_gil = match bind_q with
            | No_bind -> r1
            | _ -> Gil.Seq (r1, Gil.Action (updateEnv g' [] bind_q unit_val unit_ty)) in
          updateCtxt g' [] bind_q unit_ty, r_gil

      | Seq (r1, None, None, r2) ->
          let g1, r1 = _tr g [] No_bind r1 in
          let g2, r2 = _tr g1 xs bind_q r2 in
          g2, Gil.Seq (r1,r2)

      | Seq (r1, Some x, None, r2) ->
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

      | Star (Bounds (0, Infinity), r1) ->
          let _, r1 = _tr g [] No_bind r1 in
          do_base (Gil.Star r1)

      | Position true ->
          let r_gil = match xs, bind_q with
            | [], No_bind -> Gil.Lit (false, "")
            | _ ->
                let g_ds = deshadow g in
                let p = fst $ freshen_wrt "p" 1 $| nm g in
                let f = updateEnvP p g_ds g xs bind_q p int_ty in
                Gil.Action f in
          updateCtxt g xs bind_q unit_ty, r_gil

      | Delay _ ->
          Util.todo "Dearrow.transform._tr.Delay: Not yet supported."
      | Symb (_, _, _::_, None) ->
          Util.todo "Dearrow.transform._tr.Symb: input attributes not yet supported."
      | Star (Accumulate _, _) ->
          Util.todo "Dearrow.transform._tr.Star: star with accumulate not yet supported."
      | Star (Bounds _, _) ->
          Util.todo "Dearrow.transform._tr.Star: star with closed bounds not yet supported."

      (* These late cases should have been desugared: *)
      | Action (_, Some _) ->
          Util.impossible "Dearrow.transform._tr.Action: late actions should have been desugared."
      | Action (None, None) ->
          Util.impossible "Dearrow.transform._tr.Action: Action(None,None) should never occur."
      | Symb (_, _, _, Some _) ->
          Util.impossible "Dearrow.transform._tr.Symb: late arguments should have been desugared."
      | Seq (_, _, Some _, _) ->
          Util.impossible "Dearrow.transform._tr.Seq: late bindings should have been desugared."
      | Position false ->
          Util.impossible "Dearrow.transform._tr.Position: late position should have been desugared."
      | Assign (_, _, Some _) ->
          Util.impossible "Dearrow.transform._tr.Assign: late assign should have been desugared."
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
                 let _, r = match a.Attr.early_params with
                   | None -> _tr (demote empty) [] Return_bind r
                   | Some s ->
                       let x = get_param s in
                       let ty = Util.from_some a.Attr.early_param_type in
                       let g = singleton_var x ty in
                       _tr (demote g) [x] Return_bind r in
                 [(n, r)]
             | _ -> [])
          gr.ds;
  let free_tyvars = Util.remove_dups $| Hashtbl.fold begin fun tys _ ft ->
    List.rev_append (List.filter Ty_infer.is_tyvar tys) ft
  end con_table [] in
  begin match free_tyvars with
    | [] -> add_to_prologue gr $| Printf.sprintf "type _sv = \n"
    | [x] -> add_to_prologue gr $| Printf.sprintf "type %s _sv = \n" x
    | _ -> add_to_prologue gr $| Printf.sprintf "type (%s) _sv = \n" (String.concat " , " free_tyvars)
  end;
  Hashtbl.iter (fun tys c -> match tys with
                  | [] -> add_to_prologue gr $| Printf.sprintf "| %s\n" c
                  | _ -> add_to_prologue gr $| Printf.sprintf "| %s of %s\n" c (String.concat " * " tys)) con_table;
  add_to_prologue gr $| Printf.sprintf "let sv0 = Ykctxt_empty
(* TODO: tag environments, just like we do coroutines, to avoid full-blown comparison. *)
let sv_compare = compare\n";
