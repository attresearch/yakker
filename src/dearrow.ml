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

(*


*)

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
  val singleton : var -> ty -> sf ctxt
  val ext : 'a ctxt -> var -> ty -> pl ctxt

  (** If context is already shadow-free, guaranteed not to change. *)
  val deshadow: 'a ctxt -> sf ctxt

  (** [drop_n g n] drops the last [n] members of the environment g.
      If [n] is greater than the size of [g], then the empty environment
      is returned. *)
  val drop_n : 'a ctxt -> int -> 'a ctxt


  val tys_of_ctxt : 'a ctxt -> ty list
    (** [tys_of_ctxt g] provides the types of variables in [g]
        as an ordered list (including duplicates)  *)

  val vars_of_ctxt : 'a ctxt -> var list
    (** [vars_of_ctxt g] provides the variable names in [g]
        as an ordered list (including duplicates)  *)
end

module StringSet = Set.Make(String)
open Util.Operators

module Context : CONTEXT = struct

  (** Contexts are encoded *in reverse*.  *)


  (* We leave the first two types abstract -- they are only used for
     static enforcement of invariants, and have no real meaning. *)
  type pl                               (** "plain" - normal context, might have shadowed variables. *)
  type sf                               (** shadowing-free *)
  type 'a ctxt = (var * ty) list

  let empty = []
  let singleton v t = [(v,t)]
  let ext g v t = (v,t)::g

  let ctxt_size = List.length
  let is_empty = function [] -> true | _ -> false

  let demote g = g

  (* Replaces all shadowed bindings with fresh names.
     If context is already shadow-free, guaranteed not to change. *)
  let deshadow g =
    let freshen = Variables.freshn in
    let maybe_rename (revg, vars) (x,t) =
      if StringSet.mem x vars
      then (freshen x, t)::revg, vars
      else (x,t) :: revg, StringSet.add x vars in
    let revg, _ = List.fold_left maybe_rename ([], StringSet.empty) g in
    List.rev revg

  (** [drop_n g n] drops the last [n] members of the environment g.
      If [n] is greater than the size of [g], then the empty environment
      is returned. *)
  let drop_n g n = Util.list_drop n g


  let vars_of_ctxt g = (List.rev $ fst $ List.split) g

  let tys_of_ctxt g = (List.rev $ snd $ List.split) g

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
  let gen = Printf.sprintf
end

open Meta_prog

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


  (** An abbreviation for con_map . tys_of_ctxt *)

  let mt g = (con_map (tys_of_ctxt g)) in


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
    tuple_pat $ vars_of_ctxt in

  (** Convert a context to a string encoding an expression for
      constructing an environment based on the variables in the
      context.  Variables must be deshadowed first so that the env. is
      reconstructed correctly. *)

  let tuple_exp_of_ctxt : sf ctxt -> string =
    tuple_pat_of_ctxt in


  (** Generate a list of variables names [x_1] ... [x_n],
      where [n] is the argument. *)

  let vars_of_n : int -> exp list =
    fun n -> Util.list_make n (gen "x%d") in

  (** Generate a tuple of variables names [x_1] ... [x_n],
      where [n] is the argument. *)

  let tuple_of_n : int -> string =
    tuple_exp $ vars_of_n in


  (** /G/ composes a number of the above operations. *)
  let named_pat_of_ctxt : sf ctxt -> pat =
    fun g ->
      if is_empty g then mt g
      else
        mt g ^ "(" ^ tuple_pat_of_ctxt g ^ ")" in

  (** Build a pattern representing an environment from a context, paying
      attention only to the types of the context's bindings, but not the
      names. Use names [x_1] ... [x_m], where [m] is the size of the
      context. Important for propogating values even when there's
      shadowing in the context. *)
  let numbered_pat_of_ctxt : 'a ctxt -> pat =
    fun g ->
      match ctxt_size g with
        | 0 -> mt g
        | n -> mt g ^ "(" ^ tuple_of_n n ^ ")" in

  let numbered_exp_of_ctxt : 'a ctxt -> exp =
    numbered_pat_of_ctxt in

  (* Wrap an expression [e] so that its free variables from [g] are properly bound. *)
  let wrap g e =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    gen "fun _ -> function %s -> (%s) | _ -> failwith \"Expected %s\"" pat_in e pat_in in

  (** the template for box code. *)
  let box_templ env_pat box_exp some_pat some_exp =
    gen "function
  | %s ->
    let f = %s in
    fun input pos ->
      begin match f input pos with
      | None -> None
      | Some (n, %s) -> Some (n, %s)
      end
  | _ -> failwith \"Expected %s\"" env_pat box_exp some_pat some_exp env_pat in

  (** wrap a box e so that its free variables from G are properly bound.
      Does not take care of environment manipulations.
      **Just for illustration**. *)
  let boxWrap g e ty =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let v_c = con_map [ty] in
    box_templ pat_in e "x" (v_c ^ " x") in

  (** wrap a box e so that its free variables from G are properly bound.
      result is dropped and environment is propogated. *)
  let boxWrapProp g e =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let x = Variables.fresh () in
    box_templ (gen "(%s as %s)" pat_in x) e "_" x in

  (** wrap a box e so that its free variables from G are properly bound.
      Drop result and [k] elements from environment. *)
  let boxWrapDropK g e k =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let exp_out = named_pat_of_ctxt (drop_n g k) in
    box_templ pat_in e "_" exp_out in

  (** wrap a box e so that its free variables from G are properly bound.
      Drop result and [k] elements from environment. *)
  let boxWrapReplaceK g e k ty =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let g' = drop_n g k in
    let c = mt (ext g' "z" ty) in       (* [z] is arbitrary; we never make use
                                           of the variable name *)
    let exps_g' = vars_of_ctxt g' in
    let env_out = tuple_exp (exps_g' @ ["x"]) in
    box_templ pat_in e "x" (gen "%s(%s)" c env_out) in

  (**  drop the last k elements of an environment G of length m. *)
  let dropK g k =
    let pat_in = numbered_pat_of_ctxt g in
    let exp_out = numbered_exp_of_ctxt (drop_n g k) in
    gen "fun _ -> function %s -> %s | _ -> failwith \"Expected %s\"" pat_in exp_out pat_in in

  (** [replaceKp ppat g k e ty] replaces the last [k] elements of an
      environment corresponding to [g] with expression [e] of type
      [ty]. [ppat] specifies the pattern to be used for the position variable. *)
  let replaceKp ppat g k e ty =
    let pat_in = numbered_pat_of_ctxt g in
    let g' = drop_n g k in
    let c = mt (ext g' "z" ty) in       (* [z] is arbitrary; we never make use
                                                of the variable name *)
    let vars_g' = vars_of_n (ctxt_size g') in
    let env_out = tuple_exp (vars_g' @ [e]) in
    gen "fun %s -> function %s -> %s(%s) | _ -> failwith \"Expected %s\""
      ppat pat_in c env_out pat_in in

  (** Like [replaceK], but [ppat] is a wildcard. *)
  let replaceK = replaceKp "_" in

  (** generate a merge function that drops the newly generated value
      and k elements from the current environment. *)
  let dropKMerge g k =
    let pat_in = numbered_pat_of_ctxt g in
    let pat_out = numbered_pat_of_ctxt (drop_n g k) in
    Printf.sprintf "fun _ v1 _ -> match v1 with %s -> %s | _ -> failwith \"Expected %s\""
      pat_in pat_out pat_in in

  (** [replaceKMerge g k ty] generates a merge function that drops
      [k] elements from environment [g] of length [m] and extends [g]
      with a new value of type [ty], taken from the function's second argument. *)
  let replaceKMerge g k ty =
    let pat_v1 = numbered_pat_of_ctxt g in
    let v2_var = "x" in
    let pat_v2 = named_pat_of_ctxt (singleton v2_var ty) in
    let g' = drop_n g k in
    let c = mt (ext g' "z" ty) in       (* [z] is arbitrary; we never make use
                                           of the variable name *)
    let vars_g' = vars_of_n (ctxt_size g') in
    let env_out = tuple_exp (vars_g' @ [v2_var]) in
    Printf.sprintf "fun _ v1 v2 -> match (v1,v2) with
  | (%s, %s) -> %s(%s)
  | _ -> failwith \"Expected %s and %s\"" pat_v1 pat_v2 c env_out pat_v1 pat_v2 in


  let rec _tr (g : pl ctxt) k bind_q r =
    let do_base r =
      match k, bind_q with
        | 0, false -> r
        | _, false -> Gil.Seq (r, Gil.Action (dropK g k))
        | _, true  -> Gil.Seq (r, Gil.Action (replaceK g k unit_val unit_ty)) in
    match r.r with
      | Lit (b,s) -> do_base $| Gil.Lit (b, s)
      | DBranch (e, c) -> do_base $| Gil.DBranch (e, c, "")
      | CharRange (m,n) -> do_base $| Gil.CharRange (m,n)
      | Lookahead (b, r1) ->
          let r1 = _tr (demote empty) 0 false r1 in
          do_base $| Gil.Lookahead (b, r1)

      | Action (Some e, None) ->
          let g = deshadow g in
          let pat_in = named_pat_of_ctxt g in
          let e = match k, bind_q with
            | 0, false ->
                let x = Variables.fresh () in
                gen "fun _ -> function (%s as %s) -> %s; %s | _ -> failwith \"Expected %s\""
                  pat_in x e x pat_in
            | _, false ->
                let exp_out = named_pat_of_ctxt (drop_n g k) in
                gen "fun _ -> function %s -> %s; %s | _ -> failwith \"Expected %s\"" pat_in e exp_out pat_in
            | _, true ->
                let g' = drop_n g k in
                let ty = Util.from_some r.a.inf_type in
                let c = mt (ext g' "z" ty) in       (* [z] is arbitrary; we never make use
                                                       of the variable name *)
                let exps_g' = vars_of_ctxt g' in
                let env_out = tuple_exp (exps_g' @ [e]) in
                gen "fun _ -> function %s -> %s(%s) | _ -> failwith \"Expected %s\"" pat_in c env_out pat_in in
          Gil.Action e

      | When e ->
          let f = match k, bind_q with
            | 0, false -> "fun _ x -> x"
            | _, false -> dropK g k
            | _, true  -> replaceK g k "true" bool_ty in
          Gil.When (wrap g e, f)

      | Box (e, Some ty, bn) ->
          let e = match k, bind_q with
            | 0, false -> boxWrapProp g e
            | _, false -> boxWrapDropK g e k
            | _, true  -> boxWrapReplaceK g e k ty in
          Gil.Box(e, bn)

      | Box (_, None, _) ->
          Util.impossible "Dearrow.transform._tr.Box"

      | Symb (nt, e_opt, [], None) ->
          let merge = match k, bind_q with
            | 0, false -> None
            | _, false -> Some (dropKMerge g k)
            | _, true  -> Some (replaceKMerge g k (Util.from_some r.a.inf_type)) in
          Gil.Symb (nt, Util.option_map (wrap g) e_opt, merge)

      | Seq(r1, Some x, late, r2) ->
          let ty = Util.from_some r.a.inf_type in
          let r1 = _tr g 0 true r1 in
          let r2 = _tr (ext g x ty) (k+1) bind_q r2 in
          Gil.Seq (r1,r2)
      | Seq(r1, None, late, r2) ->
          let r1 = _tr g 0 false r1 in
          let r2 = _tr g k bind_q r2 in
          Gil.Seq (r1,r2)

      | Alt(r1, r2) ->
          let r1 = _tr g k bind_q r1 in
          let r2 = _tr g k bind_q r2 in
          Gil.Alt (r1,r2)

      (* Given lifting, Opt and Star(Bounds...) both must carry irrelevant subterms.
         Hence, we treat them like base cases. *)
      | Opt(r1) ->
          let r1 = _tr g 0 false r1 in
          do_base (Gil.Alt(Gil.Lit(false,""), r1))

      | Star(Bounds (0, Infinity), r1) ->
          let r1 = _tr g 0 false r1 in
          do_base (Gil.Star r1)

      | Position true ->
          let f = match k, bind_q with
            | 0, false -> "fun _ x -> x"
            | _, false -> dropK g k
            | _, true  -> let p = Variables.freshn "p" in replaceKp p g k p int_ty in
          Gil.Action f

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
                 let r = match a.Attr.early_params with
                   | None -> _tr (demote empty) 0 true r
                   | Some s ->
                       let x = get_param s in
                       let ty = Util.from_some a.Attr.early_param_type in
                       let g = singleton x ty in
                       _tr (demote g) 1 true r in
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
