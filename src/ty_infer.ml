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

(* Infer types and wrap and unwrap semantic values across early calls.

   Early arguments and return values need a uniform type because they
   end up in homogeneous data structures (like a semantic value stack).

   For detailed notes on the design of the code in this module please see:

       https://yakker.research.att.com/wiki/Notes-on-the-arrow-notation-transformation

 *)

open Yak
open Gul


let unit_ty = "unit"
let int_ty = "int"
let fresh_tyvar () = "'yk" ^ Variables.fresh ()
let is_tyvar x = String.length x > 1 && x.[0] = '\''


type uterm =
  | UVar of string
  | UCon of string (** only unary constructors currently *)

let string_of_uterm = function | UVar s | UCon s -> s

let inject_uterm s = if is_tyvar s then UVar s else UCon s

let no_cons = [] (** no constraints *)
let add_constraint cs x y = (inject_uterm x, inject_uterm y)::cs

(** Make a singleton constraint: *)
let mk_constraint = add_constraint no_cons
let merge_constraints = (@)
let cons_print c =
  List.iter (fun (t1,t2) -> Printf.printf "   %s = %s\n" (string_of_uterm t1) (string_of_uterm t2)) c

type substitution = (string * uterm) list
let subs_empty : substitution     = []
let subs_ext s x v : substitution = (x,v)::s
let subs_lookup_try (s : substitution) x : uterm = List.assoc x s
let subs_lookup (s : substitution) x : uterm option =
  try Some (List.assoc x s) with Not_found -> None

let subs_print s =
  List.iter (fun (x,t) -> Printf.eprintf "   %s -> %s\n" x (string_of_uterm t)) s

(** Expand the substitution [s] once with itself. *)
let subs_expand_once s =
  (* Use a fixpoint iteration. *)
  let expd = function
    | (x, UCon _) as m -> m, false
    | (x, UVar y) as m -> begin
        match subs_lookup s y with
          | None -> m, false
          | Some v -> (x, v), true
      end in
  let (s, changes) = List.split (List.map expd s) in
  s, List.mem true changes

(** Expand the substituation out of triangle form. The intent is to
    ensure that only free variables appear in the right sides. *)
let subs_expand =
  let rec _e s =
    let s, changed = subs_expand_once s in
    if changed then _e s else s in
  _e

let subs_free s =
  let free vs = function
    | (x, UCon _) -> vs
    | (x, UVar y) ->
        match subs_lookup s y with
          | None -> y::vs
          | Some v -> vs in
  let vs = List.fold_left free [] s in
  Util.remove_dups vs

let subs_vars s =
  let vars vs = function
    | (x, UCon _) -> vs
    | (x, UVar y) -> y::vs in
  let vs = List.fold_left vars [] s in
  Util.remove_dups vs


(* while [u] is a variable bound in substitution [s],
   repeatedly apply [s] to [u]. *)
let rec rep_sub s u =
  match u with
    | UVar x -> begin
        match subs_lookup s x with
          | None -> u
          | Some v -> rep_sub s v
      end
    | otherwise -> u

type uresult = UFailure of uterm * uterm | USuccess of substitution

let unify c =
  let rec _u s = function
    | [] -> USuccess s
    | (u, v) :: c ->
        let u = rep_sub s u in
        let v = rep_sub s v in
        if u = v then _u s c
        else begin
          match (u, v) with
            | (UVar x, UVar y) -> _u (subs_ext s x v) c
            | (UVar x, v) -> _u (subs_ext s x v) c
            | (u, UVar x) -> _u (subs_ext s x u) c
            | (UCon _, UCon _) -> (* must be different b/c we already checked for equality. *)
                UFailure (u, v)
        end
  in
  _u subs_empty c

module Context = struct
  type t = (var * ty) list
  let empty = []
  let ext g v t = (v,t)::g
end

module C = Context

let ty_annot r ty = {r with a = {r.a with inf_type = Some ty}}

let inf_ty_of_expr (g : C.t) e = fresh_tyvar ()
let nonterm_tyvar nt = "'yk_" ^ Variables.bnf2ocaml nt ^ "_result"
let nonterm_arg_tyvar nt = "'yk_" ^ Variables.bnf2ocaml nt ^ "_arg"


let elaborate g r =
  let rec _e g r =
    match r.r with
      | Lit _
      | CharRange _
      | Prose _
      | When _
      | Delay _
      | DBranch _ | Position false
          -> r, unit_ty, no_cons
      | Lookahead (b,r1) ->
          let r1, ty1, c1 = _e g r1 in
          {r with r = Lookahead (b,r1)},
          unit_ty,
          c1
      | Position true -> r, int_ty, no_cons
      | Action (Some early, _) ->
          let ty = inf_ty_of_expr g early in
          ty_annot r ty, ty, no_cons
      | Action (None,_) -> r, unit_ty, no_cons
      | Box (e, Some ty, bn) -> r, ty, no_cons
      | Box (e, None, bn) ->
          let x = fresh_tyvar () in
          {r with r = Box (e, Some x, bn)}, x, no_cons
      | Symb (nt, ea_opt, attrs, la_opt) ->
          let ty = nonterm_tyvar nt in
          let constraints = match ea_opt with
            | None -> no_cons
            | Some e -> mk_constraint (nonterm_arg_tyvar nt) (inf_ty_of_expr g e) in
          ty_annot r ty, ty, constraints
      | Opt r1 ->
          let r1, ty1, c1 = _e g r1 in
          mkOPT r1, ty1, add_constraint c1 ty1 unit_ty
      | Seq (r1, Some x, l_opt, r2) ->
          let r1, ty1, c1 = _e g r1 in
          let r2, ty2, c2 = _e (C.ext g x ty1) r2 in
          {r=Seq(r1, Some x, l_opt, r2);
           a={r.a with inf_type = Some ty1}} (* hijack the annotation to refer to the
                                                type of variable x. *)
          ,ty2
          ,merge_constraints c1 c2
      | Seq (r1, None, l_opt, r2) ->
          let r1, ty1, c1 = _e g r1 in
          let r2, ty2, c2 = _e g r2 in
          {r with r=Seq(r1, None, l_opt, r2)}
          ,ty2
          ,merge_constraints c1 c2
      | Alt (r1, r2) ->
          let r1, ty1, c1 = _e g r1 in
          let r2, ty2, c2 = _e g r2 in
          {r with r=Alt(r1,r2)}
          ,ty2
          ,add_constraint (merge_constraints c1 c2) ty1 ty2
      | Star (Accumulate (Some (x,e), l_opt) as l, r1) ->
          let ty0 = inf_ty_of_expr g e in
          let r1, ty1, c1 = _e (C.ext g x ty0) r1 in
          {r=Star(l, r1);
           a={r.a with inf_type = Some ty1}} (* hijack the annotation to refer to the
                                                type of variable x. *)
          ,ty1
          ,add_constraint c1 ty0 ty1
      | Star (l, r1) ->
          let r1, ty1, c1 = _e g r1 in
          {r with r=Star(l, r1)}
          ,unit_ty
          ,add_constraint c1 ty1 unit_ty
      | Assign (r1, Some x, lopt) ->
          let r1, ty1, c1 = _e g r1 in
          {r with r=Assign (r1, Some x, lopt)}
          ,unit_ty
          ,c1

      | otherwise -> r, unit_ty, no_cons in
  _e g r

let subs_apply_ty s ty = Util.option ty string_of_uterm (subs_lookup s ty)
let subs_apply_attr s a = {a with inf_type = Util.option_map (subs_apply_ty s) a.inf_type}

let subs_apply s r =
  let rec _s r =
    match r.r with
      | Box (e, Some ty, bn) ->
          let ty = subs_apply_ty s ty in
          {r with r = Box (e, Some ty, bn)}
      | Box (e, None, bn) -> r

      | Lit _
      | CharRange _
      | Lookahead _
      | Prose _
      | Delay _ | When _
      | DBranch _ | Position _
      | Action _
      | Symb _ -> {r with a = subs_apply_attr s r.a}
      | Opt r1 -> {r with r = Opt (_s r1)}

      | Seq (r1, e, l, r2) ->
          {r = Seq (_s r1, e, l, _s r2);
           a = subs_apply_attr s r.a}
      | Assign (r1, x, y) -> {r with r = Assign (_s r1, x, y)}
      | Alt (r1, r2) -> {r with r = Alt (_s r1, _s r2)}
      | Star (l, r1) -> {r = Star(l,_s r1);
                         a = subs_apply_attr s r.a}
      | otherwise -> {r with a = subs_apply_attr s r.a} in
  _s r

let debug gr =
  let ds = List.map
    (function
        RuleDef(nt, r, a) ->
          (match a.Attr.early_params with
          | None ->
              let r, ty, constraints = elaborate C.empty r in
              Printf.printf "\n%s:\n" nt;
              cons_print constraints;
              let s = match unify constraints with
                | UFailure (t1,t2) -> failwith "constraint-solving failed"
                | USuccess s -> subs_expand s in
              subs_print s;
(*               let r = subs_apply s r in *)
              RuleDef(nt, r, a)
          | Some x ->
              let r, ty, constraints = elaborate (C.ext C.empty (get_param x) (nonterm_arg_tyvar nt)) r in
              Printf.printf "\n%s:\n" nt;
              cons_print constraints;
              let s = match unify constraints with
                | UFailure (t1,t2) -> failwith "constraint-solving failed"
                | USuccess s -> subs_expand s in
              subs_print s;
(*               let r = subs_apply s r in *)
              RuleDef(nt, r, a))
      | x -> x)
    gr.ds in
  gr.ds <- ds

let infer print_subs gr =
  let cs, ds = List.split begin
    List.map
      (function
        RuleDef(nt, r, a) ->
          (match a.Attr.early_params with
          | None ->
              let r, ty, constraints = elaborate C.empty r in
              let constraints = add_constraint constraints ty (nonterm_tyvar nt) in
              constraints, RuleDef(nt, r, a)
          | Some x ->
              let r, ty, constraints = elaborate (C.ext C.empty (get_param x) (nonterm_arg_tyvar nt)) r in
              let constraints = add_constraint constraints ty (nonterm_tyvar nt) in
              (* we don't explicitly annotate argument with type variable, b/c there isn't
                 any existing field for us to use. We leave it implicit. *)
              constraints, RuleDef(nt, r, a))
      | x -> [], x)
    gr.ds
  end in
  let c = List.flatten cs in
  let s = match unify c with
    | UFailure (t1,t2) -> failwith "constraint-solving failed"
    | USuccess s -> subs_expand s in
  if print_subs then
    begin
      Printf.eprintf "\nSubstitution:\n";
      subs_print s;
    end;
  let ds =
    List.map begin function
      | RuleDef(nt, r, a) ->
          (* Add argument-type annotation, as applicable. *)
          let a =
            {a with Attr.early_param_type =
                match a.Attr.early_params with
                  | None -> None
                  | Some _ -> Some (string_of_uterm (subs_lookup_try s (nonterm_arg_tyvar nt)))
            } in
          RuleDef(nt, subs_apply s r, a)
      | x -> x
    end
    ds in
  gr.ds <- ds
