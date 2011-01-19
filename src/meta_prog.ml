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

(**
   General DeBruin-levels machinery
*)
module DB_levels = struct

  type exp =
  | InjectE of string
  | True
  | False
  | Var of int
  | App of exp * exp
  | Lam of exp
  | If of exp * exp * exp

  let app2 f x y = App (App (f,x), y)
  let app3 f x y z = App (App (App (f, x), y), z)
  let letx e1 e2 = App (Lam e2, e1)
  let case e ms = App (InjectE ("function " ^ ms), e)

  let identity_fun = Lam (Var 0)

  let exp_map f e =
    let rec walk c = function
      | (True | False | InjectE _) as e -> e
      | Var i -> f c i
      | Lam f -> Lam (walk (c+1) f)
      | App (e1, e2) -> App (walk c e1, walk c e2)
      | If (e1, e2, e3) -> If (walk c e1, walk c e2, walk c e3)
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

  (** Beta-reduce/case-eliminate where possible *)
  let simplify e =
    let rec simplify' c = function  (* c is the count of binders *)
      | (True | False | InjectE _) as e -> e
      | If (e1, e2, e3) ->
          let e1 = simplify' c e1 in
          (match e1 with
             | True -> simplify' c e2
             | False -> simplify' c e3
             | _ -> If (e1, simplify' c e2, simplify' c e3))
      | App (e1, e2) ->
          let e1 = simplify' c e1 in
          let e2 = simplify' c e2 in
          (match e1 with
             | Lam e -> simplify' c (beta_reduce c e e2)
             | _ -> App (e1, e2))
      | Var i -> Var i
      | Lam f -> Lam (simplify' (c+1) f)
    in simplify' 0 e

  (** check that the expression has at most [n] free variables.*)
  let rec check_free_at_most n = function
    | True | False | InjectE _ -> true
    | Var i -> i < n
    | Lam f -> check_free_at_most (n+1) f
    | App (e1, e2) -> check_free_at_most n e1 && check_free_at_most n e2
    | If (e1, e2, e3) -> check_free_at_most n e1
        && check_free_at_most n e2
        && check_free_at_most n e3

  let is_closed = check_free_at_most 0

  let to_string =
    let rec recur c = function
      | True -> "true"
      | False -> "false"
      | Lam f ->
          Printf.sprintf "(lam. %s)" (recur (c+1) f)
      | Var x -> if x < c then Printf.sprintf "%d" x else Printf.sprintf "'%d" x
      | App (e1,e2) -> Printf.sprintf "(%s %s)" (recur c e1) (recur c e2)
      | If (e1, e2, e3) -> Printf.sprintf "(if %s then %s else %s)" (recur c e1) (recur c e2) (recur c e3)
      | InjectE s -> Printf.sprintf "(%s)" s in
    recur

end

module DBL = DB_levels

(******************************************************************************)

module PHOAS = struct
  type 'a exp =
    Var of 'a
  | App of 'a exp * 'a exp
  | Lam of ('a -> 'a exp)
  | InjectE of string
  | True
  | False
  | If of 'a exp * 'a exp * 'a exp

  type hoas_exp = {e:'a. unit -> 'a exp}
  type open1_hoas_exp = {e_open1:'a. 'a -> 'a exp}
  type open2_hoas_exp = {e_open2:'a. 'a -> 'a -> 'a exp}
  type open3_hoas_exp = {e_open3:'a. 'a -> 'a -> 'a -> 'a exp}

  (** Sugar for a let expression. An alternative to actually applying.
      Useful for promoting sharing of results (given the CBV semantics. *)
  let let_e e1 e2 = App (e2, e1)

  let rec to_dB level = function
    | True -> DBL.True
    | False -> DBL.False
    | Var i -> DBL.Var i
    | Lam f -> DBL.Lam (to_dB (level+1) (f level))
    | App (e1, e2) -> DBL.App (to_dB level e1, to_dB level e2)
    | If (e1, e2, e3) -> DBL.If (to_dB level e1, to_dB level e2, to_dB level e3)
    | InjectE s -> DBL.InjectE s

  (* val subst' : 'a exp exp -> 'a exp *)
  let rec subst' = function
    | True -> True
    | False -> False
    | Var e -> e
    | Lam f -> Lam (fun x -> subst' (f (Var x)))
    | App (e1, e2) -> App (subst' e1, subst' e2)
    | If (e1, e2, e3) -> If (subst' e1, subst' e2, subst' e3)
    | InjectE s -> InjectE s

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
    | DBL.True -> True
    | DBL.False -> False
    | DBL.Var i -> Var (get_var ctxt i)
    | DBL.Lam f -> Lam (fun x -> from_dB (extend_ctxt ctxt x) f)
    | DBL.App (e1, e2) -> App (from_dB ctxt e1, from_dB ctxt e2)
    | DBL.If (e1, e2, e3) -> If (from_dB ctxt e1, from_dB ctxt e2, from_dB ctxt e3)
    | DBL.InjectE s -> InjectE s

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

  let mk_var c = Printf.sprintf "_x%d_" c
  let to_string' =
    let rec recur c = function
      | Var x -> x
      | InjectE s -> Printf.sprintf "(%s)" s
      | True -> "true"
      | False -> "false"
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
      | App (e1,e2) -> Printf.sprintf "(%s %s)" (recur c e1) (recur c e2)
      | If (e1, e2, e3) -> Printf.sprintf "(if %s then %s else %s)" (recur c e1) (recur c e2) (recur c e3)
    in
    recur

  let to_string {e=f} = to_string'  0 (f())

  let app2 x y z = App (App (x,y), z)
  let app3 f x y z = App (App (App (f, x), y), z)
  let lam2 f = Lam (fun x -> Lam (f x))
  let lam3 f = Lam (fun x -> lam2 (f x))

  let letx e1 e2 = App (Lam e2, e1)

end
