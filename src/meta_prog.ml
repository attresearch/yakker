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

open Yak.Util.Operators

let concat = List.concat
let map = List.map
let rev = List.rev
let foldl = List.fold_left
let foldr = List.fold_right
let filter = List.filter
let length = List.length
let mem = List.mem

module Core = struct

  type con = string
  type var = int
  type patt = con * int

  module Var_set = Set.Make(struct type t = var let compare = (-) end)
  module Vs = Var_set

  type exp =
  | App of var * var list
  | Con of con * var list
  | Let of exp list * var
  | Case of var * (patt * exp) list
  | Lam of exp

  let var i = App (i,[])

  (** Return the free variables of [e], assuming variables [0 .. n - 1] are free. *)
  let fv e n =
    let free = filter (fun x -> x < n) in
    let rec loop = function
      | App (v, vs) -> free (v::vs)
      | Con (_, vs) -> free vs
      | Lam f -> loop f
      | Case (v, cs) ->
          free (v :: (concat $ map (loop $ snd) $| cs))
      | Let (bs, v) -> free (v :: (concat $ map loop $| bs)) in
    Yak.Util.remove_dups (loop e)

  let ($<) f g x = f x $ g

(*    (\** [n] = binder count. *\) *)
(*   let strict_vars e n = *)
(*     let free v = v < n in *)
(*     let rec loop = function *)
(*       | App (f, args) -> Vs.add $| Vs.singleton f (Vs.from_list args) *)
(*       | Con (_, vs) -> Vs.from_list args *)
(*       | Lam _ -> Vs.empty *)
(*       | Let (es, v) -> foldl (Vs.union $< loop) $| Vs.singleton v $| es *)
(*       | Case (v, cs) -> let xs = match cs with [] -> Vs.empty | x::xs -> foldl (Vs.intersection $< loop $ snd) x xs *)
(*           Vs.add $| xs $| v *)

  let var_map f e =
    let rec walk c = function
      | App (v, vs) -> App (f c v, map (f c) vs)
      | Con (con, vs) -> Con (con, map (f c) vs)
      | Lam f -> Lam (walk (c + 1) f)
      | Case (v, cs) ->
          let g ((con, n), e) = (con, n), walk (c+n) e in
          Case (f c v, map g cs)
      | Let (bs, v) ->
          let g (es, n) e = (walk (c + n) e :: es, n + 1) in
          let es, n = foldl g ([], c) bs in
          Let (rev es, f n v) in
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
  *)
  (*
    (A note regarding the wildcard on the first argument of the
    function supplied to [exp_map]. This argument, [c], is the count
    of binders traversed during the map process.
    Now, every variable should be in 0..c + mf, because free variables should only
    be taken from 0..mf and variables i, mf < i <= c + mf, are bound variables.
    So, we can ignore c.)
  *)
  let shift mf n e =
    if n = 0 then e else
      let f _ i_var = if mf < i_var then i_var + n else i_var in
      var_map f e

(* The following is not clearly useful
   because the language is not pure. *)
(*   let remove_unused_bindings c es v = *)
(*     (\* [used_vars] will be the set of variables in *)

(*        [c .. c + length es - 1] *)

(*        which appear free in [es]. Note that we take into account *)
(*        that the set of locally let-bound variables is growing as *)
(*        we move down the list of bindings. Also note that the *)
(*        first expression cannot have any locally let-bound *)
(*        variables because we do not support recursion. However, *)
(*        we do not take the trouble to remove it from [es] first. *\) *)
(*     let used_vars = *)
(*       let f (n, fvs) e = (n + 1, fv e n :: fvs) in *)
(*       let (n2, fvs) = foldl f (c, []) es in *)
(*       filter (fun x -> x >= c) $ Yak.Util.remove_dups $| (v :: fvs) in *)

(*     match used_vars with *)
(*       | [] -> ? *)
(*       | v::vs -> *)
(*           let v, s, ds = (\* var, shift-amount pairs. *\) *)
(*             let g (vp, s, ds) v = (v, s + (v - vp - 1), (vp, s)::ds) in *)
(*             foldl g (v, v - c, []) vs in *)
(*           let shift_list = (v,s)::ds in *)
(*           let f = ... (\* function for var_map *\) in *)
(*           var_map ... *)

(*   let simplify e = *)
(*     (\* [c] is the count of binders. Keep in mind, therefore, that *)

(*           [c] = maximum free var + 1. *)

(*        So, [c] is the first available bound variable. This overlap happens *)
(*        because bound variables start from 0. i.e. 0 is the variable corresponding *)
(*        to being under 1 lambda. *)
(*     *\) *)
(*     let rec simplify' c = function *)
(*       | Let (es, v) -> *)
(*       | (True | False | InjectE _) as e -> e *)
(*       | Con (con, es) -> Con (con, List.map (simplify' c) es) *)
(*       | Case (d, cs) -> *)
(*           (match simplify' c d with *)
(*              | Con (con, es) -> *)
(*                  (\* Note: assuming well-formedness of expression. *)
(*                     Specifically, length es = arity(con) *\) *)
(*                  let (_, n), e_body = List.find (fun ((x,_),_) -> x = con) cs in *)
(*                  simplify' c $| case_reduce (c - 1) n e_body es *)
(*              | e -> *)
(*                  let cs' = List.map (fun ((con, n), e) -> (con, n), simplify' (c+n) e) cs in *)
(*                  Case (e, cs')) *)
(*       | App (e1, e2) -> *)
(*           let e1 = simplify' c e1 in *)
(*           let e2 = simplify' c e2 in *)
(*           (match e1 with *)
(*              | Lam e -> simplify' c (beta_reduce (c - 1) e e2) *)
(*              | _ -> App (e1, e2)) *)
(*       | Var i -> Var i *)
(*       | Lam f -> Lam (simplify' (c+1) f) *)
(*     in simplify' 0 e *)


end

(**
   General DeBruin-levels machinery
*)
module DB_levels = struct

  type con = string
  type patt = con * int

  type exp =
  | InjectE of string
  | Var of int
  | App of exp * exp
  | Lam of exp
  | Case of exp * (patt * exp) list
  | Con of con * exp list
  | Let of exp * exp

  let app2 f x y = App (App (f,x), y)
  let app3 f x y z = App (App (App (f, x), y), z)
  let letx e1 e2 = App (Lam e2, e1)

  let identity_fun = Lam (Var 0)

  let exp_map f e =
    let rec walk c = function
      | (InjectE _) as e -> e
      | Var i -> f c i
      | Con (con, es) -> Con (con, List.map (walk c) es)
      | Lam f -> Lam (walk (c+1) f)
      | Let (rhs, body) -> Let (walk c rhs, walk (c+1) body)
      | Case (d, cs) -> Case (walk c d,
                              List.map (fun ((con, n), e) -> ((con, n), walk (c+n) e)) cs)
      | App (e1, e2) -> App (walk c e1, walk c e2)
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

  (** [e_arg] has an mf of [k], while [e_body] has an mf of [k + 1],
      because [e_body] is underneath a lambda.  So, we shift [e_arg]
      up to equalize the mf, and then shift down the result, because
      we've eliminated free variable [k + 1].  *)
  let beta_reduce k e_body e_arg = shift (k+1) (-1) (subst (k+1) (shift k 1 e_arg) e_body)

  (** [args] have an mf of [k], while [e_body] has an mf of [k + n],
      because [e_body] is underneath a set of binders.  So, we shift [args]
      up to equalize the mf, and then shift down the result, because
      we've eliminated free variable [k + 1]...[k + n].  *)
  let case_reduce k n e_body =
    shift (k + n) (-n)
      $ fst
      $ List.fold_left (fun (e_body, m) e -> subst m e e_body, m - 1) (e_body, k + n)
      $ List.rev_map (shift k n)  (* rev_map so fold_left will subst args in reverse order,
                                     highest to lowest. *)

  (** Beta-reduce/case-eliminate where possible *)
  let simplify e =
    (* [c] is the count of binders. Keep in mind, therefore, that

          [c] = maximum free var + 1.

       So, [c] is the first available bound variable. This overlap happens
       because bound variables start from 0. i.e. 0 is the variable corresponding
       to being under 1 lambda.
    *)
    let rec simplify' c = function
      | (InjectE _) as e -> e
      | Con (con, es) -> Con (con, List.map (simplify' c) es)
      | Case (d, cs) ->
          (match simplify' c d with
             | Con (con, es) ->
                 (* Note: assuming well-formedness of expression.
                    Specifically, length es = arity(con) *)
                 let (_, n), e_body = List.find (fun ((x,_),_) -> x = con) cs in
                 simplify' c $| case_reduce (c - 1) n e_body es
             | e ->
                 let cs' = List.map (fun ((con, n), e) -> (con, n), simplify' (c+n) e) cs in
                 Case (e, cs'))
      | App (e1, e2) ->
          let e1 = simplify' c e1 in
          let e2 = simplify' c e2 in
          (match e1 with
             | Lam e -> simplify' c (beta_reduce (c - 1) e e2)
             | _ -> App (e1, e2))
      | Var i -> Var i
      | Lam f -> Lam (simplify' (c+1) f)
      | Let (e, f) -> Let (simplify' c e, simplify' (c+1) f)
    in simplify' 0 e

  (** check that the expression has a maximum free variable of at most [n - 1].*)
  let rec check_free_at_most n = function
    | InjectE _ -> true
    | Var i -> i < n
    | Let (e, f) -> check_free_at_most n e && check_free_at_most (n+1) f
    | Lam f -> check_free_at_most (n+1) f
    | App (e1, e2) -> check_free_at_most n e1 && check_free_at_most n e2
    | Con (_, es) -> List.fold_left (fun b e -> b && check_free_at_most n e) true es
    | Case (d, cs) -> check_free_at_most n d &&
        List.fold_left (fun b ((_,m), e) -> b && check_free_at_most (n+m) e) true cs

  let is_closed = check_free_at_most 0

  (** Print a string representation of the code. Does not print valid ocaml code. *)
  let to_string_raw =
    let rec recur c = function
      | Lam f ->
          Printf.sprintf "(lam. %s)" (recur (c+1) f)
      | Let (e,f) ->
          Printf.sprintf "(let %s. %s)" (recur c e) (recur (c+1) f)
      | Var x -> if x < c then Printf.sprintf "%d" x else Printf.sprintf "'%d" x
      | Con (con, es) -> Printf.sprintf "%s(%s)" con $ String.concat ", " $ List.map (recur c)
          $| es
      | Case (d, cs) ->
          let pcase ((con,n), e) =
            Printf.sprintf "%s<%d> -> %s" con n $| recur (c+n) e in
          Printf.sprintf "(case %s of %s)" (recur c d) $ String.concat "| " $ List.map pcase $| cs
      | App (e1,e2) -> Printf.sprintf "(%s %s)" (recur c e1) (recur c e2)
      | InjectE s -> Printf.sprintf "(%s)" s in
    recur

end

module DBL = DB_levels

(******************************************************************************)

module PHOAS = struct
  type con = string
  type patt = con * int

  type 'a exp =
    Var of 'a
  | App of 'a exp * 'a exp
  | Lam of ('a -> 'a exp)
  | Let of 'a exp * ('a -> 'a exp)
  | InjectE of string
  | Case of 'a exp * (patt * ('a list -> 'a exp)) list
  | Con of con * 'a exp list

  type hoas_exp = {e:'a. unit -> 'a exp}
  type hoas_exp2 = {e2: 'a. 'a exp}
  type open1_hoas_exp = {e_open1:'a. 'a -> 'a exp}
  type open2_hoas_exp = {e_open2:'a. 'a -> 'a -> 'a exp}
  type open3_hoas_exp = {e_open3:'a. 'a -> 'a -> 'a -> 'a exp}

  (** Sugar for a let expression. An alternative to actually applying.
      Useful for promoting sharing of results (given the CBV semantics. *)
  let let_e e1 e2 = App (e2, e1)

  let rec to_dB level = function
    | Var i -> DBL.Var i
    | Lam f -> DBL.Lam (to_dB (level+1) (f level))
    | Let (e, f) -> DBL.Let (to_dB level e, to_dB (level+1) (f level))
    | App (e1, e2) -> DBL.App (to_dB level e1, to_dB level e2)
    | Con (c, es) -> DBL.Con (c, List.map (to_dB level) es)
    | Case (d, cs) ->
        let c2db ((con,n), e_open) =
          let levels =
            let rec l c xs = if c < level then xs else l (c - 1) (c :: xs) in
            l (level + n - 1) [] in
          (con,n), to_dB (level + n) (e_open levels) in
        DBL.Case (to_dB level d, List.map c2db cs)
    | InjectE s -> DBL.InjectE s

  let convert_to_dB {e=f} = to_dB 0 (f())

  (* val subst' : 'a exp exp -> 'a exp *)
  let rec subst' = function
    | Var e -> e
    | Lam f -> Lam (fun x -> subst' (f (Var x)))
    | Let (e, f) -> Let (subst' e, fun x -> subst' (f (Var x)))
    | Con (con, es) -> Con (con, List.map subst' es)
    | Case (d, cs) ->
        let sc (p, e_open) = (p, subst' $ e_open $ List.map (fun x -> Var x)) in
        Case (subst' d, List.map sc cs)
    | App (e1, e2) -> App (subst' e1, subst' e2)
    | InjectE s -> InjectE s

  let subst e1 e2 = {e = fun () -> subst' (e1.e_open1 (e2.e ()))}
  let subst2 e1 e2 e3 = {e = fun () -> subst' ( e1.e_open2 (e2.e ()) (e3.e ()) )}

  (** Close an open expression with a variable. Useful for lifting the
      "openness" of an expression out into a surrounding expression. *)
  let vsub e v = subst' (e.e_open1 (Var v))
  let vsub2' e v1 v2 = subst' (e (Var v1) (Var v2))
  let vsub2 e v1 v2 = vsub2' e.e_open2 v1 v2


  let get_var (n,elts) i = List.nth elts (n - i - 1)
  let extend_ctxt (n,elts) x = (n+1,x::elts)
  let extend_ctxt_many (n,elts) m xs = (n+m, List.rev_append xs elts)
  let empty_ctxt () = (0,[])

  let rec from_dB ctxt = function
    | DBL.Var i -> Var (get_var ctxt i)
    | DBL.Lam f -> Lam (fun x -> from_dB (extend_ctxt ctxt x) f)
    | DBL.Let (e, f) -> Let(from_dB ctxt e, fun x -> from_dB (extend_ctxt ctxt x) f)
    | DBL.App (e1, e2) -> App (from_dB ctxt e1, from_dB ctxt e2)
    | DBL.InjectE s -> InjectE s
    | DBL.Case (d, cs) ->
        let db2c ((con, n), e_open) =
          (con, n), (fun xs -> from_dB (extend_ctxt_many ctxt n xs) e_open) in
        Case (from_dB ctxt d, List.map db2c cs)
    | DBL.Con (con, es) -> Con (con, List.map (from_dB ctxt) es)

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
      | Let (e, f) ->
          let x = mk_var c in
          Printf.sprintf "(let %s = %s in %s)" x $| recur c e $| recur (c+1) (f x)
      | Con (con, []) -> con
      | Con (con, es) ->
          Printf.sprintf "%s(%s)" con $ String.concat ", " $ List.map (recur c) $| es
      | Case (d, [("true", 0), e_then; ("false", 0), e_else]) ->
          Printf.sprintf "(if %s then %s else %s)"
            (recur c d) (recur c $| e_then []) (recur c $| e_else [])
      | Case (d, [("true", 0), e_then]) ->
          Printf.sprintf "(if %s then %s)" (recur c d) (recur c (e_then []))
      | Case (d, cs) ->
          let pcase ((con,n), e) =
            let vars =
              let rec l k xs = if k < c then xs else l (k - 1) (mk_var k :: xs) in
              l (c + n - 1) [] in
            let s_pat = String.concat ", " vars in
            let s_body = recur (c + n) (e vars) in
            Printf.sprintf "%s(%s) -> %s" con s_pat s_body in
          Printf.sprintf "(match %s with %s)" (recur c d)
            $ String.concat "| " $ List.map pcase $| cs
      | App (e1,e2) -> Printf.sprintf "(%s %s)" (recur c e1) (recur c e2)
    in
    recur

  let to_string {e=f} = to_string'  0 (f())

  let simplify = convert_from_dB $ DBL.simplify $ convert_to_dB

  let inject_string s = {e = fun () -> InjectE s}

  let app2 x y z = App (App (x,y), z)
  let app3 f x y z = App (App (App (f, x), y), z)
  let lam2 f = Lam (fun x -> Lam (f x))
  let lam3 f = Lam (fun x -> lam2 (f x))
  let ifthenelse e e1 e2 =
    Case (e, [("true", 0), (function [] -> e1 | _ -> invalid_arg "[] expected");
              ("false", 0), (function [] -> e2 | _ -> invalid_arg "[] expected")])
  let ifthen e e1 = Case (e, [("true", 0), (function [] -> e1 | _ -> invalid_arg "[] expected")])
  let etrue = Con ("true", [])
  let efalse = Con ("false", [])

end
