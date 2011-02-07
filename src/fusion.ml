(** This module implements a grammar transformation which fuses
directly subsequent semantic actions in a grammar.
    Also removes empty literals *)

let remove_empty_lit r =
  let rec loop = function
    | ( Gil.Action _ | Gil.Symb _ | Gil.Box _
      | Gil.When _ | Gil.When_special _ | Gil.Lit _ | Gil.CharRange _ | Gil.Lookahead _)
        as r -> r
    | Gil.Alt (r1, r2) -> Gil.Alt (loop r1, loop r2)
    | Gil.Star r1 -> Gil.Star (loop r1)
    | Gil.Seq (r1, r2) ->
        match loop r1 with
          | Gil.Lit (_, "") -> loop r2
          | r1' ->
              match loop r2 with
                | Gil.Lit (_, "") -> r1'
                | r2' -> Gil.Seq (r1', r2') in
  loop r

(**

    The following code implements a grammar transformation which fuses
    directly subsequent semantic actions in a grammar.

    VERSION 2: This version uses more sophisticated representations of expressions.
    The previous version used strings.

*)

module M = Meta_prog
module P = M.PHOAS

let fuse_gil_rhs =

(* TODO: In Alt case, if r1 and r2 end with the same fused action, we
   can actually factor it our of the alternative and keep going with
   the fusion. However, perhaps such factoring is best left to other
   transforms? Indeed, if we hashcons action transitions passed to
   FSM, then FSM mimimization will do this for us.*)

(* TODO: try to minimize reconstruction of tree. Currently, will
   always build fresh tree, even if nothing changes. For example, we
   could package first action (Action e) with a_acc and if it
   immediately hits a stop, it will use the bundled action rather than
   constructing a new one. *)

  let rec has_fuseable_prefix = function
    | Gil.Symb _
    | Gil.Lit _
    | Gil.CharRange _
    | Gil.When _
    | Gil.When_special _
    | Gil.Lookahead _ -> false
    | Gil.Star _ -> false

    | Gil.Action e -> true
    | Gil.Box _ -> true

    | Gil.Seq (r1, _) -> has_fuseable_prefix r1
    | Gil.Alt (r1, r2) -> has_fuseable_prefix r1 || has_fuseable_prefix r2 in

  (** Fuse a (reversed) list of actions [fn] ... [f1]:

      [  fun p v ->  fn p (.. f2 p (f1 p v)...)]
  *)
  let fuse = function
    | [] -> invalid_arg "empty list of actions passed to fuse"
    | [a] -> Gil.Action {P.e = (fun () -> P.InjectE a)}
    | actions_rev ->
        let actions_hoas = List.rev_map (fun a -> {P.e2=P.InjectE a}) actions_rev in
        Gil.Action {P.e = fun () ->
                      P.lam2 (fun p v ->
                             List.fold_left (fun e f -> P.app2 f.P.e2 (P.Var p) e) (P.Var v)
                               actions_hoas)} in

  let fuse_box actions b n = match actions with
    | [] -> invalid_arg "empty list of actions passed to fuse"
    | actions_rev ->
        let e_b = P.InjectE b in
        let actions_hoas = List.rev_map (fun a -> {P.e2=P.InjectE a}) actions_rev in
        let box =
          {P.e = (fun () ->
                    P.lam3 (fun v p ykb ->
                             let e_action = List.fold_left (fun v f -> P.app2 f.P.e2 (P.Var p) v)
                               (P.Var v) actions_hoas in
                             P.app3 e_b e_action (P.Var p) (P.Var ykb) ))} in
        Gil.Box(box, Gil.Meta.boxnull_s2p n) in

(* FAIL: this case does not work because the position variable p
   cannot be supplied by the predicate. *)
(*   let fuse_box_pred actions b p = match actions with *)
(*     | [] -> invalid_arg "empty list of actions passed to fuse" *)
(*     | actions_rev -> *)
(*         let box = {P.e = (fun () -> *)
(*                 let actions = List.rev_map (fun a -> P.InjectE a) actions_rev in *)
(*                 let e_b = P.InjectE b in *)
(*                 P.lam3 (fun v p ykb -> *)
(*                          let e_action = List.fold_left (fun v f -> P.app2 f (P.Var p) v) *)
(*                            (P.Var v) actions in *)
(*                          P.app3 e_b e_action (P.Var p) (P.Var ykb) ))} in *)
(*         let pred = {P.e = fun () -> *)
(*                 let actions = List.rev_map (fun a -> P.InjectE a) actions_rev in *)
(*                 let e_p = P.InjectE p in *)
(*                 P.Lam (fun v -> *)
(*                          let e_action = List.fold_left (fun x f -> P.app2 f (P.Var p) x) *)
(*                            (P.Var v) actions in *)
(*                          P.App (e_p, e_action))} in *)
(*         Gil.Box(box, pred) in *)

(*   let fuse_pred actions p n = match actions with *)
(*     | [] -> invalid_arg "empty list of actions passed to fuse" *)
(*     | actions_rev -> *)
(*         let action_string = *)
(*           List.fold_right (fun a s -> Printf.sprintf "(%s p %s)" a s) actions_rev "v" in *)
(*         let pred = Printf.sprintf "fun p v -> %s p %s" p action_string in *)
(*         Gil.When(pred, n) in *)

  let rec recur r = Gil.mkSEQ_rev (scan [] r [])

  (** process a scan todo list. This function is a helper function for [scan],
      and is called by leaf cases. *)
  and continue_s r_acc = function
    | [] -> r_acc
    | r::rs -> scan r_acc r rs

  (** flatten sequences into a todo-list. The todo list is not reversed. *)
  and scan r_acc r todo = match r with
    | Gil.Lit _
    | Gil.Symb _
    | Gil.CharRange _
    | Gil.Box _
    | Gil.When _ | Gil.When_special _
    | Gil.Lookahead _ -> continue_s (Gil.Meta.string_to_phoas r :: r_acc) todo

    | Gil.Alt (r1, r2) ->
        continue_s (Gil.Alt (recur r1, recur r2) :: r_acc) todo

    | Gil.Star r1 -> continue_s (Gil.Star (recur r1) :: r_acc) todo

    | Gil.Seq (r1, r2) -> scan r_acc r1 (r2::todo)

    | Gil.Action e -> continue_c r_acc [e] todo

  (** Enter, or continue, collection mode given a todo list.
      [r_acc] -- collected unfuseable rhs.
      [a_acc] -- collected fuseable rhs.
  *)
  and continue_c r_acc a_acc = function
    | [] -> (fuse a_acc) :: r_acc
    | r::rs -> collect r_acc a_acc r rs

  and collect r_acc a_acc r todo = match r with
    | Gil.Symb _
    | Gil.Lit _
    | Gil.CharRange _
    | Gil.When _ | Gil.When_special _
    | Gil.Lookahead _
    | Gil.Box (_, Gil.Runpred_null _) ->
        continue_s (Gil.Meta.string_to_phoas r :: (fuse a_acc) :: r_acc) todo

    | Gil.Star r1 ->
        continue_s (Gil.Star (recur r1) :: (fuse a_acc) :: r_acc) todo

(*     | Gil.Box (e, Gil.Runpred_null p) -> *)
(*         continue_s (fuse_box_pred a_acc e p :: r_acc) todo *)

    | Gil.Box (e, n) ->
        continue_s ((fuse_box a_acc e n) :: r_acc) todo

(*     | Gil.When (f_pred, f_next) ->  *)
(*      continue_s (fuse_pred a_acc f_pred f_next :: r_acc) todo *)

    | Gil.Seq (r1, r2) -> collect r_acc a_acc r1 (r2 :: todo)

    | Gil.Alt (r1, r2) ->
        if has_fuseable_prefix r1 || has_fuseable_prefix r2 then
          let r1_f = Gil.mkSEQ_rev (collect [] a_acc r1 []) in
          let r2_f = Gil.mkSEQ_rev (collect [] a_acc r2 []) in
          let r_f = Gil.Alt (r1_f, r2_f) in
          continue_s (r_f :: r_acc) todo
        else
          continue_s (Gil.Alt (recur r1, recur r2) :: (fuse a_acc) :: r_acc) todo

    | Gil.Action e -> continue_c r_acc (e :: a_acc) todo in

  recur

open Yak.Util.Operators

let fuse_gil_definitions ds = List.map (fun (n, r) ->
                                          (n, Gil.Meta.phoas_simplify_to_string $ fuse_gil_rhs $ remove_empty_lit $| r)) ds

let fuse_gil gr = gr.Gul.gildefs <- fuse_gil_definitions gr.Gul.gildefs
