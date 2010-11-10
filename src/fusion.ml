(** This module implements a grammar transformation which fuses
directly subsequent semantic actions in a grammar.
    Also removes empty literals *)

let fuse_rhs = 

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

  (** Fuse a (reversed) list of actions [fn] ... [f1]:

      [  fun p v ->  fn p (.. f2 p (f1 p v)...)]      
  *)
  let fuse = function
    | [] -> invalid_arg "empty list of actions passed to fuse"
    | [a] -> Gil.Action a
    | actions_rev ->
	let action_string = 
	  List.fold_right (fun a s -> Printf.sprintf "%s p (%s)" a s) actions_rev "v" in
	Gil.Action("fun p v -> " ^ action_string) in

(* TODO : FRESH VARIABLES by avoiding capture. *)

  let fuse_box actions b n = match actions with
    | [] -> invalid_arg "empty list of actions passed to fuse"
    | actions_rev ->
	let action_string = 
	  List.fold_right (fun a s -> Printf.sprintf "(%s p %s)" a s) actions_rev "v" in
	let box = Printf.sprintf "fun v p ykb -> let v' = %s in %s v' p ykb" action_string b in
	Gil.Box(box, n) in

  let fuse_box_pred actions b p = match actions with
    | [] -> invalid_arg "empty list of actions passed to fuse"
    | actions_rev ->
	let action_string = 
	  List.fold_right (fun a s -> Printf.sprintf "(%s p %s)" a s) actions_rev "v" in
	let box = Printf.sprintf "fun v p ykb -> let v' = %s in %s v' p ykb" action_string b in
	let pred = Printf.sprintf "fun v -> %s %s" p action_string in
	Gil.Box(box, Gil.Runpred_null pred) in

  let fuse_pred actions p n = match actions with
    | [] -> invalid_arg "empty list of actions passed to fuse"
    | actions_rev ->
	let action_string = 
	  List.fold_right (fun a s -> Printf.sprintf "(%s p %s)" a s) actions_rev "v" in
	let pred = Printf.sprintf "fun p v -> %s p %s" p action_string in
	Gil.When(pred, n) in

  let rec recur r = Gil.mkSEQ_rev (scan [] r [])

  and continue_s r_acc = function
    | [] -> r_acc
    | r::rs -> scan r_acc r rs

  and scan r_acc r r_k = match r with
    | Gil.Lit _ 
    | Gil.Symb _ 
    | Gil.CharRange _ 
    | Gil.Box _
    | Gil.When _
    | Gil.Lookahead _ -> continue_s (r :: r_acc) r_k

    | Gil.Alt (r1, r2) -> 
	continue_s (Gil.Alt (recur r1, recur r2) :: r_acc) r_k

    | Gil.Star r1 -> continue_s (Gil.Star (recur r1) :: r_acc) r_k

    | Gil.Seq (r1, r2) -> scan r_acc r1 (r2::r_k)

    | Gil.Action e -> continue_c r_acc [e] r_k

  and continue_c r_acc a_acc = function
    | [] -> (fuse a_acc) :: r_acc
    | r::rs -> collect r_acc a_acc r rs

  and collect r_acc a_acc r r_k = match r with
    | Gil.Symb _ 
    | Gil.Lit _ 
    | Gil.CharRange _ 
    | Gil.When _
    | Gil.Lookahead _ -> continue_s (r :: (fuse a_acc) :: r_acc) r_k

    | Gil.Alt (r1, r2) -> 
	continue_s (Gil.Alt (recur r1, recur r2) :: (fuse a_acc) :: r_acc) r_k
    | Gil.Star r1 -> 
	continue_s (Gil.Star (recur r1) :: (fuse a_acc) :: r_acc) r_k
	
    | Gil.Box (e, Gil.Runpred_null p) -> 
	continue_s (fuse_box_pred a_acc e p :: r_acc) r_k

    | Gil.Box (e, n) -> 
	continue_s ((fuse_box a_acc e n) :: r_acc) r_k

(*     | Gil.When (f_pred, f_next) ->  *)
(* 	continue_s (fuse_pred a_acc f_pred f_next :: r_acc) r_k *)

    | Gil.Seq (r1, r2) -> collect r_acc a_acc r1 (r2 :: r_k)

    | Gil.Action e -> continue_c r_acc (e :: a_acc) r_k in

  recur


let fuse_definitions ds = List.map (fun (n, r) -> (n, fuse_rhs r)) ds

let fuse gr = gr.Gul.gildefs <- fuse_definitions gr.Gul.gildefs
