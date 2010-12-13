(** This module implements a grammar transformation which fuses
directly subsequent semantic actions in a grammar.
    Also removes empty literals *)

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

    | Gil.Star r1 -> 
	continue_s (Gil.Star (recur r1) :: (fuse a_acc) :: r_acc) r_k
	
    | Gil.Box (e, Gil.Runpred_null p) -> 
	continue_s (fuse_box_pred a_acc e p :: r_acc) r_k

    | Gil.Box (e, n) -> 
	continue_s ((fuse_box a_acc e n) :: r_acc) r_k

(*     | Gil.When (f_pred, f_next) ->  *)
(* 	continue_s (fuse_pred a_acc f_pred f_next :: r_acc) r_k *)

    | Gil.Seq (r1, r2) -> collect r_acc a_acc r1 (r2 :: r_k)

    | Gil.Alt (r1, r2) -> 
	if has_fuseable_prefix r1 || has_fuseable_prefix r2 then
	  let r1_f = Gil.mkSEQ_rev (collect [] a_acc r1 []) in
	  let r2_f = Gil.mkSEQ_rev (collect [] a_acc r2 []) in
	  let r_f = Gil.Alt (r1_f, r2_f) in
	  continue_s (r_f :: r_acc) r_k
	else
	  continue_s (Gil.Alt (recur r1, recur r2) :: (fuse a_acc) :: r_acc) r_k
	    
    | Gil.Action e -> continue_c r_acc (e :: a_acc) r_k in

  recur

let remove_empty_lit r = 
  let rec loop = function
    | ( Gil.Action _ | Gil.Symb _ | Gil.Box _
      | Gil.When _ | Gil.Lit _ | Gil.CharRange _ | Gil.Lookahead _)
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

let fuse_gil_definitions ds = List.map (fun (n, r) -> (n, fuse_gil_rhs (remove_empty_lit r))) ds

let fuse_gil gr = gr.Gul.gildefs <- fuse_gil_definitions gr.Gul.gildefs



(* let fuse_gul_rhs = *)

(* (\* TODO: In Alt case, if r1 and r2 end with the same fused action, we *)
(*    can actually factor it our of the alternative and keep going with *)
(*    the fusion. However, perhaps such factoring is best left to other *)
(*    transforms? Indeed, if we hashcons action transitions passed to *)
(*    FSM, then FSM mimimization will do this for us.*\) *)

(* (\* TODO: try to minimize reconstruction of tree. Currently, will *)
(*    always build fresh tree, even if nothing changes. For example, we *)
(*    could package first action (Action e) with a_acc and if it *)
(*    immediately hits a stop, it will use the bundled action rather than *)
(*    constructing a new one. *\) *)

(*   (\** Fuse a (reversed) list of actions [fn] ... [f1]: *)

(*       [  fun p v ->  fn p (.. f2 p (f1 p v)...)] *)
(*   *\) *)
(*   let fuse = function *)
(*     | [] -> invalid_arg "empty list of actions passed to fuse" *)
(*     | [a] -> Gil.Action a *)
(*     | actions_rev -> *)
(* 	let action_string = *)
(* 	  List.fold_right (fun a s -> Printf.sprintf "%s p (%s)" a s) actions_rev "v" in *)
(* 	Gil.Action("fun p v -> " ^ action_string) in *)

(* (\* TODO : FRESH VARIABLES by avoiding capture. *\) *)

(*   let fuse_box actions b n = match actions with *)
(*     | [] -> invalid_arg "empty list of actions passed to fuse" *)
(*     | actions_rev -> *)
(* 	let action_string = *)
(* 	  List.fold_right (fun a s -> Printf.sprintf "(%s p %s)" a s) actions_rev "v" in *)
(* 	let box = Printf.sprintf "fun v p ykb -> let v' = %s in %s v' p ykb" action_string b in *)
(* 	Gil.Box(box, n) in *)

(*   let fuse_box_pred actions b p = match actions with *)
(*     | [] -> invalid_arg "empty list of actions passed to fuse" *)
(*     | actions_rev -> *)
(* 	let action_string = *)
(* 	  List.fold_right (fun a s -> Printf.sprintf "(%s p %s)" a s) actions_rev "v" in *)
(* 	let box = Printf.sprintf "fun v p ykb -> let v' = %s in %s v' p ykb" action_string b in *)
(* 	let pred = Printf.sprintf "fun v -> %s %s" p action_string in *)
(* 	Gil.Box(box, Gil.Runpred_null pred) in *)

(*   let fuse_pred actions p n = match actions with *)
(*     | [] -> invalid_arg "empty list of actions passed to fuse" *)
(*     | actions_rev -> *)
(* 	let action_string = *)
(* 	  List.fold_right (fun a s -> Printf.sprintf "(%s p %s)" a s) actions_rev "v" in *)
(* 	let pred = Printf.sprintf "fun p v -> %s p %s" p action_string in *)
(* 	Gil.When(pred, n) in *)

(*   let rec recur r = Gil.mkSEQ_rev (scan [] r []) *)

(*   (\** invocation: [scan r_acc r r_k] *)
      
(*       Scan [r] from left to right, breaking left-nested sequences *)
(*       until a leaf is found. Build up a worklist in the "continuation" *)
(*       [k]. The continuation style is used to handle left-nested *)
(*       sequences. An alternative would have been to simply flatten the *)
(*       sequence first and then fuse in a more straightforward manner. *)

(*       When we hit a leaf, we call [continue_s] to continue scanning by *)
(*       pulling the next r to process from the worklist. If the leaf is *)
(*       fusable, we switch into "collect" mode by calling [continue_c] *)
(*       and begin collecting sequential fusable items. Once collect *)
(*       reaches a non-fusable item, or the end of the worklist (in *)
(*       [continue_c]), it fuses what has been accumulated in [a_acc] *)
(*       into a single action, and places it on the list [r_acc] of *)
(*       finished items. *)

(*       @parameter [r_acc] collects finished items (leafs, fused *)
(*       actions,...) which will eventually be turned back into a sequence. *)
(*   *\) *)
(*   and scan r_acc r k = match r.r with *)
(*     | Gul.Symb _ *)
(*     | Gul.Lit _ *)
(*     | Gul.CharRange _ *)
(*     | Gul.Prose _ *)
(*     | Gul.Delay _ *)
(*     | Gul.Box _ *)
(*     | Gul.When _ *)
(*     | Gul.Minus _ *)
(*     | Gul.Lookahead _ -> continue_s (r :: r_acc) k *)

(*     | Gul.Opt r1 -> continue_s (Gul.mkOPT (recur r1) :: r_acc) k *)
(*     | Gul.Assign (r1, v1, v2) -> continue_s (Gul.mkASSIGN (recur r1, v1, v2) :: r_acc) k *)
(*     | Gul.Alt (r1, r2) -> *)
(* 	continue_s (Gul.mkALT2 (recur r1, recur r2) :: r_acc) k *)
(*     | Gul.Rcount (e, r1) -> continue_s (Gul.mkRCOUNT (e, recur r1) :: r_acc) k *)
(*     | Gul.Star (l, r1) -> continue_s (Gul.mkSTAR2 (l, recur r1) :: r_acc) k *)
(*     | Gul.Hash (l, r1) -> continue_s (Gul.mkHASH2 (l, recur r1) :: r_acc) k *)

(*     | Gul.Action (e_a, e_l) -> continue_c r_acc (Util.list_of_opt e_a) (Util.list_of_opt e_l) k *)

(*     | Gul.Seq (r1, r2) -> scan r_acc r1 (r2::k) *)

(*   and collect r_acc ea_acc la_acc r k = *)
(*     let to_scan r e l rs k = continue_s (r :: fuse e l :: rs) k in *)
(*     match r.r with *)
(*     | Gul.Symb _ *)
(*     | Gul.Lit _ *)
(*     | Gul.CharRange _ *)
(*     | Gul.Prose _ *)
(*     | Gul.Delay _ *)
(*     | Gul.When _ *)
(*     | Gul.Minus _ *)
(*     | Gul.Lookahead _ -> to_scan r ea_acc la_acc r_acc k *)

(*     | Gul.Opt r1 -> to_scan (Gul.mkOPT (recur r1)) ea_acc la_acc r_acc k *)
(*     | Gul.Assign (r1, v1, v2) -> to_scan (Gul.mkASSIGN (recur r1, v1, v2)) ea_acc la_acc r_acc k *)
(*     | Gul.Alt (r1, r2) -> *)
(* 	to_scan (Gul.mkALT2 (recur r1, recur r2)) ea_acc la_acc r_acc k *)
(*     | Gul.Rcount (e, r1) -> to_scan (Gul.mkRCOUNT (e, recur r1)) ea_acc la_acc r_acc k *)
(*     | Gul.Star (l, r1) -> to_scan (Gul.mkSTAR2 (l, recur r1)) ea_acc la_acc r_acc k *)
(*     | Gul.Hash (l, r1) -> to_scan (Gul.mkHASH2 (l, recur r1)) ea_acc la_acc r_acc k *)
	  
(*     | Gul.Box (e, ty_opt, Gul.Runpred_null p) -> *)
(* 	continue_s (fuse_box_pred a_acc e ty_opt p :: r_acc) k *)

(*     | Gul.Box (e, ty_opt, n) -> *)
(* 	continue_s ((fuse_box a_acc e ty_opt n) :: r_acc) k *)

(*     (\*     | Gul.When (f_pred, f_next) ->  *\) *)
(* (\* 	continue_s (fuse_pred a_acc f_pred f_next :: r_acc) k *\) *)

(*     | Gul.Action (e_a, e_l) -> continue_c r_acc (Util.cons_opt e_a ea_acc) (Util.cons_opt l_a la_acc) k *)

(*     | Gul.Seq (r1, r2) -> collect r_acc a_acc r1 (r2 :: k) *)

(*   and continue_s r_acc = function *)
(*     | [] -> r_acc *)
(*     | r::rs -> scan r_acc r rs *)

(*   (\** *)
(*       [ea_acc] = early action accumulator *)
(*       [la_acc] = late action accumulator *)
(*   *\) *)
(*   and continue_c r_acc ea_acc la_acc = function *)
(*     | [] -> (fuse ea_acc la_acc) :: r_acc *)
(*     | r::rs -> collect r_acc ea_acc la_acc r rs in *)

(*   recur *)


(* let fuse_gul_definitions ds = *)
(*   let f = function *)
(*     | RuleDef(n, r, a) -> RuleDef(n, fuse_gul_rhs r, a) *)
(*     | d -> d in *)
(*   List.map f ds *)

(* let fuse_gul gr = gr.Gul.ds <- fuse_gul_definitions gr.Gul.ds *)

