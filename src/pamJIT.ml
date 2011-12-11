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

module PI = Pam_internal

exception Not_regular

(** A conservative check that the subset of the transducer [p]
    reachable from state [s] is deterministicly regular. Only
    scans, charset lookahead, and completions are allowed. *)
let is_regular p start =
  let num_states = Array.length p in
  let unvisited = Array.make num_states true in
  (** a depth-first search to verify that only regular transitions
      are reachable from any state in the list of arguments. *)
  let rec dfs_regular = function
    | [] -> true
    | s::ss ->
        unvisited.(s) <- false;
        let blk = p.(s) in
        let add_states stack = function
          | PI.EatInstr (_,t)
          | PI.ALookaheadInstr (_, (PI.CsLA _), t) ->
              if unvisited.(t) then t::stack else stack
          | PI.CompleteInstr _ -> stack
          | _ -> raise Not_regular in
        let stack_opt = try Some (Array.fold_left add_states ss blk) with Not_regular -> None in
        match stack_opt with
          | None -> false
          | Some stack -> dfs_regular stack in
  dfs_regular [start]

let is_token p start =
  match p.(start) with [|PI.LexerInstr f|] -> Some f | _ -> None

let col_size = 257
  (** one for EOF *)
let iEOF = 256
  (** index of EOF *)

(** convert a program into a lookup table for
    a regexp engine.

    Assumes that [p] is deterministic and that state 0 is a dummy state. *)
let convert_to_dfa_table p =
  let num_states = Array.length p in
  let table = Array.make (col_size * num_states) 0 in
  let final_set = Array.make num_states false in
  (* Initialize [table] and [final_set]. *)
  for s = 0 to num_states - 1 do
    let blk = Array.unsafe_get p s in
    let collect_eat = function
      | PI.EatInstr(c,target) ->
          table.(s * col_size + c) <- target
      | PI.CompleteInstr _ ->
          final_set.(s) <- true
      | _ -> ()
    in
    Array.iter collect_eat blk
  done;
  table, final_set


let convert_to_nfa p =
  let num_states = Array.length p in
  let table = Array.make (col_size * num_states) 0 in
  let final_set = Array.make num_states false in
  let eps_table = Array.init num_states (fun s ->
    let blk = Array.unsafe_get p s in
    let set_eat_collect_eps eps = function
      | PI.EatInstr(c,target) ->
          table.(s * col_size + c) <- target;
          eps
      | PI.AAction2Instr(_, target) -> target::eps
      | PI.CompleteInstr _ ->
          final_set.(s) <- true;
          eps
      | _ -> eps
    in
    let eps = Array.fold_left set_eat_collect_eps [] blk in
    Array.of_list eps) in
  table, eps_table, final_set

type opt_mode = No_opt | Scan_opt | Scan_lookahead_opt | Compl_opt | Full_opt

type elr0_trans =
  | Scan_trans of PI.label array
  | Call_trans of PI.label
  | Complete_trans of PI.nonterm
  | No_trans

let lookup_trans_nt (tbl : PI.label array array) s nt =
(*   tbl.(s).(nt) *)
  Array.unsafe_get (Array.unsafe_get tbl s) nt

let error_nt = 0

exception Not_ELR0 of string

(* convert a program into lookup tables for
   a LR0 engine.

   Checks that [p] meets ELR(0) criterion.
   We don't perform any transformation of the start state at this point. *)
let convert_to_ELR0_table p start_symb start_state min_nonterm num_nonterms =
  let ntid nt =
    if nt < min_nonterm then
      invalid_arg (Printf.sprintf "nonterminal too small: %d < %d.\n" nt min_nonterm)
    else nt - min_nonterm in
  let fail_not_ELR0_msg state s =
    raise (Not_ELR0 (Printf.sprintf "%s in state %d." s state)) in
  let fail_not_ELR0 state = raise (Not_ELR0 ("Problem with state " ^ string_of_int state)) in
  let num_states = Array.length p in
  let term_table = Array.make num_states No_trans in
  let nonterm_table = Array.make_matrix num_states num_nonterms 0 in
  for s = 0 to num_states - 1 do
    let blk = Array.unsafe_get p s in
    let init_arrays = function
      | PI.EatInstr(c,target) ->
          (match term_table.(s) with
            | No_trans ->
                let a = Array.make col_size 0 in
                a.(c) <- target;
                term_table.(s) <- Scan_trans a
            | Scan_trans a ->
                if a.(c) = 0 then a.(c) <- target
                else fail_not_ELR0_msg s "nondeterministic shift"
            | Complete_trans _ -> fail_not_ELR0_msg s "shift-reduce conflict"
            | Call_trans _ -> fail_not_ELR0_msg s "shift-call conflict")
      | PI.CompleteInstr nt ->
          (match term_table.(s) with
             | No_trans ->
                 term_table.(s) <- Complete_trans (ntid nt)
             | Scan_trans _ -> fail_not_ELR0_msg s "shift-reduce conflict"
             | _ -> fail_not_ELR0 s)
      | PI.ASimpleCont2Instr (nt, _, target) ->
          if nonterm_table.(s).(ntid nt) = 0 then
            nonterm_table.(s).(ntid nt) <- target
          else fail_not_ELR0_msg s "nondeterministic continue"
      | PI.ACallInstr3 (_,target) ->
          (match term_table.(s) with
             | No_trans -> term_table.(s) <- Call_trans target
             | _ -> fail_not_ELR0 s)

      (* Instructions which can be safely ignored (because they don't
         affect LR-ness): *)
      | PI.RCompleteInstr2 _ -> ()

      (* Deprecated instructions *)
      | PI.ABlackboxInstr _ ->
          fail_not_ELR0_msg s "Deprecated instructions"

      (* Non-ELR0 compatible instructions. *)
      | PI.AAction2Instr _ | PI.AWhenInstr3 _ | PI.AContInstr3 _ | PI.WhenSpecialInstr _
      | PI.ALookaheadInstr _ | PI.DetBranchInstr _ | PI.LexerInstr _ ->
          fail_not_ELR0_msg s "Incompatible instructions"
    in
    Array.iter init_arrays blk
  done;
  (ntid start_symb), start_state, term_table, nonterm_table

let lookup_re_trans table s c = table.(s * col_size + c)

let lookup_trans te_descrs entry_idx act =
  let lb = te_descrs.(entry_idx) in
  let n = te_descrs.(entry_idx + 1) in
  let off = act - lb in
  if (off < 0) or (off >= n) then 0
  else te_descrs.(entry_idx + 2 + off)

(** Nondeterministic ELR(0) *)
module NELR0 = struct

  type lookahead_spec =
    | NoLA
    | ReLA of PI.label * PI.nonterm * PI.label
    | CsLA of PI.label array

  type multi_trans = {scans: PI.label array; (* [||] -> none *)
                      call: PI.label;        (* 0 -> none. *)
                      completes: PI.nonterm array; (* [||] -> none *)
                      ext_lookahead: lookahead_spec } (* NoLA -> none *)

  type det_trans = int
(*     | Scan_dtrans of PI.label *)
(*     | Lookahead_dtrans of PI.label *)
(*     | Call_dtrans of PI.label *)
(*     | Complete_dtrans of PI.nonterm *)

  type trans =
    | No_trans

    (* Deterministic transitions. *)

    | Scan_trans of PI.label array
    | Lookahead_trans of PI.label array
    | ExtLookahead_trans of PI.label * PI.nonterm * PI.label
    | Det_multi_trans of det_trans array
    | Call_trans of PI.label
    | Complete_trans of PI.nonterm

    (* Nondeterministic transitions. We optimized the case of
       multiple completions. Multiple scans/calls are discouraged b/c
       they can be removed by determinizing the machine, so we don't
       optimize them. *)

    | MComplete_trans of PI.nonterm array
    | Multi_trans of multi_trans
    | Many_trans of trans array (** Arbitrary collection of transitions. *)

  exception Not_NELR0 of string

  let empty_multi_trans = {scans=[||]; call=0; completes=[||]; ext_lookahead = NoLA;}

  (* convert a program into lookup tables for
     a nondeterministic ELR0 engine.

     Checks that [p] meets NELR(0) criterion.  Conflicting scans and calls
     are not allowed -- the user should determinize the transducer or split
     the offending state.

     We don't perform any transformation of the start state at this point. *)
  let to_table p start_symb start_state min_nonterm num_nonterms =
    let ntid nt =
      if nt < min_nonterm then
        invalid_arg (Printf.sprintf "nonterminal too small: %d < %d.\n" nt min_nonterm)
      else nt - min_nonterm in
    let fail_not_ELR0_msg state s =
      raise (Not_NELR0 (Printf.sprintf "%s in state %d." s state)) in
    let warn_not_ELR0_msg state s =
      () in
(*       Printf.eprintf "ELR0 warning: %s in state %d.\n" s state in *)
    let num_states = Array.length p in
    let term_table = Array.make num_states No_trans in
    let nonterm_table = Array.make_matrix num_states num_nonterms 0 in
    let mk_scan c target =
      let a = Array.make col_size 0 in
      a.(c) <- target;
      a in
    let mk_csla presence la_cs target =
      let cs = if presence then la_cs else Cs.complement 256 la_cs in
      let a = Array.make col_size 0 in
      Cs.iter (fun c -> a.(c) <- target) cs;
      (* Since EOF is not in the language, a positive lookahead cannot
         set it. Only a negative lookahead can (implicitly) set EOF. *)
      if not presence then a.(iEOF) <- target;
      a in
    let merge_csla s a1 a2 =
      Array.iteri (fun c t2 ->
                     if a1.(c) <> 0 && a1.(c) <> t2 then
                       fail_not_ELR0_msg s "nondeterministic lookahead"
                     else a1.(c) <- t2) a2 in
    let scan_dtrans t =
      if t land 0x7F000000 <> 0 then invalid_arg "target must not have any of upper 7 bits set"
      else t in
    let lookahead_dtrans t =
      if t land 0x7F000000 <> 0 then invalid_arg "target must not have any of upper 7 bits set"
      else t lor 0x1000000 in
    for s = 0 to num_states - 1 do
      let blk = Array.unsafe_get p s in
      let init_arrays = function
        | PI.EatInstr(c,target) ->
            (match term_table.(s) with
               | No_trans ->
                   term_table.(s) <- Scan_trans (mk_scan c target)
               | Scan_trans a ->
                   if a.(c) = 0 then a.(c) <- target
                   else fail_not_ELR0_msg s "nondeterministic shift"
               | Lookahead_trans a ->
                   if a.(c) = 0 then begin
                     (* initialize [a_mt] by translating [a] into a det_trans array *)
                     let a_mt = Array.init col_size (fun c -> lookahead_dtrans a.(c)) in
                     a_mt.(c) <- scan_dtrans target;
                     term_table.(s) <- Det_multi_trans a_mt
                   end
                   else begin
                     warn_not_ELR0_msg s "shift-lookahead conflict";
                     term_table.(s) <- Multi_trans {empty_multi_trans with
                                                      scans = mk_scan c target; ext_lookahead = CsLA a}
                   end
               | Det_multi_trans a_mt ->
                   let t = a_mt.(c) land 0xFFFFFF in
                   if t = 0 then
                     a_mt.(c) <- scan_dtrans target
                   else
                     let x_action = a_mt.(c) land 0x7F000000 in
                     if x_action = 0 then begin
                       if t <> target then fail_not_ELR0_msg s "nondeterministic shift"
                     end else
                       fail_not_ELR0_msg s "(det) shift-lookahead conflict"

(*                 (match a_mt.(c) with *)
(*                    | Scan_dtrans 0 | Lookahead_dtrans 0 (\* | Call_dtrans 0 *\) ->  *)
(*                        a_mt.(c) <- Scan_dtrans target  *)
(*                    | Scan_dtrans t ->  *)
(*                        if t <> target then *)
(*                          fail_not_ELR0_msg s "nondeterministic shift" *)
(*                    | Lookahead_dtrans _ ->  *)
(*                        fail_not_ELR0_msg s "(det) shift-lookahead conflict" *)
(*                    | Call_dtrans _ ->  *)
(*                        fail_not_ELR0_msg s "(det) shift-call conflict" *)
(*                    | Complete_dtrans _ ->  *)
(*                        fail_not_ELR0_msg s "(det) shift-reduce conflict" *)
(*                 ) *)
               | ExtLookahead_trans (x,y,z) ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with scans=mk_scan c target;
                                                    ext_lookahead=ReLA(x,y,z);}
               | Complete_trans nt ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with scans=mk_scan c target; completes=[|nt|];}
               | MComplete_trans nts ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with scans=mk_scan c target; completes=nts;}
               | Call_trans t ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with scans=mk_scan c target; call=t;}
               | Multi_trans ({scans=[||]} as trs) ->
                   term_table.(s) <- Multi_trans {trs with scans=mk_scan c target}
               | Multi_trans {scans=a} ->
                   if a.(c) = 0 then a.(c) <- target
                   else fail_not_ELR0_msg s "nondeterministic shift"
               | Many_trans trs ->
                   warn_not_ELR0_msg s "shift-other conflict";
                   term_table.(s) <- Many_trans (Array.append [|Scan_trans (mk_scan c target)|] trs))
        | PI.ACallInstr3 (_,target) ->
            (match term_table.(s) with
               | No_trans -> term_table.(s) <- Call_trans target
               | Scan_trans a ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with scans=a; call=target}
               | Lookahead_trans a ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with call=target;
                                                    ext_lookahead=CsLA a;}
               | (Det_multi_trans _) as tr ->
                   warn_not_ELR0_msg s "call-lookahead conflict";
                   term_table.(s) <- Many_trans [|tr; Call_trans target|]
               | ExtLookahead_trans (x,y,z) ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with call=target;
                                                    ext_lookahead=ReLA(x,y,z);}
               | Call_trans t ->
                   if t <> target then fail_not_ELR0_msg s "nondeterministic call"
               | Complete_trans nt ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with call=target; completes=[|nt|];}
               | MComplete_trans nts ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with call=target; completes=nts;}
               | Multi_trans ({call=0} as trs) ->
                   term_table.(s) <- Multi_trans {trs with call=target}
               | Multi_trans {call=t} ->
                   if t <> target then fail_not_ELR0_msg s "nondeterministic call"
               | Many_trans trs ->
                   warn_not_ELR0_msg s "call-other conflict";
                   term_table.(s) <- Many_trans (Array.append [|Call_trans target|] trs))
        | PI.CompleteInstr nt ->
            (match term_table.(s) with
               | No_trans ->
                   term_table.(s) <- Complete_trans (ntid nt)
               | Scan_trans a ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with scans=a; completes=[|ntid nt|];}
               | Lookahead_trans a ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with completes=[|ntid nt|];
                                                    ext_lookahead=CsLA a;}
               | (Det_multi_trans _) as tr ->
                   warn_not_ELR0_msg s "reduce-lookahead conflict";
                   term_table.(s) <- Many_trans [|tr; Complete_trans (ntid nt)|]
               | ExtLookahead_trans (x,y,z) ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with completes=[|ntid nt|];
                                                    ext_lookahead=ReLA(x,y,z);}
               | Call_trans t ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with completes=[|ntid nt|]; call=t;}
               | Complete_trans nt1 ->
                   term_table.(s) <- MComplete_trans [|nt1; ntid nt|]
               | MComplete_trans nts ->
                   let n = Array.length nts in
                   let nts_new = Util.array_realloc nts (n + 1) 0 in
                   nts_new.(n) <- ntid nt;
                   term_table.(s) <- MComplete_trans nts_new
               | Multi_trans ({completes=nts} as trs) ->
                   let n = Array.length nts in
                   let nts_new = Util.array_realloc nts (n + 1) 0 in
                   nts_new.(n) <- ntid nt;
                   term_table.(s) <- Multi_trans {trs with completes=nts_new}
               | Many_trans trs ->
                   warn_not_ELR0_msg s "reduce-other conflict";
                   term_table.(s) <- Many_trans (Array.append [|Complete_trans (ntid nt)|] trs))
        | PI.ASimpleCont2Instr (nt, _, target) ->
            if nonterm_table.(s).(ntid nt) = 0 then
              nonterm_table.(s).(ntid nt) <- target
            else fail_not_ELR0_msg s "nondeterministic continue"

        (* Special-casing lookahead to handle OCaml. *)
        | PI.ALookaheadInstr (presence, PI.CsLA la_cs, target) ->
            (match term_table.(s) with
               | No_trans ->
                   term_table.(s) <- Lookahead_trans (mk_csla presence la_cs target)
               | Scan_trans a ->
                   (try
                     (* initialize [a_mt] by translating [a] into a det_trans array *)
                     let a_mt = Array.init col_size (fun c -> scan_dtrans a.(c)) in
                     let cs = if presence then la_cs else Cs.complement 256 la_cs in
                     Cs.iter (fun c ->
                                if a.(c) = 0 then a_mt.(c) <- lookahead_dtrans target
                                else fail_not_ELR0_msg s "shift-lookahead conflict") cs;
                     (* Since EOF is not in the language, a positive lookahead cannot
                        set it. Only a negative lookahead can (implicitly) set EOF. *)
                     if not presence then begin
                       if a.(iEOF) = 0 then a_mt.(iEOF) <- lookahead_dtrans target
                       else fail_not_ELR0_msg s "shift-lookahead conflict on EOF";
                     end;
                     term_table.(s) <- Det_multi_trans a_mt
                   with Not_NELR0 _ ->
                     warn_not_ELR0_msg s "shift-lookahead conflict";
                     term_table.(s) <- Multi_trans {empty_multi_trans with
                                                      scans = a;
                                                      ext_lookahead = CsLA (mk_csla presence la_cs target)})
               | Lookahead_trans a1 ->
                   let a2 = mk_csla presence la_cs target in
                   merge_csla s a1 a2
               | Det_multi_trans a_mt ->
                   let cs = if presence then la_cs else Cs.complement 256 la_cs in
                   Cs.iter (fun c ->
                              let t = a_mt.(c) land 0xFFFFFF in
                              if t = 0 then
                                a_mt.(c) <- lookahead_dtrans target
                              else
                                let c_action = a_mt.(c) land 0x7F000000 in
                                if c_action = 0 then
                                  fail_not_ELR0_msg s "(det) shift-lookahead conflict"
                                else if t <> target then fail_not_ELR0_msg s "nondeterministic lookahead") cs;
                   (* Since EOF is not in the language, a positive lookahead cannot
                      set it. Only a negative lookahead can (implicitly) set EOF. *)
                   if not presence then begin
                     let t = a_mt.(iEOF) land 0xFFFFFF in
                     if t = 0 then
                       a_mt.(iEOF) <- lookahead_dtrans target
                     else
                       let action = a_mt.(iEOF) land 0x7F000000 in
                       if action <> 0 then begin
                         if t <> target then fail_not_ELR0_msg s "nondeterministic lookahead"
                       end else
                         fail_not_ELR0_msg s "unexpected trans on EOF"
                   end
  (*                          match a_mt.(c) with *)
  (*                            | Scan_dtrans 0 | Lookahead_dtrans 0 (\* | Call_dtrans 0  *\)->  *)
  (*                                a_mt.(c) <- Lookahead_dtrans target  *)
  (*                            | Scan_dtrans _ ->  *)
  (*                                fail_not_ELR0_msg s "(det) shift-lookahead conflict" *)
  (*                            | Lookahead_dtrans t ->  *)
  (*                                if t <> target then *)
  (*                                  fail_not_ELR0_msg s "nondeterministic lookahead" *)
  (*                            | Call_dtrans _ ->  *)
  (*                                fail_not_ELR0_msg s "(det) lookahead-call conflict" *)
  (*                            | Complete_dtrans _ ->  *)
  (*                                fail_not_ELR0_msg s "(det) lookahead-reduce conflict" ) cs; *)
  (*               (match a_mt.(iEOF) with  *)
  (*                  | Lookahead_dtrans 0 (\* | Call_dtrans 0  *\)->  *)
  (*                      a_mt.(iEOF) <- Lookahead_dtrans target *)
  (*                  | Lookahead_dtrans t ->  *)
  (*                      if t <> target then *)
  (*                        fail_not_ELR0_msg s "nondeterministic lookahead" *)
  (*                  | _ -> fail_not_ELR0_msg s "unexpected trans on EOF") *)
               | Complete_trans nt ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with ext_lookahead=CsLA (mk_csla presence la_cs target);
                                                    completes=[|ntid nt|];}
               | MComplete_trans nts ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with ext_lookahead=CsLA (mk_csla presence la_cs target);
                                                    completes=nts;}
               | Call_trans t_call ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with ext_lookahead=CsLA (mk_csla presence la_cs target);
                                                    call=t_call;}
               | Multi_trans ({ext_lookahead=NoLA} as trs) ->
                   term_table.(s) <- Multi_trans {trs with ext_lookahead=CsLA (mk_csla presence la_cs target);}
               | Multi_trans {ext_lookahead=CsLA a1} ->
                   let a2 = mk_csla presence la_cs target in
                   merge_csla s a1 a2
               | ExtLookahead_trans _
               | Multi_trans _ -> fail_not_ELR0_msg s "nondeterministic lookahead"
               | Many_trans trs ->
                   warn_not_ELR0_msg s "lookahead-other conflict";
                   term_table.(s) <- Many_trans (Array.append [|Lookahead_trans (mk_csla presence la_cs target)|] trs))

        | PI.ALookaheadInstr (false, PI.CfgLA (la_target, nt), target) ->
            (match term_table.(s) with
               | No_trans -> term_table.(s) <- ExtLookahead_trans (la_target, ntid nt, target)
               | Scan_trans a ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with
                                                    ext_lookahead=ReLA(la_target, ntid nt, target); scans=a;}
               | Lookahead_trans _ | Det_multi_trans _ ->
                   fail_not_ELR0_msg s "lookahead-ext.lookahead conflict"
               | ExtLookahead_trans (x,y,z) ->
                   fail_not_ELR0_msg s "nondeterministic ext.lookahead"
               | Call_trans t ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with
                                                    ext_lookahead=ReLA(la_target, ntid nt, target); call=t;}
               | Complete_trans nt ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with
                                                    ext_lookahead=ReLA(la_target, ntid nt, target);
                                                    completes=[|ntid nt|];}
               | MComplete_trans nts ->
                   term_table.(s) <- Multi_trans {empty_multi_trans with
                                                    ext_lookahead=ReLA(la_target, ntid nt, target);
                                                    completes=nts;}
               | Multi_trans ({ext_lookahead=NoLA} as trs) ->
                   term_table.(s) <- Multi_trans {trs with ext_lookahead=ReLA(la_target, ntid nt, target)}
               | Multi_trans _ ->
                   fail_not_ELR0_msg s "nondeterministic ext.lookahead"
               | Many_trans trs ->
                   warn_not_ELR0_msg s "ext.lookahead-other conflict";
                   term_table.(s) <- Many_trans (Array.append [|ExtLookahead_trans (la_target, ntid nt, target)|] trs))

        (* Instructions which can be safely ignored (because they don't
           affect LR-ness): *)
        | PI.RCompleteInstr2 _ -> ()

        (* Deprecated instructions *)
        | PI.ABlackboxInstr _ ->
            fail_not_ELR0_msg s "Deprecated instructions"

        (* Non-ELR0 compatible instructions. *)
        | PI.AAction2Instr _ | PI.AWhenInstr3 _ | PI.AContInstr3 _ | PI.WhenSpecialInstr _
        | PI.ALookaheadInstr _ | PI.DetBranchInstr _  | PI.LexerInstr _
            -> fail_not_ELR0_msg s "Incompatible instructions"
      in
      Array.iter init_arrays blk
    done;
    (ntid start_symb), start_state, term_table, nonterm_table
end


(** Dependent, nondeterministic ELR *)
module DNELR = struct

  type det_trans = int

  type nonterm = int

  type 'a trans =
    | No_trans

    (* Deterministic transitions. *)
    | Scan_trans of PI.terminal * PI.label
    | Det_trans of (PI.pos -> 'a -> 'a * PI.label)
    | Lexer_trans of (YkBuf.t -> PI.label)
    | MScan_trans of PI.label array
    | Lookahead_trans of PI.label array
    | TokLookahead_trans of PI.presence * (YkBuf.t -> bool) * PI.label
    | RegLookahead_trans of PI.presence * PI.label * nonterm * PI.label
    | ExtLookahead_trans of PI.presence * PI.label * nonterm * PI.label
    | Det_multi_trans of det_trans array
    | Call_trans of PI.label
    | Complete_trans of nonterm

    (* Nondeterministic transitions. We optimized the case of
       multiple completions. Multiple scans/calls are discouraged b/c
       they can be removed by determinizing the machine, so we don't
       optimize them. *)
    | MComplete_trans of nonterm array
    | Many_trans of 'a trans array

      (* Context-sensitive transitions. *)
    | Maybe_nullable_trans2 of nonterm * 'a PI.Pred3.t
    | Call_p_trans of 'a PI.action2 * PI.label
    | Complete_p_trans of nonterm
    | MComplete_p_trans of nonterm array
    | Action_trans of 'a Pam_internal.action2 * Pam_internal.label
    | When_trans of 'a Pam_internal.pred3 * 'a Pam_internal.next3
        * Pam_internal.label
    | When2_trans of 'a PI.Pred3.t * PI.label (** A more general "when". *)
    | Box_trans of 'a Pam_internal.blackbox * Pam_internal.label

  type 'a pnt_entry = {ctarget: PI.label; carg: 'a PI.action2; cbinder: 'a PI.binder2}

  type 'a pnt_table = 'a pnt_entry array array array

  type 'a data = {
    start_symb : int;
    start_state  : PI.label;
    term_table : 'a trans array;
    nonterm_table : PI.label array array;
    p_nonterm_table : 'a pnt_table;
  }

  let warn_extla = ref false

  let lookup_trans_pnt (tbl : 'a pnt_table) s nt =
(*     tbl.(s).(nt) *)
    Array.unsafe_get (Array.unsafe_get tbl s) nt

  let name_of = function
    | No_trans -> "EMPTY"
    | Scan_trans _ | MScan_trans _ -> "shift"
    | Det_trans _ -> "det-branch"
    | Lexer_trans _ -> "lexer"
    | Lookahead_trans _ -> "lookahead"
    | TokLookahead_trans _ -> "token lookahead"
    | RegLookahead_trans _ -> "regular lookahead"
    | ExtLookahead_trans _ -> "ext.lookahead"
    | Det_multi_trans _ -> "multi"
    | Call_trans _ -> "call"
    | Complete_trans _ | MComplete_trans _ -> "reduce"
    | Many_trans _ -> "others"

    | Call_p_trans _ -> "p.call"
    | Complete_p_trans _ | MComplete_p_trans _ -> "p.reduce"
    | Action_trans _ -> "action"
    | When_trans _ -> "when"
    | Box_trans _ -> "box"
    | Maybe_nullable_trans2 _ -> "maybe-nullable"
    | When2_trans _ -> "maybe-epsilon"

  let txcmp t1 t2 =
    match t1,t2 with
      | PI.EatInstr _, PI.EatInstr _ -> compare t1 t2
      | PI.EatInstr _, _ -> -1
      | _, PI.EatInstr _ -> 1
      | _ -> 0

  let is_nttrans = function
    | PI.ASimpleCont2Instr _ | PI.AContInstr3 _ -> true
    | _ -> false

  (* convert a program into lookup tables for
     a dep. nondeterministic ELR engine.

     We don't perform any transformation of the start state at this point. *)
  let mk_table opt_level p start_symb start_state min_nonterm num_nonterms
      f_no_arg f_no_binder =
    let ntid nt =
      if nt < min_nonterm then
        invalid_arg (Printf.sprintf "nonterminal too small: %d < %d.\n" nt min_nonterm)
      else nt - min_nonterm in
    let fail_not_ELR0_msg state msg =
      invalid_arg (Printf.sprintf "%s in state %d." msg state) in
    let warn_conflict state tr1 tr2 = () in
(*     let warn_conflict state tr1 tr2 = *)
(*       let nm1 = name_of tr1 in *)
(*       let nm2 = name_of tr2 in *)
(*       Printf.eprintf "DNELR warning: %s - %s conflict in state %d.\n" *)
(*      nm1 nm2 state in *)
    let num_states = Array.length p in
    let regulars = Hashtbl.create 11 in
    let term_table = Array.make num_states No_trans in
    let nt_class = Array.make num_nonterms false in
    let nonterm_table = Array.make_matrix num_states num_nonterms 0 in
(*     let p_nonterm_table = Array.make_matrix num_states num_nonterms *)
(*       {ctarget = 0; carg = (fun p x -> x); cbinder = (fun p x _ -> x)} in *)
    let p_nonterm_table = Array.make_matrix num_states num_nonterms [||] in
    let mk_scan c target =
      let a = Array.make col_size 0 in
      a.(c) <- target;
      a in
    let mk_csla presence la_cs target =
      let cs = if presence then la_cs else Cs.complement 256 la_cs in
      let a = Array.make col_size 0 in
      Cs.iter (fun c -> a.(c) <- target) cs;
      (* Since EOF is not in the language, a positive lookahead cannot
         set it. Only a negative lookahead can (implicitly) set EOF. *)
      if not presence then a.(iEOF) <- target;
      a in

    let mk_many s tr1 tr2 =
      warn_conflict s tr1 tr2;
      Many_trans [|tr1; tr2|] in

    (** Attempt to merge [tr] into an existing element of the array
        with merge function [f_m]. If that doesn't work ([f_m] returns
        a [Many_trans]), extend the array. *)
    let merge_many f_m s tr trs =
      let n = Array.length trs in
      assert (n > 0);
      let rec try_merge i n =
        if i < n then
          match f_m s tr trs.(i) with
            | Many_trans _ -> try_merge (i+1) n
            | tr -> trs.(i) <- tr; trs
        else begin
          warn_conflict s tr (Many_trans trs);
          Array.append [|tr|] trs
        end in
      Many_trans (try_merge 0 n) in

    let scan_dtrans t =
      if t = 0 then 0
      else if t land 0x7F000000 <> 0 then invalid_arg "target must not have any of upper 7 bits set"
      else t in

    let lookahead_dtrans t =
      if t = 0 then 0
      else if t land 0x7F000000 <> 0 then invalid_arg "target must not have any of upper 7 bits set"
      else t lor 0x1000000 in

    (* This function serves to both classify nonterminals and to
       generate the nonterm tables. While nonterminanl continuations
       should always agree on including a parameter, there need not be
       consistency regarding the binder. So, two continuations of the
       same nonterminal can disagree on the binder.

       We only maintain two classifications: simple (no arg, no binder) and
       parameterized (at least one of arg and binder).
    *)
    let add_nttrans s xs = function
      | PI.ASimpleCont2Instr (nt, binder, target) ->
          let nt = ntid nt in
          if binder == f_no_binder then begin
            if nonterm_table.(s).(nt) = 0 then
              nonterm_table.(s).(nt) <- target
            else fail_not_ELR0_msg s "nondeterministic simple continue";
            xs
          end else begin
            nt_class.(nt) <- true;
            (nt, {ctarget = target; carg = f_no_arg; cbinder = binder;}) :: xs
(*             if p_nonterm_table.(s).(nt).ctarget = 0 then *)
(*               p_nonterm_table.(s).(nt) <- *)
(*                 {ctarget = target; carg = f_no_arg; cbinder = binder;} *)
(*             else fail_not_ELR0_msg s "nondeterministic continue" *)
          end

      | PI.AContInstr3 (nt, arg, binder, target) ->
          let nt = ntid nt in
          nt_class.(nt) <- true;
          (nt, {ctarget = target; carg = arg; cbinder = binder;}) :: xs
(*           if p_nonterm_table.(s).(nt).ctarget = 0 then begin *)
(*             p_nonterm_table.(s).(nt) <- *)
(*               {ctarget = target; carg = arg; cbinder = binder;} *)
(*           end else fail_not_ELR0_msg s "nondeterministic continue" *)
      | _ ->
          invalid_arg "Unexpected (non-continue) instructions." in

    (** Map a PAM instruction to a table entry. *)
    let instr2trans s = function
      | PI.EatInstr(c,target) -> Scan_trans (c, target)
      | PI.DetBranchInstr b -> Det_trans b
      | PI.LexerInstr lf -> Lexer_trans lf
      | PI.ACallInstr3 (arg,target) ->
          if arg == f_no_arg then
            Call_trans target
          else
            Call_p_trans (arg, target)
      | PI.CompleteInstr nt ->
          let nt = ntid nt in
          if nt_class.(nt) then Complete_p_trans (nt)
          else Complete_trans (nt)

      | PI.ALookaheadInstr (presence, PI.CsLA la_cs, target) ->
          Lookahead_trans (mk_csla presence la_cs target)

      | PI.ALookaheadInstr (presence, PI.CfgLA (la_target, nt), target) ->
          (match is_token p la_target with
             | Some f ->
                 TokLookahead_trans (presence, (fun ykb -> match f ykb with
                                                  | 0 -> false
                                                  | _ -> true),
                                     target)
             | None ->
                 let is_reg =
                   match Util.find_option regulars nt with
                     | Some is_reg -> is_reg
                     | None ->
                         let is_reg = is_regular p la_target in
                         Hashtbl.add regulars nt is_reg;
                         if Logging.activated then begin
                           if is_reg then
                             Logging.log Logging.Features.lookahead "RLA: +%d\n" nt
                           else
                             Logging.log Logging.Features.lookahead "RLA: -%d\n" nt
                         end;
                         if not is_reg && !warn_extla then
                           Util.warn Util.Sys_warn
                             (Printf.sprintf "Using extended lookahead for nonterminal %d." nt);
                         is_reg in
                 if is_reg then
                   RegLookahead_trans (presence, la_target, ntid nt, target)
                 else
                   ExtLookahead_trans (presence, la_target, ntid nt, target))

      | PI.AAction2Instr (f, t) -> Action_trans (f, t)
      | PI.AWhenInstr3 (p, f, t) -> When_trans (p, f, t)
      | PI.WhenSpecialInstr (p, t) -> When2_trans (p, t)
      | PI.ABlackboxInstr (box, t) -> Box_trans (box, t)
(*
      | PI.ABoxInstr (box, f, t) -> Box_trans ((fun sv p ykb -> match box sv p ykb with
                                                  | None -> None | Some (j,sv2) -> Some (j, f sv sv2)), t)
*)

      | PI.RCompleteInstr2 (nt, p) -> Maybe_nullable_trans2 (ntid nt, p)
      | PI.ASimpleCont2Instr _ | PI.AContInstr3 _ ->
          invalid_arg "Continue instructions can not be translated to transitions" in

    (** merge [a2] into [a1], if there are no conflicts. If there are conflicts,
        [a1] is unchanged. [a1] and [a2] represent maps form terminals to states.
        @return [true], if the merge succeeds, [false] if there are conflicts. *)
    (* First we check for conflicts. Then, if there arrays are conflict-free, we
       merge them. We do this in two steps to be sure that we don't modify [a1]
       in the case of conflict. *)
    let merge_term_maps a1 a2 =
      let conflict =
        Util.array_existsi (fun c t2 -> a1.(c) <> 0 && a1.(c) <> t2) a2 in
      if conflict then false
      else (Array.iteri (fun c t2 -> if t2 <> 0 then a1.(c) <- t2) a2; true) in

    let merge_sc_la a_s a_l =
      let mt1 = Array.init col_size (fun c -> scan_dtrans a_s.(c)) in
      let mt2 = Array.init col_size (fun c -> lookahead_dtrans a_l.(c)) in
      if merge_term_maps mt1 mt2 then Some mt1 else None in

    let merge_mt_sc a_mt a_s =
      let mt2 = Array.init col_size (fun c -> scan_dtrans a_s.(c)) in
      merge_term_maps a_mt mt2 in

    let merge_mt_la a_mt a_l =
      let mt2 = Array.init col_size (fun c -> lookahead_dtrans a_l.(c)) in
      merge_term_maps a_mt mt2 in

    let rec merge_transs_no_opt s tr1 tr2 =
      match tr1, tr2 with
        | No_trans, tr | tr, No_trans -> tr
        | Many_trans trs, tr
        | tr, Many_trans trs ->
            merge_many merge_transs_no_opt s tr trs
        | _ ->
            mk_many s tr1 tr2 in

    let rec merge_transs_scan_opt s tr1 tr2 =
      match tr1, tr2 with
        | No_trans, tr | tr, No_trans -> tr
        | Many_trans trs, tr
        | tr, Many_trans trs ->
            merge_many merge_transs_scan_opt s tr trs

        | Scan_trans (c1, target1), Scan_trans (c2, target2) ->
            if c1 = c2 && target1 = target2 then tr1
            else if c1 <> c2 then
              let a = mk_scan c1 target1 in
              a.(c2) <- target2;
              MScan_trans a
            else mk_many s tr1 tr2
        | Scan_trans(c,target), MScan_trans a ->
            if a.(c) = 0 || a.(c) = target then (a.(c) <- target; tr2)
            else mk_many s tr1 tr2

        | Lookahead_trans a1, Lookahead_trans a2 ->
            if merge_term_maps a1 a2 then tr1
            else mk_many s tr1 tr2

        | _ ->
            mk_many s tr1 tr2 in

    let rec merge_transs_det_scan_opt s tr1 tr2 =
      match tr1, tr2 with
        | No_trans, tr | tr, No_trans -> tr
        | Many_trans trs, tr
        | tr, Many_trans trs ->
            merge_many merge_transs_det_scan_opt s tr trs

        | Scan_trans (c1, target1), Scan_trans (c2, target2) ->
            if c1 = c2 && target1 = target2 then tr1
            else if c1 <> c2 then
              let a = mk_scan c1 target1 in
              a.(c2) <- target2;
              MScan_trans a
            else mk_many s tr1 tr2
        | Scan_trans(c,target), MScan_trans a ->
            if a.(c) = 0 || a.(c) = target then (a.(c) <- target; tr2)
            else mk_many s tr1 tr2
        | Scan_trans(c,target), Lookahead_trans a ->
            if a.(c) = 0 then begin
              let a_mt = Array.init col_size (fun c -> lookahead_dtrans a.(c)) in
              a_mt.(c) <- scan_dtrans target;
              Det_multi_trans a_mt
            end
            else mk_many s tr1 tr2
        | Scan_trans(c,target), Det_multi_trans a_mt ->
            let t = a_mt.(c) in
            if t = 0 then
              (a_mt.(c) <- scan_dtrans target; tr2)
            else if t = scan_dtrans target then tr2
            else mk_many s tr1 tr2

        | MScan_trans a1, MScan_trans a2 ->
            if merge_term_maps a1 a2 then tr1
            else mk_many s tr1 tr2
        | MScan_trans a_s, Lookahead_trans a_l ->
            (match merge_sc_la a_s a_l with
               | Some mt -> Det_multi_trans mt
               | None -> mk_many s tr1 tr2)
        | MScan_trans a_s, Det_multi_trans a_mt  ->
            if merge_mt_sc a_mt a_s then tr2
            else mk_many s tr1 tr2

        | Lookahead_trans a1, Lookahead_trans a2 ->
            if merge_term_maps a1 a2 then tr1
            else mk_many s tr1 tr2
        | Lookahead_trans a, Det_multi_trans a_mt ->
            if merge_mt_la a_mt a then tr2
            else mk_many s tr1 tr2
        | Det_multi_trans a_mt, Lookahead_trans a ->
            if merge_mt_la a_mt a then tr2
            else mk_many s tr1 tr2

        | _ ->
            mk_many s tr1 tr2 in

    let rec merge_transs_compl_opt s tr1 tr2 =
      match tr1, tr2 with
        | No_trans, tr | tr, No_trans -> tr

        | Many_trans trs, tr
        | tr, Many_trans trs ->
            merge_many merge_transs_compl_opt s tr trs

        | Complete_trans nt, Complete_trans nt1 ->
            if nt <> nt1 then
              MComplete_trans [| nt; nt1 |]
            else tr1
        | Complete_trans nt, MComplete_trans nts ->
            if Util.array_contains nt nts then tr2
            else MComplete_trans (Array.append nts [|nt|])
        | MComplete_trans nts, Complete_trans nt ->
            if Util.array_contains nt nts then tr1
            else MComplete_trans (Array.append nts [|nt|])
        | MComplete_trans nts1, MComplete_trans nts2 ->
            MComplete_trans (Util.array_union nts1 nts2)
        | Complete_p_trans nt, Complete_p_trans nt1 ->
            if nt <> nt1 then
              MComplete_p_trans [| nt; nt1 |]
            else tr1
        | Complete_p_trans nt, MComplete_p_trans nts ->
            if Util.array_contains nt nts then tr2
            else MComplete_p_trans (Array.append nts [|nt|])
        | MComplete_p_trans nts, Complete_p_trans nt ->
            if Util.array_contains nt nts then tr1
            else MComplete_p_trans (Array.append nts [|nt|])
        | MComplete_p_trans nts1, MComplete_p_trans nts2 ->
            MComplete_p_trans (Util.array_union nts1 nts2)

        | _ ->
            mk_many s tr1 tr2 in

    let rec merge_transs s tr1 tr2 =
      match tr1, tr2 with
        | No_trans, tr | tr, No_trans -> tr

        | Many_trans trs, tr
        | tr, Many_trans trs ->
            merge_many merge_transs s tr trs

        | Scan_trans (c1, target1), Scan_trans (c2, target2) ->
            if c1 = c2 && target1 = target2 then tr1
            else if c1 <> c2 then
              let a = mk_scan c1 target1 in
              a.(c2) <- target2;
              MScan_trans a
            else mk_many s tr1 tr2
        | Scan_trans(c,target), MScan_trans a ->
            if a.(c) = 0 || a.(c) = target then (a.(c) <- target; tr2)
            else mk_many s tr1 tr2
        | Scan_trans(c,target), Lookahead_trans a ->
            if a.(c) = 0 then begin
              let a_mt = Array.init col_size (fun c -> lookahead_dtrans a.(c)) in
              a_mt.(c) <- scan_dtrans target;
              Det_multi_trans a_mt
            end
            else mk_many s tr1 tr2
        | Scan_trans(c,target), Det_multi_trans a_mt ->
            let t = a_mt.(c) in
            if t = 0 then
              (a_mt.(c) <- scan_dtrans target; tr2)
            else if t = scan_dtrans target then tr2
            else mk_many s tr1 tr2
        | Scan_trans _, ( Det_trans _ | Lexer_trans _
                        | TokLookahead_trans _ | RegLookahead_trans _ | ExtLookahead_trans _
                        | Complete_trans _
                        | MComplete_trans _
                        | Call_trans _
                        | Call_p_trans _
                        | Maybe_nullable_trans2 _
                        | Complete_p_trans _
                        | MComplete_p_trans _
                        | Action_trans _
                        | When_trans _
                        | When2_trans _
                        | Box_trans _) ->
            mk_many s tr1 tr2

        | _, Scan_trans _ -> merge_transs s tr2 tr1

        | Det_trans _, _ -> mk_many s tr1 tr2
        | _, Det_trans _ -> mk_many s tr1 tr2

        | Lexer_trans _, _ -> mk_many s tr1 tr2
        | _, Lexer_trans _ -> mk_many s tr1 tr2

        | MScan_trans a1, MScan_trans a2 ->
            if merge_term_maps a1 a2 then tr1
            else mk_many s tr1 tr2
        | MScan_trans a_s, Lookahead_trans a_l ->
            (match merge_sc_la a_s a_l with
               | Some mt -> Det_multi_trans mt
               | None -> mk_many s tr1 tr2)
        | MScan_trans a_s, Det_multi_trans a_mt  ->
            if merge_mt_sc a_mt a_s then tr2
            else mk_many s tr1 tr2

        | MScan_trans _, ( TokLookahead_trans _ | RegLookahead_trans _ | ExtLookahead_trans _
                         | Complete_trans _
                         | MComplete_trans _
                         | Call_trans _
                         | Maybe_nullable_trans2 _
                         | Call_p_trans _
                         | Complete_p_trans _
                         | MComplete_p_trans _
                         | Action_trans _
                         | When_trans _
                         | When2_trans _
                         | Box_trans _) ->
            mk_many s tr1 tr2

        | _, MScan_trans _ -> merge_transs s tr2 tr1


        | Call_trans _, _ | _, Call_trans _ ->
            mk_many s tr1 tr2

        | Complete_trans nt, Complete_trans nt1 ->
            if nt <> nt1 then
              MComplete_trans [| nt; nt1 |]
            else tr1
        | Complete_trans nt, MComplete_trans nts ->
            if Util.array_contains nt nts then tr2
            else MComplete_trans (Array.append nts [|nt|])

        | Complete_trans _, ( Lookahead_trans _
          | Det_multi_trans _
          | Call_p_trans _
          | Maybe_nullable_trans2 _
          | Complete_p_trans _
          | MComplete_p_trans _
          | Action_trans _
          | When_trans _
          | When2_trans _
          | Box_trans _
          | TokLookahead_trans _ | RegLookahead_trans _ | ExtLookahead_trans _) ->
            mk_many s tr1 tr2

        | _, Complete_trans _ -> merge_transs s tr2 tr1

        | MComplete_trans nts1, MComplete_trans nts2 ->
            MComplete_trans (Util.array_union nts1 nts2)

        | MComplete_trans _, ( Lookahead_trans _
          | Det_multi_trans _
          | Call_p_trans _
          | Maybe_nullable_trans2 _
          | Complete_p_trans _
          | MComplete_p_trans _
          | Action_trans _
          | When_trans _
          | When2_trans _
          | Box_trans _
          | TokLookahead_trans _ | RegLookahead_trans _ | ExtLookahead_trans _ )
        | _, MComplete_trans _ ->
            mk_many s tr1 tr2

        | Lookahead_trans a1, Lookahead_trans a2 ->
            if merge_term_maps a1 a2 then tr1
            else mk_many s tr1 tr2
        | Lookahead_trans a, Det_multi_trans a_mt ->
            if merge_mt_la a_mt a then tr2
            else mk_many s tr1 tr2
        | Lookahead_trans _, ( Call_p_trans _
          | Maybe_nullable_trans2 _
          | Complete_p_trans _
          | MComplete_p_trans _
          | Action_trans _
          | When_trans _
          | When2_trans _
          | Box_trans _
          | TokLookahead_trans _ | RegLookahead_trans _ | ExtLookahead_trans _) ->
            mk_many s tr1 tr2

        | _, Lookahead_trans _ -> merge_transs s tr2 tr1

        | TokLookahead_trans _, _ | _, TokLookahead_trans _
        | RegLookahead_trans _, _ | _, RegLookahead_trans _
        | ExtLookahead_trans _, _ | _, ExtLookahead_trans _ ->
            mk_many s tr1 tr2

        | Det_multi_trans mt1, Det_multi_trans mt2 ->
            if merge_term_maps mt1 mt2 then tr1
            else mk_many s tr1 tr2

        | Det_multi_trans _, ( Call_p_trans _
          | Maybe_nullable_trans2 _
          | Complete_p_trans _
          | MComplete_p_trans _
          | Action_trans _
          | When_trans _
          | When2_trans _
          | Box_trans _) ->
            mk_many s tr1 tr2

        | _, Det_multi_trans _ -> merge_transs s tr2 tr1

        | Complete_p_trans nt, Complete_p_trans nt1 ->
            if nt <> nt1 then
              MComplete_p_trans [| nt; nt1 |]
            else tr1
        | Complete_p_trans nt, MComplete_p_trans nts ->
            if Util.array_contains nt nts then tr2
            else MComplete_p_trans (Array.append nts [|nt|])

        | _, Complete_p_trans _ -> merge_transs s tr2 tr1

        | MComplete_p_trans nts1, MComplete_p_trans nts2 ->
            MComplete_p_trans (Util.array_union nts1 nts2)

        | _, MComplete_p_trans _ ->
            mk_many s tr1 tr2

        | ( Complete_p_trans _
          | MComplete_p_trans _
          | Call_p_trans _
          | Maybe_nullable_trans2 _
          | Action_trans _
          | When_trans _
          | When2_trans _
          | Box_trans _), ( Call_p_trans _
          | Maybe_nullable_trans2 _
          | Action_trans _
          | When_trans _
          | When2_trans _
          | Box_trans _) ->
            mk_many s tr1 tr2 in

    (* Process continuations first so that when completions are processed we can know the
       kind of the nonterminal (parameterized or not). *)
    (* Start from 1 to skip dummy state 0. *)
    for s = 1 to num_states - 1 do
      let blk = Array.unsafe_get p s in

      let ntinstrs = Util.array_extract_to_list is_nttrans blk in
      let entries = List.fold_left (add_nttrans s) [] ntinstrs in
      let grouped_entries =
        Util.group_by (fun ( (nt1 : nonterm), _) ( (nt2 : nonterm), _) -> compare nt1 nt2)
          entries in
      List.iter begin function
        | [] -> ()
        | (nt, e1) :: xs ->
            let _, es = List.split xs in
            p_nonterm_table.(s).(nt) <- Array.of_list (e1 :: es)
      end grouped_entries;
    done;

    let merge = match opt_level with
      | Full_opt ->  merge_transs
      | Compl_opt -> merge_transs_compl_opt
      | Scan_lookahead_opt -> merge_transs_det_scan_opt
      | Scan_opt ->  merge_transs_scan_opt
      | No_opt ->    merge_transs_no_opt in

    for s = 1 to num_states - 1 do
      let blk = Array.unsafe_get p s in
      (* Sort the block to ensure that eats are grouped together, at
         the beginning of the list. *)
      Array.stable_sort txcmp blk;

      let tinstrs = Util.array_extract_to_list (fun x -> not (is_nttrans x)) blk in
      term_table.(s) <-
        Util.list_map_reduce term_table.(s) (instr2trans s) (merge s) tinstrs
    done;

    {start_symb = (ntid start_symb); start_state = start_state;
     term_table = term_table; nonterm_table = nonterm_table;
     p_nonterm_table = p_nonterm_table;}

    let to_table p start_symb start_state min_nonterm num_nonterms
        f_no_arg f_no_binder =
      mk_table Full_opt p start_symb start_state min_nonterm num_nonterms
        f_no_arg f_no_binder

    let mk_called_table {nonterm_table = nts; p_nonterm_table = p_nts} =
      Array.init (Array.length nts) begin fun s ->
        let a = nts.(s) in
        let b = p_nts.(s) in
        let n = Array.length a in     (* [b] should have the same length. *)
        let r = ref [] in
        for nt = 0 to n - 1 do
          if a.(nt) > 0 || b.(nt) != [||] then r := nt :: !r
        done;
        Array.of_list !r
      end

    let get_num_states {term_table=t} = Array.length t

    let measure_percent {term_table=tbl} p =
      let rec loop tbl p sz i n =
        if i = sz then n
        else
          let n = if p tbl.(i) then n+1 else n in
          loop tbl p sz (i+1) n in
      let sz = Array.length tbl in
      let n = loop tbl p sz 0 0 in
      (float n) /. (float sz)

    let measure_percenti {term_table=tbl} p =
      let rec loop tbl p sz i n =
        if i = sz then n
        else
          let n = if p i tbl.(i) then n+1 else n in
          loop tbl p sz (i+1) n in
      let sz = Array.length tbl in
      let n = loop tbl p sz 0 0 in
      (float n) /. (float sz)

    let call_targets tbl =
      let n = Array.length tbl in
      let tgts = Array.make n false in
      for i = 0 to n - 1 do
        match tbl.(i) with
          | Call_trans t -> tgts.(t) <- true
          | Many_trans txs ->
              let m = Array.length txs in
              for j = 0 to m - 1 do
                match txs.(j) with
                  | Call_trans t -> tgts.(t) <- true
                  | _ -> ()
              done
          | _ -> ()
      done;
      tgts

    (** Return the set of states reachable from [s] via action
        edges. *)
  let refl_trans_closure term_table start =
    let num_states = Array.length term_table in
    let unvisited = Array.make num_states true in

    (** a depth-first search from any state in the list of arguments.
        we assume that all arguments are unvisited. *)
    let rec loop reachables s =
      let rec step reachables = function
        | Action_trans (_, t) ->
            if unvisited.(t) then loop reachables t else reachables

        | Many_trans trans ->
            Array.fold_left step reachables trans

        | No_trans
        | Maybe_nullable_trans2 _
        | Scan_trans _
        | MScan_trans _
        | Lookahead_trans _
        | Det_multi_trans _
        | TokLookahead_trans _
        | RegLookahead_trans _
        | ExtLookahead_trans _
        | Call_trans _ | Call_p_trans _
        | Complete_trans _ | Complete_p_trans _
        | MComplete_trans _ | MComplete_p_trans _
        | When_trans _
        | When2_trans _
        | Box_trans _
        | Det_trans _ | Lexer_trans _
          -> reachables in
      unvisited.(s) <- false;  (* mark s as visited. *)
      let rs = s::reachables in (* add s to list.     *)
      step rs term_table.(s) in
    loop [] start

  let reachable_calls term_table start =
    let rec has_call = function
      | Call_trans _ | Call_p_trans _ -> true
      | Many_trans trans -> Util.array_exists has_call trans
      | _ -> false in
    List.filter (fun s -> has_call term_table.(s)) (refl_trans_closure term_table start)

  let count_reachable_calls term_table start =
    (* sure could use a good optimizing compiler here: *)
    List.length (reachable_calls term_table start)

  (** Computer an interger property for every state included in the
      [states] set. [states] is encoded with a boolean array. If [states.(i)]
      then [i] is in the set. *)
  let compute_integer_property p term_table states =
    let n = Array.length states in
    let rec loop i vs =
      if i >= n then vs
      else loop (i + 1) (if states.(i) then (i, p term_table i) ::  vs else vs) in
    loop 1 []

  let compute_callee_reachable_calls term_table =
    let callees = call_targets term_table in
    compute_integer_property count_reachable_calls term_table callees

end


(* Todo:
    x separate into conversion function and merge function.
    x Factor code more intelligently in merge function.

    x Separately (and later) write optimizer for Many_trans. First verify
    that it matters by looking at prevalence in real grammars. Optimizations
    would include grouping together scans and completes. Would change [merge_many]

    Handle calls with params.

    x PERF: Can we statically mark call transitions to states with completions?
    I don't see why not and then we wouldn't need to always check.

    * PERF: improve RComp -- only needs to be checked by calls, so doesn't need to
    be in term_table.
*)
