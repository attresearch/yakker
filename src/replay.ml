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

(* The replay transform *)
open Yak
open Gul
open Variables

let replay_prologue = "
(*REPLAY PROLOGUE*)
"
let transform gr =
  let skip_count = ref 0 in
  let notskip_count = ref 0 in
  let skipped_labels = ref PSet.empty in
  let skip l =
    if !Compileopt.skip_opt then
      (skipped_labels := PSet.add l !skipped_labels; skip_count := !skip_count+1; true)
    else (notskip_count := !notskip_count+1; false) in
  let hproj = if gr.wrapped_history then "Ykd_int" else "" in
  let n_int =
    if gr.wrapped_history then
      "(match _n() with Ykd_int x -> x | _ -> failwith \"Replay.transform.n_int\")"
    else "_n()" in
  let match_cases = function
    | [] -> Util.impossible "Replay.transform.match_cases([])"
    | [(label,body)] ->
        if skip label then
          Printf.sprintf "(%s)\n" body
        else begin
          (* Could just use next case, but this prints a bit more nicely *)
          if !Compileopt.check_labels then
            Printf.sprintf "(_i(%d,%s); %s)\n " label n_int body (* NB _i defn is added to prologue by Main *)
          else
            Printf.sprintf "(ignore (*%d*) (%s); %s)\n " label n_int body (* prevent match warning *)
        end
    | cases ->
        let b = Buffer.create 11 in
        Printf.bprintf b "\n (match %s with\n " n_int;
        let rec loop = function [] -> Util.impossible "Replay.transform.match_cases"
          | (label,body)::[] ->
              notskip_count := !notskip_count+1;
              if !Compileopt.check_labels then
                Printf.bprintf b "| %s(%d) -> (%s)\n " hproj label body
              else
                Printf.bprintf b "| _(*%d*) -> (%s)\n " label body (* prevent match warning *)
          | (label,body)::tl ->
              notskip_count := !notskip_count+1;
              Printf.bprintf b "| %s(%d) -> (%s)\n " hproj label body;
              loop tl
        in loop cases;
        Printf.bprintf b ")";
        Buffer.contents b in
  (* invariant: rp is only applied to late-relevant r, and returns a non-empty list *)
  let rec rp r =
    if not(r.a.late_relevant) then (Util.warn Util.Sys_warn "Invariant violated: replay transform on non-late-relevant rhs"; []) else
    let (pre,post) = (r.a.pre,r.a.post) in
    match r.r with
      | Action (_,Some e) ->
          [(pre,e)]
      | Symb(n,_,_,Some e) -> (* TODO: attributes *)
          [(pre,Printf.sprintf "_r_%s(_n,_ps,ykinput,%s)" (Variables.bnf2ocaml n) e)]
      | Symb(n,_,_,None) -> (* TODO: attributes *)
          [(pre,Printf.sprintf "_r_%s(_n,_ps,ykinput)" (Variables.bnf2ocaml n))]
      | When _
      | Box _ ->
          (* Impossible: not late relevant *)
          []
      | Delay _ ->
          [(pre,"_n()")]
      | Position false ->
          [(pre,"_ps()")]
      | Opt r1 ->
          rp r1
      | Alt(r1,r2) ->
          (* written this way to preserve the invariant that rp is never applied to non-late-relevant rhs *)
          if not(r1.a.late_relevant) then rp r2
          else if not(r2.a.late_relevant) then rp r1
          else (rp r1)@(rp r2)
      | Assign(r1,_,late) ->
          Util.impossible "TODO late attributes"
      | Seq(r1,_,late,r2) ->
          (match r1.a.late_relevant, r2.a.late_relevant with
          | true,true ->
              let y =
                (match late with Some y -> y
                | None -> Variables.fresh()) in (* this case handled by normalization in writeup *)
              [(pre,
                Printf.sprintf
                  "\n (let %s = %s in %s)"
                  y
                  (match_cases (rp r1))
                  (match_cases (rp r2)))]
          | true,false ->
              (*TJIM: WRITEUP IS WRONG, NEEDS TO SEND UNIT TO K*)
              (*THEREFORE, ERASURE IN WRITEUP IS WRONG*)
              (*FIX BY CHANGING NORMALIZATION IN WRITEUP?*)
              (* NB unlike coroutine, we DO need to dispatch on pre to accomplish this *)
              let y =
                (match late with Some y -> y
                | None -> Variables.fresh()) in (* this case handled by normalization in writeup *)
              [(pre,
                Printf.sprintf
                  "\n (let %s = %s in ())"
                  y
                  (match_cases (rp r1)))]
          | false,true ->
              (match late with
              | None ->
                  rp r2 (* NB no dispatch required here... *)
              | Some y ->
                  [(pre, (* ...but here we must dispatch to bind x to unit *) (*HANDLED BY NORMALIZATION IN WRITEUP*)
                    Printf.sprintf "(let %s=() in %s)" y (match_cases (rp r2)))])
          | false,false ->
              (* this case is impossible because our analysis marks this as not late relevant,
                 regardless of whether there is a variable binding *)
              Util.warn Util.Sys_warn "Impossible case in replay transformation: not late relevant.";
              Pr.pr_rule_channel stderr r;
              prerr_newline ();
              [])
      | Star(Accumulate(_,Some(x,e)),r1) ->
          let g,y = fresh(),fresh() in
          let body_cases =
            List.map (fun (label,case) -> (label,Printf.sprintf "%s(%s)" g case)) (rp r1) in
          let all_cases = (post,x)::body_cases in
          [(pre,
            Printf.sprintf
              "\n (let rec %s %s = %s in %s(%s))"
              g x (match_cases all_cases) g e)]
      | Star(Accumulate(_,None),r1) (* r1 must be late relevant so we need to track pre and post anyway *)
      | Star(Bounds _,r1) ->
          (* like the last case, using a fresh variable for x and () for e *)
          let e = "()" in
          let x = Variables.fresh() in
          (* from here on, identical --- refactor *)
          let g,y = fresh(),fresh() in
          let body_cases =
            List.map (fun (label,case) -> (label,Printf.sprintf "%s(%s)" g case)) (rp r1) in
          let all_cases = (post,x)::body_cases in
          [(pre,
            Printf.sprintf
              "\n (let rec %s %s = %s in %s(%s))"
              g x (match_cases all_cases) g e)]

      (* cases below are not late relevant *)
      | Position true     -> Util.impossible "Replay.rp.Position true"
      | Action(_,None)    -> Util.impossible "Replay.rp.Action(_,None)"
      | CharRange _       -> Util.impossible "Replay.rp.CharRange"
      | Prose _           -> Util.impossible "Replay.rp.Prose"
      | Lookahead _       -> Util.impossible "Replay.rp.Lookahead"
      | Lit _             -> Util.impossible "Replay.rp.Lit"
      (* cases below should have been desugared *)
      | Rcount _          -> Util.impossible "Replay.rp.Rcount"
      | Hash _            -> Util.impossible "Replay.rp.Hash"
      | Minus _           -> Util.impossible "Replay.rp.Minus"
  in
  add_to_prologue gr replay_prologue;
  let first = ref true in
  List.iter
    (function RuleDef(n,r,a) ->
      if r.a.late_relevant then begin
        let replay_body = match_cases (rp r) in
        add_to_prologue gr
          (Printf.sprintf "%s_r_%s(_n,_ps,ykinput%s) = %s\n "
             (if !first then "let rec\n" else "and\n")
             (Variables.bnf2ocaml n)
             (match a.Attr.late_params with None -> "" | Some x -> ","^x)
             replay_body);
        first := false
      end
      | _ -> ())
    gr.ds;
  Yak.Logging.log Yak.Logging.Features.verbose "\nLabels skipped: %d ; not skipped: %d\n%!" !skip_count !notskip_count;
  !skipped_labels
