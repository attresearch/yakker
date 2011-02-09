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

(* The coroutine transform *)
open Yak
open Gul

let yk_done e = Printf.sprintf "Yk_done(%s)" e
let yk_box e k =
  let i,j,ykb,v = Variables.fresh(),Variables.fresh(),Variables.fresh(),Variables.fresh() in
  Printf.sprintf "Yk_box(fun %s %s -> match (%s) %s %s with None -> None | Some (%s,%s) -> Some(%s,%s))" i ykb e i ykb j v j (k v)

let yk_when e = Printf.sprintf "Yk_when(%s)" e
let yk_delay e1 e2 = Printf.sprintf "Yk_delay(%s,%s)" e1 e2

let string_of_varset s =
  String.concat " " (PSet.fold (fun x y -> x::y) s [])

let transform r0 =
  let ulam = function
    | [] -> Util.impossible "Coroutine.transform.ulam([])"
    | [(label,body)] ->
        if !Compileopt.check_labels then
          Printf.sprintf "(fun %d pos_ -> %s)" label body
        else
          (* Could just use next case, but this prints a bit more nicely *)
          Printf.sprintf "(fun _(*%d*) pos_ -> %s)" label body (* prevent match warning *)
    | cases ->
        let b = Buffer.create 11 in
        Printf.bprintf b "(function\n ";
        let rec loop = function [] -> Util.impossible "Coroutine.transform.ulam"
          | (label,body)::[] ->
              if !Compileopt.check_labels then
                Printf.bprintf b "| %d ->\n (fun pos_ -> %s)" label body
              else
                Printf.bprintf b "| _(*%d*) ->\n (fun pos_ -> %s)" label body (* prevent match warning *)
          | (label,body)::tl ->
              Printf.bprintf b "| %d ->\n (fun pos_ -> %s)\n " label body;
              loop tl
        in loop cases;
        Printf.bprintf b ")";
        Buffer.contents b in
  let t_ulam l = "_t"^(ulam l) in
  (* invariant: c is only applied to early-relevant r, and returns a non-empty list *)
  let rec c r k =
    if not(r.a.early_relevant) then
      (Util.warn Util.Sys_warn "Invariant violated: coroutine transform on non-early-relevant rhs";
       Pr.pr_rule_channel stderr r;
       prerr_newline ();
       [])
    else
    let (pre,post) = (r.a.pre,r.a.post) in
    if pre=0 then (match r.r with Alt _ | Opt _ -> () | _ -> Printf.eprintf "Warning: pre=0 for %s\n%!" (Pr.rule2string r));
    match r.r with
      | Action (Some e,_) ->
          [(pre, k(e))]
      | Box(e,_,_) ->  (*TJIM: in process of changing box return type*)
          [(pre, yk_box e k)]
      | Symb(n,Some e,_,_) -> (* TODO: attributes *)
          let e = yk_done e in
          let eta k =
            let x = Variables.fresh() in
            Printf.sprintf "Yk_bind(function Yk_done(%s) -> %s | _ -> failwith \"bind-%d\")" x (k(x)) post in
          [(pre, e);
           (post, eta k)]
      | Symb(n,None,_,_) -> (* nonterminal takes no arguments but returns an early result *) (* TODO: attributes *)
          let eta k =
            let x = Variables.fresh() in
            Printf.sprintf "Yk_bind(function Yk_done(%s) -> %s | _ -> failwith \"bind=%d\")" x (k(x)) post in
          [(post, eta k)]
      | Delay(e,_) ->
          let e = yk_delay (k"(_wv0)") e in
          [(pre,e)]
      | Position true ->
          [(pre,k("pos_"))] (* pos_ is bound later by t_ulam; note that k cannot capture variables *)
      | When e ->
          let e = yk_when e in
          [(pre, e);
           (post, k("(_wv0)"))]
      | DBranch(e,_) -> (* TODO: attributes *)
          if !Compileopt.late_only_dbranch then
            (Util.warn Util.Sys_warn "Invariant violated: coroutine transform on late-only dbranch.";
            [])
          else
            let e = yk_done e in
            let eta k =
              let x = Variables.fresh() in
              Printf.sprintf "Yk_bind(function Yk_done(%s) -> %s | _ -> failwith \"bind-%d\")" x (k(x)) post in
            [(pre, e);
             (post, eta k)]
      | Alt(r1,r2) ->
          (* NB both r1 and r2 are early_relevant b/c of force_early_alts *)
          (c r1 k)@(c r2 k)
      | Opt r1 ->
          Util.impossible "Coroutine.coroutine.Opt"
      | Assign(r1,None,_) -> c r1 k
      | Assign(r1,Some x,_) ->
          (match r1.a.early_relevant with
            true ->
              let g = Variables.fresh() in
              [(pre,
                Printf.sprintf
                  "let %s %s = %s in %s"
                  g x (k("(_wv0)"))
                  (t_ulam(c r1 (fun hole -> Printf.sprintf "%s(%s)" g hole))))]
          | false ->
              Printf.eprintf "Warning: assigning _wv0 in %s\n%!" (Pr.rule2string r);
              [(pre,
                Printf.sprintf "let %s=(_wv0) in %s" x (k("(_wv0)")))])
      | Seq(r1,early,_,r2) ->
          (match r1.a.early_relevant,r2.a.early_relevant with
          | true,true ->
              let x =
                (match early with Some x -> x
                | None -> Variables.fresh()) in (* this case handled by normalization in writeup *)
              let g = Variables.fresh() in
              let assignments = string_of_varset r1.a.early_assignments in
              [(pre,
                Printf.sprintf
                  "let %s %s %s = %s in %s"
                  g x assignments (t_ulam(c r2 k))
                  (t_ulam(c r1 (fun hole -> Printf.sprintf "%s (%s) %s" g hole assignments))))]
          | true,false ->
              (*TJIM: WRITEUP IS WRONG, NEEDS TO SEND UNIT TO K*)
              (*THEREFORE, ERASURE IN WRITEUP IS WRONG*)
              (*FIX BY CHANGING NORMALIZATION IN WRITEUP?*)
              (* the continuation k must receive () from r2 *)
              (* NB we don't need to dispatch on pre to accomplish this *)
              c r1 (fun x -> (k (Printf.sprintf "ignore(%s);_wv0" x)))
          | false,true ->
              (match early with
              | None ->
                  c r2 k (* NB no dispatch required here... *)
              | Some x ->
                  Printf.eprintf "Warning: binding to _wv0 in %s\n%!" (Pr.rule2string r);
                  [(pre, (* ...but here we must dispatch to bind x to unit *) (*HANDLED BY NORMALIZATION IN WRITEUP*)
                    Printf.sprintf "let %s=(_wv0) in %s" x (t_ulam(c r2 k)))])
          | false,false ->
              (* this case is impossible because our analysis marks this as not early relevant,
                 regardless of whether there is a variable binding *)
              Util.warn Util.Sys_warn "Impossible case in coroutine transformation: not early relevant";
              [])
      | Star(loop_condition,r1) ->
          let (x,e) =
            (match loop_condition with
            | Accumulate(Some(x,e),_) -> x,e
            | Accumulate(None,_) (* in this case r1 must be early relevant so we need to track pre and post anyway *)
            | Bounds _ -> Variables.fresh(),"_wv0") in
          let g = Variables.fresh() in
          let assignments = string_of_varset r1.a.early_assignments in
          [(pre,
            Printf.sprintf
              "let rec %s %s %s = %s in %s (%s) %s"
              g x assignments (t_ulam((post,k(x))::(c r1 (fun hole -> Printf.sprintf "%s (%s) %s" g hole assignments))))
              g e assignments)]
      | Position false
      | Action(None,_)
      | CharRange(_,_)
      | Prose(_)
      | Lookahead _
      | Lit _  ->
          Util.warn Util.Sys_warn "Impossible case 2 in coroutine transformation: not early relevant";
          []
      | Rcount(_,_)
      | Hash(_,_)
      | Minus(_,_) ->
          Util.warn Util.Sys_warn "Impossible case 3 in coroutine transformation: not desugared";
          [] (* should have been desugared *)
  in
  t_ulam(c r0 (fun x->"Yk_done("^x^")"))
