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

(* Generate the replay functions *)
let replay gr =
  let l = ref 2000 in
  let uses_history = ref false in
  let fresh() = uses_history := true; Util.postincr l in

  let b = Buffer.create 11 in
  let pr fmt = Printf.bprintf b fmt in
  let fname n = Printf.sprintf "_r_%s" (Variables.bnf2ocaml n) in

  let hproj = if gr.wrapped_history then "Ykd_int" else "" in

  let rec loop r =
    if not(r.a.late_relevant) then pr "()" else
    match r.r with
    | Action (early,Some e) ->
        pr "%s" e
    | Symb(n,x,y,Some e) ->
        pr "%s(_n,ykinput,%s)" (fname n) e
    | Symb(n,_,_,None) ->
        pr "%s(_n,ykinput)" (fname n)
    | Delay(false,_,_) ->
        uses_history := true; pr "_n()"
    | Position false ->
        (* TODO: eliminate in favor of Delay(false,...) *)
        uses_history := true; pr "_n()"
    | Opt r1 ->
        loop r1
    | Alt _ ->
        let alts = alt2rules r in
        pr "(match _n() with";
        List.iter
          (fun r1 ->
            let l = fresh() in
            r1.a.pre <- l;
            pr "\n | %s(%d) -> (" hproj l;
            loop r1;
            pr ")")
          alts;
        pr "\n | _ -> raise Exit)"
    | Assign(r1,_,late) ->
        Util.impossible "TODO late attributes"
    | Seq(r1,_,None,r2) ->
        loop r1;
        pr "; ";
        loop r2
    | Seq(r1,_,Some y,r2) ->
        pr "(let %s = " y;
        loop r1;
        pr " in ";
        loop r2;
        pr ")"
    | Star(Accumulate(_,Some(x,e)),r1) ->
        let l_body = fresh() in
        let l_done = fresh() in
        r1.a.pre <- l_body;
        r.a.post <- l_done;
        let g = Variables.fresh() in
        pr "(let rec %s %s =\n" g x;
        pr "(match _n() with %s(%d) -> %s | _ (*%d*) ->\n %s(" hproj l_done x l_body g;
        loop r1;
        pr "))\nin %s(%s))" g e
    | Star(_,r1) ->
        let l_body = fresh() in
        let l_done = fresh() in
        r1.a.pre <- l_body;
        r.a.post <- l_done;
        pr "(while (match _n() with %s(%d) -> true | _ (*%d*) -> false) do\n" hproj l_body l_done;
        loop r1;
        pr "done)\n"

          (* cases below are not late relevant *)
    | When _            -> Util.impossible "Replay.replay.When"
    | Box _             -> Util.impossible "Replay.replay.Box"
    | DBranch _         -> Util.impossible "Replay.replay.DBranch"
    | Delay(true,_,_)   -> Util.impossible "Replay.replay.Delay(true,_,_)"
    | Position true     -> Util.impossible "Replay.replay.Position true"
    | Action(_,None)    -> Util.impossible "Replay.replay.Action(_,None)"
    | CharRange _       -> Util.impossible "Replay.replay.CharRange"
    | Prose _           -> Util.impossible "Replay.replay.Prose"
    | Lookahead _       -> Util.impossible "Replay.replay.Lookahead"
    | Lit _             -> Util.impossible "Replay.replay.Lit"
          (* cases below should have been desugared *)
    | Rcount _          -> Util.impossible "Replay.replay.Rcount"
    | Hash _            -> Util.impossible "Replay.replay.Hash"
    | Minus _           -> Util.impossible "Replay.replay.Minus" in
  let first = ref true in
  List.iter
    (function RuleDef(n,r,a) ->
      if not r.a.late_relevant then () else begin
        pr "%s %s(_n,ykinput%s) = "
          (if !first then "\nlet rec\n" else "\nand\n")
          (fname n)
          (match a.Attr.late_params with None -> "" | Some x -> ","^x);
        first := false;
        loop r
      end
      | _ -> ())
    gr.ds;
  pr "\n";
  add_to_prologue gr (Buffer.contents b);
  !uses_history

(* Generate the reversing functions *)
let reverse gr =
  let b = Buffer.create 11 in
  let pr fmt = Printf.bprintf b fmt in
  let fname n = Printf.sprintf "_rv_%s" (Variables.bnf2ocaml n) in

  let hproj = if gr.wrapped_history then "Ykd_int" else "" in

  let rec loop r =
    if not(r.a.late_relevant) then pr "()" else
    match r.r with
    | Action (early,Some e) ->
        pr "()"
    | Symb(n,_,_,_) ->
        pr "%s()" (fname n)
    | Delay _ ->
        pr "push(_n())"
    | Position false ->       (* TODO: eliminate in favor of Delay(false,...) *)
        pr "push(_n())"
    | Opt r1 ->
        loop r1
    | Alt _ ->
        let alts = alt2rules r in
        pr "(match _n() with";
        List.iter
          (fun r1 ->
            let l = r1.a.pre in
            pr "\n | %s(%d) -> (" hproj l;
            loop r1;
            pr "; push(%s(%d)))" hproj l)
          alts;
        pr "\n | _ -> raise Exit)"
    | Assign(r1,_,late) ->
        Util.impossible "TODO late attributes"
    | Seq(r1,_,_,r2) ->
        loop r2;
        pr ";";
        loop r1
    | Star(x,r1) ->
        let l_body = r1.a.pre in
        let l_done = r.a.post in
        pr "push(%d); " l_done;
        pr "while (match _n() with %s(%d) -> true | _ (*%d*)-> false) do\n " hproj l_body l_done;
        loop r1;
        pr "; push(%s(%d))\n" hproj l_body;
        pr "done\n"

          (* cases below are not late relevant *)
    | When _            -> Util.impossible "Replay.reverse.When"
    | Box _             -> Util.impossible "Replay.reverse.Box"
    | DBranch _         -> Util.impossible "Replay.reverse.DBranch"
    | Position true     -> Util.impossible "Replay.reverse.Position true"
    | Action(_,None)    -> Util.impossible "Replay.reverse.Action(_,None)"
    | CharRange _       -> Util.impossible "Replay.reverse.CharRange"
    | Prose _           -> Util.impossible "Replay.reverse.Prose"
    | Lookahead _       -> Util.impossible "Replay.reverse.Lookahead"
    | Lit _             -> Util.impossible "Replay.reverse.Lit"
          (* cases below should have been desugared *)
    | Rcount _          -> Util.impossible "Replay.reverse.Rcount"
    | Hash _            -> Util.impossible "Replay.reverse.Hash"
    | Minus _           -> Util.impossible "Replay.reverse.Minus" in
  pr "class ['a] rvs (labels: 'a History.postfix) =\n";
  pr "let s = ref [] in\n";
  pr "let push x = s := x::!s in\n";
  pr "let rec _n() = let (x,_) = labels#next() in x\n";
  List.iter
    (function RuleDef(n,r,a) ->
      if not r.a.late_relevant then () else begin
        pr "and %s() = " (fname n);
        loop r;
        pr "\n"
      end
      | _ -> ())
    gr.ds;
  pr "in\n";
  pr "object (self)\n";
  pr "method next() = (match !s with hd::tl -> (s := tl; hd) | _ -> raise Not_found)\n";
  pr "initializer %s()\n" (fname gr.start_symbol);
  pr "end\n";
  add_to_prologue gr (Buffer.contents b)

(* Transform a Gul grammar to explicitly push replay labels *)
let transform gr =
  if not gr.grammar_late_relevant then () else
  let uses_history = replay gr in
  if uses_history then reverse gr; (* some grammars have late actions but never push anything on the history *)
  let mkOUTPUT x = mkRHS(Delay(false,string_of_int x,None)) in
  let rec loop r =
    if not(r.a.late_relevant) then () else
    match r.r with
    | Action (early,Some e) ->
        () (*r.r <- (mkACTION2(early,None)).r*) (* Don't remove, so relevance stays the same *)
    | Symb(n,x,y,Some e) ->
        () (*r.r <- (mkSYMB2(n,x,y,None)).r*) (* Don't remove, so relevance stays the same *)
    | Symb(_,_,_,None)
    | Delay(false,_,_)
    | Position false ->
        ()
    | Opt r1 ->
        loop r1
    | Alt _ ->
        let alts = alt2rules r in
        List.iter
          (fun r ->
            let l = r.a.pre in
            loop r;
            r.r <- (mkSEQ[dupRule r;mkOUTPUT(l)]).r)
          alts
    | Assign(r1,_,late) ->
        Util.impossible "TODO late attributes"
    | Seq(r1,x,y,r2) ->
        loop r1;
        loop r2;
        () (*if y<>None then r.r <- (mkSEQ2(r1,x,None,r2)).r*)  (* Don't remove, so relevance stays the same *)
    | Star(x,r1) ->
        let l_body = r1.a.pre in
        let l_done = r.a.post in
        loop r1;
        r.r <- (mkSEQ[mkOUTPUT(l_done);mkSTAR2(x,mkSEQ[r1;mkOUTPUT(l_body)])]).r

            (* cases below are not late relevant *)
    | When _            -> Util.impossible "Replay.transform.When"
    | Box _             -> Util.impossible "Replay.transform.Box"
    | DBranch _         -> Util.impossible "Replay.transform.DBranch"
    | Delay(true,_,_)   -> Util.impossible "Replay.transform.Delay(true,_,_)"
    | Position true     -> Util.impossible "Replay.transform.Position true"
    | Action(_,None)    -> Util.impossible "Replay.transform.Action(_,None)"
    | CharRange _       -> Util.impossible "Replay.transform.CharRange"
    | Prose _           -> Util.impossible "Replay.transform.Prose"
    | Lookahead _       -> Util.impossible "Replay.transform.Lookahead"
    | Lit _             -> Util.impossible "Replay.transform.Lit"
          (* cases below should have been desugared *)
    | Rcount _          -> Util.impossible "Replay.transform.Rcount"
    | Hash _            -> Util.impossible "Replay.transform.Hash"
    | Minus _           -> Util.impossible "Replay.transform.Minus" in
  List.iter
    (function RuleDef(n,r,a) ->
      if r.a.late_relevant then loop r
      | _ -> ())
    gr.ds
