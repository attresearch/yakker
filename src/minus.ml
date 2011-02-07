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

open Yak
open Gul

let cs_CHARRANGE(min,max) =
  let r = mkCHARRANGE(min,max) in
  r.a.css <- Some(Cs.range min (max+1));
  r

let rec cs_annot_rule gr r = begin
  r.a.css <- None;
  (match r.r with
    Symb(s,_,_,_) ->
      r.a.css <- (try PMap.find s gr.m with Not_found -> None)
  | Lit(case_sensitive,s) ->
      let len = String.length s in
      (if len <> 1 then r.a.css <- None else
      let c = String.get s 0 in
      if case_sensitive then
        let lower = Char.code(Char.lowercase c) in
        let upper = Char.code(Char.uppercase c) in
        let result = Cs.empty() in
        Cs.insert result lower;
        Cs.insert result upper;
        r.a.css <- Some result
      else
        let result = Cs.empty() in
        Cs.insert result (Char.code c);
        r.a.css <- Some result)
  | CharRange(min,max) ->
      r.a.css <- Some(Cs.range min (max+1))
  | Seq(r2,_,_,r3) ->
      (* Conservative: if r2 or r3 is empty string then could be character set *)
      ignore(cs_annot_rule gr r2);
      ignore(cs_annot_rule gr r3)
  | Assign(r2,_,_) ->
      ignore(cs_annot_rule gr r2)
  | Alt(r2,r3) ->
      (match cs_annot_rule gr r2,cs_annot_rule gr r3 with
        Some x, Some y ->
          let result = Cs.dup x in
          Cs.union result y;
          r.a.css <- Some result
      | _,_ -> ())
  | Minus(r2,r3) ->
      if r.a.early_relevant || r.a.late_relevant then () else
      (match cs_annot_rule gr r2,cs_annot_rule gr r3 with
        Some x, Some y ->
          let result = Cs.dup x in
          Cs.difference result y;
          r.a.css <- Some result
      | _,_ ->
          Printf.eprintf "Error: minus is used on non-character sets: ";
          Pr.pr_rule_channel stderr r;
          Printf.eprintf "\n%!")
  | Rcount(_,r2) (* Rcount conservative -- if (e == 1) and r2 is a charset then this is a charset *)
  | Lookahead (_,r2)
  | Opt r2
  | Star(_,r2)
  | Hash(_,r2) ->
      ignore(cs_annot_rule gr r2)
  | DBranch _ | Prose _ | Action _ | Position _ | Box _ | Delay _ | When _ -> ());
  r.a.css
end

let cs_annot gr =
  List.iter
    (function
        RuleDef(n,r,a) ->
          gr.m <- PMap.add n (cs_annot_rule gr r) gr.m
      | _ -> ())
    (sort_definitions gr.ds)

let cs2rule rng =
  let rngs =
    List.map
      (fun (min,max_plus_1) -> cs_CHARRANGE(min,max_plus_1 - 1))
      (Cs.cs2ranges rng) in
  match rngs with
    [] ->
      Printf.eprintf "cs2rule called on empty character set\n";
      mkLIT("")
  | hd::tl ->
      List.fold_left
        (fun result r ->
          let r3 = mkALT [result;r] in
          match result.a.css,r.a.css with
            Some cs1,Some cs2 ->
              let cs3 = Cs.dup cs1 in
              Cs.union cs3 cs2;
              r3.a.css <- Some(cs3);
              r3
          | _,_ -> Printf.eprintf "impossible cs2rule\n"; result)
        hd
        tl

(** minus elimination *)
let rec elim_rule r = match r.r with
  | Minus _ ->
      (match r.a.css with None -> ()
      | Some cs ->
          r.r <- (cs2rule cs).r)
  | Alt(r2,r3)
  | Seq(r2,_,_,r3) ->
      elim_rule r2;
      elim_rule r3
  | Lookahead(_,r2)
  | Opt(r2)
  | Assign(r2,_,_)
  | Rcount(_,r2)
  | Star(_,r2)
  | Hash(_,r2) ->
      elim_rule r2
  | Symb _ | CharRange _ | Lit _
  | DBranch _
  | Box _ | Prose _ | When _ | Action _ | Delay _ | Position _ -> ()

(* more general character set inlining *)
(* broken because lookahead inlining is broken. *)
let rec inline_rule r = match r.r with
  | Symb _
  | Minus(_,_) ->
      (match r.a.css with None -> ()
      | Some cs ->
          r.r <- (cs2rule cs).r)
  | Alt(r2,r3)
  | Seq(r2,_,_,r3) ->
      inline_rule r2;
      inline_rule r3
  | Lookahead (_,r2)
  | Opt(r2)
  | Assign(r2,_,_)
  | Rcount(_,r2)
  | Star(_,r2)
  | Hash(_,r2) ->
      inline_rule r2
  | CharRange _ | Lit _
  | DBranch _
  | Box _ | Prose _ | When _ | Action _ | Delay _ | Position _ -> ()

(* broken *)
let rec inline_rule_la r = match r.r with
  | Minus(_,_) ->
      (match r.a.css with None -> ()
      | Some cs ->
          r.r <- (cs2rule cs).r)
  | Alt(r2,r3)
  | Seq(r2,_,_,r3) ->
      inline_rule_la r2;
      inline_rule_la r3
  | Lookahead (_, ({r=Symb _} as r2)) -> inline_rule r2
  | Lookahead (_,r2)
  | Opt(r2)
  | Assign(r2,_,_)
  | Rcount(_,r2)
  | Star(_,r2)
  | Hash(_,r2) ->
      inline_rule_la r2
  | Symb _ | CharRange _ | Lit _
  | DBranch _
  | Box _ | Prose _ | When _ | Action _ | Delay _ | Position _ -> ()

let rec inline_rule_no_la r = match r.r with
  | Symb _
  | Minus(_,_) ->
      (match r.a.css with None -> ()
      | Some cs ->
          r.r <- (cs2rule cs).r)
  | Alt(r2,r3)
  | Seq(r2,_,_,r3) ->
      inline_rule_no_la r2;
      inline_rule_no_la r3
  | Lookahead (_,{r=Symb _}) -> ()
  | Lookahead (_,r2) -> inline_rule_no_la r2
  | Opt(r2)
  | Assign(r2,_,_)
  | Rcount(_,r2)
  | Star(_,r2)
  | Hash(_,r2) ->
      inline_rule_no_la r2
  | CharRange _ | Lit _
  | DBranch _
  | Box _ | Prose _ | When _ | Action _ | Delay _ | Position _ -> ()

let elim gr =
  cs_annot gr;
  List.iter
    (function
        RuleDef(n,r,_) ->
          if !Compileopt.inline_cs then inline_rule r else elim_rule r
      | _ -> ())
    gr.ds
