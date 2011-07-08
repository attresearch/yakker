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

(**
 Lift out non-symbol, non-character set lookahead to a fresh nonterminal. Needed by
   the Early automaton backend.
*)

let desugar_gil gr =
  let ds = ref [] in
  let define r =
    let nt1 = Variables.fresh_nonterminal () in
    ds := (nt1, r) :: !ds;
    Gil.Symb(nt1, None, None) in
  let rec loop r =
    let dleaf = function
      | Gil.Lookahead(b, r2) ->
          (* If it is a symbol or a character set, we leave it in
             place. Otherwise, we lift it out. *)
          let r2' = match loop r2 with
            | (Gil.Symb (nt, None, None)) as r -> r
            | r -> (match Gil.to_cs r with
                      | Some _ -> r
                      | None -> define r) in
          Gil.Lookahead(b, r2')
      | ( Gil.Symb _
        | Gil.When _ | Gil.When_special _ | Gil.DBranch _
        | Gil.Action _ | Gil.Box _
        | Gil.CharRange _ | Gil.Lit _) as r -> r
      | (Gil.Alt _ | Gil.Star _ | Gil.Seq _) ->
          invalid_arg "structural recursion should be handled by Gil.map" in
    Gil.map dleaf r in
  let d2 = List.map (fun (n, r) -> (n, loop r)) gr in
  !ds @ d2

open Gul

let desugar gr =
  Minus.cs_annot gr;
  let define r =
    let nt1 = Variables.fresh_nonterminal () in
    gr.ds <- (RuleDef(nt1, (dupRhs r), mkAttr()))::gr.ds;
    Symb(nt1,None,[],None)
  in
  let rec loop r = match r.r with
    | Lookahead(b, r2) ->
        loop r2;
        (* If it is a symbol or a character set, we leave it in
        place. Otherwise, we lift it out. *)
        (match r2 with
           | {r=Symb _}
           | {a={css=Some(_)}} -> ()
           | _ ->
               r2.r <- define r2)
    | Symb _ | Position _ | Prose _
    | When _ | Action _ | Box _ | Delay _
    | DBranch _
    | CharRange _ | Minus _ | Lit _ -> ()
    | Seq(r2,_,_,r3)
    | Alt(r2,r3) ->
        loop r2; loop r3
    | Opt(r2)
    | Assign(r2,_,_)
    | Rcount(_,r2)
    | Star(_,r2)
    | Hash(_,r2) ->
        loop r2
  in
  List.iter (function RuleDef(_,r,_) -> loop r | _ -> ()) gr.ds
