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

open Gul

(* TODO: use commas() above or parameterize in some way *)

(* encode a hash rule as combination of literal, sequence and star *)
let hash2star = function
    (Bounds(m, n), r) ->
      let m_dec = if m>0 then m-1 else 0 in
      let r_s =
        (match n with
        | Num(0) ->
            mkSTAR(m,n,r)
        | Num(n) ->
            mkSEQ([r; mkSTAR(m_dec, Num(n-1), mkSEQ [mkLIT(","); r])])
        | Infinity ->
            mkSEQ [ r; mkSTAR(m_dec, n, mkSEQ [ mkLIT(","); r ]) ]
        ) in
      if m>0 then r_s else mkOPT(r_s)
  | (l,r) -> failwith "Internal error: TODO in hash2star"

let rec elim_rule r = match r.r with
  | Hash(l,r2) ->
      elim_rule r2;
      r.r <- (hash2star(l,r2)).r
  | Minus(r2,r3)
  | Alt(r2,r3)
  | Seq(r2,_,_,r3) ->
      elim_rule r2;
      elim_rule r3
  | Lookahead (_,r2)
  | Opt(r2)
  | Assign(r2,_,_)
  | Rcount(_,r2)
  | Star(_,r2) ->
      elim_rule r2
  | Symb _ | CharRange _ | Lit _
  | Box _ | Prose _ | When _ | Action _ | Delay _ | Position _ -> ()

let elim gr =
  List.iter
    (function
        RuleDef(_,r,_) -> elim_rule r
      | _ -> ())
    gr.ds
