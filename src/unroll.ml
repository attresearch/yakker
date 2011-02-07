(*******************************************************************************
 * Copyright (c) 2010 AT&T.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Trevor Jim and Yitzhak Mandelbaum and Qian Xi
 *******************************************************************************)

(* Unroll Kleene closure *)

(* Like hash elimination, this should really be integrated with lifting,
   in case the body of a Kleene closure is a producer, and hence, should
   produce a list.

   For now, we punt if the body is a producer.
*)

open Gul
open Attr

let transform gr =
  let limit = !Compileopt.unroll_star_n in
  if limit <= 0 then () else
  Analyze.producers gr;
  Analyze.relevance gr;

  let exp r m =
    let rec loop m =
      if m <= 0 then []
      else (copyRule r)::(loop(m-1)) in
    mkSEQ(loop m) in

  let rec loop r = (* transform r and return true iff r is a producer *)
    match r.r with
    | Star(Bounds(m,ub),r1) when m<=limit ->
        if loop r1 then true (* don't unroll producers *)
        else if m<0 then false else begin
          Yak.Logging.log Yak.Logging.Features.verbose "!%!";
          let r1_m_times = exp r1 m in
          r.r <-
            (match ub with
            | Infinity ->
                mkSEQ [r1_m_times; mkSTAR(0,Infinity,copyRule r1)]
            | Num n ->
                if m<n then
                  mkSEQ [r1_m_times; mkSTAR(0,Num(n-m),copyRule r1)]
                else
                  r1_m_times).r;
          false
        end

    | Action _
    | DBranch _
    | Delay _
    | Box(_,Some _,_)
    | Position _ ->
        true

    | Box(_,None,_)
    | Lit _
    | CharRange _
    | Prose _
    | When _ ->
        false

    | Symb(n,_,_,_) ->
        Yak.PSet.mem n gr.early_producers || Yak.PSet.mem n gr.late_producers

    | Lookahead(_,r1)
    | Assign(r1,_,_) ->
        ignore(loop r1);
        false

    | Star(_,r1)
    | Rcount(_,r1)
    | Hash(_,r1)
    | Opt r1 ->
        loop r1

    | Minus(r1,r2)
    | Alt(r1,r2) ->
        let a = loop r1 in
        let b = loop r2 in
        a || b

    | Seq(r1,_,_,r2) ->
        ignore(loop r1);
        loop r2 in
  List.iter
    (function RuleDef(n,r,a) -> ignore(loop r) | _ -> ())
    gr.ds
