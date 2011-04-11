(*******************************************************************************
 * Copyright (c) 2010 AT&T.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Qian Xi
 *    Trevor Jim and Yitzhak Mandelbaum
 *******************************************************************************)

open Yak
open Gul
let add_lr1_lookahead gr =
  let first_map = Analyze.First_set_lex.first_gr gr gr.tokmap in
  let first r = Analyze.First_set_lex.first r first_map in
  let sz = List.length gr.ds in
  let nametbl = Hashtbl.create sz in
  let ftbl = Hashtbl.create sz in
  let apptbl = Hashtbl.create 101 in
  let toktbl = ref [] in
  let dummy = mkLIT("") in
  let rec findfree n x tbl =
    let n_try = n ^ string_of_int x in
    if Hashtbl.mem tbl n_try then
      findfree n (x+1) tbl
    else x, n_try in
  let mk_name n =
    try
      let x = Hashtbl.find nametbl n in
      let x',n' = findfree n x ftbl in
      Hashtbl.replace nametbl n (x' + 1);
      n'
    with Not_found ->
      Hashtbl.add nametbl n 1;
      n in

  let add_tok toktbl d = toktbl := d :: !toktbl in

  let la_app (n:nonterminal) fs =
    try
      let n_fs,_,_ = Hashtbl.find apptbl (n,fs) in
      n_fs
    with Not_found ->
      let n_fs = mk_name n in
      let f_n, a = Hashtbl.find ftbl n in
      Hashtbl.add apptbl (n, fs) (n_fs, dummy, a);
      let r_fs = f_n fs in
      Hashtbl.replace apptbl (n, fs) (n_fs, r_fs, a);
      n_fs in

  let rec tx r = match r.r with
  | Symb (n1, _, _,_) ->
      (match Analyze.search_tokmap gr.tokmap n1 with
         | None -> (fun follow -> mkSYMB (la_app n1 follow, None, None)) (* TODO: attributes *)
         | Some _ -> (fun _ -> r))
  | Lit _ | CharRange _ -> (fun follow -> r)
  | Seq(r1, None, None, r2) ->
      let r1' = tx r1 in
      let r2' = tx r2 in
      let f_r2 = first r2 in
      (fun follow ->
        mkSEQ2 (r1' (Analyze.First_set_lex.concat f_r2 follow None None),
                None, None,
                r2' follow))
  | Alt(r1,r2) ->
      let r1' = tx r1 in
      let r2' = tx r2 in
      (fun follow -> mkALT2 (r1' follow, r2' follow))
  | Star (Bounds(0,Infinity) as b, r1) ->
      let r1' = tx r1 in
      (fun follow -> mkSTAR2 (b, r1' (Analyze.First_set_lex.union (first r1) follow)))
  | Opt(r1)  ->
      let r1' = tx r1 in
      (fun follow -> mkOPT (r1' follow))
  | Minus _ -> Util.warn Util.Sys_warn "minus not processed by lookahead"; (fun follow -> r)
  | Assign _
  | Star _
  | Seq _
  | Lookahead _
  | Box _
  | Action _
  | Position _
  | Rcount _
  | Hash _
  | When _ | DBranch _
  | Delay _ | Prose _ ->
      Util.warn Util.Sys_warn "rule not handled by lookahead transformation";
      (fun _ -> mkLIT("!ERROR!")) in
  let tx_def = function
    | RuleDef(n, r, a) as d ->
        (match Analyze.search_tokmap gr.tokmap n with
          | None ->
              let r' = tx r in
              let f = (fun la -> mkSEQ2(r' la, None, None, mkLOOKAHEAD(true, Analyze.First_set_lex.to_rule la))) in
              Hashtbl.add ftbl n (f, a)
          | Some _ -> add_tok toktbl d)
    | d -> () in
  (* load [ftbl] *)
  List.iter tx_def gr.ds;
  (* load [apptbl] *)
  (* NOTE: Not clear, as yet, how to set the starting lookahead token
     in the general case. For grammars with tokenizers, the EOF token
     is probably the correct choice; for scannerless grammars, 256 is
     probably the right choice. Ultimately, we should analyze the
     grammar to figure this out, or somesuch. For now, we just pick
     one and warn. *)
  (*   let ssymb = la_app gr.start_symbol (Analyze.First_set_lex.non_singleton (256, 256)) in *)
  Util.warn Util.Sys_warn "Choosing token EOF as start symbol lookahead.";
  let ssymb = la_app gr.start_symbol (Analyze.First_set_lex.non_singleton_tok "" "EOF") in
  (* convert [apptbl] to new grammar. *)
  let retrieve_defs _ (n,r,a) ds = (RuleDef (n,r,a))::ds in
  let ds' = Hashtbl.fold retrieve_defs apptbl !toktbl in
  {gr with ds = ds'; start_symbol = ssymb;}
