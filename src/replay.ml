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

(* The variable [i_con] is used for the int constructor of the history-value data type. *)

(* Wrapping for histories.

   Histories are a polymorphic type, class ['a] history, where in the
   simplest case, 'a is int, the type of dispatch labels.  However, if
   delay is used, 'a needs to include the delayed type.  History
   wrapping finds all of the delayed types and constructs a datatype
   if necessary, and inserts projections and injections as needed.
 *)
let wrap_history gr =
  (* Get all delay types *)
  let add_types = lrfold_b
    (fun r v_left -> match r.r with | Delay(_,_,Some t) -> PSet.add t v_left | _ -> v_left) in
  let types = List.fold_left
    (fun types -> function | RuleDef(n,r,a) -> add_types r types | _ -> types)
    (PSet.add "int" PSet.empty)
    gr.ds in

  (* Set the constructor used for integers. *)
  let i_con = if 1 = PSet.cardinal types then "" else "Ykd_int" in

  (* If we only use int, then there is no need to wrap.  Otherwise,
     each type gets a corresponding datatype constructor. In the
     latter case, we also modify all [Delay]'s in the grammar to
     include the appropriate injections and projections. *)
  let hv_type =
    if 1 = PSet.cardinal types then
      "type hv = int"
    else
      (* Create the history-value datatype. *)
      let b = Buffer.create 11 in                     (* Print out the type declaration *)
      Printf.bprintf b "type hv =\n";
      Printf.bprintf b "| %s of int" i_con;         (* Hard-code this for use by labels *)
      let tbl_type_constructor = Hashtbl.create 11 in (* Map types to their constructors *)
      Hashtbl.add tbl_type_constructor "int" i_con;
      PSet.iter
        (function
           | "int" -> ()
           | t ->
               let x = "Ykd"^(fresh()) in
               Printf.bprintf b "\n| %s of (%s)" x t;    (* NB parens force a reference if t is a tuple type *)
               Hashtbl.add tbl_type_constructor t x)
        types;

      (* Wrap and unwrap at delay.
         Note: at this point, [i_con] and all entries in
         [tbl_type_constructor] will map types to a constructor with one
         argument. We rely on this invariant in the code for Delay,
         below. *)
      let find tbl x =
        try
          Hashtbl.find tbl x
        with Not_found ->
          (Printf.eprintf "Internal wrap error: could not find %S\n%!" x; raise Not_found) in
      let rec loop r =
        match r.r with
          | Delay(opn,e,topt) ->
              let wrapped,unwrapped = fresh(),fresh() in
              let constructor =
                (match topt with None -> i_con
                   | Some x -> find tbl_type_constructor x) in
              let wrap_act = Printf.sprintf "%s(%s)" constructor e in
              let unwrap_act =
                Printf.sprintf "(match %s with %s(%s) -> %s | _ -> failwith \"@delay wrap\")"
                  wrapped constructor unwrapped unwrapped in
              r.r <-
                (mkSEQ2(mkRHS(Delay(opn,wrap_act,None)),None,Some wrapped,mkACTION2(None,Some unwrap_act))).r

          | Alt(r1,r2) | Seq(r1,_,_,r2) | Minus(r1,r2) ->
              loop r1; loop r2

          | Assign(r1,_,_) | Opt r1 | Lookahead (_,r1) | Rcount(_,r1) | Star(_,r1) | Hash(_,r1) ->
              loop r1

          | Symb _ | Position _ | Lit(_,_) | CharRange(_,_) | Prose _
          | Action _ | When _ | DBranch _ | Box _ ->
              ()
      in
      List.iter
        (function RuleDef(n,r,a) -> loop r | _ -> ())
        gr.ds;
      Buffer.contents b in
  i_con, hv_type

(** Generate the replay functions and add labels to rhs as needed.
    Side-effects the AST and the grammar's prologue.*)
let replay gr i_con =
  let l = ref 2000 in
  let uses_history = ref false in
  let fresh() = uses_history := true; Util.postincr l in

  let b = Buffer.create 11 in
  let pr fmt = Printf.bprintf b fmt in
  let fname n = Printf.sprintf "_r_%s" (Variables.bnf2ocaml n) in

  let rec loop r =
    if not(r.a.late_relevant) then pr "()" else
    match r.r with
    | Action (early,Some e) ->
        pr "%s" e
    | Symb(n,x,y,Some e) ->
        pr "%s(_n,_p,ykinput,%s)" (fname n) e
    | Symb(n,_,_,None) ->
        pr "%s(_n,_p,ykinput)" (fname n)
    | Delay _ ->
        uses_history := true; pr "_n()"
    | Position false ->
        let l = fresh() in
        r.a.pre <- l;
        pr "_p()"
    | Opt r1 ->
        loop r1
    | Alt _ ->
        let alts = alts_of_rhs r in
        pr "(match _n() with";
        List.iter
          (fun r1 ->
             let l = fresh() in
             r1.a.pre <- l;
             pr "\n | %s(%d) -> (" i_con l;
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
        pr "(match _n() with %s(%d) -> %s | _ (*%d*) ->\n %s(" i_con l_done x l_body g;
        loop r1;
        pr "))\nin %s(%s))" g e
    | Star(_,r1) ->
        let l_body = fresh() in
        let l_done = fresh() in
        r1.a.pre <- l_body;
        r.a.post <- l_done;
        pr "(while (match _n() with %s(%d) -> true | _ (*%d*) -> false) do\n" i_con l_body l_done;
        loop r1;
        pr "done)\n"

          (* cases below are not late relevant *)
    | When _            -> Util.impossible "Replay.replay.When"
    | Box _             -> Util.impossible "Replay.replay.Box"
    | DBranch _         -> Util.impossible "Replay.replay.DBranch"
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
        pr "%s %s(_n,_p,ykinput%s) = "
          (if !first then "\nlet rec\n" else "\nand\n")
          (fname n)
          (match a.Attr.late_params with None -> "" | Some x -> ","^x);
        first := false;
        loop r
      end
      | _ -> ())
    gr.ds;
  pr "\n";
  Buffer.contents b, !uses_history

(* Generate the reversing functions *)
let reverse gr i_con =
  let b = Buffer.create 11 in
  let pr fmt = Printf.bprintf b fmt in
  let fname n = Printf.sprintf "_rv_%s" (Variables.bnf2ocaml n) in

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
        pr "push_pos(_p())"
    | Opt r1 ->
        loop r1
    | Alt _ ->
        let alts = alts_of_rhs r in
        pr "(match _n() with";
        List.iter
          (fun r1 ->
            let l = r1.a.pre in
            pr "\n | %s(%d) -> (" i_con l;
            loop r1;
            pr "; push(%s(%d)))" i_con l)
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
        pr "push(%s(%d)); " i_con l_done;
        pr "while (match _n() with %s(%d) -> true | _ (*%d*)-> false) do\n " i_con l_body l_done;
        loop r1;
        pr "; push(%s(%d))\n" i_con l_body;
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
  pr "class ['a] rvs (labels: 'a History.enum) =\n";
  pr "let s = ref [] in\n";
  pr "let push x = s := x::!s in\n";
  pr "let push_pos p = s := (%s p)::!s in\n" i_con;
  pr "let _n() = (let (_,x,_) = labels#next() in x) in\n";
  pr "let _p() = (let (_,_,p) = labels#next() in p) in\n";
  ignore (List.fold_left begin fun first ->
    (function RuleDef(n,r,a) ->
       if not r.a.late_relevant then first else
         begin
           if first then pr "let rec %s() = " (fname n) else pr "and %s() = " (fname n);
           loop r;
           pr "\n";
           false
         end
       | _ -> first)
  end
    true gr.ds);
  pr "in\n";
  pr "object (self)\n";
  pr "method next() = (match !s with hd::tl -> (s := tl; hd) | _ -> raise Not_found)\n";
  pr "initializer %s()\n" (fname gr.start_symbol);
  pr "end\n";
  Buffer.contents b

let gen_late_prologue i_con hv_ty_decl replay_funs_code replay_entry_point_code =
  let hist_cons =
    if !Compileopt.unit_history then
      "let _e p h = h
let _p lbl hv p h = h
let _m lbl p h h1 = h
"
    else
      (* merge doesn't carry a value -- all that matters is the
         label. However, to fit the costraint of the type (a triple), we
         have to put something in the second position, so we duplicate the
         label. *)
      (Printf.sprintf
         "let _e p h = h#empty p
let _p lbl hv p = (fun h->h#push p (lbl, hv, p))
let _m lbl p = (fun h1 h2-> h1#merge p (lbl, %s lbl, p) h2)\n" i_con) in
  Printf.sprintf "
(* History value type*)
%s

(* History constructors *)
%s

module Yk_Hashed = struct
  type t = int * hv * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
  let memoize = %B
end
module Yk_History = Yak.History.Make(Yk_Hashed)

(* Replay-related functions *)
%s
%s
"
    hv_ty_decl
    hist_cons
    !Compileopt.memoize_history
    replay_funs_code
    replay_entry_point_code

(** Transform a Gul grammar to explicitly push replay labels. Returns
    code to add to the prologue in support of the transformed grammar. *)
let transform gr =
  if not gr.grammar_late_relevant then "" else begin
    let i_con, hv_ty_decl = wrap_history gr in

    (* Generate replay-related functions.*)
  Analyze.producers gr;
  Analyze.relevance gr;
  let replay_funs_code, uses_history = replay gr i_con in
  let start_symbol = Variables.bnf2ocaml gr.start_symbol in
  let start_params =
    List.fold_left
      (fun p ->
        (function RuleDef(n,_,a) ->
          if n = gr.start_symbol then
            (match a.Attr.late_params with None -> ""
            | Some x -> "(" ^ x ^ ") ")
          else p
        | _ -> p))
      ""
      gr.ds in
  let replay_entry_point_code =
    let comma_start_params = if start_params = "" then "" else ","^start_params in
    (* Some grammars have late actions but never push anything on the
       history because the actions are "inevitable". So, we need to
       check [uses_history] before generating the code. *)
    if !Compileopt.postfix_history && uses_history then begin
      let rev_code = reverse gr i_con in
      (* define [getp] based on i_con to avoid warning over unnecessary match case. *)
      let getp = if i_con = "" then "_o#next()" else Printf.sprintf "match _o#next() with | %s(p) -> p | _ -> failwith \"wrong constructor for position.\"" i_con in
      if !Compileopt.repress_replay then
        Printf.sprintf
          "%s\nlet _replay_%s ykinput h %s= ignore(new rvs (h#right_to_left))\n"
          rev_code start_symbol start_params
      else
        Printf.sprintf
          "%s
let _replay_%s ykinput h %s=
  let _o = new rvs (h#right_to_left) in
  let _n() = _o#next() in
  let _p() = %s in
  _r_%s(_n,_p,ykinput%s)\n"
          rev_code
          start_symbol
          start_params
          getp
          start_symbol
          comma_start_params
    end
    else if !Compileopt.repress_replay then
      Printf.sprintf
        "\nlet _replay_%s ykinput h %s= ignore(h#left_to_right)\n"
        start_symbol start_params
    else
      Printf.sprintf
        "
let _replay_%s ykinput h %s=
  let _o = (h#left_to_right) in
  let _n() = (let (_,x,_) = _o#next() in x) in
  let _p() = (let (_,_,p) = _o#next() in p) in
  _r_%s(_n,_p,ykinput%s)\n"
        start_symbol
        start_params
        start_symbol
        comma_start_params in

  (* Generate history prologue boilerplate. The [i_con] constructor is
     used in the replay and reverse code, so the these declarations
     need to be appended before the functions. *)
  let prologue = gen_late_prologue i_con hv_ty_decl replay_funs_code replay_entry_point_code in

  (* Finally, transform the grammar *)
  let is_producer = Analyze.is_rhs_producer gr.early_producers gr.late_producers in
  let mkOutput l = Delay(false,Printf.sprintf "%s(%d)" i_con l,None) in
  let mkOUTPUT l = mkRHS (mkOutput l) in
  let mkBEFORE r l = mkSEQ[mkOUTPUT(l);r] in
  let mkAFTER r l =
    (* Output after r, while preserving producerness
       If [r] is not already producer, then we end new
       version of [r] with epsilon to avoid changing status of
       [r] to producer. *)
    let ep, lp = is_producer r in
    if ep || lp then
      let eo = if ep then Some (Variables.fresh()) else None in
      let lo = if lp then Some (Variables.fresh()) else None in
      mkSEQ2(r, eo, lo, mkSEQ[mkOUTPUT(l); mkACTION2(eo, lo)])
    else
      mkSEQ[r;mkOUTPUT(l);mkLIT ""] in
  let mkOUT =
    if !Compileopt.postfix_history
    then mkAFTER
    else mkBEFORE in
  let rec loop r =
    if not(r.a.late_relevant) then () else
    match r.r with
    | Action (early,Some e) ->
        () (*r.r <- (mkACTION2(early,None)).r*) (* Don't remove, so relevance stays the same *)
    | Symb(n,x,y,Some e) ->
        () (*r.r <- (mkSYMB2(n,x,y,None)).r*) (* Don't remove, so relevance stays the same *)
    | Symb(_,_,_,None) | Delay _ ->
        ()
    | Position false ->
        (* Desugar late position into a call to delay. Since delay is
           late-relevant, does not change relevance or producerness. *)
        r.r <- mkOutput r.a.pre
    | Opt r1 ->
        loop r1
    | Alt _ ->
        let alts = alts_of_rhs r in
        List.iter
          (fun r ->
             let l = r.a.pre in
             loop r;
             r.r <- (mkOUT(dupRhs r)(l)).r)
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
        r1.r <- (mkOUT (dupRhs r1) l_body).r;
        if !Compileopt.postfix_history
        then r.r <- (mkBEFORE (dupRhs r) l_done).r
        else r.r <- (mkAFTER (dupRhs r) l_done).r
            (* cases below are not late relevant *)
    | When _            -> Util.impossible "Replay.transform.When"
    | Box _             -> Util.impossible "Replay.transform.Box"
    | DBranch _         -> Util.impossible "Replay.transform.DBranch"
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
    gr.ds;
  prologue
  end
