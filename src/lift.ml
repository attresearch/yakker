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

(* The lifting transform *)

open Yak
open Gul
open Variables

let transform gr =
  (* identify nonterminal producers *)
  Analyze.producers gr;
  let is_early_producer n = PSet.mem n gr.early_producers in
  let is_late_producer n = PSet.mem n gr.late_producers in
  (* Helper function for Kleene-closure *)
  let pieces p =
    if not(p) then None,None,None,None,None else
    let x,y = fresh(),fresh() in
    (Some(x,"Yak.Util.nil")),
    (Some y),
    (Some (Printf.sprintf "%s::%s" y x)),
    (Some x),
    (Some (Printf.sprintf "(List.rev %s)" x)) in
  (* Helper function for Kleene-closure with exact repeat *)
  let pieces_repeat count p =
    (* p is whether the body of the closure is an early producer. *)
    let n = fresh() in
    if p then
      let ex = fresh() in
      (Some(n,Printf.sprintf "(%s,Yak.Util.nil)" count)),
      (Printf.sprintf "fst(%s)>0" n),
      (Some ex),
      (Some(Printf.sprintf "(fst(%s)-1,%s::snd(%s))" n ex n)),
      (Some n),
      (Printf.sprintf "fst(%s)=0" n),
      (Some(Printf.sprintf "(List.rev(snd(%s)))" n))
    else
      (Some(n,count)),
      (Printf.sprintf "%s>0" n),
      (None),
      (Some(Printf.sprintf "%s-1" n)),
      (Some n),
      (Printf.sprintf "%s=0" n),
      (None) in
  (* Helper function for Kleene-closure within bounds *)
  let pieces_between lb ub p =
    (* lb and ub are the inclusive bounds on the repeat count.
       p is whether the body of the closure is an early producer. *)
    let n = fresh() in
    if p then
      let ex = fresh() in
      (Some(n,Printf.sprintf "(0,Yak.Util.nil)")),
      (Printf.sprintf "fst(%s)<=%d" n ub),
      (Some ex),
      (Some(Printf.sprintf "(fst(%s)+1,%s::snd(%s))" n ex n)),
      (Some n),
      (Printf.sprintf "fst(%s)>=%d" n lb),
      (Some(Printf.sprintf "(List.rev(snd(%s)))" n))
    else
      (Some(n,"0")),
      (Printf.sprintf "%s<=%d" n ub),
      (None),
      (Some(Printf.sprintf "%s+1" n)),
      (Some n),
      (Printf.sprintf "%s>=%d" n lb),
      (None) in
  let pieces_above lb p =
    (* lb and Infinity are the inclusive bounds on the repeat count.
       p is whether the body of the closure is an early producer. *)
    let n = fresh() in
    if p then
      let ex = fresh() in
      (Some(n,Printf.sprintf "(0,Yak.Util.nil)")),
      (Some ex),
      (Some(Printf.sprintf "(fst(%s)+1,%s::snd(%s))" n ex n)),
      (Some n),
      (Printf.sprintf "fst(%s)>=%d" n lb),
      (Some(Printf.sprintf "(List.rev(snd(%s)))" n))
    else
      (Some(n,"0")),
      (None),
      (Some(Printf.sprintf "%s+1" n)),
      (Some n),
      (Printf.sprintf "%s>=%d" n lb),
      (None) in
  let rec loop r = begin
    match r.r with
    | Action(early,late) ->
        (early<>None,late<>None)
    | Position true -> (true,false)
    | Position false -> (false,true)
    | Symb(n,_,_,_) -> (* TODO: attributes *)
        (is_early_producer n,is_late_producer n)
    | Delay _ -> (false,true)
    | DBranch (_, {Gil.arity = 0}) -> (false, false)
    | DBranch (_, _) ->
        if !Compileopt.late_only_dbranch then
          (* TODO-dbranch: (false, true)  *)
          (false, false)
        else (true, false)
    | Box(_,Some _,_) ->
        (true,false)
    | Box(_,None,_) ->
        (false,false)
    | Minus(r1,r2) ->
        (false,false) (* TODO: should have been desugared *)
    | Assign _ | Lit _ | CharRange _ | Prose _ | When _ | Lookahead _ ->
        (false,false)
    | Star(Accumulate(_,_),r1) -> loop r1
    | Hash(Accumulate(_,_),r1) -> loop r1
    | Seq(r1,early,late,r2) ->
        let (e1,l1) = loop r1 in
        (* calculate extent for r1 if needed *)
        let need_early_extent = not e1 && early <> None in
        let need_late_extent = not l1 && late <> None in
        let mkseq = List.fold_right (fun (r,e,l) r_s -> mkSEQ2(r,e,l,r_s)) in
        if not need_early_extent && need_late_extent then begin
          let before_l,after_l = fresh(),fresh() in
          let extent_l = Printf.sprintf "Yak.YkBuf.get_string %s %s ykinput" before_l after_l in
          let r1_seq =
            [ mkPOSITION false,None,Some before_l;
              dupRhs r1,early,None;
              mkPOSITION false,None,Some after_l;
              mkACTION2(None, Some extent_l), None, late] in
          r.r <- (mkseq r1_seq r2).r
        end
        else if need_early_extent then begin
          let extent before after = Printf.sprintf "Yak.Pami.get_substring %s %s" before after in
          let extent_l before after = Printf.sprintf "Yak.YkBuf.get_string %s %s ykinput" before after in
          let before,after = fresh(),fresh() in
          let r1_seq =
            if need_late_extent then
              let before_l,after_l = fresh(),fresh() in
              [ mkPOSITION true,  Some before, None;
                mkPOSITION false, None, Some before_l;
                dupRhs r1, None,  None;
                mkPOSITION true,  Some after, None;
                mkPOSITION false, None, Some after_l;
                mkACTION2(Some(extent before after), Some(extent_l before_l after_l)), early, late ]
            else
              [ mkPOSITION true,Some before,None;
                dupRhs r1, None, late;
                mkPOSITION true, Some after, None;
                (* added ty_annot to improve type inference. TODO: add similarly to other cases. *)
                ty_annot (mkACTION2(Some(extent before after),None)) "string", early, None] in
          r.r <- (mkseq r1_seq r2).r
        end;
        loop r2

    | Opt _
    | Alt _ ->
        let alts_of_rhs = (* differs from gul.ml b/c need to desugar Opt *)
          let rec loop l r = match r.r with
          | Alt(r1,r2) -> loop (loop l r2) r1
          | Opt(r1) ->
              r.r <- (mkALT[r1;mkLIT ""]).r; loop l r
          | _ -> r::l in
          loop [] in
        let alts = alts_of_rhs r in
        let lift_p = List.map (fun r -> loop r) alts in
        let early_all_true  = List.for_all (fun (x,_) -> x)     lift_p in
        let early_all_false = List.for_all (fun (x,_) -> not x) lift_p in
        let late_all_true   = List.for_all (fun (_,x) -> x)     lift_p in
        let late_all_false  = List.for_all (fun (_,x) -> not x) lift_p in
        (match early_all_true,early_all_false,late_all_true,late_all_false with
          true,_,true,_ -> (true,true)
        | true,_,_,true -> (true,false)
        | _,true,true,_ -> (false,true)
        | _,true,_,true -> (false,false)
        | _ ->
            let is_early_producer = early_all_true || not(early_all_false) in
            let is_late_producer = late_all_true || not(late_all_false) in
            let lift_early = not(early_all_true || early_all_false) in
            let lift_late = not(late_all_true || late_all_false) in
            let bind_act lift =
              let x = fresh() in
              let some_x = Printf.sprintf "Some(%s)" x in
              fun p ->
                (match p,lift with
                | true,true -> Some x,Some some_x
                | false,true -> None,Some("None")
                | true,false -> Some x,Some x
                | false,false -> None,None) in
            let early_bind_act = bind_act lift_early in
            let late_bind_act =  bind_act lift_late in
            let lifted_alts =
              List.map2
                (fun r1 ->
                  function (early_p,late_p) ->
                    let (e_bind,e_act) = early_bind_act early_p in
                    let (l_bind,l_act) = late_bind_act late_p in
                    mkSEQ2(r1,e_bind,l_bind,mkACTION2(e_act,l_act)))
                alts lift_p in
            r.r <- (mkALT lifted_alts).r;
            (is_early_producer,is_late_producer))
    | Rcount(e,r1) ->
        (* If r1 not an early producer:

           @repeat(e)(r1) --> *@{n=e}(@when(n>0) r1 @{n-1})@n1 @when(n1=0)
           where n,n1 are fresh.
           It might be nice if we did not have to bind n1, what if n were in scope after the *?

           If r1 is an early producer:

           @repeat(e)(r1) --> *@{n=(e,[])}(@when(fst(n)>0) r1@x @{(fst(n)-1,x::snd(n))})@n1 @when(fst(n1)=0) @{snd(n1)}
           where n,n1,x are fresh.

           If r1 is a late producer, need to add equivalent late checks.
           @repeat(e)(r1) --> *@{n=e}${l=[]}(@when(n>0) r1$lx @{n-1}${lx::l})@n1$l @when(n1=0) ${List.rev l}
           or
           @repeat(e)(r1) --> *@{n=(e,[])}${l=[]}(@when(fst(n)>0) r1@x$lx @{(fst(n)-1,x::snd(n))}${lx::l})@n1$l @when(fst(n1)=0) @{snd(n1)}${List.rev l}

         *)
        let early_producer,late_producer = loop r1 in
        let (l_init,        l_elt,l_cons,l_all,        l_rev) = pieces late_producer in
        let (e_init,e_when1,e_elt,e_cons,e_all,e_when2,e_rev) = pieces_repeat e early_producer in
        if early_producer || late_producer then
          r.r <-
            (mkSEQ2(mkSTAR2(Accumulate(e_init,l_init),
                            mkSEQ2(mkWHEN(e_when1),None,None,
                                   mkSEQ2(r1,e_elt,l_elt,mkACTION2(e_cons,l_cons)))),
                    e_all,l_all,
                    mkSEQ2(mkWHEN(e_when2),None,None,
                           mkACTION2(e_rev,l_rev)))).r
        else
          r.r <-
            (mkSEQ2(mkSTAR2(Accumulate(e_init,None),
                            mkSEQ [mkWHEN e_when1;
                                   r1;
                                   mkACTION2 (e_cons, None)]),
                    e_all, None,
                    mkWHEN e_when2)).r;
        (early_producer,late_producer)
    | Star(Bounds(m,Num n),r1) ->
        let early_producer,late_producer = loop r1 in
        let (l_init,        l_elt,l_cons,l_all,        l_rev) = pieces late_producer in
        let (e_init,e_when1,e_elt,e_cons,e_all,e_when2,e_rev) = pieces_between m n early_producer in (*<--differs from Rcount here*)
        r.r <-
          (mkSEQ2(mkSTAR2(Accumulate(e_init,l_init),
                          mkSEQ2(mkWHEN(e_when1),None,None,
                                 mkSEQ2(r1,e_elt,l_elt,mkACTION2(e_cons,l_cons)))),
                  e_all,l_all,
                  mkSEQ2(mkWHEN(e_when2),None,None,
                         mkACTION2(e_rev,l_rev)))).r;
        (early_producer,late_producer)
    | Star(Bounds(0,Infinity),r1) ->
        let (early,late) = loop r1 in
        if not(early) && not(late) then (false,false) else
        let e_init,e_elt,e_cons,e_all,e_rev = pieces early in
        let l_init,l_elt,l_cons,l_all,l_rev = pieces late in
        r.r <-
          (mkSEQ2(mkSTAR2(Accumulate(e_init,l_init),
                          mkSEQ2(r1,e_elt,l_elt,mkACTION2(e_cons,l_cons))),
                  e_all,l_all,
                  mkACTION2(e_rev,l_rev))).r;
        (early,late)
    | Star(Bounds(m,Infinity),r1) ->
        let early_producer,late_producer = loop r1 in
        let (l_init,        l_elt,l_cons,l_all,        l_rev) = pieces late_producer in
        let (e_init,        e_elt,e_cons,e_all,e_when2,e_rev) = pieces_above m early_producer in
        r.r <-
          (mkSEQ2(mkSTAR2(Accumulate(e_init,l_init),
                          mkSEQ2(r1,e_elt,l_elt,mkACTION2(e_cons,l_cons))),
                  e_all,l_all,
                  mkSEQ2(mkWHEN(e_when2),None,None,
                         mkACTION2(e_rev,l_rev)))).r;
        (early_producer,late_producer)
    (* The Hash cases are just like the Star equivalents --- ought to refactor.
       Better yet: desugar the Hash. *)
    | Hash(Bounds(m,Num n),r1) ->
        let early_producer,late_producer = loop r1 in
        let (l_init,        l_elt,l_cons,l_all,        l_rev) = pieces late_producer in
        let (e_init,e_when1,e_elt,e_cons,e_all,e_when2,e_rev) = pieces_between m n early_producer in
        r.r <-
          (mkSEQ2(mkHASH2(Accumulate(e_init,l_init), (*<--only difference from Star is here*)
                          mkSEQ2(mkWHEN(e_when1),None,None,
                                 mkSEQ2(r1,e_elt,l_elt,mkACTION2(e_cons,l_cons)))),
                  e_all,l_all,
                  mkSEQ2(mkWHEN(e_when2),None,None,
                         mkACTION2(e_rev,l_rev)))).r;
        (early_producer,late_producer)
    | Hash(Bounds(0,Infinity),r1) ->
        let (early,late) = loop r1 in
        if not(early) && not(late) then (false,false) else
        let e_init,e_elt,e_cons,e_all,e_rev = pieces early in
        let l_init,l_elt,l_cons,l_all,l_rev = pieces late in
        r.r <-
          (mkSEQ2(mkHASH2(Accumulate(e_init,l_init), (*<--only difference from Star is here*)
                          mkSEQ2(r1,e_elt,l_elt,mkACTION2(e_cons,l_cons))),
                  e_all,l_all,
                  mkACTION2(e_rev,l_rev))).r;
        (early,late)
    | Hash(Bounds(m,Infinity),r1) ->
        let early_producer,late_producer = loop r1 in
        let (l_init,        l_elt,l_cons,l_all,        l_rev) = pieces late_producer in
        let (e_init,        e_elt,e_cons,e_all,e_when2,e_rev) = pieces_above m early_producer in
        r.r <-
          (mkSEQ2(mkHASH2(Accumulate(e_init,l_init), (*<--only difference from Star is here*)
                          mkSEQ2(r1,e_elt,l_elt,mkACTION2(e_cons,l_cons))),
                  e_all,l_all,
                  mkSEQ2(mkWHEN(e_when2),None,None,
                         mkACTION2(e_rev,l_rev)))).r;
        (early_producer,late_producer)
  end in
  List.iter
    (function RuleDef(n,r,a) -> ignore(loop r) | _ -> ())
    gr.ds;
  ()




(** This function adds dummy late actions to a grammar, but in PADS style rather the standard lifting interpretation.

    This new feature inserts late actions that attempt to simulate
    building a parse tree. The actions themselves are empty; what is
    significant is the choice of their location. I've attempted to
    place the actions realistically to simulate the impact of
    parse-tree building actions on the resulting automaton
    (particularly in terms of lost sharing).

    The command is called "pads-lift" because its semantics is closer
    to the way that PADS automatically builds an AST than Yakker's
    standard lifting. In particular, a sequence of elements is
    followed by an action insertion even if the last element is not
    late relevant. This corresponds to building a tuple of the
    (relevant) elements of the sequence, rather than corresponding to
    returning the last element, as standard lifting does.

    N.B.: Hash and Rcount not supported.
    N.B.: Does not preserve producerness. Best used on grammars with no early producers (attributes are fine).

*)
let pads_transform gr =
  let lift_lexical = function
    | RuleDef(n,r, ({Attr.classification = Lexical} as a)) -> (*  *)
        let r' = mkSEQ[r; mkACTION2(None, Some "()")] in
        RuleDef(n,r',a)
    | d -> d in
  gr.ds <- List.map lift_lexical gr.ds;

  (* Identify relevant nonterminals. We care about relevance rather
     than producerness, because as long as a rhs has an action
     anywhere in a sequence pads-lifting will add a late action. *)
  Analyze.producers gr;
  Analyze.relevance gr;
  let rels = late_relevance_set_of_grammar gr in

  let rec loop r = begin
    match r.r with
    | Action(_,Some _) -> true
    | Action(_,None) -> false
    | Position true -> false
    | Position false -> true
    | Symb(n,_,_,_) -> PSet.mem n rels
    | Delay _ -> true
    | DBranch (_, {Gil.arity = 0}) -> false
    | DBranch (_, _) -> false
    | Box _-> false
    | Minus(r1,r2) -> false (* TODO: should have been desugared *)
    | Assign _ | Lit _ | CharRange _ | Prose _ | When _ | Lookahead _ ->
        false
    | Star(Accumulate _, r1) -> loop r1
    | Star(Bounds _, r1) ->
        (* First, check body relevance. If it is relevant, then we
           know the star will be converted to a list by normal
           lifting, so follow the star by a unit-action to
           ensure that the rhs has the right type (unit). *)
        let rel = loop r1 in
        if rel then
          let r_action = mkACTION2(None, Some "()") in
          r.r <- (mkSEQ [dupRhs r; r_action]).r;
          true
        else false

    | Seq _ ->
        (** Convert binary Seq representation with no late binders to list of rhs *)
        let rs_of_rhs =
          let rec loop l r = match r.r with
            | Seq(r2,e_opt,None,r3) ->  (r2, e_opt) :: loop l r3
            | _ -> (r, None) :: l in
          loop [] in
        let rs = rs_of_rhs r in
        let rs = List.map (fun (r,e) -> if loop r then (r, e, Some (fresh())) else (r, e, None)) rs in
        let rest_rel, last_rel  =
          let r,rs = match List.rev rs with x::xs -> x,xs | _ -> Util.impossible "Sequence converted to empty list" in
          let lrel = match r with (_, _, Some _) -> true | _ -> false in
          let rrel = List.exists (function (_,_,Some _) -> true | _ -> false) rs in
          rrel, lrel in
        begin match rest_rel, last_rel with
          | false, false -> false
          | false, true  -> true    (* only last is relevant, so do nothing more *)
          | true, _ ->
              (* The first r_action is the real code; but, for now, we just need a mockup. *)
              (* let xs = List.fold_right (fun (_,_,l_opt) xs -> match l_opt with None -> xs | Some x -> x::xs) rs [] in *)
              (* let r_action = mkACTION2(None, Some ("(" ^ String.concat "," xs ^ ")")) in *)
              let r_action = mkACTION2(None, Some "()") in
              let mkseq = List.fold_right (fun (r,e,l) r_s -> mkSEQ2(r,e,l,r_s)) in
              let r_new = mkseq rs r_action in
              r.r <- r_new.r;
              true
        end
    | Opt _
    | Alt _ ->
        let alts_of_rhs = (* differs from gul.ml b/c need to desugar Opt *)
          let rec loop l r = match r.r with
          | Alt(r1,r2) -> loop (loop l r2) r1
          | Opt(r1) ->
              r.r <- (mkALT[r1;mkLIT ""]).r; loop l r
          | _ -> r::l in
          loop [] in
        let alts = alts_of_rhs r in
        let lift_p = List.map loop alts in
        let late_all_true   = List.for_all (fun x -> x) lift_p in
        let late_all_false  = List.for_all not lift_p in
        if late_all_true then true
        else if late_all_false then false
        else begin
          let x = fresh() in
          (* The first bind_act is the real code; but, for now, we just need a mockup. *)
(*           let some_x = Printf.sprintf "Some(%s)" x in *)
(*           let bind_act = function *)
(*             | true -> Some x, Some some_x *)
(*             | false -> None, Some("None") in *)
          let bind_act = function
            | true -> Some x, Some "()"
            | false -> None, Some "()" in
          let lifted_alts =
            List.map2
              (fun r1 late_p ->
                 let (l_bind,l_act) = bind_act late_p in
                 mkSEQ2(r1,None,l_bind,mkACTION2(None,l_act)))
              alts lift_p in
          r.r <- (mkALT lifted_alts).r;
          true
        end
    | Hash _ | Rcount _ -> assert(false)
  end in
  List.iter
    (function RuleDef(n,r,a) -> ignore(loop r) | _ -> ())
    gr.ds

