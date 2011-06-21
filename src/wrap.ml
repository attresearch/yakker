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

(* Wrap and unwrap semantic values across early calls.
 *
 * Early arguments and return values need a uniform type because they
 * end up in homogeneous data structures (like a semantic value stack).
 *)

open Yak
open Gul
open Attr
open Variables

let sv_type = "_yk_t"
let sv_unit = "Yk"^(fresh())

let find tbl x =
  try
    Hashtbl.find tbl x
  with Not_found ->
    (Printf.eprintf "Internal wrap error: could not find %S\n%!" x; raise Not_found)

let combined_type default attributes =
  let type_of_attributes =
    String.concat " * "
      (List.map snd (List.sort (fun (x,_) (y,_) -> compare x y) attributes)) in
  match default,attributes with
  |   None,[] -> None
  | Some x,[] -> Some x
  |   None,_  -> Some type_of_attributes
  | Some x,_  -> Some (x^" * "^type_of_attributes)

let output_type a =
  combined_type
    a.early_rettype
    a.output_attributes
let input_type a =
  combined_type
    (match a.early_params with None -> None | Some x -> Some(get_paramtype x))
    a.input_attributes

let wrap gr =
  (* Get all parameter and argument types *)
  let types = ref PSet.empty in
  let add_types = lrfold_b
    (fun r v_left -> match r.r with | DBranch (_,{cty=t}) -> PSet.add t v_left | _ -> v_left) in
  List.iter
    (function
        RuleDef(n,r,a) ->
          (match a.early_rettype with None -> () | Some x   -> types := PSet.add x !types);
          (match a.early_params with None -> () | Some x -> types := PSet.add (get_paramtype x) !types);
          if not !Compileopt.late_only_dbranch then types := add_types r !types
       | _ -> ())
    gr.ds;
  (* Each type gets a corresponding datatype constructor *)
  let b = Buffer.create 11 in                     (* Print out the type declaration *)
  Printf.bprintf b "type %s =\n" sv_type;
  Printf.bprintf b "| %s\n" sv_unit;
  let tbl_type_constructor = Hashtbl.create 11 in (* Map types to their constructors *)
  let add_type_constructor t =
    let x = "Yk"^(fresh()) in
    Printf.bprintf b "| %s of (%s)\n" x t; (* The parens are necessary if t is a tuple type *)
    Hashtbl.add tbl_type_constructor t x in
  PSet.iter add_type_constructor !types;
  Printf.bprintf b ";;\n";
  Printf.bprintf b "let sv0 = %s;;\n" sv_unit;
  (* For the new version *)
  Printf.bprintf b "type _wv = %s;;\n" sv_type; (* wv for 'wrapped value' *)
  Printf.bprintf b "let _wv0 = %s;;\n" sv_unit;
  (* Each nonterminal then gets a datatype constructor for its argument and its result *)
  let tbl_nt_inject = Hashtbl.create 11 in  (* injections into internal semantic value type sv_type *)
  let tbl_nt_project = Hashtbl.create 11 in (* projections out of sv_type back to user type *)
  let has_argument n = Hashtbl.mem tbl_nt_inject n in
  List.iter
    (function
        RuleDef(n,_,a) ->
          (match a.early_rettype with
          | None ->
              Hashtbl.add tbl_nt_project n None
          | Some x ->
              Hashtbl.add tbl_nt_project n (Some(find tbl_type_constructor x)));
          (match a.early_params with
          | None ->
              ()
          | Some x ->
              Hashtbl.add tbl_nt_inject n (find tbl_type_constructor (get_paramtype x)))
      | _ -> ())
    gr.ds;
  (* At every call wrap the arguments and unwrap the results.  Replace
     det. branch type with corresponding constructor, which is used
     here to inject the expression into the [sv] type, and later to
     project the result. *)
  let rec loop r = match r.r with
    | Symb(n,eopt,attrs,lopt) ->
        if attrs<>[] then Printf.eprintf "Warning: %s with attributes in Wrap\n%!" n;
        let args =
          match eopt with None ->
            begin
              if has_argument n then Printf.eprintf "Error: %s requires an argument but is called without one\n%!" n;
              None
            end
            | Some e ->
                let inject = find tbl_nt_inject n in
                Some(Printf.sprintf "%s(%s)" inject e) in
        r.r <- Symb(n,args,[],lopt); (* TODO: attributes *)
      let ntp_opt = try find tbl_nt_project n with Not_found -> None in
      (match ntp_opt with
      | None -> ()
      | Some project ->
          let x = fresh() in
          let act =
            Printf.sprintf "(match %s with %s(y) -> y | _ -> failwith \"projection\")"
              x project in
          if PSet.mem n gr.late_producers then
            let latev = fresh() in
            r.r <- (mkSEQ2(dupRhs r,Some x,Some latev,mkACTION2(Some act,Some latev))).r;
          else
            r.r <- (mkSEQ2(dupRhs r,Some x,None,mkACTION act)).r)

  | DBranch (e, c) ->
      if !Compileopt.late_only_dbranch then ()
      else
        let constructor = find tbl_type_constructor c.cty in
        let e_inj = Printf.sprintf "%s(%s)" constructor e in
        r.r <-
          (mkDBRANCH (e_inj, {c with cty=constructor})).r

  | Position _ | Lit(_,_) | CharRange(_,_) | Prose _
  | Action _ | When _ | Box _ | Delay _ -> ()
  | Seq(r2,_,_,r3) | Alt(r2,r3) | Minus(r2,r3) -> loop r2; loop r3
  | Assign(r2,_,_) | Lookahead (_,r2) | Opt r2 | Rcount(_,r2) | Star(_,r2) | Hash(_,r2) -> loop r2
  in
  List.iter
    (function RuleDef(n,r,a) -> loop r | _ -> ())
    gr.ds;
  (* At every nonterminal unwrap the arguments and wrap the result *)
  List.iter
    (function
       | RuleDef(n,r,a) ->
           (match a.early_params with
              | None ->
                  (* If no param then body ignores whatever comes in via engine; but engine should
                     be sure to pass in sv0 instead of using id at the call, else there will be
                     less sharing *)
                  ()
              | Some p ->
                  let x = fresh () in
                  let param = get_param p in
                  a.early_params <- Some(Printf.sprintf "%s:%s" x sv_type);
                  let inject = find tbl_nt_inject n in
                  let act =
                    Printf.sprintf "(match %s with %s(y) -> y | _ -> failwith \"projection\")"
                      x inject in
                  r.r <- (mkSEQ2(mkACTION act,Some param,None,dupRhs r)).r);
           let ntp_opt = try find tbl_nt_project n with Not_found -> None in
           (match ntp_opt with
              | None ->
                  (* If r has early actions but no rettype, then the early actions may cause an unwrapped value
                     to be returned... *)
                  (match r.a.early_relevant, PSet.mem n gr.late_producers with
                    true,false ->
                      (* By appending "" we effectively return a wrapped unit value *)
                      r.r <- (mkSEQ2(dupRhs r,None,None,mkLIT "")).r
                  | true,true ->
                      (* Here we must instead clobber the unwrapped early value while preserving the late value *)
                      let latev = fresh() in
                      r.r <- (mkSEQ2(dupRhs r,None,Some latev,mkACTION2(None,Some latev))).r
                  | _ -> ())
              | Some project ->
                  let x = fresh() in
                  let act = Printf.sprintf "%s(%s)" project x in
                  if PSet.mem n gr.late_producers then
                    let latev = fresh() in
                    r.r <- (mkSEQ2(dupRhs r,Some x,Some latev,mkACTION2(Some act,Some latev))).r;
                  else
                    r.r <- (mkSEQ2(dupRhs r,Some x,None,mkACTION act)).r;
                  a.early_rettype <- Some(sv_type))
       | _ -> ())
    gr.ds;
  add_to_prologue gr (Buffer.contents b);
  ()

let alt2rules = (* differs from bnf.ml b/c need to desugar Opt *) (*TODO: make default in Gul*)
  let rec loop l r = match r.r with
  | Alt(r1,r2) -> loop (loop l r2) r1
  | Opt(r1) ->
      r.r <- (mkALT[r1;mkLIT ""]).r;
      loop l r
  | _ -> r::l in
  loop []

(* Force every alternative to have the same relevance *)
let force_alt_relevance gr = (*TODO: move out of Wrap*)
  let rec loop r =
    if not(r.a.early_relevant || r.a.late_relevant) then () else
    match r.r with
    | Alt(r1,r2) ->
        (* If r1 is early relevant but r2 is not, then we force r2 to be
           early relevant, so that the coroutine/dispatch will understand
           which alternative is being taken.
           Similarly if r1 is late relevant but r2 is not, then we force r2 to be
           late relevant, so that the replay/dispatch will understand
           which alternative is being taken. *)
        (* Can have one not early_relevant if r is not an early producer (so
           it is not dealt with by lifting).  E.g., (foo@(x)|"") where foo
           does not return an early value.  This came up in tests/extract2. *)
        let force x =
          let early_action =
            (if r.a.early_relevant && not x.a.early_relevant then (x.a.early_relevant <- true; Some "()") else None) in
          let late_action =
            (if r.a.late_relevant && not x.a.late_relevant then (x.a.late_relevant <- true; Some "()") else None) in
          loop x; (* before possible assignment to x.r *)
          if early_action<>None || late_action<>None then
            x.r <- (mkSEQ[mkACTION2(early_action,late_action);dupRhs x]).r (* must put action first else "producerness" of x changes *)
          else () in
        force r1;
        force r2
    | Opt r1 ->
        (* Desugar Opt if it hasn't been done yet. *)
        let y = mkLIT("") in
        r.r <- (mkALT[r1;y]).r; (* NB r and r1 have the same relevance, y has none *)
        loop r
    | Symb _ | Position _ | Lit(_,_) | CharRange(_,_) | Prose _
    | Action _ | When _ | DBranch _ | Box _ | Delay _ -> ()
    | Seq(r1,_,_,r2) | Minus(r1,r2) -> loop r1; loop r2
    | Assign(r1,_,_) | Lookahead (_,r1) | Rcount(_,r1) | Star(_,r1) | Hash(_,r1) -> loop r1
  in
  List.iter
    (function RuleDef(n,r,a) -> loop r | _ -> ())
    gr.ds
