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

let get_paramtype s =
  (* s is a parameter and type declaration, e.g., "x:int", and this extracts the type *)
  try
    Scanf.sscanf s "%[^:]:%[^\\000]" (fun var typ -> typ)
  with End_of_file -> "int" (* no colon -> no type -> use int as default *)

let get_param s =
  (* s is a parameter and type declaration, e.g., "x:int", and this extracts the parameter *)
  try
    Scanf.sscanf s "%[^:]:%[^\\000]" (fun var typ -> var)
  with End_of_file -> s (* no colon -> no type *)

let find tbl x =
  try
    Hashtbl.find tbl x
  with Not_found ->
    (Printf.eprintf "Internal wrap error: could not find %s\n%!" x; raise Not_found)

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
  List.iter
    (function
        RuleDef(n,r,a) ->
          (match a.early_rettype with None -> () | Some x   -> types := PSet.add x !types);
          (match a.early_params with None -> () | Some x -> types := PSet.add (get_paramtype x) !types)
      | _ -> ())
    gr.ds;
  (* Each type gets a corresponding datatype constructor *)
  let b = Buffer.create 11 in                     (* Print out the type declaration *)
  Printf.bprintf b "type %s =\n" sv_type;
  Printf.bprintf b "| %s\n" sv_unit;
  let tbl_type_constructor = Hashtbl.create 11 in (* Map types to their constructors *)
  PSet.iter
    (fun t ->
      let x = "Yk"^(fresh()) in
      Printf.bprintf b "| %s of (%s)\n" x t; (* The parens are necessary if t is a tuple type *)
      Hashtbl.add tbl_type_constructor t x)
    !types;
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
  (* At every call wrap the arguments and unwrap the results*)
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
            r.r <- (mkSEQ2(dupRule r,Some x,Some latev,mkACTION2(Some act,Some latev))).r;
          else
            r.r <- (mkSEQ2(dupRule r,Some x,None,mkACTION act)).r)
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
                  r.r <- (mkSEQ2(mkACTION act,Some param,None,dupRule r)).r);
           let ntp_opt = try find tbl_nt_project n with Not_found -> None in
           (match ntp_opt with
              | None ->
                  (* If r has early actions but no rettype, then the early actions may cause an unwrapped value
                     to be returned... *)
                  (match r.a.early_relevant, PSet.mem n gr.late_producers with
                    true,false ->
                      (* By appending "" we effectively return a wrapped unit value *)
                      r.r <- (mkSEQ2(dupRule r,None,None,mkLIT "")).r
                  | true,true ->
                      (* Here we must instead clobber the unwrapped early value while preserving the late value *)
                      let latev = fresh() in
                      r.r <- (mkSEQ2(dupRule r,None,Some latev,mkACTION2(None,Some latev))).r
                  | _ -> ())
              | Some project ->
                  let x = fresh() in
                  let act = Printf.sprintf "%s(%s)" project x in
                  if PSet.mem n gr.late_producers then
                    let latev = fresh() in
                    r.r <- (mkSEQ2(dupRule r,Some x,Some latev,mkACTION2(Some act,Some latev))).r;
                  else
                    r.r <- (mkSEQ2(dupRule r,Some x,None,mkACTION act)).r;
                  a.early_rettype <- Some(sv_type))
       | _ -> ())
    gr.ds;
  add_to_prologue gr (Buffer.contents b);
  ()




(* Wrapping for histories.

   Histories are a polymorphic type, class ['a] history, where in the
   simplest case, 'a is int, the type of dispatch labels.  However, if
   delay is used, 'a needs to include the delayed type.  History
   wrapping finds all of the delayed types and constructs a datatype
   if necessary, and inserts projections and injections as needed.
 *)
let transform_history gr =
  (* Get all delay types *)
  let types = ref PSet.empty in
  types := PSet.add "int" !types;
  let rec loop r =
    match r.r with
    | Delay(_,Some x) ->
        types := PSet.add x !types

    | Alt(r1,r2) | Seq(r1,_,_,r2) | Minus(r1,r2) ->
        loop r1; loop r2

    | Assign(r1,_,_) | Opt r1 | Lookahead (_,r1) | Rcount(_,r1) | Star(_,r1) | Hash(_,r1) ->
        loop r1

    | Symb _ | Position _ | Lit(_,_) | CharRange(_,_) | Prose _
    | Action _ | When _ | Box _ | Delay(_,None) ->
        ()
  in
  List.iter
    (function RuleDef(n,r,a) -> loop r | _ -> ())
    gr.ds;
  if 1 = PSet.cardinal !types
  then
    (* No need to wrap if we only use int *)
    add_to_prologue gr "type hv = int\n;;\nlet _l2hv x = x;; (* label to hv *)\n"
  else begin
    (* Otherwise, each type gets a corresponding datatype constructor *)
    gr.wrapped_history <- true;
    let b = Buffer.create 11 in                     (* Print out the type declaration *)
    Printf.bprintf b "type hv =\n";
    Printf.bprintf b "| Ykd_int of int\n";          (* Hard-code this for use by labels, see dispatch.ml *)
    let tbl_type_constructor = Hashtbl.create 11 in (* Map types to their constructors *)
    Hashtbl.add tbl_type_constructor "int" "Ykd_int";
    PSet.iter
      (fun t ->
        if t<>"int" then begin
          let x = "Ykd"^(fresh()) in
          Printf.bprintf b "| %s of (%s)\n" x t;    (* NB parens force a reference if t is a tuple type *)
          Hashtbl.add tbl_type_constructor t x
        end)
      !types;
    Printf.bprintf b ";;\nlet _l2hv x = Ykd_int(x);; (* label to hv *)\n";
    add_to_prologue gr (Buffer.contents b);
    (* Wrap and unwrap at delay *)
    let rec loop r =
      match r.r with
      | Delay(e,topt) ->
          let wrapped,unwrapped = fresh(),fresh() in
          let constructor =
            (match topt with None -> "Ykd_int"
            | Some x -> find tbl_type_constructor x) in
          let wrap_act = Printf.sprintf "%s(%s)" constructor e in
          let unwrap_act =
            Printf.sprintf "(match %s with %s(%s) -> %s | _ -> failwith \"@delay wrap\")"
              wrapped constructor unwrapped unwrapped in
          r.r <-
            (mkSEQ2(mkDELAY(wrap_act,None),None,Some wrapped,mkACTION2(None,Some unwrap_act))).r

      | Alt(r1,r2) | Seq(r1,_,_,r2) | Minus(r1,r2) ->
          loop r1; loop r2

      | Assign(r1,_,_) | Opt r1 | Lookahead (_,r1) | Rcount(_,r1) | Star(_,r1) | Hash(_,r1) ->
          loop r1

      | Symb _ | Position _ | Lit(_,_) | CharRange(_,_) | Prose _
      | Action _ | When _ | Box _ ->
          ()
    in
    List.iter
      (function RuleDef(n,r,a) -> loop r | _ -> ())
      gr.ds
  end;
  add_to_prologue gr "
module Yk_Hashed = struct
  type t = hv * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
end
module Yk_History = Yak.History.Make(Yk_Hashed)
"

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
            x.r <- (mkSEQ[mkACTION2(early_action,late_action);dupRule x]).r (* must put action first else "producerness" of x changes *)
          else () in
        force r1;
        force r2
    | Opt r1 ->
        (* Desugar Opt if it hasn't been done yet. *)
        let y = mkLIT("") in
        r.r <- (mkALT[r1;y]).r; (* NB r and r1 have the same relevance, y has none *)
        loop r
    | Symb _ | Position _ | Lit(_,_) | CharRange(_,_) | Prose _
    | Action _ | When _ | Box _ | Delay _ -> ()
    | Seq(r1,_,_,r2) | Minus(r1,r2) -> loop r1; loop r2
    | Assign(r1,_,_) | Lookahead (_,r1) | Rcount(_,r1) | Star(_,r1) | Hash(_,r1) -> loop r1
  in
  List.iter
    (function RuleDef(n,r,a) -> loop r | _ -> ())
    gr.ds
