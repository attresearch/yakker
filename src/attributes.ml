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

(* Attribute elimination *)

(* Note attribute elimination can change "producerness", so that
   analysis needs to be run after this phase *)

open Yak
open Gul
open Attr

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

let sort = List.sort (fun (x,_) (y,_) -> compare x y) (*TEMPORARY until copyrule does this*)

let combined_type default attributes =
  let type_of_attributes =
    String.concat " * "
      (List.map snd (sort attributes)) in
  match default,attributes with
  |   None,[] -> None
  | Some x,[] -> Some x
  |   None,_  -> Some type_of_attributes
  | Some x,_  -> Some("("^x^") * "^type_of_attributes)

let output_type a =
  combined_type
    a.early_rettype
    a.output_attributes

let input_type a =
  combined_type
    (match a.early_params with
      Some "" | None -> None (*TODO: resolve "" case*)
    | Some x -> Some(get_paramtype x))
    a.input_attributes

let extract source pattern var = Printf.sprintf "(match %s with (%s) -> %s)" source pattern var
let assign r source pattern attributes =
  List.iter
    (fun attribute ->
      r.r <- (mkSEQ[mkASSIGN(mkACTION(extract source pattern attribute),Some attribute,None);dupRule r]).r)
    attributes
let bind r source pattern var =
  r.r <- (mkSEQ2(mkACTION(extract source pattern var),Some var,None,dupRule r)).r

(* Main transformation *)
(* Assumes all input attributes are present and sorted, this needs to be ensured by copyrule *)
(* Assumes parameter and input attributes are distinct *)
(* Assumes output attributes are distinct *)
let eliminate gr =
  (* At the start of each nonterminal, unpack input attributes *)
  List.iter
    (function
        RuleDef(n,r,a) ->
          if a.input_attributes=[] then () else begin
            a.input_attributes <- sort a.input_attributes; (*TEMPORARY*)
            let source = Variables.fresh() in (* parameter of nonterminal *)

            let pattern =                (* structure of parameter *)
              let inputs =
                (match a.early_params with
                | Some "" | None -> List.map fst a.input_attributes (*TODO: resolve "" case*)
                | Some y ->
                    (get_param y)::(List.map fst a.input_attributes)) in
              String.concat "," inputs in

            (* Assign the input attributes *)
            assign r source pattern (List.map fst a.input_attributes);

            (* Bind the input *)
            (match a.early_params with
            | Some "" | None -> () (*TODO: resolve "" case*)
            | Some y ->
                let param = get_param y in
                bind r source pattern param);

            (* Update input parameter and type *)
            (match input_type a with
              None -> ()
            | Some i_type ->
                a.early_params <- Some(source^":"^i_type));
            a.input_attributes <- [];
            ()
          end
      | _ -> ())
    gr.ds;
  (* At the end of each nonterminal, pack output attributes *)
  let out = Hashtbl.create 11 in (* save output attributes of each nonterminal for later use at call *)
  List.iter
    (function
        RuleDef(n,r,a) ->
          a.output_attributes <- sort a.output_attributes; (*TEMPORARY*)
          Hashtbl.add out n a.output_attributes;
          if a.output_attributes=[] then () else begin
            let early =
              match a.early_rettype with
                None -> None
              | Some _ -> Some(Variables.fresh()) in
            let act =
              let outputs =
                match early with
                  None -> List.map fst a.output_attributes
                | Some x -> x::(List.map fst a.output_attributes) in
              Some("("^(String.concat "," outputs)^")") in
            let late =
              match PSet.mem n gr.late_producers with
                false -> None
              | true -> Some(Variables.fresh()) in
            r.r <- (mkSEQ2(dupRule r,early,late,mkACTION2(act,late))).r;
            let o_type = output_type a in
            a.output_attributes <- [];
            a.early_rettype <- o_type;
            ()
          end
      | _ -> ())
    gr.ds;
  (* At every call pack the arguments and unpack the results*)
  let rec loop r = match r.r with
  | Symb(n,eopt,input_attributes,lopt) ->
      (* Pack arguments *)
      (if input_attributes=[] then () else begin
        let input_attributes = sort input_attributes in (*TEMPORARY*)
        let args =
          (match eopt with
            None ->
              List.map snd input_attributes
          | Some e ->
              e::List.map snd input_attributes) in
        let arg = Some("("^(String.concat "," args)^")") in
        r.r <- Symb(n,arg,[],lopt)
      end);

      (* Unpack results *)
      let output_attributes =
        try
          Hashtbl.find out n
        with Not_found ->
          Printf.eprintf "Warning: can't find output attributes of %s (Attributes.eliminate)\n" n;
          [] in
      if output_attributes=[] then () else begin
        let output_attributes = sort output_attributes in (*TEMPORARY*)

        let late =
          match PSet.mem n gr.late_producers with
            false -> None
          | true -> Some(Variables.fresh()) in

        let early =
          match PSet.mem n gr.early_producers with
            false -> None
          | true -> Some(Variables.fresh()) in

        let r_body = mkACTION2(early,late) in

        let source = Variables.fresh() in (* output of nonterminal *)
        let pattern =                (* structure of output *)
          let outputs =
            (match early with
              None -> List.map fst output_attributes
            | Some y ->
                y::(List.map fst output_attributes)) in
          String.concat "," outputs in
        (* Assign the output attributes *)
        assign r_body source pattern (List.map fst output_attributes);
        (* Bind the output, if any *)
        (match early with
          None -> ()
        | Some y ->
            bind r_body source pattern y);

        r.r <- (mkSEQ2(dupRule r,Some source,late,r_body)).r;

        ()
      end
  | Position _ | Lit _ | CharRange _ | Prose _
  | DBranch _
  | Action _ | When _ | Box _ | Delay _ -> ()
  | Seq(r2,_,_,r3) | Alt(r2,r3) | Minus(r2,r3) -> loop r2; loop r3
  | Assign(r2,_,_) | Lookahead (_,r2) | Opt r2 | Rcount(_,r2) | Star(_,r2) | Hash(_,r2) -> loop r2
  in
  List.iter
    (function RuleDef(n,r,a) -> loop r | _ -> ())
    gr.ds;
  ()
