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

(** Name conversion.

   We need fresh constructors corresponding to BNF rulenames (our
   nonterminals).  It is convenient if the constructors are similar
   to the rulenames, e.g., rulename DIGIT might have corresponding
   constructor DIGIT.  However, we need to make sure constructors are truly
   fresh.  This is handled by bnf2ocaml, below.

   In ocaml a constructor has syntax `tagname where

      constructor ::= capitalized-ident
            ident ::= (letter| _) { letter| 0...9| _| ' }
           letter ::= A...Z | a...z

   Our rulenames are defined by

     rulename-body = ALPHA|DIGIT|"-"|":"
          rulename = ALPHA *rulename-body

   So we have characters "-" and ":" which are not allowed in ocaml
   constructors.
*)

open Yak
let postincr = Util.postincr
let counter = ref 1

let tbl_ocaml2bnf = Hashtbl.create 11
let tbl_bnf2ocaml = Hashtbl.create 11

let bnf2ocaml s_bnf =
  match Util.find_option tbl_bnf2ocaml s_bnf with
    Some x -> x
  | None ->
      (* Avoid - and : *)
      let s = String.copy s_bnf in
      for i = 0 to String.length s - 1 do
        let c = String.get s i in
        if c='-' || c=':' then
          String.set s i '_';
      done;
      (* Make sure that ocaml name is not one that we have chosen before,
         e.g., foo-bar and foo:bar would both map to _foo_bar. *)
      let s2 = ref s in
      while Hashtbl.mem tbl_ocaml2bnf !s2 do
        if Logging.activated then
          Logging.log Logging.Features.util
            "Name clash for %s: %s is already assigned to %s.\n"
            s_bnf !s2 (Hashtbl.find tbl_ocaml2bnf !s2);
        s2 := s^(string_of_int(postincr counter))
      done;
      (* Record choice and return it *)
      Hashtbl.add tbl_ocaml2bnf !s2 s_bnf;
      Hashtbl.add tbl_bnf2ocaml s_bnf !s2;
      !s2

let bnf2ocaml_lident prefix s_bnf =
  if s_bnf.[0] >= 'A' && s_bnf.[0] <= 'Z' then
    bnf2ocaml (prefix ^ s_bnf)
  else bnf2ocaml s_bnf

(** Convert all _ to -. *)
let ocaml2bnf s_ocaml =
  let s = String.copy s_ocaml in
  for i = 0 to String.length s - 1 do
    let c = String.get s i in
    if c='_' then String.set s i '-';
  done;
  s

(* We also need fresh variables *)
(* Assume user doesn't use _ in front of their names *)
let fresh() = bnf2ocaml (Printf.sprintf "_x%d" (postincr counter))
let freshn base = bnf2ocaml (Printf.sprintf "_%s%d" base (postincr counter))

let fresh_nonterminal () = Printf.sprintf "_%d" (postincr counter)

(** Determine whether [s] is already a variable (rather than an
    arbitrary expression).  This is a conservative approximation. *)
let already_var s =
  let is_digit c = '0' <= c && c <= '9' in
  let is_lower c = 'a' <= c && c <= 'z' in
  let is_upper c = 'A' <= c && c <= 'Z' in
  let is_ok c = '_'=c || is_digit c || is_lower c || is_upper c in
  try
    for i = 0 to String.length s - 1 do
      if not(is_ok(String.get s i)) then raise Exit
    done;
    true
  with Exit ->
    false

(** A list of "well-known" names: *)

let tk_mod = "Yk_Tok_mod"
  (** The tokenizer/term-language module generated when @set-lexer is used. *)
