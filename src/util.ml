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

(** Utility functions *)

let show_warn = ref true

type ('l, 'r) binsum = Left of 'l | Right of 'r

type warn_classifier =
    User_warn (** warning sent from user code, e.g. semantic actions. *)
  | Sys_warn  (** warning sent from yakker code *)

(** Print a warning.  *)
let warn cl w = if !show_warn then Printf.eprintf "Warning: %s\n%!" w
let error cl e = Printf.eprintf "Error: %s\n%!" e

let impossible s =
  Printf.eprintf "Impossible: %s\n%!" s;
  raise Exit

(** m++ *)
let postincr m =
  let res = !m in
  m := res+1;
  res

let pipe_in_out command send_in get_out =
  let r,w = Unix.open_process command in

  (* Put data into pipe *)
  send_in w;
  flush w;
  close_out w; (* close so command process stops waiting for input *)
  (* This next because of a bug in the Batteries version of close_out *)
  (try let fd = Unix.descr_of_out_channel w in Unix.close fd with _ -> ());

  (* Get results out of pipe *)
  get_out r;

  (* Clean up *)
  (try
    ignore(Unix.close_process (r,w))
  with _ -> ())

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

let cnt = ref 1 (*TODO: move to Compileopt*)

let tbl_ocaml2bnf = Hashtbl.create 11
let tbl_bnf2ocaml = Hashtbl.create 11

let find_option t x = try Some(Hashtbl.find t x) with Not_found -> None (* in batteries not ocaml *)
let bnf2ocaml s_bnf =
  match find_option tbl_bnf2ocaml s_bnf with
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
        s2 := s^(string_of_int(postincr cnt))
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
let fresh() = bnf2ocaml (Printf.sprintf "_x%d" (postincr cnt))
let freshn base = bnf2ocaml (Printf.sprintf "_%s%d" base (postincr cnt))

let is_some = function
        | None -> false
        | _ -> true

let array_contains x xs =
  let rec loop x xs n i =
    if i = n then false
    else if x = (Array.unsafe_get xs i) then true
    else loop x xs n (succ i)
  in
  let n = Array.length xs in
  loop x xs n 0

let int_array_contains (x:int) (xs:int array) =
  let rec loop x xs n i =
    if i = n then false
    else if x = (Array.unsafe_get xs i) then true
    else loop x xs n (succ i)
  in
  let n = Array.length xs in
  loop x xs n 0

let array_existsi p xs =
  let rec loop p xs n i =
    if i = n then false
    else if p i (Array.unsafe_get xs i) then true
    else loop p xs n (succ i)
  in
  let n = Array.length xs in
  loop p xs n 0

let array_realloc oldarr len v =
  let newarr = Array.make len v in
  let m = Array.length oldarr in
  let min_len = if len < m then len - 1 else m - 1 in
  for i = 0 to min_len do
    Array.unsafe_set newarr i (Array.unsafe_get oldarr i);
  done;
  newarr

let array_union a1 a2 =
  let a_all = Array.append a1 a2 in
  Array.fast_sort compare a_all;
  if Array.length a_all < 2 then a_all else
    (* Compact the array in place, removing duplicates. *)
    let n, dups = Array.fold_left
      (fun (i, dups) x ->
         let d =
           if i = 0 then 0
           else if x = a_all.(i - 1) then dups + 1
           else (a_all.(i - dups) <- x; dups)
         in (i + 1, d)) (0, 0) a_all in
    if dups = 0 then a_all
    else Array.sub a_all 0 (n - dups)

(* We fold right to maintain the relative ordering of the elements.*)
let array_extract_to_list p arr =
  Array.fold_right (fun x ys -> if p x then x::ys else ys) arr []

(* @raise Not_found. *)
let array_binary_search arr key =
  let rec bfind arr key b e =
    if b < e then begin
      let m = (b + e) / 2 in
      let c = key - arr.(m) in
      if c = 0 then Some m
      else if c > 0 then bfind arr key (m + 1) e
      else bfind arr key b (m - 1)
    end
    else if b = e && key = arr.(b) then Some b
    else None in
  let len = Array.length arr in
  bfind arr key 0 len

let list_map_reduce v f_m f_r xs =
  List.fold_left (fun a x -> f_r a (f_m x)) v xs
