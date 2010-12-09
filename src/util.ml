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

let list_of_opt = function None -> [] | Some v -> [v]
let cons_opt opt vs = match opt with None -> vs | Some v -> v::vs

(* memory statistics *)
let memcount = ref 0
let memstat () =
  (Gc.full_major(); Printf.eprintf "%d %d\n%!" (postincr memcount) ((Gc.stat()).Gc.live_words))

(* piped processes *)
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

let find_option t x = try Some(Hashtbl.find t x) with Not_found -> None (* in batteries not ocaml *)
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

let array_exists p xs =
  let rec loop p xs n i =
    if i = n then false
    else if p (Array.unsafe_get xs i) then true
    else loop p xs n (succ i)
  in
  let n = Array.length xs in
  loop p xs n 0

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
