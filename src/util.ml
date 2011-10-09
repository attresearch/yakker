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

let warn_undefined n = warn Sys_warn ("Symbol " ^ n ^ " undefined.")

let sys_error = error Sys_warn

let impossible s =
  Printf.eprintf "Impossible: %s\n%!" s;
  raise Exit

(** Marks a features as "todo". Most useful for match cases for which
    we don't want the compiler to emit a match warning, but which still
    need addressing.*)
let todo s = invalid_arg ("Todo: " ^ s)

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
let memsize () = Gc.full_major(); (Gc.stat()).Gc.live_words

let nil = [] (* Used in some generated code because yakker parser currently chokes on [] sometimes *)

(** piped processes [pipe_in_out command send_in get_out] where
    [command] launches a process, [send_in] is function that writes
    into input of the process and [get_out] is a function that reads
    the output of the process. *)
let pipe_in_out command send_in get_out =
  let r,w = Unix.open_process command in

  (* Put data into pipe *)
  send_in w;
  flush w;
  close_out w; (* close so command process stops waiting for input *)
  (* This next because of a bug in the Batteries version of close_out
     TODO: YHM, 5/17/2011: Are we using the Batteries version of close_out
     anymore? I don't think so.*)
  (try let fd = Unix.descr_of_out_channel w in Unix.close fd with _ -> ());

  (* Get results out of pipe *)
  get_out r;

  (* Clean up *)
  (try
    ignore(Unix.close_process (r,w))
  with _ -> ())

(** Like [pipe_in_out], but returns the result from [get_out].
    @raise [Failure] if process exits abnormally. *)
let pipe_in_out_result command send_in get_out =
  let r,w = Unix.open_process command in

  (* Put data into pipe *)
  send_in w;
  flush w;
  close_out w; (* close so command process stops waiting for input *)
  (* This next because of a bug in the Batteries version of close_out *)
  (try let fd = Unix.descr_of_out_channel w in Unix.close fd with _ -> ());

  (* Get results out of pipe *)
  let res = get_out r in

  (* Clean up *)
  begin match Unix.close_process (r,w) with
    | Unix.WEXITED 0 ->   ()
    | Unix.WEXITED x ->   failwith (Printf.sprintf "ocaml exited with %d\n" x)
    | Unix.WSIGNALED x -> failwith (Printf.sprintf "ocaml exited with signal %d\n" x)
    | Unix.WSTOPPED x ->  failwith (Printf.sprintf "ocaml stopped with signal %d\n" x)
  end;

  res

let find_option t x = try Some(Hashtbl.find t x) with Not_found -> None (* in batteries not ocaml *)

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

let option_map f = function None -> None | Some x -> Some (f x)
let option_fold f v = function None -> v | Some x -> f v x

(** Like Haskell's [maybe] function. *)
let option v f = function None -> v | Some x -> f x
let option_get v = option v (fun z -> z)

let is_some = function
        | None -> false
        | _ -> true

(** extract a value in boxed in Some.
    @raises [Not_found] if option is None.*)
let from_some = function None -> raise Not_found | Some s -> s

(* return the set of unique elements in the list *)
let remove_dups xs =
  match List.fast_sort compare xs with
    | ([] | [_]) as v -> v
    | x::xs ->
        let f ((x, xs) as v) y = if x = y then v else (y, x::xs) in
        let x, xs = List.fold_left f (x, []) xs in
        x::xs

(** Group a list into sublists according the provided equality function *)
let group_by cmp xs =
  match List.fast_sort cmp xs with
    | [] -> []
    | y::xs_s ->
        let (x,xs,xss) =
          List.fold_left (fun (y, ys, xss) v -> if cmp y v = 0 then (v,y::ys,xss) else (v,[],(y::ys)::xss))
            (y, [], []) xs_s in
        (x::xs)::xss

let list_last = function
  | x :: xs -> List.fold_left (fun _ x -> x) x xs
  | [] -> raise Not_found

let list_make n f =
  let rec loop i xs =
    if i < n then loop (i + 1) ((f i)::xs) else xs in
  List.rev (loop 0 [])

let list_drop n xs =
  let rec _d n xs =
    match n, xs with
      | 0, _ -> xs
      | _, [] -> []
      | _, x::xs -> _d (n - 1) xs in
  if n <= 0 then xs
  else _d n xs

let list_mem_and_remove x xs =
  let rec _mnr zs = function
    | [] -> false, xs
    | y::ys when y = x -> true, List.rev_append zs ys
    | y::ys -> _mnr (y :: zs) ys in
  _mnr [] xs

let list_intersect cmp xs ys =
  let xs = List.sort cmp xs in
  let ys = List.sort cmp ys in
  let rec _loop1 xs ys result =
    match ys with
      | [] -> result
      | y::ys -> _loop2 xs y ys result
  and _loop2 xs y ys result =
    match xs with
      | [] -> result
      | x::xs ->
          if x < y then _loop2 xs y ys result
          else if x > y then _loop2 ys x xs result
          else (* x = y *) _loop1 xs ys (x :: result) in
  _loop1 xs ys []

let set_of_list l = List.fold_left (fun s x -> PSet.add x s) PSet.empty l

module Operators = struct
  let ($) f g x = f (g x)
  let ($|) f x = f x
end


let module_name_of_filename n =
  String.capitalize (Filename.chop_suffix (Filename.basename n) ".ml")

let file_copy inch outch =
  let buffer = String.create 4096 in
  let rec _fc ()  =
    match input inch buffer 0 4096 with
      | 0 -> ()
      | n -> output outch buffer 0 n; _fc () in
  _fc ();
  flush outch

