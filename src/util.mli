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

(***** Utility functions *****)

val cnt : int ref

type ('l, 'r) binsum = Left of 'l | Right of 'r

type warn_classifier =
    User_warn (** warning sent from user code, e.g. semantic actions. *)
  | Sys_warn  (** warning sent from yakker code *)

(** Print a warning.  *)
val warn : warn_classifier -> string -> unit

(** Print an error.  *)
val error : warn_classifier -> string -> unit

val impossible : string -> 'a

val pipe_in_out: string -> (out_channel -> unit) -> (in_channel -> unit) -> unit

(** m++ *)
val postincr : int ref -> int

(* Variable naming utilities *)
(** Convert all _ to -. *)
val ocaml2bnf : string -> string
val bnf2ocaml : string -> string
val bnf2ocaml_lident : string -> string -> string
  (** [bnf2ocaml_lident prefix s]
      like bnf2ocaml but ensures that the result is a valid
      lowercase identifier (assuming [prefix] is a valid prefix
      for a lowercase identifier).
  *)

val fresh : unit -> string
(** takes a base for the fresh variable as an argument *)
val freshn : string -> string

val find_option: ('a,'b) Hashtbl.t -> 'a -> 'b option (* in batteries not ocaml *)
val is_some: 'a option -> bool (* in batteries not ocaml *)

(* Array utilities *)
val array_existsi: (int -> 'a -> bool) -> 'a array -> bool
val array_contains : 'a -> 'a array -> bool
val int_array_contains : int -> int array -> bool
(** [array_realloc arr len v_init] creates a new array of length [len]
    with the first [min(len, length arr)] elements copied from corresponding
    positions in [arr]. All uncopied positions in the new array will be initialized
    with [v_init]. *)
val array_realloc: 'a array -> int -> 'a -> 'a array
(** Treat the arrays as sets and return a new array which is the union of
    those sets. *)
val array_union: 'a array -> 'a array -> 'a array
(** extract elements matching the predicate and return as a list. *)
val array_extract_to_list: ('a -> bool) -> 'a array -> 'a list
(** [list_map_reduce v_init f_map f_reduce xs] *)
val list_map_reduce: 'c -> ('a -> 'b) -> ('c -> 'b ->' c) -> 'a list -> 'c
