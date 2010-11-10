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

(* Parse-time functions. *)

(** get the current input position. *)
val get_position : unit -> int
val set_position : int -> unit

val get_max_position : unit -> int
  (** get the maximum input position. *)

(** begin and end positions, (end is non inclusive) *)
val get_substring : int -> int -> string

(** for internal use: *)
val default_get_substring: YkBuf.t -> YkBuf.checkpoint -> int -> int -> string
val set_get_substring_fun : (int -> int -> string) -> unit

(* Post-parse time functions. *)

(** begin and end positions, (end is non inclusive) *)
val get_string : int -> int -> YkBuf.Snapshot.t -> string
