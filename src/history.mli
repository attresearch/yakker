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

(* History data structure *)

type ('a,'b) root

(* for traversals of a history *)
class type ['a] enum =
      object
        method next : unit -> 'a
      end

class type ['a,'lbl] history =
      object ('b)
        method empty : 'lbl -> 'b
        method merge : 'lbl -> 'a -> 'b -> 'b
        method push : 'lbl -> 'a -> 'b

        method left_to_right : 'a enum
        method right_to_left : 'a enum
        method get_root : ('a,'lbl) root
          (** would prefer to hide this method, but we can't. instead,
          we simply make it nearly useless by hiding the root
          type. this is the "friend" pattern. *)
      end

type label = int

module type HV = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val memoize : bool
end

module type S =
sig
  type hv
  val compare : (hv, label) history -> (hv, label) history -> int
  val hash : (hv, label) history -> int
  val new_history : unit -> (hv, label) history

  module Root_id_set : Set.S
  val get_id_set : (hv, label) root -> Root_id_set.t
    (** Get the set of (unique) identifiers reachable from the given root. *)
  val add_id_set : (hv, label) root -> Root_id_set.t -> Root_id_set.t
    (** Add the set of (unique) identifiers reachable from the given root to the given id set. *)

  val dot_show : (hv -> string) -> (hv, label) history -> unit
  val dot_show_pretty : (hv -> string) -> (hv, label) history -> unit
end

module Make (Hv : HV) : S with type hv = Hv.t
