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

(* for postfix traversals of a history *)
class type ['a] postfix =
      object
        method next : unit -> 'a
      end

class type ['a,'lbl] history =
      object ('b)
        method empty : int -> 'b
        method merge : int -> 'a -> 'b -> 'b
        method push : int -> 'a -> 'b

	method traverse_postfix : 'a postfix
        method get_root : ('a,'lbl) root 
	  (** would prefer to hide this method, but we can't. instead,
	  we simply make it nearly useless by hiding the root
	  type. this is the "friend" pattern. *)
      end

type label

module type HV = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (Hv : HV) :
    sig
      val compare : (Hv.t, label) history -> (Hv.t, label) history -> int
      val hash : (Hv.t, label) history -> int
      val memoize : bool ref
      val new_history : unit -> (Hv.t, label) history

      val dot_show : (Hv.t -> string) -> (Hv.t, label) history -> unit
      val dot_show_pretty : (Hv.t -> string) -> (Hv.t, label) history -> unit
    end
