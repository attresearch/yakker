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

type 'a root
class ['a] history :
    ('a root -> 'a root) ->
      object ('b)
        method empty : 'b
        method merge : 'a -> 'b -> 'b
        method push : 'a -> 'b
        method get_root : 'a root (* would prefer to hide this but cannot *)
      end

(* for postfix traversals of a history *)
class ['a] postfix :
    'a history ->
      object
        method next : unit -> 'a
      end

module type HV = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
end
module Make :
  functor (Hv : HV) ->
    sig
      val compare : Hv.t history -> Hv.t history -> int
      val hash : Hv.t history -> int
      val memoize : bool ref
      val new_history : unit -> Hv.t history
    end

module Make_show :
  functor (Hv : HV) ->
    sig
      val dot_show : (Hv.t -> string) -> Hv.t history -> unit
    end
