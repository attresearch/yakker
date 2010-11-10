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

(** Module for converting old PAM programs into new
    version.
*)


(** Load a PAM program in the internal format. *)
val load_program : Pam.program -> unit Pam_internal.program

(** Create a parser based on the old PAM format. *)
module MakeParser(Spec: sig val pvm_stream : string
			    val pvm_start : int
			    val start_symbol : int
			    val pvm_symbol_table : Sppf.symbol_table
			    val blackboxes : (string -> Pam_interpreter.blackbox_fn) option
			    val external_fns : (string -> Pam_interpreter.external_fn) option
			    val predicates : (string -> Pam_interpreter.predicate) option
(* 			    val binder_table : ('a -> 'a -> 'a) array (\* binders, their indices are used in ACont *\) *)
		  end) :
sig
  val parse_fun_opt : ( (string -> unit) *
			  (string -> Pam_interpreter.node option list * YkBuf.Snapshot.t)) option
end
