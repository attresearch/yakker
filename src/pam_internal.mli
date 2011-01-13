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

type pos = int

type 'a action = 'a -> 'a

type 'a pred = 'a -> 'a option
type 'a binder = 'a -> 'a -> 'a
type 'a blackbox = 'a -> pos -> YkBuf.t -> (int (* number of bytes consumed.*)
                                            * 'a)  option
type 'a action2 = pos -> 'a -> 'a
type 'a pred2 = 'a -> bool
type 'a pred3 = pos -> 'a -> bool
type 'a next = 'a action
type 'a next3 = 'a action2
type 'a binder2 = pos -> 'a -> 'a -> 'a
type 'a box = 'a blackbox

type presence = bool

type terminal = int
type nonterm = int
type id = string
type label = int

type lookahead_spec =
  | CfgLA of label * nonterm
  | CsLA of Cs.t (** character-set lookahead *)

val min_nonterm : nonterm

module Pred3 : sig

  type recog_fn = nonterm -> label -> YkBuf.t -> bool
    (** A (transducer-based) recognition function. Given a
        nonterminal, start state and buffer, returns accept/reject. *)

  type 'a t = recog_fn -> YkBuf.t -> 'a -> 'a option

  val cs_lookaheadc : Cs.t -> 'a t
  val full_lookaheadc : bool -> nonterm -> label -> 'a t

  val truec : 'a t

  val falsec : 'a t

  val boxc : 'a blackbox -> 'a t

  val callc : 'a t -> 'a action2 -> 'a binder2 -> 'a t

  val andc : 'a t -> 'a t -> 'a t

  val orc : 'a t -> 'a t -> 'a t
end

type 'a instruction =
  | EatInstr of terminal * label
  | CompleteInstr of nonterm
  | RCompleteInstr2 of nonterm * 'a Pred3.t
  | ACallInstr3 of 'a action2 * label
  | ALookaheadInstr of presence * lookahead_spec * label
  | ABlackboxInstr of 'a blackbox * label
  | AAction2Instr of 'a action2 * label
  | WhenSpecialInstr of 'a Pred3.t * label
  | AWhenInstr3 of 'a pred3 * 'a next3 * label
  | ASimpleCont2Instr of nonterm * 'a binder2 * label (** Parameterless continue. *)
  | AContInstr3 of nonterm * 'a action2 * 'a binder2 * label

type 'a block = 'a instruction array

type 'a program = 'a block array

type symbol_table = int -> string

val default_symbol_table : symbol_table

val get_block : 'a program -> int -> 'a block

val load_internal_program : (int * 'a instruction list) list -> 'a program


