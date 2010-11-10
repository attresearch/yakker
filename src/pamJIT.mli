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

(** first argument is program array. second argument is threshold value above which a series of eat instructions is
    optimized into a table.

    first argument may be mutated.
*)
val lookup_trans : int array -> int -> int -> int

(** convert a program into a lookup table for
    a DFA-based engine.

    @return state-indexed lookup table and set of final states.

    [p] can only be converted if it has at most one Eat instruction
    per terminal at each index. *)
val convert_to_dfa_table :
  'a Pam_internal.program -> int array * bool array

(** convert a program into a lookup table and
    a table of epsilon transitions for
    a NFA-based engine.

    [p] can only be converted if it has at most one Eat instruction
    per terminal at each index.

    @return if [p] has n states, returns a n * 256 array representing
    terminal transitions, an n-array of arrays containing epsilon
    transitions, and the set of final nodes.
*)
val convert_to_nfa :
  'a Pam_internal.program -> int array * int array array * bool array


exception Not_ELR0 of string

type elr0_trans =
  | Scan_trans of Pam_internal.label array
  | Call_trans of Pam_internal.label
  | Complete_trans of Pam_internal.nonterm
  | No_trans

val lookup_trans_nt : Pam_internal.label array array -> Pam_internal.label -> int
  -> Pam_internal.label

(** [parse_ELR0_tbl pam_program start_symb start_state min_symb num_symbs]
    @return start_nonterminal, start_state, term_table, nonterm_table
*)
val convert_to_ELR0_table :
  'a Pam_internal.program -> Pam_internal.nonterm ->
  Pam_internal.label -> Pam_internal.nonterm -> int ->
  int * Pam_internal.label * elr0_trans array * Pam_internal.label array array

module NELR0 :
  sig

    type lookahead_spec =
      | NoLA
      | ReLA of Pam_internal.label * Pam_internal.nonterm * Pam_internal.label
      | CsLA of Pam_internal.label array

    type multi_trans = {scans: Pam_internal.label array; (* [||] -> none *)
			call: Pam_internal.label;        (* 0 -> none. *)
			completes: Pam_internal.nonterm array; (* [||] -> none *)
			ext_lookahead: lookahead_spec} (* NoLA -> none *)

    type det_trans = int

(*     type det_trans =  *)
(*       | Scan_dtrans of Pam_internal.label *)
(*       | Lookahead_dtrans of Pam_internal.label *)
(*       | Call_dtrans of Pam_internal.label *)
(*       | Complete_dtrans of Pam_internal.nonterm *)

    type trans =

      (* Deterministic transitions. *)
      | No_trans
      | Scan_trans of Pam_internal.label array
      | Lookahead_trans of Pam_internal.label array
      | ExtLookahead_trans of Pam_internal.label * Pam_internal.nonterm * Pam_internal.label
      | Det_multi_trans of det_trans array
      | Call_trans of Pam_internal.label
      | Complete_trans of Pam_internal.nonterm

      (* Nondeterministic transitions. We optimized the case of
	 multiple completions. Multiple scans/calls are discouraged b/c
	 they can be removed by determinizing the machine, so we don't
	 optimize them. *)
      | MComplete_trans of Pam_internal.nonterm array
      | Multi_trans of multi_trans (** Fixed collection of transitions. *)
      | Many_trans of trans array (** Arbitrary collection of transitions. *)

    exception Not_NELR0 of string

    val to_table :
      'a Pam_internal.program -> Pam_internal.nonterm ->
      Pam_internal.label -> Pam_internal.nonterm -> int ->
      int * Pam_internal.label * trans array * Pam_internal.label array array
  end

module DNELR :
  sig

    type det_trans = int

    type nonterm = int

    type 'a trans =
      (* Deterministic transitions. *)
      | No_trans
      | Scan_trans of Pam_internal.terminal * Pam_internal.label
      | MScan_trans of Pam_internal.label array
      | Lookahead_trans of Pam_internal.label array
      | RegLookahead_trans of Pam_internal.presence * Pam_internal.label * nonterm * Pam_internal.label
      | ExtLookahead_trans of Pam_internal.presence * Pam_internal.label * nonterm * Pam_internal.label
      | Det_multi_trans of det_trans array
      | Call_trans of Pam_internal.label
      | Complete_trans of nonterm

      (* Nondeterministic transitions. We optimized the case of
	 multiple completions. Multiple scans/calls are discouraged b/c
	 they can be removed by determinizing the machine, so we don't
	 optimize them. *)
      | MComplete_trans of nonterm array
      | Many_trans of 'a trans array
	  (** Arbitrary collection of transitions. *)

      (* Context-sensitive transitions. *)
      | Maybe_nullable_trans2 of nonterm * 'a Pam_internal.Pred3.t
      | Call_p_trans of 'a Pam_internal.action2 * Pam_internal.label
      | Complete_p_trans of nonterm
      | MComplete_p_trans of nonterm array
      | Action_trans of 'a Pam_internal.action2 * Pam_internal.label
      | When_trans of 'a Pam_internal.pred3 * 'a Pam_internal.next3
	  * Pam_internal.label
      | Box_trans of 'a Pam_internal.blackbox * Pam_internal.label

    type 'a pnt_entry = {
      ctarget: Pam_internal.label;
      carg: 'a Pam_internal.action2;
      cbinder: 'a Pam_internal.binder2
    }

    type 'a data = {
      start_symb : nonterm;
      start_state  : Pam_internal.label;
      term_table : 'a trans array;
      nonterm_table : Pam_internal.label array array;
      p_nonterm_table : 'a pnt_entry array array;
    }

    val lookup_trans_pnt : 'a pnt_entry array array -> Pam_internal.label -> int
      -> 'a pnt_entry

    val to_table :
      'a Pam_internal.program -> Pam_internal.nonterm ->
      Pam_internal.label -> Pam_internal.nonterm -> int ->
      'a Pam_internal.action2 -> 'a Pam_internal.binder2 ->
      'a data

    val measure_percent : 'a data -> ('a trans -> bool) -> float
    val measure_percenti : 'a data -> (int -> 'a trans -> bool) -> float
    val call_targets : 'a data -> bool array

  end

val col_size : int
val iEOF : int (** index of EOF in lookup tables. *)

val lookup_re_trans : int array -> int -> int -> int
