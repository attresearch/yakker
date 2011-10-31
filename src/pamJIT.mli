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

(** Optimization mode. [Scan_opt] groups scans and lookahead into
    tables, respectively.  It does not create combined scan/lookahead
    tables. [Scan_lookahead_opt] does both.
    [Compl_opt] groups completions, but nothing else.
*)
type opt_mode = No_opt | Scan_opt | Scan_lookahead_opt | Compl_opt | Full_opt

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
      | Det_trans of (Pam_internal.pos -> 'a -> 'a * Pam_internal.label)
      | Lexer_trans of (YkBuf.t -> Pam_internal.label)
      | MScan_trans of Pam_internal.label array
      | Lookahead_trans of Pam_internal.label array
      | TokLookahead_trans of Pam_internal.presence * (YkBuf.t -> bool) * Pam_internal.label
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
      | When2_trans of 'a Pam_internal.Pred3.t * Pam_internal.label  (** A more general "when". *)
      | Box_trans of 'a Pam_internal.blackbox * Pam_internal.label

    type 'a pnt_entry = {
      ctarget: Pam_internal.label;
      carg: 'a Pam_internal.action2;
      cbinder: 'a Pam_internal.binder2
    }

    type 'a pnt_table = 'a pnt_entry array array array

    type 'a data = {
      start_symb : nonterm;
      start_state  : Pam_internal.label;
      term_table : 'a trans array;
      nonterm_table : Pam_internal.label array array;
      p_nonterm_table : 'a pnt_table;
    }

    (** Flag to control warnings about use of extended (non-regular) lookahead. Defaults to off. *)
    val warn_extla : bool ref

    val lookup_trans_pnt : 'a pnt_table -> Pam_internal.label -> int
      -> 'a pnt_entry array

    val mk_table :
      opt_mode -> 'a Pam_internal.program -> Pam_internal.nonterm ->
      Pam_internal.label -> Pam_internal.nonterm -> int ->
      'a Pam_internal.action2 -> 'a Pam_internal.binder2 ->
      'a data

    val to_table :
      'a Pam_internal.program -> Pam_internal.nonterm ->
      Pam_internal.label -> Pam_internal.nonterm -> int ->
      'a Pam_internal.action2 -> 'a Pam_internal.binder2 ->
      'a data

    (** Derive a summary of both [nonterm_table] and [p_nonterm_table]
        fields, mapping each state to a list of nonterms with continuations
        for that state. In effect, records the called nonterminals for
        each callsite. The length of the result will be the number of states,
        with a (variable-length) array of nonterminals at each index. *)
    val mk_called_table : 'a data -> Pam_internal.nonterm array array

    val call_targets : 'a trans array -> bool array

    val get_num_states : 'a data -> int

    val measure_percent : 'a data -> ('a trans -> bool) -> float
    val measure_percenti : 'a data -> (int -> 'a trans -> bool) -> float

    (**
        [refl_trans_closure term_table state]

        Return the set of states reachable from [state] via action
        edges. *)
    val refl_trans_closure : 'a trans array -> Pam_internal.label -> Pam_internal.label list

    (**
        [reachable_calls term_table state]

        Return the set of call states reachable from [state] via action
        edges. *)
    val reachable_calls : 'a trans array -> Pam_internal.label -> Pam_internal.label list

    val count_reachable_calls : 'a trans array -> Pam_internal.label -> int


    (** invocation: [compute_integer_property p term_table states]
        [p] takes the transition function of the transducer and a state, and computes,
        for that state, an integer property.
        [term_table] is the transition function.
        [states] is a set of states of interest. It is encoded with a boolean array.
        If [states.(i)] then [i] is in the set.

        Compute an interger property for every state included in the [states] set.

        Returns a list of pairs mapping each state of interest to its integer property.
    *)
    val compute_integer_property : ('a trans array -> Pam_internal.label -> int) ->
      'a trans array -> bool array -> (Pam_internal.label * int) list

    (** For every callee in the transducer, count the number of caller states
        reachable from that callee. *)
    val compute_callee_reachable_calls : 'a trans array -> (Pam_internal.label * int) list

  end

val col_size : int
val iEOF : int (** index of EOF in lookup tables. *)

val lookup_re_trans : int array -> int -> int -> int
