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

type earley_ds = Flat_eds | Hierhash_eds | Hiermap_eds | Sparse_eds

(* Compile-time options for Yakker *)
let inline_cs = ref false
let inline_regular = ref false
let unroll_star_n = ref 3
let lookahead = ref false
let case_sensitive = ref true
let coalesce = ref true
let memoize_history = ref true
let check_labels = ref false
let postfix_history = ref true

let skip_opt = ref true
  (** Perform the label-skipping optimization for late actions. *)

let skip_opt_coroutine = ref false
  (** Perform the label-skipping optimization early actions. *)

let late_only_dbranch = ref true
  (** Treat dbranch as a late-only construct.
      Moreover, compile dbranches into LexerInstr rather than DetBranchInstr.
      In essence, this flag completely redefines dbranch. Currently, there
      is no code in elsewhere in Yakker that turns this off.
      NOTE: The late_only_dbranch is never epsilon, while the normal one is always
      epsilon. This distinction comes up when building nullability predicates. *)

let use_dbranch = ref false
  (** Flag: translate lexer declarations into uses of DBranch. Default off.
      NOTE: the set-lexer syntax uses dbranch regardless of the setting of
      this flag.
      NOTE: Currently, use of this flag is incompatible with the arrow notation
      and the [late_only_dbranch] flag, because it will cause lexer code to attempt
      to generate dbranch code that isn't late only. *)

let use_fsm = ref false
  (** Use FSM toolkit to build transducer; use FST otherwise. *)

let use_wrap_and_attr = ref true
  (** Flag: run wrapping and I/O attribute-elimination as part of regular pipeline. *)

let use_coroutines = ref true
  (** Flag: run coroutines vs. arrow-notation. *)

let collapse_calls = ref true
  (** Flag: collapse calls when possible. *)

let wrap_codegen_in_module = ref false
  (** Flag: wrap generated code in an internal module and only export
      specific elements.  Due to (unexplained) problems with ocamlopt
      compilation (in 3.11) when this feature is turned on, I've disabled it
      by default. *)

let gen_optimize_pam = ref true
  (** Flag: generate code that tells the engine to optimize the
      transducer representation before exection. On by default. *)

let gen_nullpreds = ref true
  (** Flag: Use nullability predicates in place of completions on epsilon. On by default.
      If disabled, nullability predicates will not be generated and an engine implementation
      will be chosen (for the generated code) that supports completions on epsilon. *)

let earley_ds = ref Sparse_eds
  (** Designates the choice of the engine's (Earley) data structures.
      Uses sparse sets by default.*)

let eta_expand_tx_bindings = ref false
  (** Flag: Eta-expand bindings for actions, predicates, boxes,
      etc. used in transducer representation. Avoids causing compiler
      error relating to ungeneralizable type variable. *)

let memoize_eps_parsers = ref true
  (** Flag: Insert code into epsilon-grammar parsing functions to
      memoize calls. Memoization is not necessary, because epsilon
      grammars cannot be left-recursive. It is only a potential
      optimization. In many tested cases, it actually causes a slow
      down. *)

let infer_types_in_two_passes = ref true

(*
  testing-related flags
*)

let unit_history = ref false
let repress_replay = ref false

