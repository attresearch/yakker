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
type 'a action2 = pos -> 'a -> 'a

type 'a pred = 'a -> 'a option
type 'a pred2 = 'a -> bool
type 'a pred3 = pos -> 'a -> bool
type 'a next = 'a action
type 'a next3 = 'a action2
type 'a binder = 'a -> 'a -> 'a
type 'a binder2 = pos -> 'a -> 'a -> 'a

type 'a blackbox = 'a -> int -> YkBuf.t -> (int (* number of bytes consumed.*)
                                            * 'a)  option
type 'a box = 'a blackbox

type presence = bool

type terminal = int
type nonterm = int
type id = string
type label = int

type lookahead_spec =
  | CfgLA of label * nonterm
  | CsLA of Cs.t (** character-set lookahead *)

let min_nonterm = 264

module Pred3 = struct
  type recog_fn = nonterm -> label -> YkBuf.t -> bool
    (** A (transducer-based) recognition function. Given a
        nonterminal, start state and buffer, returns accept/reject. *)

  type 'a t = recog_fn -> YkBuf.t -> 'a -> 'a option

  let truec r ykb v = Some v

  let falsec r ykb v = None

  let cs_lookaheadc cs r ykb v =
    let c = if YkBuf.is_eof ykb then 256 else Char.code (YkBuf.get_current ykb) in
    if Cs.member cs c then Some v else None

  let full_lookaheadc b nt_old target =
    let nt = nt_old - min_nonterm in
    fun r ykb v ->
      if b = r nt target ykb then Some v else None

  let boxc box r ykb v =
    let cp = YkBuf.save ykb in
    let r = match box v (YkBuf.get_offset ykb) ykb with
      | Some (0, v2) -> Some v2
      | None | Some _ -> None in
    YkBuf.restore ykb cp;
    r

  let callc symb_pred action binder r ykb v =
    let p = YkBuf.get_offset ykb in
    match symb_pred r ykb (action p v) with
        None -> None
      | Some v2 -> Some (binder p v v2)

  let andc f g r ykb v =
    match f r ykb v with
        None -> None
      | Some v2 -> g r ykb v2

  let orc f g r ykb v =
    match f r ykb v with
        None -> g r ykb v
      | Some v2 -> Some v2
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

let default_symbol_table i =
  if i = -1 then "DUMMY"
  else if i < 128 then Char.escaped (char_of_int i)
  else Printf.sprintf "S%i" i

let get_block p s = p.(s)

let load_internal_program p =
  let dummy_block = Array.make 0 (CompleteInstr 1) in
  let max_label = List.fold_left (fun m (l,_) -> max m l) 0 p in
  let p_arr = Array.make (max_label + 1) dummy_block in
  (* Put each block in its proper place. *)
  List.iter (fun (l,instrs) ->
               p_arr.(l) <- Array.of_list instrs) p;
  p_arr

