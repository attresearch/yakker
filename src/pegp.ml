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

(** PEG parser combinators.*)

(** We support two different versions of kleene-closure: the one from
    Ford's paper, in which termination only occurs on failure of the body
    of the closure, and the (apparent) one from Aurochs, which terminates
    a star if no progress is made (i.e. the underlying expression accepts
    epsilon).

*)

exception Parse_fail

let input = ref "helalo"
let input_len = ref (String.length !input)
let set_input s =
  input := s;
  input_len := String.length s
let ykbuf = ref (YkBuf.string2buf !input)
let set_ykbuf ykb =
  ykbuf := ykb

(*

   N.B. Setting positions right is quite tricky. Current invariant is
   that on entry to a combinator, position should be set correctly. So,
   the combinator does not need to set the position itself on entry.
   On exit from a combinator, the position is set at the position following
   the data parsed by the combinator.

*)

let eps st = st

let term c (i, v) =
  if i < !input_len && !input.[i] = c then
    (Yakker.set_position (i+1); (i+1, v))
  else
    raise Parse_fail

let terms s =
  match String.length s with
    | 0 -> eps
    | 1 -> term s.[0]
    | n -> (fun (i, v) ->
	      if i + n <= !input_len then
		let s1 = !input in
		let rec loop j k =
		  if k = n then (Yakker.set_position j; (j, v))
		  else if s1.[j] = s.[k] then loop (j+1) (k+1)
		  else raise Parse_fail in
		loop i 0
	      else raise Parse_fail)

let char_range lb ub (i, v) =
  if i < !input_len then
    begin
      let k = Char.code !input.[i] in
      if lb <= k && k <= ub then ( Yakker.set_position (i+1); (i+1, v) )
      else raise Parse_fail
    end
  else raise Parse_fail

let pred f_p f_r (i, v) = if (f_p i v) then (i, (f_r i v)) else raise Parse_fail

let box f_box (i, v) = match f_box v i !ykbuf with
  | Some (j, v') -> Yakker.set_position (i+j); (i+j, v')
  | None -> raise Parse_fail

let action f ( (i : int), v) = (i, f i v)

(* If lookahead succeeds, we discard its progress
   and continue with the original i and v.
*)
let pos_lookahead r st =
  ignore (r st);
  Yakker.set_position (fst st);
  st

(* If lookahead fail, we continue with the original i and v.
*)
let neg_lookahead r st =
  let failed = try r st; false with Parse_fail -> true in
  if failed then (Yakker.set_position (fst st); st) else raise Parse_fail

let seq rs st = Array.fold_left (fun st r -> r st) st rs

let alt rs ( (j, _) as st)  =
  let i_last = Array.length rs - 1 in
  let rec loop i =
    if i < i_last then
      try Array.unsafe_get rs i st
      with Parse_fail -> Yakker.set_position j; loop (i+1)
    else
      Array.unsafe_get rs i_last st
  in loop 0

(* let alt r1 r2 st  =  *)
(*   try r1 st with Parse_fail -> Yakker.set_position (fst st); r2 st *)

let star r st0 =
  let rec loop st =
    let x =
      try
	Some (r st)
      with Parse_fail -> begin
	Yakker.set_position (fst st);
	None
      end in
    match x with
      | Some ( (j, _) as st') -> if (fst st) = j then st' else loop st'
      | None -> st
  in loop st0

(** Ford's definition of star from POPL '04.
   Doesn't check for progress in r. *)
let star_strict r st0 =
  let rec loop st =
    let x =
      try
	Some (r st)
      with Parse_fail -> begin
	Yakker.set_position (fst st);
	None
      end in
    match x with
      | Some st' -> loop st'
      | None -> st
  in loop st0

let symb f_A f_call f_ret (i, v) =
  let (j, v') = f_A (i, (f_call v)) in
  j, (f_ret i v v')

(** indirect symbol reference. allows more efficient construction of recursive symbols. *)
let rsymb ref_A f_call f_ret (i, v) =
  let (j, v') = !ref_A (i, (f_call v)) in
  j, (f_ret i v v')

let define_symb f_A_body =
  let table = Hashtbl.create 11 in
  let rec f_A key =
      try
	(match Hashtbl.find table key with
	   | Some st -> st
	   | None -> raise Parse_fail
	)
      with
	| Not_found ->
	    (try
	       let st = f_A_body f_A key in
	       Hashtbl.add table key (Some st);
	       st
	     with Parse_fail -> Hashtbl.add table key None; raise Parse_fail)
  in
  f_A

module Make (P: sig type semval
		    module HT: Hashtbl.S with type key = int * semval
	     end) =
struct
  let memo_symb table f_A key =
      try
	(match P.HT.find table key with
	   | Some st -> st
	   | None -> raise Parse_fail
	)
      with
	| Not_found ->
	    (try
	       let st = f_A key in
	       P.HT.add table key (Some st);
	       st
	     with Parse_fail -> P.HT.add table key None; raise Parse_fail)
end

let parse r v = r (0, v)
let init s ykb = set_input s; set_ykbuf ykb


