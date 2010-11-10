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

(** All-match parser. Essentially depth-first Earley. *)

(* Interesting that it seems like we could quite easily add support
   for negation through a failure continuation. Although, I suppose
   that failure really means that *all* branches fail, whereas a failure
   continuation would only indicate that a single branch failed,
   unless the braches were tried sequentially.
*)

open YkBuf

let mem c cs =
  Cs.member cs (Char.code c)


type 'v k = YkBuf.t -> 'v -> unit
  (* type of continuations *)

let success_any_n results : 'v k = fun ykb v ->
  let i = get_offset ykb in
  if Logging.activated then
    begin
      Logging.log Logging.Features.combs  "Parse succeeded at position %d.\n" i;
    end;
  (* [i] is one past the last successful offset, so we decrement before saving it. *)
  results := (i-1,v) :: !results

(** only saves results which have reached EOF.
    maxp is a ref to an integer storing the maximum offset reached.
*)
let success_at_eof results : 'v k = fun ykb v ->
  (* We use the result of [fill2] as a proxy for testing is_eof, because we are only
     asking for one byte. If one byte is not available, then we must be at EOF. *)
  let at_eof = not (fill2 ykb 1) in
  let i = get_offset ykb in
  if at_eof then
    begin
      if Logging.activated then
	begin
	  Logging.log Logging.Features.combs  "Parse succeeded through EOF (position %d).\n" i;
	end;
      (* [i] is one past the last successful offset, so we decrement before saving it. *)
      results := (i-1,v) :: !results
    end
  else
    if Logging.activated then
      begin
	Logging.log Logging.Features.combs  "Parse succeeded at position %d.\n" i;
      end

(** default success continuation *)
let success = success_at_eof

(* *** PERF: don't use lists! *)
type ('r, 'k) entry = {mutable results: 'r list; mutable continuations: 'k list}
let is_new x s = not (List.mem x s)

(*

   N.B. Setting positions right is quite tricky. Current invariant is
   that on entry to a combinator, position is not guaranteed. So,
   combinators from which position might be directly checked must set the position.

   N.B. The ykBuf invariant: cursor is set to one past last consumed index.

   *** PERF: the position-setting business must be fixed. costs even more here than in earley,
   where each position is only visited once. Need actions, predicates and boxes fixed to
   stop using imperative position.

   *** PERF: should we be using fill_exp?

*)

(** Maximum offset successfully examined during parsing. *)
let max_offset = ref (-1)

let upd_max i = if i > !max_offset then max_offset := i else ()

let eps (k_s : 'v k) = k_s

let term c k_s ykb v =
  (* try to fill, and then check, the current position *)
  if fill2 ykb 1 && get_current ykb = c then
    (upd_max (get_offset ykb); advance ykb; k_s ykb v)
  else ()

let terms s =
  match String.length s with
    | 0 -> eps
    | 1 -> term s.[0]
    | n -> (fun k_s ykb v ->
	      (* try to fill complete string. *)
	      if not (fill2 ykb n) then ()
	      else
		(* re: max_offset. We set it on exit from the loop,
		   rather than at each iteration. On success or failure, prev.
		   offset will be the last one successfully examined. *)
		let rec loop k =
		  if k = n then (
		    upd_max (get_offset ykb - 1);
		    k_s ykb v)
		  else if get_current ykb = s.[k] then (advance ykb; loop (k+1))
		  else upd_max (get_offset ykb - 1) in
		(* We know n > 1, so we unroll the loop twice. *)
		if get_current ykb <> s.[0] then ()
		else
		  (advance ykb;
		   if get_current ykb <> s.[1] then upd_max (get_offset ykb - 1)
		   else
		     (advance ykb;
		      loop 2)))

let char_range lb ub k_s ykb v =
  if fill2 ykb 1 then
    let k = Char.code (get_current ykb) in
    if lb <= k && k <= ub then (
      upd_max (get_offset ykb);
      advance ykb; k_s ykb v)

let pred f_p f_r
    k_s ykb v =
  let i = get_offset ykb in
  Yakker.set_position i;
  if (f_p i v) then k_s ykb (f_r i v)

let box f_box k_s ykb v =
  let i = get_offset ykb in
  Yakker.set_position i;
  match f_box v i ykb with
    | Some (j, v') ->
	upd_max (get_offset ykb - 1);
	k_s ykb v'
    | None -> ()

let action f k_s ykb v =
  let i = get_offset ykb in
  Yakker.set_position i;
  k_s ykb (f i v)

(* If lookahead succeeds, we discard its progress and value
   and continue at the original position.
*)
let pos_lookahead r k_s ykb v =
  let cp = save ykb in
  let m = !max_offset in
  (* We need an imperative hack to deal w/ failure. This could
     probably be improved.
     ***(Could we just have the continuation return a boolean? i don't think the types work out. But what if all places where we fail we return false?)
  *)
  let succ = ref false in
  r (fun _ _ -> succ := true) ykb v;
  restore ykb cp;
  max_offset := m;
  if !succ then k_s ykb v else ()

(* If lookahead fails, we continue with the original i and v.
*)
let neg_lookahead (r: 'a k -> 'a k) k_s ykb v =
  let cp = save ykb in
  let m = !max_offset in
  (* We need an imperative hack to deal w/ failure. This could
     probably be improved.
  *)
  let succ = ref false in
  r (fun _ _ -> succ := true) ykb v;
  restore ykb cp;
  max_offset := m;
  if !succ then () else k_s ykb v

let pos_cr_lookahead lb ub k_s ykb v =
  if fill2 ykb 1 then
    let k = Char.code (get_current ykb) in
    if lb <= k && k <= ub then k_s ykb v else ()
  else ()

let neg_cr_lookahead lb ub k_s ykb v =
  if fill2 ykb 1 then
    let k = Char.code (get_current ykb) in
    if k < lb || k > ub then k_s ykb v else ()
  else k_s ykb v

(* ***PERF: sequences and alternatives should be arrays not just binary. *)
let seq r1 r2 k_s = r1 (r2 k_s)

let alt r1 r2 k_s ykb v =
  let cp = save ykb in
  r1 k_s ykb v;
  restore ykb cp;
  r2 k_s ykb v

let nseq rs k_s = Array.fold_right (fun r k_s -> r k_s) rs k_s

(** [rs] must be of length at least two. *)
let nalt rs =
  if Array.length rs < 2 then raise (Invalid_argument "nalt called with less than two branches.")
  else
    (* We know that the array is at least of length 2, so we unroll the first element.
       We also need to reset the checkpoint only in between elements, so we special case
       the last element. *)
    (fun k_s ykb v ->
       let i_last = Array.length rs - 1 in
       let cp = save ykb in
       (Array.unsafe_get rs 0) k_s ykb v;
       for i = 1 to i_last - 1 do
	 reset ykb cp;
	 (Array.unsafe_get rs i) k_s ykb v;
       done;
       restore ykb cp;
       (Array.unsafe_get rs i_last) k_s ykb v)

let nalt_w_lookahead fslist rs k_s ykb v =
  if Array.length rs < 2 then raise (Invalid_argument "nalt_w_lookahead called with less than two branches.")
  else if Array.length rs <> Array.length fslist then raise (Invalid_argument "nalt_w_lookahead called with unbalanced branch and first set list.")
  else
    if fill2 ykb 1 then
      let length = Array.length rs - 1 in
      let char_val = Char.code (get_current ykb) in
      let rec loop i =
        if i > length then ()
        else
          let fs = Array.unsafe_get fslist i in
          let r_i = Array.unsafe_get rs i in
            if Cs.member fs char_val then r_i k_s ykb v
            else loop (i+1)
      in
        loop 0
    else
      ()

let alt_w_lookahead fs1 fs2 r1 r2 k_s ykb v =
  if fill2 ykb 1 then
    let char_val = Char.code (get_current ykb) in
    if Cs.member fs1 char_val then r1 k_s ykb v
    else if Cs.member fs2 char_val then r2 k_s ykb v
    else k_s ykb v
  else
    k_s ykb v

let alt_w_lookahead0 fs1 fs2 r1 r2 k_s ykb v =
  if fill2 ykb 1 then
    let char_val = Char.code (get_current ykb) in
    if Cs.member fs1 char_val then r1 k_s ykb v
    else if Cs.member fs2 char_val then r2 k_s ykb v
    else ()
  else
    ()

let alt_w_lookahead2 fs r1 r2 k_s ykb v =
  if fill2 ykb 1 then
    let char_val = Char.code (get_current ykb) in
    if Cs.member fs char_val then r1 k_s ykb v
    else r2 k_s ykb v
  else
    r2 k_s ykb v

(*
     At a high level, star is simply:
     [let rec star r =  alt eps (seq r (fun k_s -> star r k_s))]
     Unfortunately, this representation leads to terrible performance,
     because the body of the function is not partially evaluated. So,
     we partial evaluate to get what you see below.

     ***PERF notice that choice of which branch to pursue first
     can have a tremendous performance impact. If the first branch dies
     quickly vs. it dies slowly, can lead to significantly different
     memory behaviour, because only second branch is a tail call.
     Same applies to normal alt.
*)
let star r k_s ykb v_init =
  let rec loop ykb v =
    let cp = save ykb in
    k_s ykb v; (* eps inlined *)
    restore ykb cp;
    r loop ykb v in
  loop ykb v_init

let rec star_w_lookahead fs r k_s ykb_init v_init =
  let rec loop ykb v =
    if fill2 ykb 1 then
      let char_val = Char.code (get_current ykb) in
        if Cs.member fs char_val then r loop ykb v
        else k_s ykb v
    else k_s ykb v
  in
    loop ykb_init v_init

let symb f_A f_call f_ret k_s ykb v =
  let i = get_offset ykb in
  let k_succ ykb v' = k_s ykb (f_ret i v v') in
  f_A k_succ ykb (f_call v)

(** indirect symbol reference. allows more efficient construction of recursive symbols. *)
let rsymb ref_A f_call f_ret k_s ykb v =
  let i = get_offset ykb in
  let k_succ ykb v' = k_s ykb (f_ret i v v') in
  !ref_A k_succ ykb (f_call v)

let define_symb f_A_body =
  let table = Hashtbl.create 11 in
  let rec f_A k_s ykb v =
    let i = get_offset ykb in
    let key = (i, v) in
    match (try Some(Hashtbl.find table key) with Not_found -> None) with
      | Some entry ->
	  entry.continuations <- k_s :: entry.continuations;
	  let rec process_results = function
	    | [] -> ()
	    | [(j,v')] -> advance_by ykb (j-i); k_s ykb v'
	    | (j,v')::rs ->
		let cp = save ykb in
		advance_by ykb (j-i);
		k_s ykb v';
		restore ykb cp;
		process_results rs in
	  process_results entry.results
      |	None ->
	  begin
	    let entry = {results = []; continuations = [k_s]} in
	    Hashtbl.add table key entry;
	    let k_succ ykb v' =
	      let j = get_offset ykb in
	      let r = (j, v') in
	      if is_new r entry.results then
		begin
		  entry.results <- r :: entry.results;
		  let rec process_continuations = function
		    | [] -> ()
		    | [k_s] -> k_s ykb v'
		    | k_s::ks ->
			let cp = save ykb in
			k_s ykb v';
			restore ykb cp;
			process_continuations ks in
		  process_continuations entry.continuations
		end
	    in
	    f_A_body f_A k_succ ykb v
	  end
  in
  f_A

module Make (P: sig type semval
		    module HT: Hashtbl.S with type key = int * semval
	     end) =
struct
  let memo_symb table name f_A k_s ykb v =
    let i = get_offset ykb in
    let key = (i, v) in
    match (try Some(P.HT.find table key) with Not_found -> None) with
      | Some entry ->
	  entry.continuations <- k_s :: entry.continuations;
	  let rec process_results = function
	    | [] -> ()
	    | [(j,v')] -> advance_by ykb (j-i); k_s ykb v'
	    | (j,v')::rs ->
		let cp = save ykb in
		advance_by ykb (j-i);
		k_s ykb v';
		restore ykb cp;
		process_results rs in
	  process_results entry.results
      |	None ->
	  begin
	    let entry = {results = []; continuations = [k_s]} in
	    P.HT.add table key entry;
	    let k_succ ykb v' =
	      let j = get_offset ykb in
	      let r = (j, v') in
	      if is_new r entry.results then
		begin
		  entry.results <- r :: entry.results;
		  let rec process_continuations = function
		    | [] -> ()
		    | [k_s] -> k_s ykb v'
		    | k_s::ks ->
			let cp = save ykb in
			k_s ykb v';
			restore ykb cp;
			process_continuations ks in
		  process_continuations entry.continuations
		end
	    in
	    f_A k_succ ykb v
	  end
end

let parse r ykb v =
  let results = ref [] in
  max_offset := -1;
  r (success results) ykb v;
  !results,!max_offset



(** 
    PEG parser combinators, in a continuation-passing style.
*)
module Peg = struct

(** We support two different versions of kleene-closure: the one from
    Ford's paper, in which termination only occurs on failure of the body
    of the closure, and the (apparent) one from Aurochs, which terminates
    a star if no progress is made (i.e. the underlying expression accepts
    epsilon).

*)

(** Notice that success continuations *do not* take a failure continuation argument. Practically,
this means that we can't force an input suc. continuation to return to a point of our choosing. We
can only do that for suc. continuations of our own creation. This is consistent with PEG semantics,
where backtracking is only local -- once a success has been found in a branch, the suc. continuation is
committed to, even if it later meets its premature end. *)

  let eps k_f k_s = k_s

  let term c k_f k_s ykb v =
    (* try to fill, and then check, the current position *)
    if fill2 ykb 1 && get_current ykb = c then
      (upd_max (get_offset ykb); advance ykb; k_s ykb v)
    else k_f ykb v

  let terms s =
    match String.length s with
      | 0 -> eps
      | 1 -> term s.[0]
      | n -> (fun k_f k_s ykb v ->
		(* try to fill complete string. *)
		if not (fill2 ykb n) then k_f ykb v
		else
		  (* re: max_offset. We set it on exit from the loop,
		     rather than at each iteration. On success or failure, prev.
		     offset will be the last one successfully examined. *)
		  let rec loop k =
		    if k = n then (
		      upd_max (get_offset ykb - 1);
		      k_s ykb v)
		    else if get_current ykb = s.[k] then (advance ykb; loop (k+1))
		    else (upd_max (get_offset ykb - 1); k_f ykb v) in
		  (* We know n > 1, so we unroll the loop twice. *)
		  if get_current ykb <> s.[0] then k_f ykb v
		  else
		    (advance ykb;
		     if get_current ykb <> s.[1] then (upd_max (get_offset ykb - 1); k_f ykb v)
		     else
		       (advance ykb; loop 2)))

  let char_range lb ub k_f k_s ykb v =
    if fill2 ykb 1 then
      let k = Char.code (get_current ykb) in
      if lb <= k && k <= ub then (
	upd_max (get_offset ykb);
	advance ykb; k_s ykb v)
      else k_f ykb v
    else k_f ykb v

  let pred f_p f_r
      k_f k_s ykb v =
    let i = get_offset ykb in
    Yakker.set_position i;
    if (f_p i v) then k_s ykb (f_r i v) else k_f ykb v

  let box f_box k_f k_s ykb v =
    let i = get_offset ykb in
    Yakker.set_position i;
    match f_box v i ykb with
      | Some (j, v') ->
	  upd_max (get_offset ykb - 1);
	  k_s ykb v'
      | None -> k_f ykb v

  let action f k_f k_s ykb v =
    let i = get_offset ykb in
    Yakker.set_position i;
    k_s ykb (f i v)

  (* If lookahead succeeds, we discard its progress and value
     and continue at the original position. *)
  let pos_lookahead r k_f k_s ykb v =
    let cp = save ykb in
    let m = !max_offset in
    r (fun ykb v ->
	 restore ykb cp;
	 max_offset := m;
	 k_f ykb v)
      (fun ykb v ->
	 restore ykb cp;
	 max_offset := m;
	 k_s ykb v)
      ykb v

  (* If lookahead fails, we continue with the original position and v. *)
  let neg_lookahead r k_f k_s ykb v =
    let cp = save ykb in
    let m = !max_offset in
    r (fun ykb v ->
	 restore ykb cp;
	 max_offset := m;
	 k_s ykb v)
      (fun ykb v ->
	 restore ykb cp;
	 max_offset := m;
	 k_f ykb v)
      ykb v

  let pos_cr_lookahead lb ub k_f k_s ykb v =
    if fill2 ykb 1 then
      let k = Char.code (get_current ykb) in
      if lb <= k && k <= ub then k_s ykb v else k_f ykb v
    else k_f ykb v

  let neg_cr_lookahead lb ub k_f k_s ykb v =
    if fill2 ykb 1 then
      let k = Char.code (get_current ykb) in
      if k < lb || k > ub then k_s ykb v else k_f ykb v
    else k_s ykb v

  (* ***PERF: sequences and alternatives should be arrays not just binary. *)
  let seq r1 r2 k_f k_s = r1 k_f
    (fun ykb v' -> r2 k_f k_s ykb v')

  let alt r1 r2 k_f k_s ykb v =
    let cp = save ykb in
    r1 (fun ykb v -> restore ykb cp; r2 k_f k_s ykb v) (* failure *)
      (fun ykb v -> commit ykb; k_s ykb v) (* success *)
      ykb v

  let alt_w_lookahead fs1 fs2 r1 r2 k_f k_s ykb v =
    if fill2 ykb 1 then
      let char_val = Char.code (get_current ykb) in
      if Cs.member fs1 char_val then r1 k_f k_s ykb v
      else if Cs.member fs2 char_val then r2 k_f k_s ykb v
      else k_s ykb v
    else
      k_s ykb v

  let alt_w_lookahead0 fs1 fs2 r1 r2 k_f k_s ykb v =
    if fill2 ykb 1 then
      let char_val = Char.code (get_current ykb) in
      if Cs.member fs1 char_val then r1 k_f k_s ykb v
      else if Cs.member fs2 char_val then r2 k_f k_s ykb v
      else k_f ykb v
    else
      k_f ykb v

  let alt_w_lookahead2 fs r1 r2 k_f k_s ykb v =
    if fill2 ykb 1 then
      let char_val = Char.code (get_current ykb) in
      if Cs.member fs char_val then r1 k_f k_s ykb v
      else r2 k_f k_s ykb v
    else
      r2 k_f k_s ykb v

(** Ford's definition of star from POPL '04.
    Doesn't check for progress in [r]. *)
  let star_strict r _ (*k_f*) k_s ykb v_init =
    let eps_branch cp v ykb _(*v'*) = restore ykb cp; k_s ykb v in
    let rec loop_branch ykb v =
      let cp = resave ykb in
      r (eps_branch cp v) loop_branch ykb v in
    let cp = save ykb in
    r (eps_branch cp v_init) loop_branch ykb v_init

(** Terminates when [r] is not found, or when [r] accepts epsilon. *)
  let star r _(*k_f*) k_s ykb v_init =
    let eps_branch cp v ykb _(*v'*) = restore ykb cp; k_s ykb v in
    let rec loop_branch old_cp ykb v =
      let cp = resave ykb in
      if old_cp = cp then eps_branch cp v ykb v
      else r (eps_branch cp v) (loop_branch cp) ykb v in
    let cp = save ykb in
    r (eps_branch cp v_init) (loop_branch cp) ykb v_init

  (* ***PERF optimize this combinator like we have [star]. *)
  let rec star_w_lookahead fs r k_f k_s ykb v =
    if fill2 ykb 1 then
      let char_val = Char.code (get_current ykb) in
      if Cs.member fs char_val then seq r (star_w_lookahead fs r) k_f k_s ykb v
      else k_s ykb v
    else
      k_s ykb v

  let symb f_A f_call f_ret k_f k_s ykb v =
    let i = get_offset ykb in
    let k_succ ykb v' = k_s ykb (f_ret i v v') in
    f_A k_f k_succ ykb (f_call v)

  (** indirect symbol reference. allows more efficient construction of recursive symbols. *)
  let rsymb ref_A f_call f_ret k_f k_s ykb v =
    let i = get_offset ykb in
    let k_succ ykb v' = k_s ykb (f_ret i v v') in
    !ref_A k_f k_succ ykb (f_call v)


  type 'a parse_result = Success_pr of int *  'a | Failure_pr | Unknown_pr

  module Make (P: sig type semval
		      module HT: Hashtbl.S with type key = int * semval
	       end) =
  struct
    let memo_symb table f_A k_f k_s ykb v =
      let i = get_offset ykb in
      let key = (i, v) in
      match (try P.HT.find table key with Not_found -> Unknown_pr) with
	| Unknown_pr ->
	    f_A
	      (fun ykb v' -> P.HT.add table key Failure_pr; k_f ykb v)
	      (fun ykb v' -> P.HT.add table key (Success_pr (get_offset ykb, v')); k_s ykb v')
	      ykb v
	| Failure_pr -> k_f ykb v
	| Success_pr (j,v') -> advance_by ykb (j-i); k_s ykb v'
  end

  let success ykb v = Some (get_offset ykb, v)
  let failure ykb v = None

  let parse r ykb v =
    max_offset := -1;
    r failure success ykb v, !max_offset
end

