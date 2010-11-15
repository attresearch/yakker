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

(* TODO: perhaps we can eliminate the sentinel NUL marking EOF.  It is
   not necessary but may be convenient.  We might also make it configurable,
   in case NUL is expected to be common in the input.
*)

type checkpoint = int
let ykbsize = 8192
let no_save = -1

type buffer = string

module Oldversion = struct
type producer =
      Ykp_none
    (** buffer offset n
	@return: amount generated, 0 indicates EOF. Negative return is invalid *)
    | Ykp_genfn of (buffer -> int -> int -> int)
    | Ykp_fd of Unix.file_descr

type t = {
    prod: producer;
    mutable bot: buffer;     (* buffer holding a portion of the input *)
    mutable save: int;       (* index of the first character to save on discard, or no_save=-1 *)
    mutable mark: int;
    mutable cur: int;        (* index in the buffer of the current input character *)
    mutable lim: int;        (* index just past valid portion of buffer *)
    mutable top: int;        (* index just past the end of buffer *)

    mutable eof: int;        (* -1 if EOF has not been encountered, otherwise = index of a NUL *)
    mutable save_count: int; (* number of pending saves *)
    mutable discarded: int;  (* number of characters that have been discarded from the buffer (to the left) *)
  }

(** {1 Constructors} *)

let fd2buf fd =
  let buf = String.create ykbsize in
  {prod = Ykp_fd fd; bot = buf;
   save = no_save; mark = no_save;
   cur = 0; lim = 0;
   top = ykbsize;
   eof = -1;
   save_count = 0;
   discarded = 0;
  }

let p2buf f =
  let buf = String.create ykbsize in
  {prod = Ykp_genfn f; bot = buf;
   save = no_save; mark = no_save;
   cur = 0; lim = 0;
   top = ykbsize;
   eof = -1;
   save_count = 0;
   discarded = 0;
  }

let string2buf s =
  let len = String.length s in
  let buf = String.create (len+1) in
  String.blit s 0 buf 0 len;
  buf.[len] <- '\000';
  {prod = Ykp_none; bot = buf;
   save = no_save; mark = no_save;
   cur = 0; lim = len;
   top = len + 1;
   eof = len;
   save_count = 0;
   discarded = 0;
  }

let data2buf s =
  let len = Array.length s in
  let buf = String.create (len+1) in
  for i = 0 to len-1 do
    buf.[i] <- s.(i)
  done;
  buf.[len] <- '\000';
  {prod = Ykp_none;
   bot = buf;  save = no_save;
   mark = no_save;  cur = 0;
   lim = len;    top = len + 1;
   eof = len;
   save_count = 0;
   discarded = 0;
  }

let buffer2buf b =
  let len = Buffer.length b in
  let buf = String.create (len+1) in
  for i = 0 to len-1 do
    buf.[i] <- Buffer.nth b i
  done;
  buf.[len] <- '\000';
  {prod = Ykp_none;
   bot = buf;  save = no_save;
   mark = no_save;  cur = 0;
   lim = len;    top = len + 1;
   eof = len;
   save_count = 0;
   discarded = 0;
  }

let strings2buf a start = (* convert a string array (e.g., command-line arguments) to a ykbuf with NUL
                             characters separating the arguments.  start allows starting at index <> 0 *)
  let b = Buffer.create 11 in
  let howmany = Array.length a - start in
  for i = start to start+howmany-1 do
    Buffer.add_string b a.(i);
    Buffer.add_string b "\000"
  done;
  buffer2buf b
let stringsposn2string a start i = (* return the a.(n) containing position i in strings2buf(a),
                                for error reporting on a parse failure *)
  let rec loop n i =
    let len = String.length a.(n) in
    if i <= 0 or i <= len then n else
    loop (n+1) (i-len-1) in
  loop start (i-1)

(** {1 Accessors} *)

let is_empty ykb = ykb.lim - ykb.cur <= 0

let is_eof ykb =
    ykb.cur = ykb.eof && ykb.bot.[ykb.cur] = '\000'
(*   with (Invalid_argument s) as e ->  *)
(*     prerr_endline ("is_eof: Invalid argument: " ^ s);  *)
(*     raise e *)

let get ykb i = ykb.bot.[i]

(** Return the character at the current input position.  It is the
    user's to make sure that the current input position has a valid
    character.
*)
let get_current ykb = ykb.bot.[ykb.cur]

(** Advance the cursor. *)
let advance ykb = ykb.cur <- ykb.cur + 1

(** Advance the cursor. *)
let advance_by ykb n = ykb.cur <- ykb.cur + n

(** Advance the cursor to last valid byte.*)
let advance_to_last ykb = ykb.cur <- ykb.lim - 1

(** Step back the cursor *)
let step_back ykb = ykb.cur <- ykb.cur - 1

(** Step back the cursor *)
let step_back_by ykb n = ykb.cur <- ykb.cur - n

(** Advance to eof, if it has been reached. Otherwise, do nothing.
    @return boolean indicating whether [eof] has been reached.
*)
let advance_to_eof ykb =
  if ykb.eof <> -1 then ykb.cur <- ykb.eof;
  (ykb.eof <> -1)


  (** Advance the input position and return the character at that position.
      It is the user's responsibility to call fill() and check EOF to make
      sure that the next input position has a valid character. *)
let get_next ykb =
  ykb.cur <- ykb.cur + 1;
  ykb.bot.[ykb.cur]

let peek ykb =
  let cur = ykb.cur + 1 in
  let c = ykb.bot.[cur] in
  if c = '\000' && ykb.cur = ykb.eof then -1
  else int_of_char c

  (** Mark the current input position as significant. Only one mark is
      supported per buffer.

      User is responsible for ensuring that save is already set
      before calling this function. *)
let mark ykb = ykb.mark <- ykb.cur

(** Get the offset in the input stream. *)
let get_offset ykb = ykb.cur + ykb.discarded

(** Set the cursor to correspond to the given offset in the input stream. *)
let set_offset ykb off = ykb.cur <- off - ykb.discarded

  (** Get the size of the input stream, if eof has been reached.. *)
let get_input_size ykb =
  if ykb.eof = -1 then -1 else ykb.eof + ykb.discarded

  (** Remember the current position in the stream and save all following bytes until
      the save is undone.
      @return An abstract [checkpoint] referring to the position at which [save] was called. *)
let save ykb =
  ykb.save_count <- ykb.save_count + 1;
  if not (ykb.save = no_save) then
    ykb.cur - ykb.save
  else (ykb.save <- ykb.cur; 0)

(** Combine a commit followed immediately by a save. User must ensure that at least one
    checkpoint exists. If none exists, then no checkpoint will result from this call.
    @return An abstract [checkpoint] referring to the position at which [resave] was called. *)
let resave ykb =
  if ykb.save_count <> 1 then
    ykb.cur - ykb.save
  else
    (ykb.save <- ykb.cur; 0)

  (** Restore the buffer to the checkpointed cursor position and clear the checkpoint. *)
let restore ykb x =
  ykb.cur <- ykb.save + x;
  ykb.save_count <- ykb.save_count - 1;
  if ykb.save_count = 0 then ykb.save <- no_save

  (** Reset the buffer to the checkpointed cursor position. Do not clear the checkpoint. *)
let reset ykb x =  ykb.cur <- ykb.save + x

  (** Commit the buffer to the current cursor position. *)
let commit ykb =
  ykb.save_count <- ykb.save_count - 1;
  if ykb.save_count = 0 then ykb.save <- no_save

  (** Commit the buffer to the mark position. *)
let commit_mark ykb =
  commit ykb;
  ykb.cur <- ykb.mark


(*****************************************************************)
(** Internal functions *)
(*****************************************************************)

(* Read n characters of input from the producer and put it in the buffer
   starting at the offset.  Return the number of characters actually read,
   with 0 indicating EOF.  NB can return <0 if a function producer does this.
 *)
let internal_read p b off n =
  match p with
      Ykp_none -> 0
    | Ykp_fd fd -> Unix.read fd b off n
    | Ykp_genfn f -> f b off n

(* Discard characters from the buffer (so that we can read more in). *)
(* Don't discard characters marked for saving. *)
(** @invariant: save <= cur *)
let discard ykb =
  if ykb.save > 0 then
    begin
      let count = ykb.save in
      let len = ykb.lim - count in
      (* shift [len] bytes from ykb.bot[count] to ykb.bot[0] *)
      String.blit ykb.bot count ykb.bot 0 len;
      (* Adjust indexes *)
      ykb.save <- 0;
      if ykb.mark <> no_save then ykb.mark <- ykb.mark - count;
      ykb.cur <- ykb.cur - count;
      ykb.lim <- ykb.lim - count;
      ykb.discarded <- ykb.discarded + count
    end
  else if ykb.save = no_save && ykb.cur > 0 then
    begin
      let count = ykb.cur in
      let len = ykb.lim - count in
      (* shift [len] bytes from ykb.bot[count] to ykb.bot[0] *)
      String.blit ykb.bot count ykb.bot 0 len;
      (* Adjust indexes. Note: by invariant on mark, we can assume it
	 unset because save is unset.*)
      ykb.cur <- 0;
      ykb.lim <- ykb.lim - count;
      ykb.discarded <- ykb.discarded + count
    end

(* make sure that the buffer has sufficient space to read [to_read] of characters.
   uses exponential resizing of buffer, if insufficient space is available.
*)
let exp_reserve ykb to_read =
  if ykb.top - ykb.lim < to_read then
    discard ykb;
  if ykb.top - ykb.lim < to_read then
    begin
      let size = 2 * (String.length ykb.bot) in
      let buf = String.create size in
      String.blit ykb.bot 0 buf 0 ykb.lim;
      ykb.top <- size;
      ykb.bot <- buf
    end

(* make sure that the buffer has sufficient space to read some number of characters *)
let reserve ykb to_read =
  if ykb.top - ykb.lim < to_read then
    discard ykb;
  if ykb.top - ykb.lim < to_read then
    begin
      let size = ykb.lim + to_read in
      let buf = String.create size in
      String.blit ykb.bot 0 buf 0 ykb.lim;
      ykb.top <- size;
      ykb.bot <- buf
    end

(*****************************************************************)
(** External functions *)
(*****************************************************************)

(** [fill2 ykb n] ensures that at least [n] bytes of buffered data
    are available starting at [ykb.cur], or that EOF has been read.
    return indicates whether n bytes are available. If they are not available,
    then EOF was reached before n bytes could be read.
*)
let fill2 ykb n =
  let available = ykb.lim - ykb.cur in
  if n <= available || ykb.eof >= 0 then (n <= available)
  else
    let to_read = max (n - available) ykbsize in
    reserve ykb to_read;

    let rec do_read m =
      (* A positive continue value means more data needs to be read.
	 -1 means sufficient data was read.
	 -2 means eof was encountered before sufficient data was read.
      *)
      let continue = try
	  let cnt = internal_read ykb.prod ykb.bot ykb.lim m in
	  if cnt > 0 then
	    begin
	      ykb.lim <- ykb.lim + cnt;
	      if n <= ykb.lim - ykb.cur then
		(-1) (* enough, don't care if m > n. *)
	      else
		(m - cnt)
	    end
	  else if cnt = 0 then
	    begin
	      ykb.eof <- ykb.lim;
(* 	      Printf.eprintf "Eof reached (%d).\n" ykb.lim; *)
	      ykb.bot.[ykb.lim] <- '\000';
	      (-2)
	    end
	  else
            failwith "Internal read returned negative result."
	with
	    Unix.Unix_error (Unix.EINTR,_,_) -> m
	  | Unix.Unix_error (Unix.EAGAIN,_,_) -> m
      in
      match continue with
	| -1 -> true
	| -2 -> false
	| m -> do_read m
    in
      do_read to_read

(** [fill ykb n] ensures that at least [n] bytes of buffered data
    are available starting at [ykb.cur], or that EOF has been read. *)
let fill ykb n = ignore (fill2 ykb n)


(** Ensure that at least n bytes of buffered data are available
    starting at [ykb.cur], or that EOF has been read.  Reserve space in
    the buffer using exponential resizing.
*)
let fill_exp ykb n =
  let available = ykb.lim - ykb.cur in
  if ykb.eof>=0 || n <= available then ()
  else
    let to_read = max (n - available) ykbsize in
    exp_reserve ykb to_read;

    let rec do_read m =
      let continue = try
	let cnt = internal_read ykb.prod ykb.bot ykb.lim m in
	if cnt > 0 then
	  begin
	    ykb.lim <- ykb.lim + cnt;
	    if n <= ykb.lim - ykb.cur then
	      None (* enough, don't care if m > n. *)
	      else
		Some (m - cnt)
	    end
	  else if cnt = 0 then
	    begin
	      ykb.eof <- ykb.lim;
	      ykb.bot.[ykb.lim] <- '\000';
	      None
	    end
	  else None
	with
	    Unix.Unix_error (Unix.EINTR,_,_) -> Some m
	  | Unix.Unix_error (Unix.EAGAIN,_,_) -> Some m
      in
      match continue with
	  Some m -> do_read m
	| None -> ()
    in
      do_read to_read

(**
    Read as many bytes as necessary to arrive at EOF.
*)
let skip_to_eof ykb =
  let rec loop () =
    if ykb.eof >= 0 then ykb.cur <- ykb.eof
    else (ykb.cur <- ykb.lim; fill_exp ykb ykbsize; loop ()) in
  loop ()


(* let fill_to_eof ykb = *)
(*   if ykb.eof>=0 then () *)
(*   else *)
(*     let to_read = ykbsize in *)
(*     exp_reserve ykb to_read; *)

(*     let rec read_loop () = *)
(*       try *)
(* 	internal_read ykb.prod ykb.bot ykb.lim to_read  *)
(*       with *)
(* 	  Unix.Unix_error (Unix.EINTR,_,_)  *)
(* 	| Unix.Unix_error (Unix.EAGAIN,_,_) -> read_loop () in *)
(*     let rec loop () =  *)
(*       let cnt = read_loop () in *)
(*       if cnt > 0 then (ykb.lim <- ykb.lim + cnt; loop ()) *)
(*       else if cnt = 0 then (ykb.eof <- ykb.lim; ykb.bot.[ykb.lim] <- '\000') *)
(*       else raise Failure in *)
(*     loop () *)

  (** [skip n] skip [n] bytes, or until eof; whichever comes first.
      @return number of bytes skipped. *)
let skip ykb n =
  if n < 0 then 0
  else begin
    fill ykb n;
    let available = ykb.lim - ykb.cur in
    let to_skip = min n available in
    ykb.cur <- ykb.cur + to_skip;
    to_skip
  end

(**
    Snapshot of a portion of a buffer. Behaves like the string type.
    Snapshots are created through the [snapshot] function, below.
*)
module Snapshot = struct
  type t = {basebuf: string; start:int; length: int}

  let sub sns i n =
    if n < 0 then invalid_arg "Snapshot.sub: negative length." 
    else if i + n > sns.length then 
      invalid_arg (Printf.sprintf "Snapshot.sub: invalid substring: %d+%d/%d." i n sns.length) 
    else
      String.sub sns.basebuf (sns.start+i) n

  let length sns = sns.length

  let to_string sns = String.sub sns.basebuf sns.start sns.length
end

  (** Create a snapshot of the given buffer starting from a particular
      checkpoint. Current end of stream will become the end of the snapshot,
      unless the end of the stream is EOF, in which case the snapshot will end
      one before the current end of stream. *)
let snapshot ykb cp =
  let start = ykb.save + cp in
  let length = if is_eof ykb then ykb.cur - start else ykb.cur - start + 1 in
  {Snapshot.basebuf=ykb.bot;
   (* Link to the buffer directly because the
      ykbuf is liable to change at any point. *)
   start=start;
   length=length}

  (** [extract_string ykb cp offset len] directly extracts a substring from the
      given buffer. *)
let extract_string ykb cp off length =
  let start = ykb.save + cp + off in
  String.sub ykb.bot start length
(*   try *)
(*     String.sub ykb.bot start length *)
(*   with e ->  *)
(*     Printf.eprintf "String.sub failure: save=%d cp=%d off=%d; length=%d\n" ykb.save cp off length;  *)
(*     raise e *)

let extract_char ykb cp off =
  let start = ykb.save + cp + off in
  ykb.bot.[start]

let get_internal_buffer ykb = ykb.bot

(* Interface to ocamllex *)
open Lexing
let lexbuf_fill ykb lb =
  let empty_space = (if ykb.save>=0 then ykb.save else ykb.cur)+(ykb.top-ykb.lim) in
  let fill_amount = max 512 empty_space in (* 512 chosen to be the same as Lexing.from_function *)
  let before = ykb.discarded in
  fill ykb fill_amount;
  lb.lex_buffer <- ykb.bot;
  lb.lex_buffer_len <- ykb.lim;
  lb.lex_eof_reached <- (ykb.eof>=0);
  let after = ykb.discarded in
  let s = after-before in
  if s>0 then begin
    (* from Lexing.lex_refill *)
    lb.lex_abs_pos <- lb.lex_abs_pos + s;
    lb.lex_curr_pos <- lb.lex_curr_pos - s;
    lb.lex_start_pos <- 0; (* sanity check: lex_start_pos = s *)
    lb.lex_last_pos <- lb.lex_last_pos - s;
    let t = lb.lex_mem in
    for i = 0 to Array.length t-1 do
      let v = t.(i) in
      if v >= 0 then
        t.(i) <- v-s
    done
  end

let zero_pos = {
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}

let to_lexbuf ykb =
  { refill_buff = lexbuf_fill ykb;
    lex_buffer = ykb.bot;
    lex_buffer_len = ykb.lim;
    lex_abs_pos = ykb.discarded;
    lex_start_pos = (if ykb.save>=0 then ykb.save else ykb.cur);
    lex_curr_pos = ykb.cur;
    lex_last_pos = (if ykb.save>=0 then ykb.save else ykb.cur);
    lex_last_action = -1;
    lex_eof_reached = (ykb.eof>=0);
    lex_mem = [||];
    lex_start_p = zero_pos;
    lex_curr_p = zero_pos;
  }

(* If f is an ocamllex entry point, then (wrap_ocamllex f) is suitable as a yakker @box *)
let wrap_ocamllex f start ykb =
  let lexbuf = to_lexbuf ykb in
  let result = try
    let left = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos in
    if left > 8000 then
      Printf.eprintf "Looking for position %d in table.\n%!" left;
    let result = f lexbuf in
    let right  = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos in
    let consumed = right - left in
    advance_by ykb consumed;
    if left > 8000 then
      Printf.eprintf "Token consumed: %d (%d - %d).\n%!" consumed
	lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos;
    Some(consumed,result)
  with _ -> None in
  let cpos = lexbuf.Lexing.lex_curr_pos + lexbuf.Lexing.lex_abs_pos in
  if cpos > 8000 then
    Printf.eprintf "Adding position %d to table.\n%!" cpos;
  result

(* NB Yitzhak says that this should be used for lookahead only b/c some engines
   do not permit nullable boxes *)
let peek_ocamllex f ykb =
  try
    let lexbuf = to_lexbuf ykb in
    let result = f lexbuf in
    Some(0,result)
  with _ -> None

(*
(* quick-and-dirty memoization for tokenizer functions *)
(* does not make much of an improvement *)
let wrap_ocamllex f =
  let memo_state = ref None in
  fun start ykb ->
    let compute() =
      try
        let lexbuf = to_lexbuf ykb in
        let left   = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos in
        let result = f lexbuf in
        let right  = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos in
        let consumed = right - left in
        Some(consumed,result)
      with _ -> None in
    match !memo_state with
    | Some(start',ykb',ans) ->
        if start=start' && ykb==ykb' then ans
        else
          let ans' = compute() in
          memo_state := Some(start,ykb,ans');
          ans'
    | None ->
        let ans = compute() in
        memo_state := Some(start,ykb,ans);
        ans

*)

end

(** 
    Reimplementation of YkBuf, using lexbuf data structure.
*)
module Newversion = struct

type producer =
      Ykp_none
    (** buffer offset n
	@return: amount generated, 0 indicates EOF. Negative return is invalid *)
    | Ykp_genfn of (buffer -> int -> int -> int)
    | Ykp_fd of Unix.file_descr
    | Ykp_channel of in_channel


(* 
TODO: turn this into documentation. the lexbuf field names are attrocious.

prod ~ refill_buff
bot <- lex_buffer
lim <- lex_buffer_len
top <- String.length lex_buffer  // top is redundant
discarded <- lex_abs_pos
cur <- lex_curr_pos
eof <- if lex_eof_reached then lex_buffer_len else -1

save ~ lex_start_pos,  ~ save when >-1, or cur otherwise

lex_mem ??? can't figure out what this is for.

TODO: consider changing producer to be a function.

*)

type linenum_map = (int, Lexing.position) Hashtbl.t

type t = {
    prod: producer;
    buf : Lexing.lexbuf;
(*     mutable linenum_base : int; *)
(*     mutable linenum_free : int; *)
(*       (\** Number of unneeded entries at the beginning of the array. *\) *)

    mutable linenum_map : linenum_map;

    mutable save: int;       (* index of the first character to save on discard, or no_save=-1 *)
    mutable save_count: int; (* number of pending saves *)
  }

(* TODO: 

   Change the linenum_map data structure to explicitly store linenumbers
   rather than implicitly as the array index. That way, there won't be
   any holes in the array and binary searching will be possible. Since
   both linenumbers and positions are integers, can store them unboxed in
   the array.

*)

(* let lm_discard ykb =  *)
(*   let free = ykb.linenum_free in *)
(*   if free > 0 then begin *)
(*     let map = ykb.linenum_map in *)
(*     let n = Array.length map - free in *)
(*     Array.blit map free map 0 n; *)
(*     ykb.lm_free <- 0; *)
(*     ykb.lm_base <- ykb.lm_base + free; *)
(*   end *)

(* (\** Add a new mapping from linenumber to position.  *)
(*     invocation: [lm_add map base lnum pos], where base  *)
(*     is the lowest linenum saved in the table.  *)

(*     Note that [lnum >= ykb.linenum_base] must hold. *)
(* *\) *)
(* let lm_add ykb lnum pos =  *)
(*   let base = ykb.linenum_base in *)
(*   let map = ykb.linenum_map in *)
(*   let lnum_rel = lnum - base in *)
(*   assert (lnum_rel >= 0); *)
(*   if lnum_rel < Array.length map then *)
(*     map.(lnum_rel) <- pos *)
(*   else begin *)
(*     lm_discard ykb; *)
(*     let base = ykb.linenum_base in *)
(*     let lnum_rel = lnum - base in *)
(*     if lnum_rel < Array.length map then *)
(*       map.(lnum_rel) <- pos *)
(*     else *)
(*       let n = max (lnum_rel + 1) (Array.length map * 2) in *)
(*       let map2 = Util.array_realloc map len (-1) in *)
(*       ykb.linenum_map <- map2; *)
(*       map2.(lnum_rel) <- pos *)
(*   end *)

(* (\** Perform an inverse lookup, from a position to its nearest *)
(*     linenumber and beginning-of-line position. *)

(*     @raise [Not_found] if not in the table. *\) *)
(* let lm_find_line ykb pos = *)
(*   let map = ykb.linenum_map in *)
(*   match Util.array_binary_search_greatest map pos with *)
(*     | None -> raise Not_found *)
(*     | Some i -> ykb.linenum_base + i *)

(* let lm_find_line_index ykb pos = *)
(*   let rec loop map max pos last_good current = *)
(*     if current = max then last_good  *)
(*     else if map.(current) > pos then last_good *)
(*     else loop map max pos current *)
(*   Util.array_binary_search_greatest map pos *)
(*   let map = ykb.linenum_map in *)

(* (\** Free all line numbers before the current position. *\) *)
(* let lm_free_to_curr ykb = *)
(*   let cpos = ykb.buf.lex_curr_pos in *)
(*   match lm_find_line_index ykb cpos with *)
(*       None -> () *)
(*     | Some i -> ykb.linenum_free <- max i ykb.linenum_free *)

let zero_pos = {Lexing.
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}

type lexbuf = Lexing.lexbuf = {
  refill_buff : lexbuf -> unit;
  mutable lex_buffer : string;
  mutable lex_buffer_len : int;
  mutable lex_abs_pos : int;
  mutable lex_start_pos : int;
  mutable lex_curr_pos : int;
  mutable lex_last_pos : int;
  mutable lex_last_action : int;
  mutable lex_eof_reached : bool;
  mutable lex_mem : int array;
  mutable lex_start_p : Lexing.position;
  mutable lex_curr_p : Lexing.position;
}

(** {1 Accessors} *)

let is_empty {buf=b} = b.lex_buffer_len - b.lex_curr_pos <= 0

let is_eof {buf=b} = b.lex_eof_reached && b.lex_curr_pos = b.lex_buffer_len

let get {buf=b} i = b.lex_buffer.[i]

(** Return the character at the current input position.  It is the
    user's responsibility to make sure that the current input position 
    has a valid character. *)
let get_current {buf=b} = b.lex_buffer.[b.lex_curr_pos]

(** Advance the cursor. *)
let advance {buf=b} = 
  b.lex_curr_pos <- b.lex_curr_pos + 1

(** Advance the cursor. *)
let advance_by {buf=b} n = 
  b.lex_curr_pos <- b.lex_curr_pos + n

(** Advance the cursor to last valid byte.*)
let advance_to_last {buf=b} = 
  b.lex_curr_pos <- b.lex_buffer_len - 1

(** Step back the cursor *)
let step_back {buf=b} = 
  b.lex_curr_pos <- b.lex_curr_pos - 1

(** Step back the cursor *)
let step_back_by {buf=b} n = 
  b.lex_curr_pos <- b.lex_curr_pos - n

(** Advance to eof, if it has been reached. Otherwise, do nothing.
    @return boolean indicating whether [eof] has been reached.
*)
let advance_to_eof {buf=b} =
  if b.lex_eof_reached then begin
    b.lex_curr_pos <- b.lex_buffer_len; 
    true;
  end
  else false

(** Advance the input position and return the character at that position.
    It is the user's responsibility to call fill() and check EOF to make
    sure that the next input position has a valid character. *)
let get_next {buf=b} =
  b.lex_curr_pos <- b.lex_curr_pos + 1;
  b.lex_buffer.[b.lex_curr_pos]

let peek {buf=b} =
  if b.lex_eof_reached && b.lex_curr_pos >= b.lex_buffer_len then -1
  else
    let cur = b.lex_curr_pos + 1 in
    let c = b.lex_buffer.[cur] in
    int_of_char c

(** Get the offset in the input stream. *)
let get_offset {buf=b} = b.lex_curr_pos + b.lex_abs_pos

(** Set the cursor to correspond to the given offset in the input stream. 
    NB Does not update lex_curr_p field. *)
let set_offset {buf=b} off = b.lex_curr_pos <- off - b.lex_abs_pos

(** Get the size of the input stream, if eof has been reached.. *)
let get_input_size {buf=b} =
  if b.lex_eof_reached then b.lex_buffer_len + b.lex_abs_pos else -1
    
(** Remember the current position in the stream and save all following bytes until
    the save is undone.
    @return An abstract [checkpoint] referring to the position at which [save] was called. *)
let save ykb : checkpoint =
  let b = ykb.buf in
  ykb.save_count <- ykb.save_count + 1;
  if ykb.save <> no_save then b.lex_curr_pos - ykb.save
  else (ykb.save <- b.lex_curr_pos; 0)

(** Combine a commit followed immediately by a save. User must ensure that at least one
    checkpoint exists. If none exists, then no checkpoint will result from this call.
    @return An abstract [checkpoint] referring to the position at which [resave] was called. *)
let resave ykb =
  let b = ykb.buf in
  if ykb.save_count <> 1 then
    b.lex_curr_pos - ykb.save
  else
    (ykb.save <- b.lex_curr_pos; 0)

(** Restore the buffer to the checkpointed cursor position and clear the checkpoint. *)
let restore ykb x =
  let b = ykb.buf in
  b.lex_curr_pos <- ykb.save + x;
  ykb.save_count <- ykb.save_count - 1;
  if ykb.save_count = 0 then ykb.save <- no_save

(** Reset the buffer to the checkpointed cursor position. Do not clear the checkpoint. *)
let reset ykb x = ykb.buf.lex_curr_pos <- ykb.save + x
 
(** Commit the buffer to the current cursor position. *)
let commit ykb =
  ykb.save_count <- ykb.save_count - 1;
  if ykb.save_count = 0 then ykb.save <- no_save

(*****************************************************************)
(** Internal functions *)
(*****************************************************************)

(* Read n characters of input from the producer and put it in the buffer
   starting at the offset.  Return the number of characters actually read,
   with 0 indicating EOF.  NB can return <0 if a function producer does this.
 *)
let internal_read p b off n =
  match p with
      Ykp_none -> 0
    | Ykp_fd fd -> Unix.read fd b off n
    | Ykp_channel ic -> input ic b off n
    | Ykp_genfn f -> f b off n

(* Discard characters from the buffer (so that we can read more in). *)
(* Don't discard characters marked for saving. *)
(** @invariant: save <= cur *)
let discard ({buf=b} as ykb) =
  if ykb.save > 0 then
    begin
      let count = ykb.save in
      let len = b.lex_buffer_len - count in
      (* shift [len] bytes from b.lex_buffer[count] to b.lex_buffer[0] *)
      String.blit b.lex_buffer count b.lex_buffer 0 len;
      (* Adjust indexes *)
      ykb.save <- 0;
      b.lex_curr_pos <- b.lex_curr_pos - count;
      b.lex_buffer_len <- b.lex_buffer_len - count;
      b.lex_abs_pos <- b.lex_abs_pos + count
    end
  else if ykb.save = no_save && b.lex_curr_pos > 0 then
    begin
      let count = b.lex_curr_pos in
      let len = b.lex_buffer_len - count in
      (* shift [len] bytes from b.lex_buffer[count] to b.lex_buffer[0] *)
      String.blit b.lex_buffer count b.lex_buffer 0 len;
      (* Adjust indexes. Note: by invariant on mark, we can assume it
	 unset because save is unset.*)
      b.lex_curr_pos <- 0;
      b.lex_buffer_len <- b.lex_buffer_len - count;
      b.lex_abs_pos <- b.lex_abs_pos + count
    end

(* make sure that the buffer has sufficient space to read [to_read] of characters.
   uses exponential resizing of buffer, if insufficient space is available.
*)
let exp_reserve ykb to_read =
  let b = ykb.buf in
  let top = String.length b.lex_buffer in
  if top - b.lex_buffer_len < to_read then
    discard ykb;
  if top - b.lex_buffer_len < to_read then
    begin
      let size = 2 * top in
      let buf = String.create size in
      String.blit b.lex_buffer 0 buf 0 b.lex_buffer_len;
      b.lex_buffer <- buf
    end

(* make sure that the buffer has sufficient space to read some number of characters *)
let reserve ykb to_read =
  let b = ykb.buf in
  let top = String.length b.lex_buffer in
  if top - b.lex_buffer_len < to_read then
    discard ykb;
  if top - b.lex_buffer_len < to_read then
    begin
      let size = b.lex_buffer_len + to_read in
      let buf = String.create size in
      String.blit b.lex_buffer 0 buf 0 b.lex_buffer_len;
      b.lex_buffer <- buf
    end

(*****************************************************************)
(** External functions *)
(*****************************************************************)

(** Forces buffer to add between [n] and [to_read] bytes, or up to EOF. *)
let force_fill ykb n read_size =
  let b = ykb.buf in
  reserve ykb read_size;
  let rec do_read n m =
    (* A positive continue value means more data needs to be read.
       -1 means sufficient data was read.
       -2 means eof was encountered before sufficient data was read.
    *)
    let continue = try
      let cnt = internal_read ykb.prod b.lex_buffer b.lex_buffer_len m in
      if cnt > 0 then
	begin
	  b.lex_buffer_len <- b.lex_buffer_len + cnt;
	  if n <= b.lex_buffer_len - b.lex_curr_pos then
	    (-1) (* enough, don't care if m > n. *)
	  else
	    (m - cnt)
	end
      else if cnt = 0 then
	begin
	  b.lex_eof_reached <- true;
	  (* 	      Printf.eprintf "Eof reached (%d).\n" b.lex_buffer_len; *)
	  (-2)
	end
      else
        failwith "Internal read returned negative result."
    with
	Unix.Unix_error (Unix.EINTR,_,_) -> m
      | Unix.Unix_error (Unix.EAGAIN,_,_) -> m
    in
    match continue with
      | -1 -> true
      | -2 -> false
      | m -> do_read n m
  in
  do_read n read_size

(** [fill2 ykb n] ensures that at least [n] bytes of buffered data
    are available starting at [ykb.cur], or that EOF has been read.
    return indicates whether n bytes are available. If they are not available,
    then EOF was reached before n bytes could be read.
*)
let fill2 ykb n =
  let b = ykb.buf in
  let available = b.lex_buffer_len - b.lex_curr_pos in
(*   Printf.eprintf "Available : %d\n%!" available; *)
  if n <= available || b.lex_eof_reached then (n <= available)
  else force_fill ykb n (max (n - available) ykbsize)

(** [fill ykb n] ensures that at least [n] bytes of buffered data
    are available starting at [b.lex_curr_pos], or that EOF has been read. *)
let fill ykb n = ignore (fill2 ykb n)


(** Ensure that at least n bytes of buffered data are available
    starting at [b.lex_curr_pos], or that EOF has been read.  Reserve space in
    the buffer using exponential resizing.
*)
let fill_exp ykb n =
  let b = ykb.buf in
  let available = b.lex_buffer_len - b.lex_curr_pos in
  if b.lex_eof_reached || n <= available then ()
  else
    let to_read = max (n - available) ykbsize in
    exp_reserve ykb to_read;

    let rec do_read m =
      let continue = try
	let cnt = internal_read ykb.prod b.lex_buffer b.lex_buffer_len m in
	if cnt > 0 then
	  begin
	    b.lex_buffer_len <- b.lex_buffer_len + cnt;
	    if n <= b.lex_buffer_len - b.lex_curr_pos then
	      None (* enough, don't care if m > n. *)
	      else
		Some (m - cnt)
	    end
	  else if cnt = 0 then
	    begin
	      b.lex_eof_reached <- true;
	      None
	    end
	  else None
	with
	    Unix.Unix_error (Unix.EINTR,_,_) -> Some m
	  | Unix.Unix_error (Unix.EAGAIN,_,_) -> Some m
      in
      match continue with
	  Some m -> do_read m
	| None -> ()
    in
      do_read to_read

(**
    Read as many bytes as necessary to arrive at EOF.
*)
let skip_to_eof ykb =
  let b = ykb.buf in
  b.lex_curr_pos <- b.lex_buffer_len;
  while not b.lex_eof_reached do
    fill_exp ykb ykbsize;
    b.lex_curr_pos <- b.lex_buffer_len;
  done

(** [skip n] skip [n] bytes, or until eof; whichever comes first.
    @return number of bytes skipped. *)
let skip ykb n =
  if n < 0 then 0
  else begin
    let b = ykb.buf in
    fill ykb n;
    let available = b.lex_buffer_len - b.lex_curr_pos in
    let to_skip = min n available in
    b.lex_curr_pos <- b.lex_curr_pos + to_skip;
    to_skip
  end

(**
    Snapshot of a portion of a buffer. Behaves like the string type.
    Snapshots are created through the [snapshot] function, below.
*)
module Snapshot = struct
  type t = {basebuf: string; start:int; length: int}

  (** Create a snapshot of the given buffer starting from a particular
      checkpoint. Current end of stream will become the end of the snapshot,
      unless the end of the stream is EOF, in which case the snapshot will end
      one before the current end of stream. *)
  let create ykb x =
    let b = ykb.buf in
    let start = ykb.save + x in
    let length = if is_eof ykb then b.lex_curr_pos - start else b.lex_curr_pos - start + 1 in
    {basebuf=b.lex_buffer;
     (* Link to the buffer directly because the
	ykbuf is liable to change at any point. *)
     start=start;
     length=length}

  let sub sns i n =
    if n < 0 then invalid_arg "Snapshot.sub: negative length." 
    else if i + n > sns.length then 
      invalid_arg (Printf.sprintf "Snapshot.sub: invalid substring: %d+%d/%d." i n sns.length) 
    else
      String.sub sns.basebuf (sns.start+i) n

  let length sns = sns.length

  let to_string sns = String.sub sns.basebuf sns.start sns.length
end

(** see Snapshot.create *)
let snapshot = Snapshot.create

  (** [extract_string ykb cp offset len] directly extracts a substring from the
      given buffer. *)
let extract_string ykb cp off length =
  let start = ykb.save + cp + off in
  String.sub ykb.buf.lex_buffer start length

let extract_char ykb cp off =
  let start = ykb.save + cp + off in
  ykb.buf.lex_buffer.[start]

let get_internal_buffer ykb = ykb.buf.lex_buffer

(** Return a lexbuf ready for use by ocamllex. *)
let to_lexbuf ykb = 
  let b = ykb.buf in
  let cpos = b.lex_curr_pos + b.lex_abs_pos in
(*   Printf.eprintf "Looking for position %d in table.\n%!" cpos; *)
  (try 
     let lcp = Hashtbl.find ykb.linenum_map cpos in
     b.lex_curr_p <- lcp;
(*      Printf.eprintf "Found: line %d, character %d\n%!" *)
(*        lcp.Lexing.pos_lnum (lcp.Lexing.pos_cnum - lcp.Lexing.pos_bol); *)
  with _ -> ());
  b

(** "Debrief" a lexbuf used by ocamllex. Update ykbuf as needed to
    account for modifications to lexbuf by ocamllex.*)
let from_lexbuf ykb b = 
  let cpos = b.lex_curr_pos + b.lex_abs_pos in
  let lcp = b.lex_curr_p in
(*   Printf.eprintf "Adding position %d to table. " cpos; *)
(*   Printf.eprintf "Line %d, character %d\n%!" *)
(*     lcp.Lexing.pos_lnum (lcp.Lexing.pos_cnum - lcp.Lexing.pos_bol); *)
  Hashtbl.replace ykb.linenum_map cpos lcp

(** If f is an ocamllex entry point, then (wrap_ocamllex f) is
    suitable as a yakker @box *)
let wrap_ocamllex f start ykb =
  let lexbuf = to_lexbuf ykb in
  let result = 
    try
      let left = lexbuf.lex_curr_pos + lexbuf.lex_abs_pos in
      let result = f lexbuf in
      let right = lexbuf.lex_curr_pos + lexbuf.lex_abs_pos in
      let consumed = right - left in
(*       Printf.eprintf "Token consumed: %d (%d - %d).\n%!" consumed *)
(* 	lexbuf.lex_start_pos lexbuf.lex_curr_pos; *)
      Some(consumed, result)
    with _ -> None in
  from_lexbuf ykb lexbuf;
  result

let peek_ocamllex f ykb =
  let cp = save ykb in
  let lexbuf = to_lexbuf ykb in
  let result = 
    try
      Some(0, f lexbuf)
    with _ -> None in
  restore ykb cp;
  result

(** {1 Constructors} *)

let dummy_fill _ = ()

(** ignores lexbuf argument, using lexbuf in ykbuf instead. *)
let lexbuf_fill ykb lb =
  assert (ykb.buf == lb);
  let lb = ykb.buf in
  let before = lb.lex_abs_pos in
  fill ykb 512; (* 512 chosen to be the same as Lexing.from_function *)
  let after = lb.lex_abs_pos in
  let count = after - before in
  if count > 0 then begin
    lb.lex_start_pos <- lb.lex_start_pos - count;
    (* from Lexing.lex_refill *)
    lb.Lexing.lex_last_pos <- lb.Lexing.lex_last_pos - count;
    let t = lb.Lexing.lex_mem in
    for i = 0 to Array.length t-1 do
      let v = t.(i) in
      if v >= 0 then
        t.(i) <- v - count
    done
  end

(** {2 [Lexing]-style constructors.} *)

let from_string s =
  let lexbuf = Lexing.from_string s in
  let lm = Hashtbl.create 101 in
  Hashtbl.add lm 0 zero_pos;
  {prod = Ykp_none; 
   buf = {lexbuf with Lexing.refill_buff = dummy_fill};
   save = no_save;
   save_count = 0;
(*    linenum_base = 1; *)
(*    linenum_free = 0; *)
(*    linenum_map = Array.make 10 (-1) *)
   linenum_map = lm;
  }

let from_channel ic = 
  let buf = String.create ykbsize in
  let lm = Hashtbl.create 101 in
  Hashtbl.add lm 0 zero_pos;
  let rec ykb = {
    prod = Ykp_channel ic;
    save = no_save;
    save_count = 0;
    buf = {Lexing.
       refill_buff = (fun lb -> lexbuf_fill ykb lb);
       lex_buffer = buf;
       lex_buffer_len = 0;
       lex_abs_pos = 0;
       lex_curr_pos = 0; 
       lex_start_pos = 0;
       lex_last_pos = 0;
       lex_last_action = -1;
       lex_eof_reached = false;
       lex_mem = [||];
       lex_start_p = zero_pos;
       lex_curr_p = zero_pos;
    };  
(*    linenum_base = 1; *)
(*    linenum_free = 0; *)
(*    linenum_map = Array.make 10 (-1) *)
   linenum_map = lm;
  } in
  Hashtbl.add ykb.linenum_map 0 zero_pos;
  ykb

let from_function f = 
  let buf = String.create ykbsize in
  let lm = Hashtbl.create 101 in
  Hashtbl.add lm 0 zero_pos;
  let rec ykb = {
    prod = Ykp_genfn f;
    save = no_save; 
    save_count = 0;
    buf = {Lexing.
       refill_buff = (fun lb -> lexbuf_fill ykb lb);
       lex_buffer = buf;
       lex_buffer_len = 0;
       lex_abs_pos = 0;
       lex_curr_pos = 0; 
       lex_start_pos = 0;
       lex_last_pos = 0;
       lex_last_action = -1;
       lex_eof_reached = false;
       lex_mem = [||];
       lex_start_p = zero_pos;
       lex_curr_p = zero_pos;
    };  
(*    linenum_base = 1; *)
(*    linenum_free = 0; *)
(*    linenum_map = Array.make 10 (-1) *)
   linenum_map = lm;
  } in 
  ykb

let from_fd fd = 
  let buf = String.create ykbsize in
  let lm = Hashtbl.create 101 in
  Hashtbl.add lm 0 zero_pos;
  let rec ykb = {
    prod = Ykp_fd fd;
    save = no_save;
    save_count = 0;
    buf = {Lexing.
       refill_buff = (fun lb -> lexbuf_fill ykb lb);
       lex_buffer = buf;
       lex_buffer_len = 0;
       lex_abs_pos = 0;
       lex_curr_pos = 0; 
       lex_start_pos = 0;
       lex_last_pos = 0;
       lex_last_action = -1;
       lex_eof_reached = false;
       lex_mem = [||];
       lex_start_p = zero_pos;
       lex_curr_p = zero_pos;
    };  
(*    linenum_base = 1; *)
(*    linenum_free = 0; *)
(*    linenum_map = Array.make 10 (-1) *)
   linenum_map = lm;
  } in 
  ykb

let from_internal_buffer buf len =
  let lm = Hashtbl.create 101 in
  Hashtbl.add lm 0 zero_pos;
  let rec ykb = {
    prod = Ykp_none;
    save = no_save;
    save_count = 0;
    buf = {Lexing.
       refill_buff = (fun lb -> lexbuf_fill ykb lb);
       lex_buffer = buf;
       lex_buffer_len = len;
       lex_abs_pos = 0;
       lex_curr_pos = 0; 
       lex_start_pos = 0;
       lex_last_pos = 0;
       lex_last_action = -1;
       lex_eof_reached = true;
       lex_mem = [||];
       lex_start_p = zero_pos;
       lex_curr_p = zero_pos;
    };  
(*    linenum_base = 1; *)
(*    linenum_free = 0; *)
(*    linenum_map = Array.make 10 (-1) *)
   linenum_map = lm;
  } in 
  ykb

(** {2 "Compatability" constructors.} *)

let fd2buf = from_fd
let p2buf = from_function
let string2buf = from_string

let data2buf s =
  let len = Array.length s in
  let buf = String.create len in
  for i = 0 to len-1 do
    buf.[i] <- s.(i)
  done;
  from_internal_buffer buf len

let buffer2buf b =
  let len = Buffer.length b in
  let buf = String.create (len+1) in
  for i = 0 to len-1 do
    buf.[i] <- Buffer.nth b i
  done;
  from_internal_buffer buf len

let strings2buf a start = (* convert a string array (e.g., command-line arguments) to a ykbuf with NUL
                             characters separating the arguments.  start allows starting at index <> 0 *)
  let b = Buffer.create 11 in
  let howmany = Array.length a - start in
  for i = start to start+howmany-1 do
    Buffer.add_string b a.(i);
    Buffer.add_string b "\000"
  done;
  buffer2buf b

let stringsposn2string a start i = (* return the a.(n) containing position i in strings2buf(a),
                                for error reporting on a parse failure *)
  let rec loop n i =
    let len = String.length a.(n) in
    if i <= 0 or i <= len then n else
    loop (n+1) (i-len-1) in
  loop start (i-1)


(* 

Need to identify an interface and its invariant 
with regard to the lex_start_p and lex_curr_p variables.

Here's a start:

Option 1: the [newline] function is only called within a box. Since
boxes are surrounded by save and restore, we rely on those functions
to manage the variables correctly. This invariant makes sense in the
context of Yakker, where only boxes have access to the YkBuf, but it 
doesn't give you any guarantees about general use.  Care must be taken
not to allow the box function to be called w/o the save and restore.

Can I phrase the 
invariant succinctly?

Option 2: the [newline] function can be called anywhere. We add a field
to the ykbuf which maps positions to lnum, bol fields and thereby ensure
idempotency of newline call.

 *)

end

include Newversion

let get_string i j acc = Snapshot.sub acc i (j-i)
