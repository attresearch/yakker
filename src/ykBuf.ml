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
          (*          Printf.eprintf "Eof reached (%d).\n" b.lex_buffer_len; *)
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
(*TODO: start is not used any more, ykb is sufficient; eliminate *)
let wrap_ocamllex f (start:int) ykb =
  let lexbuf = to_lexbuf ykb in
  let result =
    try
      let left = lexbuf.lex_curr_pos + lexbuf.lex_abs_pos in
      let result = f lexbuf in
      let right = lexbuf.lex_curr_pos + lexbuf.lex_abs_pos in
      let consumed = right - left in
(*       Printf.eprintf "Token consumed: %d (%d - %d).\n%!" consumed *)
(*      lexbuf.lex_start_pos lexbuf.lex_curr_pos; *)
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

let get_string i j acc = Snapshot.sub acc i (j-i)
