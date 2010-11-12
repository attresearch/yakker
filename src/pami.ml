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

exception Parse_error of string * int * int * int
  (** message, position, line number, column number (all 1-indexed) *)
  
let new_engine_flag = ref true

let mk_parse_fun _pf pp ykb = 
  let r, b = _pf ykb in
  List.map (fun (_,x) -> pp b x) r

let parse_error msg pos ykb =
  let lcp = match Util.find_option ykb.YkBuf.linenum_map (pos - 1) with Some x -> x
    | None -> YkBuf.zero_pos in
  let line = lcp.Lexing.pos_lnum in
  let cnum = lcp.Lexing.pos_cnum - lcp.Lexing.pos_bol in
  raise (Parse_error (msg, pos , line, cnum))

type prec = int
type assoc_pred = bool array

let prec_case_guard x y assoc_a = x > y or (x = y && assoc_a.(x))

(*************************************************************)
(*   External use                                            *)
(*************************************************************)
module Simple = struct

  (** [parse_file f file_name]
      parse file [file_name] with parsing function [f]. *)
  let parse_file f file =
    let infd = Unix.openfile file [Unix.O_RDONLY] 0o640 in
    let ykb = YkBuf.fd2buf infd in
    try
      let result = f ykb in
      Unix.close infd;
      result
    with e ->
      Unix.close infd;
      raise e

  (** [parse_string f s]
      parse string [s] with parsing function [f]. *)
  let parse_string f s = f (YkBuf.string2buf s)

  let _test verbose parse visualize =
    new_engine_flag := false;
    let visualize_flag = ref false in
    let args =
      match Array.to_list Sys.argv with
      | [] -> ["/dev/stdin"]
      | _::args ->
          (let rec loop = function
            | [] -> []
            | "-new-engine"::tl -> new_engine_flag := true; loop tl
            | "-viz"::tl ->         visualize_flag := true; loop tl
            | hd::tl -> hd::(loop tl) in
          loop args) in
    let args = if args=[] then ["/dev/stdin"] else args in
    let process = if !visualize_flag then visualize else parse in
    let process_file file =
      try
	process file;
	if verbose then Printf.eprintf "%s ok.\n%!" file
      with
	  Parse_error (msg, pos, lnum, cnum) ->
	    (* its really the token preceding the reported character/pos. 
	       pos is 1-based and cnum is 0-based.
	     *)
(* 	    Printf.eprintf "File %S, line %d, characters %d-%d (pos: %d):\n%s\n%!"  *)
(* 	      file lnum cnum cnum pos msg *)
	    Printf.eprintf "File %S, line %d, characters %d-%d:\n%s\n%!" 
	      file lnum cnum cnum msg
	| e ->
	    Printf.eprintf "File \"%s\" failed with exception %s\n%!" file
	      (Printexc.to_string e);
            prerr_endline (Printexc.get_backtrace())
    in
    List.iter process_file args

  let test parse visualize = _test true parse visualize

  (** quiet test. only reports failure. *)
  let qtest parse visualize = _test false parse visualize

  let run parse = _test true parse (fun _ -> failwith "Visualization not available.")

  (* deprecated *)
  let test_recognize f_r = test (fun file -> f_r file; ()) (fun _ -> ())

  (* deprecated *)
  let test_parse f_parse f_action =
    let process_file file =
      let results = f_parse file in
      Printf.eprintf "%s:" file;
      List.iter f_action results;
      Printf.eprintf "\n%!" in
    qtest process_file (fun _ -> ())

end

module Basic = struct
  (** [c] : parsing combinator *)
  let mk_recognize c v_init ykb =
    let results, max_offset = Allp.parse c ykb v_init in
    let n = YkBuf.get_input_size ykb in
    if n = -1 (* didn't reach eof *)
      || results = [] then
	parse_error "No complete parses found." (max_offset + 1) ykb
    else
      ()

  (** [c] : parsing combinator *)
  let mk_parse c v_init ykb =
    let cp = YkBuf.save ykb in
    Yakker.set_get_substring_fun (Yakker.default_get_substring ykb cp);
    let results, max_offset = Allp.parse c ykb v_init in
    if results = [] then
      let n = YkBuf.get_input_size ykb in
      let read_all =
	if n > -1 then max_offset >= n - 1
	else begin
	  (* check whether max_offset was the
	     last offset before EOF. *)
	  YkBuf.set_offset ykb (max_offset + 1);
	  not (YkBuf.fill2 ykb 1)
	end in
      YkBuf.restore ykb cp;
      let msg = if read_all then "No complete parses found." else  "Error at byte." in
      (* [max_offset] is the maximum valid offset. We add one to increase to the next invalid offset
	 and another to translate to "position." *)
      parse_error msg (max_offset + 2) ykb
    else
      begin
	YkBuf.advance_to_last ykb;
	let b = YkBuf.snapshot ykb cp in
	YkBuf.commit ykb;
	results, b
      end
end

module Peg = struct

  (** [c] : peg parsing combinator *)
  let mk_parse c v_init ykb =
    let cp = YkBuf.save ykb in
    Yakker.set_get_substring_fun (Yakker.default_get_substring ykb cp);
    match Allp.Peg.parse c ykb v_init with
      | (Some res), max_offset ->
	  let b = YkBuf.snapshot ykb cp in
	  YkBuf.commit ykb;
	  [res], b
      | None, max_offset ->
	  let n = YkBuf.get_input_size ykb in
	  let read_all =
	    if n > -1 then max_offset >= n - 1
	    else begin
	      (* check whether max_offset was the
		 last offset before EOF. *)
	      YkBuf.set_offset ykb (max_offset + 1);
	      not (YkBuf.fill2 ykb 1)
	    end in
	  YkBuf.restore ykb cp;
	  let msg = if read_all then "No complete parses found." else  "Error at byte." in
	  (* [max_offset] is the maximum valid offset. We add one to increase to the next invalid offset
	     and another to translate to "position." *)
	  parse_error msg (max_offset + 2) ykb


end

module Wfe = struct
  let mk_parse pf data sv0 pp ykb =
    let cp = YkBuf.save ykb in
    Yakker.set_get_substring_fun (Yakker.default_get_substring ykb cp);
    let parse_result = pf data sv0 ykb in
    if parse_result = [] then
      let read_all = YkBuf.is_eof ykb in
      let msg = if read_all then "No complete parses found." else  "Error at byte." in
      let pos = YkBuf.get_offset ykb + 1 in
      YkBuf.restore ykb cp;
      parse_error msg pos ykb
    else
      let b = YkBuf.snapshot ykb cp in
      YkBuf.commit ykb;
      List.map (pp b) parse_result

(** Only run the post-parse function on the first (if any) result from parsing. *)
  let mk_parse_single pf data sv0 pp ykb =
    let cp = YkBuf.save ykb in
    Yakker.set_get_substring_fun (Yakker.default_get_substring ykb cp);
    let parse_result = pf data sv0 ykb in
    match parse_result with
      | [] ->
	  let read_all = YkBuf.is_eof ykb in
	  let msg = if read_all then "No complete parses found." else  "Error at byte." in
	  let b = YkBuf.to_lexbuf ykb in
	  let lcp = b.Lexing.lex_curr_p in
	  let line = lcp.Lexing.pos_lnum in
	  let cnum = lcp.Lexing.pos_cnum - lcp.Lexing.pos_bol in
	  let pos = YkBuf.get_offset ykb + 1 in
	  YkBuf.restore ykb cp;
	  raise (Parse_error (msg, pos, line, cnum))
      | x::_ ->
	  let b = YkBuf.snapshot ykb cp in
	  YkBuf.commit ykb;
	  pp b x
end

(** Express as a functor so as to avoid requiring the dypgen libraries to be compiled
    with Yakker. *)
module Dypgen (Dyp: sig exception Syntax_error end) = struct

  (** Alternative to [Simple.parse_file] where the first argument is a
      function taking a lexbuf, instead of a ykbuf.  *)
  let parse_file f file =
    let f_in = open_in file in
    let lexbuf = Lexing.from_channel f_in in
    try
      let result = f lexbuf in
      close_in f_in;
      result
    with e -> close_in f_in; raise e

(** Create a Yakker parsing function from a Dypgen parsing function.
    Converts Dypgen exceptions to Yakker exceptions. *)
  let mk_parse f lexbuf =
    try
      f lexbuf
    with
      | Dyp.Syntax_error ->
	  raise (Parse_error ("Syntax error.", 
			      lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum, 
			      lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum,
			      lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum -
				lexbuf.Lexing.lex_curr_p.Lexing.pos_bol))

end
