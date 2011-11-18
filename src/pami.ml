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

let _internal_get_substring =
  ref (fun o n ->
         failwith "get_substring function not initialized.")
let default_get_substring ykb cp off len = YkBuf.extract_string ykb cp off len
let get_substring i j = !_internal_get_substring i (j-i)
let set_get_substring_fun f = _internal_get_substring := f

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

  let log_stats fmt = Logging.log Logging.Features.stats fmt

  let report_transducer_info = function
    | None -> ()
    | Some trans_data ->
        let pct_mt = PamJIT.DNELR.measure_percent trans_data (function PamJIT.DNELR.Many_trans _ -> true | _ -> false) in
        let pct_scan = PamJIT.DNELR.measure_percent trans_data (function PamJIT.DNELR.Scan_trans _ | PamJIT.DNELR.MScan_trans _ -> true | _ -> false) in
        let call_targets = PamJIT.DNELR.call_targets trans_data.PamJIT.DNELR.term_table in
        let pct_ndc = PamJIT.DNELR.measure_percenti trans_data (fun i tx -> call_targets.(i) && (match tx with PamJIT.DNELR.Many_trans _ -> true | _ -> false)) in
        log_stats "%d states.\n" (PamJIT.DNELR.get_num_states trans_data);
        log_stats "%.2f%% of states are nondeterministic.\n%.2f%% of call targets are nondeterministic.\n%.2f%% of states are scans.\n"
          (pct_mt *. 100.0) (pct_ndc *. 100.0) (pct_scan *. 100.0)

  let report_callc_info = function
    | None -> ()
    | Some trans_data ->
        let tcrs = PamJIT.DNELR.compute_callee_reachable_calls trans_data.PamJIT.DNELR.term_table in
        let r_counts = snd (List.split tcrs) in
        let rc_distro = Logging.Distributions.calculate r_counts in
        log_stats "Transducer callee reachable stats:\n";
        Logging.Distributions.log_distro rc_distro;
        log_stats "\n"

  let _test verbose print_pos parse visualize info_opt =
    let visualize_flag = ref false in
    let args =
      match Array.to_list Sys.argv with
      | [] -> failwith "Impossible: Sys.argv cannot be empty."
      | _::args ->
          (let rec loop = function
            | [] -> []
            | "--ensure-logging"::tl
            | "-e"::tl  ->
                if not Logging.activated then begin
                  Printf.eprintf "Error: Logging not activated!\n";
                  exit 1
                end;
                loop tl
(*             | "--no-warn-extla"::tl -> PamJIT.DNELR.warn_extla := false; loop tl *)
            | "-"::tl | "--viz"::tl -> visualize_flag := true; loop tl
            | "-s"::tl | "--stats"::tl ->
                Logging.add_features Logging.Features.stats; loop tl
            | "-l"::tl | "--log-engine"::tl ->
                Logging.add_features (Logging.Features.scans
                                      lor Logging.Features.reg_ne
                                      lor Logging.Features.comp_ne
                                      lor Logging.Features.calls_ne
                                      lor Logging.Features.eof_ne);
                loop tl
            | "-i"::tl | "--info"::tl ->
                Logging.add_features Logging.Features.stats;
                report_transducer_info info_opt;
                report_callc_info info_opt;
                if tl = [] then exit 0 else loop tl
            | "--cc"::tl ->
                Logging.add_features Logging.Features.stats;
                report_callc_info info_opt;
                if tl = [] then exit 0 else loop tl
            | "--exit"::[] -> exit 0    (* point is to allow measurement of basic transducer manipulations. *)
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
            if print_pos then
              Printf.eprintf "File %S, line %d, characters %d-%d (pos: %d):\n%s\n%!"
                file lnum cnum cnum pos msg
            else
              Printf.eprintf "File %S, line %d, characters %d-%d:\n%s\n%!"
                file lnum cnum cnum msg
        | e ->
            Printf.eprintf "File \"%s\" failed with exception %s\n%!" file
              (Printexc.to_string e);
            prerr_endline (Printexc.get_backtrace())
    in
    List.iter process_file args

  let test parse visualize = _test true false parse visualize None

  (** quiet test. only reports failure. *)
  let qtest parse visualize = _test false false parse visualize None

  let run parse = _test true true parse (fun _ -> failwith "Visualization not available.") None
  let qrun parse = _test false true parse (fun _ -> failwith "Visualization not available.") None

  (** quiet, debug run.  admit command line args turning on logging and reporting transducer stats.  *)
  let qdrun parse info = _test false true parse (fun _ -> failwith "Visualization not available.") (Some info)

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
    set_get_substring_fun (default_get_substring ykb cp);
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
    set_get_substring_fun (default_get_substring ykb cp);
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
    set_get_substring_fun (default_get_substring ykb cp);
    let parse_result = pf data sv0 ykb in
    if parse_result = [] then
      let read_all = YkBuf.is_eof ykb in
      let msg = if read_all then "No complete parses found." else  "Error at byte." in
      let pos = YkBuf.get_offset ykb + 1 in (* add 1 to translate from 0-indexed to 1-indexed. *)
      YkBuf.restore ykb cp;
      parse_error msg pos ykb
    else
      let b = YkBuf.snapshot ykb cp in
      YkBuf.commit ykb;
      List.map (pp b) parse_result

(** Only run the post-parse function on the first (if any) result from parsing. *)
  let mk_parse_single pf data sv0 pp ykb =
    let cp = YkBuf.save ykb in
    set_get_substring_fun (default_get_substring ykb cp);
    let parse_result = pf data sv0 ykb in
    match parse_result with
      | [] ->
          let read_all = YkBuf.is_eof ykb in
          let msg = if read_all then "No complete parses found." else  "Error at byte." in
          let pos = YkBuf.get_offset ykb + 1 in
          YkBuf.restore ykb cp;
          parse_error msg pos ykb
      | [x] ->
          let b = YkBuf.snapshot ykb cp in
          YkBuf.commit ykb;
          pp b x
      | x::_ ->
          Printf.eprintf "mk_parse_single: ambiguity in parse.\n";
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
