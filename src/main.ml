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

open Yak
open Gul
open Cmdline

let phase_order = (* preorder on phases *)
  let rec add g = function
      [] -> g
    | [s] -> Tgraph.add_edge g s s
    | s::t::tl -> add (Tgraph.add_edge (Tgraph.add_edge g s s) s t) (t::tl) in
  Tgraph.tc
    (List.fold_left add Tgraph.empty
       [
        (* standalone commands that handle their own output*)
        [Print_npreds_cmd];
        [Analyze_cmd];
        [Lookahead_analysis_cmd];
        [Strip_late_actions_cmd];
        [Transform_cmd];

        (* commands that handle their own output and invoke a chain of transformations *)
        [Translate_cmd; Fuse_cmd];
        [      Dot_cmd; Fuse_cmd];
        [  Compile_cmd; Fuse_cmd];
        [     Exec_cmd; Fuse_cmd];

        (* transformations that are part of the usual compiling process *)
        (* this ordering is over-specified, but that's useful for determinism *)
        [Fuse_cmd; Dispatch_cmd; Wrap_cmd; Attributes_cmd; Lift_cmd;
         Desugar_cmd; Unroll_star_cmd; Inline_regular_cmd; Copyrule_cmd;
         Hash_cmd; Minus_cmd; Tx_prec_cmd; Close_under_core_cmd;
         Subset_cmd; Lexer_cmd];
      ])
let phase_order_sorted = Tgraph.tsort phase_order
let phases_of cmd =
  let phases =
    if !Cmdline.only then [cmd] else
    List.filter
      (fun x -> Tgraph.is_edge phase_order cmd x)
      phase_order_sorted in
  let phases = (* Add an output phase if necessary *)
    match cmd with
    | Lexer_cmd
    | Subset_cmd
    | Close_under_core_cmd
    | Tx_prec_cmd
    | Minus_cmd
    | Hash_cmd
    | Copyrule_cmd
    | Inline_regular_cmd
    | Unroll_star_cmd
    | Lookahead_analysis_cmd
    | Desugar_cmd
    | Lift_cmd
    | Attributes_cmd
    | Wrap_cmd               -> Print_gul_cmd::phases

    | Dispatch_cmd
    | Fuse_cmd               -> Print_gil_cmd::phases

    | _                      -> phases in
  List.rev phases

;;Logging.add_features (Logging.Features.none
(*   lor Logging.Features.completions *)
(*   lor Logging.Features.scans *)
(*   lor Logging.Features.registrations *)
(*   lor Logging.Features.reg_ne *)
(*   lor Logging.Features.lookahead *)
(*   lor Logging.Features.sppf *)
(*   lor Logging.Features.calls *)
)
;;

(* NB this use of Logging.log is not guarded by Logging.activated, so the -v flag
   takes effect in either case *)
let vprintf x = Logging.log Logging.Features.verbose x

let outch = ref stdout

let cmd, files, roots, backend = Cmdline.process()
let files = if files=[] then ["/dev/stdin"] else files

let core =
  try
    let gr = List.hd (Yakker_grammar.parse_string Strings.core_bnf) in
    gr.ds
  with e ->
    Printf.eprintf "Internal error: exception %s when parsing core rules\n%!" (Printexc.to_string e);
    raise e

let try_fsm f gr = (* FSM version *)
  Util.pipe_in_out
    (Printf.sprintf
       "fsmcompile | fsmrmepsilon | fsmdeterminize | fsmprint | %s |fsmcompile | fsmrmepsilon | fsmdeterminize | fsmminimize | fsmprint"
       (Fsm.remove_CallEps()))
    (fun w -> Fsm.grammar_fsm w gr)
    (fun r -> f gr r !outch)

let try_fst f gr = (* OpenFST version *)
  Util.pipe_in_out
    (Printf.sprintf
        "fstcompile --acceptor | fstrmepsilon | fstdeterminize | fstprint --acceptor | %s |fstcompile --acceptor | fstrmepsilon | fstdeterminize | fstminimize /dev/stdin | fstprint --acceptor"
       (Fsm.remove_CallEps())
    )
    (fun w -> Fsm.grammar_fsm w gr)
    (fun r -> f gr r !outch)

let use_fsm = true
let gil_transducer,gil_dot =
  if use_fsm then
    (* FSM *)
    try_fsm Fsm.fsm_transducer,try_fsm Fsm.fsm_dot
  else
    (* FST *)
    try_fst Fsm.fsm_transducer,try_fst Fsm.fsm_dot

let dump_prologue gr =
  List.iter
    (function Ocaml x ->
(*      Printf.printf "dumping--\n%s%!" x;*)
      Printf.fprintf !outch "%s" x;
()(*      Printf.printf "X%s%!" x*)
      | _ -> failwith "dump_prologue")
    (List.rev gr.prologue)

let dump_epilogue gr =
  List.iter
    (function Ocaml x -> Printf.fprintf !outch "%s" x
      | _ -> failwith "dump_epilogue")
    gr.epilogue

let add_boilerplate2 backend gr =
  let post_parse_function =
    match gr.grammar_early_relevant,gr.grammar_late_relevant with
    | (true,true) ->
        Printf.sprintf "
    (fun ykinput (_,h) ->
      let _o = (h#traverse_postfix) in
      let _n() = (let (x,_) = _o#next() in x) in
      _r_%s(_n,ykinput)
    )" (Variables.bnf2ocaml gr.start_symbol)
    | (false,true) ->
        Printf.sprintf "
    (fun ykinput h ->
      let _o = (h#traverse_postfix) in
      let _n() = (let (x,_) = _o#next() in x) in
      _r_%s(_n,ykinput)
    )
" (Variables.bnf2ocaml gr.start_symbol)
    | _ -> "(fun ykinput x -> ())" in
  let boilerplate =
    (match backend with
       | Trans_BE ->
           Printf.sprintf "
let start_symb = get_symb_action \"%s\"

module P2__ = Yak.Engine.Full_yakker(struct type t = sv let cmp = sv_compare end)

let _wfe_data_ = Yak.PamJIT.DNELR.to_table (Yak.Pam_internal.load_internal_program program)
  start_symb (get_symb_start start_symb) %d num_symbols
  %s %s

let parse = Yak.Pami.Wfe.mk_parse P2__.parse _wfe_data_ sv0 %s
let visualize = parse
let visualize_file = Yak.Pami.Simple.parse_file visualize
let visualize_string = Yak.Pami.Simple.parse_string visualize
\n"
    gr.start_symbol Fsm.min_symbol Fsm.default_call_tx Fsm.default_binder_tx post_parse_function
       | Fun_BE | Peg_BE _ ->
      Printf.sprintf "\nlet visualize = Yak.Pami.mk_parse_fun __parse
   (fun input state_node ->
      Printf.printf \"Visualization not supported by Gil interpreter.\n\")
let visualize_file = Yak.Pami.Simple.parse_file visualize
let visualize_string = Yak.Pami.Simple.parse_string visualize

let parse = Yak.Pami.mk_parse_fun __parse %s
"       post_parse_function)
    ^
      "let parse_file = Yak.Pami.Simple.parse_file parse
let parse_string = Yak.Pami.Simple.parse_string parse
;;\n" in
  add_to_epilogue gr boilerplate

let do_phase name thunk =
  vprintf "%s...%!" name;
  let result = thunk() in
  vprintf "done\n%!";
  result

let do_compile gr =
  do_phase "compiling to backend" (fun () ->
    dump_prologue gr;
    (match backend with
    | Fun_BE -> Gil_gen.pr_gil_definitions2 !outch gr.start_symbol gr.tokmap gr.gildefs
    | Peg_BE liberal -> Gil_gen.Peg.pr_definitions !outch liberal gr.start_symbol gr.gildefs
    | Trans_BE ->
        gil_transducer gr.gildefs);
    add_boilerplate2 backend gr;
    dump_epilogue gr)

let do_phases gr =
  List.iter
    (function
      | Lexer_cmd ->
          do_phase "lexer transform" (fun () ->
            Lexutil.transform gr;
            Lexutil.run_ocamllex gr)
      | Subset_cmd ->
          if roots <> [] then
            do_phase "subsetting" (fun () ->
              gr.ds <- Gul.get_reachable gr.ds roots)
      | Close_under_core_cmd ->
          do_phase "closing under core rules" (fun () ->
            gr.ds <- Gul.close_definitions gr.ds core)
      | Tx_prec_cmd ->
          do_phase "precedence transform" (fun () ->
            Analyze.producers gr;
            Analyze.relevance gr;
            Analyze.prec_rewrite gr)
      | Minus_cmd ->
          do_phase "minus elimination" (fun () ->
            Analyze.producers gr;
            Analyze.relevance gr;
            Minus.elim gr)
      | Hash_cmd ->
          do_phase "hash elimination" (fun () ->
            Hash.elim gr)
      | Copyrule_cmd ->
          do_phase "copy rule transform" (fun () ->
            Copyrule.transform gr)
      | Inline_regular_cmd ->
          if !Compileopt.inline_regular then
            (do_phase "inlining regular grammars" (fun () ->
              Analyze.producers gr;
              Analyze.relevance gr;
              Analyze.regular_inline gr true;
              vprintf "%t"
                (fun outc ->
                  if (List.exists (function RuleDef(_,r,_) -> r.a.is_regular | _ -> false) gr.ds) then begin
                    Printf.fprintf outc "These nonterminals have regular right-hand sides:";
                    List.iter
                      (function RuleDef(n,r,_) -> if r.a.is_regular then Printf.fprintf outc " %s" n | _ -> ())
                      gr.ds;
                    Printf.fprintf outc "\n"
                  end
                  else
                    Printf.fprintf outc "No nonterminals have regular right-hand sides\n")))
      | Unroll_star_cmd ->
          if !Compileopt.unroll_star_n > 0 then
            (do_phase "unrolling Kleene closure" (fun () ->
              Analyze.producers gr;
              Analyze.relevance gr;
              Analyze.unroll_analyze gr;
              vprintf "%t" (fun outc -> Pr.pr_grammar outc gr)))
      | Desugar_cmd ->
          do_phase "desugaring" (fun () ->
            Desugar.desugar gr)
      | Lift_cmd ->
          do_phase "lifting" (fun () ->
            Analyze.producers gr;
            Analyze.relevance gr;
            Lift.transform gr)
      | Attributes_cmd ->
          do_phase "attributes" (fun () ->
            Analyze.producers gr;
            Analyze.relevance gr;
            Attributes.eliminate gr)
      | Wrap_cmd ->
          do_phase "wrapping" (fun () ->
            Analyze.producers gr;
            Analyze.relevance gr;
            if gr.grammar_early_relevant then begin
              Wrap.wrap gr; Analyze.relevance gr;
              Wrap.force_alt_relevance gr; Analyze.relevance gr
            end;
            Wrap.transform_history gr)
      | Print_relevance_cmd ->
          vprintf "%t" (fun outc ->
            Analyze.relevance gr;
            List.iter
              (function
                  RuleDef(n,r,a) ->
                    Printf.fprintf outc "Rel[%s,%s] Prod[%s,%s] <-- %s\n%!"
                      (if r.a.early_relevant then "T" else "F")
                      (if r.a.late_relevant then "T" else "F")
                      (if (PSet.mem n gr.early_producers) then "T" else "F")
                      (if (PSet.mem n gr.late_producers) then "T" else "F")
                      n
                | _ -> ())
              gr.ds)
      | Print_gul_cmd ->
          Pr.pr_grammar stdout gr
      | Dispatch_cmd ->
          do_phase "dispatching" (fun () ->
            Analyze.relevance gr;
            Label.transform gr;
            if !Compileopt.check_labels then
              add_to_prologue gr
                "let _i (x,y) = if x=y then y else failwith(Printf.sprintf \"_i expected %d, got %d\" x y)\n";
            Analyze.assignments gr;
            Replay.transform gr;
            Dispatch.transform gr)
      | Fuse_cmd ->
          if !Compileopt.coalesce then
            do_phase "coalescing actions" (fun () -> Fusion.fuse gr)
      | Compile_cmd ->
          do_compile gr

      | Exec_cmd ->
          begin
            (* If the commands come with no epilogue, default to parsing the start symbol. *)
            if gr.epilogue=[] then
              add_to_epilogue gr "Yak.Pami.Simple.run parse_file";
            (* redirect output to a temporary file *)
            let (temp_file_name,temp_chan) = Filename.open_temp_file "yakker" ".ml" in

            outch := temp_chan;

            do_compile gr;

            (* make sure compiled output is flushed *)
            close_out temp_chan;
            (* have ocaml execute the output *)
            (* need to pass -I flag so that ocaml can find yakker.cmi etc. *)
            (* currently the Makefile stashes this in buildinfo.ml, the build_dir
               is where those files end up during the build.
               TODO: eventually this ought to be the install location *)
            let args = String.concat " " (List.map (fun s -> "\""^s^"\"") (!Cmdline.exec_l)) in
            let command = Printf.sprintf "ocaml -I \"%s\" unix.cma yak.cma %s %s" Buildinfo.build_dir temp_file_name args in
            (match Unix.system command with
              Unix.WEXITED x ->   () (* Printf.eprintf "ocaml exited with %d\n%!" x        *)
            | Unix.WSIGNALED x -> () (* Printf.eprintf "ocaml exited with signal %d\n%!" x *)
            | Unix.WSTOPPED x ->  () (* Printf.eprintf "ocaml stopped with %d\n%!" x       *)
            );
            (* clean up temp file *)
            Sys.remove temp_file_name
          end
      | Dot_cmd ->
          gil_dot gr.gildefs
      | Print_gil_cmd ->
          begin
            let b = Buffer.create 11 in
            if gr.prologue=[] then () else begin
              Printf.bprintf b "{\n";
              List.iter
                (function Ocaml x -> Printf.bprintf b "%s" x
                  | _ -> failwith "main")
                (List.rev gr.prologue);
              Printf.bprintf b "\n}\n\n"
            end;
            Pr.Gil.Pretty.pr_definitions b gr.gildefs;
            Printf.printf "%s\n%!" (Buffer.contents b)
          end
      | Translate_cmd ->
          let plugin = !Cmdline.translate_plugin in
          do_phase "exporting to alternate syntax" (fun () ->
            match plugin with Dypgen_PI (is_scannerless) ->
              let b = Buffer.create 11 in
              Pr.Gil.Dypgen.pr_grammar b is_scannerless gr gr.gildefs; (*TODO: Change Dypgen.pr_grammar to use gr.gildefs*)
              Printf.fprintf !outch "%s\n%!" (Buffer.contents b))
      | Print_npreds_cmd ->
          do_phase "printing nullable predicates" (fun () ->
            let tbl_ntnames = Hashtbl.create 11 in
            let ntnum = ref 264 in
            let add_nonterm nt =
              begin
                match Util.find_option tbl_ntnames nt with
                | Some x -> x
                | None ->
                    let num = !ntnum in
                    incr ntnum;
                    Hashtbl.add tbl_ntnames nt num;
                    num
              end
            in
            (* use dummy get_start function because there is no transducer. *)
            Nullable_pred.process_grammar !outch add_nonterm (fun _ -> 0) gr)
      | Analyze_cmd ->
          (match !Cmdline.analysis with
            A_precedence_sets ->
              do_phase "lexer transform" (fun () ->
                Lexutil.transform gr;
                Lexutil.run_ocamllex gr);
              do_phase "precedence analysis" (fun () ->
                let v = Analyze.build_prec_sets gr in
                Analyze.print_prec_sets v))
      | Lookahead_analysis_cmd ->
          do_phase "FIRST/FOLLOW set analysis" (fun () ->
            Analyze.producers gr;
            Analyze.relevance gr;
            vprintf "%t"
              (fun outc -> Analyze.First_set_lex.report gr outc gr.tokmap))
      | Strip_late_actions_cmd ->
          do_phase "stripping late actions" (fun () ->
            Pr.pr_grammar stdout (remove_late_actions gr))
      | Transform_cmd  ->
          let txs = !Cmdline.transforms in
          let do_tx gr = function
            | Inline_regular_Tx -> Analyze.regular_inline gr true; gr
            | Add_LR1_lookahead_Tx ->
                let first_map = Analyze.First_set_lex.first_gr gr gr.tokmap in
                let first r = Analyze.First_set_lex.first r first_map in
                let sz = List.length gr.ds in
                let nametbl = Hashtbl.create sz in
                let ftbl = Hashtbl.create sz in
                let apptbl = Hashtbl.create 101 in
                let dummy = mkLIT("") in
                let rec findfree n x tbl =
                  let n_try = n ^ string_of_int x in
                  if Hashtbl.mem tbl n_try then
                    findfree n (x+1) tbl
                  else x, n_try in
                let mk_name n =
                  try
                    let x = Hashtbl.find nametbl n in
                    let x',n' = findfree n x ftbl in
                    Hashtbl.replace nametbl n (x' + 1);
                    n'
                  with Not_found ->
                    Hashtbl.add nametbl n 1;
                    n in
                let la_app (n:nonterminal) fs =
                  try
                    let n_fs,_,_ = Hashtbl.find apptbl (n,fs) in
                    n_fs
                  with Not_found ->
                    let n_fs = mk_name n in
                    let f_n, a = Hashtbl.find ftbl n in
                    Hashtbl.add apptbl (n, fs) (n_fs, dummy, a);
                    let r_fs = f_n fs in
                    Hashtbl.replace apptbl (n, fs) (n_fs, r_fs, a);
                    n_fs in
                let rec tx r = match r.r with
                | Symb (n1, _, _,_) -> (fun follow -> mkSYMB (la_app n1 follow, None, None)) (* TODO: attributes *)
                | Lit _ | CharRange _ -> (fun follow -> r)
                | Seq(r1, None, None, r2) ->
                    let r1' = tx r1 in
                    let r2' = tx r2 in
                    let f_r2 = first r2 in
                    (fun follow ->
                      mkSEQ2 (r1' (Analyze.First_set_lex.concat f_r2 follow None None),
                              None, None,
                              r2' follow))
                | Alt(r1,r2) ->
                    let r1' = tx r1 in
                    let r2' = tx r2 in
                    (fun follow -> mkALT2 (r1' follow, r2' follow))
                | Star (Bounds(0,Infinity) as b, r1) ->
                    let r1' = tx r1 in
                    (fun follow -> mkSTAR2 (b, r1' (Analyze.First_set_lex.union (first r1) follow)))
                | Opt(r1)  ->
                    let r1' = tx r1 in
                    (fun follow -> mkOPT (r1' follow))
                | Minus _ -> Util.warn Util.Sys_warn "minus not processed by lookahead"; (fun follow -> r)
                | Assign _
                | Star _
                | Seq _
                | Lookahead _
                | Box _
                | Action _
                | Position _
                | Rcount _
                | Hash _
                | When _
                | Delay _ | Prose _ ->
                    Util.warn Util.Sys_warn "rule not handled by lookahead transformation";
                    (fun _ -> mkLIT("!ERROR!")) in
                let tx_def = function
                  | RuleDef(n, r, a) ->
                      let r' = tx r in
                      Hashtbl.add ftbl n
                        ((fun la -> mkSEQ2(r' la, None, None, mkLOOKAHEAD(true, Analyze.First_set_lex.to_rule la))),
                         a)
                  | d -> () in
                (* load [ftbl] *)
                List.iter tx_def gr.ds;
                (* load [apptbl] *)
                let ssymb = la_app gr.start_symbol (Analyze.First_set_lex.non_singleton (256, 256)) in
                (* convert [apptbl] to new grammar. *)
                let retrieve_defs _ (n,r,a) ds = (RuleDef (n,r,a))::ds in
                let ds' = Hashtbl.fold retrieve_defs apptbl [] in
                {gr with ds = ds'; start_symbol = ssymb;} in
          let gr = List.fold_left do_tx gr txs in
          Pr.pr_grammar stdout gr

      | _ -> failwith "Internal error: unexpected phase")

let parse_file file =
  do_phase "parsing bnf" (fun () ->
    match Yakker_grammar.parse_file file with
      [gr] -> gr
    | [] ->
        prerr_endline "Error: empty input";
        exit 1
    | gr::_ ->
        prerr_endline "Warning: yakker input has multiple parses";
        gr)

;;
match cmd with
(* Deal first with commands that do not operate on Gul grammars *)
| Info_cmd ->
    Printf.printf "Yakker version %s.\n" Version.text;
    Printf.printf "Logging is ";
    if Logging.activated then Printf.printf "activated.\n"
    else Printf.printf "not activated.\n";
    exit 0

| Rfc_cmd ->
    Rfc.get !Cmdline.rfc_num

| Extract_cmd ->
    List.iter
      (Extract_grammar.extract stdout)
      files

| _ ->
    (* Commands that operate on Gul grammars are handled by do_phases *)
    (try
      List.iter
        (fun file ->
          let gr = parse_file file in
          do_phases gr (phases_of cmd))
        files;
    with e ->
      prerr_endline ("Exception raised:\n" ^ Printexc.to_string e);
      prerr_endline (Printexc.get_backtrace());
      exit 1)
