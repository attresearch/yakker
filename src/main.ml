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
open Cmdline;;

let phase_order = (* preorder on phases *)
  let rec add g = function
      [] -> g
    | [s] -> Tgraph.add_edge g s s
    | s::t::tl -> add (Tgraph.add_edge (Tgraph.add_edge g s s) s t) (t::tl) in
  Tgraph.tc
    (List.fold_left add Tgraph.empty
       [
        (* standalone commands that handle their own output*)
        [Extract_cmd];             (* these do not operate on Gul.grammars *)
        [Info_cmd];
        [Rfc_cmd];
        [Parse_cmd];               (* Parse only. *)

        [Print_gil_cmd];           (* takes Gil as input *)

        [Lookahead_analysis_cmd];  (* these take Gul.grammars as input *)
        [Generators_analysis_cmd];
        [Dependency_graph_cmd];
        [Lr1_lookahead_cmd; Lexer_cmd];  (* Too few preprocessing elements here, but ok for starters. *)
        [Precedence_analysis_cmd; Lexer_cmd];
        [Print_gul_cmd];
        [Print_npreds_cmd];
        [Print_relevance_cmd];
        [Sort_cmd];
        [Strip_late_actions_cmd];

        (* commands that handle their own output and
           invoke a chain of transformations ending with fuse *)
        [Translate_dypgen_scannerless_cmd; Fuse_cmd];
        [Translate_dypgen_cmd            ; Fuse_cmd];

        [Dot_cmd                         ; Desugar_gil_cmd; Inline_nullable_cmd; Fuse_cmd];
        [Compile_cmd                     ; Desugar_gil_cmd; Inline_nullable_cmd; Fuse_cmd];
        [Exec_cmd                        ; Desugar_gil_cmd; Inline_nullable_cmd; Fuse_cmd];

        (* commands that do not handle their own output *)
        (* they compose the usual sequence of transformations in compilation *)
        (* this ordering is over-specified, but that's useful for determinism *)
        [Fuse_cmd;                 (* Gil transformer *)
         Coroutine_cmd;            (* transforms Gul.grammar to Gil *)
         (* Gul.grammar transformers *)
         Replay_cmd; Wrap_cmd; Attributes_cmd; Lift_cmd; Desugar_cmd; Unroll_star_cmd;
         Inline_regular_cmd; Copyrule_cmd; Hash_cmd; Minus_cmd;
         Tx_prec_cmd; Close_under_core_cmd; Subset_cmd; Lexer_cmd];

        [Dearrow_cmd; Infer_ty_cmd; Lift_cmd];
      ])
let phase_order_sorted = Tgraph.tsort phase_order
let phases_of cmd =
  let phases =
    if !Cmdline.only then [cmd] else
    List.filter
      (fun x -> Tgraph.is_edge phase_order cmd x)
      phase_order_sorted in
  let phases = (* Remove phases according to -after flag *)
    match !Cmdline.after with None -> phases
    | Some phase ->
        List.filter
          (fun x -> not(Tgraph.is_edge phase_order phase x))
          phases in
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
    | Infer_ty_cmd
    | Wrap_cmd
    | Replay_cmd             -> Print_gul_cmd::phases

    | Coroutine_cmd
    | Dearrow_cmd
    | Inline_nullable_cmd
    | Desugar_gil_cmd
    | Fuse_cmd               -> Print_gil_cmd::phases

    | _                      -> phases in
  List.rev phases

(* NB this use of Logging.log is not guarded by Logging.activated, so the -v flag
   takes effect in either case *)
let vprintf x = Logging.log Logging.Features.verbose x

let outch = ref stdout

let cmd, files, roots, backend = Cmdline.process()
let files = if files=[] then ["/dev/stdin"] else files

let do_phase name thunk =
  vprintf "%s...%!" name;
  let result = thunk() in
  vprintf "done\n%!";
  result

let core =
  try
    do_phase "parsing core rules" (fun () ->
      let gr = List.hd (Yakker_grammar.parse_string Strings.core_bnf) in
      gr.ds)
  with e ->
    Printf.eprintf "Internal error: exception %s when parsing core rules\n%!" (Printexc.to_string e);
    raise e

let try_fsm f out gr = (* FSM version *)
  Util.pipe_in_out
    (Printf.sprintf
       "fsmcompile | fsmrmepsilon | fsmdeterminize | fsmprint | %s |fsmcompile | fsmrmepsilon | fsmdeterminize | fsmminimize | fsmprint"
       (Fsm.remove_CallEps()))
    (fun w -> Fsm.grammar_fsm w gr)
    (fun r -> f gr r out)

let try_fst f out gr = (* OpenFST version *)
  Util.pipe_in_out
    (Printf.sprintf
        "fstcompile --acceptor | fstrmepsilon | fstdeterminize | fstprint --acceptor | %s |fstcompile --acceptor | fstrmepsilon | fstdeterminize | fstminimize /dev/stdin | fstprint --acceptor"
       (Fsm.remove_CallEps())
    )
    (fun w -> Fsm.grammar_fsm w gr)
    (fun r -> f gr r out)

let gil_transducer,gil_dot =
  if !Compileopt.use_fsm then
    (* FSM *)
    (fun is_sv_known -> try_fsm (Fsm.fsm_transducer is_sv_known)), try_fsm Fsm.fsm_dot
  else
    (* FST *)
    (fun is_sv_known -> try_fst (Fsm.fsm_transducer is_sv_known)), try_fst Fsm.fsm_dot

let add_boilerplate backend gr =
  if backend = Wadler_BE then ()
  else let mk_trans_bp1 = Printf.sprintf "
let start_symb = get_symb_action %S

module P2__ = Yak.Engine.Full_yakker (%s)
                                     (struct type t = sv let cmp = sv_compare %s end)

let _wfe_data_ = Yak.PamJIT.DNELR.to_table (Yak.Pam_internal.load_internal_program program)
  start_symb (get_symb_start start_symb) %d num_symbols
  %s %s

let parse = Yak.Pami.Wfe.mk_parse P2__.parse _wfe_data_ sv0 %s
let visualize = parse
let visualize_file = Yak.Pami.Simple.parse_file visualize
let visualize_string = Yak.Pami.Simple.parse_string visualize
\n" in
  let mk_other_bp1 = Printf.sprintf "\nlet visualize = Yak.Pami.mk_parse_fun __parse
   (fun input state_node ->
      Printf.printf \"Visualization not supported by Gil interpreter.\n\")
let visualize_file = Yak.Pami.Simple.parse_file visualize
let visualize_string = Yak.Pami.Simple.parse_string visualize

let parse = Yak.Pami.mk_parse_fun __parse %s
" in
  let term_lang = if gr.has_single_lexer then Variables.tk_mod else "Yak.Engine.Scannerless_term_lang" in
  (* The [unit_history] flag overrides the standard history relevance. *)
  let post_parse_function =
    if gr.grammar_late_relevant && not !Compileopt.unit_history  && not !Compileopt.repress_replay then
      let name = Variables.bnf2ocaml gr.start_symbol in
      if gr.grammar_early_relevant then
        Printf.sprintf "(fun ykinput (_,h) -> _replay_%s ykinput h)" name
      else Printf.sprintf "_replay_%s" name
    else
      "(fun ykinput x -> ())" in
  let inspector_fields =
    if gr.grammar_late_relevant && not !Compileopt.unit_history then
      let patt = if gr.grammar_early_relevant then "(_,h)" else "h" in
      Printf.sprintf "type idata = Yk_History.Root_id_set.t
  let create_idata () = Yk_History.Root_id_set.empty
  let inspect %s s = Yk_History.add_id_set h#get_root s
  let summarize_inspection s = string_of_int (Yk_History.Root_id_set.cardinal s)" patt
    else
      "include Yak.Engine.Dummy_inspector" in
  let boilerplate_vary =
    match backend with
      | Trans_BE ->
          mk_trans_bp1 gr.start_symbol term_lang inspector_fields Fsm.min_symbol Fsm.default_call_tx
            Fsm.default_binder_tx post_parse_function
      | Fun_BE | Peg_BE _ ->
          mk_other_bp1 post_parse_function
      | Wadler_BE -> "" in
  let boilerplate_shared =
    "let parse_file = Yak.Pami.Simple.parse_file parse
let parse_string = Yak.Pami.Simple.parse_string parse\n;;\n"in
  add_to_epilogue gr (boilerplate_vary ^ boilerplate_shared)

let print_prologue ch gr =
  List.iter begin function
    | Ocaml x ->
        Printf.fprintf ch "%s" x
    | _ -> failwith "Non-ocaml blob in prologue"
  end
    (List.rev gr.prologue)

let print_epilogue ch gr =
  List.iter begin function
    | Ocaml x ->
        Printf.fprintf ch "%s" x
    | _ -> failwith "Non-ocaml blob in epilogue"
  end
    gr.epilogue

let do_compile is_sv_known out gr =
  do_phase "compiling to backend" begin fun () ->
    let print_prologue () = print_prologue out gr in
    let print_epilogue () = print_epilogue out gr in

    (match backend with
    | Fun_BE ->
        print_prologue ();
        Gil_gen.pr_gil_definitions2 out gr.start_symbol gr.tokmap gr.gildefs
    | Wadler_BE ->
        gr.prologue <- [];
        gr.epilogue <- [];
        Gil_gen.Wadler.pr_definitions out gr.start_symbol gr.gildefs
    | Peg_BE liberal ->
        print_prologue ();
        Gil_gen.Peg.pr_definitions out liberal gr.start_symbol gr.gildefs
    | Trans_BE ->
        print_prologue ();
        gil_transducer is_sv_known out gr.gildefs);

    add_boilerplate backend gr;

    if is_sv_known then
      begin
        print_epilogue ()
      end;
  end

let do_compile_for_arrow out gr =
  do_phase "compiling to backend" begin fun () ->
    let print_prologue () = print_prologue out gr in

    (match backend with
    | Fun_BE ->
        print_prologue ();
        Gil_gen.pr_gil_definitions2 out gr.start_symbol gr.tokmap gr.gildefs
    | Wadler_BE ->
        gr.prologue <- [];
        gr.epilogue <- [];
        Gil_gen.Wadler.pr_definitions out gr.start_symbol gr.gildefs
    | Peg_BE liberal ->
        print_prologue ();
        Gil_gen.Peg.pr_definitions out liberal gr.start_symbol gr.gildefs
    | Trans_BE ->
        print_prologue ();
        if !Compileopt.wrap_codegen_in_module then begin
          Printf.fprintf out "module Internal : sig
    val __default_call : 'a -> 'b -> sv
    val __default_ret : 'a -> 'b -> 'c -> 'b
    val num_symbols : int
    val symbol_table : int -> string
    val get_symb_action : string -> int
    val get_symb_start : int -> int
    val program : (int * sv Yak.Pam_internal.instruction list) list
  end = struct\n"
        end;

        gil_transducer true out gr.gildefs;

        if !Compileopt.wrap_codegen_in_module then begin
          Printf.fprintf out "end\nopen Internal;;\n\n";
        end;
    );

    add_boilerplate backend gr;

    print_epilogue out gr
  end

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
          do_phase "unrolling Kleene closure" (fun () ->
            Unroll.transform gr)
      | Desugar_cmd ->
          do_phase "desugaring" (fun () ->
            Desugar.desugar gr)
      | Lift_cmd ->
          do_phase "lifting" (fun () ->
            Analyze.producers gr;
            Analyze.relevance gr;
            Lift.transform gr)
      | Attributes_cmd ->
          if !Compileopt.use_wrap_and_attr then
            do_phase "attributes" begin fun () ->
              Analyze.producers gr;
              Analyze.relevance gr;
              Attributes.eliminate gr
            end
      | Wrap_cmd ->
          if !Compileopt.use_wrap_and_attr then
            do_phase "wrapping" begin fun () ->
              Analyze.producers gr;
              Analyze.relevance gr;
              if gr.grammar_early_relevant then begin
                Wrap.wrap gr; Analyze.relevance gr;
              end;
              if gr.grammar_early_relevant || gr.grammar_late_relevant then begin
                Wrap.force_alt_relevance gr; Analyze.relevance gr;
              end;
            end
      | Print_relevance_cmd ->
          let outc = !outch in
          Analyze.producers gr;
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
            gr.ds
      | Sort_cmd ->
          gr.ds <- Gul.sort_definitions gr.ds;
          Pr.pr_grammar stdout gr
      | Print_gul_cmd ->
          Pr.pr_grammar stdout gr
      | Replay_cmd ->
          do_phase "replay transformation" (fun () ->
            Replay.transform gr)
      | Coroutine_cmd ->
          do_phase "dispatching" begin fun () ->
            Analyze.producers gr;
            Analyze.relevance gr;
            if !Compileopt.use_coroutines then
              begin
                if !Compileopt.check_labels then
                  add_to_prologue gr
                    "let _i (x,y) = if x=y then y else failwith(Printf.sprintf \"_i expected %d, got %d\" x y)\n";
                Analyze.assignments gr;
                Coroutine.transform_grammar gr
              end
            else begin
               Ty_infer.infer false gr;
               Dearrow.transform gr
            end
          end
      | Infer_ty_cmd ->
          do_phase "inferring types" (fun () -> Ty_infer.infer true gr)
      | Dearrow_cmd ->
          do_phase "desugaring arrow notation" (fun () -> Dearrow.transform gr)
      | Fuse_cmd ->
          if !Compileopt.coalesce then
            do_phase "coalescing actions" (fun () -> Fusion.fuse_gil gr)
      | Inline_nullable_cmd ->
            do_phase "inlining nullability predicates" begin fun () ->
              let npreds = Nullable_pred.Gil.preds_from_grammar gr.gildefs in
              gr.gildefs <- Inline_nullable.inline npreds gr.gildefs
            end
      | Desugar_gil_cmd ->
            do_phase "desugaring Gil" begin fun () ->
              gr.gildefs <- Desugar.desugar_gil gr.gildefs
            end
      | Compile_cmd ->
          if !Compileopt.use_coroutines then
            do_compile true !outch gr
          else
            begin
              Dearrow.extend_prologue (do_compile false) gr;
              (* Output full version of compiled output *)
              do_compile_for_arrow !outch gr;
            end

      | Exec_cmd ->
          begin
            (* If the commands come with no epilogue, default to parsing the start symbol. *)
            if gr.epilogue=[] then
              add_to_epilogue gr "Yak.Pami.Simple.run parse_file";

            (* redirect output to a temporary file *)
            let (temp_file_name, temp_chan) = Filename.open_temp_file "yakker" ".ml" in

            do_compile true temp_chan gr;

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
          gil_dot !outch gr.gildefs
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
      | Translate_dypgen_cmd ->
          do_phase "translating to dypgen" (fun () ->
            let is_scannerless = false in
            let b = Buffer.create 11 in
            Pr.Gil.Dypgen.pr_grammar b is_scannerless gr;
            Printf.fprintf !outch "%s\n%!" (Buffer.contents b))
      | Translate_dypgen_scannerless_cmd ->
          do_phase "translating to dypgen-scannerless" (fun () ->
            let is_scannerless = true in
            let b = Buffer.create 11 in
            Pr.Gil.Dypgen.pr_grammar b is_scannerless gr;
            Printf.fprintf !outch "%s\n%!" (Buffer.contents b))
      | Print_npreds_cmd ->
          do_phase "printing nullable predicates" (fun () ->
            Nullable_pred.Gul.print_nullable_predicates gr !outch)
      | Precedence_analysis_cmd ->
          do_phase "precedence analysis" (fun () ->
            let v = Analyze.build_prec_sets gr in
            Analyze.print_prec_sets v)
      | Lookahead_analysis_cmd ->
          do_phase "FIRST/FOLLOW set analysis" (fun () ->
            Analyze.producers gr;
            Analyze.relevance gr;
            vprintf "%t"
              (fun outc -> Analyze.First_set_lex.report gr outc gr.tokmap))
      | Generators_analysis_cmd ->
          do_phase "Generators analysis" begin fun () ->
            let gs = Gul.get_generators_of gr.ds roots in
            PSet.iter (Printf.printf "%s\n") gs
          end
      | Dependency_graph_cmd ->
          do_phase "WORK IN PROGRESS: Generating dependency graph" begin fun () -> ()
(*             let g = Gul.dependency_graph gr.ds in *)
          end
      | Strip_late_actions_cmd ->
          do_phase "stripping late actions" (fun () ->
            Pr.pr_grammar stdout (remove_late_actions gr))
      | Lr1_lookahead_cmd ->
          (* TODO: once debugged this should be put into the standard pipeline, controlled by flag  *)
          do_phase "adding LR(1) lookahead" (fun () ->
            let gr = Lookahead.add_lr1_lookahead gr in
            Pr.pr_grammar stdout gr)
(*           do_phase "adding LALR(1) lookahead" begin fun () -> *)
(*             let gr = Lookahead.differentiate_lalr1 gr in *)
(*             Pr.pr_grammar stdout gr end *)
      | Extract_cmd -> failwith "Internal error: Extract_cmd in do_phases"
      | Parse_cmd -> failwith "Internal error: Parse_cmd in do_phases"
      | Info_cmd -> failwith "Internal error: Info_cmd in do_phases"
      | Rfc_cmd -> failwith "Internal error: Rfc_cmd in do_phases")

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

;; (* sanity checks *)
if cmd=Exec_cmd && List.length files <> 1 then failwith "exec must be used on a single file";;
(* these aren't supported because we do not parse Gil *)
if cmd=Print_gil_cmd then failwith "print-gil is not yet supported";;
if !Cmdline.only then
  match cmd with
  | Translate_dypgen_cmd             -> failwith "translate-dypgen -only is not supported"
  | Translate_dypgen_scannerless_cmd -> failwith "translate-dypgen-scannerless -only is not supported"
  | Dot_cmd                          -> failwith "dot -only is not supported"
  | Compile_cmd                      -> failwith "compile -only is not supported"
  | Exec_cmd                         -> failwith "exec -only is not supported"
  | Fuse_cmd                         -> failwith "fuse -only is not supported"
  | _ -> ()

;; Logging.add_features (Logging.Features.none
(*   lor Logging.Features.completions *)
(*   lor Logging.Features.scans *)
(*   lor Logging.Features.registrations *)
(*   lor Logging.Features.reg_ne *)
(*   lor Logging.Features.lookahead *)
(*   lor Logging.Features.sppf *)
(*   lor Logging.Features.calls *)
)

;; match cmd with
(* Deal first with commands that do not operate on Gul grammars *)
| Info_cmd ->
    Printf.printf "Yakker version %s.\n" Version.text;
    Printf.printf "Logging is ";
    if Logging.activated then Printf.printf "activated.\n"
    else Printf.printf "not activated.\n";
    exit 0

| Rfc_cmd ->
    Rfc.get !Cmdline.rfc_num

| Parse_cmd ->
    (try
      List.iter
        (fun file -> ignore (parse_file file))
        files;
    with e ->
      prerr_endline ("Exception raised:\n" ^ Printexc.to_string e);
      prerr_endline (Printexc.get_backtrace());
      exit 1)

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
