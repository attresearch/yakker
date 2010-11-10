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

(* let test_sppf_p = ref false *)
(* let only_load_p = ref false *)

(* let arg_specs = [ *)
(*   "-l", (Arg.Set only_load_p), "Only load and compile the interpreter."; *)
(*   "--test-sppf", (Arg.Set test_sppf_p), "Test the SPPF library."; *)
(* ] *)

(* let usage_message = "pami [options] transducer [file ...]" *)

(* let arg_specs = [ *)
(*   Arg.command "Only load and compile the interpreter." "-l" (Arg.Set only_load_p); *)
(*   Arg.command "Test the SPPF library." "--test-sppf" (Arg.Set test_sppf_p); *)
(* ] *)
(* let files = Arg.handle usage_message arg_specs  *)


(* (\** list of files to parse, specified on the command line. *\) *)
(* let add_files_p = ref [] *)
(* ;; *)
(* Arg.parse arg_specs  *)
(*     (fun s -> add_files_p := s::!add_files_p)  *)
(*     usage_message *)

(* let files = !add_files_p *)

let rec getfiles i fs =
  if i < Array.length Sys.argv then getfiles (i+1) (Sys.argv.(i)::fs) else fs
let files = List.rev (getfiles 1 [])

let (pvm_stream, pvm_start, files) =
  match files with
      f1::f2::fs -> (f1, int_of_string f2, fs)
    | _ -> (print_endline "Missing arguments"; exit 1)

module TheParser = Pami.MakeParser(struct let pvm_stream = pvm_stream
				     let pvm_start = pvm_start
				     let pvm_symbol_table = Sppf.default_symbol_table
				     let blackboxes = None
				     let external_fns = None
				     let predicates = None
			      end)
;;
match TheParser.parse_fun_opt with
    None -> prerr_endline "Parser generation failed."
  | Some (f,_) -> List.iter f files
