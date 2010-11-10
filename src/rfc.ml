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

let get n =
  if n < 0 then raise(Invalid_argument "get_rfc");
  let rfcname = Filename.temp_file (Printf.sprintf "rfc%d.txt." n) "" in
  let finally() =
    (* clean up temp file *)
    Sys.remove rfcname;
    () in
  (* Would prefer to use https, but the server seems less reliable *)
  let command = Printf.sprintf "curl -o %s http://tools.ietf.org/rfc/rfc%d.txt" rfcname n in
  Logging.log Logging.Features.verbose "%s\n%!" command;
  (match Unix.system command with
    Unix.WEXITED x ->   Printf.eprintf "curl exited with %d\n%!" x
  | Unix.WSIGNALED x -> Printf.eprintf "curl exited with signal %d\n%!" x
  | Unix.WSTOPPED x ->  Printf.eprintf "curl stopped with %d\n%!" x
  );
  if Hashtbl.mem Rfcs.fixups n then begin
    let patchname,out = Filename.open_temp_file (Printf.sprintf "rfc%d.txt.patch." n) "" in
    output_string out (Hashtbl.find Rfcs.fixups n);
    close_out out;
    let command = Printf.sprintf "patch %s %s" rfcname patchname in
    Logging.log Logging.Features.verbose "%s\n%!" command;
    (match Unix.system command with
      Unix.WEXITED x ->   Printf.eprintf "patch exited with %d\n%!" x
    | Unix.WSIGNALED x -> Printf.eprintf "patch exited with signal %d\n%!" x
    | Unix.WSTOPPED x ->  Printf.eprintf "patch stopped with %d\n%!" x
    );
    Sys.remove patchname
  end;
  Logging.log Logging.Features.verbose "Starting extraction\n%!";
  (try
    ignore(Extract_grammar.parse_file rfcname);
  with _ -> ());
  finally()
