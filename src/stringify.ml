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

(* stringify: turn a file into an ocaml string *)

let filename2varname file =
  (* Convert a filename into a string which can serve as an ocaml variable *)
  (* Map characters other than a-z, A-Z, 0-9, and _ to _ *)
  let s = String.copy file in
  for i = 0 to String.length s - 1 do
    let c = String.get s i in
    if not(('a' <= Char.lowercase c && Char.lowercase c <= 'z') || ('0' <= c && c <= '9') || c = '_') then
      String.set s i '_';
  done;
  s

let stringify file =
  Printf.printf "\"";
  let ch = open_in file in
  begin
    try
      (* Echo until end-of-file; escape backslash and double quote *)
      while true do
        let c = input_char ch in
        match c with
        | '\\' -> (print_char c; print_char c)
        | '\"' -> (print_char '\\'; print_char c)
        | _ -> print_char c
      done
    with _ -> ()
  end;
  Printf.printf "\"";
  close_in ch

let to_var file =
  let varname = filename2varname (Filename.basename file) in
  Printf.printf "let %s =\n" varname;
  stringify file;
  Printf.printf ";;\n"

let fixups dir =
  let subdirs = Sys.readdir dir in
  Printf.printf "let fixups = Hashtbl.create %d;;\n" (Array.length subdirs);
  Array.iter
    (fun d ->
      Printf.printf "Hashtbl.add fixups %s\n" d;
      stringify (Filename.concat dir (Filename.concat d "fixup.patch"));
      Printf.printf ";;\n")
    subdirs
;;

match Array.to_list Sys.argv with
| [_;"-rfcs";dir] -> fixups dir
| [] | [_] ->        to_var "/dev/stdin"
| cmd::args ->       List.iter to_var args

;;


