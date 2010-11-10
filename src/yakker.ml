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

(**
    Parse-time functions.
*)

let current_position = ref 0

(** Maximum position encountered during parsing. *)
let max_position = ref 0

let upd_max i = if i > !max_position then max_position := i else ()

let set_position i =
  if Logging.activated then
    begin
      upd_max i;
      Logging.log Logging.Features.position "CP=%d\n" i;
    end;
  current_position := i
let get_position () =
  if Logging.activated then
    Logging.log Logging.Features.position "get_position: %d\n" !current_position;
  !current_position
let get_max_position () = !max_position

let _internal_get_substring =
  ref (fun o n ->
	 failwith "Yakker.get_substring function not initialized.")
let default_get_substring ykb cp off len = YkBuf.extract_string ykb cp off len
let get_substring i j = !_internal_get_substring i (j-i)
let set_get_substring_fun f = _internal_get_substring := f

(**
    Post-parse functions.
*)

let get_string i j acc = YkBuf.Snapshot.sub acc i (j-i)


