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

let activated = true

type level = int

module Features = struct
  let none          = 0x00
  let completions   = 0x01
  let registrations = 0x02
  let scans         = 0x04
  let lookahead     = 0x08
  let worklist      = 0x10
  let verbose       = 0x20
  let actions       = 0x40
  let extent        = 0x80
  let crawling     = 0x100
  let calls        = 0x200
  let position     = 0x400
  let nullpred     = 0x800
  let sppf        = 0x1000
  let util        = 0x2000
  let combs       = 0x4000
  let reg_ne      = 0x8000 (** registrations, new engine *)
  let comp_ne    = 0x10000 (** completions, new engine *)
  let calls_ne   = 0x20000 (** calls, new engine *)
  let stats      = 0x40000
  let eof_ne     = 0x80000 (** processing of earley set on EOF. *)
end

let current_features = ref Features.none

let set_features fs = current_features := fs

let add_features fs =
  current_features := !current_features lor fs

(** Add to log if any of the specified features are active *)
let log_any features =
  if (features land !current_features) <> 0 then Printf.eprintf
  else Printf.ifprintf stderr

(** Add to log if all of the specified features are active *)
let log_all features =
  if (features land !current_features) = features then Printf.eprintf
  else Printf.ifprintf stderr

let log = log_any

module Counters = struct

  let counters = Hashtbl.create 11

  let init () = Hashtbl.clear counters

  let register c = Hashtbl.add counters c 0

  let increment c =
    let n = Hashtbl.find counters c in
    Hashtbl.replace counters c (n+1)

  let report () =
    Hashtbl.iter (fun c n -> log Features.stats "%s = %d\n" c n) counters
end

module Distributions = struct

  let table : (string, int list) Hashtbl.t = Hashtbl.create 11

  let init () = Hashtbl.clear table

  let register key = Hashtbl.add table key ([] : int list)

  let add_value k v =
    let vs = Hashtbl.find table k in
    Hashtbl.replace table k (v::vs)

  let report () =
    Hashtbl.iter (fun k vs ->
		      let distribution =
			match List.sort (-) vs with
			  | [] -> []
			  | x::xs ->
			      let v, n, d = List.fold_left
				(fun (v_last, n, dis) v ->
				   if v = v_last then (v_last, n + 1, dis)
				   else (v, 1, (v_last, n)::dis)) (x, 1, []) xs in
			      List.rev ((v,n)::d) in
		      log Features.stats "%s:" k;
		      List.iter (fun (v, n) ->
				   log Features.stats " %d, %d |" v n)
			distribution;
		      log Features.stats "\n") table

end
