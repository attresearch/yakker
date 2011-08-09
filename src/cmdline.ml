
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

type command =
| Attributes_cmd
| Close_under_core_cmd
| Compile_cmd
| Copyrule_cmd
| Coroutine_cmd
| Desugar_cmd
| Desugar_gil_cmd
| Dearrow_cmd
| Dependency_graph_cmd
| Dispatch_cmd
| Dot_cmd
| Exec_cmd
| Extract_cmd
| Fuse_cmd
| Generators_analysis_cmd
| Hash_cmd
| Infer_ty_cmd
| Info_cmd
| Inline_regular_cmd
| Inline_nullable_cmd
| Lexer_cmd
| Lift_cmd
| Lookahead_analysis_cmd
| Lr1_lookahead_cmd
| Minus_cmd
| Precedence_analysis_cmd
| Print_gil_cmd
| Print_gul_cmd
| Print_npreds_cmd
| Print_relevance_cmd
| Replay_cmd
| Rfc_cmd
| Sort_cmd
| Strip_late_actions_cmd
| Subset_cmd
| Translate_dypgen_cmd
| Translate_dypgen_scannerless_cmd
| Tx_prec_cmd
| Unroll_star_cmd
| Wrap_cmd

let cmd = ref Print_gul_cmd
let after = ref None
let files = ref []
let roots = ref []
let exec_l = ref []
let rfc_num = ref 0
let only = ref false

type backend =
  | Fun_BE   (** Functional backend (parser combinators).*)
  | Wadler_BE   (** Haskell functional backend (parser combinators).*)
  | Peg_BE of bool (** PEG backend. argument indicates whether
                       to use more liberal (PADS-style) star. *)
  | Trans_BE (** Transducer backend. *)

let backend = ref Trans_BE
type hv = int
;;

module Yk_Hashed = struct
  type t = int * hv * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
  let memoize = true
end
module Yk_History = Yak.History.Make(Yk_Hashed)

let rec
 _r_cmd_line_args(_n,_p,ykinput) = (let c = _r_command(_n,_p,ykinput) in (); (while (match _n() with (2000) -> true | _ (*2001*) -> false) do
_r_args(_n,_p,ykinput)done)
; ();  cmd := c )
and
 _r_phases(_n,_p,ykinput) = (match _n() with
 | (2002) -> ((); ();  Attributes_cmd )
 | (2003) -> ((); ();  Close_under_core_cmd )
 | (2004) -> ((); ();  Copyrule_cmd )
 | (2005) -> ((); ();  Desugar_cmd )
 | (2006) -> ((); ();  Desugar_gil_cmd )
 | (2007) -> ((); ();  Hash_cmd )
 | (2008) -> ((); ();  Dearrow_cmd )
 | (2009) -> ((); ();  Infer_ty_cmd )
 | (2010) -> ((); ();  Inline_regular_cmd )
 | (2011) -> ((); ();  Inline_nullable_cmd )
 | (2012) -> ((); ();  Lexer_cmd )
 | (2013) -> ((); ();  Lift_cmd )
 | (2014) -> ((); ();  Minus_cmd )
 | (2015) -> ((); ();  Tx_prec_cmd )
 | (2016) -> ((); ();  Subset_cmd )
 | (2017) -> ((); ();  Unroll_star_cmd )
 | (2018) -> ((); ();  Wrap_cmd )
 | (2019) -> ((); ();  Replay_cmd )
 | _ -> raise Exit)
and
 _r_command(_n,_p,ykinput) = (match _n() with
 | (2020) -> ((let p = _r_phases(_n,_p,ykinput) in  (match p with
                                               Inline_regular_cmd -> Compileopt.inline_regular := true
                                             | Unroll_star_cmd -> if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1
                                             | _ -> ());
                                            p ))
 | (2021) -> ((); ();  Compile_cmd )
 | (2022) -> ((); ();  Coroutine_cmd )
 | (2023) -> ((); ();  Dependency_graph_cmd )
 | (2024) -> ((); ();  Dispatch_cmd )
 | (2025) -> ((); ();  Dot_cmd )
 | (2026) -> ((); (); (let _x4 = _p() in (); (let _x3 = _p() in (let f = Yak.YkBuf.get_string _x4 _x3 ykinput in (let l = (let _x8 = (let rec _x19 _x8 =
(match _n() with (2030) -> _x8 | _ (*2029*) ->
 _x19((let _x7 = (); (let _x6 = _p() in (); (let _x5 = _p() in (let x = Yak.YkBuf.get_string _x6 _x5 ykinput in x))) in _x7::_x8)))
in _x19(Yak.Util.nil)) in (List.rev _x8)) in ();  files := f::!files; exec_l := l; Exec_cmd )))))
 | (2033) -> ((); ();  Extract_cmd )
 | (2034) -> ((); ();  Compileopt.coalesce := true; Fuse_cmd )
 | (2035) -> ((); ();  Generators_analysis_cmd )
 | (2036) -> ((); ();  Info_cmd )
 | (2037) -> ((); ();  Lookahead_analysis_cmd )
 | (2038) -> ((); ();  Lr1_lookahead_cmd )
 | (2039) -> ((); ();  Precedence_analysis_cmd )
 | (2040) -> ((); ();  Print_gul_cmd )
 | (2041) -> ((); ();  Print_gil_cmd )
 | (2042) -> ((); ();  Print_npreds_cmd )
 | (2043) -> ((); ();  Print_npreds_cmd )
 | (2044) -> ((); ();  Print_relevance_cmd )
 | (2045) -> ((); (let _x10 = _p() in (); (let _x9 = _p() in (let n = Yak.YkBuf.get_string _x10 _x9 ykinput in ();  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" ))))
 | (2048) -> ((); ();  Sort_cmd )
 | (2049) -> ((); ();  Strip_late_actions_cmd )
 | (2050) -> ((); ();  Translate_dypgen_cmd )
 | (2051) -> ((); ();  Translate_dypgen_scannerless_cmd )
 | _ -> raise Exit)
and
 _r_args(_n,_p,ykinput) = (match _n() with
 | (2052) -> ((); (); (let p = _r_phases(_n,_p,ykinput) in  after := Some p ; ()))
 | (2053) -> ((); ();  Compileopt.use_coroutines := false ; ())
 | (2054) -> ((); (); (let b = (match _n() with
 | (2055) -> ((); Fun_BE)
 | (2056) -> ((); Trans_BE)
 | (2057) -> ((); Wadler_BE)
 | (2058) -> ((); Peg_BE false)
 | (2059) -> ((); Peg_BE true)
 | _ -> raise Exit) in ();  backend := b ; ()))
 | (2060) -> ((); ();  Compileopt.case_sensitive := false ; ())
 | (2061) -> ((); ();  Compileopt.check_labels := true ; ())
 | (2062) -> ((); (); (let _x12 = _p() in (); (let _x11 = _p() in (let n = Yak.YkBuf.get_string _x12 _x11 ykinput in ();  Variables.counter := (int_of_string n) ; ()))))
 | (2065) -> ((); ();  Compileopt.inline_cs := true ; ())
 | (2066) -> ((); ();  Compileopt.inline_regular := true ; ())
 | (2067) -> ((); ();  Compileopt.lookahead := true ; ())
 | (2068) -> ((); ();  Compileopt.memoize_history := true ; ())
 | (2069) -> ((); ();  Compileopt.coalesce := false ; ())
 | (2070) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2071) -> ((); ();  Compileopt.repress_replay := true ; ())
 | (2072) -> ((); ();  Compileopt.skip_opt := false ; ())
 | (2073) -> ((); ();  only := true ; ())
 | (2074) -> ((); ();  Compileopt.postfix_history := false ; ())
 | (2075) -> ((); (); (let _x14 = _p() in (); (let _x13 = _p() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2078) -> ((); ();  Compileopt.unit_history := true ; ())
 | (2079) -> ((); (); (let _x16 = _p() in (); (let _x15 = _p() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  Compileopt.unroll_star_n := (int_of_string n) ; ()))))
 | (2082) -> ((); ();  Compileopt.use_fsm := true ; ())
 | (2083) -> ((); ();  Compileopt.use_fsm := false ; ())
 | (2084) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2085) -> ((let _x18 = _p() in (); (let _x17 = _p() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
 | _ -> raise Exit)
class ['a] rvs (labels: 'a History.enum) =
let s = ref [] in
let push x = s := x::!s in
let push_pos p = s := ( p)::!s in
let _n() = (let (_,x,_) = labels#next() in x) in
let _p() = (let (_,_,p) = labels#next() in p) in
let rec _rv_cmd_line_args() = ();();push((2001)); while (match _n() with (2000) -> true | _ (*2001*)-> false) do
 _rv_args(); push((2000))
done
;();_rv_command()
and _rv_phases() = (match _n() with
 | (2002) -> (();();(); push((2002)))
 | (2003) -> (();();(); push((2003)))
 | (2004) -> (();();(); push((2004)))
 | (2005) -> (();();(); push((2005)))
 | (2006) -> (();();(); push((2006)))
 | (2007) -> (();();(); push((2007)))
 | (2008) -> (();();(); push((2008)))
 | (2009) -> (();();(); push((2009)))
 | (2010) -> (();();(); push((2010)))
 | (2011) -> (();();(); push((2011)))
 | (2012) -> (();();(); push((2012)))
 | (2013) -> (();();(); push((2013)))
 | (2014) -> (();();(); push((2014)))
 | (2015) -> (();();(); push((2015)))
 | (2016) -> (();();(); push((2016)))
 | (2017) -> (();();(); push((2017)))
 | (2018) -> (();();(); push((2018)))
 | (2019) -> (();();(); push((2019)))
 | _ -> raise Exit)
and _rv_command() = (match _n() with
 | (2020) -> (();_rv_phases(); push((2020)))
 | (2021) -> (();();(); push((2021)))
 | (2022) -> (();();(); push((2022)))
 | (2023) -> (();();(); push((2023)))
 | (2024) -> (();();(); push((2024)))
 | (2025) -> (();();(); push((2025)))
 | (2026) -> (();();();push((2030)); while (match _n() with (2029) -> true | _ (*2030*)-> false) do
 ();();();push_pos(_p());();push_pos(_p());(); push((2029))
done
;();push_pos(_p());();push_pos(_p());();(); push((2026)))
 | (2033) -> (();();(); push((2033)))
 | (2034) -> (();();(); push((2034)))
 | (2035) -> (();();(); push((2035)))
 | (2036) -> (();();(); push((2036)))
 | (2037) -> (();();(); push((2037)))
 | (2038) -> (();();(); push((2038)))
 | (2039) -> (();();(); push((2039)))
 | (2040) -> (();();(); push((2040)))
 | (2041) -> (();();(); push((2041)))
 | (2042) -> (();();(); push((2042)))
 | (2043) -> (();();(); push((2043)))
 | (2044) -> (();();(); push((2044)))
 | (2045) -> (();();();push_pos(_p());();push_pos(_p());(); push((2045)))
 | (2048) -> (();();(); push((2048)))
 | (2049) -> (();();(); push((2049)))
 | (2050) -> (();();(); push((2050)))
 | (2051) -> (();();(); push((2051)))
 | _ -> raise Exit)
and _rv_args() = (match _n() with
 | (2052) -> (();();_rv_phases();();(); push((2052)))
 | (2053) -> (();();();(); push((2053)))
 | (2054) -> (();();();(match _n() with
 | (2055) -> (();(); push((2055)))
 | (2056) -> (();(); push((2056)))
 | (2057) -> (();(); push((2057)))
 | (2058) -> (();(); push((2058)))
 | (2059) -> (();(); push((2059)))
 | _ -> raise Exit);();(); push((2054)))
 | (2060) -> (();();();(); push((2060)))
 | (2061) -> (();();();(); push((2061)))
 | (2062) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2062)))
 | (2065) -> (();();();(); push((2065)))
 | (2066) -> (();();();(); push((2066)))
 | (2067) -> (();();();(); push((2067)))
 | (2068) -> (();();();(); push((2068)))
 | (2069) -> (();();();(); push((2069)))
 | (2070) -> (();();();(); push((2070)))
 | (2071) -> (();();();(); push((2071)))
 | (2072) -> (();();();(); push((2072)))
 | (2073) -> (();();();(); push((2073)))
 | (2074) -> (();();();(); push((2074)))
 | (2075) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2075)))
 | (2078) -> (();();();(); push((2078)))
 | (2079) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2079)))
 | (2082) -> (();();();(); push((2082)))
 | (2083) -> (();();();(); push((2083)))
 | (2084) -> (();();();(); push((2084)))
 | (2085) -> (();();();();push_pos(_p());();push_pos(_p()); push((2085)))
 | _ -> raise Exit)
in
object (self)
method next() = (match !s with hd::tl -> (s := tl; hd) | _ -> raise Not_found)
initializer _rv_cmd_line_args()
end

let _replay_cmd_line_args ykinput h =
  let _o = new rvs (h#right_to_left) in
  let _n() = _o#next() in
  let _p() = _o#next() in
  _r_cmd_line_args(_n,_p,ykinput)
(* History constructors *)
let _e p h = h#empty p
let _p lbl hv p = (fun h->h#push p (lbl, hv, p))
let _m lbl p = (fun h1 h2-> h1#merge p (lbl,  lbl, p) h2)

(*LATE PROLOGUE*)
type _pos = int (* input positions *)
let hv_compare = Yk_History.compare
type sv = (int * hv * _pos, Yak.History.label) Yak.History.history
let sv0 = Yk_History.new_history()
let sv_compare = hv_compare
let sv_hash = Yk_History.hash
let sv_eq x y = sv_compare x y = 0
let key_eq (i,v1) (j,v2) = i = j &&  sv_eq v1 v2
let key_hash (i,v) = i lxor (sv_hash v)

(** Hashtable for top-down parsing. *)
module TDHashtable = Hashtbl.Make(struct type t = int * sv let equal = key_eq let hash = key_hash end)

let __default_call _ _ = sv0;;
let __cc_call _ x = x;;
let __default_ret _ v1 _ = v1;;
let num_symbols = 11

let symbol_table = function
  | 267 -> "cmd-line-args"
  | 272 -> "command"
  | 268 -> "o"
  | 264 -> "CHAR"
  | 270 -> "file"
  | 265 -> "DIGIT"
  | 273 -> "args"
  | 271 -> "phases"
  | 274 -> "eof"
  | 269 -> "arg"
  | 266 -> "OCTET"
  | x -> if x < 256 then Yak.Pam_internal.default_symbol_table x else "?unknown?"

let get_symb_action = function
  | "cmd-line-args" -> 267
  | "command" -> 272
  | "o" -> 268
  | "CHAR" -> 264
  | "file" -> 270
  | "DIGIT" -> 265
  | "args" -> 273
  | "phases" -> 271
  | "eof" -> 274
  | "arg" -> 269
  | "OCTET" -> 266
  | _ -> raise Not_found

let get_symb_start = function
  | 274 -> 11
  | 273 -> 10
  | 272 -> 9
  | 271 -> 8
  | 270 -> 7
  | 269 -> 6
  | 268 -> 5
  | 267 -> 4
  | 266 -> 3
  | 265 -> 2
  | 264 -> 1
  | _ -> raise Not_found

module Pred3 = Yak.Pam_internal.Pred3
let __a50 = (_p 1069 ((2009)));;
let __a5 = (_p 1012 ((2000)));;
let __a71 = (_p 1437 ((2066)));;
let __a69 = (_p 1303 ((2044)));;
let __a7 = (_p 1537 ((2084)));;
let __a39 = (_p 1161 ((2022)));;
let __a79 = (_p 1405 ((2060)));;
let __a54 = (_p 1419 ((2064)));;
let __a75 = (_p 1168 ((2023)));;
let __a67 = (_p 1382 ((2057)));;
let __a6 = (_p 1308 ((2046)));;
let __a60 = (_p 1510 ((2080)));;
let __a33 = (_p 1175 ((2024)));;
let __a16 = (_p 1324 ((2048)));;
let __a35 = (_p 1219 ((2026)));;
let __a59 = (_p 1473 ((2072)));;
let __a26 = (_p 1317 ((2045)));;
let __a18 = (_p 1104 ((2014)));;
let __a2 = (_p 1006 ((2001)));;
let __a1 = (_p 1540 ((2086)));;
let __a8 = (_p 1182 ((2025)));;
let __a81 = (_p 1331 ((2049)));;
let __a77 = (_p 1449 ((2068)));;
let __a30 = (_p 1355 ((2052)));;
let __a19 = (_p 1275 ((2040)));;
let __a43 = (_p 1111 ((2015)));;
let __a29 = (_p 1226 ((2033)));;
let __a53 = (_p 1394 ((2059)));;
let __a9 = (_p 1549 ((2085)));;
let __a24 = (_p 1118 ((2016)));;
let __a40 = (_p 1282 ((2041)));;
let __a38 = (_p 1531 ((2083)));;
let __a14 = (_p 1233 ((2034)));;
let __a46 = (_p 1431 ((2065)));;
let __a20 = (_p 1311 ((2047)));;
let __a73 = (_p 1513 ((2081)));;
let __a55 = (_p 1289 ((2042)));;
let __a45 = (_p 1376 ((2056)));;
let __a51 = (_p 1125 ((2017)));;
let __a42 = (_p 1020 ((2002)));;
let __a72 = (_p 1485 ((2074)));;
let __a66 = (_p 1240 ((2035)));;
let __a44 = (fun _x0_ _x1_ -> (((_p 1213 ((2029))) _x0_) (((_p 1207 ((2032))) _x0_) _x1_)));;
let __a65 = (_p 1076 ((2010)));;
let __a85 = (_p 1296 ((2043)));;
let __a74 = (_p 1027 ((2003)));;
let __a4 = (_p 1543 ((2087)));;
let __a15 = (_p 1247 ((2036)));;
let __a34 = (_p 1204 ((2031)));;
let __a68 = (_p 1083 ((2011)));;
let __a48 = (_p 1467 ((2071)));;
let __a63 = (_p 1505 ((2078)));;
let __a22 = (_p 1490 ((2076)));;
let __a32 = (_p 1034 ((2004)));;
let __a80 = (_p 1254 ((2037)));;
let __a47 = (_p 1443 ((2067)));;
let __a62 = (_p 1411 ((2061)));;
let __a17 = (_p 1090 ((2012)));;
let __a82 = (_p 1388 ((2058)));;
let __a27 = (_p 1041 ((2005)));;
let __a41 = (_p 1499 ((2075)));;
let __a61 = (_p 1261 ((2038)));;
let __a25 = (fun _x0_ _x1_ -> (((_p 1197 ((2030))) _x0_) (((_p 1191 ((2028))) _x0_) _x1_)));;
let __a11 = (_p 1097 ((2013)));;
let __a76 = (_p 1338 ((2050)));;
let __a49 = (_p 1048 ((2006)));;
let __a37 = (_p 1525 ((2082)));;
let __a83 = (_p 1268 ((2039)));;
let __a36 = (_p 1416 ((2063)));;
let __a13 = (_p 1188 ((2027)));;
let __a78 = (_p 1519 ((2079)));;
let __a52 = (_p 1370 ((2055)));;
let __a12 = (_p 1132 ((2018)));;
let __g0 = (_e);;
let __a86 = (_p 1345 ((2051)));;
let __a21 = (_p 1479 ((2073)));;
let __a3 = (_p 1147 ((2020)));;
let __a10 = (_p 1055 ((2007)));;
let __a57 = (_p 1425 ((2062)));;
let __a56 = (_p 1399 ((2054)));;
let __a23 = (_p 1139 ((2019)));;
let __a31 = (_p 1493 ((2077)));;
let __a70 = (_p 1361 ((2053)));;
let __a58 = (_p 1455 ((2069)));;
let __a64 = (_p 1062 ((2008)));;
let __a28 = (_p 1154 ((2021)));;
let __a84 = (_p 1461 ((2070)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1002);;
let __binder2 = (_m 1144);;
let __binder3 = (_m 1011);;
let __binder4 = (_m 1352);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(45,426)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(104,427)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [AAction2Instr(__a31,428)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(115,429)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(115,430)]);
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,431)]);
(5, [EatInstr(0,16)]);
(389, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,432)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,44)]);
(390, [EatInstr(97,433)]);
(7, [EatInstr(127,46);EatInstr(126,46);EatInstr(125,46);EatInstr(124,46);EatInstr(123,46);EatInstr(122,46);EatInstr(121,46);EatInstr(120,46);EatInstr(119,46);EatInstr(118,46);EatInstr(117,46);EatInstr(116,46);EatInstr(115,46);EatInstr(114,46);EatInstr(113,46);EatInstr(112,46);EatInstr(111,46);EatInstr(110,46);EatInstr(109,46);EatInstr(108,46);EatInstr(107,46);EatInstr(106,46);EatInstr(105,46);EatInstr(104,46);EatInstr(103,46);EatInstr(102,46);EatInstr(101,46);EatInstr(100,46);EatInstr(99,46);EatInstr(98,46);EatInstr(97,46);EatInstr(96,46);EatInstr(95,46);EatInstr(94,46);EatInstr(93,46);EatInstr(92,46);EatInstr(91,46);EatInstr(90,46);EatInstr(89,46);EatInstr(88,46);EatInstr(87,46);EatInstr(86,46);EatInstr(85,46);EatInstr(84,46);EatInstr(83,46);EatInstr(82,46);EatInstr(81,46);EatInstr(80,46);EatInstr(79,46);EatInstr(78,46);EatInstr(77,46);EatInstr(76,46);EatInstr(75,46);EatInstr(74,46);EatInstr(73,46);EatInstr(72,46);EatInstr(71,46);EatInstr(70,46);EatInstr(69,46);EatInstr(68,46);EatInstr(67,46);EatInstr(66,46);EatInstr(65,46);EatInstr(64,46);EatInstr(63,46);EatInstr(62,46);EatInstr(61,46);EatInstr(60,46);EatInstr(59,46);EatInstr(58,46);EatInstr(57,46);EatInstr(56,46);EatInstr(55,46);EatInstr(54,46);EatInstr(53,46);EatInstr(52,46);EatInstr(51,46);EatInstr(50,46);EatInstr(49,46);EatInstr(48,46);EatInstr(47,46);EatInstr(46,46);EatInstr(44,46);EatInstr(43,46);EatInstr(42,46);EatInstr(41,46);EatInstr(40,46);EatInstr(39,46);EatInstr(38,46);EatInstr(37,46);EatInstr(36,46);EatInstr(35,46);EatInstr(34,46);EatInstr(33,46);EatInstr(32,46);EatInstr(31,46);EatInstr(30,46);EatInstr(29,46);EatInstr(28,46);EatInstr(27,46);EatInstr(26,46);EatInstr(25,46);EatInstr(24,46);EatInstr(23,46);EatInstr(22,46);EatInstr(21,46);EatInstr(20,46);EatInstr(19,46);EatInstr(18,46);EatInstr(17,46);EatInstr(16,46);EatInstr(15,46);EatInstr(14,46);EatInstr(13,46);EatInstr(12,46);EatInstr(11,46);EatInstr(10,46);EatInstr(9,46);EatInstr(8,46);EatInstr(7,46);EatInstr(6,46);EatInstr(5,46);EatInstr(4,46);EatInstr(3,46);EatInstr(2,46);EatInstr(1,46)]);
(391, [EatInstr(115,434)]);
(8, [EatInstr(119,28);EatInstr(117,27);EatInstr(115,26);EatInstr(114,25);EatInstr(112,24);EatInstr(109,23);EatInstr(108,22);EatInstr(105,21);EatInstr(104,20);EatInstr(100,19);EatInstr(99,18);EatInstr(97,17)]);
(392, [EatInstr(101,435)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(103,33);EatInstr(102,32);EatInstr(101,31);EatInstr(100,30);EatInstr(99,29);AContInstr3(271,__g0,__binder2,40);ACallInstr3(__g0,8)]);
(393, [AAction2Instr(__a32,254)]);
(10, [EatInstr(45,41);AAction2Instr(__a1,42)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43)]);
(394, [EatInstr(105,436)]);
(395, [EatInstr(101,437)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(108,438)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(103,439)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(101,440)]);
(15, [AAction2Instr(__a2,145)]);
(399, [EatInstr(97,441)]);
(16, [CompleteInstr(268)]);
(400, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,442)]);
(17, [EatInstr(116,49);EatInstr(114,48)]);
(401, [EatInstr(121,443)]);
(18, [EatInstr(111,51);EatInstr(108,50)]);
(402, [AAction2Instr(__a33,78)]);
(19, [EatInstr(101,52)]);
(403, [AAction2Instr(__a35,78);AAction2Instr(__a34,444)]);
(20, [EatInstr(97,53)]);
(404, [EatInstr(97,445)]);
(21, [EatInstr(110,54)]);
(405, [EatInstr(45,446)]);
(22, [EatInstr(105,56);EatInstr(101,55)]);
(406, [EatInstr(104,447)]);
(23, [EatInstr(105,57)]);
(407, [EatInstr(101,448)]);
(24, [EatInstr(114,58)]);
(408, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,449)]);
(25, [EatInstr(101,59)]);
(409, [EatInstr(101,450)]);
(26, [EatInstr(117,60)]);
(410, [EatInstr(108,451)]);
(27, [EatInstr(110,61)]);
(411, [EatInstr(101,452)]);
(28, [EatInstr(114,62)]);
(412, [EatInstr(101,453)]);
(29, [EatInstr(111,63)]);
(413, [EatInstr(45,454)]);
(30, [EatInstr(111,66);EatInstr(105,65);EatInstr(101,64)]);
(414, [EatInstr(116,455)]);
(31, [EatInstr(120,67)]);
(415, [EatInstr(119,459);EatInstr(116,458);EatInstr(112,457);EatInstr(102,456)]);
(32, [EatInstr(117,68)]);
(416, [EatInstr(101,460)]);
(33, [EatInstr(101,69)]);
(417, [EatInstr(98,461)]);
(34, [EatInstr(110,70)]);
(418, [AAction2Instr(__a36,462)]);
(35, [EatInstr(114,72);EatInstr(111,71)]);
(419, [EatInstr(115,463)]);
(36, [EatInstr(114,73)]);
(420, [EatInstr(101,464)]);
(37, [EatInstr(102,74)]);
(421, [EatInstr(100,465)]);
(38, [EatInstr(116,76);EatInstr(111,75)]);
(422, [EatInstr(104,466)]);
(39, [EatInstr(114,77)]);
(423, [EatInstr(115,467)]);
(40, [AAction2Instr(__a3,78)]);
(424, [EatInstr(122,468)]);
(41, [EatInstr(118,90);EatInstr(117,89);EatInstr(114,88);EatInstr(112,87);EatInstr(111,86);EatInstr(110,85);EatInstr(109,84);EatInstr(108,83);EatInstr(105,82);EatInstr(99,81);EatInstr(98,80);EatInstr(97,79)]);
(425, [EatInstr(121,469)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,91)]);
(426, [EatInstr(111,470)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(105,471)]);
(44, [ALookaheadInstr(false,CfgLA (1,264),45);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,44)]);
(428, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,472)]);
(45, [CompleteInstr(269)]);
(429, [EatInstr(116,473)]);
(46, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,46)]);
(430, [EatInstr(116,474)]);
(47, [CompleteInstr(270)]);
(431, [AAction2Instr(__a37,196)]);
(48, [EatInstr(114,94)]);
(432, [AAction2Instr(__a38,196)]);
(49, [EatInstr(116,95)]);
(433, [EatInstr(116,475)]);
(50, [EatInstr(111,96)]);
(434, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,476)]);
(51, [EatInstr(112,97)]);
(435, [EatInstr(114,477)]);
(52, [EatInstr(115,98)]);
(436, [EatInstr(108,478)]);
(53, [EatInstr(115,99)]);
(437, [EatInstr(115,479)]);
(54, [EatInstr(108,101);EatInstr(102,100)]);
(438, [EatInstr(108,480)]);
(55, [EatInstr(120,102)]);
(439, [EatInstr(117,481)]);
(56, [EatInstr(102,103)]);
(440, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,482)]);
(57, [EatInstr(110,104)]);
(441, [EatInstr(114,483)]);
(58, [EatInstr(101,105)]);
(442, [AAction2Instr(__a39,78)]);
(59, [EatInstr(112,106)]);
(443, [EatInstr(45,484)]);
(60, [EatInstr(98,107)]);
(444, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,485)]);
(61, [EatInstr(114,108)]);
(445, [EatInstr(116,486)]);
(62, [EatInstr(97,109)]);
(446, [EatInstr(97,487)]);
(63, [EatInstr(114,111);EatInstr(109,110)]);
(447, [EatInstr(101,488)]);
(64, [EatInstr(112,112)]);
(448, [EatInstr(45,489)]);
(65, [EatInstr(115,113)]);
(449, [AAction2Instr(__a40,78)]);
(66, [EatInstr(116,114)]);
(450, [EatInstr(100,490)]);
(67, [EatInstr(116,116);EatInstr(101,115)]);
(451, [EatInstr(97,491)]);
(68, [EatInstr(115,117)]);
(452, [EatInstr(118,492)]);
(69, [EatInstr(116,118)]);
(453, [EatInstr(45,493)]);
(70, [EatInstr(102,119)]);
(454, [EatInstr(100,494)]);
(71, [EatInstr(111,120)]);
(455, [EatInstr(97,495)]);
(72, [EatInstr(49,121)]);
(456, [EatInstr(117,496)]);
(73, [EatInstr(105,123);EatInstr(101,122)]);
(457, [EatInstr(101,497)]);
(74, [EatInstr(99,124)]);
(458, [EatInstr(120,498)]);
(75, [EatInstr(114,125)]);
(459, [EatInstr(97,499)]);
(76, [EatInstr(114,126)]);
(460, [EatInstr(110,500)]);
(77, [EatInstr(97,127)]);
(461, [EatInstr(101,501)]);
(78, [CompleteInstr(272)]);
(462, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,536)]);
(79, [EatInstr(114,129);EatInstr(102,128)]);
(463, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,502)]);
(80, [EatInstr(97,130)]);
(464, [EatInstr(103,503)]);
(81, [EatInstr(111,133);EatInstr(104,132);EatInstr(97,131)]);
(465, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,504)]);
(82, [EatInstr(110,134)]);
(466, [EatInstr(105,505)]);
(83, [EatInstr(111,135)]);
(467, [EatInstr(99,506)]);
(84, [EatInstr(101,136)]);
(468, [EatInstr(101,507)]);
(85, [EatInstr(111,137)]);
(469, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(86, [EatInstr(110,138)]);
(470, [EatInstr(112,509)]);
(87, [EatInstr(114,139)]);
(471, [EatInstr(115,510)]);
(88, [EatInstr(111,140)]);
(472, [AAction2Instr(__a41,196)]);
(89, [EatInstr(115,142);EatInstr(110,141)]);
(473, [EatInstr(111,511)]);
(90, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,143)]);
(474, [EatInstr(97,512)]);
(91, [AAction2Instr(__a4,144)]);
(475, [EatInstr(105,513)]);
(92, [AAction2Instr(__a5,145)]);
(476, [AAction2Instr(__a42,254)]);
(93, [CompleteInstr(267)]);
(477, [EatInstr(45,514)]);
(94, [EatInstr(111,146)]);
(478, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,515)]);
(95, [EatInstr(114,147)]);
(479, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,516)]);
(96, [EatInstr(115,148)]);
(480, [EatInstr(97,517)]);
(97, [EatInstr(121,149)]);
(481, [EatInstr(108,518)]);
(98, [EatInstr(117,150)]);
(482, [AAction2Instr(__a43,254)]);
(99, [EatInstr(104,151)]);
(483, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,519)]);
(100, [EatInstr(101,152)]);
(484, [EatInstr(103,520)]);
(101, [EatInstr(105,153)]);
(485, [AAction2Instr(__a44,359)]);
(102, [EatInstr(101,154)]);
(486, [EatInstr(111,521)]);
(103, [EatInstr(116,155)]);
(487, [EatInstr(110,522)]);
(104, [EatInstr(117,156)]);
(488, [EatInstr(97,523)]);
(105, [EatInstr(99,157)]);
(489, [EatInstr(97,524)]);
(106, [EatInstr(108,158)]);
(490, [EatInstr(115,525)]);
(107, [EatInstr(115,159)]);
(491, [EatInstr(98,526)]);
(108, [EatInstr(111,160)]);
(492, [EatInstr(97,527)]);
(109, [EatInstr(112,161)]);
(493, [EatInstr(97,528)]);
(110, [EatInstr(112,162)]);
(494, [EatInstr(121,529)]);
(111, [EatInstr(111,163)]);
(495, [EatInstr(116,530)]);
(112, [EatInstr(101,164)]);
(496, [EatInstr(110,531)]);
(113, [EatInstr(112,165)]);
(497, [EatInstr(103,532)]);
(114, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,166)]);
(498, [AAction2Instr(__a45,630)]);
(115, [EatInstr(99,167)]);
(499, [EatInstr(100,533)]);
(116, [EatInstr(114,168)]);
(500, [EatInstr(115,534)]);
(117, [EatInstr(101,169)]);
(501, [EatInstr(108,535)]);
(118, [EatInstr(45,170)]);
(502, [AAction2Instr(__a46,196)]);
(119, [EatInstr(111,171)]);
(503, [EatInstr(117,538)]);
(120, [EatInstr(107,172)]);
(504, [AAction2Instr(__a47,196)]);
(121, [EatInstr(45,173)]);
(505, [EatInstr(115,539)]);
(122, [EatInstr(99,174)]);
(506, [EatInstr(101,540)]);
(123, [EatInstr(110,175)]);
(507, [EatInstr(45,541)]);
(124, [AAction2Instr(__a6,176)]);
(508, [AAction2Instr(__a48,196)]);
(125, [EatInstr(116,177)]);
(509, [EatInstr(116,542)]);
(126, [EatInstr(105,178)]);
(510, [EatInstr(116,543)]);
(127, [EatInstr(110,179)]);
(511, [EatInstr(114,544)]);
(128, [EatInstr(116,180)]);
(512, [EatInstr(114,545)]);
(129, [EatInstr(114,181)]);
(513, [EatInstr(111,546)]);
(130, [EatInstr(99,182)]);
(514, [EatInstr(99,547)]);
(131, [EatInstr(115,183)]);
(515, [AAction2Instr(__a49,254)]);
(132, [EatInstr(101,184)]);
(516, [AAction2Instr(__a50,254)]);
(133, [EatInstr(117,185)]);
(517, [EatInstr(98,548)]);
(134, [EatInstr(108,186)]);
(518, [EatInstr(97,549)]);
(135, [EatInstr(111,187)]);
(519, [AAction2Instr(__a51,254)]);
(136, [EatInstr(109,188)]);
(520, [EatInstr(114,550)]);
(137, [EatInstr(45,189)]);
(521, [EatInstr(114,551)]);
(138, [EatInstr(108,190)]);
(522, [EatInstr(97,552)]);
(139, [EatInstr(101,191)]);
(523, [EatInstr(100,553)]);
(140, [EatInstr(111,192)]);
(524, [EatInstr(110,554)]);
(141, [EatInstr(114,194);EatInstr(105,193)]);
(525, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,555)]);
(142, [EatInstr(101,195)]);
(526, [EatInstr(108,556)]);
(143, [AAction2Instr(__a7,196)]);
(527, [EatInstr(110,557)]);
(144, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,197)]);
(528, [EatInstr(99,558)]);
(145, [ALookaheadInstr(false,CfgLA (3,266),93);ACallInstr3(__default_call,11);AContInstr3(273,__g0,__binder3,92);ACallInstr3(__g0,10);ASimpleCont2Instr(274,__binder0,93)]);
(529, [EatInstr(112,559)]);
(146, [EatInstr(119,198)]);
(530, [EatInstr(105,560)]);
(147, [EatInstr(105,199)]);
(531, [AAction2Instr(__a52,630)]);
(148, [EatInstr(101,200)]);
(532, [EatInstr(45,561);AAction2Instr(__a53,630)]);
(149, [EatInstr(114,201)]);
(533, [EatInstr(108,563)]);
(150, [EatInstr(103,202)]);
(534, [EatInstr(105,564)]);
(151, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,203)]);
(535, [EatInstr(115,565)]);
(152, [EatInstr(114,204)]);
(536, [AAction2Instr(__a54,537);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,536)]);
(153, [EatInstr(110,205)]);
(537, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,566)]);
(154, [EatInstr(114,206)]);
(538, [EatInstr(108,567)]);
(155, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,207)]);
(539, [EatInstr(116,568)]);
(156, [EatInstr(115,208)]);
(540, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,569)]);
(157, [EatInstr(101,209)]);
(541, [EatInstr(104,570)]);
(158, [EatInstr(97,210)]);
(542, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,571)]);
(159, [EatInstr(101,211)]);
(543, [EatInstr(111,572)]);
(160, [EatInstr(108,212)]);
(544, [EatInstr(121,573)]);
(161, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,213)]);
(545, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,574)]);
(162, [EatInstr(105,214)]);
(546, [EatInstr(110,575)]);
(163, [EatInstr(117,215)]);
(547, [EatInstr(111,576)]);
(164, [EatInstr(110,216)]);
(548, [EatInstr(108,577)]);
(165, [EatInstr(97,217)]);
(549, [EatInstr(114,578)]);
(166, [AAction2Instr(__a8,78)]);
(550, [EatInstr(97,579)]);
(167, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,218)]);
(551, [EatInstr(115,580)]);
(168, [EatInstr(97,219)]);
(552, [EatInstr(108,581)]);
(169, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,220)]);
(553, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,582)]);
(170, [EatInstr(103,221)]);
(554, [EatInstr(97,583)]);
(171, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,222)]);
(555, [AAction2Instr(__a55,78)]);
(172, [EatInstr(97,223)]);
(556, [EatInstr(101,584)]);
(173, [EatInstr(108,224)]);
(557, [EatInstr(99,585)]);
(174, [EatInstr(101,225)]);
(558, [EatInstr(116,586)]);
(175, [EatInstr(116,226)]);
(559, [EatInstr(103,587)]);
(176, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,275)]);
(560, [EatInstr(111,588)]);
(177, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,227)]);
(561, [EatInstr(115,589)]);
(178, [EatInstr(112,228)]);
(562, [AAction2Instr(__a56,196)]);
(179, [EatInstr(115,229)]);
(563, [EatInstr(101,590)]);
(180, [EatInstr(101,230)]);
(564, [EatInstr(116,591)]);
(181, [EatInstr(111,231)]);
(565, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,592)]);
(182, [EatInstr(107,232)]);
(566, [AAction2Instr(__a57,196)]);
(183, [EatInstr(101,233)]);
(567, [EatInstr(97,593)]);
(184, [EatInstr(99,234)]);
(568, [EatInstr(111,594)]);
(185, [EatInstr(110,235)]);
(569, [AAction2Instr(__a58,196)]);
(186, [EatInstr(105,236)]);
(570, [EatInstr(105,595)]);
(187, [EatInstr(107,237)]);
(571, [AAction2Instr(__a59,196)]);
(188, [EatInstr(111,238)]);
(572, [EatInstr(114,596)]);
(189, [EatInstr(115,242);EatInstr(114,241);EatInstr(109,240);EatInstr(99,239)]);
(573, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,597)]);
(190, [EatInstr(121,243)]);
(574, [AAction2Instr(__a60,598)]);
(191, [EatInstr(102,244)]);
(575, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,599)]);
(192, [EatInstr(116,245)]);
(576, [EatInstr(114,600)]);
(193, [EatInstr(116,246)]);
(577, [EatInstr(101,601)]);
(194, [EatInstr(111,247)]);
(578, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,602)]);
(195, [EatInstr(45,248)]);
(579, [EatInstr(112,603)]);
(196, [CompleteInstr(273)]);
(580, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,604)]);
(197, [AAction2Instr(__a9,196)]);
(581, [EatInstr(121,605)]);
(198, [EatInstr(45,249)]);
(582, [AAction2Instr(__a61,78)]);
(199, [EatInstr(98,250)]);
(583, [EatInstr(108,606)]);
(200, [EatInstr(45,251)]);
(584, [EatInstr(45,607)]);
(201, [EatInstr(117,252)]);
(585, [EatInstr(101,608)]);
(202, [EatInstr(97,253)]);
(586, [EatInstr(105,609)]);
(203, [AAction2Instr(__a10,254)]);
(587, [EatInstr(101,610)]);
(204, [EatInstr(45,255)]);
(588, [EatInstr(110,611)]);
(205, [EatInstr(101,256)]);
(589, [EatInstr(116,612)]);
(206, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,257)]);
(590, [EatInstr(114,613)]);
(207, [AAction2Instr(__a11,254)]);
(591, [EatInstr(105,614)]);
(208, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,258)]);
(592, [AAction2Instr(__a62,196)]);
(209, [EatInstr(100,259)]);
(593, [EatInstr(114,615)]);
(210, [EatInstr(121,260)]);
(594, [EatInstr(114,616)]);
(211, [EatInstr(116,261)]);
(595, [EatInstr(115,617)]);
(212, [EatInstr(108,262)]);
(596, [EatInstr(121,618)]);
(213, [AAction2Instr(__a12,254)]);
(597, [AAction2Instr(__a63,196)]);
(214, [EatInstr(108,263)]);
(598, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,636)]);
(215, [EatInstr(116,264)]);
(599, [AAction2Instr(__a64,254)]);
(216, [EatInstr(100,265)]);
(600, [EatInstr(101,619)]);
(217, [EatInstr(116,266)]);
(601, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,620)]);
(218, [AAction2Instr(__a13,267)]);
(602, [AAction2Instr(__a65,254)]);
(219, [EatInstr(99,268)]);
(603, [EatInstr(104,621)]);
(220, [AAction2Instr(__a14,78)]);
(604, [AAction2Instr(__a66,78)]);
(221, [EatInstr(101,269)]);
(605, [EatInstr(115,622)]);
(222, [AAction2Instr(__a15,78)]);
(606, [EatInstr(121,623)]);
(223, [EatInstr(104,270)]);
(607, [EatInstr(112,624)]);
(224, [EatInstr(111,271)]);
(608, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,625)]);
(225, [EatInstr(100,272)]);
(609, [EatInstr(111,626)]);
(226, [EatInstr(45,274);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,273)]);
(610, [EatInstr(110,627)]);
(227, [AAction2Instr(__a16,78)]);
(611, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,628)]);
(228, [EatInstr(45,277)]);
(612, [EatInstr(114,629)]);
(229, [EatInstr(108,278)]);
(613, [AAction2Instr(__a67,630)]);
(230, [EatInstr(114,279)]);
(614, [EatInstr(118,631)]);
(231, [EatInstr(119,280)]);
(615, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,632)]);
(232, [EatInstr(101,281)]);
(616, [EatInstr(121,633)]);
(233, [EatInstr(45,282)]);
(617, [EatInstr(116,634)]);
(234, [EatInstr(107,283)]);
(618, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,635)]);
(235, [EatInstr(116,284)]);
(619, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,638)]);
(236, [EatInstr(110,285)]);
(620, [AAction2Instr(__a68,254)]);
(237, [EatInstr(97,286)]);
(621, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,639)]);
(238, [EatInstr(105,287)]);
(622, [EatInstr(105,640)]);
(239, [EatInstr(111,288)]);
(623, [EatInstr(115,641)]);
(240, [EatInstr(101,289)]);
(624, [EatInstr(114,642)]);
(241, [EatInstr(101,290)]);
(625, [AAction2Instr(__a69,78)]);
(242, [EatInstr(107,291)]);
(626, [EatInstr(110,643)]);
(243, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,292)]);
(627, [EatInstr(45,645);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,644)]);
(244, [EatInstr(105,293)]);
(628, [AAction2Instr(__a70,196)]);
(245, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,294)]);
(629, [EatInstr(105,646)]);
(246, [EatInstr(45,295)]);
(630, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,562)]);
(247, [EatInstr(108,296)]);
(631, [EatInstr(101,647)]);
(248, [EatInstr(102,297)]);
(632, [AAction2Instr(__a71,196)]);
(249, [EatInstr(110,298)]);
(633, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,648)]);
(250, [EatInstr(117,299)]);
(634, [EatInstr(111,649)]);
(251, [EatInstr(117,300)]);
(635, [AAction2Instr(__a72,196)]);
(252, [EatInstr(108,301)]);
(636, [AAction2Instr(__a73,637);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,636)]);
(253, [EatInstr(114,302)]);
(637, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,650)]);
(254, [CompleteInstr(271)]);
(638, [AAction2Instr(__a74,254)]);
(255, [EatInstr(116,304)]);
(639, [AAction2Instr(__a75,78)]);
(256, [EatInstr(45,305)]);
(640, [EatInstr(115,651)]);
(257, [AAction2Instr(__a17,254)]);
(641, [EatInstr(105,652)]);
(258, [AAction2Instr(__a18,254)]);
(642, [EatInstr(101,653)]);
(259, [EatInstr(101,306)]);
(643, [EatInstr(115,654)]);
(260, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,307)]);
(644, [AAction2Instr(__a76,78)]);
(261, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,308)]);
(645, [EatInstr(115,655)]);
(262, [EatInstr(45,309)]);
(646, [EatInstr(99,656)]);
(263, [EatInstr(101,310)]);
(647, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,657)]);
(264, [EatInstr(105,311)]);
(648, [AAction2Instr(__a77,196)]);
(265, [EatInstr(101,312)]);
(649, [EatInstr(114,658)]);
(266, [EatInstr(99,313)]);
(650, [AAction2Instr(__a78,196)]);
(267, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,314)]);
(651, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,659)]);
(268, [EatInstr(116,315)]);
(652, [EatInstr(115,660)]);
(269, [EatInstr(110,316)]);
(653, [EatInstr(100,661)]);
(270, [EatInstr(101,317)]);
(654, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,662)]);
(271, [EatInstr(111,318)]);
(655, [EatInstr(99,663)]);
(272, [EatInstr(101,319)]);
(656, [EatInstr(116,664)]);
(273, [AAction2Instr(__a19,78)]);
(657, [AAction2Instr(__a79,196)]);
(274, [EatInstr(114,322);EatInstr(110,321);EatInstr(103,320)]);
(658, [EatInstr(121,665)]);
(275, [AAction2Instr(__a20,276);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,275)]);
(659, [AAction2Instr(__a80,78)]);
(276, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,323)]);
(660, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,666)]);
(277, [EatInstr(108,324)]);
(661, [EatInstr(105,667)]);
(278, [EatInstr(97,325)]);
(662, [AAction2Instr(__a81,78)]);
(279, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,326)]);
(663, [EatInstr(97,668)]);
(280, [EatInstr(45,327)]);
(664, [AAction2Instr(__a82,630)]);
(281, [EatInstr(110,328)]);
(665, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,669)]);
(282, [EatInstr(105,329)]);
(666, [AAction2Instr(__a83,78)]);
(283, [EatInstr(45,330)]);
(667, [EatInstr(99,670)]);
(284, [EatInstr(101,331)]);
(668, [EatInstr(110,671)]);
(285, [EatInstr(101,332)]);
(669, [AAction2Instr(__a84,196)]);
(286, [EatInstr(104,333)]);
(670, [EatInstr(97,672)]);
(287, [EatInstr(122,334)]);
(671, [EatInstr(110,673)]);
(288, [EatInstr(97,335)]);
(672, [EatInstr(116,674)]);
(289, [EatInstr(109,336)]);
(673, [EatInstr(101,675)]);
(290, [EatInstr(112,337)]);
(674, [EatInstr(101,676)]);
(291, [EatInstr(105,338)]);
(675, [EatInstr(114,677)]);
(292, [AAction2Instr(__a21,196)]);
(676, [EatInstr(115,678)]);
(293, [EatInstr(120,339)]);
(677, [EatInstr(108,679)]);
(294, [AAction2Instr(__a22,340)]);
(678, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,680)]);
(295, [EatInstr(104,341)]);
(679, [EatInstr(101,681)]);
(296, [EatInstr(108,342)]);
(680, [AAction2Instr(__a85,78)]);
(297, [EatInstr(115,343)]);
(681, [EatInstr(115,682)]);
(298, [EatInstr(111,344)]);
(682, [EatInstr(115,683)]);
(299, [EatInstr(116,345)]);
(683, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,684)]);
(300, [EatInstr(110,346)]);
(684, [AAction2Instr(__a86,78)]);
(301, [EatInstr(101,347)]);
(302, [EatInstr(45,349);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,348)]);
(304, [EatInstr(121,350)]);
(305, [EatInstr(114,352);EatInstr(110,351)]);
(306, [EatInstr(110,353)]);
(307, [AAction2Instr(__a23,254)]);
(308, [AAction2Instr(__a24,254)]);
(309, [EatInstr(115,354)]);
(310, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,355)]);
(311, [EatInstr(110,356)]);
(312, [EatInstr(110,357)]);
(313, [EatInstr(104,358)]);
(314, [AAction2Instr(__a25,359)]);
(315, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,360)]);
(316, [EatInstr(101,361)]);
(317, [EatInstr(97,362)]);
(318, [EatInstr(107,363)]);
(319, [EatInstr(110,364)]);
(320, [EatInstr(105,365)]);
(321, [EatInstr(117,367);EatInstr(112,366)]);
(322, [EatInstr(101,368)]);
(323, [AAction2Instr(__a26,78)]);
(324, [EatInstr(97,369)]);
(325, [EatInstr(116,370)]);
(326, [AContInstr3(271,__g0,__binder4,371);ACallInstr3(__g0,8)]);
(327, [EatInstr(110,372)]);
(328, [EatInstr(100,373)]);
(329, [EatInstr(110,374)]);
(330, [EatInstr(108,375)]);
(331, [EatInstr(114,376)]);
(332, [EatInstr(45,377)]);
(333, [EatInstr(101,378)]);
(334, [EatInstr(101,379)]);
(335, [EatInstr(108,380)]);
(336, [EatInstr(111,381)]);
(337, [EatInstr(108,382)]);
(338, [EatInstr(112,383)]);
(339, [EatInstr(45,384)]);
(340, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,385)]);
(341, [EatInstr(105,386)]);
(342, [EatInstr(45,387)]);
(343, [EatInstr(116,389);EatInstr(109,388)]);
(344, [EatInstr(116,390)]);
(345, [EatInstr(101,391)]);
(346, [EatInstr(100,392)]);
(347, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,393)]);
(348, [AAction2Instr(__a27,254)]);
(349, [EatInstr(103,394)]);
(350, [EatInstr(112,395)]);
(351, [EatInstr(117,396)]);
(352, [EatInstr(101,397)]);
(353, [EatInstr(99,398)]);
(354, [EatInstr(116,399)]);
(355, [AAction2Instr(__a28,78)]);
(356, [EatInstr(101,400)]);
(357, [EatInstr(99,401)]);
(358, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,402)]);
(359, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,403)]);
(360, [AAction2Instr(__a29,78)]);
(361, [EatInstr(114,404)]);
(362, [EatInstr(100,405)]);
(363, [EatInstr(97,406)]);
(364, [EatInstr(99,407)]);
(365, [EatInstr(108,408)]);
(366, [EatInstr(114,409)]);
(367, [EatInstr(108,410)]);
(368, [EatInstr(108,411)]);
(369, [EatInstr(116,412)]);
(370, [EatInstr(101,413)]);
(371, [AAction2Instr(__a30,196)]);
(372, [EatInstr(111,414)]);
(373, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,415)]);
(374, [EatInstr(115,416)]);
(375, [EatInstr(97,417)]);
(376, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,418)]);
(377, [EatInstr(114,420);EatInstr(99,419)]);
(378, [EatInstr(97,421)]);
(379, [EatInstr(45,422)]);
(380, [EatInstr(101,423)]);
(381, [EatInstr(105,424)]);
(382, [EatInstr(97,425)]);
]

let start_symb = get_symb_action "cmd-line-args"

module P2__ = Yak.Engine.Full_yakker (Yak.Engine.Scannerless_term_lang)
                                     (struct type t = sv let cmp = sv_compare type idata = Yk_History.Root_id_set.t
  let create_idata () = Yk_History.Root_id_set.empty
  let inspect h s = Yk_History.add_id_set h#get_root s
  let summarize_inspection s = string_of_int (Yk_History.Root_id_set.cardinal s) end)

let _wfe_data_ = Yak.PamJIT.DNELR.to_table (Yak.Pam_internal.load_internal_program program)
  start_symb (get_symb_start start_symb) 264 num_symbols
  __default_call __default_ret

let parse = Yak.Pami.Wfe.mk_parse P2__.parse _wfe_data_ sv0 _replay_cmd_line_args
let visualize = parse
let visualize_file = Yak.Pami.Simple.parse_file visualize
let visualize_string = Yak.Pami.Simple.parse_string visualize

let parse_file = Yak.Pami.Simple.parse_file parse
let parse_string = Yak.Pami.Simple.parse_string parse
;;


let args_ykbuf = Yak.YkBuf.strings2buf Sys.argv 1
let what_arg = Yak.YkBuf.stringsposn2string Sys.argv 1
let process() =
    (try
      ignore(parse args_ykbuf); (!cmd, List.rev !files, List.rev !roots, !backend)
    with
      Yak.Pami.Parse_error("Error at byte.", i, _, _) ->
        let bad_arg = what_arg i in
        let bad = Sys.argv.(bad_arg) in
        if bad_arg = 1 then
          raise(Failure(Printf.sprintf "Unrecognized command '%s'" bad))
        else if String.length bad > 0 && String.get bad 0 = '-' then
          raise(Failure(Printf.sprintf "Unrecognized option '%s'" bad))
        else
          raise(Failure(Printf.sprintf "Bad argument '%s'" bad)))
