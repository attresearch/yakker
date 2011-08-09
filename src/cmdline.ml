
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
 | (2022) -> ((); ();  Dependency_graph_cmd )
 | (2023) -> ((); ();  Dispatch_cmd )
 | (2024) -> ((); ();  Dot_cmd )
 | (2025) -> ((); (); (let _x4 = _p() in (); (let _x3 = _p() in (let f = Yak.YkBuf.get_string _x4 _x3 ykinput in (let l = (let _x8 = (let rec _x19 _x8 =
(match _n() with (2029) -> _x8 | _ (*2028*) ->
 _x19((let _x7 = (); (let _x6 = _p() in (); (let _x5 = _p() in (let x = Yak.YkBuf.get_string _x6 _x5 ykinput in x))) in _x7::_x8)))
in _x19(Yak.Util.nil)) in (List.rev _x8)) in ();  files := f::!files; exec_l := l; Exec_cmd )))))
 | (2032) -> ((); ();  Extract_cmd )
 | (2033) -> ((); ();  Compileopt.coalesce := true; Fuse_cmd )
 | (2034) -> ((); ();  Generators_analysis_cmd )
 | (2035) -> ((); ();  Info_cmd )
 | (2036) -> ((); ();  Lookahead_analysis_cmd )
 | (2037) -> ((); ();  Lr1_lookahead_cmd )
 | (2038) -> ((); ();  Precedence_analysis_cmd )
 | (2039) -> ((); ();  Print_gul_cmd )
 | (2040) -> ((); ();  Print_gil_cmd )
 | (2041) -> ((); ();  Print_npreds_cmd )
 | (2042) -> ((); ();  Print_npreds_cmd )
 | (2043) -> ((); ();  Print_relevance_cmd )
 | (2044) -> ((); (let _x10 = _p() in (); (let _x9 = _p() in (let n = Yak.YkBuf.get_string _x10 _x9 ykinput in ();  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" ))))
 | (2047) -> ((); ();  Sort_cmd )
 | (2048) -> ((); ();  Strip_late_actions_cmd )
 | (2049) -> ((); ();  Translate_dypgen_cmd )
 | (2050) -> ((); ();  Translate_dypgen_scannerless_cmd )
 | _ -> raise Exit)
and
 _r_args(_n,_p,ykinput) = (match _n() with
 | (2051) -> ((); (); (let p = _r_phases(_n,_p,ykinput) in  after := Some p ; ()))
 | (2052) -> ((); ();  Compileopt.use_coroutines := false ; ())
 | (2053) -> ((); (); (let b = (match _n() with
 | (2054) -> ((); Fun_BE)
 | (2055) -> ((); Trans_BE)
 | (2056) -> ((); Wadler_BE)
 | (2057) -> ((); Peg_BE false)
 | (2058) -> ((); Peg_BE true)
 | _ -> raise Exit) in ();  backend := b ; ()))
 | (2059) -> ((); ();  Compileopt.case_sensitive := false ; ())
 | (2060) -> ((); ();  Compileopt.check_labels := true ; ())
 | (2061) -> ((); (); (let _x12 = _p() in (); (let _x11 = _p() in (let n = Yak.YkBuf.get_string _x12 _x11 ykinput in ();  Variables.counter := (int_of_string n) ; ()))))
 | (2064) -> ((); ();  Compileopt.inline_cs := true ; ())
 | (2065) -> ((); ();  Compileopt.inline_regular := true ; ())
 | (2066) -> ((); ();  Compileopt.lookahead := true ; ())
 | (2067) -> ((); ();  Compileopt.memoize_history := true ; ())
 | (2068) -> ((); ();  Compileopt.coalesce := false ; ())
 | (2069) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2070) -> ((); ();  Compileopt.repress_replay := true ; ())
 | (2071) -> ((); ();  Compileopt.skip_opt := false ; ())
 | (2072) -> ((); ();  only := true ; ())
 | (2073) -> ((); ();  Compileopt.postfix_history := false ; ())
 | (2074) -> ((); (); (let _x14 = _p() in (); (let _x13 = _p() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2077) -> ((); ();  Compileopt.unit_history := true ; ())
 | (2078) -> ((); (); (let _x16 = _p() in (); (let _x15 = _p() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  Compileopt.unroll_star_n := (int_of_string n) ; ()))))
 | (2081) -> ((); ();  Compileopt.use_fsm := true ; ())
 | (2082) -> ((); ();  Compileopt.use_fsm := false ; ())
 | (2083) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2084) -> ((let _x18 = _p() in (); (let _x17 = _p() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
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
 | (2025) -> (();();();push((2029)); while (match _n() with (2028) -> true | _ (*2029*)-> false) do
 ();();();push_pos(_p());();push_pos(_p());(); push((2028))
done
;();push_pos(_p());();push_pos(_p());();(); push((2025)))
 | (2032) -> (();();(); push((2032)))
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
 | (2044) -> (();();();push_pos(_p());();push_pos(_p());(); push((2044)))
 | (2047) -> (();();(); push((2047)))
 | (2048) -> (();();(); push((2048)))
 | (2049) -> (();();(); push((2049)))
 | (2050) -> (();();(); push((2050)))
 | _ -> raise Exit)
and _rv_args() = (match _n() with
 | (2051) -> (();();_rv_phases();();(); push((2051)))
 | (2052) -> (();();();(); push((2052)))
 | (2053) -> (();();();(match _n() with
 | (2054) -> (();(); push((2054)))
 | (2055) -> (();(); push((2055)))
 | (2056) -> (();(); push((2056)))
 | (2057) -> (();(); push((2057)))
 | (2058) -> (();(); push((2058)))
 | _ -> raise Exit);();(); push((2053)))
 | (2059) -> (();();();(); push((2059)))
 | (2060) -> (();();();(); push((2060)))
 | (2061) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2061)))
 | (2064) -> (();();();(); push((2064)))
 | (2065) -> (();();();(); push((2065)))
 | (2066) -> (();();();(); push((2066)))
 | (2067) -> (();();();(); push((2067)))
 | (2068) -> (();();();(); push((2068)))
 | (2069) -> (();();();(); push((2069)))
 | (2070) -> (();();();(); push((2070)))
 | (2071) -> (();();();(); push((2071)))
 | (2072) -> (();();();(); push((2072)))
 | (2073) -> (();();();(); push((2073)))
 | (2074) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2074)))
 | (2077) -> (();();();(); push((2077)))
 | (2078) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2078)))
 | (2081) -> (();();();(); push((2081)))
 | (2082) -> (();();();(); push((2082)))
 | (2083) -> (();();();(); push((2083)))
 | (2084) -> (();();();();push_pos(_p());();push_pos(_p()); push((2084)))
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
let __a66 = (_p 1375 ((2056)));;
let __a53 = (_p 1412 ((2063)));;
let __a49 = (_p 1069 ((2009)));;
let __a5 = (_p 1012 ((2000)));;
let __a74 = (_p 1161 ((2022)));;
let __a43 = (fun _x0_ _x1_ -> (((_p 1206 ((2028))) _x0_) (((_p 1200 ((2031))) _x0_) _x1_)));;
let __a6 = (_p 1301 ((2045)));;
let __a33 = (_p 1168 ((2023)));;
let __a34 = (_p 1197 ((2030)));;
let __a16 = (_p 1317 ((2047)));;
let __a78 = (_p 1398 ((2059)));;
let __a62 = (_p 1498 ((2077)));;
let __a35 = (_p 1212 ((2025)));;
let __a58 = (_p 1466 ((2071)));;
let __a1 = (_p 1533 ((2085)));;
let __a8 = (_p 1175 ((2024)));;
let __a26 = (_p 1310 ((2044)));;
let __a80 = (_p 1324 ((2048)));;
let __a30 = (_p 1348 ((2051)));;
let __a76 = (_p 1442 ((2067)));;
let __a18 = (_p 1104 ((2014)));;
let __a29 = (_p 1219 ((2032)));;
let __a2 = (_p 1006 ((2001)));;
let __a52 = (_p 1387 ((2058)));;
let __a75 = (_p 1331 ((2049)));;
let __a9 = (_p 1542 ((2084)));;
let __a39 = (_p 1275 ((2040)));;
let __a38 = (_p 1524 ((2082)));;
let __a42 = (_p 1111 ((2015)));;
let __a14 = (_p 1226 ((2033)));;
let __a45 = (_p 1424 ((2064)));;
let __a20 = (_p 1304 ((2046)));;
let __a72 = (_p 1506 ((2080)));;
let __a44 = (_p 1369 ((2055)));;
let __a54 = (_p 1282 ((2041)));;
let __a24 = (_p 1118 ((2016)));;
let __a71 = (_p 1478 ((2073)));;
let __a65 = (_p 1233 ((2034)));;
let __a84 = (_p 1289 ((2042)));;
let __a4 = (_p 1536 ((2086)));;
let __a83 = (_p 1454 ((2069)));;
let __a50 = (_p 1125 ((2017)));;
let __a41 = (_p 1020 ((2002)));;
let __a15 = (_p 1240 ((2035)));;
let __a64 = (_p 1076 ((2010)));;
let __a73 = (_p 1027 ((2003)));;
let __a68 = (_p 1296 ((2043)));;
let __a47 = (_p 1460 ((2070)));;
let __a22 = (_p 1483 ((2075)));;
let __a79 = (_p 1247 ((2036)));;
let __a46 = (_p 1436 ((2066)));;
let __a67 = (_p 1083 ((2011)));;
let __a61 = (_p 1404 ((2060)));;
let __a32 = (_p 1034 ((2004)));;
let __a81 = (_p 1381 ((2057)));;
let __a60 = (_p 1254 ((2037)));;
let __a40 = (_p 1492 ((2074)));;
let __a59 = (_p 1503 ((2079)));;
let __a25 = (fun _x0_ _x1_ -> (((_p 1190 ((2029))) _x0_) (((_p 1184 ((2027))) _x0_) _x1_)));;
let __a17 = (_p 1090 ((2012)));;
let __a37 = (_p 1518 ((2081)));;
let __a36 = (_p 1409 ((2062)));;
let __a27 = (_p 1041 ((2005)));;
let __a82 = (_p 1261 ((2038)));;
let __a51 = (_p 1363 ((2054)));;
let __a11 = (_p 1097 ((2013)));;
let __a13 = (_p 1181 ((2026)));;
let __a85 = (_p 1338 ((2050)));;
let __a77 = (_p 1512 ((2078)));;
let __a48 = (_p 1048 ((2006)));;
let __a19 = (_p 1268 ((2039)));;
let __a21 = (_p 1472 ((2072)));;
let __a56 = (_p 1418 ((2061)));;
let __a31 = (_p 1486 ((2076)));;
let __a69 = (_p 1354 ((2052)));;
let __a57 = (_p 1448 ((2068)));;
let __a55 = (_p 1392 ((2053)));;
let __a12 = (_p 1132 ((2018)));;
let __g0 = (_e);;
let __a3 = (_p 1147 ((2020)));;
let __a10 = (_p 1055 ((2007)));;
let __a23 = (_p 1139 ((2019)));;
let __a63 = (_p 1062 ((2008)));;
let __a28 = (_p 1154 ((2021)));;
let __a70 = (_p 1430 ((2065)));;
let __a7 = (_p 1530 ((2083)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1002);;
let __binder2 = (_m 1144);;
let __binder3 = (_m 1011);;
let __binder4 = (_m 1345);;
open Yak.Pam_internal
let program = [
(383, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,425)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(97,426)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(115,427)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(101,428)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [AAction2Instr(__a32,251)]);
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [EatInstr(105,429)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(101,430)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,44)]);
(390, [EatInstr(108,431)]);
(7, [EatInstr(127,46);EatInstr(126,46);EatInstr(125,46);EatInstr(124,46);EatInstr(123,46);EatInstr(122,46);EatInstr(121,46);EatInstr(120,46);EatInstr(119,46);EatInstr(118,46);EatInstr(117,46);EatInstr(116,46);EatInstr(115,46);EatInstr(114,46);EatInstr(113,46);EatInstr(112,46);EatInstr(111,46);EatInstr(110,46);EatInstr(109,46);EatInstr(108,46);EatInstr(107,46);EatInstr(106,46);EatInstr(105,46);EatInstr(104,46);EatInstr(103,46);EatInstr(102,46);EatInstr(101,46);EatInstr(100,46);EatInstr(99,46);EatInstr(98,46);EatInstr(97,46);EatInstr(96,46);EatInstr(95,46);EatInstr(94,46);EatInstr(93,46);EatInstr(92,46);EatInstr(91,46);EatInstr(90,46);EatInstr(89,46);EatInstr(88,46);EatInstr(87,46);EatInstr(86,46);EatInstr(85,46);EatInstr(84,46);EatInstr(83,46);EatInstr(82,46);EatInstr(81,46);EatInstr(80,46);EatInstr(79,46);EatInstr(78,46);EatInstr(77,46);EatInstr(76,46);EatInstr(75,46);EatInstr(74,46);EatInstr(73,46);EatInstr(72,46);EatInstr(71,46);EatInstr(70,46);EatInstr(69,46);EatInstr(68,46);EatInstr(67,46);EatInstr(66,46);EatInstr(65,46);EatInstr(64,46);EatInstr(63,46);EatInstr(62,46);EatInstr(61,46);EatInstr(60,46);EatInstr(59,46);EatInstr(58,46);EatInstr(57,46);EatInstr(56,46);EatInstr(55,46);EatInstr(54,46);EatInstr(53,46);EatInstr(52,46);EatInstr(51,46);EatInstr(50,46);EatInstr(49,46);EatInstr(48,46);EatInstr(47,46);EatInstr(46,46);EatInstr(44,46);EatInstr(43,46);EatInstr(42,46);EatInstr(41,46);EatInstr(40,46);EatInstr(39,46);EatInstr(38,46);EatInstr(37,46);EatInstr(36,46);EatInstr(35,46);EatInstr(34,46);EatInstr(33,46);EatInstr(32,46);EatInstr(31,46);EatInstr(30,46);EatInstr(29,46);EatInstr(28,46);EatInstr(27,46);EatInstr(26,46);EatInstr(25,46);EatInstr(24,46);EatInstr(23,46);EatInstr(22,46);EatInstr(21,46);EatInstr(20,46);EatInstr(19,46);EatInstr(18,46);EatInstr(17,46);EatInstr(16,46);EatInstr(15,46);EatInstr(14,46);EatInstr(13,46);EatInstr(12,46);EatInstr(11,46);EatInstr(10,46);EatInstr(9,46);EatInstr(8,46);EatInstr(7,46);EatInstr(6,46);EatInstr(5,46);EatInstr(4,46);EatInstr(3,46);EatInstr(2,46);EatInstr(1,46)]);
(391, [EatInstr(103,432)]);
(8, [EatInstr(119,28);EatInstr(117,27);EatInstr(115,26);EatInstr(114,25);EatInstr(112,24);EatInstr(109,23);EatInstr(108,22);EatInstr(105,21);EatInstr(104,20);EatInstr(100,19);EatInstr(99,18);EatInstr(97,17)]);
(392, [EatInstr(101,433)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(103,33);EatInstr(102,32);EatInstr(101,31);EatInstr(100,30);EatInstr(99,29);AContInstr3(271,__g0,__binder2,40);ACallInstr3(__g0,8)]);
(393, [EatInstr(97,434)]);
(10, [EatInstr(45,41);AAction2Instr(__a1,42)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43)]);
(394, [EatInstr(121,435)]);
(395, [AAction2Instr(__a33,78)]);
(12, [CompleteInstr(264)]);
(396, [AAction2Instr(__a35,78);AAction2Instr(__a34,436)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(97,437)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(45,438)]);
(15, [AAction2Instr(__a2,144)]);
(399, [EatInstr(104,439)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(101,440)]);
(17, [EatInstr(116,49);EatInstr(114,48)]);
(401, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,441)]);
(18, [EatInstr(111,51);EatInstr(108,50)]);
(402, [EatInstr(101,442)]);
(19, [EatInstr(101,52)]);
(403, [EatInstr(108,443)]);
(20, [EatInstr(97,53)]);
(404, [EatInstr(101,444)]);
(21, [EatInstr(110,54)]);
(405, [EatInstr(101,445)]);
(22, [EatInstr(105,56);EatInstr(101,55)]);
(406, [EatInstr(45,446)]);
(23, [EatInstr(105,57)]);
(407, [EatInstr(116,447)]);
(24, [EatInstr(114,58)]);
(408, [EatInstr(119,451);EatInstr(116,450);EatInstr(112,449);EatInstr(102,448)]);
(25, [EatInstr(101,59)]);
(409, [EatInstr(101,452)]);
(26, [EatInstr(117,60)]);
(410, [EatInstr(98,453)]);
(27, [EatInstr(110,61)]);
(411, [AAction2Instr(__a36,454)]);
(28, [EatInstr(114,62)]);
(412, [EatInstr(115,455)]);
(29, [EatInstr(111,63)]);
(413, [EatInstr(101,456)]);
(30, [EatInstr(111,66);EatInstr(105,65);EatInstr(101,64)]);
(414, [EatInstr(100,457)]);
(31, [EatInstr(120,67)]);
(415, [EatInstr(104,458)]);
(32, [EatInstr(117,68)]);
(416, [EatInstr(115,459)]);
(33, [EatInstr(101,69)]);
(417, [EatInstr(122,460)]);
(34, [EatInstr(110,70)]);
(418, [EatInstr(121,461)]);
(35, [EatInstr(114,72);EatInstr(111,71)]);
(419, [EatInstr(111,462)]);
(36, [EatInstr(114,73)]);
(420, [EatInstr(105,463)]);
(37, [EatInstr(102,74)]);
(421, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,464)]);
(38, [EatInstr(116,76);EatInstr(111,75)]);
(422, [EatInstr(116,465)]);
(39, [EatInstr(114,77)]);
(423, [EatInstr(116,466)]);
(40, [AAction2Instr(__a3,78)]);
(424, [AAction2Instr(__a37,194)]);
(41, [EatInstr(118,90);EatInstr(117,89);EatInstr(114,88);EatInstr(112,87);EatInstr(111,86);EatInstr(110,85);EatInstr(109,84);EatInstr(108,83);EatInstr(105,82);EatInstr(99,81);EatInstr(98,80);EatInstr(97,79)]);
(425, [AAction2Instr(__a38,194)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,91)]);
(426, [EatInstr(116,467)]);
(43, [CompleteInstr(274)]);
(427, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,468)]);
(44, [ALookaheadInstr(false,CfgLA (1,264),45);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,44)]);
(428, [EatInstr(114,469)]);
(45, [CompleteInstr(269)]);
(429, [EatInstr(108,470)]);
(46, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,46)]);
(430, [EatInstr(115,471)]);
(47, [CompleteInstr(270)]);
(431, [EatInstr(108,472)]);
(48, [EatInstr(114,94)]);
(432, [EatInstr(117,473)]);
(49, [EatInstr(116,95)]);
(433, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,474)]);
(50, [EatInstr(111,96)]);
(434, [EatInstr(114,475)]);
(51, [EatInstr(112,97)]);
(435, [EatInstr(45,476)]);
(52, [EatInstr(115,98)]);
(436, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,477)]);
(53, [EatInstr(115,99)]);
(437, [EatInstr(116,478)]);
(54, [EatInstr(108,101);EatInstr(102,100)]);
(438, [EatInstr(97,479)]);
(55, [EatInstr(120,102)]);
(439, [EatInstr(101,480)]);
(56, [EatInstr(102,103)]);
(440, [EatInstr(45,481)]);
(57, [EatInstr(110,104)]);
(441, [AAction2Instr(__a39,78)]);
(58, [EatInstr(101,105)]);
(442, [EatInstr(100,482)]);
(59, [EatInstr(112,106)]);
(443, [EatInstr(97,483)]);
(60, [EatInstr(98,107)]);
(444, [EatInstr(118,484)]);
(61, [EatInstr(114,108)]);
(445, [EatInstr(45,485)]);
(62, [EatInstr(97,109)]);
(446, [EatInstr(100,486)]);
(63, [EatInstr(109,110)]);
(447, [EatInstr(97,487)]);
(64, [EatInstr(112,111)]);
(448, [EatInstr(117,488)]);
(65, [EatInstr(115,112)]);
(449, [EatInstr(101,489)]);
(66, [EatInstr(116,113)]);
(450, [EatInstr(120,490)]);
(67, [EatInstr(116,115);EatInstr(101,114)]);
(451, [EatInstr(97,491)]);
(68, [EatInstr(115,116)]);
(452, [EatInstr(110,492)]);
(69, [EatInstr(116,117)]);
(453, [EatInstr(101,493)]);
(70, [EatInstr(102,118)]);
(454, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,528)]);
(71, [EatInstr(111,119)]);
(455, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,494)]);
(72, [EatInstr(49,120)]);
(456, [EatInstr(103,495)]);
(73, [EatInstr(105,122);EatInstr(101,121)]);
(457, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,496)]);
(74, [EatInstr(99,123)]);
(458, [EatInstr(105,497)]);
(75, [EatInstr(114,124)]);
(459, [EatInstr(99,498)]);
(76, [EatInstr(114,125)]);
(460, [EatInstr(101,499)]);
(77, [EatInstr(97,126)]);
(461, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,500)]);
(78, [CompleteInstr(272)]);
(462, [EatInstr(112,501)]);
(79, [EatInstr(114,128);EatInstr(102,127)]);
(463, [EatInstr(115,502)]);
(80, [EatInstr(97,129)]);
(464, [AAction2Instr(__a40,194)]);
(81, [EatInstr(111,132);EatInstr(104,131);EatInstr(97,130)]);
(465, [EatInstr(111,503)]);
(82, [EatInstr(110,133)]);
(466, [EatInstr(97,504)]);
(83, [EatInstr(111,134)]);
(467, [EatInstr(105,505)]);
(84, [EatInstr(101,135)]);
(468, [AAction2Instr(__a41,251)]);
(85, [EatInstr(111,136)]);
(469, [EatInstr(45,506)]);
(86, [EatInstr(110,137)]);
(470, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,507)]);
(87, [EatInstr(114,138)]);
(471, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(88, [EatInstr(111,139)]);
(472, [EatInstr(97,509)]);
(89, [EatInstr(115,141);EatInstr(110,140)]);
(473, [EatInstr(108,510)]);
(90, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,142)]);
(474, [AAction2Instr(__a42,251)]);
(91, [AAction2Instr(__a4,143)]);
(475, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,511)]);
(92, [AAction2Instr(__a5,144)]);
(476, [EatInstr(103,512)]);
(93, [CompleteInstr(267)]);
(477, [AAction2Instr(__a43,353)]);
(94, [EatInstr(111,145)]);
(478, [EatInstr(111,513)]);
(95, [EatInstr(114,146)]);
(479, [EatInstr(110,514)]);
(96, [EatInstr(115,147)]);
(480, [EatInstr(97,515)]);
(97, [EatInstr(121,148)]);
(481, [EatInstr(97,516)]);
(98, [EatInstr(117,149)]);
(482, [EatInstr(115,517)]);
(99, [EatInstr(104,150)]);
(483, [EatInstr(98,518)]);
(100, [EatInstr(101,151)]);
(484, [EatInstr(97,519)]);
(101, [EatInstr(105,152)]);
(485, [EatInstr(97,520)]);
(102, [EatInstr(101,153)]);
(486, [EatInstr(121,521)]);
(103, [EatInstr(116,154)]);
(487, [EatInstr(116,522)]);
(104, [EatInstr(117,155)]);
(488, [EatInstr(110,523)]);
(105, [EatInstr(99,156)]);
(489, [EatInstr(103,524)]);
(106, [EatInstr(108,157)]);
(490, [AAction2Instr(__a44,622)]);
(107, [EatInstr(115,158)]);
(491, [EatInstr(100,525)]);
(108, [EatInstr(111,159)]);
(492, [EatInstr(115,526)]);
(109, [EatInstr(112,160)]);
(493, [EatInstr(108,527)]);
(110, [EatInstr(112,161)]);
(494, [AAction2Instr(__a45,194)]);
(111, [EatInstr(101,162)]);
(495, [EatInstr(117,530)]);
(112, [EatInstr(112,163)]);
(496, [AAction2Instr(__a46,194)]);
(113, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,164)]);
(497, [EatInstr(115,531)]);
(114, [EatInstr(99,165)]);
(498, [EatInstr(101,532)]);
(115, [EatInstr(114,166)]);
(499, [EatInstr(45,533)]);
(116, [EatInstr(101,167)]);
(500, [AAction2Instr(__a47,194)]);
(117, [EatInstr(45,168)]);
(501, [EatInstr(116,534)]);
(118, [EatInstr(111,169)]);
(502, [EatInstr(116,535)]);
(119, [EatInstr(107,170)]);
(503, [EatInstr(114,536)]);
(120, [EatInstr(45,171)]);
(504, [EatInstr(114,537)]);
(121, [EatInstr(99,172)]);
(505, [EatInstr(111,538)]);
(122, [EatInstr(110,173)]);
(506, [EatInstr(99,539)]);
(123, [AAction2Instr(__a6,174)]);
(507, [AAction2Instr(__a48,251)]);
(124, [EatInstr(116,175)]);
(508, [AAction2Instr(__a49,251)]);
(125, [EatInstr(105,176)]);
(509, [EatInstr(98,540)]);
(126, [EatInstr(110,177)]);
(510, [EatInstr(97,541)]);
(127, [EatInstr(116,178)]);
(511, [AAction2Instr(__a50,251)]);
(128, [EatInstr(114,179)]);
(512, [EatInstr(114,542)]);
(129, [EatInstr(99,180)]);
(513, [EatInstr(114,543)]);
(130, [EatInstr(115,181)]);
(514, [EatInstr(97,544)]);
(131, [EatInstr(101,182)]);
(515, [EatInstr(100,545)]);
(132, [EatInstr(117,183)]);
(516, [EatInstr(110,546)]);
(133, [EatInstr(108,184)]);
(517, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,547)]);
(134, [EatInstr(111,185)]);
(518, [EatInstr(108,548)]);
(135, [EatInstr(109,186)]);
(519, [EatInstr(110,549)]);
(136, [EatInstr(45,187)]);
(520, [EatInstr(99,550)]);
(137, [EatInstr(108,188)]);
(521, [EatInstr(112,551)]);
(138, [EatInstr(101,189)]);
(522, [EatInstr(105,552)]);
(139, [EatInstr(111,190)]);
(523, [AAction2Instr(__a51,622)]);
(140, [EatInstr(114,192);EatInstr(105,191)]);
(524, [EatInstr(45,553);AAction2Instr(__a52,622)]);
(141, [EatInstr(101,193)]);
(525, [EatInstr(108,555)]);
(142, [AAction2Instr(__a7,194)]);
(526, [EatInstr(105,556)]);
(143, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,195)]);
(527, [EatInstr(115,557)]);
(144, [ALookaheadInstr(false,CfgLA (3,266),93);ACallInstr3(__default_call,11);AContInstr3(273,__g0,__binder3,92);ACallInstr3(__g0,10);ASimpleCont2Instr(274,__binder0,93)]);
(528, [AAction2Instr(__a53,529);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,528)]);
(145, [EatInstr(119,196)]);
(529, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,558)]);
(146, [EatInstr(105,197)]);
(530, [EatInstr(108,559)]);
(147, [EatInstr(101,198)]);
(531, [EatInstr(116,560)]);
(148, [EatInstr(114,199)]);
(532, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,561)]);
(149, [EatInstr(103,200)]);
(533, [EatInstr(104,562)]);
(150, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,201)]);
(534, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,563)]);
(151, [EatInstr(114,202)]);
(535, [EatInstr(111,564)]);
(152, [EatInstr(110,203)]);
(536, [EatInstr(121,565)]);
(153, [EatInstr(114,204)]);
(537, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,566)]);
(154, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,205)]);
(538, [EatInstr(110,567)]);
(155, [EatInstr(115,206)]);
(539, [EatInstr(111,568)]);
(156, [EatInstr(101,207)]);
(540, [EatInstr(108,569)]);
(157, [EatInstr(97,208)]);
(541, [EatInstr(114,570)]);
(158, [EatInstr(101,209)]);
(542, [EatInstr(97,571)]);
(159, [EatInstr(108,210)]);
(543, [EatInstr(115,572)]);
(160, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,211)]);
(544, [EatInstr(108,573)]);
(161, [EatInstr(105,212)]);
(545, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,574)]);
(162, [EatInstr(110,213)]);
(546, [EatInstr(97,575)]);
(163, [EatInstr(97,214)]);
(547, [AAction2Instr(__a54,78)]);
(164, [AAction2Instr(__a8,78)]);
(548, [EatInstr(101,576)]);
(165, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,215)]);
(549, [EatInstr(99,577)]);
(166, [EatInstr(97,216)]);
(550, [EatInstr(116,578)]);
(167, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,217)]);
(551, [EatInstr(103,579)]);
(168, [EatInstr(103,218)]);
(552, [EatInstr(111,580)]);
(169, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,219)]);
(553, [EatInstr(115,581)]);
(170, [EatInstr(97,220)]);
(554, [AAction2Instr(__a55,194)]);
(171, [EatInstr(108,221)]);
(555, [EatInstr(101,582)]);
(172, [EatInstr(101,222)]);
(556, [EatInstr(116,583)]);
(173, [EatInstr(116,223)]);
(557, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,584)]);
(174, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,271)]);
(558, [AAction2Instr(__a56,194)]);
(175, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,224)]);
(559, [EatInstr(97,585)]);
(176, [EatInstr(112,225)]);
(560, [EatInstr(111,586)]);
(177, [EatInstr(115,226)]);
(561, [AAction2Instr(__a57,194)]);
(178, [EatInstr(101,227)]);
(562, [EatInstr(105,587)]);
(179, [EatInstr(111,228)]);
(563, [AAction2Instr(__a58,194)]);
(180, [EatInstr(107,229)]);
(564, [EatInstr(114,588)]);
(181, [EatInstr(101,230)]);
(565, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,589)]);
(182, [EatInstr(99,231)]);
(566, [AAction2Instr(__a59,590)]);
(183, [EatInstr(110,232)]);
(567, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,591)]);
(184, [EatInstr(105,233)]);
(568, [EatInstr(114,592)]);
(185, [EatInstr(107,234)]);
(569, [EatInstr(101,593)]);
(186, [EatInstr(111,235)]);
(570, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,594)]);
(187, [EatInstr(115,239);EatInstr(114,238);EatInstr(109,237);EatInstr(99,236)]);
(571, [EatInstr(112,595)]);
(188, [EatInstr(121,240)]);
(572, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,596)]);
(189, [EatInstr(102,241)]);
(573, [EatInstr(121,597)]);
(190, [EatInstr(116,242)]);
(574, [AAction2Instr(__a60,78)]);
(191, [EatInstr(116,243)]);
(575, [EatInstr(108,598)]);
(192, [EatInstr(111,244)]);
(576, [EatInstr(45,599)]);
(193, [EatInstr(45,245)]);
(577, [EatInstr(101,600)]);
(194, [CompleteInstr(273)]);
(578, [EatInstr(105,601)]);
(195, [AAction2Instr(__a9,194)]);
(579, [EatInstr(101,602)]);
(196, [EatInstr(45,246)]);
(580, [EatInstr(110,603)]);
(197, [EatInstr(98,247)]);
(581, [EatInstr(116,604)]);
(198, [EatInstr(45,248)]);
(582, [EatInstr(114,605)]);
(199, [EatInstr(117,249)]);
(583, [EatInstr(105,606)]);
(200, [EatInstr(97,250)]);
(584, [AAction2Instr(__a61,194)]);
(201, [AAction2Instr(__a10,251)]);
(585, [EatInstr(114,607)]);
(202, [EatInstr(45,252)]);
(586, [EatInstr(114,608)]);
(203, [EatInstr(101,253)]);
(587, [EatInstr(115,609)]);
(204, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,254)]);
(588, [EatInstr(121,610)]);
(205, [AAction2Instr(__a11,251)]);
(589, [AAction2Instr(__a62,194)]);
(206, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,255)]);
(590, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,628)]);
(207, [EatInstr(100,256)]);
(591, [AAction2Instr(__a63,251)]);
(208, [EatInstr(121,257)]);
(592, [EatInstr(101,611)]);
(209, [EatInstr(116,258)]);
(593, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,612)]);
(210, [EatInstr(108,259)]);
(594, [AAction2Instr(__a64,251)]);
(211, [AAction2Instr(__a12,251)]);
(595, [EatInstr(104,613)]);
(212, [EatInstr(108,260)]);
(596, [AAction2Instr(__a65,78)]);
(213, [EatInstr(100,261)]);
(597, [EatInstr(115,614)]);
(214, [EatInstr(116,262)]);
(598, [EatInstr(121,615)]);
(215, [AAction2Instr(__a13,263)]);
(599, [EatInstr(112,616)]);
(216, [EatInstr(99,264)]);
(600, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,617)]);
(217, [AAction2Instr(__a14,78)]);
(601, [EatInstr(111,618)]);
(218, [EatInstr(101,265)]);
(602, [EatInstr(110,619)]);
(219, [AAction2Instr(__a15,78)]);
(603, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,620)]);
(220, [EatInstr(104,266)]);
(604, [EatInstr(114,621)]);
(221, [EatInstr(111,267)]);
(605, [AAction2Instr(__a66,622)]);
(222, [EatInstr(100,268)]);
(606, [EatInstr(118,623)]);
(223, [EatInstr(45,270);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,269)]);
(607, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,624)]);
(224, [AAction2Instr(__a16,78)]);
(608, [EatInstr(121,625)]);
(225, [EatInstr(45,273)]);
(609, [EatInstr(116,626)]);
(226, [EatInstr(108,274)]);
(610, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,627)]);
(227, [EatInstr(114,275)]);
(611, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,630)]);
(228, [EatInstr(119,276)]);
(612, [AAction2Instr(__a67,251)]);
(229, [EatInstr(101,277)]);
(613, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,631)]);
(230, [EatInstr(45,278)]);
(614, [EatInstr(105,632)]);
(231, [EatInstr(107,279)]);
(615, [EatInstr(115,633)]);
(232, [EatInstr(116,280)]);
(616, [EatInstr(114,634)]);
(233, [EatInstr(110,281)]);
(617, [AAction2Instr(__a68,78)]);
(234, [EatInstr(97,282)]);
(618, [EatInstr(110,635)]);
(235, [EatInstr(105,283)]);
(619, [EatInstr(45,637);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,636)]);
(236, [EatInstr(111,284)]);
(620, [AAction2Instr(__a69,194)]);
(237, [EatInstr(101,285)]);
(621, [EatInstr(105,638)]);
(238, [EatInstr(101,286)]);
(622, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,554)]);
(239, [EatInstr(107,287)]);
(623, [EatInstr(101,639)]);
(240, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,288)]);
(624, [AAction2Instr(__a70,194)]);
(241, [EatInstr(105,289)]);
(625, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,640)]);
(242, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,290)]);
(626, [EatInstr(111,641)]);
(243, [EatInstr(45,291)]);
(627, [AAction2Instr(__a71,194)]);
(244, [EatInstr(108,292)]);
(628, [AAction2Instr(__a72,629);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,628)]);
(245, [EatInstr(102,293)]);
(629, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,642)]);
(246, [EatInstr(110,294)]);
(630, [AAction2Instr(__a73,251)]);
(247, [EatInstr(117,295)]);
(631, [AAction2Instr(__a74,78)]);
(248, [EatInstr(117,296)]);
(632, [EatInstr(115,643)]);
(249, [EatInstr(108,297)]);
(633, [EatInstr(105,644)]);
(250, [EatInstr(114,298)]);
(634, [EatInstr(101,645)]);
(251, [CompleteInstr(271)]);
(635, [EatInstr(115,646)]);
(252, [EatInstr(116,300)]);
(636, [AAction2Instr(__a75,78)]);
(253, [EatInstr(45,301)]);
(637, [EatInstr(115,647)]);
(254, [AAction2Instr(__a17,251)]);
(638, [EatInstr(99,648)]);
(255, [AAction2Instr(__a18,251)]);
(639, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,649)]);
(256, [EatInstr(101,302)]);
(640, [AAction2Instr(__a76,194)]);
(257, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,303)]);
(641, [EatInstr(114,650)]);
(258, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,304)]);
(642, [AAction2Instr(__a77,194)]);
(259, [EatInstr(45,305)]);
(643, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,651)]);
(260, [EatInstr(101,306)]);
(644, [EatInstr(115,652)]);
(261, [EatInstr(101,307)]);
(645, [EatInstr(100,653)]);
(262, [EatInstr(99,308)]);
(646, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,654)]);
(263, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,309)]);
(647, [EatInstr(99,655)]);
(264, [EatInstr(116,310)]);
(648, [EatInstr(116,656)]);
(265, [EatInstr(110,311)]);
(649, [AAction2Instr(__a78,194)]);
(266, [EatInstr(101,312)]);
(650, [EatInstr(121,657)]);
(267, [EatInstr(111,313)]);
(651, [AAction2Instr(__a79,78)]);
(268, [EatInstr(101,314)]);
(652, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,658)]);
(269, [AAction2Instr(__a19,78)]);
(653, [EatInstr(105,659)]);
(270, [EatInstr(114,317);EatInstr(110,316);EatInstr(103,315)]);
(654, [AAction2Instr(__a80,78)]);
(271, [AAction2Instr(__a20,272);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,271)]);
(655, [EatInstr(97,660)]);
(272, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,318)]);
(656, [AAction2Instr(__a81,622)]);
(273, [EatInstr(108,319)]);
(657, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,661)]);
(274, [EatInstr(97,320)]);
(658, [AAction2Instr(__a82,78)]);
(275, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,321)]);
(659, [EatInstr(99,662)]);
(276, [EatInstr(45,322)]);
(660, [EatInstr(110,663)]);
(277, [EatInstr(110,323)]);
(661, [AAction2Instr(__a83,194)]);
(278, [EatInstr(105,324)]);
(662, [EatInstr(97,664)]);
(279, [EatInstr(45,325)]);
(663, [EatInstr(110,665)]);
(280, [EatInstr(101,326)]);
(664, [EatInstr(116,666)]);
(281, [EatInstr(101,327)]);
(665, [EatInstr(101,667)]);
(282, [EatInstr(104,328)]);
(666, [EatInstr(101,668)]);
(283, [EatInstr(122,329)]);
(667, [EatInstr(114,669)]);
(284, [EatInstr(97,330)]);
(668, [EatInstr(115,670)]);
(285, [EatInstr(109,331)]);
(669, [EatInstr(108,671)]);
(286, [EatInstr(112,332)]);
(670, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,672)]);
(287, [EatInstr(105,333)]);
(671, [EatInstr(101,673)]);
(288, [AAction2Instr(__a21,194)]);
(672, [AAction2Instr(__a84,78)]);
(289, [EatInstr(120,334)]);
(673, [EatInstr(115,674)]);
(290, [AAction2Instr(__a22,335)]);
(674, [EatInstr(115,675)]);
(291, [EatInstr(104,336)]);
(675, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,676)]);
(292, [EatInstr(108,337)]);
(676, [AAction2Instr(__a85,78)]);
(293, [EatInstr(115,338)]);
(294, [EatInstr(111,339)]);
(295, [EatInstr(116,340)]);
(296, [EatInstr(110,341)]);
(297, [EatInstr(101,342)]);
(298, [EatInstr(45,344);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,343)]);
(300, [EatInstr(121,345)]);
(301, [EatInstr(114,347);EatInstr(110,346)]);
(302, [EatInstr(110,348)]);
(303, [AAction2Instr(__a23,251)]);
(304, [AAction2Instr(__a24,251)]);
(305, [EatInstr(115,349)]);
(306, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,350)]);
(307, [EatInstr(110,351)]);
(308, [EatInstr(104,352)]);
(309, [AAction2Instr(__a25,353)]);
(310, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,354)]);
(311, [EatInstr(101,355)]);
(312, [EatInstr(97,356)]);
(313, [EatInstr(107,357)]);
(314, [EatInstr(110,358)]);
(315, [EatInstr(105,359)]);
(316, [EatInstr(117,361);EatInstr(112,360)]);
(317, [EatInstr(101,362)]);
(318, [AAction2Instr(__a26,78)]);
(319, [EatInstr(97,363)]);
(320, [EatInstr(116,364)]);
(321, [AContInstr3(271,__g0,__binder4,365);ACallInstr3(__g0,8)]);
(322, [EatInstr(110,366)]);
(323, [EatInstr(100,367)]);
(324, [EatInstr(110,368)]);
(325, [EatInstr(108,369)]);
(326, [EatInstr(114,370)]);
(327, [EatInstr(45,371)]);
(328, [EatInstr(101,372)]);
(329, [EatInstr(101,373)]);
(330, [EatInstr(108,374)]);
(331, [EatInstr(111,375)]);
(332, [EatInstr(108,376)]);
(333, [EatInstr(112,377)]);
(334, [EatInstr(45,378)]);
(335, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,379)]);
(336, [EatInstr(105,380)]);
(337, [EatInstr(45,381)]);
(338, [EatInstr(116,383);EatInstr(109,382)]);
(339, [EatInstr(116,384)]);
(340, [EatInstr(101,385)]);
(341, [EatInstr(100,386)]);
(342, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,387)]);
(343, [AAction2Instr(__a27,251)]);
(344, [EatInstr(103,388)]);
(345, [EatInstr(112,389)]);
(346, [EatInstr(117,390)]);
(347, [EatInstr(101,391)]);
(348, [EatInstr(99,392)]);
(349, [EatInstr(116,393)]);
(350, [AAction2Instr(__a28,78)]);
(351, [EatInstr(99,394)]);
(352, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,395)]);
(353, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,396)]);
(354, [AAction2Instr(__a29,78)]);
(355, [EatInstr(114,397)]);
(356, [EatInstr(100,398)]);
(357, [EatInstr(97,399)]);
(358, [EatInstr(99,400)]);
(359, [EatInstr(108,401)]);
(360, [EatInstr(114,402)]);
(361, [EatInstr(108,403)]);
(362, [EatInstr(108,404)]);
(363, [EatInstr(116,405)]);
(364, [EatInstr(101,406)]);
(365, [AAction2Instr(__a30,194)]);
(366, [EatInstr(111,407)]);
(367, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,408)]);
(368, [EatInstr(115,409)]);
(369, [EatInstr(97,410)]);
(370, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,411)]);
(371, [EatInstr(114,413);EatInstr(99,412)]);
(372, [EatInstr(97,414)]);
(373, [EatInstr(45,415)]);
(374, [EatInstr(101,416)]);
(375, [EatInstr(105,417)]);
(376, [EatInstr(97,418)]);
(377, [EatInstr(45,419)]);
(378, [EatInstr(104,420)]);
(379, [AAction2Instr(__a31,421)]);
(380, [EatInstr(115,422)]);
(381, [EatInstr(115,423)]);
(382, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,424)]);
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
