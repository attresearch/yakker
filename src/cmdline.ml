
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
| Dispatch_cmd
| Dot_cmd
| Exec_cmd
| Extract_cmd
| Fuse_cmd
| Hash_cmd
| Info_cmd
| Inline_regular_cmd
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
| Rfc_cmd
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
  | Peg_BE of bool (** PEG backend. argument indicates whether
                       to use more liberal (PADS-style) star. *)
  | Trans_BE (** Transducer backend. *)

let backend = ref Trans_BE
type hv = int
;;
let _l2hv x = x;; (* label to hv *)

module Yk_Hashed = struct
  type t = hv * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
end
module Yk_History = Yak.History.Make(Yk_Hashed)

(*REPLAY PROLOGUE*)
let rec
_r_cmd_line_args(_n,ykinput) = (ignore (*1000*) (_n()); 
 (let c = (ignore (*1001*) (_n()); _r_command(_n,ykinput))
  in (ignore (*1003*) (_n()); 
 (let _x21 = (ignore (*1004*) (_n()); 
 (let _x4 = (ignore (*1005*) (_n()); 
 (let rec _x23 _x4 = (match _n() with 1006 -> _x4 | _x22 -> _x23((ignore (*1007*) (_x22); 
 (let _x3 = (ignore (*1008*) (_n()); _r_args(_n,ykinput))
  in (ignore (*1010*) (_n()); _x3::_x4)
 ))
 )) in _x23(Yak.Util.nil)))
  in (ignore (*1011*) (_n()); (List.rev _x4))
 ))
  in (ignore (*1013*) (_n());  cmd := c )
 ))
 ))
 
 and
_r_phases(_n,ykinput) = 
 (match _n() with
 | (1016) -> ( Attributes_cmd )
 | (1019) -> ( Close_under_core_cmd )
 | (1022) -> ( Copyrule_cmd )
 | (1025) -> ( Desugar_cmd )
 | (1028) -> ( Hash_cmd )
 | (1031) -> ( Inline_regular_cmd )
 | (1034) -> ( Lexer_cmd )
 | (1037) -> ( Lift_cmd )
 | (1040) -> ( Minus_cmd )
 | (1043) -> ( Tx_prec_cmd )
 | (1046) -> ( Subset_cmd )
 | (1049) -> ( Unroll_star_cmd )
 | _(*1052*) -> ( Wrap_cmd )
 )
 and
_r_command(_n,ykinput) = 
 (match _n() with
 | (1053) -> (
 (let p = (ignore (*1054*) (_n()); _r_phases(_n,ykinput))
  in (ignore (*1056*) (_n());  (match p with
                                               Inline_regular_cmd -> Compileopt.inline_regular := true
                                             | Unroll_star_cmd -> if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1
                                             | _ -> ());
                                            p )
 ))
 | (1059) -> ( Compile_cmd )
 | (1062) -> ( Dispatch_cmd )
 | (1065) -> ( Dot_cmd )
 | (1068) -> (
 (let _x6 = (ignore (*1069*) (_n()); _n())
  in (ignore (*1072*) (_n()); 
 (let _x5 = (ignore (*1073*) (_n()); _n())
  in (ignore (*1075*) (_n()); 
 (let f = (ignore (*1076*) (_n()); Yak.YkBuf.get_string _x6 _x5 ykinput)
  in (ignore (*1077*) (_n()); 
 (let l = (ignore (*1078*) (_n()); 
 (let _x10 = (ignore (*1079*) (_n()); 
 (let rec _x25 _x10 = (match _n() with 1080 -> _x10 | _x24 -> _x25((ignore (*1081*) (_x24); 
 (let _x9 = (ignore (*1083*) (_n()); 
 (let _x8 = (ignore (*1084*) (_n()); _n())
  in (ignore (*1087*) (_n()); 
 (let _x7 = (ignore (*1088*) (_n()); _n())
  in (ignore (*1090*) (_n()); 
 (let x = (ignore (*1091*) (_n()); Yak.YkBuf.get_string _x8 _x7 ykinput)
  in (ignore (*1092*) (_n()); x)
 ))
 ))
 ))
  in (ignore (*1093*) (_n()); _x9::_x10)
 ))
 )) in _x25(Yak.Util.nil)))
  in (ignore (*1094*) (_n()); (List.rev _x10))
 ))
  in (ignore (*1096*) (_n());  files := f::!files; exec_l := l; Exec_cmd )
 ))
 ))
 ))
 ))
 | (1099) -> ( Extract_cmd )
 | (1102) -> ( Compileopt.coalesce := true; Fuse_cmd )
 | (1105) -> ( Info_cmd )
 | (1108) -> ( Lookahead_analysis_cmd )
 | (1111) -> ( Lr1_lookahead_cmd )
 | (1114) -> ( Precedence_analysis_cmd )
 | (1117) -> ( Print_gul_cmd )
 | (1120) -> ( Print_gil_cmd )
 | (1123) -> ( Print_npreds_cmd )
 | (1126) -> ( Print_npreds_cmd )
 | (1129) -> ( Print_relevance_cmd )
 | (1131) -> (
 (let _x12 = (ignore (*1132*) (_n()); _n())
  in (ignore (*1135*) (_n()); 
 (let _x11 = (ignore (*1136*) (_n()); _n())
  in (ignore (*1138*) (_n()); 
 (let n = (ignore (*1139*) (_n()); Yak.YkBuf.get_string _x12 _x11 ykinput)
  in (ignore (*1141*) (_n());  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" )
 ))
 ))
 ))
 | (1144) -> ( Strip_late_actions_cmd )
 | (1147) -> ( Translate_dypgen_cmd )
 | _(*1150*) -> ( Translate_dypgen_scannerless_cmd )
 )
 and
_r_args(_n,ykinput) = 
 (match _n() with
 | (1153) -> (
 (let p = (ignore (*1154*) (_n()); _r_phases(_n,ykinput))
  in (ignore (*1156*) (_n());  after := Some p )
 ))
 | (1159) -> (
 (let b = 
 (match _n() with
 | (1161) -> (Fun_BE)
 | (1163) -> (Trans_BE)
 | (1165) -> (Peg_BE false)
 | _(*1167*) -> (Peg_BE true)
 ) in (ignore (*1169*) (_n());  backend := b )
 ))
 | (1172) -> ( Compileopt.case_sensitive := false )
 | (1175) -> ( Compileopt.check_labels := true )
 | (1178) -> (
 (let _x14 = (ignore (*1179*) (_n()); _n())
  in (ignore (*1182*) (_n()); 
 (let _x13 = (ignore (*1183*) (_n()); _n())
  in (ignore (*1185*) (_n()); 
 (let n = (ignore (*1186*) (_n()); Yak.YkBuf.get_string _x14 _x13 ykinput)
  in (ignore (*1188*) (_n());  Variables.counter := (int_of_string n) )
 ))
 ))
 ))
 | (1191) -> ( Compileopt.inline_cs := true )
 | (1194) -> ( Compileopt.inline_regular := true )
 | (1197) -> ( Compileopt.memoize_history := true )
 | (1200) -> ( Compileopt.memoize_history := false )
 | (1203) -> ( Compileopt.unit_history := true )
 | (1206) -> ( Compileopt.repress_replay := true )
 | (1209) -> ( Compileopt.lookahead := true )
 | (1212) -> ( Compileopt.coalesce := false )
 | (1215) -> ( only := true )
 | (1218) -> (
 (let _x16 = (ignore (*1219*) (_n()); _n())
  in (ignore (*1222*) (_n()); 
 (let _x15 = (ignore (*1223*) (_n()); _n())
  in (ignore (*1225*) (_n()); 
 (let x = (ignore (*1226*) (_n()); Yak.YkBuf.get_string _x16 _x15 ykinput)
  in (ignore (*1228*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | (1231) -> (
 (let _x18 = (ignore (*1232*) (_n()); _n())
  in (ignore (*1235*) (_n()); 
 (let _x17 = (ignore (*1236*) (_n()); _n())
  in (ignore (*1238*) (_n()); 
 (let n = (ignore (*1239*) (_n()); Yak.YkBuf.get_string _x18 _x17 ykinput)
  in (ignore (*1241*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1244) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | _(*1245*) -> (
 (let _x20 = (ignore (*1246*) (_n()); _n())
  in (ignore (*1249*) (_n()); 
 (let _x19 = (ignore (*1250*) (_n()); _n())
  in (ignore (*1252*) (_n()); 
 (let f = (ignore (*1253*) (_n()); Yak.YkBuf.get_string _x20 _x19 ykinput)
  in (ignore (*1255*) (_n());  files := f::!files )
 ))
 ))
 ))
 )
 
(*LATE PROLOGUE*)
type _pos = int (* input positions *)
let hv_compare = Yk_History.compare
type sv = (hv*_pos, Yak.History.label) Yak.History.history
let sv0 = Yk_History.new_history()
let sv_compare = hv_compare
let sv_hash = Yk_History.hash

(* History transformers *)
let _p x p = (fun h->h#push p ((x),p))
let _p_pos x p = (fun h->(h#push p ((x),p))#push p ((p),p))
let _p_pos_only x p = (fun h->h#push p ((p),p))
let _m x p = (fun h1 h2-> h1#merge p ((x),p) h2)

let sv_eq x y = sv_compare x y = 0
let key_eq (i,v1) (j,v2) = i = j &&  sv_eq v1 v2
let key_hash (i,v) = i lxor (sv_hash v)

(** Hashtable for top-down parsing. *)
module TDHashtable = Hashtbl.Make(struct type t = int * sv let equal = key_eq let hash = key_hash end)

let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a61 = _p 1169;;
let __a21 = _p 1034;;
let __a28 = _p 1141;;
let __a48 = _p 1191;;
let __a44 = _p 1228;;
let __a77 = _p 1200;;
let __a15 = _p 1037;;
let __a74 = _p 1144;;
let __a67 = _p 1194;;
let __a47 = fun p v -> _p 1186 p (_p 1185 p (_p_pos 1183 p (_p 1182 p (v))));;
let __a12 = _p 1010;;
let __a63 = _p 1203;;
let __a59 = fun p v -> _p_pos 1232 p (_p 1231 p (v));;
let __a69 = _p 1147;;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a70 = _p 1197;;
let __a37 = _p 1062;;
let __a13 = _p 1013;;
let __a50 = _p 1206;;
let __a1 = fun p v -> _p 1054 p (_p 1053 p (v));;
let __a43 = _p 1120;;
let __a11 = _p 1255;;
let __a40 = fun p v -> _p_pos 1179 p (_p 1178 p (v));;
let __a10 = _p 1065;;
let __a45 = _p 1016;;
let __a72 = _p 1172;;
let __a54 = _p 1123;;
let __a49 = _p 1209;;
let __a7 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a68 = _p 1019;;
let __a78 = _p 1126;;
let __a62 = _p 1175;;
let __a3 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a22 = _p 1040;;
let __a6 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a66 = _p 1129;;
let __a46 = _p 1043;;
let __a79 = _p 1150;;
let __a35 = fun p v -> _p 1226 p (_p 1225 p (_p_pos 1223 p (_p 1222 p (v))));;
let __a41 = fun p v -> _p_pos 1084 p (_p 1083 p (v));;
let __a18 = _p 1102;;
let __a26 = _p 1046;;
let __a42 = _p 1096;;
let __a20 = fun p v -> _p 1139 p (_p 1138 p (_p_pos 1136 p (_p 1135 p (v))));;
let __a33 = fun p v -> _p 1094 p (_p 1080 p (v));;
let __a19 = _p 1105;;
let __a58 = _p 1212;;
let __a51 = _p 1049;;
let __a2 = fun p v -> _p_pos 1246 p (_p 1245 p (v));;
let __a38 = _p 1156;;
let __a34 = _p 1099;;
let __a73 = _p 1108;;
let __a36 = _p 1022;;
let __a24 = _p 1215;;
let __a39 = _p 1159;;
let __a27 = fun p v -> _p 1079 p (_p 1078 p (_p 1077 p (_p 1076 p (_p 1075 p (_p_pos 1073 p (_p 1072 p (v)))))));;
let __a29 = fun p v -> _p 1154 p (_p 1153 p (v));;
let __a5 = fun p v -> _p 1253 p (_p 1252 p (_p_pos 1250 p (_p 1249 p (v))));;
let __a17 = fun p v -> _p_pos 1069 p (_p 1068 p (v));;
let __a30 = _p 1025;;
let __a25 = fun p v -> _p_pos 1219 p (_p 1218 p (v));;
let __a14 = _p 1028;;
let __a71 = _p 1241;;
let __a65 = fun p v -> _p 1239 p (_p 1238 p (_p_pos 1236 p (_p 1235 p (v))));;
let __a8 = fun p v -> _p_pos 1132 p (_p 1131 p (v));;
let __a16 = _p 1052;;
let __a9 = _p 1244;;
let __a57 = _p 1188;;
let __a60 = _p 1111;;
let __a55 = _p 1161;;
let __a4 = _p 1056;;
let __a75 = _p 1114;;
let __a53 = _p 1163;;
let __a76 = _p 1165;;
let __a31 = _p 1059;;
let __a64 = _p 1031;;
let __a52 = fun p v -> _p 1093 p (_p 1092 p (_p 1091 p (_p 1090 p (_p_pos 1088 p (_p 1087 p (v))))));;
let __a23 = _p 1117;;
let __a56 = _p 1167;;
let __a32 = _p 1081;;
let __binder0 = __default_ret;;
let __binder1 = _m 1002;;
let __binder2 = _m 1055;;
let __binder3 = _m 1009;;
let __binder4 = _m 1155;;
let binders : (sv -> sv -> sv) array = [| |]
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

open Yak.Pam_internal
module SV_hashtbl = Hashtbl.Make(struct
                          type t = sv
                          let equal a b = sv_compare a b = 0
                          let hash = Hashtbl.hash end)
module Pred = Pred3
let rec nullable_cmd_line_args __lookahead _p0_ _x0_ = None

and nullable_command __lookahead _p0_ _x0_ = None

and nullable_o __lookahead _p0_ _x0_ = None

and nullable_CHAR __lookahead _p0_ _x0_ = None

and nullable_file __lookahead _p0_ _x0_ = None

and nullable_DIGIT __lookahead _p0_ _x0_ = None

and nullable_args __lookahead _p0_ _x0_ = None

and nullable_phases __lookahead _p0_ _x0_ = None

and nullable_arg __lookahead _p0_ _x0_ = None

and nullable_eof __lookahead _p0_ _x0_ = ((((Pred.full_lookaheadc false 266 3) __lookahead) _p0_) _x0_)

and nullable_OCTET __lookahead _p0_ _x0_ = None

let program : (int * sv instruction list) list = [
(383, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,413)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [AAction2Instr(__a44,171)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(111,414)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(97,415)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [AAction2Instr(__a45,221)]);
(4, [AAction2Instr(__a0,15)]);
(388, [EatInstr(45,416)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(108,417)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,17)]);
(390, [AAction2Instr(__a46,221)]);
(7, [EatInstr(127,18);EatInstr(126,18);EatInstr(125,18);EatInstr(124,18);EatInstr(123,18);EatInstr(122,18);EatInstr(121,18);EatInstr(120,18);EatInstr(119,18);EatInstr(118,18);EatInstr(117,18);EatInstr(116,18);EatInstr(115,18);EatInstr(114,18);EatInstr(113,18);EatInstr(112,18);EatInstr(111,18);EatInstr(110,18);EatInstr(109,18);EatInstr(108,18);EatInstr(107,18);EatInstr(106,18);EatInstr(105,18);EatInstr(104,18);EatInstr(103,18);EatInstr(102,18);EatInstr(101,18);EatInstr(100,18);EatInstr(99,18);EatInstr(98,18);EatInstr(97,18);EatInstr(96,18);EatInstr(95,18);EatInstr(94,18);EatInstr(93,18);EatInstr(92,18);EatInstr(91,18);EatInstr(90,18);EatInstr(89,18);EatInstr(88,18);EatInstr(87,18);EatInstr(86,18);EatInstr(85,18);EatInstr(84,18);EatInstr(83,18);EatInstr(82,18);EatInstr(81,18);EatInstr(80,18);EatInstr(79,18);EatInstr(78,18);EatInstr(77,18);EatInstr(76,18);EatInstr(75,18);EatInstr(74,18);EatInstr(73,18);EatInstr(72,18);EatInstr(71,18);EatInstr(70,18);EatInstr(69,18);EatInstr(68,18);EatInstr(67,18);EatInstr(66,18);EatInstr(65,18);EatInstr(64,18);EatInstr(63,18);EatInstr(62,18);EatInstr(61,18);EatInstr(60,18);EatInstr(59,18);EatInstr(58,18);EatInstr(57,18);EatInstr(56,18);EatInstr(55,18);EatInstr(54,18);EatInstr(53,18);EatInstr(52,18);EatInstr(51,18);EatInstr(50,18);EatInstr(49,18);EatInstr(48,18);EatInstr(47,18);EatInstr(46,18);EatInstr(44,18);EatInstr(43,18);EatInstr(42,18);EatInstr(41,18);EatInstr(40,18);EatInstr(39,18);EatInstr(38,18);EatInstr(37,18);EatInstr(36,18);EatInstr(35,18);EatInstr(34,18);EatInstr(33,18);EatInstr(32,18);EatInstr(31,18);EatInstr(30,18);EatInstr(29,18);EatInstr(28,18);EatInstr(27,18);EatInstr(26,18);EatInstr(25,18);EatInstr(24,18);EatInstr(23,18);EatInstr(22,18);EatInstr(21,18);EatInstr(20,18);EatInstr(19,18);EatInstr(18,18);EatInstr(17,18);EatInstr(16,18);EatInstr(15,18);EatInstr(14,18);EatInstr(13,18);EatInstr(12,18);EatInstr(11,18);EatInstr(10,18);EatInstr(9,18);EatInstr(8,18);EatInstr(7,18);EatInstr(6,18);EatInstr(5,18);EatInstr(4,18);EatInstr(3,18);EatInstr(2,18);EatInstr(1,18)]);
(391, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,418)]);
(8, [EatInstr(119,29);EatInstr(117,28);EatInstr(115,27);EatInstr(112,26);EatInstr(109,25);EatInstr(108,24);EatInstr(105,23);EatInstr(104,22);EatInstr(100,21);EatInstr(99,20);EatInstr(97,19)]);
(392, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,419)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(102,33);EatInstr(101,32);EatInstr(100,31);EatInstr(99,30);AAction2Instr(__a1,40)]);
(393, [EatInstr(110,420)]);
(10, [EatInstr(45,41);AAction2Instr(__a2,42)]);
(394, [EatInstr(97,421)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43);RCompleteInstr2(274,nullable_eof)]);
(395, [EatInstr(97,422)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(115,423)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(98,424)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(97,425)]);
(15, [ACallInstr3(__default_call,9);ASimpleCont2Instr(272,__binder1,45)]);
(399, [EatInstr(97,426)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(121,427)]);
(17, [ALookaheadInstr(false,CfgLA (1,264),46);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,17)]);
(401, [EatInstr(117,428)]);
(18, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,18)]);
(402, [EatInstr(101,429)]);
(19, [EatInstr(116,48)]);
(403, [EatInstr(120,430)]);
(20, [EatInstr(111,50);EatInstr(108,49)]);
(404, [EatInstr(115,431)]);
(21, [EatInstr(101,51)]);
(405, [EatInstr(108,432)]);
(22, [EatInstr(97,52)]);
(406, [AAction2Instr(__a47,433);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,406)]);
(23, [EatInstr(110,53)]);
(407, [AAction2Instr(__a48,171)]);
(24, [EatInstr(105,55);EatInstr(101,54)]);
(408, [EatInstr(117,434)]);
(25, [EatInstr(105,56)]);
(409, [AAction2Instr(__a49,171)]);
(26, [EatInstr(114,57)]);
(410, [EatInstr(115,435)]);
(27, [EatInstr(117,58)]);
(411, [EatInstr(101,436)]);
(28, [EatInstr(110,59)]);
(412, [EatInstr(45,437)]);
(29, [EatInstr(114,60)]);
(413, [AAction2Instr(__a50,171)]);
(30, [EatInstr(111,61)]);
(414, [EatInstr(114,438)]);
(31, [EatInstr(111,63);EatInstr(105,62)]);
(415, [EatInstr(114,439)]);
(32, [EatInstr(120,64)]);
(416, [EatInstr(99,440)]);
(33, [EatInstr(117,65)]);
(417, [EatInstr(97,441)]);
(34, [EatInstr(110,66)]);
(418, [AAction2Instr(__a51,221)]);
(35, [EatInstr(114,68);EatInstr(111,67)]);
(419, [AAction2Instr(__a52,299)]);
(36, [EatInstr(114,69)]);
(420, [EatInstr(97,442)]);
(37, [EatInstr(102,70)]);
(421, [EatInstr(100,443)]);
(38, [EatInstr(116,71)]);
(422, [EatInstr(110,444)]);
(39, [EatInstr(114,72)]);
(423, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,445)]);
(40, [ASimpleCont2Instr(271,__binder2,73);ACallInstr3(__default_call,8)]);
(424, [EatInstr(108,446)]);
(41, [EatInstr(118,84);EatInstr(117,83);EatInstr(114,82);EatInstr(111,81);EatInstr(110,80);EatInstr(109,79);EatInstr(108,78);EatInstr(105,77);EatInstr(99,76);EatInstr(98,75);EatInstr(97,74)]);
(425, [EatInstr(110,447)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,85)]);
(426, [EatInstr(99,448)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(112,449)]);
(428, [EatInstr(110,450)]);
(45, [AAction2Instr(__a3,86)]);
(429, [EatInstr(103,451)]);
(46, [CompleteInstr(269)]);
(430, [AAction2Instr(__a53,452)]);
(47, [CompleteInstr(270)]);
(431, [EatInstr(105,453)]);
(48, [EatInstr(116,87)]);
(432, [EatInstr(115,454)]);
(49, [EatInstr(111,88)]);
(433, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,455)]);
(50, [EatInstr(112,89)]);
(434, [EatInstr(108,456)]);
(51, [EatInstr(115,90)]);
(435, [EatInstr(116,457)]);
(52, [EatInstr(115,91)]);
(436, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,458)]);
(53, [EatInstr(108,92)]);
(437, [EatInstr(104,459)]);
(54, [EatInstr(120,93)]);
(438, [EatInstr(121,460)]);
(55, [EatInstr(102,94)]);
(439, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,461)]);
(56, [EatInstr(110,95)]);
(440, [EatInstr(111,462)]);
(57, [EatInstr(101,96)]);
(441, [EatInstr(114,463)]);
(58, [EatInstr(98,97)]);
(442, [EatInstr(108,464)]);
(59, [EatInstr(114,98)]);
(443, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,465)]);
(60, [EatInstr(97,99)]);
(444, [EatInstr(97,466)]);
(61, [EatInstr(109,100)]);
(445, [AAction2Instr(__a54,114)]);
(62, [EatInstr(115,101)]);
(446, [EatInstr(101,467)]);
(63, [EatInstr(116,102)]);
(447, [EatInstr(99,468)]);
(64, [EatInstr(116,104);EatInstr(101,103)]);
(448, [EatInstr(116,469)]);
(65, [EatInstr(115,105)]);
(449, [EatInstr(103,470)]);
(66, [EatInstr(102,106)]);
(450, [AAction2Instr(__a55,452)]);
(67, [EatInstr(111,107)]);
(451, [EatInstr(45,471);AAction2Instr(__a56,452)]);
(68, [EatInstr(49,108)]);
(452, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,472)]);
(69, [EatInstr(105,110);EatInstr(101,109)]);
(453, [EatInstr(116,473)]);
(70, [EatInstr(99,111)]);
(454, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,474)]);
(71, [EatInstr(114,112)]);
(455, [AAction2Instr(__a57,171)]);
(72, [EatInstr(97,113)]);
(456, [EatInstr(97,475)]);
(73, [AAction2Instr(__a4,114)]);
(457, [EatInstr(111,476)]);
(74, [EatInstr(102,115)]);
(458, [AAction2Instr(__a58,171)]);
(75, [EatInstr(97,116)]);
(459, [EatInstr(105,477)]);
(76, [EatInstr(111,119);EatInstr(104,118);EatInstr(97,117)]);
(460, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,478)]);
(77, [EatInstr(110,120)]);
(461, [AAction2Instr(__a59,479)]);
(78, [EatInstr(111,121)]);
(462, [EatInstr(114,480)]);
(79, [EatInstr(101,122)]);
(463, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,481)]);
(80, [EatInstr(111,123)]);
(464, [EatInstr(121,482)]);
(81, [EatInstr(110,124)]);
(465, [AAction2Instr(__a60,114)]);
(82, [EatInstr(111,125)]);
(466, [EatInstr(108,483)]);
(83, [EatInstr(110,126)]);
(467, [EatInstr(45,484)]);
(84, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,127)]);
(468, [EatInstr(101,485)]);
(85, [AAction2Instr(__a5,128)]);
(469, [EatInstr(105,486)]);
(86, [AAction2Instr(__a7,130);AAction2Instr(__a6,129)]);
(470, [EatInstr(101,487)]);
(87, [EatInstr(114,131)]);
(471, [EatInstr(115,488)]);
(88, [EatInstr(115,132)]);
(472, [AAction2Instr(__a61,171)]);
(89, [EatInstr(121,133)]);
(473, [EatInstr(105,489)]);
(90, [EatInstr(117,134)]);
(474, [AAction2Instr(__a62,171)]);
(91, [EatInstr(104,135)]);
(475, [EatInstr(114,490)]);
(92, [EatInstr(105,136)]);
(476, [EatInstr(114,491)]);
(93, [EatInstr(101,137)]);
(477, [EatInstr(115,492)]);
(94, [EatInstr(116,138)]);
(478, [AAction2Instr(__a63,171)]);
(95, [EatInstr(117,139)]);
(479, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,493)]);
(96, [EatInstr(99,140)]);
(480, [EatInstr(101,494)]);
(97, [EatInstr(115,141)]);
(481, [AAction2Instr(__a64,221)]);
(98, [EatInstr(111,142)]);
(482, [EatInstr(115,495)]);
(99, [EatInstr(112,143)]);
(483, [EatInstr(121,496)]);
(100, [EatInstr(112,144)]);
(484, [EatInstr(112,497)]);
(101, [EatInstr(112,145)]);
(485, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,498)]);
(102, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,146)]);
(486, [EatInstr(111,499)]);
(103, [EatInstr(99,147)]);
(487, [EatInstr(110,500)]);
(104, [EatInstr(114,148)]);
(488, [EatInstr(116,501)]);
(105, [EatInstr(101,149)]);
(489, [EatInstr(118,502)]);
(106, [EatInstr(111,150)]);
(490, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,503)]);
(107, [EatInstr(107,151)]);
(491, [EatInstr(121,504)]);
(108, [EatInstr(45,152)]);
(492, [EatInstr(116,505)]);
(109, [EatInstr(99,153)]);
(493, [AAction2Instr(__a65,506);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,493)]);
(110, [EatInstr(110,154)]);
(494, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,507)]);
(111, [AAction2Instr(__a8,155)]);
(495, [EatInstr(105,508)]);
(112, [EatInstr(105,156)]);
(496, [EatInstr(115,509)]);
(113, [EatInstr(110,157)]);
(497, [EatInstr(114,510)]);
(114, [CompleteInstr(272)]);
(498, [AAction2Instr(__a66,114)]);
(115, [EatInstr(116,158)]);
(499, [EatInstr(110,511)]);
(116, [EatInstr(99,159)]);
(500, [EatInstr(45,513);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,512)]);
(117, [EatInstr(115,160)]);
(501, [EatInstr(114,514)]);
(118, [EatInstr(101,161)]);
(502, [EatInstr(101,515)]);
(119, [EatInstr(117,162)]);
(503, [AAction2Instr(__a67,171)]);
(120, [EatInstr(108,163)]);
(504, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,516)]);
(121, [EatInstr(111,164)]);
(505, [EatInstr(111,517)]);
(122, [EatInstr(109,165)]);
(506, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,518)]);
(123, [EatInstr(45,166)]);
(507, [AAction2Instr(__a68,221)]);
(124, [EatInstr(108,167)]);
(508, [EatInstr(115,519)]);
(125, [EatInstr(111,168)]);
(509, [EatInstr(105,520)]);
(126, [EatInstr(114,170);EatInstr(105,169)]);
(510, [EatInstr(101,521)]);
(127, [AAction2Instr(__a9,171)]);
(511, [EatInstr(115,522)]);
(128, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,172)]);
(512, [AAction2Instr(__a69,114)]);
(129, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder3,173)]);
(513, [EatInstr(115,523)]);
(130, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,174)]);
(514, [EatInstr(105,524)]);
(131, [EatInstr(105,175)]);
(515, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(132, [EatInstr(101,176)]);
(516, [AAction2Instr(__a70,171)]);
(133, [EatInstr(114,177)]);
(517, [EatInstr(114,526)]);
(134, [EatInstr(103,178)]);
(518, [AAction2Instr(__a71,171)]);
(135, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,179)]);
(519, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,527)]);
(136, [EatInstr(110,180)]);
(520, [EatInstr(115,528)]);
(137, [EatInstr(114,181)]);
(521, [EatInstr(100,529)]);
(138, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,182)]);
(522, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,530)]);
(139, [EatInstr(115,183)]);
(523, [EatInstr(99,531)]);
(140, [EatInstr(101,184)]);
(524, [EatInstr(99,532)]);
(141, [EatInstr(101,185)]);
(525, [AAction2Instr(__a72,171)]);
(142, [EatInstr(108,186)]);
(526, [EatInstr(121,533)]);
(143, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,187)]);
(527, [AAction2Instr(__a73,114)]);
(144, [EatInstr(105,188)]);
(528, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,534)]);
(145, [EatInstr(97,189)]);
(529, [EatInstr(105,535)]);
(146, [AAction2Instr(__a10,114)]);
(530, [AAction2Instr(__a74,114)]);
(147, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,190)]);
(531, [EatInstr(97,536)]);
(148, [EatInstr(97,191)]);
(532, [EatInstr(116,537)]);
(149, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,192)]);
(533, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(150, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,193)]);
(534, [AAction2Instr(__a75,114)]);
(151, [EatInstr(97,194)]);
(535, [EatInstr(99,539)]);
(152, [EatInstr(108,195)]);
(536, [EatInstr(110,540)]);
(153, [EatInstr(101,196)]);
(537, [AAction2Instr(__a76,452)]);
(154, [EatInstr(116,197)]);
(538, [AAction2Instr(__a77,171)]);
(155, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,198)]);
(539, [EatInstr(97,541)]);
(156, [EatInstr(112,199)]);
(540, [EatInstr(110,542)]);
(157, [EatInstr(115,200)]);
(541, [EatInstr(116,543)]);
(158, [EatInstr(101,201)]);
(542, [EatInstr(101,544)]);
(159, [EatInstr(107,202)]);
(543, [EatInstr(101,545)]);
(160, [EatInstr(101,203)]);
(544, [EatInstr(114,546)]);
(161, [EatInstr(99,204)]);
(545, [EatInstr(115,547)]);
(162, [EatInstr(110,205)]);
(546, [EatInstr(108,548)]);
(163, [EatInstr(105,206)]);
(547, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,549)]);
(164, [EatInstr(107,207)]);
(548, [EatInstr(101,550)]);
(165, [EatInstr(111,208)]);
(549, [AAction2Instr(__a78,114)]);
(166, [EatInstr(114,211);EatInstr(109,210);EatInstr(99,209)]);
(550, [EatInstr(115,551)]);
(167, [EatInstr(121,212)]);
(551, [EatInstr(115,552)]);
(168, [EatInstr(116,213)]);
(552, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,553)]);
(169, [EatInstr(116,214)]);
(553, [AAction2Instr(__a79,114)]);
(170, [EatInstr(111,215)]);
(171, [CompleteInstr(273)]);
(172, [AAction2Instr(__a11,171)]);
(173, [AAction2Instr(__a12,86)]);
(174, [AAction2Instr(__a13,216)]);
(175, [EatInstr(98,217)]);
(176, [EatInstr(45,218)]);
(177, [EatInstr(117,219)]);
(178, [EatInstr(97,220)]);
(179, [AAction2Instr(__a14,221)]);
(180, [EatInstr(101,222)]);
(181, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,223)]);
(182, [AAction2Instr(__a15,221)]);
(183, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,224)]);
(184, [EatInstr(100,225)]);
(185, [EatInstr(116,226)]);
(186, [EatInstr(108,227)]);
(187, [AAction2Instr(__a16,221)]);
(188, [EatInstr(108,228)]);
(189, [EatInstr(116,229)]);
(190, [AAction2Instr(__a17,230)]);
(191, [EatInstr(99,231)]);
(192, [AAction2Instr(__a18,114)]);
(193, [AAction2Instr(__a19,114)]);
(194, [EatInstr(104,232)]);
(195, [EatInstr(111,233)]);
(196, [EatInstr(100,234)]);
(197, [EatInstr(45,236);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,235)]);
(198, [AAction2Instr(__a20,237);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,198)]);
(199, [EatInstr(45,238)]);
(200, [EatInstr(108,239)]);
(201, [EatInstr(114,240)]);
(202, [EatInstr(101,241)]);
(203, [EatInstr(45,242)]);
(204, [EatInstr(107,243)]);
(205, [EatInstr(116,244)]);
(206, [EatInstr(110,245)]);
(207, [EatInstr(97,246)]);
(208, [EatInstr(105,247)]);
(209, [EatInstr(111,248)]);
(210, [EatInstr(101,249)]);
(211, [EatInstr(101,250)]);
(212, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,251)]);
(213, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,252)]);
(214, [EatInstr(45,253)]);
(215, [EatInstr(108,254)]);
(216, [CompleteInstr(267)]);
(217, [EatInstr(117,255)]);
(218, [EatInstr(117,256)]);
(219, [EatInstr(108,257)]);
(220, [EatInstr(114,258)]);
(221, [CompleteInstr(271)]);
(222, [EatInstr(45,259)]);
(223, [AAction2Instr(__a21,221)]);
(224, [AAction2Instr(__a22,221)]);
(225, [EatInstr(101,260)]);
(226, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,261)]);
(227, [EatInstr(45,262)]);
(228, [EatInstr(101,263)]);
(229, [EatInstr(99,264)]);
(230, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,265)]);
(231, [EatInstr(116,266)]);
(232, [EatInstr(101,267)]);
(233, [EatInstr(111,268)]);
(234, [EatInstr(101,269)]);
(235, [AAction2Instr(__a23,114)]);
(236, [EatInstr(114,272);EatInstr(110,271);EatInstr(103,270)]);
(237, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,273)]);
(238, [EatInstr(108,274)]);
(239, [EatInstr(97,275)]);
(240, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,276)]);
(241, [EatInstr(110,277)]);
(242, [EatInstr(105,278)]);
(243, [EatInstr(45,279)]);
(244, [EatInstr(101,280)]);
(245, [EatInstr(101,281)]);
(246, [EatInstr(104,282)]);
(247, [EatInstr(122,283)]);
(248, [EatInstr(97,284)]);
(249, [EatInstr(109,285)]);
(250, [EatInstr(112,286)]);
(251, [AAction2Instr(__a24,171)]);
(252, [AAction2Instr(__a25,287)]);
(253, [EatInstr(104,288)]);
(254, [EatInstr(108,289)]);
(255, [EatInstr(116,290)]);
(256, [EatInstr(110,291)]);
(257, [EatInstr(101,292)]);
(258, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,293)]);
(259, [EatInstr(114,294)]);
(260, [EatInstr(110,295)]);
(261, [AAction2Instr(__a26,221)]);
(262, [EatInstr(115,296)]);
(263, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,297)]);
(264, [EatInstr(104,298)]);
(265, [AAction2Instr(__a27,299)]);
(266, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,300)]);
(267, [EatInstr(97,301)]);
(268, [EatInstr(107,302)]);
(269, [EatInstr(110,303)]);
(270, [EatInstr(105,304)]);
(271, [EatInstr(117,306);EatInstr(112,305)]);
(272, [EatInstr(101,307)]);
(273, [AAction2Instr(__a28,114)]);
(274, [EatInstr(97,308)]);
(275, [EatInstr(116,309)]);
(276, [AAction2Instr(__a29,310)]);
(277, [EatInstr(100,311)]);
(278, [EatInstr(110,312)]);
(279, [EatInstr(108,313)]);
(280, [EatInstr(114,314)]);
(281, [EatInstr(45,315)]);
(282, [EatInstr(101,316)]);
(283, [EatInstr(101,317)]);
(284, [EatInstr(108,318)]);
(285, [EatInstr(111,319)]);
(286, [EatInstr(108,320)]);
(287, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,321)]);
(288, [EatInstr(105,322)]);
(289, [EatInstr(45,323)]);
(290, [EatInstr(101,324)]);
(291, [EatInstr(100,325)]);
(292, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,326)]);
(293, [AAction2Instr(__a30,221)]);
(294, [EatInstr(101,327)]);
(295, [EatInstr(99,328)]);
(296, [EatInstr(116,329)]);
(297, [AAction2Instr(__a31,114)]);
(298, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,330)]);
(299, [AAction2Instr(__a33,332);AAction2Instr(__a32,331)]);
(300, [AAction2Instr(__a34,114)]);
(301, [EatInstr(100,333)]);
(302, [EatInstr(97,334)]);
(303, [EatInstr(99,335)]);
(304, [EatInstr(108,336)]);
(305, [EatInstr(114,337)]);
(306, [EatInstr(108,338)]);
(307, [EatInstr(108,339)]);
(308, [EatInstr(116,340)]);
(309, [EatInstr(101,341)]);
(310, [ASimpleCont2Instr(271,__binder4,342);ACallInstr3(__default_call,8)]);
(311, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,343)]);
(312, [EatInstr(115,344)]);
(313, [EatInstr(97,345)]);
(314, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,346)]);
(315, [EatInstr(114,348);EatInstr(99,347)]);
(316, [EatInstr(97,349)]);
(317, [EatInstr(45,350)]);
(318, [EatInstr(101,351)]);
(319, [EatInstr(105,352)]);
(320, [EatInstr(97,353)]);
(321, [AAction2Instr(__a35,354)]);
(322, [EatInstr(115,355)]);
(323, [EatInstr(115,356)]);
(324, [EatInstr(115,357)]);
(325, [EatInstr(101,358)]);
(326, [AAction2Instr(__a36,221)]);
(327, [EatInstr(103,359)]);
(328, [EatInstr(101,360)]);
(329, [EatInstr(97,361)]);
(330, [AAction2Instr(__a37,114)]);
(331, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,362)]);
(332, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,363)]);
(333, [EatInstr(45,364)]);
(334, [EatInstr(104,365)]);
(335, [EatInstr(101,366)]);
(336, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,367)]);
(337, [EatInstr(101,368)]);
(338, [EatInstr(108,369)]);
(339, [EatInstr(101,370)]);
(340, [EatInstr(101,371)]);
(341, [EatInstr(45,372)]);
(342, [AAction2Instr(__a38,171)]);
(343, [AAction2Instr(__a39,373)]);
(344, [EatInstr(101,374)]);
(345, [EatInstr(98,375)]);
(346, [AAction2Instr(__a40,376)]);
(347, [EatInstr(115,377)]);
(348, [EatInstr(101,378)]);
(349, [EatInstr(100,379)]);
(350, [EatInstr(104,380)]);
(351, [EatInstr(115,381)]);
(352, [EatInstr(122,382)]);
(353, [EatInstr(121,383)]);
(354, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,384)]);
(355, [EatInstr(116,385)]);
(356, [EatInstr(116,386)]);
(357, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,387)]);
(358, [EatInstr(114,388)]);
(359, [EatInstr(117,389)]);
(360, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,390)]);
(361, [EatInstr(114,391)]);
(362, [AAction2Instr(__a41,392)]);
(363, [AAction2Instr(__a42,114)]);
(364, [EatInstr(97,393)]);
(365, [EatInstr(101,394)]);
(366, [EatInstr(45,395)]);
(367, [AAction2Instr(__a43,114)]);
(368, [EatInstr(100,396)]);
(369, [EatInstr(97,397)]);
(370, [EatInstr(118,398)]);
(371, [EatInstr(45,399)]);
(372, [EatInstr(100,400)]);
(373, [EatInstr(116,403);EatInstr(112,402);EatInstr(102,401)]);
(374, [EatInstr(110,404)]);
(375, [EatInstr(101,405)]);
(376, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,406)]);
(377, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,407)]);
(378, [EatInstr(103,408)]);
(379, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,409)]);
(380, [EatInstr(105,410)]);
(381, [EatInstr(99,411)]);
(382, [EatInstr(101,412)]);
]

let start_symb = get_symb_action "cmd-line-args"

module P2__ = Yak.Engine.Full_yakker(struct type t = sv let cmp = sv_compare
                                            type idata = Yk_History.Root_id_set.t
  let create_idata () = Yk_History.Root_id_set.empty
  let inspect h s = Yk_History.add_id_set h#get_root s
  let summarize_inspection s = string_of_int (Yk_History.Root_id_set.cardinal s) end)

let _wfe_data_ = Yak.PamJIT.DNELR.to_table (Yak.Pam_internal.load_internal_program program)
  start_symb (get_symb_start start_symb) 264 num_symbols
  __default_call __default_ret

let parse = Yak.Pami.Wfe.mk_parse P2__.parse _wfe_data_ sv0 
    (fun ykinput h ->
      let _o = (h#traverse_postfix) in
      let _n() = (let (x,_) = _o#next() in x) in
      _r_cmd_line_args(_n,ykinput)
    )

let visualize = parse
let visualize_file = Yak.Pami.Simple.parse_file visualize
let visualize_string = Yak.Pami.Simple.parse_string visualize

let parse_file = Yak.Pami.Simple.parse_file parse
let parse_string = Yak.Pami.Simple.parse_string parse
;;
Yk_History.memoize := false;;


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
