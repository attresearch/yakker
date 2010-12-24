
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
 | (1206) -> ( Compileopt.skip_opt := true )
 | (1209) -> ( Compileopt.repress_replay := true )
 | (1212) -> ( Compileopt.lookahead := true )
 | (1215) -> ( Compileopt.coalesce := false )
 | (1218) -> ( only := true )
 | (1221) -> (
 (let _x16 = (ignore (*1222*) (_n()); _n())
  in (ignore (*1225*) (_n()); 
 (let _x15 = (ignore (*1226*) (_n()); _n())
  in (ignore (*1228*) (_n()); 
 (let x = (ignore (*1229*) (_n()); Yak.YkBuf.get_string _x16 _x15 ykinput)
  in (ignore (*1231*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | (1234) -> (
 (let _x18 = (ignore (*1235*) (_n()); _n())
  in (ignore (*1238*) (_n()); 
 (let _x17 = (ignore (*1239*) (_n()); _n())
  in (ignore (*1241*) (_n()); 
 (let n = (ignore (*1242*) (_n()); Yak.YkBuf.get_string _x18 _x17 ykinput)
  in (ignore (*1244*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1247) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | _(*1248*) -> (
 (let _x20 = (ignore (*1249*) (_n()); _n())
  in (ignore (*1252*) (_n()); 
 (let _x19 = (ignore (*1253*) (_n()); _n())
  in (ignore (*1255*) (_n()); 
 (let f = (ignore (*1256*) (_n()); Yak.YkBuf.get_string _x20 _x19 ykinput)
  in (ignore (*1258*) (_n());  files := f::!files )
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
let __a62 = _p 1169;;
let __a21 = _p 1034;;
let __a35 = fun p v -> _p 1229 p (_p 1228 p (_p_pos 1226 p (_p 1225 p (v))));;
let __a28 = _p 1141;;
let __a49 = _p 1191;;
let __a78 = _p 1200;;
let __a15 = _p 1037;;
let __a75 = _p 1144;;
let __a68 = _p 1194;;
let __a48 = fun p v -> _p 1186 p (_p 1185 p (_p_pos 1183 p (_p 1182 p (v))));;
let __a12 = _p 1010;;
let __a64 = _p 1203;;
let __a70 = _p 1147;;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a71 = _p 1197;;
let __a37 = _p 1062;;
let __a13 = _p 1013;;
let __a45 = _p 1206;;
let __a1 = fun p v -> _p 1054 p (_p 1053 p (v));;
let __a43 = _p 1120;;
let __a40 = fun p v -> _p_pos 1179 p (_p 1178 p (v));;
let __a5 = fun p v -> _p 1256 p (_p 1255 p (_p_pos 1253 p (_p 1252 p (v))));;
let __a10 = _p 1065;;
let __a46 = _p 1016;;
let __a73 = _p 1172;;
let __a55 = _p 1123;;
let __a51 = _p 1209;;
let __a11 = _p 1258;;
let __a7 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a69 = _p 1019;;
let __a44 = _p 1231;;
let __a79 = _p 1126;;
let __a63 = _p 1175;;
let __a3 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a22 = _p 1040;;
let __a6 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a67 = _p 1129;;
let __a60 = fun p v -> _p_pos 1235 p (_p 1234 p (v));;
let __a47 = _p 1043;;
let __a80 = _p 1150;;
let __a41 = fun p v -> _p_pos 1084 p (_p 1083 p (v));;
let __a18 = _p 1102;;
let __a26 = _p 1046;;
let __a42 = _p 1096;;
let __a20 = fun p v -> _p 1139 p (_p 1138 p (_p_pos 1136 p (_p 1135 p (v))));;
let __a33 = fun p v -> _p 1094 p (_p 1080 p (v));;
let __a19 = _p 1105;;
let __a52 = _p 1049;;
let __a50 = _p 1212;;
let __a38 = _p 1156;;
let __a34 = _p 1099;;
let __a74 = _p 1108;;
let __a66 = fun p v -> _p 1242 p (_p 1241 p (_p_pos 1239 p (_p 1238 p (v))));;
let __a36 = _p 1022;;
let __a59 = _p 1215;;
let __a39 = _p 1159;;
let __a27 = fun p v -> _p 1079 p (_p 1078 p (_p 1077 p (_p 1076 p (_p 1075 p (_p_pos 1073 p (_p 1072 p (v)))))));;
let __a29 = fun p v -> _p 1154 p (_p 1153 p (v));;
let __a17 = fun p v -> _p_pos 1069 p (_p 1068 p (v));;
let __a30 = _p 1025;;
let __a24 = _p 1218;;
let __a25 = fun p v -> _p_pos 1222 p (_p 1221 p (v));;
let __a14 = _p 1028;;
let __a8 = fun p v -> _p_pos 1132 p (_p 1131 p (v));;
let __a72 = _p 1244;;
let __a2 = fun p v -> _p_pos 1249 p (_p 1248 p (v));;
let __a16 = _p 1052;;
let __a58 = _p 1188;;
let __a61 = _p 1111;;
let __a56 = _p 1161;;
let __a9 = _p 1247;;
let __a4 = _p 1056;;
let __a76 = _p 1114;;
let __a54 = _p 1163;;
let __a77 = _p 1165;;
let __a31 = _p 1059;;
let __a65 = _p 1031;;
let __a53 = fun p v -> _p 1093 p (_p 1092 p (_p 1091 p (_p 1090 p (_p_pos 1088 p (_p 1087 p (v))))));;
let __a23 = _p 1117;;
let __a57 = _p 1167;;
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
(383, [EatInstr(101,414)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,415)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,416)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(103,417)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,418)]);
(4, [AAction2Instr(__a0,15)]);
(388, [EatInstr(105,419)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(99,420)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,17)]);
(390, [EatInstr(101,421)]);
(7, [EatInstr(127,18);EatInstr(126,18);EatInstr(125,18);EatInstr(124,18);EatInstr(123,18);EatInstr(122,18);EatInstr(121,18);EatInstr(120,18);EatInstr(119,18);EatInstr(118,18);EatInstr(117,18);EatInstr(116,18);EatInstr(115,18);EatInstr(114,18);EatInstr(113,18);EatInstr(112,18);EatInstr(111,18);EatInstr(110,18);EatInstr(109,18);EatInstr(108,18);EatInstr(107,18);EatInstr(106,18);EatInstr(105,18);EatInstr(104,18);EatInstr(103,18);EatInstr(102,18);EatInstr(101,18);EatInstr(100,18);EatInstr(99,18);EatInstr(98,18);EatInstr(97,18);EatInstr(96,18);EatInstr(95,18);EatInstr(94,18);EatInstr(93,18);EatInstr(92,18);EatInstr(91,18);EatInstr(90,18);EatInstr(89,18);EatInstr(88,18);EatInstr(87,18);EatInstr(86,18);EatInstr(85,18);EatInstr(84,18);EatInstr(83,18);EatInstr(82,18);EatInstr(81,18);EatInstr(80,18);EatInstr(79,18);EatInstr(78,18);EatInstr(77,18);EatInstr(76,18);EatInstr(75,18);EatInstr(74,18);EatInstr(73,18);EatInstr(72,18);EatInstr(71,18);EatInstr(70,18);EatInstr(69,18);EatInstr(68,18);EatInstr(67,18);EatInstr(66,18);EatInstr(65,18);EatInstr(64,18);EatInstr(63,18);EatInstr(62,18);EatInstr(61,18);EatInstr(60,18);EatInstr(59,18);EatInstr(58,18);EatInstr(57,18);EatInstr(56,18);EatInstr(55,18);EatInstr(54,18);EatInstr(53,18);EatInstr(52,18);EatInstr(51,18);EatInstr(50,18);EatInstr(49,18);EatInstr(48,18);EatInstr(47,18);EatInstr(46,18);EatInstr(44,18);EatInstr(43,18);EatInstr(42,18);EatInstr(41,18);EatInstr(40,18);EatInstr(39,18);EatInstr(38,18);EatInstr(37,18);EatInstr(36,18);EatInstr(35,18);EatInstr(34,18);EatInstr(33,18);EatInstr(32,18);EatInstr(31,18);EatInstr(30,18);EatInstr(29,18);EatInstr(28,18);EatInstr(27,18);EatInstr(26,18);EatInstr(25,18);EatInstr(24,18);EatInstr(23,18);EatInstr(22,18);EatInstr(21,18);EatInstr(20,18);EatInstr(19,18);EatInstr(18,18);EatInstr(17,18);EatInstr(16,18);EatInstr(15,18);EatInstr(14,18);EatInstr(13,18);EatInstr(12,18);EatInstr(11,18);EatInstr(10,18);EatInstr(9,18);EatInstr(8,18);EatInstr(7,18);EatInstr(6,18);EatInstr(5,18);EatInstr(4,18);EatInstr(3,18);EatInstr(2,18);EatInstr(1,18)]);
(391, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,422)]);
(8, [EatInstr(119,29);EatInstr(117,28);EatInstr(115,27);EatInstr(112,26);EatInstr(109,25);EatInstr(108,24);EatInstr(105,23);EatInstr(104,22);EatInstr(100,21);EatInstr(99,20);EatInstr(97,19)]);
(392, [AAction2Instr(__a44,174)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(102,33);EatInstr(101,32);EatInstr(100,31);EatInstr(99,30);AAction2Instr(__a1,40)]);
(393, [AAction2Instr(__a45,174)]);
(10, [EatInstr(45,41);AAction2Instr(__a2,42)]);
(394, [EatInstr(111,423)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43);RCompleteInstr2(274,nullable_eof)]);
(395, [EatInstr(97,424)]);
(12, [CompleteInstr(264)]);
(396, [AAction2Instr(__a46,225)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(45,425)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(108,426)]);
(15, [ACallInstr3(__default_call,9);ASimpleCont2Instr(272,__binder1,45)]);
(399, [AAction2Instr(__a47,225)]);
(16, [CompleteInstr(268)]);
(400, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,427)]);
(17, [ALookaheadInstr(false,CfgLA (1,264),46);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,17)]);
(401, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,428)]);
(18, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,18)]);
(402, [EatInstr(110,429)]);
(19, [EatInstr(116,48)]);
(403, [EatInstr(97,430)]);
(20, [EatInstr(111,50);EatInstr(108,49)]);
(404, [EatInstr(97,431)]);
(21, [EatInstr(101,51)]);
(405, [EatInstr(115,432)]);
(22, [EatInstr(97,52)]);
(406, [EatInstr(98,433)]);
(23, [EatInstr(110,53)]);
(407, [EatInstr(97,434)]);
(24, [EatInstr(105,55);EatInstr(101,54)]);
(408, [EatInstr(97,435)]);
(25, [EatInstr(105,56)]);
(409, [EatInstr(121,436)]);
(26, [EatInstr(114,57)]);
(410, [EatInstr(117,437)]);
(27, [EatInstr(117,58)]);
(411, [EatInstr(101,438)]);
(28, [EatInstr(110,59)]);
(412, [EatInstr(120,439)]);
(29, [EatInstr(114,60)]);
(413, [EatInstr(115,440)]);
(30, [EatInstr(111,61)]);
(414, [EatInstr(108,441)]);
(31, [EatInstr(111,63);EatInstr(105,62)]);
(415, [AAction2Instr(__a48,442);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,415)]);
(32, [EatInstr(120,64)]);
(416, [AAction2Instr(__a49,174)]);
(33, [EatInstr(117,65)]);
(417, [EatInstr(117,443)]);
(34, [EatInstr(110,66)]);
(418, [AAction2Instr(__a50,174)]);
(35, [EatInstr(114,68);EatInstr(111,67)]);
(419, [EatInstr(115,444)]);
(36, [EatInstr(114,69)]);
(420, [EatInstr(101,445)]);
(37, [EatInstr(102,70)]);
(421, [EatInstr(45,446)]);
(38, [EatInstr(116,71)]);
(422, [AAction2Instr(__a51,174)]);
(39, [EatInstr(114,72)]);
(423, [EatInstr(114,447)]);
(40, [ASimpleCont2Instr(271,__binder2,73);ACallInstr3(__default_call,8)]);
(424, [EatInstr(114,448)]);
(41, [EatInstr(118,85);EatInstr(117,84);EatInstr(115,83);EatInstr(114,82);EatInstr(111,81);EatInstr(110,80);EatInstr(109,79);EatInstr(108,78);EatInstr(105,77);EatInstr(99,76);EatInstr(98,75);EatInstr(97,74)]);
(425, [EatInstr(99,449)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,86)]);
(426, [EatInstr(97,450)]);
(43, [CompleteInstr(274)]);
(427, [AAction2Instr(__a52,225)]);
(428, [AAction2Instr(__a53,305)]);
(45, [AAction2Instr(__a3,87)]);
(429, [EatInstr(97,451)]);
(46, [CompleteInstr(269)]);
(430, [EatInstr(100,452)]);
(47, [CompleteInstr(270)]);
(431, [EatInstr(110,453)]);
(48, [EatInstr(116,88)]);
(432, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,454)]);
(49, [EatInstr(111,89)]);
(433, [EatInstr(108,455)]);
(50, [EatInstr(112,90)]);
(434, [EatInstr(110,456)]);
(51, [EatInstr(115,91)]);
(435, [EatInstr(99,457)]);
(52, [EatInstr(115,92)]);
(436, [EatInstr(112,458)]);
(53, [EatInstr(108,93)]);
(437, [EatInstr(110,459)]);
(54, [EatInstr(120,94)]);
(438, [EatInstr(103,460)]);
(55, [EatInstr(102,95)]);
(439, [AAction2Instr(__a54,461)]);
(56, [EatInstr(110,96)]);
(440, [EatInstr(105,462)]);
(57, [EatInstr(101,97)]);
(441, [EatInstr(115,463)]);
(58, [EatInstr(98,98)]);
(442, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,464)]);
(59, [EatInstr(114,99)]);
(443, [EatInstr(108,465)]);
(60, [EatInstr(97,100)]);
(444, [EatInstr(116,466)]);
(61, [EatInstr(109,101)]);
(445, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,467)]);
(62, [EatInstr(115,102)]);
(446, [EatInstr(104,468)]);
(63, [EatInstr(116,103)]);
(447, [EatInstr(121,469)]);
(64, [EatInstr(116,105);EatInstr(101,104)]);
(448, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,470)]);
(65, [EatInstr(115,106)]);
(449, [EatInstr(111,471)]);
(66, [EatInstr(102,107)]);
(450, [EatInstr(114,472)]);
(67, [EatInstr(111,108)]);
(451, [EatInstr(108,473)]);
(68, [EatInstr(49,109)]);
(452, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,474)]);
(69, [EatInstr(105,111);EatInstr(101,110)]);
(453, [EatInstr(97,475)]);
(70, [EatInstr(99,112)]);
(454, [AAction2Instr(__a55,115)]);
(71, [EatInstr(114,113)]);
(455, [EatInstr(101,476)]);
(72, [EatInstr(97,114)]);
(456, [EatInstr(99,477)]);
(73, [AAction2Instr(__a4,115)]);
(457, [EatInstr(116,478)]);
(74, [EatInstr(102,116)]);
(458, [EatInstr(103,479)]);
(75, [EatInstr(97,117)]);
(459, [AAction2Instr(__a56,461)]);
(76, [EatInstr(111,120);EatInstr(104,119);EatInstr(97,118)]);
(460, [EatInstr(45,480);AAction2Instr(__a57,461)]);
(77, [EatInstr(110,121)]);
(461, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,481)]);
(78, [EatInstr(111,122)]);
(462, [EatInstr(116,482)]);
(79, [EatInstr(101,123)]);
(463, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,483)]);
(80, [EatInstr(111,124)]);
(464, [AAction2Instr(__a58,174)]);
(81, [EatInstr(110,125)]);
(465, [EatInstr(97,484)]);
(82, [EatInstr(111,126)]);
(466, [EatInstr(111,485)]);
(83, [EatInstr(107,127)]);
(467, [AAction2Instr(__a59,174)]);
(84, [EatInstr(110,128)]);
(468, [EatInstr(105,486)]);
(85, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,129)]);
(469, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,487)]);
(86, [AAction2Instr(__a5,130)]);
(470, [AAction2Instr(__a60,488)]);
(87, [AAction2Instr(__a7,132);AAction2Instr(__a6,131)]);
(471, [EatInstr(114,489)]);
(88, [EatInstr(114,133)]);
(472, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,490)]);
(89, [EatInstr(115,134)]);
(473, [EatInstr(121,491)]);
(90, [EatInstr(121,135)]);
(474, [AAction2Instr(__a61,115)]);
(91, [EatInstr(117,136)]);
(475, [EatInstr(108,492)]);
(92, [EatInstr(104,137)]);
(476, [EatInstr(45,493)]);
(93, [EatInstr(105,138)]);
(477, [EatInstr(101,494)]);
(94, [EatInstr(101,139)]);
(478, [EatInstr(105,495)]);
(95, [EatInstr(116,140)]);
(479, [EatInstr(101,496)]);
(96, [EatInstr(117,141)]);
(480, [EatInstr(115,497)]);
(97, [EatInstr(99,142)]);
(481, [AAction2Instr(__a62,174)]);
(98, [EatInstr(115,143)]);
(482, [EatInstr(105,498)]);
(99, [EatInstr(111,144)]);
(483, [AAction2Instr(__a63,174)]);
(100, [EatInstr(112,145)]);
(484, [EatInstr(114,499)]);
(101, [EatInstr(112,146)]);
(485, [EatInstr(114,500)]);
(102, [EatInstr(112,147)]);
(486, [EatInstr(115,501)]);
(103, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,148)]);
(487, [AAction2Instr(__a64,174)]);
(104, [EatInstr(99,149)]);
(488, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,502)]);
(105, [EatInstr(114,150)]);
(489, [EatInstr(101,503)]);
(106, [EatInstr(101,151)]);
(490, [AAction2Instr(__a65,225)]);
(107, [EatInstr(111,152)]);
(491, [EatInstr(115,504)]);
(108, [EatInstr(107,153)]);
(492, [EatInstr(121,505)]);
(109, [EatInstr(45,154)]);
(493, [EatInstr(112,506)]);
(110, [EatInstr(99,155)]);
(494, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,507)]);
(111, [EatInstr(110,156)]);
(495, [EatInstr(111,508)]);
(112, [AAction2Instr(__a8,157)]);
(496, [EatInstr(110,509)]);
(113, [EatInstr(105,158)]);
(497, [EatInstr(116,510)]);
(114, [EatInstr(110,159)]);
(498, [EatInstr(118,511)]);
(115, [CompleteInstr(272)]);
(499, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,512)]);
(116, [EatInstr(116,160)]);
(500, [EatInstr(121,513)]);
(117, [EatInstr(99,161)]);
(501, [EatInstr(116,514)]);
(118, [EatInstr(115,162)]);
(502, [AAction2Instr(__a66,515);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,502)]);
(119, [EatInstr(101,163)]);
(503, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,516)]);
(120, [EatInstr(117,164)]);
(504, [EatInstr(105,517)]);
(121, [EatInstr(108,165)]);
(505, [EatInstr(115,518)]);
(122, [EatInstr(111,166)]);
(506, [EatInstr(114,519)]);
(123, [EatInstr(109,167)]);
(507, [AAction2Instr(__a67,115)]);
(124, [EatInstr(45,168)]);
(508, [EatInstr(110,520)]);
(125, [EatInstr(108,169)]);
(509, [EatInstr(45,522);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,521)]);
(126, [EatInstr(111,170)]);
(510, [EatInstr(114,523)]);
(127, [EatInstr(105,171)]);
(511, [EatInstr(101,524)]);
(128, [EatInstr(114,173);EatInstr(105,172)]);
(512, [AAction2Instr(__a68,174)]);
(129, [AAction2Instr(__a9,174)]);
(513, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(130, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,175)]);
(514, [EatInstr(111,526)]);
(131, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder3,176)]);
(515, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,527)]);
(132, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,177)]);
(516, [AAction2Instr(__a69,225)]);
(133, [EatInstr(105,178)]);
(517, [EatInstr(115,528)]);
(134, [EatInstr(101,179)]);
(518, [EatInstr(105,529)]);
(135, [EatInstr(114,180)]);
(519, [EatInstr(101,530)]);
(136, [EatInstr(103,181)]);
(520, [EatInstr(115,531)]);
(137, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,182)]);
(521, [AAction2Instr(__a70,115)]);
(138, [EatInstr(110,183)]);
(522, [EatInstr(115,532)]);
(139, [EatInstr(114,184)]);
(523, [EatInstr(105,533)]);
(140, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,185)]);
(524, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,534)]);
(141, [EatInstr(115,186)]);
(525, [AAction2Instr(__a71,174)]);
(142, [EatInstr(101,187)]);
(526, [EatInstr(114,535)]);
(143, [EatInstr(101,188)]);
(527, [AAction2Instr(__a72,174)]);
(144, [EatInstr(108,189)]);
(528, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,536)]);
(145, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,190)]);
(529, [EatInstr(115,537)]);
(146, [EatInstr(105,191)]);
(530, [EatInstr(100,538)]);
(147, [EatInstr(97,192)]);
(531, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,539)]);
(148, [AAction2Instr(__a10,115)]);
(532, [EatInstr(99,540)]);
(149, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,193)]);
(533, [EatInstr(99,541)]);
(150, [EatInstr(97,194)]);
(534, [AAction2Instr(__a73,174)]);
(151, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,195)]);
(535, [EatInstr(121,542)]);
(152, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,196)]);
(536, [AAction2Instr(__a74,115)]);
(153, [EatInstr(97,197)]);
(537, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,543)]);
(154, [EatInstr(108,198)]);
(538, [EatInstr(105,544)]);
(155, [EatInstr(101,199)]);
(539, [AAction2Instr(__a75,115)]);
(156, [EatInstr(116,200)]);
(540, [EatInstr(97,545)]);
(157, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,201)]);
(541, [EatInstr(116,546)]);
(158, [EatInstr(112,202)]);
(542, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,547)]);
(159, [EatInstr(115,203)]);
(543, [AAction2Instr(__a76,115)]);
(160, [EatInstr(101,204)]);
(544, [EatInstr(99,548)]);
(161, [EatInstr(107,205)]);
(545, [EatInstr(110,549)]);
(162, [EatInstr(101,206)]);
(546, [AAction2Instr(__a77,461)]);
(163, [EatInstr(99,207)]);
(547, [AAction2Instr(__a78,174)]);
(164, [EatInstr(110,208)]);
(548, [EatInstr(97,550)]);
(165, [EatInstr(105,209)]);
(549, [EatInstr(110,551)]);
(166, [EatInstr(107,210)]);
(550, [EatInstr(116,552)]);
(167, [EatInstr(111,211)]);
(551, [EatInstr(101,553)]);
(168, [EatInstr(114,214);EatInstr(109,213);EatInstr(99,212)]);
(552, [EatInstr(101,554)]);
(169, [EatInstr(121,215)]);
(553, [EatInstr(114,555)]);
(170, [EatInstr(116,216)]);
(554, [EatInstr(115,556)]);
(171, [EatInstr(112,217)]);
(555, [EatInstr(108,557)]);
(172, [EatInstr(116,218)]);
(556, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,558)]);
(173, [EatInstr(111,219)]);
(557, [EatInstr(101,559)]);
(174, [CompleteInstr(273)]);
(558, [AAction2Instr(__a79,115)]);
(175, [AAction2Instr(__a11,174)]);
(559, [EatInstr(115,560)]);
(176, [AAction2Instr(__a12,87)]);
(560, [EatInstr(115,561)]);
(177, [AAction2Instr(__a13,220)]);
(561, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,562)]);
(178, [EatInstr(98,221)]);
(562, [AAction2Instr(__a80,115)]);
(179, [EatInstr(45,222)]);
(180, [EatInstr(117,223)]);
(181, [EatInstr(97,224)]);
(182, [AAction2Instr(__a14,225)]);
(183, [EatInstr(101,226)]);
(184, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,227)]);
(185, [AAction2Instr(__a15,225)]);
(186, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,228)]);
(187, [EatInstr(100,229)]);
(188, [EatInstr(116,230)]);
(189, [EatInstr(108,231)]);
(190, [AAction2Instr(__a16,225)]);
(191, [EatInstr(108,232)]);
(192, [EatInstr(116,233)]);
(193, [AAction2Instr(__a17,234)]);
(194, [EatInstr(99,235)]);
(195, [AAction2Instr(__a18,115)]);
(196, [AAction2Instr(__a19,115)]);
(197, [EatInstr(104,236)]);
(198, [EatInstr(111,237)]);
(199, [EatInstr(100,238)]);
(200, [EatInstr(45,240);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,239)]);
(201, [AAction2Instr(__a20,241);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,201)]);
(202, [EatInstr(45,242)]);
(203, [EatInstr(108,243)]);
(204, [EatInstr(114,244)]);
(205, [EatInstr(101,245)]);
(206, [EatInstr(45,246)]);
(207, [EatInstr(107,247)]);
(208, [EatInstr(116,248)]);
(209, [EatInstr(110,249)]);
(210, [EatInstr(97,250)]);
(211, [EatInstr(105,251)]);
(212, [EatInstr(111,252)]);
(213, [EatInstr(101,253)]);
(214, [EatInstr(101,254)]);
(215, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,255)]);
(216, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,256)]);
(217, [EatInstr(45,257)]);
(218, [EatInstr(45,258)]);
(219, [EatInstr(108,259)]);
(220, [CompleteInstr(267)]);
(221, [EatInstr(117,260)]);
(222, [EatInstr(117,261)]);
(223, [EatInstr(108,262)]);
(224, [EatInstr(114,263)]);
(225, [CompleteInstr(271)]);
(226, [EatInstr(45,264)]);
(227, [AAction2Instr(__a21,225)]);
(228, [AAction2Instr(__a22,225)]);
(229, [EatInstr(101,265)]);
(230, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,266)]);
(231, [EatInstr(45,267)]);
(232, [EatInstr(101,268)]);
(233, [EatInstr(99,269)]);
(234, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,270)]);
(235, [EatInstr(116,271)]);
(236, [EatInstr(101,272)]);
(237, [EatInstr(111,273)]);
(238, [EatInstr(101,274)]);
(239, [AAction2Instr(__a23,115)]);
(240, [EatInstr(114,277);EatInstr(110,276);EatInstr(103,275)]);
(241, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,278)]);
(242, [EatInstr(108,279)]);
(243, [EatInstr(97,280)]);
(244, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,281)]);
(245, [EatInstr(110,282)]);
(246, [EatInstr(105,283)]);
(247, [EatInstr(45,284)]);
(248, [EatInstr(101,285)]);
(249, [EatInstr(101,286)]);
(250, [EatInstr(104,287)]);
(251, [EatInstr(122,288)]);
(252, [EatInstr(97,289)]);
(253, [EatInstr(109,290)]);
(254, [EatInstr(112,291)]);
(255, [AAction2Instr(__a24,174)]);
(256, [AAction2Instr(__a25,292)]);
(257, [EatInstr(111,293)]);
(258, [EatInstr(104,294)]);
(259, [EatInstr(108,295)]);
(260, [EatInstr(116,296)]);
(261, [EatInstr(110,297)]);
(262, [EatInstr(101,298)]);
(263, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,299)]);
(264, [EatInstr(114,300)]);
(265, [EatInstr(110,301)]);
(266, [AAction2Instr(__a26,225)]);
(267, [EatInstr(115,302)]);
(268, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,303)]);
(269, [EatInstr(104,304)]);
(270, [AAction2Instr(__a27,305)]);
(271, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,306)]);
(272, [EatInstr(97,307)]);
(273, [EatInstr(107,308)]);
(274, [EatInstr(110,309)]);
(275, [EatInstr(105,310)]);
(276, [EatInstr(117,312);EatInstr(112,311)]);
(277, [EatInstr(101,313)]);
(278, [AAction2Instr(__a28,115)]);
(279, [EatInstr(97,314)]);
(280, [EatInstr(116,315)]);
(281, [AAction2Instr(__a29,316)]);
(282, [EatInstr(100,317)]);
(283, [EatInstr(110,318)]);
(284, [EatInstr(108,319)]);
(285, [EatInstr(114,320)]);
(286, [EatInstr(45,321)]);
(287, [EatInstr(101,322)]);
(288, [EatInstr(101,323)]);
(289, [EatInstr(108,324)]);
(290, [EatInstr(111,325)]);
(291, [EatInstr(108,326)]);
(292, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,327)]);
(293, [EatInstr(112,328)]);
(294, [EatInstr(105,329)]);
(295, [EatInstr(45,330)]);
(296, [EatInstr(101,331)]);
(297, [EatInstr(100,332)]);
(298, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,333)]);
(299, [AAction2Instr(__a30,225)]);
(300, [EatInstr(101,334)]);
(301, [EatInstr(99,335)]);
(302, [EatInstr(116,336)]);
(303, [AAction2Instr(__a31,115)]);
(304, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,337)]);
(305, [AAction2Instr(__a33,339);AAction2Instr(__a32,338)]);
(306, [AAction2Instr(__a34,115)]);
(307, [EatInstr(100,340)]);
(308, [EatInstr(97,341)]);
(309, [EatInstr(99,342)]);
(310, [EatInstr(108,343)]);
(311, [EatInstr(114,344)]);
(312, [EatInstr(108,345)]);
(313, [EatInstr(108,346)]);
(314, [EatInstr(116,347)]);
(315, [EatInstr(101,348)]);
(316, [ASimpleCont2Instr(271,__binder4,349);ACallInstr3(__default_call,8)]);
(317, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,350)]);
(318, [EatInstr(115,351)]);
(319, [EatInstr(97,352)]);
(320, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,353)]);
(321, [EatInstr(114,355);EatInstr(99,354)]);
(322, [EatInstr(97,356)]);
(323, [EatInstr(45,357)]);
(324, [EatInstr(101,358)]);
(325, [EatInstr(105,359)]);
(326, [EatInstr(97,360)]);
(327, [AAction2Instr(__a35,361)]);
(328, [EatInstr(116,362)]);
(329, [EatInstr(115,363)]);
(330, [EatInstr(115,364)]);
(331, [EatInstr(115,365)]);
(332, [EatInstr(101,366)]);
(333, [AAction2Instr(__a36,225)]);
(334, [EatInstr(103,367)]);
(335, [EatInstr(101,368)]);
(336, [EatInstr(97,369)]);
(337, [AAction2Instr(__a37,115)]);
(338, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,370)]);
(339, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,371)]);
(340, [EatInstr(45,372)]);
(341, [EatInstr(104,373)]);
(342, [EatInstr(101,374)]);
(343, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,375)]);
(344, [EatInstr(101,376)]);
(345, [EatInstr(108,377)]);
(346, [EatInstr(101,378)]);
(347, [EatInstr(101,379)]);
(348, [EatInstr(45,380)]);
(349, [AAction2Instr(__a38,174)]);
(350, [AAction2Instr(__a39,381)]);
(351, [EatInstr(101,382)]);
(352, [EatInstr(98,383)]);
(353, [AAction2Instr(__a40,384)]);
(354, [EatInstr(115,385)]);
(355, [EatInstr(101,386)]);
(356, [EatInstr(100,387)]);
(357, [EatInstr(104,388)]);
(358, [EatInstr(115,389)]);
(359, [EatInstr(122,390)]);
(360, [EatInstr(121,391)]);
(361, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,392)]);
(362, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,393)]);
(363, [EatInstr(116,394)]);
(364, [EatInstr(116,395)]);
(365, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,396)]);
(366, [EatInstr(114,397)]);
(367, [EatInstr(117,398)]);
(368, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,399)]);
(369, [EatInstr(114,400)]);
(370, [AAction2Instr(__a41,401)]);
(371, [AAction2Instr(__a42,115)]);
(372, [EatInstr(97,402)]);
(373, [EatInstr(101,403)]);
(374, [EatInstr(45,404)]);
(375, [AAction2Instr(__a43,115)]);
(376, [EatInstr(100,405)]);
(377, [EatInstr(97,406)]);
(378, [EatInstr(118,407)]);
(379, [EatInstr(45,408)]);
(380, [EatInstr(100,409)]);
(381, [EatInstr(116,412);EatInstr(112,411);EatInstr(102,410)]);
(382, [EatInstr(110,413)]);
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
