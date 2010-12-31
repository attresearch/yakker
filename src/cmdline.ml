
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
  let memoize = false
end
module Yk_History = Yak.History.Make(Yk_Hashed)

(*REPLAY PROLOGUE*)
let rec
_r_cmd_line_args(_n,_ps,ykinput) = (
 (let c = (_r_command(_n,_ps,ykinput))
 in (
 (let _x19 = (
 (let rec _x22 _x20 = 
 (match _n() with
 | (1006) -> (_x20)
 | _(*1007*) -> (_x22(_r_args(_n,_ps,ykinput)))
 ) in _x22(())))
 in ( cmd := c )
))
))

 and
_r_phases(_n,_ps,ykinput) = 
 (match _n() with
 | (1013) -> ( Attributes_cmd )
 | (1016) -> ( Close_under_core_cmd )
 | (1019) -> ( Copyrule_cmd )
 | (1022) -> ( Desugar_cmd )
 | (1025) -> ( Hash_cmd )
 | (1028) -> ( Inline_regular_cmd )
 | (1031) -> ( Lexer_cmd )
 | (1034) -> ( Lift_cmd )
 | (1037) -> ( Minus_cmd )
 | (1040) -> ( Tx_prec_cmd )
 | (1043) -> ( Subset_cmd )
 | (1046) -> ( Unroll_star_cmd )
 | _(*1049*) -> ( Wrap_cmd )
 )
 and
_r_command(_n,_ps,ykinput) = 
 (match _n() with
 | (1050) -> (
 (let p = (_r_phases(_n,_ps,ykinput))
 in ( (match p with
                                               Inline_regular_cmd -> Compileopt.inline_regular := true
                                             | Unroll_star_cmd -> if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1
                                             | _ -> ());
                                            p )
))
 | (1056) -> ( Compile_cmd )
 | (1059) -> ( Dispatch_cmd )
 | (1062) -> ( Dot_cmd )
 | (1065) -> (
 (let _x4 = (_ps())
 in (
 (let _x3 = (_ps())
 in (
 (let f = (Yak.YkBuf.get_string _x4 _x3 ykinput)
 in (
 (let l = (
 (let _x8 = (
 (let rec _x24 _x8 = 
 (match _n() with
 | (1075) -> (_x8)
 | _(*1076*) -> (_x24(
 (let _x7 = (
 (let _x6 = (_ps())
 in (
 (let _x5 = (_ps())
 in (
 (let x = (Yak.YkBuf.get_string _x6 _x5 ykinput)
 in (x)
))
))
))
 in (_x7::_x8)
)))
 ) in _x24(Yak.Util.nil)))
 in ((List.rev _x8))
))
 in ( files := f::!files; exec_l := l; Exec_cmd )
))
))
))
))
 | (1092) -> ( Extract_cmd )
 | (1095) -> ( Compileopt.coalesce := true; Fuse_cmd )
 | (1098) -> ( Info_cmd )
 | (1101) -> ( Lookahead_analysis_cmd )
 | (1104) -> ( Lr1_lookahead_cmd )
 | (1107) -> ( Precedence_analysis_cmd )
 | (1110) -> ( Print_gul_cmd )
 | (1113) -> ( Print_gil_cmd )
 | (1116) -> ( Print_npreds_cmd )
 | (1119) -> ( Print_npreds_cmd )
 | (1122) -> ( Print_relevance_cmd )
 | (1124) -> (
 (let _x10 = (_ps())
 in (
 (let _x9 = (_ps())
 in (
 (let n = (Yak.YkBuf.get_string _x10 _x9 ykinput)
 in ( try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" )
))
))
))
 | (1135) -> ( Strip_late_actions_cmd )
 | (1138) -> ( Translate_dypgen_cmd )
 | _(*1141*) -> ( Translate_dypgen_scannerless_cmd )
 )
 and
_r_args(_n,_ps,ykinput) = 
 (match _n() with
 | (1144) -> (
 (let p = (_r_phases(_n,_ps,ykinput))
 in (
 (let _x43 = ( after := Some p )
 in ()))
))
 | (1151) -> (
 (let b = 
 (match _n() with
 | (1153) -> (Fun_BE)
 | (1155) -> (Trans_BE)
 | (1157) -> (Peg_BE false)
 | _(*1159*) -> (Peg_BE true)
 ) in (
 (let _x42 = ( backend := b )
 in ()))
))
 | (1165) -> (
 (let _x41 = ( Compileopt.case_sensitive := false )
 in ()))
 | (1169) -> (
 (let _x40 = ( Compileopt.check_labels := true )
 in ()))
 | (1173) -> (
 (let _x12 = (_ps())
 in (
 (let _x11 = (_ps())
 in (
 (let n = (Yak.YkBuf.get_string _x12 _x11 ykinput)
 in (
 (let _x39 = ( Variables.counter := (int_of_string n) )
 in ()))
))
))
))
 | (1185) -> (
 (let _x38 = ( Compileopt.inline_cs := true )
 in ()))
 | (1189) -> (
 (let _x37 = ( Compileopt.inline_regular := true )
 in ()))
 | (1193) -> (
 (let _x36 = ( Compileopt.memoize_history := true )
 in ()))
 | (1197) -> (
 (let _x35 = ( Compileopt.memoize_history := false )
 in ()))
 | (1201) -> (
 (let _x34 = ( Compileopt.unit_history := true )
 in ()))
 | (1205) -> (
 (let _x33 = ( Compileopt.skip_opt := false )
 in ()))
 | (1209) -> (
 (let _x32 = ( Compileopt.repress_replay := true )
 in ()))
 | (1213) -> (
 (let _x31 = ( Compileopt.lookahead := true )
 in ()))
 | (1217) -> (
 (let _x30 = ( Compileopt.coalesce := false )
 in ()))
 | (1221) -> (
 (let _x29 = ( only := true )
 in ()))
 | (1225) -> (
 (let _x14 = (_ps())
 in (
 (let _x13 = (_ps())
 in (
 (let x = (Yak.YkBuf.get_string _x14 _x13 ykinput)
 in (
 (let _x28 = ( roots := x::!roots )
 in ()))
))
))
))
 | (1237) -> (
 (let _x16 = (_ps())
 in (
 (let _x15 = (_ps())
 in (
 (let n = (Yak.YkBuf.get_string _x16 _x15 ykinput)
 in (
 (let _x27 = ( Compileopt.unroll_star_n := (int_of_string n) )
 in ()))
))
))
))
 | (1249) -> (
 (let _x26 = ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 in ()))
 | _(*1251*) -> (
 (let _x18 = (_ps())
 in (
 (let _x17 = (_ps())
 in (
 (let f = (Yak.YkBuf.get_string _x18 _x17 ykinput)
 in (
 (let _x25 = ( files := f::!files )
 in ()))
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
let _p_pos x p = (fun h->(h#push p ((x),p))#push p ((x),p))
let _p_pos_only x p = (fun h->h#push p ((x),p))
let _m x p = (fun h1 h2-> h1#merge p ((x),p) h2)

let sv_eq x y = sv_compare x y = 0
let key_eq (i,v1) (j,v2) = i = j &&  sv_eq v1 v2
let key_hash (i,v) = i lxor (sv_hash v)

(** Hashtable for top-down parsing. *)
module TDHashtable = Hashtbl.Make(struct type t = int * sv let equal = key_eq let hash = key_hash end)

let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a51 = _p 1169;;
let __a13 = _p 1034;;
let __a67 = _p 1141;;
let __a17 = _p 1037;;
let __a59 = _p 1193;;
let __a52 = _p 1201;;
let __a22 = _p 1144;;
let __a41 = _p_pos_only 1082;;
let __a65 = _p 1197;;
let __a48 = _p 1205;;
let __a7 = _p 1062;;
let __a35 = _p 1013;;
let __a58 = _p 1016;;
let __a55 = _p 1122;;
let __a28 = _p_pos_only 1229;;
let __a40 = _p 1209;;
let __a30 = _p 1019;;
let __a36 = _p 1040;;
let __a8 = fun p v -> _p_pos_only 1066 p (_p 1065 p (v));;
let __a4 = _p_pos_only 1255;;
let __a26 = _p 1092;;
let __a21 = _p 1043;;
let __a61 = _p 1101;;
let __a31 = _p 1151;;
let __a9 = _p 1095;;
let __a42 = _p 1046;;
let __a50 = _p 1104;;
let __a45 = _p 1153;;
let __a20 = _p_pos_only 1069;;
let __a14 = _p 1049;;
let __a43 = _p 1155;;
let __a10 = _p 1098;;
let __a63 = _p 1107;;
let __a39 = _p 1213;;
let __a37 = _p_pos_only 1177;;
let __a11 = _p_pos_only 1128;;
let __a64 = _p 1157;;
let __a27 = _p 1022;;
let __a49 = fun p v -> _p_pos_only 1238 p (_p 1237 p (v));;
let __a46 = _p 1159;;
let __a32 = fun p v -> _p_pos_only 1174 p (_p 1173 p (v));;
let __a47 = _p 1217;;
let __a12 = _p 1025;;
let __a25 = _p 1075;;
let __a24 = _p 1076;;
let __a53 = _p 1028;;
let __a62 = _p 1135;;
let __a38 = _p 1185;;
let __a0 = _p 1050;;
let __a5 = fun p v -> _p_pos_only 1125 p (_p 1124 p (v));;
let __a1 = fun p v -> _p_pos_only 1252 p (_p 1251 p (v));;
let __a57 = _p 1138;;
let __a15 = _p 1110;;
let __a56 = _p 1189;;
let __a19 = fun p v -> _p_pos_only 1226 p (_p 1225 p (v));;
let __a3 = _p 1006;;
let __a34 = _p 1113;;
let __a23 = _p 1056;;
let __a2 = _p 1007;;
let __a54 = _p_pos_only 1241;;
let __a6 = _p 1249;;
let __a33 = _p_pos_only 1079;;
let __a18 = _p 1221;;
let __a60 = _p 1165;;
let __a29 = _p 1059;;
let __a44 = _p 1116;;
let __a16 = _p 1031;;
let __a66 = _p 1119;;
let __binder0 = __default_ret;;
let __binder1 = _m 1002;;
let __binder2 = _m 1052;;
let __binder3 = _m 1008;;
let __binder4 = _m 1146;;
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
(383, [EatInstr(97,413)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(97,414)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(115,415)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(98,416)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(97,417)]);
(4, [EatInstr(116,24);EatInstr(115,23);EatInstr(114,22);EatInstr(112,21);EatInstr(108,20);EatInstr(105,19);EatInstr(102,18);EatInstr(101,17);EatInstr(100,16);EatInstr(99,15);AAction2Instr(__a0,26);ASimpleCont2Instr(272,__binder1,25)]);
(388, [EatInstr(97,418)]);
(5, [EatInstr(0,27)]);
(389, [EatInstr(121,419)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,28)]);
(390, [AAction2Instr(__a35,228)]);
(7, [EatInstr(127,29);EatInstr(126,29);EatInstr(125,29);EatInstr(124,29);EatInstr(123,29);EatInstr(122,29);EatInstr(121,29);EatInstr(120,29);EatInstr(119,29);EatInstr(118,29);EatInstr(117,29);EatInstr(116,29);EatInstr(115,29);EatInstr(114,29);EatInstr(113,29);EatInstr(112,29);EatInstr(111,29);EatInstr(110,29);EatInstr(109,29);EatInstr(108,29);EatInstr(107,29);EatInstr(106,29);EatInstr(105,29);EatInstr(104,29);EatInstr(103,29);EatInstr(102,29);EatInstr(101,29);EatInstr(100,29);EatInstr(99,29);EatInstr(98,29);EatInstr(97,29);EatInstr(96,29);EatInstr(95,29);EatInstr(94,29);EatInstr(93,29);EatInstr(92,29);EatInstr(91,29);EatInstr(90,29);EatInstr(89,29);EatInstr(88,29);EatInstr(87,29);EatInstr(86,29);EatInstr(85,29);EatInstr(84,29);EatInstr(83,29);EatInstr(82,29);EatInstr(81,29);EatInstr(80,29);EatInstr(79,29);EatInstr(78,29);EatInstr(77,29);EatInstr(76,29);EatInstr(75,29);EatInstr(74,29);EatInstr(73,29);EatInstr(72,29);EatInstr(71,29);EatInstr(70,29);EatInstr(69,29);EatInstr(68,29);EatInstr(67,29);EatInstr(66,29);EatInstr(65,29);EatInstr(64,29);EatInstr(63,29);EatInstr(62,29);EatInstr(61,29);EatInstr(60,29);EatInstr(59,29);EatInstr(58,29);EatInstr(57,29);EatInstr(56,29);EatInstr(55,29);EatInstr(54,29);EatInstr(53,29);EatInstr(52,29);EatInstr(51,29);EatInstr(50,29);EatInstr(49,29);EatInstr(48,29);EatInstr(47,29);EatInstr(46,29);EatInstr(44,29);EatInstr(43,29);EatInstr(42,29);EatInstr(41,29);EatInstr(40,29);EatInstr(39,29);EatInstr(38,29);EatInstr(37,29);EatInstr(36,29);EatInstr(35,29);EatInstr(34,29);EatInstr(33,29);EatInstr(32,29);EatInstr(31,29);EatInstr(30,29);EatInstr(29,29);EatInstr(28,29);EatInstr(27,29);EatInstr(26,29);EatInstr(25,29);EatInstr(24,29);EatInstr(23,29);EatInstr(22,29);EatInstr(21,29);EatInstr(20,29);EatInstr(19,29);EatInstr(18,29);EatInstr(17,29);EatInstr(16,29);EatInstr(15,29);EatInstr(14,29);EatInstr(13,29);EatInstr(12,29);EatInstr(11,29);EatInstr(10,29);EatInstr(9,29);EatInstr(8,29);EatInstr(7,29);EatInstr(6,29);EatInstr(5,29);EatInstr(4,29);EatInstr(3,29);EatInstr(2,29);EatInstr(1,29)]);
(391, [EatInstr(45,420)]);
(8, [EatInstr(119,40);EatInstr(117,39);EatInstr(115,38);EatInstr(112,37);EatInstr(109,36);EatInstr(108,35);EatInstr(105,34);EatInstr(104,33);EatInstr(100,32);EatInstr(99,31);EatInstr(97,30)]);
(392, [EatInstr(108,421)]);
(9, [EatInstr(116,24);EatInstr(115,23);EatInstr(114,22);EatInstr(112,21);EatInstr(108,20);EatInstr(105,19);EatInstr(102,18);EatInstr(101,17);EatInstr(100,16);EatInstr(99,15);AAction2Instr(__a0,26)]);
(393, [AAction2Instr(__a36,228)]);
(10, [EatInstr(45,41);AAction2Instr(__a1,42)]);
(394, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,422)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43);RCompleteInstr2(274,nullable_eof)]);
(395, [EatInstr(117,423)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(101,424)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(120,425)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(115,426)]);
(15, [EatInstr(111,45)]);
(399, [EatInstr(108,427)]);
(16, [EatInstr(111,47);EatInstr(105,46)]);
(400, [AAction2Instr(__a37,128);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,400)]);
(17, [EatInstr(120,48)]);
(401, [AAction2Instr(__a38,169)]);
(18, [EatInstr(117,49)]);
(402, [EatInstr(117,428)]);
(19, [EatInstr(110,50)]);
(403, [AAction2Instr(__a39,169)]);
(20, [EatInstr(114,52);EatInstr(111,51)]);
(404, [EatInstr(115,429)]);
(21, [EatInstr(114,53)]);
(405, [EatInstr(101,430)]);
(22, [EatInstr(102,54)]);
(406, [EatInstr(45,431)]);
(23, [EatInstr(116,55)]);
(407, [AAction2Instr(__a40,169)]);
(24, [EatInstr(114,56)]);
(408, [EatInstr(116,432)]);
(25, [AAction2Instr(__a3,58);AAction2Instr(__a2,57)]);
(409, [EatInstr(114,433)]);
(26, [ASimpleCont2Instr(271,__binder2,59);ACallInstr3(__default_call,8)]);
(410, [EatInstr(114,434)]);
(27, [CompleteInstr(268)]);
(411, [AAction2Instr(__a41,288)]);
(28, [ALookaheadInstr(false,CfgLA (1,264),60);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,28)]);
(412, [EatInstr(97,435)]);
(29, [ALookaheadInstr(false,CfgLA (1,264),61);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,29)]);
(413, [EatInstr(100,436)]);
(30, [EatInstr(116,62)]);
(414, [EatInstr(110,437)]);
(31, [EatInstr(111,64);EatInstr(108,63)]);
(415, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,438)]);
(32, [EatInstr(101,65)]);
(416, [EatInstr(108,439)]);
(33, [EatInstr(97,66)]);
(417, [EatInstr(110,440)]);
(34, [EatInstr(110,67)]);
(418, [EatInstr(99,441)]);
(35, [EatInstr(105,69);EatInstr(101,68)]);
(419, [EatInstr(112,442)]);
(36, [EatInstr(105,70)]);
(420, [EatInstr(99,443)]);
(37, [EatInstr(114,71)]);
(421, [EatInstr(97,444)]);
(38, [EatInstr(117,72)]);
(422, [AAction2Instr(__a42,228)]);
(39, [EatInstr(110,73)]);
(423, [EatInstr(110,445)]);
(40, [EatInstr(114,74)]);
(424, [EatInstr(103,446)]);
(41, [EatInstr(118,85);EatInstr(117,84);EatInstr(114,83);EatInstr(111,82);EatInstr(110,81);EatInstr(109,80);EatInstr(108,79);EatInstr(105,78);EatInstr(99,77);EatInstr(98,76);EatInstr(97,75)]);
(425, [AAction2Instr(__a43,128)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,86)]);
(426, [EatInstr(105,447)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(115,448)]);
(428, [EatInstr(108,449)]);
(45, [EatInstr(109,87)]);
(429, [EatInstr(116,450)]);
(46, [EatInstr(115,88)]);
(430, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,451)]);
(47, [EatInstr(116,89)]);
(431, [EatInstr(104,452)]);
(48, [EatInstr(116,91);EatInstr(101,90)]);
(432, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,453)]);
(49, [EatInstr(115,92)]);
(433, [EatInstr(121,454)]);
(50, [EatInstr(102,93)]);
(434, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,455)]);
(51, [EatInstr(111,94)]);
(435, [EatInstr(108,456)]);
(52, [EatInstr(49,95)]);
(436, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,457)]);
(53, [EatInstr(105,97);EatInstr(101,96)]);
(437, [EatInstr(97,458)]);
(54, [EatInstr(99,98)]);
(438, [AAction2Instr(__a44,59)]);
(55, [EatInstr(114,99)]);
(439, [EatInstr(101,459)]);
(56, [EatInstr(97,100)]);
(440, [EatInstr(99,460)]);
(57, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder3,25)]);
(441, [EatInstr(116,461)]);
(58, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,101)]);
(442, [EatInstr(103,462)]);
(59, [CompleteInstr(272)]);
(443, [EatInstr(111,463)]);
(60, [CompleteInstr(269)]);
(444, [EatInstr(114,464)]);
(61, [CompleteInstr(270)]);
(445, [AAction2Instr(__a45,128)]);
(62, [EatInstr(116,102)]);
(446, [EatInstr(45,465);AAction2Instr(__a46,128)]);
(63, [EatInstr(111,103)]);
(447, [EatInstr(116,466)]);
(64, [EatInstr(112,104)]);
(448, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,467)]);
(65, [EatInstr(115,105)]);
(449, [EatInstr(97,468)]);
(66, [EatInstr(115,106)]);
(450, [EatInstr(111,469)]);
(67, [EatInstr(108,107)]);
(451, [AAction2Instr(__a47,169)]);
(68, [EatInstr(120,108)]);
(452, [EatInstr(105,470)]);
(69, [EatInstr(102,109)]);
(453, [AAction2Instr(__a48,169)]);
(70, [EatInstr(110,110)]);
(454, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,471)]);
(71, [EatInstr(101,111)]);
(455, [AAction2Instr(__a49,472)]);
(72, [EatInstr(98,112)]);
(456, [EatInstr(121,473)]);
(73, [EatInstr(114,113)]);
(457, [AAction2Instr(__a50,59)]);
(74, [EatInstr(97,114)]);
(458, [EatInstr(108,474)]);
(75, [EatInstr(102,115)]);
(459, [EatInstr(45,475)]);
(76, [EatInstr(97,116)]);
(460, [EatInstr(101,476)]);
(77, [EatInstr(111,119);EatInstr(104,118);EatInstr(97,117)]);
(461, [EatInstr(105,477)]);
(78, [EatInstr(110,120)]);
(462, [EatInstr(101,478)]);
(79, [EatInstr(111,121)]);
(463, [EatInstr(114,479)]);
(80, [EatInstr(101,122)]);
(464, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,480)]);
(81, [EatInstr(111,123)]);
(465, [EatInstr(115,481)]);
(82, [EatInstr(110,124)]);
(466, [EatInstr(105,482)]);
(83, [EatInstr(111,125)]);
(467, [AAction2Instr(__a51,169)]);
(84, [EatInstr(110,126)]);
(468, [EatInstr(114,483)]);
(85, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,127)]);
(469, [EatInstr(114,484)]);
(86, [AAction2Instr(__a4,128)]);
(470, [EatInstr(115,485)]);
(87, [EatInstr(112,129)]);
(471, [AAction2Instr(__a52,169)]);
(88, [EatInstr(112,130)]);
(472, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,486)]);
(89, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,131)]);
(473, [EatInstr(115,487)]);
(90, [EatInstr(99,132)]);
(474, [EatInstr(121,488)]);
(91, [EatInstr(114,133)]);
(475, [EatInstr(112,489)]);
(92, [EatInstr(101,134)]);
(476, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,490)]);
(93, [EatInstr(111,135)]);
(477, [EatInstr(111,491)]);
(94, [EatInstr(107,136)]);
(478, [EatInstr(110,492)]);
(95, [EatInstr(45,137)]);
(479, [EatInstr(101,493)]);
(96, [EatInstr(99,138)]);
(480, [AAction2Instr(__a53,228)]);
(97, [EatInstr(110,139)]);
(481, [EatInstr(116,494)]);
(98, [AAction2Instr(__a5,140)]);
(482, [EatInstr(118,495)]);
(99, [EatInstr(105,141)]);
(483, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,496)]);
(100, [EatInstr(110,142)]);
(484, [EatInstr(121,497)]);
(101, [CompleteInstr(267)]);
(485, [EatInstr(116,498)]);
(102, [EatInstr(114,143)]);
(486, [AAction2Instr(__a54,128);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,486)]);
(103, [EatInstr(115,144)]);
(487, [EatInstr(105,499)]);
(104, [EatInstr(121,145)]);
(488, [EatInstr(115,500)]);
(105, [EatInstr(117,146)]);
(489, [EatInstr(114,501)]);
(106, [EatInstr(104,147)]);
(490, [AAction2Instr(__a55,59)]);
(107, [EatInstr(105,148)]);
(491, [EatInstr(110,502)]);
(108, [EatInstr(101,149)]);
(492, [EatInstr(45,504);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,503)]);
(109, [EatInstr(116,150)]);
(493, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,505)]);
(110, [EatInstr(117,151)]);
(494, [EatInstr(114,506)]);
(111, [EatInstr(99,152)]);
(495, [EatInstr(101,507)]);
(112, [EatInstr(115,153)]);
(496, [AAction2Instr(__a56,169)]);
(113, [EatInstr(111,154)]);
(497, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(114, [EatInstr(112,155)]);
(498, [EatInstr(111,509)]);
(115, [EatInstr(116,156)]);
(499, [EatInstr(115,510)]);
(116, [EatInstr(99,157)]);
(500, [EatInstr(105,511)]);
(117, [EatInstr(115,158)]);
(501, [EatInstr(101,512)]);
(118, [EatInstr(101,159)]);
(502, [EatInstr(115,513)]);
(119, [EatInstr(117,160)]);
(503, [AAction2Instr(__a57,59)]);
(120, [EatInstr(108,161)]);
(504, [EatInstr(115,514)]);
(121, [EatInstr(111,162)]);
(505, [AAction2Instr(__a58,228)]);
(122, [EatInstr(109,163)]);
(506, [EatInstr(105,515)]);
(123, [EatInstr(45,164)]);
(507, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,516)]);
(124, [EatInstr(108,165)]);
(508, [AAction2Instr(__a59,169)]);
(125, [EatInstr(111,166)]);
(509, [EatInstr(114,517)]);
(126, [EatInstr(114,168);EatInstr(105,167)]);
(510, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,518)]);
(127, [AAction2Instr(__a6,169)]);
(511, [EatInstr(115,519)]);
(128, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,169)]);
(512, [EatInstr(100,520)]);
(129, [EatInstr(105,170)]);
(513, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,521)]);
(130, [EatInstr(97,171)]);
(514, [EatInstr(99,522)]);
(131, [AAction2Instr(__a7,59)]);
(515, [EatInstr(99,523)]);
(132, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,172)]);
(516, [AAction2Instr(__a60,169)]);
(133, [EatInstr(97,173)]);
(517, [EatInstr(121,524)]);
(134, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,174)]);
(518, [AAction2Instr(__a61,59)]);
(135, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,175)]);
(519, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(136, [EatInstr(97,176)]);
(520, [EatInstr(105,526)]);
(137, [EatInstr(108,177)]);
(521, [AAction2Instr(__a62,59)]);
(138, [EatInstr(101,178)]);
(522, [EatInstr(97,527)]);
(139, [EatInstr(116,179)]);
(523, [EatInstr(116,528)]);
(140, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,180)]);
(524, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,529)]);
(141, [EatInstr(112,181)]);
(525, [AAction2Instr(__a63,59)]);
(142, [EatInstr(115,182)]);
(526, [EatInstr(99,530)]);
(143, [EatInstr(105,183)]);
(527, [EatInstr(110,531)]);
(144, [EatInstr(101,184)]);
(528, [AAction2Instr(__a64,128)]);
(145, [EatInstr(114,185)]);
(529, [AAction2Instr(__a65,169)]);
(146, [EatInstr(103,186)]);
(530, [EatInstr(97,532)]);
(147, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,187)]);
(531, [EatInstr(110,533)]);
(148, [EatInstr(110,188)]);
(532, [EatInstr(116,534)]);
(149, [EatInstr(114,189)]);
(533, [EatInstr(101,535)]);
(150, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,190)]);
(534, [EatInstr(101,536)]);
(151, [EatInstr(115,191)]);
(535, [EatInstr(114,537)]);
(152, [EatInstr(101,192)]);
(536, [EatInstr(115,538)]);
(153, [EatInstr(101,193)]);
(537, [EatInstr(108,539)]);
(154, [EatInstr(108,194)]);
(538, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,540)]);
(155, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,195)]);
(539, [EatInstr(101,541)]);
(156, [EatInstr(101,196)]);
(540, [AAction2Instr(__a66,59)]);
(157, [EatInstr(107,197)]);
(541, [EatInstr(115,542)]);
(158, [EatInstr(101,198)]);
(542, [EatInstr(115,543)]);
(159, [EatInstr(99,199)]);
(543, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,544)]);
(160, [EatInstr(110,200)]);
(544, [AAction2Instr(__a67,59)]);
(161, [EatInstr(105,201)]);
(162, [EatInstr(107,202)]);
(163, [EatInstr(111,203)]);
(164, [EatInstr(115,207);EatInstr(114,206);EatInstr(109,205);EatInstr(99,204)]);
(165, [EatInstr(121,208)]);
(166, [EatInstr(116,209)]);
(167, [EatInstr(116,210)]);
(168, [EatInstr(111,211)]);
(169, [CompleteInstr(273)]);
(170, [EatInstr(108,212)]);
(171, [EatInstr(116,213)]);
(172, [AAction2Instr(__a8,214)]);
(173, [EatInstr(99,215)]);
(174, [AAction2Instr(__a9,59)]);
(175, [AAction2Instr(__a10,59)]);
(176, [EatInstr(104,216)]);
(177, [EatInstr(111,217)]);
(178, [EatInstr(100,218)]);
(179, [EatInstr(45,220);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,219)]);
(180, [AAction2Instr(__a11,221);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,180)]);
(181, [EatInstr(45,222)]);
(182, [EatInstr(108,223)]);
(183, [EatInstr(98,224)]);
(184, [EatInstr(45,225)]);
(185, [EatInstr(117,226)]);
(186, [EatInstr(97,227)]);
(187, [AAction2Instr(__a12,228)]);
(188, [EatInstr(101,229)]);
(189, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,230)]);
(190, [AAction2Instr(__a13,228)]);
(191, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,231)]);
(192, [EatInstr(100,232)]);
(193, [EatInstr(116,233)]);
(194, [EatInstr(108,234)]);
(195, [AAction2Instr(__a14,228)]);
(196, [EatInstr(114,235)]);
(197, [EatInstr(101,236)]);
(198, [EatInstr(45,237)]);
(199, [EatInstr(107,238)]);
(200, [EatInstr(116,239)]);
(201, [EatInstr(110,240)]);
(202, [EatInstr(97,241)]);
(203, [EatInstr(105,242)]);
(204, [EatInstr(111,243)]);
(205, [EatInstr(101,244)]);
(206, [EatInstr(101,245)]);
(207, [EatInstr(107,246)]);
(208, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,247)]);
(209, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,248)]);
(210, [EatInstr(45,249)]);
(211, [EatInstr(108,250)]);
(212, [EatInstr(101,251)]);
(213, [EatInstr(99,252)]);
(214, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,253)]);
(215, [EatInstr(116,254)]);
(216, [EatInstr(101,255)]);
(217, [EatInstr(111,256)]);
(218, [EatInstr(101,257)]);
(219, [AAction2Instr(__a15,59)]);
(220, [EatInstr(114,260);EatInstr(110,259);EatInstr(103,258)]);
(221, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,59)]);
(222, [EatInstr(108,261)]);
(223, [EatInstr(97,262)]);
(224, [EatInstr(117,263)]);
(225, [EatInstr(117,264)]);
(226, [EatInstr(108,265)]);
(227, [EatInstr(114,266)]);
(228, [CompleteInstr(271)]);
(229, [EatInstr(45,267)]);
(230, [AAction2Instr(__a16,228)]);
(231, [AAction2Instr(__a17,228)]);
(232, [EatInstr(101,268)]);
(233, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,269)]);
(234, [EatInstr(45,270)]);
(235, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,271)]);
(236, [EatInstr(110,272)]);
(237, [EatInstr(105,273)]);
(238, [EatInstr(45,274)]);
(239, [EatInstr(101,275)]);
(240, [EatInstr(101,276)]);
(241, [EatInstr(104,277)]);
(242, [EatInstr(122,278)]);
(243, [EatInstr(97,279)]);
(244, [EatInstr(109,280)]);
(245, [EatInstr(112,281)]);
(246, [EatInstr(105,282)]);
(247, [AAction2Instr(__a18,169)]);
(248, [AAction2Instr(__a19,283)]);
(249, [EatInstr(104,284)]);
(250, [EatInstr(108,285)]);
(251, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,286)]);
(252, [EatInstr(104,287)]);
(253, [AAction2Instr(__a20,288)]);
(254, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,289)]);
(255, [EatInstr(97,290)]);
(256, [EatInstr(107,291)]);
(257, [EatInstr(110,292)]);
(258, [EatInstr(105,293)]);
(259, [EatInstr(117,295);EatInstr(112,294)]);
(260, [EatInstr(101,296)]);
(261, [EatInstr(97,297)]);
(262, [EatInstr(116,298)]);
(263, [EatInstr(116,299)]);
(264, [EatInstr(110,300)]);
(265, [EatInstr(101,301)]);
(266, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,302)]);
(267, [EatInstr(114,303)]);
(268, [EatInstr(110,304)]);
(269, [AAction2Instr(__a21,228)]);
(270, [EatInstr(115,305)]);
(271, [AAction2Instr(__a22,306)]);
(272, [EatInstr(100,307)]);
(273, [EatInstr(110,308)]);
(274, [EatInstr(108,309)]);
(275, [EatInstr(114,310)]);
(276, [EatInstr(45,311)]);
(277, [EatInstr(101,312)]);
(278, [EatInstr(101,313)]);
(279, [EatInstr(108,314)]);
(280, [EatInstr(111,315)]);
(281, [EatInstr(108,316)]);
(282, [EatInstr(112,317)]);
(283, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,318)]);
(284, [EatInstr(105,319)]);
(285, [EatInstr(45,320)]);
(286, [AAction2Instr(__a23,59)]);
(287, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,321)]);
(288, [AAction2Instr(__a25,221);AAction2Instr(__a24,322)]);
(289, [AAction2Instr(__a26,59)]);
(290, [EatInstr(100,323)]);
(291, [EatInstr(97,324)]);
(292, [EatInstr(99,325)]);
(293, [EatInstr(108,326)]);
(294, [EatInstr(114,327)]);
(295, [EatInstr(108,328)]);
(296, [EatInstr(108,329)]);
(297, [EatInstr(116,330)]);
(298, [EatInstr(101,331)]);
(299, [EatInstr(101,332)]);
(300, [EatInstr(100,333)]);
(301, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,334)]);
(302, [AAction2Instr(__a27,228)]);
(303, [EatInstr(101,335)]);
(304, [EatInstr(99,336)]);
(305, [EatInstr(116,337)]);
(306, [ASimpleCont2Instr(271,__binder4,169);ACallInstr3(__default_call,8)]);
(307, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,338)]);
(308, [EatInstr(115,339)]);
(309, [EatInstr(97,340)]);
(310, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,341)]);
(311, [EatInstr(114,343);EatInstr(99,342)]);
(312, [EatInstr(97,344)]);
(313, [EatInstr(45,345)]);
(314, [EatInstr(101,346)]);
(315, [EatInstr(105,347)]);
(316, [EatInstr(97,348)]);
(317, [EatInstr(45,349)]);
(318, [AAction2Instr(__a28,128)]);
(319, [EatInstr(115,350)]);
(320, [EatInstr(115,351)]);
(321, [AAction2Instr(__a29,59)]);
(322, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,352)]);
(323, [EatInstr(45,353)]);
(324, [EatInstr(104,354)]);
(325, [EatInstr(101,355)]);
(326, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,356)]);
(327, [EatInstr(101,357)]);
(328, [EatInstr(108,358)]);
(329, [EatInstr(101,359)]);
(330, [EatInstr(101,360)]);
(331, [EatInstr(45,361)]);
(332, [EatInstr(115,362)]);
(333, [EatInstr(101,363)]);
(334, [AAction2Instr(__a30,228)]);
(335, [EatInstr(103,364)]);
(336, [EatInstr(101,365)]);
(337, [EatInstr(97,366)]);
(338, [AAction2Instr(__a31,367)]);
(339, [EatInstr(101,368)]);
(340, [EatInstr(98,369)]);
(341, [AAction2Instr(__a32,370)]);
(342, [EatInstr(115,371)]);
(343, [EatInstr(101,372)]);
(344, [EatInstr(100,373)]);
(345, [EatInstr(104,374)]);
(346, [EatInstr(115,375)]);
(347, [EatInstr(122,376)]);
(348, [EatInstr(121,377)]);
(349, [EatInstr(111,378)]);
(350, [EatInstr(116,379)]);
(351, [EatInstr(116,380)]);
(352, [AAction2Instr(__a33,381)]);
(353, [EatInstr(97,382)]);
(354, [EatInstr(101,383)]);
(355, [EatInstr(45,384)]);
(356, [AAction2Instr(__a34,59)]);
(357, [EatInstr(100,385)]);
(358, [EatInstr(97,386)]);
(359, [EatInstr(118,387)]);
(360, [EatInstr(45,388)]);
(361, [EatInstr(100,389)]);
(362, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,390)]);
(363, [EatInstr(114,391)]);
(364, [EatInstr(117,392)]);
(365, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,393)]);
(366, [EatInstr(114,394)]);
(367, [EatInstr(116,397);EatInstr(112,396);EatInstr(102,395)]);
(368, [EatInstr(110,398)]);
(369, [EatInstr(101,399)]);
(370, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,400)]);
(371, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,401)]);
(372, [EatInstr(103,402)]);
(373, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,403)]);
(374, [EatInstr(105,404)]);
(375, [EatInstr(99,405)]);
(376, [EatInstr(101,406)]);
(377, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,407)]);
(378, [EatInstr(112,408)]);
(379, [EatInstr(111,409)]);
(380, [EatInstr(97,410)]);
(381, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,411)]);
(382, [EatInstr(110,412)]);
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
      let _ps() = (let (_,p) = _o#next() in p) in
      _r_cmd_line_args(_n,_ps,ykinput)
    )

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
