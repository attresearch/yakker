
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
let _e p h = h#empty p
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
let __a9 = _p 1034;;
let __a67 = _p 1141;;
let __a16 = _p 1037;;
let __a59 = _p 1193;;
let __a52 = _p 1201;;
let __a22 = _p 1144;;
let __a42 = _p_pos_only 1082;;
let __a65 = _p 1197;;
let __a48 = _p 1205;;
let __a7 = _p 1062;;
let __a35 = _p 1013;;
let __a57 = _p 1016;;
let __a55 = _p 1122;;
let __a28 = _p_pos_only 1229;;
let __a40 = _p 1209;;
let __a29 = _p 1019;;
let __a36 = _p 1040;;
let __a11 = fun p v -> _p_pos_only 1066 p (_p 1065 p (v));;
let __a4 = _p_pos_only 1255;;
let __a27 = _p 1092;;
let __a20 = _p 1043;;
let __a61 = _p 1101;;
let __a31 = _p 1151;;
let __a12 = _p 1095;;
let __a41 = _p 1046;;
let __a50 = _p 1104;;
let __a45 = _p 1153;;
let __a21 = _p_pos_only 1069;;
let __a10 = _p 1049;;
let __a43 = _p 1155;;
let __a13 = _p 1098;;
let __a63 = _p 1107;;
let __a39 = _p 1213;;
let __a37 = _p_pos_only 1177;;
let __a14 = _p_pos_only 1128;;
let __a64 = _p 1157;;
let __a23 = _p 1022;;
let __a49 = fun p v -> _p_pos_only 1238 p (_p 1237 p (v));;
let __a46 = _p 1159;;
let __a32 = fun p v -> _p_pos_only 1174 p (_p 1173 p (v));;
let __a47 = _p 1217;;
let __a8 = _p 1025;;
let __a26 = _p 1075;;
let __a25 = _p 1076;;
let __a53 = _p 1028;;
let __a62 = _p 1135;;
let __a38 = _p 1185;;
let __a0 = _p 1050;;
let __a5 = fun p v -> _p_pos_only 1125 p (_p 1124 p (v));;
let __a1 = fun p v -> _p_pos_only 1252 p (_p 1251 p (v));;
let __a58 = _p 1138;;
let __a17 = _p 1110;;
let __a56 = _p 1189;;
let __a19 = fun p v -> _p_pos_only 1226 p (_p 1225 p (v));;
let __a3 = _p 1006;;
let __a34 = _p 1113;;
let __a24 = _p 1056;;
let __a2 = _p 1007;;
let __a54 = _p_pos_only 1241;;
let __a6 = _p 1249;;
let __a33 = _p_pos_only 1079;;
let __a18 = _p 1221;;
let __a60 = _p 1165;;
let __a30 = _p 1059;;
let __a44 = _p 1116;;
let __a15 = _p 1031;;
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
(383, [EatInstr(108,412)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [AAction2Instr(__a36,216)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,413)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,414)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(110,415)]);
(4, [AContInstr3(272,_e,__binder1,15);ACallInstr3(_e,9)]);
(388, [EatInstr(97,416)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(97,417)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,17)]);
(390, [EatInstr(115,418)]);
(7, [EatInstr(127,18);EatInstr(126,18);EatInstr(125,18);EatInstr(124,18);EatInstr(123,18);EatInstr(122,18);EatInstr(121,18);EatInstr(120,18);EatInstr(119,18);EatInstr(118,18);EatInstr(117,18);EatInstr(116,18);EatInstr(115,18);EatInstr(114,18);EatInstr(113,18);EatInstr(112,18);EatInstr(111,18);EatInstr(110,18);EatInstr(109,18);EatInstr(108,18);EatInstr(107,18);EatInstr(106,18);EatInstr(105,18);EatInstr(104,18);EatInstr(103,18);EatInstr(102,18);EatInstr(101,18);EatInstr(100,18);EatInstr(99,18);EatInstr(98,18);EatInstr(97,18);EatInstr(96,18);EatInstr(95,18);EatInstr(94,18);EatInstr(93,18);EatInstr(92,18);EatInstr(91,18);EatInstr(90,18);EatInstr(89,18);EatInstr(88,18);EatInstr(87,18);EatInstr(86,18);EatInstr(85,18);EatInstr(84,18);EatInstr(83,18);EatInstr(82,18);EatInstr(81,18);EatInstr(80,18);EatInstr(79,18);EatInstr(78,18);EatInstr(77,18);EatInstr(76,18);EatInstr(75,18);EatInstr(74,18);EatInstr(73,18);EatInstr(72,18);EatInstr(71,18);EatInstr(70,18);EatInstr(69,18);EatInstr(68,18);EatInstr(67,18);EatInstr(66,18);EatInstr(65,18);EatInstr(64,18);EatInstr(63,18);EatInstr(62,18);EatInstr(61,18);EatInstr(60,18);EatInstr(59,18);EatInstr(58,18);EatInstr(57,18);EatInstr(56,18);EatInstr(55,18);EatInstr(54,18);EatInstr(53,18);EatInstr(52,18);EatInstr(51,18);EatInstr(50,18);EatInstr(49,18);EatInstr(48,18);EatInstr(47,18);EatInstr(46,18);EatInstr(44,18);EatInstr(43,18);EatInstr(42,18);EatInstr(41,18);EatInstr(40,18);EatInstr(39,18);EatInstr(38,18);EatInstr(37,18);EatInstr(36,18);EatInstr(35,18);EatInstr(34,18);EatInstr(33,18);EatInstr(32,18);EatInstr(31,18);EatInstr(30,18);EatInstr(29,18);EatInstr(28,18);EatInstr(27,18);EatInstr(26,18);EatInstr(25,18);EatInstr(24,18);EatInstr(23,18);EatInstr(22,18);EatInstr(21,18);EatInstr(20,18);EatInstr(19,18);EatInstr(18,18);EatInstr(17,18);EatInstr(16,18);EatInstr(15,18);EatInstr(14,18);EatInstr(13,18);EatInstr(12,18);EatInstr(11,18);EatInstr(10,18);EatInstr(9,18);EatInstr(8,18);EatInstr(7,18);EatInstr(6,18);EatInstr(5,18);EatInstr(4,18);EatInstr(3,18);EatInstr(2,18);EatInstr(1,18)]);
(391, [EatInstr(98,419)]);
(8, [EatInstr(119,29);EatInstr(117,28);EatInstr(115,27);EatInstr(112,26);EatInstr(109,25);EatInstr(108,24);EatInstr(105,23);EatInstr(104,22);EatInstr(100,21);EatInstr(99,20);EatInstr(97,19)]);
(392, [EatInstr(97,420)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(102,33);EatInstr(101,32);EatInstr(100,31);EatInstr(99,30);AAction2Instr(__a0,40)]);
(393, [EatInstr(97,421)]);
(10, [EatInstr(45,41);AAction2Instr(__a1,42)]);
(394, [EatInstr(121,422)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43);RCompleteInstr2(274,nullable_eof)]);
(395, [EatInstr(117,423)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(101,424)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(120,425)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(115,426)]);
(15, [AAction2Instr(__a3,46);AAction2Instr(__a2,45)]);
(399, [EatInstr(108,427)]);
(16, [CompleteInstr(268)]);
(400, [AAction2Instr(__a37,128);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,400)]);
(17, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,17)]);
(401, [AAction2Instr(__a38,169)]);
(18, [ALookaheadInstr(false,CfgLA (1,264),48);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,18)]);
(402, [EatInstr(117,428)]);
(19, [EatInstr(116,49)]);
(403, [AAction2Instr(__a39,169)]);
(20, [EatInstr(111,51);EatInstr(108,50)]);
(404, [EatInstr(115,429)]);
(21, [EatInstr(101,52)]);
(405, [EatInstr(101,430)]);
(22, [EatInstr(97,53)]);
(406, [EatInstr(45,431)]);
(23, [EatInstr(110,54)]);
(407, [AAction2Instr(__a40,169)]);
(24, [EatInstr(105,56);EatInstr(101,55)]);
(408, [EatInstr(116,432)]);
(25, [EatInstr(105,57)]);
(409, [EatInstr(114,433)]);
(26, [EatInstr(114,58)]);
(410, [EatInstr(114,434)]);
(27, [EatInstr(117,59)]);
(411, [EatInstr(99,435)]);
(28, [EatInstr(110,60)]);
(412, [EatInstr(97,436)]);
(29, [EatInstr(114,61)]);
(413, [AAction2Instr(__a41,216)]);
(30, [EatInstr(111,62)]);
(414, [AAction2Instr(__a42,295)]);
(31, [EatInstr(111,64);EatInstr(105,63)]);
(415, [EatInstr(97,437)]);
(32, [EatInstr(120,65)]);
(416, [EatInstr(100,438)]);
(33, [EatInstr(117,66)]);
(417, [EatInstr(110,439)]);
(34, [EatInstr(110,67)]);
(418, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,440)]);
(35, [EatInstr(114,69);EatInstr(111,68)]);
(419, [EatInstr(108,441)]);
(36, [EatInstr(114,70)]);
(420, [EatInstr(110,442)]);
(37, [EatInstr(102,71)]);
(421, [EatInstr(99,443)]);
(38, [EatInstr(116,72)]);
(422, [EatInstr(112,444)]);
(39, [EatInstr(114,73)]);
(423, [EatInstr(110,445)]);
(40, [AContInstr3(271,_e,__binder2,74);ACallInstr3(_e,8)]);
(424, [EatInstr(103,446)]);
(41, [EatInstr(118,85);EatInstr(117,84);EatInstr(114,83);EatInstr(111,82);EatInstr(110,81);EatInstr(109,80);EatInstr(108,79);EatInstr(105,78);EatInstr(99,77);EatInstr(98,76);EatInstr(97,75)]);
(425, [AAction2Instr(__a43,128)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,86)]);
(426, [EatInstr(105,447)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(115,448)]);
(428, [EatInstr(108,449)]);
(45, [AContInstr3(273,_e,__binder3,15);ACallInstr3(_e,10)]);
(429, [EatInstr(116,450)]);
(46, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,87)]);
(430, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,451)]);
(47, [CompleteInstr(269)]);
(431, [EatInstr(104,452)]);
(48, [CompleteInstr(270)]);
(432, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,453)]);
(49, [EatInstr(116,88)]);
(433, [EatInstr(121,454)]);
(50, [EatInstr(111,89)]);
(434, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,455)]);
(51, [EatInstr(112,90)]);
(435, [EatInstr(111,456)]);
(52, [EatInstr(115,91)]);
(436, [EatInstr(114,457)]);
(53, [EatInstr(115,92)]);
(437, [EatInstr(108,458)]);
(54, [EatInstr(108,93)]);
(438, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,459)]);
(55, [EatInstr(120,94)]);
(439, [EatInstr(97,460)]);
(56, [EatInstr(102,95)]);
(440, [AAction2Instr(__a44,74)]);
(57, [EatInstr(110,96)]);
(441, [EatInstr(101,461)]);
(58, [EatInstr(101,97)]);
(442, [EatInstr(99,462)]);
(59, [EatInstr(98,98)]);
(443, [EatInstr(116,463)]);
(60, [EatInstr(114,99)]);
(444, [EatInstr(103,464)]);
(61, [EatInstr(97,100)]);
(445, [AAction2Instr(__a45,128)]);
(62, [EatInstr(109,101)]);
(446, [EatInstr(45,465);AAction2Instr(__a46,128)]);
(63, [EatInstr(115,102)]);
(447, [EatInstr(116,466)]);
(64, [EatInstr(116,103)]);
(448, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,467)]);
(65, [EatInstr(116,105);EatInstr(101,104)]);
(449, [EatInstr(97,468)]);
(66, [EatInstr(115,106)]);
(450, [EatInstr(111,469)]);
(67, [EatInstr(102,107)]);
(451, [AAction2Instr(__a47,169)]);
(68, [EatInstr(111,108)]);
(452, [EatInstr(105,470)]);
(69, [EatInstr(49,109)]);
(453, [AAction2Instr(__a48,169)]);
(70, [EatInstr(105,111);EatInstr(101,110)]);
(454, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,471)]);
(71, [EatInstr(99,112)]);
(455, [AAction2Instr(__a49,472)]);
(72, [EatInstr(114,113)]);
(456, [EatInstr(114,473)]);
(73, [EatInstr(97,114)]);
(457, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,474)]);
(74, [CompleteInstr(272)]);
(458, [EatInstr(121,475)]);
(75, [EatInstr(102,115)]);
(459, [AAction2Instr(__a50,74)]);
(76, [EatInstr(97,116)]);
(460, [EatInstr(108,476)]);
(77, [EatInstr(111,119);EatInstr(104,118);EatInstr(97,117)]);
(461, [EatInstr(45,477)]);
(78, [EatInstr(110,120)]);
(462, [EatInstr(101,478)]);
(79, [EatInstr(111,121)]);
(463, [EatInstr(105,479)]);
(80, [EatInstr(101,122)]);
(464, [EatInstr(101,480)]);
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
(87, [CompleteInstr(267)]);
(471, [AAction2Instr(__a52,169)]);
(88, [EatInstr(114,129)]);
(472, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,486)]);
(89, [EatInstr(115,130)]);
(473, [EatInstr(101,487)]);
(90, [EatInstr(121,131)]);
(474, [AAction2Instr(__a53,216)]);
(91, [EatInstr(117,132)]);
(475, [EatInstr(115,488)]);
(92, [EatInstr(104,133)]);
(476, [EatInstr(121,489)]);
(93, [EatInstr(105,134)]);
(477, [EatInstr(112,490)]);
(94, [EatInstr(101,135)]);
(478, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,491)]);
(95, [EatInstr(116,136)]);
(479, [EatInstr(111,492)]);
(96, [EatInstr(117,137)]);
(480, [EatInstr(110,493)]);
(97, [EatInstr(99,138)]);
(481, [EatInstr(116,494)]);
(98, [EatInstr(115,139)]);
(482, [EatInstr(118,495)]);
(99, [EatInstr(111,140)]);
(483, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,496)]);
(100, [EatInstr(112,141)]);
(484, [EatInstr(121,497)]);
(101, [EatInstr(112,142)]);
(485, [EatInstr(116,498)]);
(102, [EatInstr(112,143)]);
(486, [AAction2Instr(__a54,128);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,486)]);
(103, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,144)]);
(487, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,499)]);
(104, [EatInstr(99,145)]);
(488, [EatInstr(105,500)]);
(105, [EatInstr(114,146)]);
(489, [EatInstr(115,501)]);
(106, [EatInstr(101,147)]);
(490, [EatInstr(114,502)]);
(107, [EatInstr(111,148)]);
(491, [AAction2Instr(__a55,74)]);
(108, [EatInstr(107,149)]);
(492, [EatInstr(110,503)]);
(109, [EatInstr(45,150)]);
(493, [EatInstr(45,505);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,504)]);
(110, [EatInstr(99,151)]);
(494, [EatInstr(114,506)]);
(111, [EatInstr(110,152)]);
(495, [EatInstr(101,507)]);
(112, [AAction2Instr(__a5,153)]);
(496, [AAction2Instr(__a56,169)]);
(113, [EatInstr(105,154)]);
(497, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(114, [EatInstr(110,155)]);
(498, [EatInstr(111,509)]);
(115, [EatInstr(116,156)]);
(499, [AAction2Instr(__a57,216)]);
(116, [EatInstr(99,157)]);
(500, [EatInstr(115,510)]);
(117, [EatInstr(115,158)]);
(501, [EatInstr(105,511)]);
(118, [EatInstr(101,159)]);
(502, [EatInstr(101,512)]);
(119, [EatInstr(117,160)]);
(503, [EatInstr(115,513)]);
(120, [EatInstr(108,161)]);
(504, [AAction2Instr(__a58,74)]);
(121, [EatInstr(111,162)]);
(505, [EatInstr(115,514)]);
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
(130, [EatInstr(101,171)]);
(514, [EatInstr(99,522)]);
(131, [EatInstr(114,172)]);
(515, [EatInstr(99,523)]);
(132, [EatInstr(103,173)]);
(516, [AAction2Instr(__a60,169)]);
(133, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,174)]);
(517, [EatInstr(121,524)]);
(134, [EatInstr(110,175)]);
(518, [AAction2Instr(__a61,74)]);
(135, [EatInstr(114,176)]);
(519, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(136, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,177)]);
(520, [EatInstr(105,526)]);
(137, [EatInstr(115,178)]);
(521, [AAction2Instr(__a62,74)]);
(138, [EatInstr(101,179)]);
(522, [EatInstr(97,527)]);
(139, [EatInstr(101,180)]);
(523, [EatInstr(116,528)]);
(140, [EatInstr(108,181)]);
(524, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,529)]);
(141, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,182)]);
(525, [AAction2Instr(__a63,74)]);
(142, [EatInstr(105,183)]);
(526, [EatInstr(99,530)]);
(143, [EatInstr(97,184)]);
(527, [EatInstr(110,531)]);
(144, [AAction2Instr(__a7,74)]);
(528, [AAction2Instr(__a64,128)]);
(145, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,185)]);
(529, [AAction2Instr(__a65,169)]);
(146, [EatInstr(97,186)]);
(530, [EatInstr(97,532)]);
(147, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,187)]);
(531, [EatInstr(110,533)]);
(148, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,188)]);
(532, [EatInstr(116,534)]);
(149, [EatInstr(97,189)]);
(533, [EatInstr(101,535)]);
(150, [EatInstr(108,190)]);
(534, [EatInstr(101,536)]);
(151, [EatInstr(101,191)]);
(535, [EatInstr(114,537)]);
(152, [EatInstr(116,192)]);
(536, [EatInstr(115,538)]);
(153, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,193)]);
(537, [EatInstr(108,539)]);
(154, [EatInstr(112,194)]);
(538, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,540)]);
(155, [EatInstr(115,195)]);
(539, [EatInstr(101,541)]);
(156, [EatInstr(101,196)]);
(540, [AAction2Instr(__a66,74)]);
(157, [EatInstr(107,197)]);
(541, [EatInstr(115,542)]);
(158, [EatInstr(101,198)]);
(542, [EatInstr(115,543)]);
(159, [EatInstr(99,199)]);
(543, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,544)]);
(160, [EatInstr(110,200)]);
(544, [AAction2Instr(__a67,74)]);
(161, [EatInstr(105,201)]);
(162, [EatInstr(107,202)]);
(163, [EatInstr(111,203)]);
(164, [EatInstr(115,207);EatInstr(114,206);EatInstr(109,205);EatInstr(99,204)]);
(165, [EatInstr(121,208)]);
(166, [EatInstr(116,209)]);
(167, [EatInstr(116,210)]);
(168, [EatInstr(111,211)]);
(169, [CompleteInstr(273)]);
(170, [EatInstr(98,212)]);
(171, [EatInstr(45,213)]);
(172, [EatInstr(117,214)]);
(173, [EatInstr(97,215)]);
(174, [AAction2Instr(__a8,216)]);
(175, [EatInstr(101,217)]);
(176, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,218)]);
(177, [AAction2Instr(__a9,216)]);
(178, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,219)]);
(179, [EatInstr(100,220)]);
(180, [EatInstr(116,221)]);
(181, [EatInstr(108,222)]);
(182, [AAction2Instr(__a10,216)]);
(183, [EatInstr(108,223)]);
(184, [EatInstr(116,224)]);
(185, [AAction2Instr(__a11,225)]);
(186, [EatInstr(99,226)]);
(187, [AAction2Instr(__a12,74)]);
(188, [AAction2Instr(__a13,74)]);
(189, [EatInstr(104,227)]);
(190, [EatInstr(111,228)]);
(191, [EatInstr(100,229)]);
(192, [EatInstr(45,231);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,230)]);
(193, [AAction2Instr(__a14,232);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,193)]);
(194, [EatInstr(45,233)]);
(195, [EatInstr(108,234)]);
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
(212, [EatInstr(117,251)]);
(213, [EatInstr(117,252)]);
(214, [EatInstr(108,253)]);
(215, [EatInstr(114,254)]);
(216, [CompleteInstr(271)]);
(217, [EatInstr(45,255)]);
(218, [AAction2Instr(__a15,216)]);
(219, [AAction2Instr(__a16,216)]);
(220, [EatInstr(101,256)]);
(221, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,257)]);
(222, [EatInstr(45,258)]);
(223, [EatInstr(101,259)]);
(224, [EatInstr(99,260)]);
(225, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,261)]);
(226, [EatInstr(116,262)]);
(227, [EatInstr(101,263)]);
(228, [EatInstr(111,264)]);
(229, [EatInstr(101,265)]);
(230, [AAction2Instr(__a17,74)]);
(231, [EatInstr(114,268);EatInstr(110,267);EatInstr(103,266)]);
(232, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,74)]);
(233, [EatInstr(108,269)]);
(234, [EatInstr(97,270)]);
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
(251, [EatInstr(116,286)]);
(252, [EatInstr(110,287)]);
(253, [EatInstr(101,288)]);
(254, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,289)]);
(255, [EatInstr(114,290)]);
(256, [EatInstr(110,291)]);
(257, [AAction2Instr(__a20,216)]);
(258, [EatInstr(115,292)]);
(259, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,293)]);
(260, [EatInstr(104,294)]);
(261, [AAction2Instr(__a21,295)]);
(262, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,296)]);
(263, [EatInstr(97,297)]);
(264, [EatInstr(107,298)]);
(265, [EatInstr(110,299)]);
(266, [EatInstr(105,300)]);
(267, [EatInstr(117,302);EatInstr(112,301)]);
(268, [EatInstr(101,303)]);
(269, [EatInstr(97,304)]);
(270, [EatInstr(116,305)]);
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
(286, [EatInstr(101,321)]);
(287, [EatInstr(100,322)]);
(288, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,323)]);
(289, [AAction2Instr(__a23,216)]);
(290, [EatInstr(101,324)]);
(291, [EatInstr(99,325)]);
(292, [EatInstr(116,326)]);
(293, [AAction2Instr(__a24,74)]);
(294, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,327)]);
(295, [AAction2Instr(__a26,232);AAction2Instr(__a25,328)]);
(296, [AAction2Instr(__a27,74)]);
(297, [EatInstr(100,329)]);
(298, [EatInstr(97,330)]);
(299, [EatInstr(99,331)]);
(300, [EatInstr(108,332)]);
(301, [EatInstr(114,333)]);
(302, [EatInstr(108,334)]);
(303, [EatInstr(108,335)]);
(304, [EatInstr(116,336)]);
(305, [EatInstr(101,337)]);
(306, [AContInstr3(271,_e,__binder4,169);ACallInstr3(_e,8)]);
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
(321, [EatInstr(115,352)]);
(322, [EatInstr(101,353)]);
(323, [AAction2Instr(__a29,216)]);
(324, [EatInstr(103,354)]);
(325, [EatInstr(101,355)]);
(326, [EatInstr(97,356)]);
(327, [AAction2Instr(__a30,74)]);
(328, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,357)]);
(329, [EatInstr(45,358)]);
(330, [EatInstr(104,359)]);
(331, [EatInstr(101,360)]);
(332, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,361)]);
(333, [EatInstr(101,362)]);
(334, [EatInstr(108,363)]);
(335, [EatInstr(101,364)]);
(336, [EatInstr(101,365)]);
(337, [EatInstr(45,366)]);
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
(352, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,381)]);
(353, [EatInstr(114,382)]);
(354, [EatInstr(117,383)]);
(355, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,384)]);
(356, [EatInstr(114,385)]);
(357, [AAction2Instr(__a33,386)]);
(358, [EatInstr(97,387)]);
(359, [EatInstr(101,388)]);
(360, [EatInstr(45,389)]);
(361, [AAction2Instr(__a34,74)]);
(362, [EatInstr(100,390)]);
(363, [EatInstr(97,391)]);
(364, [EatInstr(118,392)]);
(365, [EatInstr(45,393)]);
(366, [EatInstr(100,394)]);
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
(381, [AAction2Instr(__a35,216)]);
(382, [EatInstr(45,411)]);
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
