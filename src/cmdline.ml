
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
let _l2hv x = x;; (* label to hv *)

module Yk_Hashed = struct
  type t = hv * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
  let memoize = true
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
 | (1135) -> ( Sort_cmd )
 | (1138) -> ( Strip_late_actions_cmd )
 | (1141) -> ( Translate_dypgen_cmd )
 | _(*1144*) -> ( Translate_dypgen_scannerless_cmd )
 )
 and
_r_args(_n,_ps,ykinput) = 
 (match _n() with
 | (1147) -> (
 (let p = (_r_phases(_n,_ps,ykinput))
 in (
 (let _x45 = ( after := Some p )
 in ()))
))
 | (1154) -> (
 (let b = 
 (match _n() with
 | (1156) -> (Fun_BE)
 | (1158) -> (Trans_BE)
 | (1160) -> (Wadler_BE)
 | (1162) -> (Peg_BE false)
 | _(*1164*) -> (Peg_BE true)
 ) in (
 (let _x44 = ( backend := b )
 in ()))
))
 | (1170) -> (
 (let _x43 = ( Compileopt.case_sensitive := false )
 in ()))
 | (1174) -> (
 (let _x42 = ( Compileopt.check_labels := true )
 in ()))
 | (1178) -> (
 (let _x12 = (_ps())
 in (
 (let _x11 = (_ps())
 in (
 (let n = (Yak.YkBuf.get_string _x12 _x11 ykinput)
 in (
 (let _x41 = ( Variables.counter := (int_of_string n) )
 in ()))
))
))
))
 | (1190) -> (
 (let _x40 = ( Compileopt.inline_cs := true )
 in ()))
 | (1194) -> (
 (let _x39 = ( Compileopt.inline_regular := true )
 in ()))
 | (1198) -> (
 (let _x38 = ( Compileopt.memoize_history := true )
 in ()))
 | (1202) -> (
 (let _x37 = ( Compileopt.memoize_history := false )
 in ()))
 | (1206) -> (
 (let _x36 = ( Compileopt.unit_history := true )
 in ()))
 | (1210) -> (
 (let _x35 = ( Compileopt.skip_opt := false )
 in ()))
 | (1214) -> (
 (let _x34 = ( Compileopt.repress_replay := true )
 in ()))
 | (1218) -> (
 (let _x33 = ( Compileopt.lookahead := true )
 in ()))
 | (1222) -> (
 (let _x32 = ( Compileopt.use_fsm := true )
 in ()))
 | (1226) -> (
 (let _x31 = ( Compileopt.use_fsm := false )
 in ()))
 | (1230) -> (
 (let _x30 = ( Compileopt.coalesce := false )
 in ()))
 | (1234) -> (
 (let _x29 = ( only := true )
 in ()))
 | (1238) -> (
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
 | (1250) -> (
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
 | (1262) -> (
 (let _x26 = ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 in ()))
 | _(*1264*) -> (
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
module Pred3 = Yak.Pam_internal.Pred3
module SV_hashtbl = Hashtbl.Make(struct
                          type t = sv
                          let equal a b = sv_compare a b = 0
                          let hash = Hashtbl.hash end)
module Pred = Pred3
let __a34 = (_p 1226);;
let __a10 = (_p 1034);;
let __a70 = (_p 1202);;
let __a60 = (_p 1194);;
let __a65 = (_p 1170);;
let __a3 = (_p_pos_only 1268);;
let __a45 = (_p_pos_only 1182);;
let __a38 = (_p 1040);;
let __a68 = (_p 1107);;
let __a49 = (_p 1156);;
let __a43 = (_p_pos_only 1082);;
let __a7 = (_p 1262);;
let __a47 = (_p 1075);;
let __a6 = (fun _x0_ _x1_ -> (((_p_pos_only 1125) _x0_) (((_p 1124) _x0_) _x1_)));;
let __a40 = (_p 1218);;
let __a69 = (_p 1162);;
let __a4 = (_p 1007);;
let __a26 = (_p 1056);;
let __a36 = (_p 1113);;
let __a17 = (_p 1037);;
let __a8 = (_p 1062);;
let __a37 = (_p 1013);;
let __a51 = (_p 1230);;
let __a28 = (_p_pos_only 1242);;
let __a35 = (_p_pos_only 1079);;
let __a27 = (_p 1092);;
let __a22 = (_p 1043);;
let __a31 = (_p 1154);;
let __a32 = (fun _x0_ _x1_ -> (((_p_pos_only 1179) _x0_) (((_p 1178) _x0_) _x1_)));;
let __a15 = (_p 1135);;
let __a59 = (_p 1160);;
let __a48 = (_p 1116);;
let __a19 = (_p_pos_only 1128);;
let __a30 = (_p 1059);;
let __a33 = (_p 1222);;
let __a63 = (_p 1141);;
let __a39 = (_p 1190);;
let __a62 = (_p 1016);;
let __a58 = (_p 1122);;
let __a42 = (_p 1046);;
let __a13 = (_p 1095);;
let __a25 = (_p 1022);;
let __a41 = (_p 1214);;
let __a53 = (fun _x0_ _x1_ -> (((_p_pos_only 1251) _x0_) (((_p 1250) _x0_) _x1_)));;
let __a46 = (_p 1076);;
let __a67 = (_p 1138);;
let __a23 = (_p_pos_only 1069);;
let __a71 = (_p 1119);;
let __a72 = (_p 1144);;
let __a64 = (_p 1198);;
let __a56 = (_p 1206);;
let __a55 = (_p 1174);;
let __a2 = (fun _x0_ _x1_ -> (((_p_pos_only 1265) _x0_) (((_p 1264) _x0_) _x1_)));;
let __a29 = (_p 1019);;
let __a66 = (_p 1101);;
let __a14 = (_p 1098);;
let __a11 = (_p 1049);;
let __a9 = (_p 1025);;
let __a21 = (fun _x0_ _x1_ -> (((_p_pos_only 1239) _x0_) (((_p 1238) _x0_) _x1_)));;
let __a61 = (_p_pos_only 1254);;
let __a1 = (_p 1050);;
let __a5 = (_p 1006);;
let __a12 = (fun _x0_ _x1_ -> (((_p_pos_only 1066) _x0_) (((_p 1065) _x0_) _x1_)));;
let __a16 = (_p 1031);;
let __a24 = (_p 1147);;
let __g0 = (_e);;
let __a20 = (_p 1234);;
let __a54 = (_p 1104);;
let __a52 = (_p 1210);;
let __a44 = (_p 1158);;
let __a57 = (_p 1028);;
let __a18 = (_p 1110);;
let __a50 = (_p 1164);;
let __binder0 = __default_ret;;
let __binder1 = (_m 1002);;
let __binder2 = (_m 1052);;
let __binder3 = (_m 1008);;
let __binder4 = (_m 1149);;
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
let program : (int * sv instruction list) list = [
(383, [EatInstr(103,413)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,414)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(105,415)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(99,416)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(101,417)]);
(4, [AContInstr3(272,__g0,__binder1,85);ACallInstr3(__g0,9)]);
(388, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,418)]);
(5, [EatInstr(0,15)]);
(389, [EatInstr(112,419)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,43)]);
(390, [EatInstr(111,420)]);
(7, [EatInstr(127,45);EatInstr(126,45);EatInstr(125,45);EatInstr(124,45);EatInstr(123,45);EatInstr(122,45);EatInstr(121,45);EatInstr(120,45);EatInstr(119,45);EatInstr(118,45);EatInstr(117,45);EatInstr(116,45);EatInstr(115,45);EatInstr(114,45);EatInstr(113,45);EatInstr(112,45);EatInstr(111,45);EatInstr(110,45);EatInstr(109,45);EatInstr(108,45);EatInstr(107,45);EatInstr(106,45);EatInstr(105,45);EatInstr(104,45);EatInstr(103,45);EatInstr(102,45);EatInstr(101,45);EatInstr(100,45);EatInstr(99,45);EatInstr(98,45);EatInstr(97,45);EatInstr(96,45);EatInstr(95,45);EatInstr(94,45);EatInstr(93,45);EatInstr(92,45);EatInstr(91,45);EatInstr(90,45);EatInstr(89,45);EatInstr(88,45);EatInstr(87,45);EatInstr(86,45);EatInstr(85,45);EatInstr(84,45);EatInstr(83,45);EatInstr(82,45);EatInstr(81,45);EatInstr(80,45);EatInstr(79,45);EatInstr(78,45);EatInstr(77,45);EatInstr(76,45);EatInstr(75,45);EatInstr(74,45);EatInstr(73,45);EatInstr(72,45);EatInstr(71,45);EatInstr(70,45);EatInstr(69,45);EatInstr(68,45);EatInstr(67,45);EatInstr(66,45);EatInstr(65,45);EatInstr(64,45);EatInstr(63,45);EatInstr(62,45);EatInstr(61,45);EatInstr(60,45);EatInstr(59,45);EatInstr(58,45);EatInstr(57,45);EatInstr(56,45);EatInstr(55,45);EatInstr(54,45);EatInstr(53,45);EatInstr(52,45);EatInstr(51,45);EatInstr(50,45);EatInstr(49,45);EatInstr(48,45);EatInstr(47,45);EatInstr(46,45);EatInstr(44,45);EatInstr(43,45);EatInstr(42,45);EatInstr(41,45);EatInstr(40,45);EatInstr(39,45);EatInstr(38,45);EatInstr(37,45);EatInstr(36,45);EatInstr(35,45);EatInstr(34,45);EatInstr(33,45);EatInstr(32,45);EatInstr(31,45);EatInstr(30,45);EatInstr(29,45);EatInstr(28,45);EatInstr(27,45);EatInstr(26,45);EatInstr(25,45);EatInstr(24,45);EatInstr(23,45);EatInstr(22,45);EatInstr(21,45);EatInstr(20,45);EatInstr(19,45);EatInstr(18,45);EatInstr(17,45);EatInstr(16,45);EatInstr(15,45);EatInstr(14,45);EatInstr(13,45);EatInstr(12,45);EatInstr(11,45);EatInstr(10,45);EatInstr(9,45);EatInstr(8,45);EatInstr(7,45);EatInstr(6,45);EatInstr(5,45);EatInstr(4,45);EatInstr(3,45);EatInstr(2,45);EatInstr(1,45)]);
(391, [EatInstr(97,421)]);
(8, [EatInstr(119,26);EatInstr(117,25);EatInstr(115,24);EatInstr(112,23);EatInstr(109,22);EatInstr(108,21);EatInstr(105,20);EatInstr(104,19);EatInstr(100,18);EatInstr(99,17);EatInstr(97,16)]);
(392, [AAction2Instr(__a37,528)]);
(9, [EatInstr(116,36);EatInstr(115,35);EatInstr(114,34);EatInstr(112,33);EatInstr(108,32);EatInstr(105,31);EatInstr(102,30);EatInstr(101,29);EatInstr(100,28);EatInstr(99,27);AAction2Instr(__a1,37)]);
(393, [EatInstr(45,422)]);
(10, [EatInstr(45,38);AAction2Instr(__a2,39)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),40)]);
(394, [EatInstr(108,423)]);
(395, [AAction2Instr(__a38,528)]);
(12, [CompleteInstr(264)]);
(396, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,424)]);
(13, [CompleteInstr(265)]);
(397, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,425)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(110,426)]);
(15, [CompleteInstr(268)]);
(399, [EatInstr(97,427)]);
(16, [EatInstr(116,47)]);
(400, [EatInstr(97,428)]);
(17, [EatInstr(111,49);EatInstr(108,48)]);
(401, [EatInstr(115,429)]);
(18, [EatInstr(101,50)]);
(402, [EatInstr(98,430)]);
(19, [EatInstr(97,51)]);
(403, [EatInstr(97,431)]);
(20, [EatInstr(110,52)]);
(404, [EatInstr(97,432)]);
(21, [EatInstr(105,54);EatInstr(101,53)]);
(405, [EatInstr(121,433)]);
(22, [EatInstr(105,55)]);
(406, [EatInstr(117,434)]);
(23, [EatInstr(114,56)]);
(407, [EatInstr(101,435)]);
(24, [EatInstr(117,57)]);
(408, [EatInstr(120,436)]);
(25, [EatInstr(110,58)]);
(409, [EatInstr(97,437)]);
(26, [EatInstr(114,59)]);
(410, [EatInstr(115,438)]);
(27, [EatInstr(111,60)]);
(411, [EatInstr(108,439)]);
(28, [EatInstr(111,62);EatInstr(105,61)]);
(412, [AAction2Instr(__a39,171)]);
(29, [EatInstr(120,63)]);
(413, [EatInstr(117,441)]);
(30, [EatInstr(117,64)]);
(414, [AAction2Instr(__a40,171)]);
(31, [EatInstr(110,65)]);
(415, [EatInstr(115,442)]);
(32, [EatInstr(114,67);EatInstr(111,66)]);
(416, [EatInstr(101,443)]);
(33, [EatInstr(114,68)]);
(417, [EatInstr(45,444)]);
(34, [EatInstr(102,69)]);
(418, [AAction2Instr(__a41,171)]);
(35, [EatInstr(116,71);EatInstr(111,70)]);
(419, [EatInstr(116,445)]);
(36, [EatInstr(114,72)]);
(420, [EatInstr(114,446)]);
(37, [AContInstr3(271,__g0,__binder2,187);ACallInstr3(__g0,8)]);
(421, [EatInstr(114,447)]);
(38, [EatInstr(118,83);EatInstr(117,82);EatInstr(114,81);EatInstr(111,80);EatInstr(110,79);EatInstr(109,78);EatInstr(108,77);EatInstr(105,76);EatInstr(99,75);EatInstr(98,74);EatInstr(97,73)]);
(422, [EatInstr(99,448)]);
(39, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,84)]);
(423, [EatInstr(97,449)]);
(40, [CompleteInstr(274)]);
(424, [AAction2Instr(__a42,528)]);
(41, [AContInstr3(273,__g0,__binder3,85);ACallInstr3(__g0,10)]);
(425, [AAction2Instr(__a43,450)]);
(42, [ALookaheadInstr(false,CfgLA (3,266),86);ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,86)]);
(426, [EatInstr(97,451)]);
(43, [ALookaheadInstr(false,CfgLA (1,264),44);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,43)]);
(427, [EatInstr(100,452)]);
(44, [CompleteInstr(269)]);
(428, [EatInstr(110,453)]);
(45, [ALookaheadInstr(false,CfgLA (1,264),46);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,45)]);
(429, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,454)]);
(46, [CompleteInstr(270)]);
(430, [EatInstr(108,455)]);
(47, [EatInstr(116,87)]);
(431, [EatInstr(110,456)]);
(48, [EatInstr(111,88)]);
(432, [EatInstr(99,457)]);
(49, [EatInstr(112,89)]);
(433, [EatInstr(112,458)]);
(50, [EatInstr(115,90)]);
(434, [EatInstr(110,459)]);
(51, [EatInstr(115,91)]);
(435, [EatInstr(103,460)]);
(52, [EatInstr(108,92)]);
(436, [AAction2Instr(__a44,358)]);
(53, [EatInstr(120,93)]);
(437, [EatInstr(100,461)]);
(54, [EatInstr(102,94)]);
(438, [EatInstr(105,462)]);
(55, [EatInstr(110,95)]);
(439, [EatInstr(115,463)]);
(56, [EatInstr(101,96)]);
(440, [AAction2Instr(__a45,358);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,440)]);
(57, [EatInstr(98,97)]);
(441, [EatInstr(108,464)]);
(58, [EatInstr(114,98)]);
(442, [EatInstr(116,465)]);
(59, [EatInstr(97,99)]);
(443, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,466)]);
(60, [EatInstr(109,100)]);
(444, [EatInstr(104,467)]);
(61, [EatInstr(115,101)]);
(445, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,468)]);
(62, [EatInstr(116,102)]);
(446, [EatInstr(121,469)]);
(63, [EatInstr(116,104);EatInstr(101,103)]);
(447, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,470)]);
(64, [EatInstr(115,105)]);
(448, [EatInstr(111,471)]);
(65, [EatInstr(102,106)]);
(449, [EatInstr(114,472)]);
(66, [EatInstr(111,107)]);
(450, [AAction2Instr(__a47,336);AAction2Instr(__a46,335)]);
(67, [EatInstr(49,108)]);
(451, [EatInstr(108,473)]);
(68, [EatInstr(105,110);EatInstr(101,109)]);
(452, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,474)]);
(69, [EatInstr(99,111)]);
(453, [EatInstr(97,475)]);
(70, [EatInstr(114,112)]);
(454, [AAction2Instr(__a48,187)]);
(71, [EatInstr(114,113)]);
(455, [EatInstr(101,476)]);
(72, [EatInstr(97,114)]);
(456, [EatInstr(99,477)]);
(73, [EatInstr(102,115)]);
(457, [EatInstr(116,478)]);
(74, [EatInstr(97,116)]);
(458, [EatInstr(103,479)]);
(75, [EatInstr(111,119);EatInstr(104,118);EatInstr(97,117)]);
(459, [AAction2Instr(__a49,358)]);
(76, [EatInstr(110,120)]);
(460, [EatInstr(45,480);AAction2Instr(__a50,358)]);
(77, [EatInstr(111,121)]);
(461, [EatInstr(108,481)]);
(78, [EatInstr(101,122)]);
(462, [EatInstr(116,482)]);
(79, [EatInstr(111,123)]);
(463, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,483)]);
(80, [EatInstr(110,124)]);
(464, [EatInstr(97,484)]);
(81, [EatInstr(111,125)]);
(465, [EatInstr(111,485)]);
(82, [EatInstr(115,127);EatInstr(110,126)]);
(466, [AAction2Instr(__a51,171)]);
(83, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,128)]);
(467, [EatInstr(105,486)]);
(84, [AAction2Instr(__a3,358)]);
(468, [AAction2Instr(__a52,171)]);
(85, [AAction2Instr(__a5,42);AAction2Instr(__a4,41)]);
(469, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,487)]);
(86, [CompleteInstr(267)]);
(470, [AAction2Instr(__a53,488)]);
(87, [EatInstr(114,129)]);
(471, [EatInstr(114,489)]);
(88, [EatInstr(115,130)]);
(472, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,490)]);
(89, [EatInstr(121,131)]);
(473, [EatInstr(121,491)]);
(90, [EatInstr(117,132)]);
(474, [AAction2Instr(__a54,187)]);
(91, [EatInstr(104,133)]);
(475, [EatInstr(108,492)]);
(92, [EatInstr(105,134)]);
(476, [EatInstr(45,493)]);
(93, [EatInstr(101,135)]);
(477, [EatInstr(101,494)]);
(94, [EatInstr(116,136)]);
(478, [EatInstr(105,495)]);
(95, [EatInstr(117,137)]);
(479, [EatInstr(101,496)]);
(96, [EatInstr(99,138)]);
(480, [EatInstr(115,497)]);
(97, [EatInstr(115,139)]);
(481, [EatInstr(101,498)]);
(98, [EatInstr(111,140)]);
(482, [EatInstr(105,499)]);
(99, [EatInstr(112,141)]);
(483, [AAction2Instr(__a55,171)]);
(100, [EatInstr(112,142)]);
(484, [EatInstr(114,500)]);
(101, [EatInstr(112,143)]);
(485, [EatInstr(114,501)]);
(102, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,144)]);
(486, [EatInstr(115,502)]);
(103, [EatInstr(99,145)]);
(487, [AAction2Instr(__a56,171)]);
(104, [EatInstr(114,146)]);
(488, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,516)]);
(105, [EatInstr(101,147)]);
(489, [EatInstr(101,503)]);
(106, [EatInstr(111,148)]);
(490, [AAction2Instr(__a57,528)]);
(107, [EatInstr(107,149)]);
(491, [EatInstr(115,504)]);
(108, [EatInstr(45,150)]);
(492, [EatInstr(121,505)]);
(109, [EatInstr(99,151)]);
(493, [EatInstr(112,506)]);
(110, [EatInstr(110,152)]);
(494, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,507)]);
(111, [AAction2Instr(__a6,153)]);
(495, [EatInstr(111,508)]);
(112, [EatInstr(116,154)]);
(496, [EatInstr(110,509)]);
(113, [EatInstr(105,155)]);
(497, [EatInstr(116,510)]);
(114, [EatInstr(110,156)]);
(498, [EatInstr(114,511)]);
(115, [EatInstr(116,157)]);
(499, [EatInstr(118,512)]);
(116, [EatInstr(99,158)]);
(500, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,513)]);
(117, [EatInstr(115,159)]);
(501, [EatInstr(121,514)]);
(118, [EatInstr(101,160)]);
(502, [EatInstr(116,515)]);
(119, [EatInstr(117,161)]);
(503, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,517)]);
(120, [EatInstr(108,162)]);
(504, [EatInstr(105,518)]);
(121, [EatInstr(111,163)]);
(505, [EatInstr(115,519)]);
(122, [EatInstr(109,164)]);
(506, [EatInstr(114,520)]);
(123, [EatInstr(45,165)]);
(507, [AAction2Instr(__a58,187)]);
(124, [EatInstr(108,166)]);
(508, [EatInstr(110,521)]);
(125, [EatInstr(111,167)]);
(509, [EatInstr(45,523);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,522)]);
(126, [EatInstr(114,169);EatInstr(105,168)]);
(510, [EatInstr(114,524)]);
(127, [EatInstr(101,170)]);
(511, [AAction2Instr(__a59,358)]);
(128, [AAction2Instr(__a7,171)]);
(512, [EatInstr(101,525)]);
(129, [EatInstr(105,172)]);
(513, [AAction2Instr(__a60,171)]);
(130, [EatInstr(101,173)]);
(514, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,526)]);
(131, [EatInstr(114,174)]);
(515, [EatInstr(111,527)]);
(132, [EatInstr(103,175)]);
(516, [AAction2Instr(__a61,358);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,516)]);
(133, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,176)]);
(517, [AAction2Instr(__a62,528)]);
(134, [EatInstr(110,177)]);
(518, [EatInstr(115,529)]);
(135, [EatInstr(114,178)]);
(519, [EatInstr(105,530)]);
(136, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,179)]);
(520, [EatInstr(101,531)]);
(137, [EatInstr(115,180)]);
(521, [EatInstr(115,532)]);
(138, [EatInstr(101,181)]);
(522, [AAction2Instr(__a63,187)]);
(139, [EatInstr(101,182)]);
(523, [EatInstr(115,533)]);
(140, [EatInstr(108,183)]);
(524, [EatInstr(105,534)]);
(141, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,184)]);
(525, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,535)]);
(142, [EatInstr(105,185)]);
(526, [AAction2Instr(__a64,171)]);
(143, [EatInstr(97,186)]);
(527, [EatInstr(114,536)]);
(144, [AAction2Instr(__a8,187)]);
(528, [CompleteInstr(271)]);
(145, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,188)]);
(529, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,537)]);
(146, [EatInstr(97,189)]);
(530, [EatInstr(115,538)]);
(147, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,190)]);
(531, [EatInstr(100,539)]);
(148, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,191)]);
(532, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,540)]);
(149, [EatInstr(97,192)]);
(533, [EatInstr(99,541)]);
(150, [EatInstr(108,193)]);
(534, [EatInstr(99,542)]);
(151, [EatInstr(101,194)]);
(535, [AAction2Instr(__a65,171)]);
(152, [EatInstr(116,195)]);
(536, [EatInstr(121,543)]);
(153, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,235)]);
(537, [AAction2Instr(__a66,187)]);
(154, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,196)]);
(538, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,544)]);
(155, [EatInstr(112,197)]);
(539, [EatInstr(105,545)]);
(156, [EatInstr(115,198)]);
(540, [AAction2Instr(__a67,187)]);
(157, [EatInstr(101,199)]);
(541, [EatInstr(97,546)]);
(158, [EatInstr(107,200)]);
(542, [EatInstr(116,547)]);
(159, [EatInstr(101,201)]);
(543, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,548)]);
(160, [EatInstr(99,202)]);
(544, [AAction2Instr(__a68,187)]);
(161, [EatInstr(110,203)]);
(545, [EatInstr(99,549)]);
(162, [EatInstr(105,204)]);
(546, [EatInstr(110,550)]);
(163, [EatInstr(107,205)]);
(547, [AAction2Instr(__a69,358)]);
(164, [EatInstr(111,206)]);
(548, [AAction2Instr(__a70,171)]);
(165, [EatInstr(115,210);EatInstr(114,209);EatInstr(109,208);EatInstr(99,207)]);
(549, [EatInstr(97,551)]);
(166, [EatInstr(121,211)]);
(550, [EatInstr(110,552)]);
(167, [EatInstr(116,212)]);
(551, [EatInstr(116,553)]);
(168, [EatInstr(116,213)]);
(552, [EatInstr(101,554)]);
(169, [EatInstr(111,214)]);
(553, [EatInstr(101,555)]);
(170, [EatInstr(45,215)]);
(554, [EatInstr(114,556)]);
(171, [CompleteInstr(273)]);
(555, [EatInstr(115,557)]);
(172, [EatInstr(98,216)]);
(556, [EatInstr(108,558)]);
(173, [EatInstr(45,217)]);
(557, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,559)]);
(174, [EatInstr(117,218)]);
(558, [EatInstr(101,560)]);
(175, [EatInstr(97,219)]);
(559, [AAction2Instr(__a71,187)]);
(176, [AAction2Instr(__a9,528)]);
(560, [EatInstr(115,561)]);
(177, [EatInstr(101,220)]);
(561, [EatInstr(115,562)]);
(178, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,221)]);
(562, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,563)]);
(179, [AAction2Instr(__a10,528)]);
(563, [AAction2Instr(__a72,187)]);
(180, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,222)]);
(181, [EatInstr(100,223)]);
(182, [EatInstr(116,224)]);
(183, [EatInstr(108,225)]);
(184, [AAction2Instr(__a11,528)]);
(185, [EatInstr(108,226)]);
(186, [EatInstr(116,227)]);
(187, [CompleteInstr(272)]);
(188, [AAction2Instr(__a12,228)]);
(189, [EatInstr(99,229)]);
(190, [AAction2Instr(__a13,187)]);
(191, [AAction2Instr(__a14,187)]);
(192, [EatInstr(104,230)]);
(193, [EatInstr(111,231)]);
(194, [EatInstr(100,232)]);
(195, [EatInstr(45,234);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,233)]);
(196, [AAction2Instr(__a15,187)]);
(197, [EatInstr(45,236)]);
(198, [EatInstr(108,237)]);
(199, [EatInstr(114,238)]);
(200, [EatInstr(101,239)]);
(201, [EatInstr(45,240)]);
(202, [EatInstr(107,241)]);
(203, [EatInstr(116,242)]);
(204, [EatInstr(110,243)]);
(205, [EatInstr(97,244)]);
(206, [EatInstr(105,245)]);
(207, [EatInstr(111,246)]);
(208, [EatInstr(101,247)]);
(209, [EatInstr(101,248)]);
(210, [EatInstr(107,249)]);
(211, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,250)]);
(212, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,251)]);
(213, [EatInstr(45,252)]);
(214, [EatInstr(108,253)]);
(215, [EatInstr(102,254)]);
(216, [EatInstr(117,255)]);
(217, [EatInstr(117,256)]);
(218, [EatInstr(108,257)]);
(219, [EatInstr(114,258)]);
(220, [EatInstr(45,260)]);
(221, [AAction2Instr(__a16,528)]);
(222, [AAction2Instr(__a17,528)]);
(223, [EatInstr(101,261)]);
(224, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,262)]);
(225, [EatInstr(45,263)]);
(226, [EatInstr(101,264)]);
(227, [EatInstr(99,265)]);
(228, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,266)]);
(229, [EatInstr(116,267)]);
(230, [EatInstr(101,268)]);
(231, [EatInstr(111,269)]);
(232, [EatInstr(101,270)]);
(233, [AAction2Instr(__a18,187)]);
(234, [EatInstr(114,273);EatInstr(110,272);EatInstr(103,271)]);
(235, [AAction2Instr(__a19,336);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,235)]);
(236, [EatInstr(108,274)]);
(237, [EatInstr(97,275)]);
(238, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,276)]);
(239, [EatInstr(110,277)]);
(240, [EatInstr(105,278)]);
(241, [EatInstr(45,279)]);
(242, [EatInstr(101,280)]);
(243, [EatInstr(101,281)]);
(244, [EatInstr(104,282)]);
(245, [EatInstr(122,283)]);
(246, [EatInstr(97,284)]);
(247, [EatInstr(109,285)]);
(248, [EatInstr(112,286)]);
(249, [EatInstr(105,287)]);
(250, [AAction2Instr(__a20,171)]);
(251, [AAction2Instr(__a21,288)]);
(252, [EatInstr(104,289)]);
(253, [EatInstr(108,290)]);
(254, [EatInstr(115,291)]);
(255, [EatInstr(116,292)]);
(256, [EatInstr(110,293)]);
(257, [EatInstr(101,294)]);
(258, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,295)]);
(260, [EatInstr(114,296)]);
(261, [EatInstr(110,297)]);
(262, [AAction2Instr(__a22,528)]);
(263, [EatInstr(115,298)]);
(264, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,299)]);
(265, [EatInstr(104,300)]);
(266, [AAction2Instr(__a23,450)]);
(267, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,301)]);
(268, [EatInstr(97,302)]);
(269, [EatInstr(107,303)]);
(270, [EatInstr(110,304)]);
(271, [EatInstr(105,305)]);
(272, [EatInstr(117,307);EatInstr(112,306)]);
(273, [EatInstr(101,308)]);
(274, [EatInstr(97,309)]);
(275, [EatInstr(116,310)]);
(276, [AAction2Instr(__a24,311)]);
(277, [EatInstr(100,312)]);
(278, [EatInstr(110,313)]);
(279, [EatInstr(108,314)]);
(280, [EatInstr(114,315)]);
(281, [EatInstr(45,316)]);
(282, [EatInstr(101,317)]);
(283, [EatInstr(101,318)]);
(284, [EatInstr(108,319)]);
(285, [EatInstr(111,320)]);
(286, [EatInstr(108,321)]);
(287, [EatInstr(112,322)]);
(288, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,323)]);
(289, [EatInstr(105,324)]);
(290, [EatInstr(45,325)]);
(291, [EatInstr(116,327);EatInstr(109,326)]);
(292, [EatInstr(101,328)]);
(293, [EatInstr(100,329)]);
(294, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,330)]);
(295, [AAction2Instr(__a25,528)]);
(296, [EatInstr(101,331)]);
(297, [EatInstr(99,332)]);
(298, [EatInstr(116,333)]);
(299, [AAction2Instr(__a26,187)]);
(300, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,334)]);
(301, [AAction2Instr(__a27,187)]);
(302, [EatInstr(100,337)]);
(303, [EatInstr(97,338)]);
(304, [EatInstr(99,339)]);
(305, [EatInstr(108,340)]);
(306, [EatInstr(114,341)]);
(307, [EatInstr(108,342)]);
(308, [EatInstr(108,343)]);
(309, [EatInstr(116,344)]);
(310, [EatInstr(101,345)]);
(311, [AContInstr3(271,__g0,__binder4,171);ACallInstr3(__g0,8)]);
(312, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,346)]);
(313, [EatInstr(115,347)]);
(314, [EatInstr(97,348)]);
(315, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,349)]);
(316, [EatInstr(114,351);EatInstr(99,350)]);
(317, [EatInstr(97,352)]);
(318, [EatInstr(45,353)]);
(319, [EatInstr(101,354)]);
(320, [EatInstr(105,355)]);
(321, [EatInstr(97,356)]);
(322, [EatInstr(45,357)]);
(323, [AAction2Instr(__a28,358)]);
(324, [EatInstr(115,359)]);
(325, [EatInstr(115,360)]);
(326, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,361)]);
(327, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,362)]);
(328, [EatInstr(115,363)]);
(329, [EatInstr(101,364)]);
(330, [AAction2Instr(__a29,528)]);
(331, [EatInstr(103,365)]);
(332, [EatInstr(101,366)]);
(333, [EatInstr(97,367)]);
(334, [AAction2Instr(__a30,187)]);
(335, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,368)]);
(336, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,187)]);
(337, [EatInstr(45,369)]);
(338, [EatInstr(104,370)]);
(339, [EatInstr(101,371)]);
(340, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,372)]);
(341, [EatInstr(101,373)]);
(342, [EatInstr(108,374)]);
(343, [EatInstr(101,375)]);
(344, [EatInstr(101,376)]);
(345, [EatInstr(45,377)]);
(346, [AAction2Instr(__a31,378)]);
(347, [EatInstr(101,379)]);
(348, [EatInstr(98,380)]);
(349, [AAction2Instr(__a32,381)]);
(350, [EatInstr(115,382)]);
(351, [EatInstr(101,383)]);
(352, [EatInstr(100,384)]);
(353, [EatInstr(104,385)]);
(354, [EatInstr(115,386)]);
(355, [EatInstr(122,387)]);
(356, [EatInstr(121,388)]);
(357, [EatInstr(111,389)]);
(358, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,171)]);
(359, [EatInstr(116,390)]);
(360, [EatInstr(116,391)]);
(361, [AAction2Instr(__a33,171)]);
(362, [AAction2Instr(__a34,171)]);
(363, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,392)]);
(364, [EatInstr(114,393)]);
(365, [EatInstr(117,394)]);
(366, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,395)]);
(367, [EatInstr(114,396)]);
(368, [AAction2Instr(__a35,397)]);
(369, [EatInstr(97,398)]);
(370, [EatInstr(101,399)]);
(371, [EatInstr(45,400)]);
(372, [AAction2Instr(__a36,187)]);
(373, [EatInstr(100,401)]);
(374, [EatInstr(97,402)]);
(375, [EatInstr(118,403)]);
(376, [EatInstr(45,404)]);
(377, [EatInstr(100,405)]);
(378, [EatInstr(119,409);EatInstr(116,408);EatInstr(112,407);EatInstr(102,406)]);
(379, [EatInstr(110,410)]);
(380, [EatInstr(101,411)]);
(381, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,440)]);
(382, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,412)]);
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
