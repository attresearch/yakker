
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
let __a36 = (_p 1226);;
let __a10 = (_p 1034);;
let __a70 = (_p 1202);;
let __a61 = (_p 1194);;
let __a65 = (_p 1170);;
let __a5 = (_p_pos_only 1268);;
let __a41 = (_p_pos_only 1182);;
let __a40 = (_p 1040);;
let __a68 = (_p 1107);;
let __a49 = (_p 1156);;
let __a46 = (_p_pos_only 1082);;
let __a7 = (_p 1262);;
let __a6 = (fun _x0_ _x1_ -> (((_p_pos_only 1125) _x0_) (((_p 1124) _x0_) _x1_)));;
let __a28 = (_p 1075);;
let __a43 = (_p 1218);;
let __a69 = (_p 1162);;
let __a3 = (_p 1007);;
let __a26 = (_p 1056);;
let __a38 = (_p 1113);;
let __a18 = (_p 1037);;
let __a8 = (_p 1062);;
let __a39 = (_p 1013);;
let __a51 = (_p 1230);;
let __a30 = (_p_pos_only 1242);;
let __a37 = (_p_pos_only 1079);;
let __a29 = (_p 1092);;
let __a22 = (_p 1043);;
let __a33 = (_p 1154);;
let __a34 = (fun _x0_ _x1_ -> (((_p_pos_only 1179) _x0_) (((_p 1178) _x0_) _x1_)));;
let __a16 = (_p 1135);;
let __a60 = (_p 1160);;
let __a48 = (_p 1116);;
let __a15 = (_p_pos_only 1128);;
let __a32 = (_p 1059);;
let __a35 = (_p 1222);;
let __a63 = (_p 1141);;
let __a42 = (_p 1190);;
let __a62 = (_p 1016);;
let __a59 = (_p 1122);;
let __a45 = (_p 1046);;
let __a13 = (_p 1095);;
let __a25 = (_p 1022);;
let __a44 = (_p 1214);;
let __a53 = (fun _x0_ _x1_ -> (((_p_pos_only 1251) _x0_) (((_p 1250) _x0_) _x1_)));;
let __a27 = (_p 1076);;
let __a67 = (_p 1138);;
let __a23 = (_p_pos_only 1069);;
let __a71 = (_p 1119);;
let __a72 = (_p 1144);;
let __a64 = (_p 1198);;
let __a56 = (_p 1206);;
let __a55 = (_p 1174);;
let __a2 = (fun _x0_ _x1_ -> (((_p_pos_only 1265) _x0_) (((_p 1264) _x0_) _x1_)));;
let __a31 = (_p 1019);;
let __a66 = (_p 1101);;
let __a14 = (_p 1098);;
let __a11 = (_p 1049);;
let __a9 = (_p 1025);;
let __a21 = (fun _x0_ _x1_ -> (((_p_pos_only 1239) _x0_) (((_p 1238) _x0_) _x1_)));;
let __a58 = (_p_pos_only 1254);;
let __a1 = (_p 1050);;
let __a4 = (_p 1006);;
let __a12 = (fun _x0_ _x1_ -> (((_p_pos_only 1066) _x0_) (((_p 1065) _x0_) _x1_)));;
let __a17 = (_p 1031);;
let __a24 = (_p 1147);;
let __g0 = (_e);;
let __a20 = (_p 1234);;
let __a54 = (_p 1104);;
let __a52 = (_p 1210);;
let __a47 = (_p 1158);;
let __a57 = (_p 1028);;
let __a19 = (_p 1110);;
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
(383, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,414)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,415)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(103,416)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,417)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(105,418)]);
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [EatInstr(99,419)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(101,420)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,17)]);
(390, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,421)]);
(7, [EatInstr(127,18);EatInstr(126,18);EatInstr(125,18);EatInstr(124,18);EatInstr(123,18);EatInstr(122,18);EatInstr(121,18);EatInstr(120,18);EatInstr(119,18);EatInstr(118,18);EatInstr(117,18);EatInstr(116,18);EatInstr(115,18);EatInstr(114,18);EatInstr(113,18);EatInstr(112,18);EatInstr(111,18);EatInstr(110,18);EatInstr(109,18);EatInstr(108,18);EatInstr(107,18);EatInstr(106,18);EatInstr(105,18);EatInstr(104,18);EatInstr(103,18);EatInstr(102,18);EatInstr(101,18);EatInstr(100,18);EatInstr(99,18);EatInstr(98,18);EatInstr(97,18);EatInstr(96,18);EatInstr(95,18);EatInstr(94,18);EatInstr(93,18);EatInstr(92,18);EatInstr(91,18);EatInstr(90,18);EatInstr(89,18);EatInstr(88,18);EatInstr(87,18);EatInstr(86,18);EatInstr(85,18);EatInstr(84,18);EatInstr(83,18);EatInstr(82,18);EatInstr(81,18);EatInstr(80,18);EatInstr(79,18);EatInstr(78,18);EatInstr(77,18);EatInstr(76,18);EatInstr(75,18);EatInstr(74,18);EatInstr(73,18);EatInstr(72,18);EatInstr(71,18);EatInstr(70,18);EatInstr(69,18);EatInstr(68,18);EatInstr(67,18);EatInstr(66,18);EatInstr(65,18);EatInstr(64,18);EatInstr(63,18);EatInstr(62,18);EatInstr(61,18);EatInstr(60,18);EatInstr(59,18);EatInstr(58,18);EatInstr(57,18);EatInstr(56,18);EatInstr(55,18);EatInstr(54,18);EatInstr(53,18);EatInstr(52,18);EatInstr(51,18);EatInstr(50,18);EatInstr(49,18);EatInstr(48,18);EatInstr(47,18);EatInstr(46,18);EatInstr(44,18);EatInstr(43,18);EatInstr(42,18);EatInstr(41,18);EatInstr(40,18);EatInstr(39,18);EatInstr(38,18);EatInstr(37,18);EatInstr(36,18);EatInstr(35,18);EatInstr(34,18);EatInstr(33,18);EatInstr(32,18);EatInstr(31,18);EatInstr(30,18);EatInstr(29,18);EatInstr(28,18);EatInstr(27,18);EatInstr(26,18);EatInstr(25,18);EatInstr(24,18);EatInstr(23,18);EatInstr(22,18);EatInstr(21,18);EatInstr(20,18);EatInstr(19,18);EatInstr(18,18);EatInstr(17,18);EatInstr(16,18);EatInstr(15,18);EatInstr(14,18);EatInstr(13,18);EatInstr(12,18);EatInstr(11,18);EatInstr(10,18);EatInstr(9,18);EatInstr(8,18);EatInstr(7,18);EatInstr(6,18);EatInstr(5,18);EatInstr(4,18);EatInstr(3,18);EatInstr(2,18);EatInstr(1,18)]);
(391, [EatInstr(112,422)]);
(8, [EatInstr(119,29);EatInstr(117,28);EatInstr(115,27);EatInstr(112,26);EatInstr(109,25);EatInstr(108,24);EatInstr(105,23);EatInstr(104,22);EatInstr(100,21);EatInstr(99,20);EatInstr(97,19)]);
(392, [EatInstr(111,423)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(102,33);EatInstr(101,32);EatInstr(100,31);EatInstr(99,30);AAction2Instr(__a1,40)]);
(393, [EatInstr(97,424)]);
(10, [EatInstr(45,41);AAction2Instr(__a2,42)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43)]);
(394, [AAction2Instr(__a39,223)]);
(395, [EatInstr(45,425)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(108,426)]);
(13, [CompleteInstr(265)]);
(397, [AAction2Instr(__a40,223)]);
(14, [CompleteInstr(266)]);
(398, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,427)]);
(15, [AAction2Instr(__a4,46);AAction2Instr(__a3,45)]);
(399, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,428)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(110,429)]);
(17, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,17)]);
(401, [EatInstr(97,430)]);
(18, [ALookaheadInstr(false,CfgLA (1,264),48);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,18)]);
(402, [EatInstr(97,431)]);
(19, [EatInstr(116,49)]);
(403, [EatInstr(115,432)]);
(20, [EatInstr(111,51);EatInstr(108,50)]);
(404, [EatInstr(98,433)]);
(21, [EatInstr(101,52)]);
(405, [EatInstr(97,434)]);
(22, [EatInstr(97,53)]);
(406, [EatInstr(97,435)]);
(23, [EatInstr(110,54)]);
(407, [EatInstr(121,436)]);
(24, [EatInstr(105,56);EatInstr(101,55)]);
(408, [EatInstr(117,437)]);
(25, [EatInstr(105,57)]);
(409, [EatInstr(101,438)]);
(26, [EatInstr(114,58)]);
(410, [EatInstr(120,439)]);
(27, [EatInstr(117,59)]);
(411, [EatInstr(97,440)]);
(28, [EatInstr(110,60)]);
(412, [EatInstr(115,441)]);
(29, [EatInstr(114,61)]);
(413, [EatInstr(108,442)]);
(30, [EatInstr(111,62)]);
(414, [AAction2Instr(__a41,131);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,414)]);
(31, [EatInstr(111,64);EatInstr(105,63)]);
(415, [AAction2Instr(__a42,174)]);
(32, [EatInstr(120,65)]);
(416, [EatInstr(117,443)]);
(33, [EatInstr(117,66)]);
(417, [AAction2Instr(__a43,174)]);
(34, [EatInstr(110,67)]);
(418, [EatInstr(115,444)]);
(35, [EatInstr(114,69);EatInstr(111,68)]);
(419, [EatInstr(101,445)]);
(36, [EatInstr(114,70)]);
(420, [EatInstr(45,446)]);
(37, [EatInstr(102,71)]);
(421, [AAction2Instr(__a44,174)]);
(38, [EatInstr(116,73);EatInstr(111,72)]);
(422, [EatInstr(116,447)]);
(39, [EatInstr(114,74)]);
(423, [EatInstr(114,448)]);
(40, [AContInstr3(271,__g0,__binder2,75);ACallInstr3(__g0,8)]);
(424, [EatInstr(114,449)]);
(41, [EatInstr(118,86);EatInstr(117,85);EatInstr(114,84);EatInstr(111,83);EatInstr(110,82);EatInstr(109,81);EatInstr(108,80);EatInstr(105,79);EatInstr(99,78);EatInstr(98,77);EatInstr(97,76)]);
(425, [EatInstr(99,450)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,87)]);
(426, [EatInstr(97,451)]);
(43, [CompleteInstr(274)]);
(427, [AAction2Instr(__a45,223)]);
(428, [AAction2Instr(__a46,304)]);
(45, [AContInstr3(273,__g0,__binder3,15);ACallInstr3(__g0,10)]);
(429, [EatInstr(97,452)]);
(46, [ALookaheadInstr(false,CfgLA (3,266),88);ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,88)]);
(430, [EatInstr(100,453)]);
(47, [CompleteInstr(269)]);
(431, [EatInstr(110,454)]);
(48, [CompleteInstr(270)]);
(432, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,455)]);
(49, [EatInstr(116,89)]);
(433, [EatInstr(108,456)]);
(50, [EatInstr(111,90)]);
(434, [EatInstr(110,457)]);
(51, [EatInstr(112,91)]);
(435, [EatInstr(99,458)]);
(52, [EatInstr(115,92)]);
(436, [EatInstr(112,459)]);
(53, [EatInstr(115,93)]);
(437, [EatInstr(110,460)]);
(54, [EatInstr(108,94)]);
(438, [EatInstr(103,461)]);
(55, [EatInstr(120,95)]);
(439, [AAction2Instr(__a47,131)]);
(56, [EatInstr(102,96)]);
(440, [EatInstr(100,462)]);
(57, [EatInstr(110,97)]);
(441, [EatInstr(105,463)]);
(58, [EatInstr(101,98)]);
(442, [EatInstr(115,464)]);
(59, [EatInstr(98,99)]);
(443, [EatInstr(108,465)]);
(60, [EatInstr(114,100)]);
(444, [EatInstr(116,466)]);
(61, [EatInstr(97,101)]);
(445, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,467)]);
(62, [EatInstr(109,102)]);
(446, [EatInstr(104,468)]);
(63, [EatInstr(115,103)]);
(447, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,469)]);
(64, [EatInstr(116,104)]);
(448, [EatInstr(121,470)]);
(65, [EatInstr(116,106);EatInstr(101,105)]);
(449, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,471)]);
(66, [EatInstr(115,107)]);
(450, [EatInstr(111,472)]);
(67, [EatInstr(102,108)]);
(451, [EatInstr(114,473)]);
(68, [EatInstr(111,109)]);
(452, [EatInstr(108,474)]);
(69, [EatInstr(49,110)]);
(453, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,475)]);
(70, [EatInstr(105,112);EatInstr(101,111)]);
(454, [EatInstr(97,476)]);
(71, [EatInstr(99,113)]);
(455, [AAction2Instr(__a48,75)]);
(72, [EatInstr(114,114)]);
(456, [EatInstr(101,477)]);
(73, [EatInstr(114,115)]);
(457, [EatInstr(99,478)]);
(74, [EatInstr(97,116)]);
(458, [EatInstr(116,479)]);
(75, [CompleteInstr(272)]);
(459, [EatInstr(103,480)]);
(76, [EatInstr(102,117)]);
(460, [AAction2Instr(__a49,131)]);
(77, [EatInstr(97,118)]);
(461, [EatInstr(45,481);AAction2Instr(__a50,131)]);
(78, [EatInstr(111,121);EatInstr(104,120);EatInstr(97,119)]);
(462, [EatInstr(108,482)]);
(79, [EatInstr(110,122)]);
(463, [EatInstr(116,483)]);
(80, [EatInstr(111,123)]);
(464, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,484)]);
(81, [EatInstr(101,124)]);
(465, [EatInstr(97,485)]);
(82, [EatInstr(111,125)]);
(466, [EatInstr(111,486)]);
(83, [EatInstr(110,126)]);
(467, [AAction2Instr(__a51,174)]);
(84, [EatInstr(111,127)]);
(468, [EatInstr(105,487)]);
(85, [EatInstr(115,129);EatInstr(110,128)]);
(469, [AAction2Instr(__a52,174)]);
(86, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,130)]);
(470, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,488)]);
(87, [AAction2Instr(__a5,131)]);
(471, [AAction2Instr(__a53,489)]);
(88, [CompleteInstr(267)]);
(472, [EatInstr(114,490)]);
(89, [EatInstr(114,132)]);
(473, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,491)]);
(90, [EatInstr(115,133)]);
(474, [EatInstr(121,492)]);
(91, [EatInstr(121,134)]);
(475, [AAction2Instr(__a54,75)]);
(92, [EatInstr(117,135)]);
(476, [EatInstr(108,493)]);
(93, [EatInstr(104,136)]);
(477, [EatInstr(45,494)]);
(94, [EatInstr(105,137)]);
(478, [EatInstr(101,495)]);
(95, [EatInstr(101,138)]);
(479, [EatInstr(105,496)]);
(96, [EatInstr(116,139)]);
(480, [EatInstr(101,497)]);
(97, [EatInstr(117,140)]);
(481, [EatInstr(115,498)]);
(98, [EatInstr(99,141)]);
(482, [EatInstr(101,499)]);
(99, [EatInstr(115,142)]);
(483, [EatInstr(105,500)]);
(100, [EatInstr(111,143)]);
(484, [AAction2Instr(__a55,174)]);
(101, [EatInstr(112,144)]);
(485, [EatInstr(114,501)]);
(102, [EatInstr(112,145)]);
(486, [EatInstr(114,502)]);
(103, [EatInstr(112,146)]);
(487, [EatInstr(115,503)]);
(104, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,147)]);
(488, [AAction2Instr(__a56,174)]);
(105, [EatInstr(99,148)]);
(489, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,504)]);
(106, [EatInstr(114,149)]);
(490, [EatInstr(101,505)]);
(107, [EatInstr(101,150)]);
(491, [AAction2Instr(__a57,223)]);
(108, [EatInstr(111,151)]);
(492, [EatInstr(115,506)]);
(109, [EatInstr(107,152)]);
(493, [EatInstr(121,507)]);
(110, [EatInstr(45,153)]);
(494, [EatInstr(112,508)]);
(111, [EatInstr(99,154)]);
(495, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,509)]);
(112, [EatInstr(110,155)]);
(496, [EatInstr(111,510)]);
(113, [AAction2Instr(__a6,156)]);
(497, [EatInstr(110,511)]);
(114, [EatInstr(116,157)]);
(498, [EatInstr(116,512)]);
(115, [EatInstr(105,158)]);
(499, [EatInstr(114,513)]);
(116, [EatInstr(110,159)]);
(500, [EatInstr(118,514)]);
(117, [EatInstr(116,160)]);
(501, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,515)]);
(118, [EatInstr(99,161)]);
(502, [EatInstr(121,516)]);
(119, [EatInstr(115,162)]);
(503, [EatInstr(116,517)]);
(120, [EatInstr(101,163)]);
(504, [AAction2Instr(__a58,131);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,504)]);
(121, [EatInstr(117,164)]);
(505, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,518)]);
(122, [EatInstr(108,165)]);
(506, [EatInstr(105,519)]);
(123, [EatInstr(111,166)]);
(507, [EatInstr(115,520)]);
(124, [EatInstr(109,167)]);
(508, [EatInstr(114,521)]);
(125, [EatInstr(45,168)]);
(509, [AAction2Instr(__a59,75)]);
(126, [EatInstr(108,169)]);
(510, [EatInstr(110,522)]);
(127, [EatInstr(111,170)]);
(511, [EatInstr(45,524);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,523)]);
(128, [EatInstr(114,172);EatInstr(105,171)]);
(512, [EatInstr(114,525)]);
(129, [EatInstr(101,173)]);
(513, [AAction2Instr(__a60,131)]);
(130, [AAction2Instr(__a7,174)]);
(514, [EatInstr(101,526)]);
(131, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,174)]);
(515, [AAction2Instr(__a61,174)]);
(132, [EatInstr(105,175)]);
(516, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,527)]);
(133, [EatInstr(101,176)]);
(517, [EatInstr(111,528)]);
(134, [EatInstr(114,177)]);
(518, [AAction2Instr(__a62,223)]);
(135, [EatInstr(103,178)]);
(519, [EatInstr(115,529)]);
(136, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,179)]);
(520, [EatInstr(105,530)]);
(137, [EatInstr(110,180)]);
(521, [EatInstr(101,531)]);
(138, [EatInstr(114,181)]);
(522, [EatInstr(115,532)]);
(139, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,182)]);
(523, [AAction2Instr(__a63,75)]);
(140, [EatInstr(115,183)]);
(524, [EatInstr(115,533)]);
(141, [EatInstr(101,184)]);
(525, [EatInstr(105,534)]);
(142, [EatInstr(101,185)]);
(526, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,535)]);
(143, [EatInstr(108,186)]);
(527, [AAction2Instr(__a64,174)]);
(144, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,187)]);
(528, [EatInstr(114,536)]);
(145, [EatInstr(105,188)]);
(529, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,537)]);
(146, [EatInstr(97,189)]);
(530, [EatInstr(115,538)]);
(147, [AAction2Instr(__a8,75)]);
(531, [EatInstr(100,539)]);
(148, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,190)]);
(532, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,540)]);
(149, [EatInstr(97,191)]);
(533, [EatInstr(99,541)]);
(150, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,192)]);
(534, [EatInstr(99,542)]);
(151, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,193)]);
(535, [AAction2Instr(__a65,174)]);
(152, [EatInstr(97,194)]);
(536, [EatInstr(121,543)]);
(153, [EatInstr(108,195)]);
(537, [AAction2Instr(__a66,75)]);
(154, [EatInstr(101,196)]);
(538, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,544)]);
(155, [EatInstr(116,197)]);
(539, [EatInstr(105,545)]);
(156, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,198)]);
(540, [AAction2Instr(__a67,75)]);
(157, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,199)]);
(541, [EatInstr(97,546)]);
(158, [EatInstr(112,200)]);
(542, [EatInstr(116,547)]);
(159, [EatInstr(115,201)]);
(543, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,548)]);
(160, [EatInstr(101,202)]);
(544, [AAction2Instr(__a68,75)]);
(161, [EatInstr(107,203)]);
(545, [EatInstr(99,549)]);
(162, [EatInstr(101,204)]);
(546, [EatInstr(110,550)]);
(163, [EatInstr(99,205)]);
(547, [AAction2Instr(__a69,131)]);
(164, [EatInstr(110,206)]);
(548, [AAction2Instr(__a70,174)]);
(165, [EatInstr(105,207)]);
(549, [EatInstr(97,551)]);
(166, [EatInstr(107,208)]);
(550, [EatInstr(110,552)]);
(167, [EatInstr(111,209)]);
(551, [EatInstr(116,553)]);
(168, [EatInstr(115,213);EatInstr(114,212);EatInstr(109,211);EatInstr(99,210)]);
(552, [EatInstr(101,554)]);
(169, [EatInstr(121,214)]);
(553, [EatInstr(101,555)]);
(170, [EatInstr(116,215)]);
(554, [EatInstr(114,556)]);
(171, [EatInstr(116,216)]);
(555, [EatInstr(115,557)]);
(172, [EatInstr(111,217)]);
(556, [EatInstr(108,558)]);
(173, [EatInstr(45,218)]);
(557, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,559)]);
(174, [CompleteInstr(273)]);
(558, [EatInstr(101,560)]);
(175, [EatInstr(98,219)]);
(559, [AAction2Instr(__a71,75)]);
(176, [EatInstr(45,220)]);
(560, [EatInstr(115,561)]);
(177, [EatInstr(117,221)]);
(561, [EatInstr(115,562)]);
(178, [EatInstr(97,222)]);
(562, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,563)]);
(179, [AAction2Instr(__a9,223)]);
(563, [AAction2Instr(__a72,75)]);
(180, [EatInstr(101,224)]);
(181, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,225)]);
(182, [AAction2Instr(__a10,223)]);
(183, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,226)]);
(184, [EatInstr(100,227)]);
(185, [EatInstr(116,228)]);
(186, [EatInstr(108,229)]);
(187, [AAction2Instr(__a11,223)]);
(188, [EatInstr(108,230)]);
(189, [EatInstr(116,231)]);
(190, [AAction2Instr(__a12,232)]);
(191, [EatInstr(99,233)]);
(192, [AAction2Instr(__a13,75)]);
(193, [AAction2Instr(__a14,75)]);
(194, [EatInstr(104,234)]);
(195, [EatInstr(111,235)]);
(196, [EatInstr(100,236)]);
(197, [EatInstr(45,238);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,237)]);
(198, [AAction2Instr(__a15,239);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,198)]);
(199, [AAction2Instr(__a16,75)]);
(200, [EatInstr(45,240)]);
(201, [EatInstr(108,241)]);
(202, [EatInstr(114,242)]);
(203, [EatInstr(101,243)]);
(204, [EatInstr(45,244)]);
(205, [EatInstr(107,245)]);
(206, [EatInstr(116,246)]);
(207, [EatInstr(110,247)]);
(208, [EatInstr(97,248)]);
(209, [EatInstr(105,249)]);
(210, [EatInstr(111,250)]);
(211, [EatInstr(101,251)]);
(212, [EatInstr(101,252)]);
(213, [EatInstr(107,253)]);
(214, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,254)]);
(215, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,255)]);
(216, [EatInstr(45,256)]);
(217, [EatInstr(108,257)]);
(218, [EatInstr(102,258)]);
(219, [EatInstr(117,259)]);
(220, [EatInstr(117,260)]);
(221, [EatInstr(108,261)]);
(222, [EatInstr(114,262)]);
(223, [CompleteInstr(271)]);
(224, [EatInstr(45,263)]);
(225, [AAction2Instr(__a17,223)]);
(226, [AAction2Instr(__a18,223)]);
(227, [EatInstr(101,264)]);
(228, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,265)]);
(229, [EatInstr(45,266)]);
(230, [EatInstr(101,267)]);
(231, [EatInstr(99,268)]);
(232, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,269)]);
(233, [EatInstr(116,270)]);
(234, [EatInstr(101,271)]);
(235, [EatInstr(111,272)]);
(236, [EatInstr(101,273)]);
(237, [AAction2Instr(__a19,75)]);
(238, [EatInstr(114,276);EatInstr(110,275);EatInstr(103,274)]);
(239, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,75)]);
(240, [EatInstr(108,277)]);
(241, [EatInstr(97,278)]);
(242, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,279)]);
(243, [EatInstr(110,280)]);
(244, [EatInstr(105,281)]);
(245, [EatInstr(45,282)]);
(246, [EatInstr(101,283)]);
(247, [EatInstr(101,284)]);
(248, [EatInstr(104,285)]);
(249, [EatInstr(122,286)]);
(250, [EatInstr(97,287)]);
(251, [EatInstr(109,288)]);
(252, [EatInstr(112,289)]);
(253, [EatInstr(105,290)]);
(254, [AAction2Instr(__a20,174)]);
(255, [AAction2Instr(__a21,291)]);
(256, [EatInstr(104,292)]);
(257, [EatInstr(108,293)]);
(258, [EatInstr(115,294)]);
(259, [EatInstr(116,295)]);
(260, [EatInstr(110,296)]);
(261, [EatInstr(101,297)]);
(262, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,298)]);
(263, [EatInstr(114,299)]);
(264, [EatInstr(110,300)]);
(265, [AAction2Instr(__a22,223)]);
(266, [EatInstr(115,301)]);
(267, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,302)]);
(268, [EatInstr(104,303)]);
(269, [AAction2Instr(__a23,304)]);
(270, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,305)]);
(271, [EatInstr(97,306)]);
(272, [EatInstr(107,307)]);
(273, [EatInstr(110,308)]);
(274, [EatInstr(105,309)]);
(275, [EatInstr(117,311);EatInstr(112,310)]);
(276, [EatInstr(101,312)]);
(277, [EatInstr(97,313)]);
(278, [EatInstr(116,314)]);
(279, [AAction2Instr(__a24,315)]);
(280, [EatInstr(100,316)]);
(281, [EatInstr(110,317)]);
(282, [EatInstr(108,318)]);
(283, [EatInstr(114,319)]);
(284, [EatInstr(45,320)]);
(285, [EatInstr(101,321)]);
(286, [EatInstr(101,322)]);
(287, [EatInstr(108,323)]);
(288, [EatInstr(111,324)]);
(289, [EatInstr(108,325)]);
(290, [EatInstr(112,326)]);
(291, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,327)]);
(292, [EatInstr(105,328)]);
(293, [EatInstr(45,329)]);
(294, [EatInstr(116,331);EatInstr(109,330)]);
(295, [EatInstr(101,332)]);
(296, [EatInstr(100,333)]);
(297, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,334)]);
(298, [AAction2Instr(__a25,223)]);
(299, [EatInstr(101,335)]);
(300, [EatInstr(99,336)]);
(301, [EatInstr(116,337)]);
(302, [AAction2Instr(__a26,75)]);
(303, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,338)]);
(304, [AAction2Instr(__a28,239);AAction2Instr(__a27,339)]);
(305, [AAction2Instr(__a29,75)]);
(306, [EatInstr(100,340)]);
(307, [EatInstr(97,341)]);
(308, [EatInstr(99,342)]);
(309, [EatInstr(108,343)]);
(310, [EatInstr(114,344)]);
(311, [EatInstr(108,345)]);
(312, [EatInstr(108,346)]);
(313, [EatInstr(116,347)]);
(314, [EatInstr(101,348)]);
(315, [AContInstr3(271,__g0,__binder4,174);ACallInstr3(__g0,8)]);
(316, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,349)]);
(317, [EatInstr(115,350)]);
(318, [EatInstr(97,351)]);
(319, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,352)]);
(320, [EatInstr(114,354);EatInstr(99,353)]);
(321, [EatInstr(97,355)]);
(322, [EatInstr(45,356)]);
(323, [EatInstr(101,357)]);
(324, [EatInstr(105,358)]);
(325, [EatInstr(97,359)]);
(326, [EatInstr(45,360)]);
(327, [AAction2Instr(__a30,131)]);
(328, [EatInstr(115,361)]);
(329, [EatInstr(115,362)]);
(330, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,363)]);
(331, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,364)]);
(332, [EatInstr(115,365)]);
(333, [EatInstr(101,366)]);
(334, [AAction2Instr(__a31,223)]);
(335, [EatInstr(103,367)]);
(336, [EatInstr(101,368)]);
(337, [EatInstr(97,369)]);
(338, [AAction2Instr(__a32,75)]);
(339, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,370)]);
(340, [EatInstr(45,371)]);
(341, [EatInstr(104,372)]);
(342, [EatInstr(101,373)]);
(343, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,374)]);
(344, [EatInstr(101,375)]);
(345, [EatInstr(108,376)]);
(346, [EatInstr(101,377)]);
(347, [EatInstr(101,378)]);
(348, [EatInstr(45,379)]);
(349, [AAction2Instr(__a33,380)]);
(350, [EatInstr(101,381)]);
(351, [EatInstr(98,382)]);
(352, [AAction2Instr(__a34,383)]);
(353, [EatInstr(115,384)]);
(354, [EatInstr(101,385)]);
(355, [EatInstr(100,386)]);
(356, [EatInstr(104,387)]);
(357, [EatInstr(115,388)]);
(358, [EatInstr(122,389)]);
(359, [EatInstr(121,390)]);
(360, [EatInstr(111,391)]);
(361, [EatInstr(116,392)]);
(362, [EatInstr(116,393)]);
(363, [AAction2Instr(__a35,174)]);
(364, [AAction2Instr(__a36,174)]);
(365, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,394)]);
(366, [EatInstr(114,395)]);
(367, [EatInstr(117,396)]);
(368, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,397)]);
(369, [EatInstr(114,398)]);
(370, [AAction2Instr(__a37,399)]);
(371, [EatInstr(97,400)]);
(372, [EatInstr(101,401)]);
(373, [EatInstr(45,402)]);
(374, [AAction2Instr(__a38,75)]);
(375, [EatInstr(100,403)]);
(376, [EatInstr(97,404)]);
(377, [EatInstr(118,405)]);
(378, [EatInstr(45,406)]);
(379, [EatInstr(100,407)]);
(380, [EatInstr(119,411);EatInstr(116,410);EatInstr(112,409);EatInstr(102,408)]);
(381, [EatInstr(110,412)]);
(382, [EatInstr(101,413)]);
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
