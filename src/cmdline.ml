
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
| Dearrow_cmd
| Dispatch_cmd
| Dot_cmd
| Exec_cmd
| Extract_cmd
| Fuse_cmd
| Hash_cmd
| Infer_ty_cmd
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
 | (1028) -> ( Dearrow_cmd )
 | (1031) -> ( Infer_ty_cmd )
 | (1034) -> ( Inline_regular_cmd )
 | (1037) -> ( Lexer_cmd )
 | (1040) -> ( Lift_cmd )
 | (1043) -> ( Minus_cmd )
 | (1046) -> ( Tx_prec_cmd )
 | (1049) -> ( Subset_cmd )
 | (1052) -> ( Unroll_star_cmd )
 | _(*1055*) -> ( Wrap_cmd )
 )
 and
_r_command(_n,_ps,ykinput) =
 (match _n() with
 | (1056) -> (
 (let p = (_r_phases(_n,_ps,ykinput))
 in ( (match p with
                                               Inline_regular_cmd -> Compileopt.inline_regular := true
                                             | Unroll_star_cmd -> if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1
                                             | _ -> ());
                                            p )
))
 | (1062) -> ( Compile_cmd )
 | (1065) -> ( Dispatch_cmd )
 | (1068) -> ( Dot_cmd )
 | (1071) -> (
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
 | (1081) -> (_x8)
 | _(*1082*) -> (_x24(
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
 | (1098) -> ( Extract_cmd )
 | (1101) -> ( Compileopt.coalesce := true; Fuse_cmd )
 | (1104) -> ( Info_cmd )
 | (1107) -> ( Lookahead_analysis_cmd )
 | (1110) -> ( Lr1_lookahead_cmd )
 | (1113) -> ( Precedence_analysis_cmd )
 | (1116) -> ( Print_gul_cmd )
 | (1119) -> ( Print_gil_cmd )
 | (1122) -> ( Print_npreds_cmd )
 | (1125) -> ( Print_npreds_cmd )
 | (1128) -> ( Print_relevance_cmd )
 | (1130) -> (
 (let _x10 = (_ps())
 in (
 (let _x9 = (_ps())
 in (
 (let n = (Yak.YkBuf.get_string _x10 _x9 ykinput)
 in ( try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" )
))
))
))
 | (1141) -> ( Sort_cmd )
 | (1144) -> ( Strip_late_actions_cmd )
 | (1147) -> ( Translate_dypgen_cmd )
 | _(*1150*) -> ( Translate_dypgen_scannerless_cmd )
 )
 and
_r_args(_n,_ps,ykinput) =
 (match _n() with
 | (1153) -> (
 (let p = (_r_phases(_n,_ps,ykinput))
 in (
 (let _x46 = ( after := Some p )
 in ()))
))
 | (1160) -> (
 (let _x45 = ( Compileopt.use_coroutines := false )
 in ()))
 | (1164) -> (
 (let b =
 (match _n() with
 | (1166) -> (Fun_BE)
 | (1168) -> (Trans_BE)
 | (1170) -> (Wadler_BE)
 | (1172) -> (Peg_BE false)
 | _(*1174*) -> (Peg_BE true)
 ) in (
 (let _x44 = ( backend := b )
 in ()))
))
 | (1180) -> (
 (let _x43 = ( Compileopt.case_sensitive := false )
 in ()))
 | (1184) -> (
 (let _x42 = ( Compileopt.check_labels := true )
 in ()))
 | (1188) -> (
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
 | (1200) -> (
 (let _x40 = ( Compileopt.inline_cs := true )
 in ()))
 | (1204) -> (
 (let _x39 = ( Compileopt.inline_regular := true )
 in ()))
 | (1208) -> (
 (let _x38 = ( Compileopt.memoize_history := true )
 in ()))
 | (1212) -> (
 (let _x37 = ( Compileopt.memoize_history := false )
 in ()))
 | (1216) -> (
 (let _x36 = ( Compileopt.unit_history := true )
 in ()))
 | (1220) -> (
 (let _x35 = ( Compileopt.skip_opt := false )
 in ()))
 | (1224) -> (
 (let _x34 = ( Compileopt.repress_replay := true )
 in ()))
 | (1228) -> (
 (let _x33 = ( Compileopt.lookahead := true )
 in ()))
 | (1232) -> (
 (let _x32 = ( Compileopt.use_fsm := true )
 in ()))
 | (1236) -> (
 (let _x31 = ( Compileopt.use_fsm := false )
 in ()))
 | (1240) -> (
 (let _x30 = ( Compileopt.coalesce := false )
 in ()))
 | (1244) -> (
 (let _x29 = ( only := true )
 in ()))
 | (1248) -> (
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
 | (1260) -> (
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
 | (1272) -> (
 (let _x26 = ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 in ()))
 | _(*1274*) -> (
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
let __a59 = (_p 1034);;
let __a62 = (_p 1170);;
let __a2 = (fun _x0_ _x1_ -> (((_p_pos_only 1275) _x0_) (((_p 1274) _x0_) _x1_)));;
let __a10 = (_p 1040);;
let __a33 = (_p 1232);;
let __a69 = (_p 1107);;
let __a21 = (fun _x0_ _x1_ -> (((_p_pos_only 1249) _x0_) (((_p 1248) _x0_) _x1_)));;
let __a71 = (_p 1113);;
let __a12 = (fun _x0_ _x1_ -> (((_p_pos_only 1072) _x0_) (((_p 1071) _x0_) _x1_)));;
let __a4 = (_p 1007);;
let __a1 = (_p 1056);;
let __a48 = (_p 1081);;
let __a41 = (_p 1224);;
let __a16 = (_p 1037);;
let __a39 = (_p 1200);;
let __a26 = (_p 1062);;
let __a37 = (_p 1013);;
let __a17 = (_p 1043);;
let __a57 = (_p 1216);;
let __a35 = (_p_pos_only 1085);;
let __a56 = (_p 1184);;
let __a61 = (_p 1160);;
let __a18 = (_p 1116);;
let __a15 = (_p 1141);;
let __a64 = (_p_pos_only 1264);;
let __a67 = (_p 1208);;
let __a65 = (_p 1016);;
let __a49 = (_p 1122);;
let __a19 = (_p_pos_only 1134);;
let __a30 = (_p 1065);;
let __a38 = (_p 1046);;
let __a25 = (_p 1022);;
let __a44 = (_p_pos_only 1088);;
let __a6 = (fun _x0_ _x1_ -> (((_p_pos_only 1131) _x0_) (((_p 1130) _x0_) _x1_)));;
let __a20 = (_p 1244);;
let __a43 = (_p 1052);;
let __a53 = (_p 1220);;
let __a47 = (_p 1082);;
let __a45 = (_p 1168);;
let __a36 = (_p 1119);;
let __a70 = (_p 1144);;
let __a23 = (_p_pos_only 1075);;
let __a74 = (_p 1125);;
let __a51 = (_p 1174);;
let __a8 = (_p 1068);;
let __a29 = (_p 1019);;
let __a75 = (_p 1150);;
let __a13 = (_p 1101);;
let __a34 = (_p 1236);;
let __a73 = (_p 1212);;
let __a27 = (_p 1098);;
let __a22 = (_p 1049);;
let __a68 = (_p 1180);;
let __a46 = (_p_pos_only 1192);;
let __a32 = (fun _x0_ _x1_ -> (((_p_pos_only 1189) _x0_) (((_p 1188) _x0_) _x1_)));;
let __a3 = (_p_pos_only 1278);;
let __a9 = (_p 1025);;
let __a5 = (_p 1006);;
let __a11 = (_p 1055);;
let __a50 = (_p 1166);;
let __a7 = (_p 1272);;
let __a42 = (_p 1031);;
let __a40 = (_p 1228);;
let __a66 = (_p 1147);;
let __a63 = (_p 1204);;
let __g0 = (_e);;
let __a72 = (_p 1172);;
let __a60 = (_p 1128);;
let __a14 = (_p 1104);;
let __a24 = (_p 1153);;
let __a58 = (_p 1028);;
let __a54 = (fun _x0_ _x1_ -> (((_p_pos_only 1261) _x0_) (((_p 1260) _x0_) _x1_)));;
let __a52 = (_p 1240);;
let __a28 = (_p_pos_only 1252);;
let __a55 = (_p 1110);;
let __a31 = (_p 1164);;
let __binder0 = __default_ret;;
let __binder1 = (_m 1002);;
let __binder2 = (_m 1058);;
let __binder3 = (_m 1008);;
let __binder4 = (_m 1155);;
let binders = [| |]
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
let program = [
(383, [AAction2Instr(__a33,179)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [AAction2Instr(__a34,179)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(116,417)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,418)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(114,419)]);
(4, [AContInstr3(272,__g0,__binder1,87);ACallInstr3(__g0,9)]);
(388, [EatInstr(115,420)]);
(5, [EatInstr(0,15)]);
(389, [EatInstr(117,421)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,43)]);
(390, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,422)]);
(7, [EatInstr(127,45);EatInstr(126,45);EatInstr(125,45);EatInstr(124,45);EatInstr(123,45);EatInstr(122,45);EatInstr(121,45);EatInstr(120,45);EatInstr(119,45);EatInstr(118,45);EatInstr(117,45);EatInstr(116,45);EatInstr(115,45);EatInstr(114,45);EatInstr(113,45);EatInstr(112,45);EatInstr(111,45);EatInstr(110,45);EatInstr(109,45);EatInstr(108,45);EatInstr(107,45);EatInstr(106,45);EatInstr(105,45);EatInstr(104,45);EatInstr(103,45);EatInstr(102,45);EatInstr(101,45);EatInstr(100,45);EatInstr(99,45);EatInstr(98,45);EatInstr(97,45);EatInstr(96,45);EatInstr(95,45);EatInstr(94,45);EatInstr(93,45);EatInstr(92,45);EatInstr(91,45);EatInstr(90,45);EatInstr(89,45);EatInstr(88,45);EatInstr(87,45);EatInstr(86,45);EatInstr(85,45);EatInstr(84,45);EatInstr(83,45);EatInstr(82,45);EatInstr(81,45);EatInstr(80,45);EatInstr(79,45);EatInstr(78,45);EatInstr(77,45);EatInstr(76,45);EatInstr(75,45);EatInstr(74,45);EatInstr(73,45);EatInstr(72,45);EatInstr(71,45);EatInstr(70,45);EatInstr(69,45);EatInstr(68,45);EatInstr(67,45);EatInstr(66,45);EatInstr(65,45);EatInstr(64,45);EatInstr(63,45);EatInstr(62,45);EatInstr(61,45);EatInstr(60,45);EatInstr(59,45);EatInstr(58,45);EatInstr(57,45);EatInstr(56,45);EatInstr(55,45);EatInstr(54,45);EatInstr(53,45);EatInstr(52,45);EatInstr(51,45);EatInstr(50,45);EatInstr(49,45);EatInstr(48,45);EatInstr(47,45);EatInstr(46,45);EatInstr(44,45);EatInstr(43,45);EatInstr(42,45);EatInstr(41,45);EatInstr(40,45);EatInstr(39,45);EatInstr(38,45);EatInstr(37,45);EatInstr(36,45);EatInstr(35,45);EatInstr(34,45);EatInstr(33,45);EatInstr(32,45);EatInstr(31,45);EatInstr(30,45);EatInstr(29,45);EatInstr(28,45);EatInstr(27,45);EatInstr(26,45);EatInstr(25,45);EatInstr(24,45);EatInstr(23,45);EatInstr(22,45);EatInstr(21,45);EatInstr(20,45);EatInstr(19,45);EatInstr(18,45);EatInstr(17,45);EatInstr(16,45);EatInstr(15,45);EatInstr(14,45);EatInstr(13,45);EatInstr(12,45);EatInstr(11,45);EatInstr(10,45);EatInstr(9,45);EatInstr(8,45);EatInstr(7,45);EatInstr(6,45);EatInstr(5,45);EatInstr(4,45);EatInstr(3,45);EatInstr(2,45);EatInstr(1,45)]);
(391, [EatInstr(114,423)]);
(8, [EatInstr(119,26);EatInstr(117,25);EatInstr(115,24);EatInstr(112,23);EatInstr(109,22);EatInstr(108,21);EatInstr(105,20);EatInstr(104,19);EatInstr(100,18);EatInstr(99,17);EatInstr(97,16)]);
(392, [AAction2Instr(__a35,424)]);
(9, [EatInstr(116,36);EatInstr(115,35);EatInstr(114,34);EatInstr(112,33);EatInstr(108,32);EatInstr(105,31);EatInstr(102,30);EatInstr(101,29);EatInstr(100,28);EatInstr(99,27);AAction2Instr(__a1,37)]);
(393, [EatInstr(97,425)]);
(10, [EatInstr(45,38);AAction2Instr(__a2,39)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),40)]);
(394, [EatInstr(101,426)]);
(395, [EatInstr(45,427)]);
(12, [CompleteInstr(264)]);
(396, [AAction2Instr(__a36,74)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(100,428)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(97,429)]);
(15, [CompleteInstr(268)]);
(399, [EatInstr(118,430)]);
(16, [EatInstr(116,48);EatInstr(114,47)]);
(400, [EatInstr(45,431)]);
(17, [EatInstr(111,50);EatInstr(108,49)]);
(401, [EatInstr(100,432)]);
(18, [EatInstr(101,51)]);
(402, [EatInstr(97,433)]);
(19, [EatInstr(97,52)]);
(403, [EatInstr(119,437);EatInstr(116,436);EatInstr(112,435);EatInstr(102,434)]);
(20, [EatInstr(110,53)]);
(404, [EatInstr(110,438)]);
(21, [EatInstr(105,55);EatInstr(101,54)]);
(405, [EatInstr(101,439)]);
(22, [EatInstr(105,56)]);
(406, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,471)]);
(23, [EatInstr(114,57)]);
(407, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,440)]);
(24, [EatInstr(117,58)]);
(408, [EatInstr(103,441)]);
(25, [EatInstr(110,59)]);
(409, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,442)]);
(26, [EatInstr(114,60)]);
(410, [EatInstr(105,443)]);
(27, [EatInstr(111,61)]);
(411, [EatInstr(99,444)]);
(28, [EatInstr(111,63);EatInstr(105,62)]);
(412, [EatInstr(101,445)]);
(29, [EatInstr(120,64)]);
(413, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,446)]);
(30, [EatInstr(117,65)]);
(414, [EatInstr(112,447)]);
(31, [EatInstr(110,66)]);
(415, [EatInstr(111,448)]);
(32, [EatInstr(114,68);EatInstr(111,67)]);
(416, [EatInstr(97,449)]);
(33, [EatInstr(114,69)]);
(417, [EatInstr(105,450)]);
(34, [EatInstr(102,70)]);
(418, [AAction2Instr(__a37,566)]);
(35, [EatInstr(116,72);EatInstr(111,71)]);
(419, [EatInstr(45,451)]);
(36, [EatInstr(114,73)]);
(420, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,452)]);
(37, [AContInstr3(271,__g0,__binder2,74);ACallInstr3(__g0,8)]);
(421, [EatInstr(108,453)]);
(38, [EatInstr(118,85);EatInstr(117,84);EatInstr(114,83);EatInstr(111,82);EatInstr(110,81);EatInstr(109,80);EatInstr(108,79);EatInstr(105,78);EatInstr(99,77);EatInstr(98,76);EatInstr(97,75)]);
(422, [AAction2Instr(__a38,566)]);
(39, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,86)]);
(423, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,454)]);
(40, [CompleteInstr(274)]);
(424, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,455)]);
(41, [AContInstr3(273,__g0,__binder3,87);ACallInstr3(__g0,10)]);
(425, [EatInstr(110,456)]);
(42, [ALookaheadInstr(false,CfgLA (3,266),88);ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,88)]);
(426, [EatInstr(97,457)]);
(43, [ALookaheadInstr(false,CfgLA (1,264),44);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,43)]);
(427, [EatInstr(97,458)]);
(44, [CompleteInstr(269)]);
(428, [EatInstr(115,459)]);
(45, [ALookaheadInstr(false,CfgLA (1,264),46);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,45)]);
(429, [EatInstr(98,460)]);
(46, [CompleteInstr(270)]);
(430, [EatInstr(97,461)]);
(47, [EatInstr(114,89)]);
(431, [EatInstr(97,462)]);
(48, [EatInstr(116,90)]);
(432, [EatInstr(121,463)]);
(49, [EatInstr(111,91)]);
(433, [EatInstr(116,464)]);
(50, [EatInstr(112,92)]);
(434, [EatInstr(117,465)]);
(51, [EatInstr(115,93)]);
(435, [EatInstr(101,466)]);
(52, [EatInstr(115,94)]);
(436, [EatInstr(120,467)]);
(53, [EatInstr(108,96);EatInstr(102,95)]);
(437, [EatInstr(97,468)]);
(54, [EatInstr(120,97)]);
(438, [EatInstr(115,469)]);
(55, [EatInstr(102,98)]);
(439, [EatInstr(108,470)]);
(56, [EatInstr(110,99)]);
(440, [AAction2Instr(__a39,179)]);
(57, [EatInstr(101,100)]);
(441, [EatInstr(117,472)]);
(58, [EatInstr(98,101)]);
(442, [AAction2Instr(__a40,179)]);
(59, [EatInstr(114,102)]);
(443, [EatInstr(115,473)]);
(60, [EatInstr(97,103)]);
(444, [EatInstr(101,474)]);
(61, [EatInstr(109,104)]);
(445, [EatInstr(45,475)]);
(62, [EatInstr(115,105)]);
(446, [AAction2Instr(__a41,179)]);
(63, [EatInstr(116,106)]);
(447, [EatInstr(116,476)]);
(64, [EatInstr(116,108);EatInstr(101,107)]);
(448, [EatInstr(114,477)]);
(65, [EatInstr(115,109)]);
(449, [EatInstr(114,478)]);
(66, [EatInstr(102,110)]);
(450, [EatInstr(111,479)]);
(67, [EatInstr(111,111)]);
(451, [EatInstr(99,480)]);
(68, [EatInstr(49,112)]);
(452, [AAction2Instr(__a42,566)]);
(69, [EatInstr(105,114);EatInstr(101,113)]);
(453, [EatInstr(97,481)]);
(70, [EatInstr(99,115)]);
(454, [AAction2Instr(__a43,566)]);
(71, [EatInstr(114,116)]);
(455, [AAction2Instr(__a44,482)]);
(72, [EatInstr(114,117)]);
(456, [EatInstr(97,483)]);
(73, [EatInstr(97,118)]);
(457, [EatInstr(100,484)]);
(74, [CompleteInstr(272)]);
(458, [EatInstr(110,485)]);
(75, [EatInstr(114,120);EatInstr(102,119)]);
(459, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,486)]);
(76, [EatInstr(97,121)]);
(460, [EatInstr(108,487)]);
(77, [EatInstr(111,124);EatInstr(104,123);EatInstr(97,122)]);
(461, [EatInstr(110,488)]);
(78, [EatInstr(110,125)]);
(462, [EatInstr(99,489)]);
(79, [EatInstr(111,126)]);
(463, [EatInstr(112,490)]);
(80, [EatInstr(101,127)]);
(464, [EatInstr(105,491)]);
(81, [EatInstr(111,128)]);
(465, [EatInstr(110,492)]);
(82, [EatInstr(110,129)]);
(466, [EatInstr(103,493)]);
(83, [EatInstr(111,130)]);
(467, [AAction2Instr(__a45,380)]);
(84, [EatInstr(115,132);EatInstr(110,131)]);
(468, [EatInstr(100,494)]);
(85, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,133)]);
(469, [EatInstr(105,495)]);
(86, [AAction2Instr(__a3,380)]);
(470, [EatInstr(115,496)]);
(87, [AAction2Instr(__a5,42);AAction2Instr(__a4,41)]);
(471, [AAction2Instr(__a46,380);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,471)]);
(88, [CompleteInstr(267)]);
(472, [EatInstr(108,497)]);
(89, [EatInstr(111,134)]);
(473, [EatInstr(116,498)]);
(90, [EatInstr(114,135)]);
(474, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,499)]);
(91, [EatInstr(115,136)]);
(475, [EatInstr(104,500)]);
(92, [EatInstr(121,137)]);
(476, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,501)]);
(93, [EatInstr(117,138)]);
(477, [EatInstr(121,502)]);
(94, [EatInstr(104,139)]);
(478, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,503)]);
(95, [EatInstr(101,140)]);
(479, [EatInstr(110,504)]);
(96, [EatInstr(105,141)]);
(480, [EatInstr(111,505)]);
(97, [EatInstr(101,142)]);
(481, [EatInstr(114,506)]);
(98, [EatInstr(116,143)]);
(482, [AAction2Instr(__a48,357);AAction2Instr(__a47,356)]);
(99, [EatInstr(117,144)]);
(483, [EatInstr(108,507)]);
(100, [EatInstr(99,145)]);
(484, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(101, [EatInstr(115,146)]);
(485, [EatInstr(97,509)]);
(102, [EatInstr(111,147)]);
(486, [AAction2Instr(__a49,74)]);
(103, [EatInstr(112,148)]);
(487, [EatInstr(101,510)]);
(104, [EatInstr(112,149)]);
(488, [EatInstr(99,511)]);
(105, [EatInstr(112,150)]);
(489, [EatInstr(116,512)]);
(106, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,151)]);
(490, [EatInstr(103,513)]);
(107, [EatInstr(99,152)]);
(491, [EatInstr(111,514)]);
(108, [EatInstr(114,153)]);
(492, [AAction2Instr(__a50,380)]);
(109, [EatInstr(101,154)]);
(493, [EatInstr(45,515);AAction2Instr(__a51,380)]);
(110, [EatInstr(111,155)]);
(494, [EatInstr(108,516)]);
(111, [EatInstr(107,156)]);
(495, [EatInstr(116,517)]);
(112, [EatInstr(45,157)]);
(496, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,518)]);
(113, [EatInstr(99,158)]);
(497, [EatInstr(97,519)]);
(114, [EatInstr(110,159)]);
(498, [EatInstr(111,520)]);
(115, [AAction2Instr(__a6,160)]);
(499, [AAction2Instr(__a52,179)]);
(116, [EatInstr(116,161)]);
(500, [EatInstr(105,521)]);
(117, [EatInstr(105,162)]);
(501, [AAction2Instr(__a53,179)]);
(118, [EatInstr(110,163)]);
(502, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,522)]);
(119, [EatInstr(116,164)]);
(503, [AAction2Instr(__a54,523)]);
(120, [EatInstr(114,165)]);
(504, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,524)]);
(121, [EatInstr(99,166)]);
(505, [EatInstr(114,525)]);
(122, [EatInstr(115,167)]);
(506, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,526)]);
(123, [EatInstr(101,168)]);
(507, [EatInstr(121,527)]);
(124, [EatInstr(117,169)]);
(508, [AAction2Instr(__a55,74)]);
(125, [EatInstr(108,170)]);
(509, [EatInstr(108,528)]);
(126, [EatInstr(111,171)]);
(510, [EatInstr(45,529)]);
(127, [EatInstr(109,172)]);
(511, [EatInstr(101,530)]);
(128, [EatInstr(45,173)]);
(512, [EatInstr(105,531)]);
(129, [EatInstr(108,174)]);
(513, [EatInstr(101,532)]);
(130, [EatInstr(111,175)]);
(514, [EatInstr(110,533)]);
(131, [EatInstr(114,177);EatInstr(105,176)]);
(515, [EatInstr(115,534)]);
(132, [EatInstr(101,178)]);
(516, [EatInstr(101,535)]);
(133, [AAction2Instr(__a7,179)]);
(517, [EatInstr(105,536)]);
(134, [EatInstr(119,180)]);
(518, [AAction2Instr(__a56,179)]);
(135, [EatInstr(105,181)]);
(519, [EatInstr(114,537)]);
(136, [EatInstr(101,182)]);
(520, [EatInstr(114,538)]);
(137, [EatInstr(114,183)]);
(521, [EatInstr(115,539)]);
(138, [EatInstr(103,184)]);
(522, [AAction2Instr(__a57,179)]);
(139, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,185)]);
(523, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,554)]);
(140, [EatInstr(114,186)]);
(524, [AAction2Instr(__a58,566)]);
(141, [EatInstr(110,187)]);
(525, [EatInstr(101,540)]);
(142, [EatInstr(114,188)]);
(526, [AAction2Instr(__a59,566)]);
(143, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,189)]);
(527, [EatInstr(115,541)]);
(144, [EatInstr(115,190)]);
(528, [EatInstr(121,542)]);
(145, [EatInstr(101,191)]);
(529, [EatInstr(112,543)]);
(146, [EatInstr(101,192)]);
(530, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,544)]);
(147, [EatInstr(108,193)]);
(531, [EatInstr(111,545)]);
(148, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,194)]);
(532, [EatInstr(110,546)]);
(149, [EatInstr(105,195)]);
(533, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,547)]);
(150, [EatInstr(97,196)]);
(534, [EatInstr(116,548)]);
(151, [AAction2Instr(__a8,74)]);
(535, [EatInstr(114,549)]);
(152, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,197)]);
(536, [EatInstr(118,550)]);
(153, [EatInstr(97,198)]);
(537, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,551)]);
(154, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,199)]);
(538, [EatInstr(121,552)]);
(155, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,200)]);
(539, [EatInstr(116,553)]);
(156, [EatInstr(97,201)]);
(540, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,555)]);
(157, [EatInstr(108,202)]);
(541, [EatInstr(105,556)]);
(158, [EatInstr(101,203)]);
(542, [EatInstr(115,557)]);
(159, [EatInstr(116,204)]);
(543, [EatInstr(114,558)]);
(160, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,247)]);
(544, [AAction2Instr(__a60,74)]);
(161, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,205)]);
(545, [EatInstr(110,559)]);
(162, [EatInstr(112,206)]);
(546, [EatInstr(45,561);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,560)]);
(163, [EatInstr(115,207)]);
(547, [AAction2Instr(__a61,179)]);
(164, [EatInstr(101,208)]);
(548, [EatInstr(114,562)]);
(165, [EatInstr(111,209)]);
(549, [AAction2Instr(__a62,380)]);
(166, [EatInstr(107,210)]);
(550, [EatInstr(101,563)]);
(167, [EatInstr(101,211)]);
(551, [AAction2Instr(__a63,179)]);
(168, [EatInstr(99,212)]);
(552, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,564)]);
(169, [EatInstr(110,213)]);
(553, [EatInstr(111,565)]);
(170, [EatInstr(105,214)]);
(554, [AAction2Instr(__a64,380);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,554)]);
(171, [EatInstr(107,215)]);
(555, [AAction2Instr(__a65,566)]);
(172, [EatInstr(111,216)]);
(556, [EatInstr(115,567)]);
(173, [EatInstr(115,220);EatInstr(114,219);EatInstr(109,218);EatInstr(99,217)]);
(557, [EatInstr(105,568)]);
(174, [EatInstr(121,221)]);
(558, [EatInstr(101,569)]);
(175, [EatInstr(116,222)]);
(559, [EatInstr(115,570)]);
(176, [EatInstr(116,223)]);
(560, [AAction2Instr(__a66,74)]);
(177, [EatInstr(111,224)]);
(561, [EatInstr(115,571)]);
(178, [EatInstr(45,225)]);
(562, [EatInstr(105,572)]);
(179, [CompleteInstr(273)]);
(563, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,573)]);
(180, [EatInstr(45,226)]);
(564, [AAction2Instr(__a67,179)]);
(181, [EatInstr(98,227)]);
(565, [EatInstr(114,574)]);
(182, [EatInstr(45,228)]);
(566, [CompleteInstr(271)]);
(183, [EatInstr(117,229)]);
(567, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,575)]);
(184, [EatInstr(97,230)]);
(568, [EatInstr(115,576)]);
(185, [AAction2Instr(__a9,566)]);
(569, [EatInstr(100,577)]);
(186, [EatInstr(45,231)]);
(570, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,578)]);
(187, [EatInstr(101,232)]);
(571, [EatInstr(99,579)]);
(188, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,233)]);
(572, [EatInstr(99,580)]);
(189, [AAction2Instr(__a10,566)]);
(573, [AAction2Instr(__a68,179)]);
(190, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,234)]);
(574, [EatInstr(121,581)]);
(191, [EatInstr(100,235)]);
(575, [AAction2Instr(__a69,74)]);
(192, [EatInstr(116,236)]);
(576, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,582)]);
(193, [EatInstr(108,237)]);
(577, [EatInstr(105,583)]);
(194, [AAction2Instr(__a11,566)]);
(578, [AAction2Instr(__a70,74)]);
(195, [EatInstr(108,238)]);
(579, [EatInstr(97,584)]);
(196, [EatInstr(116,239)]);
(580, [EatInstr(116,585)]);
(197, [AAction2Instr(__a12,240)]);
(581, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,586)]);
(198, [EatInstr(99,241)]);
(582, [AAction2Instr(__a71,74)]);
(199, [AAction2Instr(__a13,74)]);
(583, [EatInstr(99,587)]);
(200, [AAction2Instr(__a14,74)]);
(584, [EatInstr(110,588)]);
(201, [EatInstr(104,242)]);
(585, [AAction2Instr(__a72,380)]);
(202, [EatInstr(111,243)]);
(586, [AAction2Instr(__a73,179)]);
(203, [EatInstr(100,244)]);
(587, [EatInstr(97,589)]);
(204, [EatInstr(45,246);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,245)]);
(588, [EatInstr(110,590)]);
(205, [AAction2Instr(__a15,74)]);
(589, [EatInstr(116,591)]);
(206, [EatInstr(45,248)]);
(590, [EatInstr(101,592)]);
(207, [EatInstr(108,249)]);
(591, [EatInstr(101,593)]);
(208, [EatInstr(114,250)]);
(592, [EatInstr(114,594)]);
(209, [EatInstr(119,251)]);
(593, [EatInstr(115,595)]);
(210, [EatInstr(101,252)]);
(594, [EatInstr(108,596)]);
(211, [EatInstr(45,253)]);
(595, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,597)]);
(212, [EatInstr(107,254)]);
(596, [EatInstr(101,598)]);
(213, [EatInstr(116,255)]);
(597, [AAction2Instr(__a74,74)]);
(214, [EatInstr(110,256)]);
(598, [EatInstr(115,599)]);
(215, [EatInstr(97,257)]);
(599, [EatInstr(115,600)]);
(216, [EatInstr(105,258)]);
(600, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,601)]);
(217, [EatInstr(111,259)]);
(601, [AAction2Instr(__a75,74)]);
(218, [EatInstr(101,260)]);
(219, [EatInstr(101,261)]);
(220, [EatInstr(107,262)]);
(221, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,263)]);
(222, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,264)]);
(223, [EatInstr(45,265)]);
(224, [EatInstr(108,266)]);
(225, [EatInstr(102,267)]);
(226, [EatInstr(110,268)]);
(227, [EatInstr(117,269)]);
(228, [EatInstr(117,270)]);
(229, [EatInstr(108,271)]);
(230, [EatInstr(114,272)]);
(231, [EatInstr(116,274)]);
(232, [EatInstr(45,275)]);
(233, [AAction2Instr(__a16,566)]);
(234, [AAction2Instr(__a17,566)]);
(235, [EatInstr(101,276)]);
(236, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,277)]);
(237, [EatInstr(45,278)]);
(238, [EatInstr(101,279)]);
(239, [EatInstr(99,280)]);
(240, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,281)]);
(241, [EatInstr(116,282)]);
(242, [EatInstr(101,283)]);
(243, [EatInstr(111,284)]);
(244, [EatInstr(101,285)]);
(245, [AAction2Instr(__a18,74)]);
(246, [EatInstr(114,288);EatInstr(110,287);EatInstr(103,286)]);
(247, [AAction2Instr(__a19,357);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,247)]);
(248, [EatInstr(108,289)]);
(249, [EatInstr(97,290)]);
(250, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,291)]);
(251, [EatInstr(45,292)]);
(252, [EatInstr(110,293)]);
(253, [EatInstr(105,294)]);
(254, [EatInstr(45,295)]);
(255, [EatInstr(101,296)]);
(256, [EatInstr(101,297)]);
(257, [EatInstr(104,298)]);
(258, [EatInstr(122,299)]);
(259, [EatInstr(97,300)]);
(260, [EatInstr(109,301)]);
(261, [EatInstr(112,302)]);
(262, [EatInstr(105,303)]);
(263, [AAction2Instr(__a20,179)]);
(264, [AAction2Instr(__a21,304)]);
(265, [EatInstr(104,305)]);
(266, [EatInstr(108,306)]);
(267, [EatInstr(115,307)]);
(268, [EatInstr(111,308)]);
(269, [EatInstr(116,309)]);
(270, [EatInstr(110,310)]);
(271, [EatInstr(101,311)]);
(272, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,312)]);
(274, [EatInstr(121,313)]);
(275, [EatInstr(114,314)]);
(276, [EatInstr(110,315)]);
(277, [AAction2Instr(__a22,566)]);
(278, [EatInstr(115,316)]);
(279, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,317)]);
(280, [EatInstr(104,318)]);
(281, [AAction2Instr(__a23,482)]);
(282, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,319)]);
(283, [EatInstr(97,320)]);
(284, [EatInstr(107,321)]);
(285, [EatInstr(110,322)]);
(286, [EatInstr(105,323)]);
(287, [EatInstr(117,325);EatInstr(112,324)]);
(288, [EatInstr(101,326)]);
(289, [EatInstr(97,327)]);
(290, [EatInstr(116,328)]);
(291, [AAction2Instr(__a24,329)]);
(292, [EatInstr(110,330)]);
(293, [EatInstr(100,331)]);
(294, [EatInstr(110,332)]);
(295, [EatInstr(108,333)]);
(296, [EatInstr(114,334)]);
(297, [EatInstr(45,335)]);
(298, [EatInstr(101,336)]);
(299, [EatInstr(101,337)]);
(300, [EatInstr(108,338)]);
(301, [EatInstr(111,339)]);
(302, [EatInstr(108,340)]);
(303, [EatInstr(112,341)]);
(304, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,342)]);
(305, [EatInstr(105,343)]);
(306, [EatInstr(45,344)]);
(307, [EatInstr(116,346);EatInstr(109,345)]);
(308, [EatInstr(116,347)]);
(309, [EatInstr(101,348)]);
(310, [EatInstr(100,349)]);
(311, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,350)]);
(312, [AAction2Instr(__a25,566)]);
(313, [EatInstr(112,351)]);
(314, [EatInstr(101,352)]);
(315, [EatInstr(99,353)]);
(316, [EatInstr(116,354)]);
(317, [AAction2Instr(__a26,74)]);
(318, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,355)]);
(319, [AAction2Instr(__a27,74)]);
(320, [EatInstr(100,358)]);
(321, [EatInstr(97,359)]);
(322, [EatInstr(99,360)]);
(323, [EatInstr(108,361)]);
(324, [EatInstr(114,362)]);
(325, [EatInstr(108,363)]);
(326, [EatInstr(108,364)]);
(327, [EatInstr(116,365)]);
(328, [EatInstr(101,366)]);
(329, [AContInstr3(271,__g0,__binder4,179);ACallInstr3(__g0,8)]);
(330, [EatInstr(111,367)]);
(331, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,368)]);
(332, [EatInstr(115,369)]);
(333, [EatInstr(97,370)]);
(334, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,371)]);
(335, [EatInstr(114,373);EatInstr(99,372)]);
(336, [EatInstr(97,374)]);
(337, [EatInstr(45,375)]);
(338, [EatInstr(101,376)]);
(339, [EatInstr(105,377)]);
(340, [EatInstr(97,378)]);
(341, [EatInstr(45,379)]);
(342, [AAction2Instr(__a28,380)]);
(343, [EatInstr(115,381)]);
(344, [EatInstr(115,382)]);
(345, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,383)]);
(346, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,384)]);
(347, [EatInstr(97,385)]);
(348, [EatInstr(115,386)]);
(349, [EatInstr(101,387)]);
(350, [AAction2Instr(__a29,566)]);
(351, [EatInstr(101,388)]);
(352, [EatInstr(103,389)]);
(353, [EatInstr(101,390)]);
(354, [EatInstr(97,391)]);
(355, [AAction2Instr(__a30,74)]);
(356, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,392)]);
(357, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,74)]);
(358, [EatInstr(45,393)]);
(359, [EatInstr(104,394)]);
(360, [EatInstr(101,395)]);
(361, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,396)]);
(362, [EatInstr(101,397)]);
(363, [EatInstr(108,398)]);
(364, [EatInstr(101,399)]);
(365, [EatInstr(101,400)]);
(366, [EatInstr(45,401)]);
(367, [EatInstr(116,402)]);
(368, [AAction2Instr(__a31,403)]);
(369, [EatInstr(101,404)]);
(370, [EatInstr(98,405)]);
(371, [AAction2Instr(__a32,406)]);
(372, [EatInstr(115,407)]);
(373, [EatInstr(101,408)]);
(374, [EatInstr(100,409)]);
(375, [EatInstr(104,410)]);
(376, [EatInstr(115,411)]);
(377, [EatInstr(122,412)]);
(378, [EatInstr(121,413)]);
(379, [EatInstr(111,414)]);
(380, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,179)]);
(381, [EatInstr(116,415)]);
(382, [EatInstr(116,416)]);
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
