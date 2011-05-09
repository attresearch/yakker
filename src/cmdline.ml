
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
 | (1028) -> ( Infer_ty_cmd )
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
_r_command(_n,_ps,ykinput) = 
 (match _n() with
 | (1053) -> (
 (let p = (_r_phases(_n,_ps,ykinput))
 in ( (match p with
                                               Inline_regular_cmd -> Compileopt.inline_regular := true
                                             | Unroll_star_cmd -> if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1
                                             | _ -> ());
                                            p )
))
 | (1059) -> ( Compile_cmd )
 | (1062) -> ( Dispatch_cmd )
 | (1065) -> ( Dot_cmd )
 | (1068) -> (
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
 | (1078) -> (_x8)
 | _(*1079*) -> (_x24(
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
 | (1095) -> ( Extract_cmd )
 | (1098) -> ( Compileopt.coalesce := true; Fuse_cmd )
 | (1101) -> ( Info_cmd )
 | (1104) -> ( Lookahead_analysis_cmd )
 | (1107) -> ( Lr1_lookahead_cmd )
 | (1110) -> ( Precedence_analysis_cmd )
 | (1113) -> ( Print_gul_cmd )
 | (1116) -> ( Print_gil_cmd )
 | (1119) -> ( Print_npreds_cmd )
 | (1122) -> ( Print_npreds_cmd )
 | (1125) -> ( Print_relevance_cmd )
 | (1127) -> (
 (let _x10 = (_ps())
 in (
 (let _x9 = (_ps())
 in (
 (let n = (Yak.YkBuf.get_string _x10 _x9 ykinput)
 in ( try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" )
))
))
))
 | (1138) -> ( Sort_cmd )
 | (1141) -> ( Strip_late_actions_cmd )
 | (1144) -> ( Translate_dypgen_cmd )
 | _(*1147*) -> ( Translate_dypgen_scannerless_cmd )
 )
 and
_r_args(_n,_ps,ykinput) = 
 (match _n() with
 | (1150) -> (
 (let p = (_r_phases(_n,_ps,ykinput))
 in (
 (let _x45 = ( after := Some p )
 in ()))
))
 | (1157) -> (
 (let b = 
 (match _n() with
 | (1159) -> (Fun_BE)
 | (1161) -> (Trans_BE)
 | (1163) -> (Wadler_BE)
 | (1165) -> (Peg_BE false)
 | _(*1167*) -> (Peg_BE true)
 ) in (
 (let _x44 = ( backend := b )
 in ()))
))
 | (1173) -> (
 (let _x43 = ( Compileopt.case_sensitive := false )
 in ()))
 | (1177) -> (
 (let _x42 = ( Compileopt.check_labels := true )
 in ()))
 | (1181) -> (
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
 | (1193) -> (
 (let _x40 = ( Compileopt.inline_cs := true )
 in ()))
 | (1197) -> (
 (let _x39 = ( Compileopt.inline_regular := true )
 in ()))
 | (1201) -> (
 (let _x38 = ( Compileopt.memoize_history := true )
 in ()))
 | (1205) -> (
 (let _x37 = ( Compileopt.memoize_history := false )
 in ()))
 | (1209) -> (
 (let _x36 = ( Compileopt.unit_history := true )
 in ()))
 | (1213) -> (
 (let _x35 = ( Compileopt.skip_opt := false )
 in ()))
 | (1217) -> (
 (let _x34 = ( Compileopt.repress_replay := true )
 in ()))
 | (1221) -> (
 (let _x33 = ( Compileopt.lookahead := true )
 in ()))
 | (1225) -> (
 (let _x32 = ( Compileopt.use_fsm := true )
 in ()))
 | (1229) -> (
 (let _x31 = ( Compileopt.use_fsm := false )
 in ()))
 | (1233) -> (
 (let _x30 = ( Compileopt.coalesce := false )
 in ()))
 | (1237) -> (
 (let _x29 = ( only := true )
 in ()))
 | (1241) -> (
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
 | (1253) -> (
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
 | (1265) -> (
 (let _x26 = ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 in ()))
 | _(*1267*) -> (
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
let __a17 = (_p 1034);;
let __a34 = (fun _x0_ _x1_ -> (((_p_pos_only 1182) _x0_) (((_p 1181) _x0_) _x1_)));;
let __a18 = (_p 1040);;
let __a20 = (_p 1237);;
let __a55 = (_p 1107);;
let __a53 = (_p 1213);;
let __a37 = (_p_pos_only 1082);;
let __a3 = (_p 1007);;
let __a19 = (_p 1113);;
let __a51 = (_p 1167);;
let __a54 = (fun _x0_ _x1_ -> (((_p_pos_only 1254) _x0_) (((_p 1253) _x0_) _x1_)));;
let __a10 = (_p 1037);;
let __a36 = (_p 1229);;
let __a71 = (_p 1205);;
let __a62 = (_p 1197);;
let __a15 = (_p_pos_only 1131);;
let __a32 = (_p 1062);;
let __a39 = (_p 1013);;
let __a66 = (_p 1173);;
let __a41 = (_p_pos_only 1185);;
let __a40 = (_p 1043);;
let __a50 = (_p 1159);;
let __a47 = (_p_pos_only 1085);;
let __a7 = (_p 1265);;
let __a2 = (fun _x0_ _x1_ -> (((_p_pos_only 1268) _x0_) (((_p 1267) _x0_) _x1_)));;
let __a28 = (_p 1078);;
let __a70 = (_p 1165);;
let __a26 = (_p 1059);;
let __a38 = (_p 1116);;
let __a68 = (_p 1141);;
let __a23 = (_p_pos_only 1072);;
let __a72 = (_p 1122);;
let __a63 = (_p 1016);;
let __a8 = (_p 1065);;
let __a12 = (fun _x0_ _x1_ -> (((_p_pos_only 1069) _x0_) (((_p 1068) _x0_) _x1_)));;
let __a21 = (fun _x0_ _x1_ -> (((_p_pos_only 1242) _x0_) (((_p 1241) _x0_) _x1_)));;
let __a52 = (_p 1233);;
let __a30 = (_p_pos_only 1245);;
let __a22 = (_p 1046);;
let __a29 = (_p 1095);;
let __a25 = (_p 1022);;
let __a33 = (_p 1157);;
let __a16 = (_p 1138);;
let __a11 = (_p 1052);;
let __a61 = (_p 1163);;
let __a49 = (_p 1119);;
let __a35 = (_p 1225);;
let __a65 = (_p 1201);;
let __a64 = (_p 1144);;
let __a42 = (_p 1193);;
let __a60 = (_p 1125);;
let __a31 = (_p 1019);;
let __a14 = (_p 1101);;
let __a24 = (_p 1150);;
let __a13 = (_p 1098);;
let __a46 = (_p 1049);;
let __a44 = (_p 1217);;
let __a9 = (_p 1025);;
let __a27 = (_p 1079);;
let __a48 = (_p 1161);;
let __a4 = (_p 1006);;
let __a58 = (_p 1031);;
let __a6 = (fun _x0_ _x1_ -> (((_p_pos_only 1128) _x0_) (((_p 1127) _x0_) _x1_)));;
let __a73 = (_p 1147);;
let __g0 = (_e);;
let __a57 = (_p 1209);;
let __a56 = (_p 1177);;
let __a67 = (_p 1104);;
let __a5 = (_p_pos_only 1271);;
let __a45 = (_p 1028);;
let __a69 = (_p 1110);;
let __a59 = (_p_pos_only 1257);;
let __a1 = (_p 1053);;
let __a43 = (_p 1221);;
let __binder0 = __default_ret;;
let __binder1 = (_m 1002);;
let __binder2 = (_m 1055);;
let __binder3 = (_m 1008);;
let __binder4 = (_m 1152);;
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
(383, [EatInstr(100,412)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(97,413)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(118,414)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(45,415)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(100,416)]);
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [EatInstr(119,420);EatInstr(116,419);EatInstr(112,418);EatInstr(102,417)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(110,421)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,17)]);
(390, [EatInstr(101,422)]);
(7, [EatInstr(127,18);EatInstr(126,18);EatInstr(125,18);EatInstr(124,18);EatInstr(123,18);EatInstr(122,18);EatInstr(121,18);EatInstr(120,18);EatInstr(119,18);EatInstr(118,18);EatInstr(117,18);EatInstr(116,18);EatInstr(115,18);EatInstr(114,18);EatInstr(113,18);EatInstr(112,18);EatInstr(111,18);EatInstr(110,18);EatInstr(109,18);EatInstr(108,18);EatInstr(107,18);EatInstr(106,18);EatInstr(105,18);EatInstr(104,18);EatInstr(103,18);EatInstr(102,18);EatInstr(101,18);EatInstr(100,18);EatInstr(99,18);EatInstr(98,18);EatInstr(97,18);EatInstr(96,18);EatInstr(95,18);EatInstr(94,18);EatInstr(93,18);EatInstr(92,18);EatInstr(91,18);EatInstr(90,18);EatInstr(89,18);EatInstr(88,18);EatInstr(87,18);EatInstr(86,18);EatInstr(85,18);EatInstr(84,18);EatInstr(83,18);EatInstr(82,18);EatInstr(81,18);EatInstr(80,18);EatInstr(79,18);EatInstr(78,18);EatInstr(77,18);EatInstr(76,18);EatInstr(75,18);EatInstr(74,18);EatInstr(73,18);EatInstr(72,18);EatInstr(71,18);EatInstr(70,18);EatInstr(69,18);EatInstr(68,18);EatInstr(67,18);EatInstr(66,18);EatInstr(65,18);EatInstr(64,18);EatInstr(63,18);EatInstr(62,18);EatInstr(61,18);EatInstr(60,18);EatInstr(59,18);EatInstr(58,18);EatInstr(57,18);EatInstr(56,18);EatInstr(55,18);EatInstr(54,18);EatInstr(53,18);EatInstr(52,18);EatInstr(51,18);EatInstr(50,18);EatInstr(49,18);EatInstr(48,18);EatInstr(47,18);EatInstr(46,18);EatInstr(44,18);EatInstr(43,18);EatInstr(42,18);EatInstr(41,18);EatInstr(40,18);EatInstr(39,18);EatInstr(38,18);EatInstr(37,18);EatInstr(36,18);EatInstr(35,18);EatInstr(34,18);EatInstr(33,18);EatInstr(32,18);EatInstr(31,18);EatInstr(30,18);EatInstr(29,18);EatInstr(28,18);EatInstr(27,18);EatInstr(26,18);EatInstr(25,18);EatInstr(24,18);EatInstr(23,18);EatInstr(22,18);EatInstr(21,18);EatInstr(20,18);EatInstr(19,18);EatInstr(18,18);EatInstr(17,18);EatInstr(16,18);EatInstr(15,18);EatInstr(14,18);EatInstr(13,18);EatInstr(12,18);EatInstr(11,18);EatInstr(10,18);EatInstr(9,18);EatInstr(8,18);EatInstr(7,18);EatInstr(6,18);EatInstr(5,18);EatInstr(4,18);EatInstr(3,18);EatInstr(2,18);EatInstr(1,18)]);
(391, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,423)]);
(8, [EatInstr(119,29);EatInstr(117,28);EatInstr(115,27);EatInstr(112,26);EatInstr(109,25);EatInstr(108,24);EatInstr(105,23);EatInstr(104,22);EatInstr(100,21);EatInstr(99,20);EatInstr(97,19)]);
(392, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,424)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(102,33);EatInstr(101,32);EatInstr(100,31);EatInstr(99,30);AAction2Instr(__a1,40)]);
(393, [EatInstr(103,425)]);
(10, [EatInstr(45,41);AAction2Instr(__a2,42)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43)]);
(394, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,426)]);
(395, [EatInstr(105,427)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(99,428)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(101,429)]);
(14, [CompleteInstr(266)]);
(398, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,430)]);
(15, [AAction2Instr(__a4,46);AAction2Instr(__a3,45)]);
(399, [EatInstr(112,431)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(111,432)]);
(17, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,17)]);
(401, [EatInstr(97,433)]);
(18, [ALookaheadInstr(false,CfgLA (1,264),48);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,18)]);
(402, [AAction2Instr(__a39,226)]);
(19, [EatInstr(116,49)]);
(403, [EatInstr(45,434)]);
(20, [EatInstr(111,51);EatInstr(108,50)]);
(404, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,435)]);
(21, [EatInstr(101,52)]);
(405, [EatInstr(108,436)]);
(22, [EatInstr(97,53)]);
(406, [AAction2Instr(__a40,226)]);
(23, [EatInstr(110,54)]);
(407, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,437)]);
(24, [EatInstr(105,56);EatInstr(101,55)]);
(408, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,438)]);
(25, [EatInstr(105,57)]);
(409, [EatInstr(110,439)]);
(26, [EatInstr(114,58)]);
(410, [EatInstr(97,440)]);
(27, [EatInstr(117,59)]);
(411, [EatInstr(97,441)]);
(28, [EatInstr(110,60)]);
(412, [EatInstr(115,442)]);
(29, [EatInstr(114,61)]);
(413, [EatInstr(98,443)]);
(30, [EatInstr(111,62)]);
(414, [EatInstr(97,444)]);
(31, [EatInstr(111,64);EatInstr(105,63)]);
(415, [EatInstr(97,445)]);
(32, [EatInstr(120,65)]);
(416, [EatInstr(121,446)]);
(33, [EatInstr(117,66)]);
(417, [EatInstr(117,447)]);
(34, [EatInstr(110,67)]);
(418, [EatInstr(101,448)]);
(35, [EatInstr(114,69);EatInstr(111,68)]);
(419, [EatInstr(120,449)]);
(36, [EatInstr(114,70)]);
(420, [EatInstr(97,450)]);
(37, [EatInstr(102,71)]);
(421, [EatInstr(115,451)]);
(38, [EatInstr(116,73);EatInstr(111,72)]);
(422, [EatInstr(108,452)]);
(39, [EatInstr(114,74)]);
(423, [AAction2Instr(__a41,132);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,423)]);
(40, [AContInstr3(271,__g0,__binder2,75);ACallInstr3(__g0,8)]);
(424, [AAction2Instr(__a42,176)]);
(41, [EatInstr(118,86);EatInstr(117,85);EatInstr(114,84);EatInstr(111,83);EatInstr(110,82);EatInstr(109,81);EatInstr(108,80);EatInstr(105,79);EatInstr(99,78);EatInstr(98,77);EatInstr(97,76)]);
(425, [EatInstr(117,453)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,87)]);
(426, [AAction2Instr(__a43,176)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(115,454)]);
(428, [EatInstr(101,455)]);
(45, [AContInstr3(273,__g0,__binder3,15);ACallInstr3(__g0,10)]);
(429, [EatInstr(45,456)]);
(46, [ALookaheadInstr(false,CfgLA (3,266),88);ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,88)]);
(430, [AAction2Instr(__a44,176)]);
(47, [CompleteInstr(269)]);
(431, [EatInstr(116,457)]);
(48, [CompleteInstr(270)]);
(432, [EatInstr(114,458)]);
(49, [EatInstr(116,89)]);
(433, [EatInstr(114,459)]);
(50, [EatInstr(111,90)]);
(434, [EatInstr(99,460)]);
(51, [EatInstr(112,91)]);
(435, [AAction2Instr(__a45,226)]);
(52, [EatInstr(115,92)]);
(436, [EatInstr(97,461)]);
(53, [EatInstr(115,93)]);
(437, [AAction2Instr(__a46,226)]);
(54, [EatInstr(108,95);EatInstr(102,94)]);
(438, [AAction2Instr(__a47,310)]);
(55, [EatInstr(120,96)]);
(439, [EatInstr(97,462)]);
(56, [EatInstr(102,97)]);
(440, [EatInstr(100,463)]);
(57, [EatInstr(110,98)]);
(441, [EatInstr(110,464)]);
(58, [EatInstr(101,99)]);
(442, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,465)]);
(59, [EatInstr(98,100)]);
(443, [EatInstr(108,466)]);
(60, [EatInstr(114,101)]);
(444, [EatInstr(110,467)]);
(61, [EatInstr(97,102)]);
(445, [EatInstr(99,468)]);
(62, [EatInstr(109,103)]);
(446, [EatInstr(112,469)]);
(63, [EatInstr(115,104)]);
(447, [EatInstr(110,470)]);
(64, [EatInstr(116,105)]);
(448, [EatInstr(103,471)]);
(65, [EatInstr(116,107);EatInstr(101,106)]);
(449, [AAction2Instr(__a48,132)]);
(66, [EatInstr(115,108)]);
(450, [EatInstr(100,472)]);
(67, [EatInstr(102,109)]);
(451, [EatInstr(105,473)]);
(68, [EatInstr(111,110)]);
(452, [EatInstr(115,474)]);
(69, [EatInstr(49,111)]);
(453, [EatInstr(108,475)]);
(70, [EatInstr(105,113);EatInstr(101,112)]);
(454, [EatInstr(116,476)]);
(71, [EatInstr(99,114)]);
(455, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,477)]);
(72, [EatInstr(114,115)]);
(456, [EatInstr(104,478)]);
(73, [EatInstr(114,116)]);
(457, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,479)]);
(74, [EatInstr(97,117)]);
(458, [EatInstr(121,480)]);
(75, [CompleteInstr(272)]);
(459, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,481)]);
(76, [EatInstr(102,118)]);
(460, [EatInstr(111,482)]);
(77, [EatInstr(97,119)]);
(461, [EatInstr(114,483)]);
(78, [EatInstr(111,122);EatInstr(104,121);EatInstr(97,120)]);
(462, [EatInstr(108,484)]);
(79, [EatInstr(110,123)]);
(463, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,485)]);
(80, [EatInstr(111,124)]);
(464, [EatInstr(97,486)]);
(81, [EatInstr(101,125)]);
(465, [AAction2Instr(__a49,75)]);
(82, [EatInstr(111,126)]);
(466, [EatInstr(101,487)]);
(83, [EatInstr(110,127)]);
(467, [EatInstr(99,488)]);
(84, [EatInstr(111,128)]);
(468, [EatInstr(116,489)]);
(85, [EatInstr(115,130);EatInstr(110,129)]);
(469, [EatInstr(103,490)]);
(86, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,131)]);
(470, [AAction2Instr(__a50,132)]);
(87, [AAction2Instr(__a5,132)]);
(471, [EatInstr(45,491);AAction2Instr(__a51,132)]);
(88, [CompleteInstr(267)]);
(472, [EatInstr(108,492)]);
(89, [EatInstr(114,133)]);
(473, [EatInstr(116,493)]);
(90, [EatInstr(115,134)]);
(474, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,494)]);
(91, [EatInstr(121,135)]);
(475, [EatInstr(97,495)]);
(92, [EatInstr(117,136)]);
(476, [EatInstr(111,496)]);
(93, [EatInstr(104,137)]);
(477, [AAction2Instr(__a52,176)]);
(94, [EatInstr(101,138)]);
(478, [EatInstr(105,497)]);
(95, [EatInstr(105,139)]);
(479, [AAction2Instr(__a53,176)]);
(96, [EatInstr(101,140)]);
(480, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,498)]);
(97, [EatInstr(116,141)]);
(481, [AAction2Instr(__a54,499)]);
(98, [EatInstr(117,142)]);
(482, [EatInstr(114,500)]);
(99, [EatInstr(99,143)]);
(483, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,501)]);
(100, [EatInstr(115,144)]);
(484, [EatInstr(121,502)]);
(101, [EatInstr(111,145)]);
(485, [AAction2Instr(__a55,75)]);
(102, [EatInstr(112,146)]);
(486, [EatInstr(108,503)]);
(103, [EatInstr(112,147)]);
(487, [EatInstr(45,504)]);
(104, [EatInstr(112,148)]);
(488, [EatInstr(101,505)]);
(105, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,149)]);
(489, [EatInstr(105,506)]);
(106, [EatInstr(99,150)]);
(490, [EatInstr(101,507)]);
(107, [EatInstr(114,151)]);
(491, [EatInstr(115,508)]);
(108, [EatInstr(101,152)]);
(492, [EatInstr(101,509)]);
(109, [EatInstr(111,153)]);
(493, [EatInstr(105,510)]);
(110, [EatInstr(107,154)]);
(494, [AAction2Instr(__a56,176)]);
(111, [EatInstr(45,155)]);
(495, [EatInstr(114,511)]);
(112, [EatInstr(99,156)]);
(496, [EatInstr(114,512)]);
(113, [EatInstr(110,157)]);
(497, [EatInstr(115,513)]);
(114, [AAction2Instr(__a6,158)]);
(498, [AAction2Instr(__a57,176)]);
(115, [EatInstr(116,159)]);
(499, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,514)]);
(116, [EatInstr(105,160)]);
(500, [EatInstr(101,515)]);
(117, [EatInstr(110,161)]);
(501, [AAction2Instr(__a58,226)]);
(118, [EatInstr(116,162)]);
(502, [EatInstr(115,516)]);
(119, [EatInstr(99,163)]);
(503, [EatInstr(121,517)]);
(120, [EatInstr(115,164)]);
(504, [EatInstr(112,518)]);
(121, [EatInstr(101,165)]);
(505, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,519)]);
(122, [EatInstr(117,166)]);
(506, [EatInstr(111,520)]);
(123, [EatInstr(108,167)]);
(507, [EatInstr(110,521)]);
(124, [EatInstr(111,168)]);
(508, [EatInstr(116,522)]);
(125, [EatInstr(109,169)]);
(509, [EatInstr(114,523)]);
(126, [EatInstr(45,170)]);
(510, [EatInstr(118,524)]);
(127, [EatInstr(108,171)]);
(511, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(128, [EatInstr(111,172)]);
(512, [EatInstr(121,526)]);
(129, [EatInstr(114,174);EatInstr(105,173)]);
(513, [EatInstr(116,527)]);
(130, [EatInstr(101,175)]);
(514, [AAction2Instr(__a59,132);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,514)]);
(131, [AAction2Instr(__a7,176)]);
(515, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,528)]);
(132, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,176)]);
(516, [EatInstr(105,529)]);
(133, [EatInstr(105,177)]);
(517, [EatInstr(115,530)]);
(134, [EatInstr(101,178)]);
(518, [EatInstr(114,531)]);
(135, [EatInstr(114,179)]);
(519, [AAction2Instr(__a60,75)]);
(136, [EatInstr(103,180)]);
(520, [EatInstr(110,532)]);
(137, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,181)]);
(521, [EatInstr(45,534);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,533)]);
(138, [EatInstr(114,182)]);
(522, [EatInstr(114,535)]);
(139, [EatInstr(110,183)]);
(523, [AAction2Instr(__a61,132)]);
(140, [EatInstr(114,184)]);
(524, [EatInstr(101,536)]);
(141, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,185)]);
(525, [AAction2Instr(__a62,176)]);
(142, [EatInstr(115,186)]);
(526, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,537)]);
(143, [EatInstr(101,187)]);
(527, [EatInstr(111,538)]);
(144, [EatInstr(101,188)]);
(528, [AAction2Instr(__a63,226)]);
(145, [EatInstr(108,189)]);
(529, [EatInstr(115,539)]);
(146, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,190)]);
(530, [EatInstr(105,540)]);
(147, [EatInstr(105,191)]);
(531, [EatInstr(101,541)]);
(148, [EatInstr(97,192)]);
(532, [EatInstr(115,542)]);
(149, [AAction2Instr(__a8,75)]);
(533, [AAction2Instr(__a64,75)]);
(150, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,193)]);
(534, [EatInstr(115,543)]);
(151, [EatInstr(97,194)]);
(535, [EatInstr(105,544)]);
(152, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,195)]);
(536, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,545)]);
(153, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,196)]);
(537, [AAction2Instr(__a65,176)]);
(154, [EatInstr(97,197)]);
(538, [EatInstr(114,546)]);
(155, [EatInstr(108,198)]);
(539, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,547)]);
(156, [EatInstr(101,199)]);
(540, [EatInstr(115,548)]);
(157, [EatInstr(116,200)]);
(541, [EatInstr(100,549)]);
(158, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,201)]);
(542, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,550)]);
(159, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,202)]);
(543, [EatInstr(99,551)]);
(160, [EatInstr(112,203)]);
(544, [EatInstr(99,552)]);
(161, [EatInstr(115,204)]);
(545, [AAction2Instr(__a66,176)]);
(162, [EatInstr(101,205)]);
(546, [EatInstr(121,553)]);
(163, [EatInstr(107,206)]);
(547, [AAction2Instr(__a67,75)]);
(164, [EatInstr(101,207)]);
(548, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,554)]);
(165, [EatInstr(99,208)]);
(549, [EatInstr(105,555)]);
(166, [EatInstr(110,209)]);
(550, [AAction2Instr(__a68,75)]);
(167, [EatInstr(105,210)]);
(551, [EatInstr(97,556)]);
(168, [EatInstr(107,211)]);
(552, [EatInstr(116,557)]);
(169, [EatInstr(111,212)]);
(553, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,558)]);
(170, [EatInstr(115,216);EatInstr(114,215);EatInstr(109,214);EatInstr(99,213)]);
(554, [AAction2Instr(__a69,75)]);
(171, [EatInstr(121,217)]);
(555, [EatInstr(99,559)]);
(172, [EatInstr(116,218)]);
(556, [EatInstr(110,560)]);
(173, [EatInstr(116,219)]);
(557, [AAction2Instr(__a70,132)]);
(174, [EatInstr(111,220)]);
(558, [AAction2Instr(__a71,176)]);
(175, [EatInstr(45,221)]);
(559, [EatInstr(97,561)]);
(176, [CompleteInstr(273)]);
(560, [EatInstr(110,562)]);
(177, [EatInstr(98,222)]);
(561, [EatInstr(116,563)]);
(178, [EatInstr(45,223)]);
(562, [EatInstr(101,564)]);
(179, [EatInstr(117,224)]);
(563, [EatInstr(101,565)]);
(180, [EatInstr(97,225)]);
(564, [EatInstr(114,566)]);
(181, [AAction2Instr(__a9,226)]);
(565, [EatInstr(115,567)]);
(182, [EatInstr(45,227)]);
(566, [EatInstr(108,568)]);
(183, [EatInstr(101,228)]);
(567, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,569)]);
(184, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,229)]);
(568, [EatInstr(101,570)]);
(185, [AAction2Instr(__a10,226)]);
(569, [AAction2Instr(__a72,75)]);
(186, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,230)]);
(570, [EatInstr(115,571)]);
(187, [EatInstr(100,231)]);
(571, [EatInstr(115,572)]);
(188, [EatInstr(116,232)]);
(572, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,573)]);
(189, [EatInstr(108,233)]);
(573, [AAction2Instr(__a73,75)]);
(190, [AAction2Instr(__a11,226)]);
(191, [EatInstr(108,234)]);
(192, [EatInstr(116,235)]);
(193, [AAction2Instr(__a12,236)]);
(194, [EatInstr(99,237)]);
(195, [AAction2Instr(__a13,75)]);
(196, [AAction2Instr(__a14,75)]);
(197, [EatInstr(104,238)]);
(198, [EatInstr(111,239)]);
(199, [EatInstr(100,240)]);
(200, [EatInstr(45,242);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,241)]);
(201, [AAction2Instr(__a15,243);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,201)]);
(202, [AAction2Instr(__a16,75)]);
(203, [EatInstr(45,244)]);
(204, [EatInstr(108,245)]);
(205, [EatInstr(114,246)]);
(206, [EatInstr(101,247)]);
(207, [EatInstr(45,248)]);
(208, [EatInstr(107,249)]);
(209, [EatInstr(116,250)]);
(210, [EatInstr(110,251)]);
(211, [EatInstr(97,252)]);
(212, [EatInstr(105,253)]);
(213, [EatInstr(111,254)]);
(214, [EatInstr(101,255)]);
(215, [EatInstr(101,256)]);
(216, [EatInstr(107,257)]);
(217, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,258)]);
(218, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,259)]);
(219, [EatInstr(45,260)]);
(220, [EatInstr(108,261)]);
(221, [EatInstr(102,262)]);
(222, [EatInstr(117,263)]);
(223, [EatInstr(117,264)]);
(224, [EatInstr(108,265)]);
(225, [EatInstr(114,266)]);
(226, [CompleteInstr(271)]);
(227, [EatInstr(116,267)]);
(228, [EatInstr(45,268)]);
(229, [AAction2Instr(__a17,226)]);
(230, [AAction2Instr(__a18,226)]);
(231, [EatInstr(101,269)]);
(232, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,270)]);
(233, [EatInstr(45,271)]);
(234, [EatInstr(101,272)]);
(235, [EatInstr(99,273)]);
(236, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,274)]);
(237, [EatInstr(116,275)]);
(238, [EatInstr(101,276)]);
(239, [EatInstr(111,277)]);
(240, [EatInstr(101,278)]);
(241, [AAction2Instr(__a19,75)]);
(242, [EatInstr(114,281);EatInstr(110,280);EatInstr(103,279)]);
(243, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,75)]);
(244, [EatInstr(108,282)]);
(245, [EatInstr(97,283)]);
(246, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,284)]);
(247, [EatInstr(110,285)]);
(248, [EatInstr(105,286)]);
(249, [EatInstr(45,287)]);
(250, [EatInstr(101,288)]);
(251, [EatInstr(101,289)]);
(252, [EatInstr(104,290)]);
(253, [EatInstr(122,291)]);
(254, [EatInstr(97,292)]);
(255, [EatInstr(109,293)]);
(256, [EatInstr(112,294)]);
(257, [EatInstr(105,295)]);
(258, [AAction2Instr(__a20,176)]);
(259, [AAction2Instr(__a21,296)]);
(260, [EatInstr(104,297)]);
(261, [EatInstr(108,298)]);
(262, [EatInstr(115,299)]);
(263, [EatInstr(116,300)]);
(264, [EatInstr(110,301)]);
(265, [EatInstr(101,302)]);
(266, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,303)]);
(267, [EatInstr(121,304)]);
(268, [EatInstr(114,305)]);
(269, [EatInstr(110,306)]);
(270, [AAction2Instr(__a22,226)]);
(271, [EatInstr(115,307)]);
(272, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,308)]);
(273, [EatInstr(104,309)]);
(274, [AAction2Instr(__a23,310)]);
(275, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,311)]);
(276, [EatInstr(97,312)]);
(277, [EatInstr(107,313)]);
(278, [EatInstr(110,314)]);
(279, [EatInstr(105,315)]);
(280, [EatInstr(117,317);EatInstr(112,316)]);
(281, [EatInstr(101,318)]);
(282, [EatInstr(97,319)]);
(283, [EatInstr(116,320)]);
(284, [AAction2Instr(__a24,321)]);
(285, [EatInstr(100,322)]);
(286, [EatInstr(110,323)]);
(287, [EatInstr(108,324)]);
(288, [EatInstr(114,325)]);
(289, [EatInstr(45,326)]);
(290, [EatInstr(101,327)]);
(291, [EatInstr(101,328)]);
(292, [EatInstr(108,329)]);
(293, [EatInstr(111,330)]);
(294, [EatInstr(108,331)]);
(295, [EatInstr(112,332)]);
(296, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,333)]);
(297, [EatInstr(105,334)]);
(298, [EatInstr(45,335)]);
(299, [EatInstr(116,337);EatInstr(109,336)]);
(300, [EatInstr(101,338)]);
(301, [EatInstr(100,339)]);
(302, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,340)]);
(303, [AAction2Instr(__a25,226)]);
(304, [EatInstr(112,341)]);
(305, [EatInstr(101,342)]);
(306, [EatInstr(99,343)]);
(307, [EatInstr(116,344)]);
(308, [AAction2Instr(__a26,75)]);
(309, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,345)]);
(310, [AAction2Instr(__a28,243);AAction2Instr(__a27,346)]);
(311, [AAction2Instr(__a29,75)]);
(312, [EatInstr(100,347)]);
(313, [EatInstr(97,348)]);
(314, [EatInstr(99,349)]);
(315, [EatInstr(108,350)]);
(316, [EatInstr(114,351)]);
(317, [EatInstr(108,352)]);
(318, [EatInstr(108,353)]);
(319, [EatInstr(116,354)]);
(320, [EatInstr(101,355)]);
(321, [AContInstr3(271,__g0,__binder4,176);ACallInstr3(__g0,8)]);
(322, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,356)]);
(323, [EatInstr(115,357)]);
(324, [EatInstr(97,358)]);
(325, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,359)]);
(326, [EatInstr(114,361);EatInstr(99,360)]);
(327, [EatInstr(97,362)]);
(328, [EatInstr(45,363)]);
(329, [EatInstr(101,364)]);
(330, [EatInstr(105,365)]);
(331, [EatInstr(97,366)]);
(332, [EatInstr(45,367)]);
(333, [AAction2Instr(__a30,132)]);
(334, [EatInstr(115,368)]);
(335, [EatInstr(115,369)]);
(336, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,370)]);
(337, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,371)]);
(338, [EatInstr(115,372)]);
(339, [EatInstr(101,373)]);
(340, [AAction2Instr(__a31,226)]);
(341, [EatInstr(101,374)]);
(342, [EatInstr(103,375)]);
(343, [EatInstr(101,376)]);
(344, [EatInstr(97,377)]);
(345, [AAction2Instr(__a32,75)]);
(346, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,378)]);
(347, [EatInstr(45,379)]);
(348, [EatInstr(104,380)]);
(349, [EatInstr(101,381)]);
(350, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,382)]);
(351, [EatInstr(101,383)]);
(352, [EatInstr(108,384)]);
(353, [EatInstr(101,385)]);
(354, [EatInstr(101,386)]);
(355, [EatInstr(45,387)]);
(356, [AAction2Instr(__a33,388)]);
(357, [EatInstr(101,389)]);
(358, [EatInstr(98,390)]);
(359, [AAction2Instr(__a34,391)]);
(360, [EatInstr(115,392)]);
(361, [EatInstr(101,393)]);
(362, [EatInstr(100,394)]);
(363, [EatInstr(104,395)]);
(364, [EatInstr(115,396)]);
(365, [EatInstr(122,397)]);
(366, [EatInstr(121,398)]);
(367, [EatInstr(111,399)]);
(368, [EatInstr(116,400)]);
(369, [EatInstr(116,401)]);
(370, [AAction2Instr(__a35,176)]);
(371, [AAction2Instr(__a36,176)]);
(372, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,402)]);
(373, [EatInstr(114,403)]);
(374, [EatInstr(115,404)]);
(375, [EatInstr(117,405)]);
(376, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,406)]);
(377, [EatInstr(114,407)]);
(378, [AAction2Instr(__a37,408)]);
(379, [EatInstr(97,409)]);
(380, [EatInstr(101,410)]);
(381, [EatInstr(45,411)]);
(382, [AAction2Instr(__a38,75)]);
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
