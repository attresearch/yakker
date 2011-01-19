
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
 (let _x45 = ( after := Some p )
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
 (let _x44 = ( backend := b )
 in ()))
))
 | (1165) -> (
 (let _x43 = ( Compileopt.case_sensitive := false )
 in ()))
 | (1169) -> (
 (let _x42 = ( Compileopt.check_labels := true )
 in ()))
 | (1173) -> (
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
 | (1185) -> (
 (let _x40 = ( Compileopt.inline_cs := true )
 in ()))
 | (1189) -> (
 (let _x39 = ( Compileopt.inline_regular := true )
 in ()))
 | (1193) -> (
 (let _x38 = ( Compileopt.memoize_history := true )
 in ()))
 | (1197) -> (
 (let _x37 = ( Compileopt.memoize_history := false )
 in ()))
 | (1201) -> (
 (let _x36 = ( Compileopt.unit_history := true )
 in ()))
 | (1205) -> (
 (let _x35 = ( Compileopt.skip_opt := false )
 in ()))
 | (1209) -> (
 (let _x34 = ( Compileopt.repress_replay := true )
 in ()))
 | (1213) -> (
 (let _x33 = ( Compileopt.lookahead := true )
 in ()))
 | (1217) -> (
 (let _x32 = ( Compileopt.use_fsm := true )
 in ()))
 | (1221) -> (
 (let _x31 = ( Compileopt.use_fsm := false )
 in ()))
 | (1225) -> (
 (let _x30 = ( Compileopt.coalesce := false )
 in ()))
 | (1229) -> (
 (let _x29 = ( only := true )
 in ()))
 | (1233) -> (
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
 | (1245) -> (
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
 | (1257) -> (
 (let _x26 = ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 in ()))
 | _(*1259*) -> (
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

let __a54 = _p 1169;;
let __a10 = _p 1034;;
let __a70 = _p 1141;;
let __a57 = _p_pos_only 1249;;
let __a19 = _p 1229;;
let __a17 = _p 1037;;
let __a62 = _p 1193;;
let __a55 = _p 1201;;
let __a23 = _p 1144;;
let __a45 = _p_pos_only 1082;;
let __a68 = _p 1197;;
let __a51 = _p 1205;;
let __a8 = _p 1062;;
let __a38 = _p 1013;;
let __a60 = _p 1016;;
let __a58 = _p 1122;;
let __a7 = _p 1257;;
let __a43 = _p 1209;;
let __a30 = _p 1019;;
let __a39 = _p 1040;;
let __a12 = fun p v -> _p_pos_only 1066 p (_p 1065 p (v));;
let __a28 = _p 1092;;
let __a21 = _p 1043;;
let __a64 = _p 1101;;
let __a32 = _p 1151;;
let __a13 = _p 1095;;
let __a44 = _p 1046;;
let __a53 = _p 1104;;
let __a52 = fun p v -> _p_pos_only 1246 p (_p 1245 p (v));;
let __a48 = _p 1153;;
let __a22 = _p_pos_only 1069;;
let __a46 = _p 1155;;
let __a11 = _p 1049;;
let __a14 = _p 1098;;
let __a66 = _p 1107;;
let __a42 = _p 1213;;
let __a40 = _p_pos_only 1177;;
let __a15 = _p_pos_only 1128;;
let __a67 = _p 1157;;
let __a24 = _p 1022;;
let __a49 = _p 1159;;
let __a29 = _p_pos_only 1237;;
let __a33 = fun p v -> _p_pos_only 1174 p (_p 1173 p (v));;
let __a34 = _p 1217;;
let __a9 = _p 1025;;
let __a27 = _p 1075;;
let __a26 = _p 1076;;
let __a56 = _p 1028;;
let __a20 = fun p v -> _p_pos_only 1234 p (_p 1233 p (v));;
let __a65 = _p 1135;;
let __a5 = _p_pos_only 1263;;
let __a41 = _p 1185;;
let __a0 = _p 1050;;
let __a6 = fun p v -> _p_pos_only 1125 p (_p 1124 p (v));;
let __a61 = _p 1138;;
let __a18 = _p 1110;;
let __a59 = _p 1189;;
let __a3 = _p 1006;;
let __a37 = _p 1113;;
let __a25 = _p 1056;;
let __a2 = _p 1007;;
let __a36 = _p_pos_only 1079;;
let __a35 = _p 1221;;
let __a63 = _p 1165;;
let __a47 = _p 1116;;
let __p4 = (fun la ykb v -> match nullable_eof la ykb sv0 with | None -> None | Some _ -> Some v);;
let __a31 = _p 1059;;
let __a1 = fun p v -> _p_pos_only 1260 p (_p 1259 p (v));;
let __a16 = _p 1031;;
let __a69 = _p 1119;;
let __a50 = _p 1225;;
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
let program : (int * sv instruction list) list = [
(383, [EatInstr(105,413)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(99,414)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(101,415)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,416)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(112,417)]);
(4, [AContInstr3(272,_e,__binder1,15);ACallInstr3(_e,9)]);
(388, [EatInstr(111,418)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(97,419)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,17)]);
(390, [AAction2Instr(__a38,219)]);
(7, [EatInstr(127,18);EatInstr(126,18);EatInstr(125,18);EatInstr(124,18);EatInstr(123,18);EatInstr(122,18);EatInstr(121,18);EatInstr(120,18);EatInstr(119,18);EatInstr(118,18);EatInstr(117,18);EatInstr(116,18);EatInstr(115,18);EatInstr(114,18);EatInstr(113,18);EatInstr(112,18);EatInstr(111,18);EatInstr(110,18);EatInstr(109,18);EatInstr(108,18);EatInstr(107,18);EatInstr(106,18);EatInstr(105,18);EatInstr(104,18);EatInstr(103,18);EatInstr(102,18);EatInstr(101,18);EatInstr(100,18);EatInstr(99,18);EatInstr(98,18);EatInstr(97,18);EatInstr(96,18);EatInstr(95,18);EatInstr(94,18);EatInstr(93,18);EatInstr(92,18);EatInstr(91,18);EatInstr(90,18);EatInstr(89,18);EatInstr(88,18);EatInstr(87,18);EatInstr(86,18);EatInstr(85,18);EatInstr(84,18);EatInstr(83,18);EatInstr(82,18);EatInstr(81,18);EatInstr(80,18);EatInstr(79,18);EatInstr(78,18);EatInstr(77,18);EatInstr(76,18);EatInstr(75,18);EatInstr(74,18);EatInstr(73,18);EatInstr(72,18);EatInstr(71,18);EatInstr(70,18);EatInstr(69,18);EatInstr(68,18);EatInstr(67,18);EatInstr(66,18);EatInstr(65,18);EatInstr(64,18);EatInstr(63,18);EatInstr(62,18);EatInstr(61,18);EatInstr(60,18);EatInstr(59,18);EatInstr(58,18);EatInstr(57,18);EatInstr(56,18);EatInstr(55,18);EatInstr(54,18);EatInstr(53,18);EatInstr(52,18);EatInstr(51,18);EatInstr(50,18);EatInstr(49,18);EatInstr(48,18);EatInstr(47,18);EatInstr(46,18);EatInstr(44,18);EatInstr(43,18);EatInstr(42,18);EatInstr(41,18);EatInstr(40,18);EatInstr(39,18);EatInstr(38,18);EatInstr(37,18);EatInstr(36,18);EatInstr(35,18);EatInstr(34,18);EatInstr(33,18);EatInstr(32,18);EatInstr(31,18);EatInstr(30,18);EatInstr(29,18);EatInstr(28,18);EatInstr(27,18);EatInstr(26,18);EatInstr(25,18);EatInstr(24,18);EatInstr(23,18);EatInstr(22,18);EatInstr(21,18);EatInstr(20,18);EatInstr(19,18);EatInstr(18,18);EatInstr(17,18);EatInstr(16,18);EatInstr(15,18);EatInstr(14,18);EatInstr(13,18);EatInstr(12,18);EatInstr(11,18);EatInstr(10,18);EatInstr(9,18);EatInstr(8,18);EatInstr(7,18);EatInstr(6,18);EatInstr(5,18);EatInstr(4,18);EatInstr(3,18);EatInstr(2,18);EatInstr(1,18)]);
(391, [EatInstr(45,420)]);
(8, [EatInstr(119,29);EatInstr(117,28);EatInstr(115,27);EatInstr(112,26);EatInstr(109,25);EatInstr(108,24);EatInstr(105,23);EatInstr(104,22);EatInstr(100,21);EatInstr(99,20);EatInstr(97,19)]);
(392, [EatInstr(108,421)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(102,33);EatInstr(101,32);EatInstr(100,31);EatInstr(99,30);AAction2Instr(__a0,40)]);
(393, [AAction2Instr(__a39,219)]);
(10, [EatInstr(45,41);AAction2Instr(__a1,42)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43)]);
(394, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,422)]);
(395, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,423)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(110,424)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(97,425)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(97,426)]);
(15, [AAction2Instr(__a3,46);AAction2Instr(__a2,45)]);
(399, [EatInstr(115,427)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(98,428)]);
(17, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,17)]);
(401, [EatInstr(97,429)]);
(18, [ALookaheadInstr(false,CfgLA (1,264),48);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,18)]);
(402, [EatInstr(97,430)]);
(19, [EatInstr(116,49)]);
(403, [EatInstr(121,431)]);
(20, [EatInstr(111,51);EatInstr(108,50)]);
(404, [EatInstr(117,432)]);
(21, [EatInstr(101,52)]);
(405, [EatInstr(101,433)]);
(22, [EatInstr(97,53)]);
(406, [EatInstr(120,434)]);
(23, [EatInstr(110,54)]);
(407, [EatInstr(115,435)]);
(24, [EatInstr(105,56);EatInstr(101,55)]);
(408, [EatInstr(108,436)]);
(25, [EatInstr(105,57)]);
(409, [AAction2Instr(__a40,129);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,409)]);
(26, [EatInstr(114,58)]);
(410, [AAction2Instr(__a41,171)]);
(27, [EatInstr(117,59)]);
(411, [EatInstr(117,437)]);
(28, [EatInstr(110,60)]);
(412, [AAction2Instr(__a42,171)]);
(29, [EatInstr(114,61)]);
(413, [EatInstr(115,438)]);
(30, [EatInstr(111,62)]);
(414, [EatInstr(101,439)]);
(31, [EatInstr(111,64);EatInstr(105,63)]);
(415, [EatInstr(45,440)]);
(32, [EatInstr(120,65)]);
(416, [AAction2Instr(__a43,171)]);
(33, [EatInstr(117,66)]);
(417, [EatInstr(116,441)]);
(34, [EatInstr(110,67)]);
(418, [EatInstr(114,442)]);
(35, [EatInstr(114,69);EatInstr(111,68)]);
(419, [EatInstr(114,443)]);
(36, [EatInstr(114,70)]);
(420, [EatInstr(99,444)]);
(37, [EatInstr(102,71)]);
(421, [EatInstr(97,445)]);
(38, [EatInstr(116,72)]);
(422, [AAction2Instr(__a44,219)]);
(39, [EatInstr(114,73)]);
(423, [AAction2Instr(__a45,300)]);
(40, [AContInstr3(271,_e,__binder2,74);ACallInstr3(_e,8)]);
(424, [EatInstr(97,446)]);
(41, [EatInstr(118,85);EatInstr(117,84);EatInstr(114,83);EatInstr(111,82);EatInstr(110,81);EatInstr(109,80);EatInstr(108,79);EatInstr(105,78);EatInstr(99,77);EatInstr(98,76);EatInstr(97,75)]);
(425, [EatInstr(100,447)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,86)]);
(426, [EatInstr(110,448)]);
(43, [CompleteInstr(274)]);
(427, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,449)]);
(428, [EatInstr(108,450)]);
(45, [AContInstr3(273,_e,__binder3,15);ACallInstr3(_e,10)]);
(429, [EatInstr(110,451)]);
(46, [ACallInstr3(__default_call,11);WhenSpecialInstr(__p4,87);ASimpleCont2Instr(274,__binder0,87)]);
(430, [EatInstr(99,452)]);
(47, [CompleteInstr(269)]);
(431, [EatInstr(112,453)]);
(48, [CompleteInstr(270)]);
(432, [EatInstr(110,454)]);
(49, [EatInstr(116,88)]);
(433, [EatInstr(103,455)]);
(50, [EatInstr(111,89)]);
(434, [AAction2Instr(__a46,129)]);
(51, [EatInstr(112,90)]);
(435, [EatInstr(105,456)]);
(52, [EatInstr(115,91)]);
(436, [EatInstr(115,457)]);
(53, [EatInstr(115,92)]);
(437, [EatInstr(108,458)]);
(54, [EatInstr(108,93)]);
(438, [EatInstr(116,459)]);
(55, [EatInstr(120,94)]);
(439, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,460)]);
(56, [EatInstr(102,95)]);
(440, [EatInstr(104,461)]);
(57, [EatInstr(110,96)]);
(441, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,462)]);
(58, [EatInstr(101,97)]);
(442, [EatInstr(121,463)]);
(59, [EatInstr(98,98)]);
(443, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,464)]);
(60, [EatInstr(114,99)]);
(444, [EatInstr(111,465)]);
(61, [EatInstr(97,100)]);
(445, [EatInstr(114,466)]);
(62, [EatInstr(109,101)]);
(446, [EatInstr(108,467)]);
(63, [EatInstr(115,102)]);
(447, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,468)]);
(64, [EatInstr(116,103)]);
(448, [EatInstr(97,469)]);
(65, [EatInstr(116,105);EatInstr(101,104)]);
(449, [AAction2Instr(__a47,74)]);
(66, [EatInstr(115,106)]);
(450, [EatInstr(101,470)]);
(67, [EatInstr(102,107)]);
(451, [EatInstr(99,471)]);
(68, [EatInstr(111,108)]);
(452, [EatInstr(116,472)]);
(69, [EatInstr(49,109)]);
(453, [EatInstr(103,473)]);
(70, [EatInstr(105,111);EatInstr(101,110)]);
(454, [AAction2Instr(__a48,129)]);
(71, [EatInstr(99,112)]);
(455, [EatInstr(45,474);AAction2Instr(__a49,129)]);
(72, [EatInstr(114,113)]);
(456, [EatInstr(116,475)]);
(73, [EatInstr(97,114)]);
(457, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,476)]);
(74, [CompleteInstr(272)]);
(458, [EatInstr(97,477)]);
(75, [EatInstr(102,115)]);
(459, [EatInstr(111,478)]);
(76, [EatInstr(97,116)]);
(460, [AAction2Instr(__a50,171)]);
(77, [EatInstr(111,119);EatInstr(104,118);EatInstr(97,117)]);
(461, [EatInstr(105,479)]);
(78, [EatInstr(110,120)]);
(462, [AAction2Instr(__a51,171)]);
(79, [EatInstr(111,121)]);
(463, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,480)]);
(80, [EatInstr(101,122)]);
(464, [AAction2Instr(__a52,481)]);
(81, [EatInstr(111,123)]);
(465, [EatInstr(114,482)]);
(82, [EatInstr(110,124)]);
(466, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,483)]);
(83, [EatInstr(111,125)]);
(467, [EatInstr(121,484)]);
(84, [EatInstr(115,127);EatInstr(110,126)]);
(468, [AAction2Instr(__a53,74)]);
(85, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,128)]);
(469, [EatInstr(108,485)]);
(86, [AAction2Instr(__a5,129)]);
(470, [EatInstr(45,486)]);
(87, [CompleteInstr(267)]);
(471, [EatInstr(101,487)]);
(88, [EatInstr(114,130)]);
(472, [EatInstr(105,488)]);
(89, [EatInstr(115,131)]);
(473, [EatInstr(101,489)]);
(90, [EatInstr(121,132)]);
(474, [EatInstr(115,490)]);
(91, [EatInstr(117,133)]);
(475, [EatInstr(105,491)]);
(92, [EatInstr(104,134)]);
(476, [AAction2Instr(__a54,171)]);
(93, [EatInstr(105,135)]);
(477, [EatInstr(114,492)]);
(94, [EatInstr(101,136)]);
(478, [EatInstr(114,493)]);
(95, [EatInstr(116,137)]);
(479, [EatInstr(115,494)]);
(96, [EatInstr(117,138)]);
(480, [AAction2Instr(__a55,171)]);
(97, [EatInstr(99,139)]);
(481, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,495)]);
(98, [EatInstr(115,140)]);
(482, [EatInstr(101,496)]);
(99, [EatInstr(111,141)]);
(483, [AAction2Instr(__a56,219)]);
(100, [EatInstr(112,142)]);
(484, [EatInstr(115,497)]);
(101, [EatInstr(112,143)]);
(485, [EatInstr(121,498)]);
(102, [EatInstr(112,144)]);
(486, [EatInstr(112,499)]);
(103, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,145)]);
(487, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,500)]);
(104, [EatInstr(99,146)]);
(488, [EatInstr(111,501)]);
(105, [EatInstr(114,147)]);
(489, [EatInstr(110,502)]);
(106, [EatInstr(101,148)]);
(490, [EatInstr(116,503)]);
(107, [EatInstr(111,149)]);
(491, [EatInstr(118,504)]);
(108, [EatInstr(107,150)]);
(492, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,505)]);
(109, [EatInstr(45,151)]);
(493, [EatInstr(121,506)]);
(110, [EatInstr(99,152)]);
(494, [EatInstr(116,507)]);
(111, [EatInstr(110,153)]);
(495, [AAction2Instr(__a57,129);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,495)]);
(112, [AAction2Instr(__a6,154)]);
(496, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(113, [EatInstr(105,155)]);
(497, [EatInstr(105,509)]);
(114, [EatInstr(110,156)]);
(498, [EatInstr(115,510)]);
(115, [EatInstr(116,157)]);
(499, [EatInstr(114,511)]);
(116, [EatInstr(99,158)]);
(500, [AAction2Instr(__a58,74)]);
(117, [EatInstr(115,159)]);
(501, [EatInstr(110,512)]);
(118, [EatInstr(101,160)]);
(502, [EatInstr(45,514);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,513)]);
(119, [EatInstr(117,161)]);
(503, [EatInstr(114,515)]);
(120, [EatInstr(108,162)]);
(504, [EatInstr(101,516)]);
(121, [EatInstr(111,163)]);
(505, [AAction2Instr(__a59,171)]);
(122, [EatInstr(109,164)]);
(506, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,517)]);
(123, [EatInstr(45,165)]);
(507, [EatInstr(111,518)]);
(124, [EatInstr(108,166)]);
(508, [AAction2Instr(__a60,219)]);
(125, [EatInstr(111,167)]);
(509, [EatInstr(115,519)]);
(126, [EatInstr(114,169);EatInstr(105,168)]);
(510, [EatInstr(105,520)]);
(127, [EatInstr(101,170)]);
(511, [EatInstr(101,521)]);
(128, [AAction2Instr(__a7,171)]);
(512, [EatInstr(115,522)]);
(129, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,171)]);
(513, [AAction2Instr(__a61,74)]);
(130, [EatInstr(105,172)]);
(514, [EatInstr(115,523)]);
(131, [EatInstr(101,173)]);
(515, [EatInstr(105,524)]);
(132, [EatInstr(114,174)]);
(516, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(133, [EatInstr(103,175)]);
(517, [AAction2Instr(__a62,171)]);
(134, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,176)]);
(518, [EatInstr(114,526)]);
(135, [EatInstr(110,177)]);
(519, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,527)]);
(136, [EatInstr(114,178)]);
(520, [EatInstr(115,528)]);
(137, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,179)]);
(521, [EatInstr(100,529)]);
(138, [EatInstr(115,180)]);
(522, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,530)]);
(139, [EatInstr(101,181)]);
(523, [EatInstr(99,531)]);
(140, [EatInstr(101,182)]);
(524, [EatInstr(99,532)]);
(141, [EatInstr(108,183)]);
(525, [AAction2Instr(__a63,171)]);
(142, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,184)]);
(526, [EatInstr(121,533)]);
(143, [EatInstr(105,185)]);
(527, [AAction2Instr(__a64,74)]);
(144, [EatInstr(97,186)]);
(528, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,534)]);
(145, [AAction2Instr(__a8,74)]);
(529, [EatInstr(105,535)]);
(146, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,187)]);
(530, [AAction2Instr(__a65,74)]);
(147, [EatInstr(97,188)]);
(531, [EatInstr(97,536)]);
(148, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,189)]);
(532, [EatInstr(116,537)]);
(149, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,190)]);
(533, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(150, [EatInstr(97,191)]);
(534, [AAction2Instr(__a66,74)]);
(151, [EatInstr(108,192)]);
(535, [EatInstr(99,539)]);
(152, [EatInstr(101,193)]);
(536, [EatInstr(110,540)]);
(153, [EatInstr(116,194)]);
(537, [AAction2Instr(__a67,129)]);
(154, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,195)]);
(538, [AAction2Instr(__a68,171)]);
(155, [EatInstr(112,196)]);
(539, [EatInstr(97,541)]);
(156, [EatInstr(115,197)]);
(540, [EatInstr(110,542)]);
(157, [EatInstr(101,198)]);
(541, [EatInstr(116,543)]);
(158, [EatInstr(107,199)]);
(542, [EatInstr(101,544)]);
(159, [EatInstr(101,200)]);
(543, [EatInstr(101,545)]);
(160, [EatInstr(99,201)]);
(544, [EatInstr(114,546)]);
(161, [EatInstr(110,202)]);
(545, [EatInstr(115,547)]);
(162, [EatInstr(105,203)]);
(546, [EatInstr(108,548)]);
(163, [EatInstr(107,204)]);
(547, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,549)]);
(164, [EatInstr(111,205)]);
(548, [EatInstr(101,550)]);
(165, [EatInstr(115,209);EatInstr(114,208);EatInstr(109,207);EatInstr(99,206)]);
(549, [AAction2Instr(__a69,74)]);
(166, [EatInstr(121,210)]);
(550, [EatInstr(115,551)]);
(167, [EatInstr(116,211)]);
(551, [EatInstr(115,552)]);
(168, [EatInstr(116,212)]);
(552, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,553)]);
(169, [EatInstr(111,213)]);
(553, [AAction2Instr(__a70,74)]);
(170, [EatInstr(45,214)]);
(171, [CompleteInstr(273)]);
(172, [EatInstr(98,215)]);
(173, [EatInstr(45,216)]);
(174, [EatInstr(117,217)]);
(175, [EatInstr(97,218)]);
(176, [AAction2Instr(__a9,219)]);
(177, [EatInstr(101,220)]);
(178, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,221)]);
(179, [AAction2Instr(__a10,219)]);
(180, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,222)]);
(181, [EatInstr(100,223)]);
(182, [EatInstr(116,224)]);
(183, [EatInstr(108,225)]);
(184, [AAction2Instr(__a11,219)]);
(185, [EatInstr(108,226)]);
(186, [EatInstr(116,227)]);
(187, [AAction2Instr(__a12,228)]);
(188, [EatInstr(99,229)]);
(189, [AAction2Instr(__a13,74)]);
(190, [AAction2Instr(__a14,74)]);
(191, [EatInstr(104,230)]);
(192, [EatInstr(111,231)]);
(193, [EatInstr(100,232)]);
(194, [EatInstr(45,234);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,233)]);
(195, [AAction2Instr(__a15,235);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,195)]);
(196, [EatInstr(45,236)]);
(197, [EatInstr(108,237)]);
(198, [EatInstr(114,238)]);
(199, [EatInstr(101,239)]);
(200, [EatInstr(45,240)]);
(201, [EatInstr(107,241)]);
(202, [EatInstr(116,242)]);
(203, [EatInstr(110,243)]);
(204, [EatInstr(97,244)]);
(205, [EatInstr(105,245)]);
(206, [EatInstr(111,246)]);
(207, [EatInstr(101,247)]);
(208, [EatInstr(101,248)]);
(209, [EatInstr(107,249)]);
(210, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,250)]);
(211, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,251)]);
(212, [EatInstr(45,252)]);
(213, [EatInstr(108,253)]);
(214, [EatInstr(102,254)]);
(215, [EatInstr(117,255)]);
(216, [EatInstr(117,256)]);
(217, [EatInstr(108,257)]);
(218, [EatInstr(114,258)]);
(219, [CompleteInstr(271)]);
(220, [EatInstr(45,259)]);
(221, [AAction2Instr(__a16,219)]);
(222, [AAction2Instr(__a17,219)]);
(223, [EatInstr(101,260)]);
(224, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,261)]);
(225, [EatInstr(45,262)]);
(226, [EatInstr(101,263)]);
(227, [EatInstr(99,264)]);
(228, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,265)]);
(229, [EatInstr(116,266)]);
(230, [EatInstr(101,267)]);
(231, [EatInstr(111,268)]);
(232, [EatInstr(101,269)]);
(233, [AAction2Instr(__a18,74)]);
(234, [EatInstr(114,272);EatInstr(110,271);EatInstr(103,270)]);
(235, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,74)]);
(236, [EatInstr(108,273)]);
(237, [EatInstr(97,274)]);
(238, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,275)]);
(239, [EatInstr(110,276)]);
(240, [EatInstr(105,277)]);
(241, [EatInstr(45,278)]);
(242, [EatInstr(101,279)]);
(243, [EatInstr(101,280)]);
(244, [EatInstr(104,281)]);
(245, [EatInstr(122,282)]);
(246, [EatInstr(97,283)]);
(247, [EatInstr(109,284)]);
(248, [EatInstr(112,285)]);
(249, [EatInstr(105,286)]);
(250, [AAction2Instr(__a19,171)]);
(251, [AAction2Instr(__a20,287)]);
(252, [EatInstr(104,288)]);
(253, [EatInstr(108,289)]);
(254, [EatInstr(115,290)]);
(255, [EatInstr(116,291)]);
(256, [EatInstr(110,292)]);
(257, [EatInstr(101,293)]);
(258, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,294)]);
(259, [EatInstr(114,295)]);
(260, [EatInstr(110,296)]);
(261, [AAction2Instr(__a21,219)]);
(262, [EatInstr(115,297)]);
(263, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,298)]);
(264, [EatInstr(104,299)]);
(265, [AAction2Instr(__a22,300)]);
(266, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,301)]);
(267, [EatInstr(97,302)]);
(268, [EatInstr(107,303)]);
(269, [EatInstr(110,304)]);
(270, [EatInstr(105,305)]);
(271, [EatInstr(117,307);EatInstr(112,306)]);
(272, [EatInstr(101,308)]);
(273, [EatInstr(97,309)]);
(274, [EatInstr(116,310)]);
(275, [AAction2Instr(__a23,311)]);
(276, [EatInstr(100,312)]);
(277, [EatInstr(110,313)]);
(278, [EatInstr(108,314)]);
(279, [EatInstr(114,315)]);
(280, [EatInstr(45,316)]);
(281, [EatInstr(101,317)]);
(282, [EatInstr(101,318)]);
(283, [EatInstr(108,319)]);
(284, [EatInstr(111,320)]);
(285, [EatInstr(108,321)]);
(286, [EatInstr(112,322)]);
(287, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,323)]);
(288, [EatInstr(105,324)]);
(289, [EatInstr(45,325)]);
(290, [EatInstr(116,327);EatInstr(109,326)]);
(291, [EatInstr(101,328)]);
(292, [EatInstr(100,329)]);
(293, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,330)]);
(294, [AAction2Instr(__a24,219)]);
(295, [EatInstr(101,331)]);
(296, [EatInstr(99,332)]);
(297, [EatInstr(116,333)]);
(298, [AAction2Instr(__a25,74)]);
(299, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,334)]);
(300, [AAction2Instr(__a27,235);AAction2Instr(__a26,335)]);
(301, [AAction2Instr(__a28,74)]);
(302, [EatInstr(100,336)]);
(303, [EatInstr(97,337)]);
(304, [EatInstr(99,338)]);
(305, [EatInstr(108,339)]);
(306, [EatInstr(114,340)]);
(307, [EatInstr(108,341)]);
(308, [EatInstr(108,342)]);
(309, [EatInstr(116,343)]);
(310, [EatInstr(101,344)]);
(311, [AContInstr3(271,_e,__binder4,171);ACallInstr3(_e,8)]);
(312, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,345)]);
(313, [EatInstr(115,346)]);
(314, [EatInstr(97,347)]);
(315, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,348)]);
(316, [EatInstr(114,350);EatInstr(99,349)]);
(317, [EatInstr(97,351)]);
(318, [EatInstr(45,352)]);
(319, [EatInstr(101,353)]);
(320, [EatInstr(105,354)]);
(321, [EatInstr(97,355)]);
(322, [EatInstr(45,356)]);
(323, [AAction2Instr(__a29,129)]);
(324, [EatInstr(115,357)]);
(325, [EatInstr(115,358)]);
(326, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,359)]);
(327, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,360)]);
(328, [EatInstr(115,361)]);
(329, [EatInstr(101,362)]);
(330, [AAction2Instr(__a30,219)]);
(331, [EatInstr(103,363)]);
(332, [EatInstr(101,364)]);
(333, [EatInstr(97,365)]);
(334, [AAction2Instr(__a31,74)]);
(335, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,366)]);
(336, [EatInstr(45,367)]);
(337, [EatInstr(104,368)]);
(338, [EatInstr(101,369)]);
(339, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,370)]);
(340, [EatInstr(101,371)]);
(341, [EatInstr(108,372)]);
(342, [EatInstr(101,373)]);
(343, [EatInstr(101,374)]);
(344, [EatInstr(45,375)]);
(345, [AAction2Instr(__a32,376)]);
(346, [EatInstr(101,377)]);
(347, [EatInstr(98,378)]);
(348, [AAction2Instr(__a33,379)]);
(349, [EatInstr(115,380)]);
(350, [EatInstr(101,381)]);
(351, [EatInstr(100,382)]);
(352, [EatInstr(104,383)]);
(353, [EatInstr(115,384)]);
(354, [EatInstr(122,385)]);
(355, [EatInstr(121,386)]);
(356, [EatInstr(111,387)]);
(357, [EatInstr(116,388)]);
(358, [EatInstr(116,389)]);
(359, [AAction2Instr(__a34,171)]);
(360, [AAction2Instr(__a35,171)]);
(361, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,390)]);
(362, [EatInstr(114,391)]);
(363, [EatInstr(117,392)]);
(364, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,393)]);
(365, [EatInstr(114,394)]);
(366, [AAction2Instr(__a36,395)]);
(367, [EatInstr(97,396)]);
(368, [EatInstr(101,397)]);
(369, [EatInstr(45,398)]);
(370, [AAction2Instr(__a37,74)]);
(371, [EatInstr(100,399)]);
(372, [EatInstr(97,400)]);
(373, [EatInstr(118,401)]);
(374, [EatInstr(45,402)]);
(375, [EatInstr(100,403)]);
(376, [EatInstr(116,406);EatInstr(112,405);EatInstr(102,404)]);
(377, [EatInstr(110,407)]);
(378, [EatInstr(101,408)]);
(379, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,409)]);
(380, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,410)]);
(381, [EatInstr(103,411)]);
(382, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,412)]);
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
