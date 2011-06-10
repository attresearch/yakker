
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
let _l2hv x = x;; (* label to hv *)

module Yk_Hashed = struct
  type t = hv * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
  let memoize = true
end
module Yk_History = Yak.History.Make(Yk_Hashed)

let rec
 _r_cmd_line_args(_n,ykinput) = (let c = _r_command(_n,ykinput) in (); (while (match _n() with (2000) -> true | _ (*2001*) -> false) do
_r_args(_n,ykinput)done)
; ();  cmd := c )
and
 _r_phases(_n,ykinput) = (match _n() with
 | (2002) -> ((); ();  Attributes_cmd )
 | (2003) -> ((); ();  Close_under_core_cmd )
 | (2004) -> ((); ();  Copyrule_cmd )
 | (2005) -> ((); ();  Desugar_cmd )
 | (2006) -> ((); ();  Hash_cmd )
 | (2007) -> ((); ();  Dearrow_cmd )
 | (2008) -> ((); ();  Infer_ty_cmd )
 | (2009) -> ((); ();  Inline_regular_cmd )
 | (2010) -> ((); ();  Lexer_cmd )
 | (2011) -> ((); ();  Lift_cmd )
 | (2012) -> ((); ();  Minus_cmd )
 | (2013) -> ((); ();  Tx_prec_cmd )
 | (2014) -> ((); ();  Subset_cmd )
 | (2015) -> ((); ();  Unroll_star_cmd )
 | (2016) -> ((); ();  Wrap_cmd )
 | (2017) -> ((); ();  Replay_cmd )
 | _ -> raise Exit)
and
 _r_command(_n,ykinput) = (match _n() with
 | (2018) -> ((let p = _r_phases(_n,ykinput) in  (match p with
                                               Inline_regular_cmd -> Compileopt.inline_regular := true
                                             | Unroll_star_cmd -> if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1
                                             | _ -> ());
                                            p ))
 | (2019) -> ((); ();  Compile_cmd )
 | (2020) -> ((); ();  Dispatch_cmd )
 | (2021) -> ((); ();  Dot_cmd )
 | (2022) -> ((); (); (let _x4 = _n() in (); (let _x3 = _n() in (let f = Yak.YkBuf.get_string _x4 _x3 ykinput in (let l = (let _x8 = (let rec _x19 _x8 =
(match _n() with (2024) -> _x8 | _ (*2023*) ->
 _x19((let _x7 = (); (let _x6 = _n() in (); (let _x5 = _n() in (let x = Yak.YkBuf.get_string _x6 _x5 ykinput in x))) in _x7::_x8)))
in _x19(Yak.Util.nil)) in (List.rev _x8)) in ();  files := f::!files; exec_l := l; Exec_cmd )))))
 | (2025) -> ((); ();  Extract_cmd )
 | (2026) -> ((); ();  Compileopt.coalesce := true; Fuse_cmd )
 | (2027) -> ((); ();  Info_cmd )
 | (2028) -> ((); ();  Lookahead_analysis_cmd )
 | (2029) -> ((); ();  Lr1_lookahead_cmd )
 | (2030) -> ((); ();  Precedence_analysis_cmd )
 | (2031) -> ((); ();  Print_gul_cmd )
 | (2032) -> ((); ();  Print_gil_cmd )
 | (2033) -> ((); ();  Print_npreds_cmd )
 | (2034) -> ((); ();  Print_npreds_cmd )
 | (2035) -> ((); ();  Print_relevance_cmd )
 | (2036) -> ((); (let _x10 = _n() in (); (let _x9 = _n() in (let n = Yak.YkBuf.get_string _x10 _x9 ykinput in ();  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" ))))
 | (2037) -> ((); ();  Sort_cmd )
 | (2038) -> ((); ();  Strip_late_actions_cmd )
 | (2039) -> ((); ();  Translate_dypgen_cmd )
 | (2040) -> ((); ();  Translate_dypgen_scannerless_cmd )
 | _ -> raise Exit)
and
 _r_args(_n,ykinput) = (match _n() with
 | (2041) -> ((); (); (let p = _r_phases(_n,ykinput) in  after := Some p ; ()))
 | (2042) -> ((); ();  Compileopt.use_coroutines := false ; ())
 | (2043) -> ((); (); (let b = (match _n() with
 | (2044) -> ((); Fun_BE)
 | (2045) -> ((); Trans_BE)
 | (2046) -> ((); Wadler_BE)
 | (2047) -> ((); Peg_BE false)
 | (2048) -> ((); Peg_BE true)
 | _ -> raise Exit) in ();  backend := b ; ()))
 | (2049) -> ((); ();  Compileopt.case_sensitive := false ; ())
 | (2050) -> ((); ();  Compileopt.check_labels := true ; ())
 | (2051) -> ((); (); (let _x12 = _n() in (); (let _x11 = _n() in (let n = Yak.YkBuf.get_string _x12 _x11 ykinput in ();  Variables.counter := (int_of_string n) ; ()))))
 | (2052) -> ((); ();  Compileopt.inline_cs := true ; ())
 | (2053) -> ((); ();  Compileopt.inline_regular := true ; ())
 | (2054) -> ((); ();  Compileopt.memoize_history := true ; ())
 | (2055) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2056) -> ((); ();  Compileopt.unit_history := true ; ())
 | (2057) -> ((); ();  Compileopt.skip_opt := false ; ())
 | (2058) -> ((); ();  Compileopt.repress_replay := true ; ())
 | (2059) -> ((); ();  Compileopt.lookahead := true ; ())
 | (2060) -> ((); ();  Compileopt.use_fsm := true ; ())
 | (2061) -> ((); ();  Compileopt.use_fsm := false ; ())
 | (2062) -> ((); ();  Compileopt.coalesce := false ; ())
 | (2063) -> ((); ();  only := true ; ())
 | (2064) -> ((); (); (let _x14 = _n() in (); (let _x13 = _n() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2065) -> ((); (); (let _x16 = _n() in (); (let _x15 = _n() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  Compileopt.unroll_star_n := (int_of_string n) ; ()))))
 | (2066) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2067) -> ((let _x18 = _n() in (); (let _x17 = _n() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
 | _ -> raise Exit)
class ['a] rvs (labels: 'a History.postfix) =
let s = ref [] in
let push x = s := x::!s in
let rec _n() = let (x,_) = labels#next() in x
and _rv_cmd_line_args() = ();();push((2001)); while (match _n() with (2000) -> true | _ (*2001*)-> false) do
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
 | _ -> raise Exit)
and _rv_command() = (match _n() with
 | (2018) -> (();_rv_phases(); push((2018)))
 | (2019) -> (();();(); push((2019)))
 | (2020) -> (();();(); push((2020)))
 | (2021) -> (();();(); push((2021)))
 | (2022) -> (();();();push((2024)); while (match _n() with (2023) -> true | _ (*2024*)-> false) do
 ();();();push(_n());();push(_n());(); push((2023))
done
;();push(_n());();push(_n());();(); push((2022)))
 | (2025) -> (();();(); push((2025)))
 | (2026) -> (();();(); push((2026)))
 | (2027) -> (();();(); push((2027)))
 | (2028) -> (();();(); push((2028)))
 | (2029) -> (();();(); push((2029)))
 | (2030) -> (();();(); push((2030)))
 | (2031) -> (();();(); push((2031)))
 | (2032) -> (();();(); push((2032)))
 | (2033) -> (();();(); push((2033)))
 | (2034) -> (();();(); push((2034)))
 | (2035) -> (();();(); push((2035)))
 | (2036) -> (();();();push(_n());();push(_n());(); push((2036)))
 | (2037) -> (();();(); push((2037)))
 | (2038) -> (();();(); push((2038)))
 | (2039) -> (();();(); push((2039)))
 | (2040) -> (();();(); push((2040)))
 | _ -> raise Exit)
and _rv_args() = (match _n() with
 | (2041) -> (();();_rv_phases();();(); push((2041)))
 | (2042) -> (();();();(); push((2042)))
 | (2043) -> (();();();(match _n() with
 | (2044) -> (();(); push((2044)))
 | (2045) -> (();(); push((2045)))
 | (2046) -> (();(); push((2046)))
 | (2047) -> (();(); push((2047)))
 | (2048) -> (();(); push((2048)))
 | _ -> raise Exit);();(); push((2043)))
 | (2049) -> (();();();(); push((2049)))
 | (2050) -> (();();();(); push((2050)))
 | (2051) -> (();();();();push(_n());();push(_n());();(); push((2051)))
 | (2052) -> (();();();(); push((2052)))
 | (2053) -> (();();();(); push((2053)))
 | (2054) -> (();();();(); push((2054)))
 | (2055) -> (();();();(); push((2055)))
 | (2056) -> (();();();(); push((2056)))
 | (2057) -> (();();();(); push((2057)))
 | (2058) -> (();();();(); push((2058)))
 | (2059) -> (();();();(); push((2059)))
 | (2060) -> (();();();(); push((2060)))
 | (2061) -> (();();();(); push((2061)))
 | (2062) -> (();();();(); push((2062)))
 | (2063) -> (();();();(); push((2063)))
 | (2064) -> (();();();();push(_n());();push(_n());();(); push((2064)))
 | (2065) -> (();();();();push(_n());();push(_n());();(); push((2065)))
 | (2066) -> (();();();(); push((2066)))
 | (2067) -> (();();();();push(_n());();push(_n()); push((2067)))
 | _ -> raise Exit)
in
object (self)
method next() = (match !s with hd::tl -> (s := tl; hd) | _ -> raise Not_found)
initializer _rv_cmd_line_args()
end

let _replay_cmd_line_args ykinput h =
  let _o = new rvs (h#rtl) in
  let _n() = _o#next() in
  _r_cmd_line_args(_n,ykinput)
(* History constructors *)
let _e p h = h#empty p
let _p x p = (fun h->h#push p ( x,p))
let _p_pos p = (fun h->h#push p ( p,p))
let _m x p = (fun h1 h2-> h1#merge p ( x,p) h2)

(*LATE PROLOGUE*)
type _pos = int (* input positions *)
let hv_compare = Yk_History.compare
type sv = (hv*_pos, Yak.History.label) Yak.History.history
let sv0 = Yk_History.new_history()
let sv_compare = hv_compare
let sv_hash = Yk_History.hash
let sv_eq x y = sv_compare x y = 0
let key_eq (i,v1) (j,v2) = i = j &&  sv_eq v1 v2
let key_hash (i,v) = i lxor (sv_hash v)

(** Hashtable for top-down parsing. *)
module TDHashtable = Hashtbl.Make(struct type t = int * sv let equal = key_eq let hash = key_hash end)

let __default_call _ _ = sv0;;
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
let __a30 = (_p(2061));;
let __a15 = (_p(2012));;
let __a63 = (_p(2028));;
let __a32 = (_p(2064));;
let __a41 = (_p(2015));;
let __a33 = (_p(2002));;
let __a46 = (_p(2051));;
let __a7 = (_p(2067));;
let __a3 = (_p(2018));;
let __a60 = (_p(2054));;
let __a22 = (_p(2005));;
let __a25 = (_p(2041));;
let __a48 = (_p(2057));;
let __a40 = (_p(2008));;
let __a42 = (_p(2044));;
let __a16 = (_p(2031));;
let __a65 = (_p(2047));;
let __a20 = (fun _x0_ _x1_ -> (((_p(2024)) _x0_) (((_p_pos) _x0_) _x1_)));;
let __a68 = (_p(2034));;
let __a6 = (_p(2021));;
let __a13 = (_p(2037));;
let __a29 = (_p(2060));;
let __a9 = (_p(2011));;
let __a12 = (_p(2027));;
let __a17 = (_p(2063));;
let __a19 = (_p(2014));;
let __a50 = (_p(2050));;
let __a2 = (_p(2001));;
let __a5 = (_p(2066));;
let __a18 = (_p(2017));;
let __a35 = (fun _x0_ _x1_ -> (((_p(2023)) _x0_) (((_p_pos) _x0_) _x1_)));;
let __a57 = (_p(2053));;
let __a26 = (_p(2004));;
let __a69 = (_p(2040));;
let __a52 = (_p(2007));;
let __a51 = (_p(2056));;
let __a45 = (_p(2043));;
let __a66 = (_p(2030));;
let __a38 = (_p(2059));;
let __a54 = (_p(2046));;
let __a44 = (_p(2033));;
let __a62 = (_p(2049));;
let __a27 = (_p(2020));;
let __a21 = (_p(2036));;
let __a59 = (_p(2039));;
let __a14 = (_p(2010));;
let __a11 = (_p(2026));;
let __a47 = (_p(2062));;
let __a34 = (_p(2013));;
let __a49 = (_p(2029));;
let __a4 = (_p(2000));;
let __a61 = (_p(2065));;
let __a10 = (_p(2016));;
let __a58 = (_p(2003));;
let __a37 = (_p(2052));;
let __a23 = (_p(2019));;
let __a1 = (_p_pos);;
let __a67 = (_p(2055));;
let __a8 = (_p(2006));;
let __a56 = (_p(2042));;
let __a53 = (_p(2009));;
let __a39 = (_p(2058));;
let __g0 = (_e);;
let __a36 = (_p(2045));;
let __a31 = (_p(2032));;
let __a43 = (_p(2048));;
let __a55 = (_p(2035));;
let __a28 = (_p(2022));;
let __a64 = (_p(2038));;
let __a24 = (_p(2025));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1002);;
let __binder2 = (_m 1098);;
let __binder3 = (_m 1011);;
let __binder4 = (_m 1241);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(115,421)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(101,422)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(100,423)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(104,424)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(115,425)]);
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [EatInstr(122,426)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(121,427)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,43)]);
(390, [EatInstr(111,428)]);
(7, [EatInstr(127,45);EatInstr(126,45);EatInstr(125,45);EatInstr(124,45);EatInstr(123,45);EatInstr(122,45);EatInstr(121,45);EatInstr(120,45);EatInstr(119,45);EatInstr(118,45);EatInstr(117,45);EatInstr(116,45);EatInstr(115,45);EatInstr(114,45);EatInstr(113,45);EatInstr(112,45);EatInstr(111,45);EatInstr(110,45);EatInstr(109,45);EatInstr(108,45);EatInstr(107,45);EatInstr(106,45);EatInstr(105,45);EatInstr(104,45);EatInstr(103,45);EatInstr(102,45);EatInstr(101,45);EatInstr(100,45);EatInstr(99,45);EatInstr(98,45);EatInstr(97,45);EatInstr(96,45);EatInstr(95,45);EatInstr(94,45);EatInstr(93,45);EatInstr(92,45);EatInstr(91,45);EatInstr(90,45);EatInstr(89,45);EatInstr(88,45);EatInstr(87,45);EatInstr(86,45);EatInstr(85,45);EatInstr(84,45);EatInstr(83,45);EatInstr(82,45);EatInstr(81,45);EatInstr(80,45);EatInstr(79,45);EatInstr(78,45);EatInstr(77,45);EatInstr(76,45);EatInstr(75,45);EatInstr(74,45);EatInstr(73,45);EatInstr(72,45);EatInstr(71,45);EatInstr(70,45);EatInstr(69,45);EatInstr(68,45);EatInstr(67,45);EatInstr(66,45);EatInstr(65,45);EatInstr(64,45);EatInstr(63,45);EatInstr(62,45);EatInstr(61,45);EatInstr(60,45);EatInstr(59,45);EatInstr(58,45);EatInstr(57,45);EatInstr(56,45);EatInstr(55,45);EatInstr(54,45);EatInstr(53,45);EatInstr(52,45);EatInstr(51,45);EatInstr(50,45);EatInstr(49,45);EatInstr(48,45);EatInstr(47,45);EatInstr(46,45);EatInstr(44,45);EatInstr(43,45);EatInstr(42,45);EatInstr(41,45);EatInstr(40,45);EatInstr(39,45);EatInstr(38,45);EatInstr(37,45);EatInstr(36,45);EatInstr(35,45);EatInstr(34,45);EatInstr(33,45);EatInstr(32,45);EatInstr(31,45);EatInstr(30,45);EatInstr(29,45);EatInstr(28,45);EatInstr(27,45);EatInstr(26,45);EatInstr(25,45);EatInstr(24,45);EatInstr(23,45);EatInstr(22,45);EatInstr(21,45);EatInstr(20,45);EatInstr(19,45);EatInstr(18,45);EatInstr(17,45);EatInstr(16,45);EatInstr(15,45);EatInstr(14,45);EatInstr(13,45);EatInstr(12,45);EatInstr(11,45);EatInstr(10,45);EatInstr(9,45);EatInstr(8,45);EatInstr(7,45);EatInstr(6,45);EatInstr(5,45);EatInstr(4,45);EatInstr(3,45);EatInstr(2,45);EatInstr(1,45)]);
(391, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,429)]);
(8, [EatInstr(119,28);EatInstr(117,27);EatInstr(115,26);EatInstr(114,25);EatInstr(112,24);EatInstr(109,23);EatInstr(108,22);EatInstr(105,21);EatInstr(104,20);EatInstr(100,19);EatInstr(99,18);EatInstr(97,17)]);
(392, [EatInstr(116,430)]);
(9, [EatInstr(116,38);EatInstr(115,37);EatInstr(114,36);EatInstr(112,35);EatInstr(108,34);EatInstr(105,33);EatInstr(102,32);EatInstr(101,31);EatInstr(100,30);EatInstr(99,29);AContInstr3(271,__g0,__binder2,39);ACallInstr3(__g0,8)]);
(393, [EatInstr(116,431)]);
(10, [EatInstr(45,40);AAction2Instr(__a1,41)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),42)]);
(394, [AAction2Instr(__a29,232)]);
(395, [AAction2Instr(__a30,232)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(116,432)]);
(13, [CompleteInstr(265)]);
(397, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,433)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(114,434)]);
(15, [AAction2Instr(__a2,136)]);
(399, [EatInstr(115,435)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(117,436)]);
(17, [EatInstr(116,48);EatInstr(114,47)]);
(401, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,437)]);
(18, [EatInstr(111,50);EatInstr(108,49)]);
(402, [EatInstr(114,438)]);
(19, [EatInstr(101,51)]);
(403, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,439)]);
(20, [EatInstr(97,52)]);
(404, [EatInstr(97,440)]);
(21, [EatInstr(110,53)]);
(405, [EatInstr(101,441)]);
(22, [EatInstr(105,55);EatInstr(101,54)]);
(406, [EatInstr(45,442)]);
(23, [EatInstr(105,56)]);
(407, [AAction2Instr(__a31,202)]);
(24, [EatInstr(114,57)]);
(408, [EatInstr(100,443)]);
(25, [EatInstr(101,58)]);
(409, [EatInstr(97,444)]);
(26, [EatInstr(117,59)]);
(410, [EatInstr(118,445)]);
(27, [EatInstr(110,60)]);
(411, [EatInstr(45,446)]);
(28, [EatInstr(114,61)]);
(412, [EatInstr(100,447)]);
(29, [EatInstr(111,62)]);
(413, [EatInstr(97,448)]);
(30, [EatInstr(111,64);EatInstr(105,63)]);
(414, [EatInstr(117,449)]);
(31, [EatInstr(120,65)]);
(415, [EatInstr(101,450)]);
(32, [EatInstr(117,66)]);
(416, [EatInstr(120,451)]);
(33, [EatInstr(110,67)]);
(417, [EatInstr(97,452)]);
(34, [EatInstr(114,69);EatInstr(111,68)]);
(418, [EatInstr(110,453)]);
(35, [EatInstr(114,70)]);
(419, [EatInstr(101,454)]);
(36, [EatInstr(102,71)]);
(420, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,484)]);
(37, [EatInstr(116,73);EatInstr(111,72)]);
(421, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,455)]);
(38, [EatInstr(114,74)]);
(422, [EatInstr(103,456)]);
(39, [AAction2Instr(__a3,202)]);
(423, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,457)]);
(40, [EatInstr(118,85);EatInstr(117,84);EatInstr(114,83);EatInstr(111,82);EatInstr(110,81);EatInstr(109,80);EatInstr(108,79);EatInstr(105,78);EatInstr(99,77);EatInstr(98,76);EatInstr(97,75)]);
(424, [EatInstr(105,458)]);
(41, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,86)]);
(425, [EatInstr(99,459)]);
(42, [CompleteInstr(274)]);
(426, [EatInstr(101,460)]);
(43, [ALookaheadInstr(false,CfgLA (1,264),44);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,43)]);
(427, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,461)]);
(44, [CompleteInstr(269)]);
(428, [EatInstr(112,462)]);
(45, [ALookaheadInstr(false,CfgLA (1,264),46);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,45)]);
(429, [AAction2Instr(__a32,232)]);
(46, [CompleteInstr(270)]);
(430, [EatInstr(111,463)]);
(47, [EatInstr(114,89)]);
(431, [EatInstr(97,464)]);
(48, [EatInstr(116,90)]);
(432, [EatInstr(105,465)]);
(49, [EatInstr(111,91)]);
(433, [AAction2Instr(__a33,582)]);
(50, [EatInstr(112,92)]);
(434, [EatInstr(45,466)]);
(51, [EatInstr(115,93)]);
(435, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,467)]);
(52, [EatInstr(115,94)]);
(436, [EatInstr(108,468)]);
(53, [EatInstr(108,96);EatInstr(102,95)]);
(437, [AAction2Instr(__a34,582)]);
(54, [EatInstr(120,97)]);
(438, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,469)]);
(55, [EatInstr(102,98)]);
(439, [AAction2Instr(__a35,330)]);
(56, [EatInstr(110,99)]);
(440, [EatInstr(110,470)]);
(57, [EatInstr(101,100)]);
(441, [EatInstr(97,471)]);
(58, [EatInstr(112,101)]);
(442, [EatInstr(97,472)]);
(59, [EatInstr(98,102)]);
(443, [EatInstr(115,473)]);
(60, [EatInstr(114,103)]);
(444, [EatInstr(98,474)]);
(61, [EatInstr(97,104)]);
(445, [EatInstr(97,475)]);
(62, [EatInstr(109,105)]);
(446, [EatInstr(97,476)]);
(63, [EatInstr(115,106)]);
(447, [EatInstr(121,477)]);
(64, [EatInstr(116,107)]);
(448, [EatInstr(116,478)]);
(65, [EatInstr(116,109);EatInstr(101,108)]);
(449, [EatInstr(110,479)]);
(66, [EatInstr(115,110)]);
(450, [EatInstr(103,480)]);
(67, [EatInstr(102,111)]);
(451, [AAction2Instr(__a36,563)]);
(68, [EatInstr(111,112)]);
(452, [EatInstr(100,481)]);
(69, [EatInstr(49,113)]);
(453, [EatInstr(115,482)]);
(70, [EatInstr(105,115);EatInstr(101,114)]);
(454, [EatInstr(108,483)]);
(71, [EatInstr(99,116)]);
(455, [AAction2Instr(__a37,232)]);
(72, [EatInstr(114,117)]);
(456, [EatInstr(117,486)]);
(73, [EatInstr(114,118)]);
(457, [AAction2Instr(__a38,232)]);
(74, [EatInstr(97,119)]);
(458, [EatInstr(115,487)]);
(75, [EatInstr(114,121);EatInstr(102,120)]);
(459, [EatInstr(101,488)]);
(76, [EatInstr(97,122)]);
(460, [EatInstr(45,489)]);
(77, [EatInstr(111,125);EatInstr(104,124);EatInstr(97,123)]);
(461, [AAction2Instr(__a39,232)]);
(78, [EatInstr(110,126)]);
(462, [EatInstr(116,490)]);
(79, [EatInstr(111,127)]);
(463, [EatInstr(114,491)]);
(80, [EatInstr(101,128)]);
(464, [EatInstr(114,492)]);
(81, [EatInstr(111,129)]);
(465, [EatInstr(111,493)]);
(82, [EatInstr(110,130)]);
(466, [EatInstr(99,494)]);
(83, [EatInstr(111,131)]);
(467, [AAction2Instr(__a40,582)]);
(84, [EatInstr(115,133);EatInstr(110,132)]);
(468, [EatInstr(97,495)]);
(85, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,134)]);
(469, [AAction2Instr(__a41,582)]);
(86, [AAction2Instr(__a1,135)]);
(470, [EatInstr(97,496)]);
(87, [AAction2Instr(__a4,136)]);
(471, [EatInstr(100,497)]);
(88, [CompleteInstr(267)]);
(472, [EatInstr(110,498)]);
(89, [EatInstr(111,137)]);
(473, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,499)]);
(90, [EatInstr(114,138)]);
(474, [EatInstr(108,500)]);
(91, [EatInstr(115,139)]);
(475, [EatInstr(110,501)]);
(92, [EatInstr(121,140)]);
(476, [EatInstr(99,502)]);
(93, [EatInstr(117,141)]);
(477, [EatInstr(112,503)]);
(94, [EatInstr(104,142)]);
(478, [EatInstr(105,504)]);
(95, [EatInstr(101,143)]);
(479, [AAction2Instr(__a42,563)]);
(96, [EatInstr(105,144)]);
(480, [EatInstr(45,505);AAction2Instr(__a43,563)]);
(97, [EatInstr(101,145)]);
(481, [EatInstr(108,507)]);
(98, [EatInstr(116,146)]);
(482, [EatInstr(105,508)]);
(99, [EatInstr(117,147)]);
(483, [EatInstr(115,509)]);
(100, [EatInstr(99,148)]);
(484, [AAction2Instr(__a1,485);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,484)]);
(101, [EatInstr(108,149)]);
(485, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,510)]);
(102, [EatInstr(115,150)]);
(486, [EatInstr(108,511)]);
(103, [EatInstr(111,151)]);
(487, [EatInstr(116,512)]);
(104, [EatInstr(112,152)]);
(488, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,513)]);
(105, [EatInstr(112,153)]);
(489, [EatInstr(104,514)]);
(106, [EatInstr(112,154)]);
(490, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,515)]);
(107, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,155)]);
(491, [EatInstr(121,516)]);
(108, [EatInstr(99,156)]);
(492, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,517)]);
(109, [EatInstr(114,157)]);
(493, [EatInstr(110,518)]);
(110, [EatInstr(101,158)]);
(494, [EatInstr(111,519)]);
(111, [EatInstr(111,159)]);
(495, [EatInstr(114,520)]);
(112, [EatInstr(107,160)]);
(496, [EatInstr(108,521)]);
(113, [EatInstr(45,161)]);
(497, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,522)]);
(114, [EatInstr(99,162)]);
(498, [EatInstr(97,523)]);
(115, [EatInstr(110,163)]);
(499, [AAction2Instr(__a44,202)]);
(116, [AAction2Instr(__a1,164)]);
(500, [EatInstr(101,524)]);
(117, [EatInstr(116,165)]);
(501, [EatInstr(99,525)]);
(118, [EatInstr(105,166)]);
(502, [EatInstr(116,526)]);
(119, [EatInstr(110,167)]);
(503, [EatInstr(103,527)]);
(120, [EatInstr(116,168)]);
(504, [EatInstr(111,528)]);
(121, [EatInstr(114,169)]);
(505, [EatInstr(115,529)]);
(122, [EatInstr(99,170)]);
(506, [AAction2Instr(__a45,232)]);
(123, [EatInstr(115,171)]);
(507, [EatInstr(101,530)]);
(124, [EatInstr(101,172)]);
(508, [EatInstr(116,531)]);
(125, [EatInstr(117,173)]);
(509, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,532)]);
(126, [EatInstr(108,174)]);
(510, [AAction2Instr(__a46,232)]);
(127, [EatInstr(111,175)]);
(511, [EatInstr(97,533)]);
(128, [EatInstr(109,176)]);
(512, [EatInstr(111,534)]);
(129, [EatInstr(45,177)]);
(513, [AAction2Instr(__a47,232)]);
(130, [EatInstr(108,178)]);
(514, [EatInstr(105,535)]);
(131, [EatInstr(111,179)]);
(515, [AAction2Instr(__a48,232)]);
(132, [EatInstr(114,181);EatInstr(105,180)]);
(516, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,536)]);
(133, [EatInstr(101,182)]);
(517, [AAction2Instr(__a1,537)]);
(134, [AAction2Instr(__a5,232)]);
(518, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(135, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,183)]);
(519, [EatInstr(114,539)]);
(136, [ALookaheadInstr(false,CfgLA (3,266),88);ACallInstr3(__default_call,11);AContInstr3(273,__g0,__binder3,87);ACallInstr3(__g0,10);ASimpleCont2Instr(274,__binder0,88)]);
(520, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,540)]);
(137, [EatInstr(119,184)]);
(521, [EatInstr(121,541)]);
(138, [EatInstr(105,185)]);
(522, [AAction2Instr(__a49,202)]);
(139, [EatInstr(101,186)]);
(523, [EatInstr(108,542)]);
(140, [EatInstr(114,187)]);
(524, [EatInstr(45,543)]);
(141, [EatInstr(103,188)]);
(525, [EatInstr(101,544)]);
(142, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,189)]);
(526, [EatInstr(105,545)]);
(143, [EatInstr(114,190)]);
(527, [EatInstr(101,546)]);
(144, [EatInstr(110,191)]);
(528, [EatInstr(110,547)]);
(145, [EatInstr(114,192)]);
(529, [EatInstr(116,548)]);
(146, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,193)]);
(530, [EatInstr(114,549)]);
(147, [EatInstr(115,194)]);
(531, [EatInstr(105,550)]);
(148, [EatInstr(101,195)]);
(532, [AAction2Instr(__a50,232)]);
(149, [EatInstr(97,196)]);
(533, [EatInstr(114,551)]);
(150, [EatInstr(101,197)]);
(534, [EatInstr(114,552)]);
(151, [EatInstr(108,198)]);
(535, [EatInstr(115,553)]);
(152, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,199)]);
(536, [AAction2Instr(__a51,232)]);
(153, [EatInstr(105,200)]);
(537, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,568)]);
(154, [EatInstr(97,201)]);
(538, [AAction2Instr(__a52,582)]);
(155, [AAction2Instr(__a6,202)]);
(539, [EatInstr(101,554)]);
(156, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,203)]);
(540, [AAction2Instr(__a53,582)]);
(157, [EatInstr(97,204)]);
(541, [EatInstr(115,555)]);
(158, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,205)]);
(542, [EatInstr(121,556)]);
(159, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,206)]);
(543, [EatInstr(112,557)]);
(160, [EatInstr(97,207)]);
(544, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,558)]);
(161, [EatInstr(108,208)]);
(545, [EatInstr(111,559)]);
(162, [EatInstr(101,209)]);
(546, [EatInstr(110,560)]);
(163, [EatInstr(116,210)]);
(547, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,561)]);
(164, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,255)]);
(548, [EatInstr(114,562)]);
(165, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,211)]);
(549, [AAction2Instr(__a54,563)]);
(166, [EatInstr(112,212)]);
(550, [EatInstr(118,564)]);
(167, [EatInstr(115,213)]);
(551, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,565)]);
(168, [EatInstr(101,214)]);
(552, [EatInstr(121,566)]);
(169, [EatInstr(111,215)]);
(553, [EatInstr(116,567)]);
(170, [EatInstr(107,216)]);
(554, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,570)]);
(171, [EatInstr(101,217)]);
(555, [EatInstr(105,571)]);
(172, [EatInstr(99,218)]);
(556, [EatInstr(115,572)]);
(173, [EatInstr(110,219)]);
(557, [EatInstr(114,573)]);
(174, [EatInstr(105,220)]);
(558, [AAction2Instr(__a55,202)]);
(175, [EatInstr(107,221)]);
(559, [EatInstr(110,574)]);
(176, [EatInstr(111,222)]);
(560, [EatInstr(45,576);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,575)]);
(177, [EatInstr(115,226);EatInstr(114,225);EatInstr(109,224);EatInstr(99,223)]);
(561, [AAction2Instr(__a56,232)]);
(178, [EatInstr(121,227)]);
(562, [EatInstr(105,577)]);
(179, [EatInstr(116,228)]);
(563, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,506)]);
(180, [EatInstr(116,229)]);
(564, [EatInstr(101,578)]);
(181, [EatInstr(111,230)]);
(565, [AAction2Instr(__a57,232)]);
(182, [EatInstr(45,231)]);
(566, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,579)]);
(183, [AAction2Instr(__a7,232)]);
(567, [EatInstr(111,580)]);
(184, [EatInstr(45,233)]);
(568, [AAction2Instr(__a1,569);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,568)]);
(185, [EatInstr(98,234)]);
(569, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,581)]);
(186, [EatInstr(45,235)]);
(570, [AAction2Instr(__a58,582)]);
(187, [EatInstr(117,236)]);
(571, [EatInstr(115,583)]);
(188, [EatInstr(97,237)]);
(572, [EatInstr(105,584)]);
(189, [AAction2Instr(__a8,582)]);
(573, [EatInstr(101,585)]);
(190, [EatInstr(45,238)]);
(574, [EatInstr(115,586)]);
(191, [EatInstr(101,239)]);
(575, [AAction2Instr(__a59,202)]);
(192, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,240)]);
(576, [EatInstr(115,587)]);
(193, [AAction2Instr(__a9,582)]);
(577, [EatInstr(99,588)]);
(194, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,241)]);
(578, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,589)]);
(195, [EatInstr(100,242)]);
(579, [AAction2Instr(__a60,232)]);
(196, [EatInstr(121,243)]);
(580, [EatInstr(114,590)]);
(197, [EatInstr(116,244)]);
(581, [AAction2Instr(__a61,232)]);
(198, [EatInstr(108,245)]);
(582, [CompleteInstr(271)]);
(199, [AAction2Instr(__a10,582)]);
(583, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,591)]);
(200, [EatInstr(108,246)]);
(584, [EatInstr(115,592)]);
(201, [EatInstr(116,247)]);
(585, [EatInstr(100,593)]);
(202, [CompleteInstr(272)]);
(586, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,594)]);
(203, [AAction2Instr(__a1,248)]);
(587, [EatInstr(99,595)]);
(204, [EatInstr(99,249)]);
(588, [EatInstr(116,596)]);
(205, [AAction2Instr(__a11,202)]);
(589, [AAction2Instr(__a62,232)]);
(206, [AAction2Instr(__a12,202)]);
(590, [EatInstr(121,597)]);
(207, [EatInstr(104,250)]);
(591, [AAction2Instr(__a63,202)]);
(208, [EatInstr(111,251)]);
(592, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,598)]);
(209, [EatInstr(100,252)]);
(593, [EatInstr(105,599)]);
(210, [EatInstr(45,254);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,253)]);
(594, [AAction2Instr(__a64,202)]);
(211, [AAction2Instr(__a13,202)]);
(595, [EatInstr(97,600)]);
(212, [EatInstr(45,257)]);
(596, [AAction2Instr(__a65,563)]);
(213, [EatInstr(108,258)]);
(597, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,601)]);
(214, [EatInstr(114,259)]);
(598, [AAction2Instr(__a66,202)]);
(215, [EatInstr(119,260)]);
(599, [EatInstr(99,602)]);
(216, [EatInstr(101,261)]);
(600, [EatInstr(110,603)]);
(217, [EatInstr(45,262)]);
(601, [AAction2Instr(__a67,232)]);
(218, [EatInstr(107,263)]);
(602, [EatInstr(97,604)]);
(219, [EatInstr(116,264)]);
(603, [EatInstr(110,605)]);
(220, [EatInstr(110,265)]);
(604, [EatInstr(116,606)]);
(221, [EatInstr(97,266)]);
(605, [EatInstr(101,607)]);
(222, [EatInstr(105,267)]);
(606, [EatInstr(101,608)]);
(223, [EatInstr(111,268)]);
(607, [EatInstr(114,609)]);
(224, [EatInstr(101,269)]);
(608, [EatInstr(115,610)]);
(225, [EatInstr(101,270)]);
(609, [EatInstr(108,611)]);
(226, [EatInstr(107,271)]);
(610, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,612)]);
(227, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,272)]);
(611, [EatInstr(101,613)]);
(228, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,273)]);
(612, [AAction2Instr(__a68,202)]);
(229, [EatInstr(45,274)]);
(613, [EatInstr(115,614)]);
(230, [EatInstr(108,275)]);
(614, [EatInstr(115,615)]);
(231, [EatInstr(102,276)]);
(615, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,616)]);
(232, [CompleteInstr(273)]);
(616, [AAction2Instr(__a69,202)]);
(233, [EatInstr(110,277)]);
(234, [EatInstr(117,278)]);
(235, [EatInstr(117,279)]);
(236, [EatInstr(108,280)]);
(237, [EatInstr(114,281)]);
(238, [EatInstr(116,283)]);
(239, [EatInstr(45,284)]);
(240, [AAction2Instr(__a14,582)]);
(241, [AAction2Instr(__a15,582)]);
(242, [EatInstr(101,285)]);
(243, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,286)]);
(244, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,287)]);
(245, [EatInstr(45,288)]);
(246, [EatInstr(101,289)]);
(247, [EatInstr(99,290)]);
(248, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,291)]);
(249, [EatInstr(116,292)]);
(250, [EatInstr(101,293)]);
(251, [EatInstr(111,294)]);
(252, [EatInstr(101,295)]);
(253, [AAction2Instr(__a16,202)]);
(254, [EatInstr(114,298);EatInstr(110,297);EatInstr(103,296)]);
(255, [AAction2Instr(__a1,256);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,255)]);
(256, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,299)]);
(257, [EatInstr(108,300)]);
(258, [EatInstr(97,301)]);
(259, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,302)]);
(260, [EatInstr(45,303)]);
(261, [EatInstr(110,304)]);
(262, [EatInstr(105,305)]);
(263, [EatInstr(45,306)]);
(264, [EatInstr(101,307)]);
(265, [EatInstr(101,308)]);
(266, [EatInstr(104,309)]);
(267, [EatInstr(122,310)]);
(268, [EatInstr(97,311)]);
(269, [EatInstr(109,312)]);
(270, [EatInstr(112,313)]);
(271, [EatInstr(105,314)]);
(272, [AAction2Instr(__a17,232)]);
(273, [AAction2Instr(__a1,315)]);
(274, [EatInstr(104,316)]);
(275, [EatInstr(108,317)]);
(276, [EatInstr(115,318)]);
(277, [EatInstr(111,319)]);
(278, [EatInstr(116,320)]);
(279, [EatInstr(110,321)]);
(280, [EatInstr(101,322)]);
(281, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,323)]);
(283, [EatInstr(121,324)]);
(284, [EatInstr(114,325)]);
(285, [EatInstr(110,326)]);
(286, [AAction2Instr(__a18,582)]);
(287, [AAction2Instr(__a19,582)]);
(288, [EatInstr(115,327)]);
(289, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,328)]);
(290, [EatInstr(104,329)]);
(291, [AAction2Instr(__a20,330)]);
(292, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,331)]);
(293, [EatInstr(97,332)]);
(294, [EatInstr(107,333)]);
(295, [EatInstr(110,334)]);
(296, [EatInstr(105,335)]);
(297, [EatInstr(117,337);EatInstr(112,336)]);
(298, [EatInstr(101,338)]);
(299, [AAction2Instr(__a21,202)]);
(300, [EatInstr(97,339)]);
(301, [EatInstr(116,340)]);
(302, [AContInstr3(271,__g0,__binder4,341);ACallInstr3(__g0,8)]);
(303, [EatInstr(110,342)]);
(304, [EatInstr(100,343)]);
(305, [EatInstr(110,344)]);
(306, [EatInstr(108,345)]);
(307, [EatInstr(114,346)]);
(308, [EatInstr(45,347)]);
(309, [EatInstr(101,348)]);
(310, [EatInstr(101,349)]);
(311, [EatInstr(108,350)]);
(312, [EatInstr(111,351)]);
(313, [EatInstr(108,352)]);
(314, [EatInstr(112,353)]);
(315, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,354)]);
(316, [EatInstr(105,355)]);
(317, [EatInstr(45,356)]);
(318, [EatInstr(116,358);EatInstr(109,357)]);
(319, [EatInstr(116,359)]);
(320, [EatInstr(101,360)]);
(321, [EatInstr(100,361)]);
(322, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,362)]);
(323, [AAction2Instr(__a22,582)]);
(324, [EatInstr(112,363)]);
(325, [EatInstr(101,364)]);
(326, [EatInstr(99,365)]);
(327, [EatInstr(116,366)]);
(328, [AAction2Instr(__a23,202)]);
(329, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,367)]);
(330, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,368)]);
(331, [AAction2Instr(__a24,202)]);
(332, [EatInstr(100,369)]);
(333, [EatInstr(97,370)]);
(334, [EatInstr(99,371)]);
(335, [EatInstr(108,372)]);
(336, [EatInstr(114,373)]);
(337, [EatInstr(108,374)]);
(338, [EatInstr(108,375)]);
(339, [EatInstr(116,376)]);
(340, [EatInstr(101,377)]);
(341, [AAction2Instr(__a25,232)]);
(342, [EatInstr(111,378)]);
(343, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,379)]);
(344, [EatInstr(115,380)]);
(345, [EatInstr(97,381)]);
(346, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,382)]);
(347, [EatInstr(114,384);EatInstr(99,383)]);
(348, [EatInstr(97,385)]);
(349, [EatInstr(45,386)]);
(350, [EatInstr(101,387)]);
(351, [EatInstr(105,388)]);
(352, [EatInstr(97,389)]);
(353, [EatInstr(45,390)]);
(354, [AAction2Instr(__a1,391)]);
(355, [EatInstr(115,392)]);
(356, [EatInstr(115,393)]);
(357, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,394)]);
(358, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,395)]);
(359, [EatInstr(97,396)]);
(360, [EatInstr(115,397)]);
(361, [EatInstr(101,398)]);
(362, [AAction2Instr(__a26,582)]);
(363, [EatInstr(101,399)]);
(364, [EatInstr(103,400)]);
(365, [EatInstr(101,401)]);
(366, [EatInstr(97,402)]);
(367, [AAction2Instr(__a27,202)]);
(368, [AAction2Instr(__a28,202);AAction2Instr(__a1,403)]);
(369, [EatInstr(45,404)]);
(370, [EatInstr(104,405)]);
(371, [EatInstr(101,406)]);
(372, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,407)]);
(373, [EatInstr(101,408)]);
(374, [EatInstr(108,409)]);
(375, [EatInstr(101,410)]);
(376, [EatInstr(101,411)]);
(377, [EatInstr(45,412)]);
(378, [EatInstr(116,413)]);
(379, [EatInstr(119,417);EatInstr(116,416);EatInstr(112,415);EatInstr(102,414)]);
(380, [EatInstr(101,418)]);
(381, [EatInstr(98,419)]);
(382, [AAction2Instr(__a1,420)]);
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
