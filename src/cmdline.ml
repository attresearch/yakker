
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
 | _ -> raise Exit)
and
 _r_command(_n,ykinput) = (match _n() with
 | (2017) -> ((let p = _r_phases(_n,ykinput) in  (match p with
                                               Inline_regular_cmd -> Compileopt.inline_regular := true
                                             | Unroll_star_cmd -> if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1
                                             | _ -> ());
                                            p ))
 | (2018) -> ((); ();  Compile_cmd )
 | (2019) -> ((); ();  Dispatch_cmd )
 | (2020) -> ((); ();  Dot_cmd )
 | (2021) -> ((); (); (let _x4 = _n() in (); (let _x3 = _n() in (let f = Yak.YkBuf.get_string _x4 _x3 ykinput in (let l = (let _x8 = (let rec _x19 _x8 =
(match _n() with (2023) -> _x8 | _ (*2022*) ->
 _x19((let _x7 = (); (let _x6 = _n() in (); (let _x5 = _n() in (let x = Yak.YkBuf.get_string _x6 _x5 ykinput in x))) in _x7::_x8)))
in _x19(Yak.Util.nil)) in (List.rev _x8)) in ();  files := f::!files; exec_l := l; Exec_cmd )))))
 | (2024) -> ((); ();  Extract_cmd )
 | (2025) -> ((); ();  Compileopt.coalesce := true; Fuse_cmd )
 | (2026) -> ((); ();  Info_cmd )
 | (2027) -> ((); ();  Lookahead_analysis_cmd )
 | (2028) -> ((); ();  Lr1_lookahead_cmd )
 | (2029) -> ((); ();  Precedence_analysis_cmd )
 | (2030) -> ((); ();  Print_gul_cmd )
 | (2031) -> ((); ();  Print_gil_cmd )
 | (2032) -> ((); ();  Print_npreds_cmd )
 | (2033) -> ((); ();  Print_npreds_cmd )
 | (2034) -> ((); ();  Print_relevance_cmd )
 | (2035) -> ((); (let _x10 = _n() in (); (let _x9 = _n() in (let n = Yak.YkBuf.get_string _x10 _x9 ykinput in ();  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" ))))
 | (2036) -> ((); ();  Sort_cmd )
 | (2037) -> ((); ();  Strip_late_actions_cmd )
 | (2038) -> ((); ();  Translate_dypgen_cmd )
 | (2039) -> ((); ();  Translate_dypgen_scannerless_cmd )
 | _ -> raise Exit)
and
 _r_args(_n,ykinput) = (match _n() with
 | (2040) -> ((); (); (let p = _r_phases(_n,ykinput) in  after := Some p ; ()))
 | (2041) -> ((); ();  Compileopt.use_coroutines := false ; ())
 | (2042) -> ((); (); (let b = (match _n() with
 | (2043) -> ((); Fun_BE)
 | (2044) -> ((); Trans_BE)
 | (2045) -> ((); Wadler_BE)
 | (2046) -> ((); Peg_BE false)
 | (2047) -> ((); Peg_BE true)
 | _ -> raise Exit) in ();  backend := b ; ()))
 | (2048) -> ((); ();  Compileopt.case_sensitive := false ; ())
 | (2049) -> ((); ();  Compileopt.check_labels := true ; ())
 | (2050) -> ((); (); (let _x12 = _n() in (); (let _x11 = _n() in (let n = Yak.YkBuf.get_string _x12 _x11 ykinput in ();  Variables.counter := (int_of_string n) ; ()))))
 | (2051) -> ((); ();  Compileopt.inline_cs := true ; ())
 | (2052) -> ((); ();  Compileopt.inline_regular := true ; ())
 | (2053) -> ((); ();  Compileopt.memoize_history := true ; ())
 | (2054) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2055) -> ((); ();  Compileopt.unit_history := true ; ())
 | (2056) -> ((); ();  Compileopt.skip_opt := false ; ())
 | (2057) -> ((); ();  Compileopt.repress_replay := true ; ())
 | (2058) -> ((); ();  Compileopt.lookahead := true ; ())
 | (2059) -> ((); ();  Compileopt.use_fsm := true ; ())
 | (2060) -> ((); ();  Compileopt.use_fsm := false ; ())
 | (2061) -> ((); ();  Compileopt.coalesce := false ; ())
 | (2062) -> ((); ();  only := true ; ())
 | (2063) -> ((); (); (let _x14 = _n() in (); (let _x13 = _n() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2064) -> ((); (); (let _x16 = _n() in (); (let _x15 = _n() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  Compileopt.unroll_star_n := (int_of_string n) ; ()))))
 | (2065) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2066) -> ((let _x18 = _n() in (); (let _x17 = _n() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
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
 | _ -> raise Exit)
and _rv_command() = (match _n() with
 | (2017) -> (();_rv_phases(); push((2017)))
 | (2018) -> (();();(); push((2018)))
 | (2019) -> (();();(); push((2019)))
 | (2020) -> (();();(); push((2020)))
 | (2021) -> (();();();push((2023)); while (match _n() with (2022) -> true | _ (*2023*)-> false) do
 ();();();push(_n());();push(_n());(); push((2022))
done
;();push(_n());();push(_n());();(); push((2021)))
 | (2024) -> (();();(); push((2024)))
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
 | (2035) -> (();();();push(_n());();push(_n());(); push((2035)))
 | (2036) -> (();();(); push((2036)))
 | (2037) -> (();();(); push((2037)))
 | (2038) -> (();();(); push((2038)))
 | (2039) -> (();();(); push((2039)))
 | _ -> raise Exit)
and _rv_args() = (match _n() with
 | (2040) -> (();();_rv_phases();();(); push((2040)))
 | (2041) -> (();();();(); push((2041)))
 | (2042) -> (();();();(match _n() with
 | (2043) -> (();(); push((2043)))
 | (2044) -> (();(); push((2044)))
 | (2045) -> (();(); push((2045)))
 | (2046) -> (();(); push((2046)))
 | (2047) -> (();(); push((2047)))
 | _ -> raise Exit);();(); push((2042)))
 | (2048) -> (();();();(); push((2048)))
 | (2049) -> (();();();(); push((2049)))
 | (2050) -> (();();();();push(_n());();push(_n());();(); push((2050)))
 | (2051) -> (();();();(); push((2051)))
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
 | (2063) -> (();();();();push(_n());();push(_n());();(); push((2063)))
 | (2064) -> (();();();();push(_n());();push(_n());();(); push((2064)))
 | (2065) -> (();();();(); push((2065)))
 | (2066) -> (();();();();push(_n());();push(_n()); push((2066)))
 | _ -> raise Exit)
in
object (self)
method next() = (match !s with hd::tl -> (s := tl; hd) | _ -> raise Not_found)
initializer _rv_cmd_line_args()
end

let _replay_cmd_line_args ykinput h =
  let _o = new rvs (h#rtl) in
  let _n() = _o#next() in
  _r_cmd_line_args(_n,ykinput)(* History constructors *)let _e p h = h#empty p
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
let __a46 = (_p(2061));;
let __a15 = (_p(2012));;
let __a48 = (_p(2028));;
let __a60 = (_p(2064));;
let __a40 = (_p(2015));;
let __a32 = (_p(2002));;
let __a36 = (_p(2051));;
let __a22 = (_p(2018));;
let __a66 = (_p(2054));;
let __a21 = (_p(2005));;
let __a55 = (_p(2041));;
let __a38 = (_p(2057));;
let __a39 = (_p(2008));;
let __a35 = (_p(2044));;
let __a30 = (_p(2031));;
let __a42 = (_p(2047));;
let __a54 = (_p(2034));;
let __a27 = (_p(2021));;
let __a63 = (_p(2037));;
let __a23 = (_p(2024));;
let __a29 = (_p(2060));;
let __a9 = (_p(2011));;
let __a62 = (_p(2027));;
let __a31 = (_p(2063));;
let __a18 = (_p(2014));;
let __a45 = (_p(2050));;
let __a2 = (_p(2001));;
let __a7 = (_p(2066));;
let __a3 = (_p(2017));;
let __a19 = (fun _x0_ _x1_ -> (((_p(2023)) _x0_) (((_p_pos) _x0_) _x1_)));;
let __a59 = (_p(2053));;
let __a25 = (_p(2004));;
let __a24 = (_p(2040));;
let __a51 = (_p(2007));;
let __a47 = (_p(2056));;
let __a41 = (_p(2043));;
let __a28 = (_p(2059));;
let __a16 = (_p(2030));;
let __a64 = (_p(2046));;
let __a67 = (_p(2033));;
let __a49 = (_p(2049));;
let __a6 = (_p(2020));;
let __a13 = (_p(2036));;
let __a68 = (_p(2039));;
let __a14 = (_p(2010));;
let __a12 = (_p(2026));;
let __a33 = (_p(2013));;
let __a17 = (_p(2062));;
let __a65 = (_p(2029));;
let __a4 = (_p(2000));;
let __a10 = (_p(2016));;
let __a5 = (_p(2065));;
let __a57 = (_p(2003));;
let __a56 = (_p(2052));;
let __a26 = (_p(2019));;
let __a1 = (_p_pos);;
let __a50 = (_p(2055));;
let __a8 = (_p(2006));;
let __a44 = (_p(2042));;
let __a52 = (_p(2009));;
let __a37 = (_p(2058));;
let __g0 = (_e);;
let __a53 = (_p(2045));;
let __a34 = (fun _x0_ _x1_ -> (((_p(2022)) _x0_) (((_p_pos) _x0_) _x1_)));;
let __a43 = (_p(2032));;
let __a61 = (_p(2048));;
let __a20 = (_p(2035));;
let __a58 = (_p(2038));;
let __a11 = (_p(2025));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1002);;
let __binder2 = (_m 1093);;
let __binder3 = (_m 1011);;
let __binder4 = (_m 1236);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(111,421)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,422)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(116,423)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(116,424)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [AAction2Instr(__a28,227)]);
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [AAction2Instr(__a29,227)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(116,425)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,42)]);
(390, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,426)]);
(7, [EatInstr(127,44);EatInstr(126,44);EatInstr(125,44);EatInstr(124,44);EatInstr(123,44);EatInstr(122,44);EatInstr(121,44);EatInstr(120,44);EatInstr(119,44);EatInstr(118,44);EatInstr(117,44);EatInstr(116,44);EatInstr(115,44);EatInstr(114,44);EatInstr(113,44);EatInstr(112,44);EatInstr(111,44);EatInstr(110,44);EatInstr(109,44);EatInstr(108,44);EatInstr(107,44);EatInstr(106,44);EatInstr(105,44);EatInstr(104,44);EatInstr(103,44);EatInstr(102,44);EatInstr(101,44);EatInstr(100,44);EatInstr(99,44);EatInstr(98,44);EatInstr(97,44);EatInstr(96,44);EatInstr(95,44);EatInstr(94,44);EatInstr(93,44);EatInstr(92,44);EatInstr(91,44);EatInstr(90,44);EatInstr(89,44);EatInstr(88,44);EatInstr(87,44);EatInstr(86,44);EatInstr(85,44);EatInstr(84,44);EatInstr(83,44);EatInstr(82,44);EatInstr(81,44);EatInstr(80,44);EatInstr(79,44);EatInstr(78,44);EatInstr(77,44);EatInstr(76,44);EatInstr(75,44);EatInstr(74,44);EatInstr(73,44);EatInstr(72,44);EatInstr(71,44);EatInstr(70,44);EatInstr(69,44);EatInstr(68,44);EatInstr(67,44);EatInstr(66,44);EatInstr(65,44);EatInstr(64,44);EatInstr(63,44);EatInstr(62,44);EatInstr(61,44);EatInstr(60,44);EatInstr(59,44);EatInstr(58,44);EatInstr(57,44);EatInstr(56,44);EatInstr(55,44);EatInstr(54,44);EatInstr(53,44);EatInstr(52,44);EatInstr(51,44);EatInstr(50,44);EatInstr(49,44);EatInstr(48,44);EatInstr(47,44);EatInstr(46,44);EatInstr(44,44);EatInstr(43,44);EatInstr(42,44);EatInstr(41,44);EatInstr(40,44);EatInstr(39,44);EatInstr(38,44);EatInstr(37,44);EatInstr(36,44);EatInstr(35,44);EatInstr(34,44);EatInstr(33,44);EatInstr(32,44);EatInstr(31,44);EatInstr(30,44);EatInstr(29,44);EatInstr(28,44);EatInstr(27,44);EatInstr(26,44);EatInstr(25,44);EatInstr(24,44);EatInstr(23,44);EatInstr(22,44);EatInstr(21,44);EatInstr(20,44);EatInstr(19,44);EatInstr(18,44);EatInstr(17,44);EatInstr(16,44);EatInstr(15,44);EatInstr(14,44);EatInstr(13,44);EatInstr(12,44);EatInstr(11,44);EatInstr(10,44);EatInstr(9,44);EatInstr(8,44);EatInstr(7,44);EatInstr(6,44);EatInstr(5,44);EatInstr(4,44);EatInstr(3,44);EatInstr(2,44);EatInstr(1,44)]);
(391, [EatInstr(114,427)]);
(8, [EatInstr(119,27);EatInstr(117,26);EatInstr(115,25);EatInstr(112,24);EatInstr(109,23);EatInstr(108,22);EatInstr(105,21);EatInstr(104,20);EatInstr(100,19);EatInstr(99,18);EatInstr(97,17)]);
(392, [EatInstr(115,428)]);
(9, [EatInstr(116,37);EatInstr(115,36);EatInstr(114,35);EatInstr(112,34);EatInstr(108,33);EatInstr(105,32);EatInstr(102,31);EatInstr(101,30);EatInstr(100,29);EatInstr(99,28);AContInstr3(271,__g0,__binder2,38);ACallInstr3(__g0,8)]);
(393, [EatInstr(117,429)]);
(10, [EatInstr(45,39);AAction2Instr(__a1,40)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),41)]);
(394, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,430)]);
(395, [EatInstr(114,431)]);
(12, [CompleteInstr(264)]);
(396, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,432)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(97,433)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(101,434)]);
(15, [AAction2Instr(__a2,133)]);
(399, [EatInstr(45,435)]);
(16, [CompleteInstr(268)]);
(400, [AAction2Instr(__a30,197)]);
(17, [EatInstr(116,47);EatInstr(114,46)]);
(401, [EatInstr(100,436)]);
(18, [EatInstr(111,49);EatInstr(108,48)]);
(402, [EatInstr(97,437)]);
(19, [EatInstr(101,50)]);
(403, [EatInstr(118,438)]);
(20, [EatInstr(97,51)]);
(404, [EatInstr(45,439)]);
(21, [EatInstr(110,52)]);
(405, [EatInstr(100,440)]);
(22, [EatInstr(105,54);EatInstr(101,53)]);
(406, [EatInstr(97,441)]);
(23, [EatInstr(105,55)]);
(407, [EatInstr(117,442)]);
(24, [EatInstr(114,56)]);
(408, [EatInstr(101,443)]);
(25, [EatInstr(117,57)]);
(409, [EatInstr(120,444)]);
(26, [EatInstr(110,58)]);
(410, [EatInstr(97,445)]);
(27, [EatInstr(114,59)]);
(411, [EatInstr(110,446)]);
(28, [EatInstr(111,60)]);
(412, [EatInstr(101,447)]);
(29, [EatInstr(111,62);EatInstr(105,61)]);
(413, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,477)]);
(30, [EatInstr(120,63)]);
(414, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,448)]);
(31, [EatInstr(117,64)]);
(415, [EatInstr(103,449)]);
(32, [EatInstr(110,65)]);
(416, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,450)]);
(33, [EatInstr(114,67);EatInstr(111,66)]);
(417, [EatInstr(105,451)]);
(34, [EatInstr(114,68)]);
(418, [EatInstr(99,452)]);
(35, [EatInstr(102,69)]);
(419, [EatInstr(101,453)]);
(36, [EatInstr(116,71);EatInstr(111,70)]);
(420, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,454)]);
(37, [EatInstr(114,72)]);
(421, [EatInstr(112,455)]);
(38, [AAction2Instr(__a3,197)]);
(422, [AAction2Instr(__a31,227)]);
(39, [EatInstr(118,83);EatInstr(117,82);EatInstr(114,81);EatInstr(111,80);EatInstr(110,79);EatInstr(109,78);EatInstr(108,77);EatInstr(105,76);EatInstr(99,75);EatInstr(98,74);EatInstr(97,73)]);
(423, [EatInstr(111,456)]);
(40, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,84)]);
(424, [EatInstr(97,457)]);
(41, [CompleteInstr(274)]);
(425, [EatInstr(105,458)]);
(42, [ALookaheadInstr(false,CfgLA (1,264),43);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,42)]);
(426, [AAction2Instr(__a32,574)]);
(43, [CompleteInstr(269)]);
(427, [EatInstr(45,459)]);
(44, [ALookaheadInstr(false,CfgLA (1,264),45);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,44)]);
(428, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,460)]);
(45, [CompleteInstr(270)]);
(429, [EatInstr(108,461)]);
(46, [EatInstr(114,87)]);
(430, [AAction2Instr(__a33,574)]);
(47, [EatInstr(116,88)]);
(431, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,462)]);
(48, [EatInstr(111,89)]);
(432, [AAction2Instr(__a34,323)]);
(49, [EatInstr(112,90)]);
(433, [EatInstr(110,463)]);
(50, [EatInstr(115,91)]);
(434, [EatInstr(97,464)]);
(51, [EatInstr(115,92)]);
(435, [EatInstr(97,465)]);
(52, [EatInstr(108,94);EatInstr(102,93)]);
(436, [EatInstr(115,466)]);
(53, [EatInstr(120,95)]);
(437, [EatInstr(98,467)]);
(54, [EatInstr(102,96)]);
(438, [EatInstr(97,468)]);
(55, [EatInstr(110,97)]);
(439, [EatInstr(97,469)]);
(56, [EatInstr(101,98)]);
(440, [EatInstr(121,470)]);
(57, [EatInstr(98,99)]);
(441, [EatInstr(116,471)]);
(58, [EatInstr(114,100)]);
(442, [EatInstr(110,472)]);
(59, [EatInstr(97,101)]);
(443, [EatInstr(103,473)]);
(60, [EatInstr(109,102)]);
(444, [AAction2Instr(__a35,593)]);
(61, [EatInstr(115,103)]);
(445, [EatInstr(100,474)]);
(62, [EatInstr(116,104)]);
(446, [EatInstr(115,475)]);
(63, [EatInstr(116,106);EatInstr(101,105)]);
(447, [EatInstr(108,476)]);
(64, [EatInstr(115,107)]);
(448, [AAction2Instr(__a36,227)]);
(65, [EatInstr(102,108)]);
(449, [EatInstr(117,479)]);
(66, [EatInstr(111,109)]);
(450, [AAction2Instr(__a37,227)]);
(67, [EatInstr(49,110)]);
(451, [EatInstr(115,480)]);
(68, [EatInstr(105,112);EatInstr(101,111)]);
(452, [EatInstr(101,481)]);
(69, [EatInstr(99,113)]);
(453, [EatInstr(45,482)]);
(70, [EatInstr(114,114)]);
(454, [AAction2Instr(__a38,227)]);
(71, [EatInstr(114,115)]);
(455, [EatInstr(116,483)]);
(72, [EatInstr(97,116)]);
(456, [EatInstr(114,484)]);
(73, [EatInstr(114,118);EatInstr(102,117)]);
(457, [EatInstr(114,485)]);
(74, [EatInstr(97,119)]);
(458, [EatInstr(111,486)]);
(75, [EatInstr(111,122);EatInstr(104,121);EatInstr(97,120)]);
(459, [EatInstr(99,487)]);
(76, [EatInstr(110,123)]);
(460, [AAction2Instr(__a39,574)]);
(77, [EatInstr(111,124)]);
(461, [EatInstr(97,488)]);
(78, [EatInstr(101,125)]);
(462, [AAction2Instr(__a40,574)]);
(79, [EatInstr(111,126)]);
(463, [EatInstr(97,489)]);
(80, [EatInstr(110,127)]);
(464, [EatInstr(100,490)]);
(81, [EatInstr(111,128)]);
(465, [EatInstr(110,491)]);
(82, [EatInstr(115,130);EatInstr(110,129)]);
(466, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,492)]);
(83, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,131)]);
(467, [EatInstr(108,493)]);
(84, [AAction2Instr(__a1,132)]);
(468, [EatInstr(110,494)]);
(85, [AAction2Instr(__a4,133)]);
(469, [EatInstr(99,495)]);
(86, [CompleteInstr(267)]);
(470, [EatInstr(112,496)]);
(87, [EatInstr(111,134)]);
(471, [EatInstr(105,497)]);
(88, [EatInstr(114,135)]);
(472, [AAction2Instr(__a41,593)]);
(89, [EatInstr(115,136)]);
(473, [EatInstr(45,498);AAction2Instr(__a42,593)]);
(90, [EatInstr(121,137)]);
(474, [EatInstr(108,500)]);
(91, [EatInstr(117,138)]);
(475, [EatInstr(105,501)]);
(92, [EatInstr(104,139)]);
(476, [EatInstr(115,502)]);
(93, [EatInstr(101,140)]);
(477, [AAction2Instr(__a1,478);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,477)]);
(94, [EatInstr(105,141)]);
(478, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,503)]);
(95, [EatInstr(101,142)]);
(479, [EatInstr(108,504)]);
(96, [EatInstr(116,143)]);
(480, [EatInstr(116,505)]);
(97, [EatInstr(117,144)]);
(481, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,506)]);
(98, [EatInstr(99,145)]);
(482, [EatInstr(104,507)]);
(99, [EatInstr(115,146)]);
(483, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(100, [EatInstr(111,147)]);
(484, [EatInstr(121,509)]);
(101, [EatInstr(112,148)]);
(485, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,510)]);
(102, [EatInstr(112,149)]);
(486, [EatInstr(110,511)]);
(103, [EatInstr(112,150)]);
(487, [EatInstr(111,512)]);
(104, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,151)]);
(488, [EatInstr(114,513)]);
(105, [EatInstr(99,152)]);
(489, [EatInstr(108,514)]);
(106, [EatInstr(114,153)]);
(490, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,515)]);
(107, [EatInstr(101,154)]);
(491, [EatInstr(97,516)]);
(108, [EatInstr(111,155)]);
(492, [AAction2Instr(__a43,197)]);
(109, [EatInstr(107,156)]);
(493, [EatInstr(101,517)]);
(110, [EatInstr(45,157)]);
(494, [EatInstr(99,518)]);
(111, [EatInstr(99,158)]);
(495, [EatInstr(116,519)]);
(112, [EatInstr(110,159)]);
(496, [EatInstr(103,520)]);
(113, [AAction2Instr(__a1,160)]);
(497, [EatInstr(111,521)]);
(114, [EatInstr(116,161)]);
(498, [EatInstr(115,522)]);
(115, [EatInstr(105,162)]);
(499, [AAction2Instr(__a44,227)]);
(116, [EatInstr(110,163)]);
(500, [EatInstr(101,523)]);
(117, [EatInstr(116,164)]);
(501, [EatInstr(116,524)]);
(118, [EatInstr(114,165)]);
(502, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(119, [EatInstr(99,166)]);
(503, [AAction2Instr(__a45,227)]);
(120, [EatInstr(115,167)]);
(504, [EatInstr(97,526)]);
(121, [EatInstr(101,168)]);
(505, [EatInstr(111,527)]);
(122, [EatInstr(117,169)]);
(506, [AAction2Instr(__a46,227)]);
(123, [EatInstr(108,170)]);
(507, [EatInstr(105,528)]);
(124, [EatInstr(111,171)]);
(508, [AAction2Instr(__a47,227)]);
(125, [EatInstr(109,172)]);
(509, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,529)]);
(126, [EatInstr(45,173)]);
(510, [AAction2Instr(__a1,530)]);
(127, [EatInstr(108,174)]);
(511, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,531)]);
(128, [EatInstr(111,175)]);
(512, [EatInstr(114,532)]);
(129, [EatInstr(114,177);EatInstr(105,176)]);
(513, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,533)]);
(130, [EatInstr(101,178)]);
(514, [EatInstr(121,534)]);
(131, [AAction2Instr(__a5,227)]);
(515, [AAction2Instr(__a48,197)]);
(132, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,179)]);
(516, [EatInstr(108,535)]);
(133, [ALookaheadInstr(false,CfgLA (3,266),86);ACallInstr3(__default_call,11);AContInstr3(273,__g0,__binder3,85);ACallInstr3(__g0,10);ASimpleCont2Instr(274,__binder0,86)]);
(517, [EatInstr(45,536)]);
(134, [EatInstr(119,180)]);
(518, [EatInstr(101,537)]);
(135, [EatInstr(105,181)]);
(519, [EatInstr(105,538)]);
(136, [EatInstr(101,182)]);
(520, [EatInstr(101,539)]);
(137, [EatInstr(114,183)]);
(521, [EatInstr(110,540)]);
(138, [EatInstr(103,184)]);
(522, [EatInstr(116,541)]);
(139, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,185)]);
(523, [EatInstr(114,542)]);
(140, [EatInstr(114,186)]);
(524, [EatInstr(105,543)]);
(141, [EatInstr(110,187)]);
(525, [AAction2Instr(__a49,227)]);
(142, [EatInstr(114,188)]);
(526, [EatInstr(114,544)]);
(143, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,189)]);
(527, [EatInstr(114,545)]);
(144, [EatInstr(115,190)]);
(528, [EatInstr(115,546)]);
(145, [EatInstr(101,191)]);
(529, [AAction2Instr(__a50,227)]);
(146, [EatInstr(101,192)]);
(530, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,560)]);
(147, [EatInstr(108,193)]);
(531, [AAction2Instr(__a51,574)]);
(148, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,194)]);
(532, [EatInstr(101,547)]);
(149, [EatInstr(105,195)]);
(533, [AAction2Instr(__a52,574)]);
(150, [EatInstr(97,196)]);
(534, [EatInstr(115,548)]);
(151, [AAction2Instr(__a6,197)]);
(535, [EatInstr(121,549)]);
(152, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,198)]);
(536, [EatInstr(112,550)]);
(153, [EatInstr(97,199)]);
(537, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,551)]);
(154, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,200)]);
(538, [EatInstr(111,552)]);
(155, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,201)]);
(539, [EatInstr(110,553)]);
(156, [EatInstr(97,202)]);
(540, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,554)]);
(157, [EatInstr(108,203)]);
(541, [EatInstr(114,555)]);
(158, [EatInstr(101,204)]);
(542, [AAction2Instr(__a53,593)]);
(159, [EatInstr(116,205)]);
(543, [EatInstr(118,556)]);
(160, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,249)]);
(544, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,557)]);
(161, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,206)]);
(545, [EatInstr(121,558)]);
(162, [EatInstr(112,207)]);
(546, [EatInstr(116,559)]);
(163, [EatInstr(115,208)]);
(547, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,562)]);
(164, [EatInstr(101,209)]);
(548, [EatInstr(105,563)]);
(165, [EatInstr(111,210)]);
(549, [EatInstr(115,564)]);
(166, [EatInstr(107,211)]);
(550, [EatInstr(114,565)]);
(167, [EatInstr(101,212)]);
(551, [AAction2Instr(__a54,197)]);
(168, [EatInstr(99,213)]);
(552, [EatInstr(110,566)]);
(169, [EatInstr(110,214)]);
(553, [EatInstr(45,568);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,567)]);
(170, [EatInstr(105,215)]);
(554, [AAction2Instr(__a55,227)]);
(171, [EatInstr(107,216)]);
(555, [EatInstr(105,569)]);
(172, [EatInstr(111,217)]);
(556, [EatInstr(101,570)]);
(173, [EatInstr(115,221);EatInstr(114,220);EatInstr(109,219);EatInstr(99,218)]);
(557, [AAction2Instr(__a56,227)]);
(174, [EatInstr(121,222)]);
(558, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,571)]);
(175, [EatInstr(116,223)]);
(559, [EatInstr(111,572)]);
(176, [EatInstr(116,224)]);
(560, [AAction2Instr(__a1,561);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,560)]);
(177, [EatInstr(111,225)]);
(561, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,573)]);
(178, [EatInstr(45,226)]);
(562, [AAction2Instr(__a57,574)]);
(179, [AAction2Instr(__a7,227)]);
(563, [EatInstr(115,575)]);
(180, [EatInstr(45,228)]);
(564, [EatInstr(105,576)]);
(181, [EatInstr(98,229)]);
(565, [EatInstr(101,577)]);
(182, [EatInstr(45,230)]);
(566, [EatInstr(115,578)]);
(183, [EatInstr(117,231)]);
(567, [AAction2Instr(__a58,197)]);
(184, [EatInstr(97,232)]);
(568, [EatInstr(115,579)]);
(185, [AAction2Instr(__a8,574)]);
(569, [EatInstr(99,580)]);
(186, [EatInstr(45,233)]);
(570, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,581)]);
(187, [EatInstr(101,234)]);
(571, [AAction2Instr(__a59,227)]);
(188, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,235)]);
(572, [EatInstr(114,582)]);
(189, [AAction2Instr(__a9,574)]);
(573, [AAction2Instr(__a60,227)]);
(190, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,236)]);
(574, [CompleteInstr(271)]);
(191, [EatInstr(100,237)]);
(575, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,583)]);
(192, [EatInstr(116,238)]);
(576, [EatInstr(115,584)]);
(193, [EatInstr(108,239)]);
(577, [EatInstr(100,585)]);
(194, [AAction2Instr(__a10,574)]);
(578, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,586)]);
(195, [EatInstr(108,240)]);
(579, [EatInstr(99,587)]);
(196, [EatInstr(116,241)]);
(580, [EatInstr(116,588)]);
(197, [CompleteInstr(272)]);
(581, [AAction2Instr(__a61,227)]);
(198, [AAction2Instr(__a1,242)]);
(582, [EatInstr(121,589)]);
(199, [EatInstr(99,243)]);
(583, [AAction2Instr(__a62,197)]);
(200, [AAction2Instr(__a11,197)]);
(584, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,590)]);
(201, [AAction2Instr(__a12,197)]);
(585, [EatInstr(105,591)]);
(202, [EatInstr(104,244)]);
(586, [AAction2Instr(__a63,197)]);
(203, [EatInstr(111,245)]);
(587, [EatInstr(97,592)]);
(204, [EatInstr(100,246)]);
(588, [AAction2Instr(__a64,593)]);
(205, [EatInstr(45,248);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,247)]);
(589, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,594)]);
(206, [AAction2Instr(__a13,197)]);
(590, [AAction2Instr(__a65,197)]);
(207, [EatInstr(45,251)]);
(591, [EatInstr(99,595)]);
(208, [EatInstr(108,252)]);
(592, [EatInstr(110,596)]);
(209, [EatInstr(114,253)]);
(593, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,499)]);
(210, [EatInstr(119,254)]);
(594, [AAction2Instr(__a66,227)]);
(211, [EatInstr(101,255)]);
(595, [EatInstr(97,597)]);
(212, [EatInstr(45,256)]);
(596, [EatInstr(110,598)]);
(213, [EatInstr(107,257)]);
(597, [EatInstr(116,599)]);
(214, [EatInstr(116,258)]);
(598, [EatInstr(101,600)]);
(215, [EatInstr(110,259)]);
(599, [EatInstr(101,601)]);
(216, [EatInstr(97,260)]);
(600, [EatInstr(114,602)]);
(217, [EatInstr(105,261)]);
(601, [EatInstr(115,603)]);
(218, [EatInstr(111,262)]);
(602, [EatInstr(108,604)]);
(219, [EatInstr(101,263)]);
(603, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,605)]);
(220, [EatInstr(101,264)]);
(604, [EatInstr(101,606)]);
(221, [EatInstr(107,265)]);
(605, [AAction2Instr(__a67,197)]);
(222, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,266)]);
(606, [EatInstr(115,607)]);
(223, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,267)]);
(607, [EatInstr(115,608)]);
(224, [EatInstr(45,268)]);
(608, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,609)]);
(225, [EatInstr(108,269)]);
(609, [AAction2Instr(__a68,197)]);
(226, [EatInstr(102,270)]);
(227, [CompleteInstr(273)]);
(228, [EatInstr(110,271)]);
(229, [EatInstr(117,272)]);
(230, [EatInstr(117,273)]);
(231, [EatInstr(108,274)]);
(232, [EatInstr(114,275)]);
(233, [EatInstr(116,277)]);
(234, [EatInstr(45,278)]);
(235, [AAction2Instr(__a14,574)]);
(236, [AAction2Instr(__a15,574)]);
(237, [EatInstr(101,279)]);
(238, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,280)]);
(239, [EatInstr(45,281)]);
(240, [EatInstr(101,282)]);
(241, [EatInstr(99,283)]);
(242, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,284)]);
(243, [EatInstr(116,285)]);
(244, [EatInstr(101,286)]);
(245, [EatInstr(111,287)]);
(246, [EatInstr(101,288)]);
(247, [AAction2Instr(__a16,197)]);
(248, [EatInstr(114,291);EatInstr(110,290);EatInstr(103,289)]);
(249, [AAction2Instr(__a1,250);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,249)]);
(250, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,292)]);
(251, [EatInstr(108,293)]);
(252, [EatInstr(97,294)]);
(253, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,295)]);
(254, [EatInstr(45,296)]);
(255, [EatInstr(110,297)]);
(256, [EatInstr(105,298)]);
(257, [EatInstr(45,299)]);
(258, [EatInstr(101,300)]);
(259, [EatInstr(101,301)]);
(260, [EatInstr(104,302)]);
(261, [EatInstr(122,303)]);
(262, [EatInstr(97,304)]);
(263, [EatInstr(109,305)]);
(264, [EatInstr(112,306)]);
(265, [EatInstr(105,307)]);
(266, [AAction2Instr(__a17,227)]);
(267, [AAction2Instr(__a1,308)]);
(268, [EatInstr(104,309)]);
(269, [EatInstr(108,310)]);
(270, [EatInstr(115,311)]);
(271, [EatInstr(111,312)]);
(272, [EatInstr(116,313)]);
(273, [EatInstr(110,314)]);
(274, [EatInstr(101,315)]);
(275, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,316)]);
(277, [EatInstr(121,317)]);
(278, [EatInstr(114,318)]);
(279, [EatInstr(110,319)]);
(280, [AAction2Instr(__a18,574)]);
(281, [EatInstr(115,320)]);
(282, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,321)]);
(283, [EatInstr(104,322)]);
(284, [AAction2Instr(__a19,323)]);
(285, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,324)]);
(286, [EatInstr(97,325)]);
(287, [EatInstr(107,326)]);
(288, [EatInstr(110,327)]);
(289, [EatInstr(105,328)]);
(290, [EatInstr(117,330);EatInstr(112,329)]);
(291, [EatInstr(101,331)]);
(292, [AAction2Instr(__a20,197)]);
(293, [EatInstr(97,332)]);
(294, [EatInstr(116,333)]);
(295, [AContInstr3(271,__g0,__binder4,334);ACallInstr3(__g0,8)]);
(296, [EatInstr(110,335)]);
(297, [EatInstr(100,336)]);
(298, [EatInstr(110,337)]);
(299, [EatInstr(108,338)]);
(300, [EatInstr(114,339)]);
(301, [EatInstr(45,340)]);
(302, [EatInstr(101,341)]);
(303, [EatInstr(101,342)]);
(304, [EatInstr(108,343)]);
(305, [EatInstr(111,344)]);
(306, [EatInstr(108,345)]);
(307, [EatInstr(112,346)]);
(308, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,347)]);
(309, [EatInstr(105,348)]);
(310, [EatInstr(45,349)]);
(311, [EatInstr(116,351);EatInstr(109,350)]);
(312, [EatInstr(116,352)]);
(313, [EatInstr(101,353)]);
(314, [EatInstr(100,354)]);
(315, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,355)]);
(316, [AAction2Instr(__a21,574)]);
(317, [EatInstr(112,356)]);
(318, [EatInstr(101,357)]);
(319, [EatInstr(99,358)]);
(320, [EatInstr(116,359)]);
(321, [AAction2Instr(__a22,197)]);
(322, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,360)]);
(323, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,361)]);
(324, [AAction2Instr(__a23,197)]);
(325, [EatInstr(100,362)]);
(326, [EatInstr(97,363)]);
(327, [EatInstr(99,364)]);
(328, [EatInstr(108,365)]);
(329, [EatInstr(114,366)]);
(330, [EatInstr(108,367)]);
(331, [EatInstr(108,368)]);
(332, [EatInstr(116,369)]);
(333, [EatInstr(101,370)]);
(334, [AAction2Instr(__a24,227)]);
(335, [EatInstr(111,371)]);
(336, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,372)]);
(337, [EatInstr(115,373)]);
(338, [EatInstr(97,374)]);
(339, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,375)]);
(340, [EatInstr(114,377);EatInstr(99,376)]);
(341, [EatInstr(97,378)]);
(342, [EatInstr(45,379)]);
(343, [EatInstr(101,380)]);
(344, [EatInstr(105,381)]);
(345, [EatInstr(97,382)]);
(346, [EatInstr(45,383)]);
(347, [AAction2Instr(__a1,384)]);
(348, [EatInstr(115,385)]);
(349, [EatInstr(115,386)]);
(350, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,387)]);
(351, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,388)]);
(352, [EatInstr(97,389)]);
(353, [EatInstr(115,390)]);
(354, [EatInstr(101,391)]);
(355, [AAction2Instr(__a25,574)]);
(356, [EatInstr(101,392)]);
(357, [EatInstr(103,393)]);
(358, [EatInstr(101,394)]);
(359, [EatInstr(97,395)]);
(360, [AAction2Instr(__a26,197)]);
(361, [AAction2Instr(__a27,197);AAction2Instr(__a1,396)]);
(362, [EatInstr(45,397)]);
(363, [EatInstr(104,398)]);
(364, [EatInstr(101,399)]);
(365, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,400)]);
(366, [EatInstr(101,401)]);
(367, [EatInstr(108,402)]);
(368, [EatInstr(101,403)]);
(369, [EatInstr(101,404)]);
(370, [EatInstr(45,405)]);
(371, [EatInstr(116,406)]);
(372, [EatInstr(119,410);EatInstr(116,409);EatInstr(112,408);EatInstr(102,407)]);
(373, [EatInstr(101,411)]);
(374, [EatInstr(98,412)]);
(375, [AAction2Instr(__a1,413)]);
(376, [EatInstr(115,414)]);
(377, [EatInstr(101,415)]);
(378, [EatInstr(100,416)]);
(379, [EatInstr(104,417)]);
(380, [EatInstr(115,418)]);
(381, [EatInstr(122,419)]);
(382, [EatInstr(121,420)]);
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

let parse = Yak.Pami.Wfe.mk_parse P2__.parse _wfe_data_ sv0 (fun ykinput h -> _replay_cmd_line_args ykinput h)
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
