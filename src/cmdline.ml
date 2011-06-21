
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
 | (2054) -> ((); ();  Compileopt.lookahead := true ; ())
 | (2055) -> ((); ();  Compileopt.memoize_history := true ; ())
 | (2056) -> ((); ();  Compileopt.coalesce := false ; ())
 | (2057) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2058) -> ((); ();  Compileopt.repress_replay := true ; ())
 | (2059) -> ((); ();  Compileopt.skip_opt := false ; ())
 | (2060) -> ((); ();  only := true ; ())
 | (2061) -> ((); ();  Compileopt.postfix_history := false ; ())
 | (2062) -> ((); (); (let _x14 = _n() in (); (let _x13 = _n() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2063) -> ((); ();  Compileopt.unit_history := true ; ())
 | (2064) -> ((); (); (let _x16 = _n() in (); (let _x15 = _n() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  Compileopt.unroll_star_n := (int_of_string n) ; ()))))
 | (2065) -> ((); ();  Compileopt.use_fsm := true ; ())
 | (2066) -> ((); ();  Compileopt.use_fsm := false ; ())
 | (2067) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2068) -> ((let _x18 = _n() in (); (let _x17 = _n() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
 | _ -> raise Exit)
class ['a] rvs (labels: 'a History.enum) =
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
 | (2062) -> (();();();();push(_n());();push(_n());();(); push((2062)))
 | (2063) -> (();();();(); push((2063)))
 | (2064) -> (();();();();push(_n());();push(_n());();(); push((2064)))
 | (2065) -> (();();();(); push((2065)))
 | (2066) -> (();();();(); push((2066)))
 | (2067) -> (();();();(); push((2067)))
 | (2068) -> (();();();();push(_n());();push(_n()); push((2068)))
 | _ -> raise Exit)
in
object (self)
method next() = (match !s with hd::tl -> (s := tl; hd) | _ -> raise Not_found)
initializer _rv_cmd_line_args()
end

let _replay_cmd_line_args ykinput h =
  let _o = new rvs (h#right_to_left) in
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
let __a58 = (_p(2061));;
let __a15 = (_p(2012));;
let __a64 = (_p(2028));;
let __a62 = (_p(2064));;
let __a41 = (_p(2015));;
let __a33 = (_p(2002));;
let __a46 = (_p(2051));;
let __a5 = (_p(2067));;
let __a3 = (_p(2018));;
let __a22 = (_p(2005));;
let __a38 = (_p(2054));;
let __a25 = (_p(2041));;
let __a68 = (_p(2057));;
let __a40 = (_p(2008));;
let __a42 = (_p(2044));;
let __a16 = (_p(2031));;
let __a66 = (_p(2047));;
let __a20 = (fun _x0_ _x1_ -> (((_p(2024)) _x0_) (((_p_pos) _x0_) _x1_)));;
let __a69 = (_p(2034));;
let __a6 = (_p(2021));;
let __a13 = (_p(2037));;
let __a17 = (_p(2060));;
let __a9 = (_p(2011));;
let __a12 = (_p(2027));;
let __a51 = (_p(2063));;
let __a19 = (_p(2014));;
let __a50 = (_p(2050));;
let __a2 = (_p(2001));;
let __a30 = (_p(2066));;
let __a18 = (_p(2017));;
let __a35 = (fun _x0_ _x1_ -> (((_p(2023)) _x0_) (((_p_pos) _x0_) _x1_)));;
let __a57 = (_p(2053));;
let __a26 = (_p(2004));;
let __a70 = (_p(2040));;
let __a52 = (_p(2007));;
let __a47 = (_p(2056));;
let __a45 = (_p(2043));;
let __a67 = (_p(2030));;
let __a48 = (_p(2059));;
let __a54 = (_p(2046));;
let __a44 = (_p(2033));;
let __a63 = (_p(2049));;
let __a27 = (_p(2020));;
let __a21 = (_p(2036));;
let __a60 = (_p(2039));;
let __a14 = (_p(2010));;
let __a11 = (_p(2026));;
let __a34 = (_p(2013));;
let __a32 = (_p(2062));;
let __a49 = (_p(2029));;
let __a4 = (_p(2000));;
let __a10 = (_p(2016));;
let __a29 = (_p(2065));;
let __a59 = (_p(2003));;
let __a37 = (_p(2052));;
let __a23 = (_p(2019));;
let __a1 = (_p_pos);;
let __a7 = (_p(2068));;
let __a61 = (_p(2055));;
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
let __a65 = (_p(2038));;
let __a24 = (_p(2025));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1002);;
let __binder2 = (_m 1098);;
let __binder3 = (_m 1011);;
let __binder4 = (_m 1241);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(101,419)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(45,420)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(116,421)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(119,425);EatInstr(116,424);EatInstr(112,423);EatInstr(102,422)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(101,426)]);
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [EatInstr(98,427)]);
(5, [EatInstr(0,16)]);
(389, [AAction2Instr(__a1,428)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,43)]);
(390, [EatInstr(115,429)]);
(7, [EatInstr(127,45);EatInstr(126,45);EatInstr(125,45);EatInstr(124,45);EatInstr(123,45);EatInstr(122,45);EatInstr(121,45);EatInstr(120,45);EatInstr(119,45);EatInstr(118,45);EatInstr(117,45);EatInstr(116,45);EatInstr(115,45);EatInstr(114,45);EatInstr(113,45);EatInstr(112,45);EatInstr(111,45);EatInstr(110,45);EatInstr(109,45);EatInstr(108,45);EatInstr(107,45);EatInstr(106,45);EatInstr(105,45);EatInstr(104,45);EatInstr(103,45);EatInstr(102,45);EatInstr(101,45);EatInstr(100,45);EatInstr(99,45);EatInstr(98,45);EatInstr(97,45);EatInstr(96,45);EatInstr(95,45);EatInstr(94,45);EatInstr(93,45);EatInstr(92,45);EatInstr(91,45);EatInstr(90,45);EatInstr(89,45);EatInstr(88,45);EatInstr(87,45);EatInstr(86,45);EatInstr(85,45);EatInstr(84,45);EatInstr(83,45);EatInstr(82,45);EatInstr(81,45);EatInstr(80,45);EatInstr(79,45);EatInstr(78,45);EatInstr(77,45);EatInstr(76,45);EatInstr(75,45);EatInstr(74,45);EatInstr(73,45);EatInstr(72,45);EatInstr(71,45);EatInstr(70,45);EatInstr(69,45);EatInstr(68,45);EatInstr(67,45);EatInstr(66,45);EatInstr(65,45);EatInstr(64,45);EatInstr(63,45);EatInstr(62,45);EatInstr(61,45);EatInstr(60,45);EatInstr(59,45);EatInstr(58,45);EatInstr(57,45);EatInstr(56,45);EatInstr(55,45);EatInstr(54,45);EatInstr(53,45);EatInstr(52,45);EatInstr(51,45);EatInstr(50,45);EatInstr(49,45);EatInstr(48,45);EatInstr(47,45);EatInstr(46,45);EatInstr(44,45);EatInstr(43,45);EatInstr(42,45);EatInstr(41,45);EatInstr(40,45);EatInstr(39,45);EatInstr(38,45);EatInstr(37,45);EatInstr(36,45);EatInstr(35,45);EatInstr(34,45);EatInstr(33,45);EatInstr(32,45);EatInstr(31,45);EatInstr(30,45);EatInstr(29,45);EatInstr(28,45);EatInstr(27,45);EatInstr(26,45);EatInstr(25,45);EatInstr(24,45);EatInstr(23,45);EatInstr(22,45);EatInstr(21,45);EatInstr(20,45);EatInstr(19,45);EatInstr(18,45);EatInstr(17,45);EatInstr(16,45);EatInstr(15,45);EatInstr(14,45);EatInstr(13,45);EatInstr(12,45);EatInstr(11,45);EatInstr(10,45);EatInstr(9,45);EatInstr(8,45);EatInstr(7,45);EatInstr(6,45);EatInstr(5,45);EatInstr(4,45);EatInstr(3,45);EatInstr(2,45);EatInstr(1,45)]);
(391, [EatInstr(101,430)]);
(8, [EatInstr(119,28);EatInstr(117,27);EatInstr(115,26);EatInstr(114,25);EatInstr(112,24);EatInstr(109,23);EatInstr(108,22);EatInstr(105,21);EatInstr(104,20);EatInstr(100,19);EatInstr(99,18);EatInstr(97,17)]);
(392, [EatInstr(100,431)]);
(9, [EatInstr(116,38);EatInstr(115,37);EatInstr(114,36);EatInstr(112,35);EatInstr(108,34);EatInstr(105,33);EatInstr(102,32);EatInstr(101,31);EatInstr(100,30);EatInstr(99,29);AContInstr3(271,__g0,__binder2,39);ACallInstr3(__g0,8)]);
(393, [EatInstr(104,432)]);
(10, [EatInstr(45,40);AAction2Instr(__a1,41)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),42)]);
(394, [EatInstr(115,433)]);
(395, [EatInstr(122,434)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(121,435)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(111,436)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(105,437)]);
(15, [AAction2Instr(__a2,138)]);
(399, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,438)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(116,439)]);
(17, [EatInstr(116,48);EatInstr(114,47)]);
(401, [EatInstr(116,440)]);
(18, [EatInstr(111,50);EatInstr(108,49)]);
(402, [AAction2Instr(__a29,236)]);
(19, [EatInstr(101,51)]);
(403, [AAction2Instr(__a30,236)]);
(20, [EatInstr(97,52)]);
(404, [EatInstr(116,441)]);
(21, [EatInstr(110,53)]);
(405, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,442)]);
(22, [EatInstr(105,55);EatInstr(101,54)]);
(406, [EatInstr(114,443)]);
(23, [EatInstr(105,56)]);
(407, [EatInstr(115,444)]);
(24, [EatInstr(114,57)]);
(408, [EatInstr(117,445)]);
(25, [EatInstr(101,58)]);
(409, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,446)]);
(26, [EatInstr(117,59)]);
(410, [EatInstr(114,447)]);
(27, [EatInstr(110,60)]);
(411, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,448)]);
(28, [EatInstr(114,61)]);
(412, [EatInstr(97,449)]);
(29, [EatInstr(111,62)]);
(413, [EatInstr(101,450)]);
(30, [EatInstr(111,64);EatInstr(105,63)]);
(414, [EatInstr(45,451)]);
(31, [EatInstr(120,65)]);
(415, [AAction2Instr(__a31,205)]);
(32, [EatInstr(117,66)]);
(416, [EatInstr(100,452)]);
(33, [EatInstr(110,67)]);
(417, [EatInstr(97,453)]);
(34, [EatInstr(114,69);EatInstr(111,68)]);
(418, [EatInstr(118,454)]);
(35, [EatInstr(114,70)]);
(419, [EatInstr(45,455)]);
(36, [EatInstr(102,71)]);
(420, [EatInstr(100,456)]);
(37, [EatInstr(116,73);EatInstr(111,72)]);
(421, [EatInstr(97,457)]);
(38, [EatInstr(114,74)]);
(422, [EatInstr(117,458)]);
(39, [AAction2Instr(__a3,205)]);
(423, [EatInstr(101,459)]);
(40, [EatInstr(118,86);EatInstr(117,85);EatInstr(114,84);EatInstr(112,83);EatInstr(111,82);EatInstr(110,81);EatInstr(109,80);EatInstr(108,79);EatInstr(105,78);EatInstr(99,77);EatInstr(98,76);EatInstr(97,75)]);
(424, [EatInstr(120,460)]);
(41, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,87)]);
(425, [EatInstr(97,461)]);
(42, [CompleteInstr(274)]);
(426, [EatInstr(110,462)]);
(43, [ALookaheadInstr(false,CfgLA (1,264),44);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,43)]);
(427, [EatInstr(101,463)]);
(44, [CompleteInstr(269)]);
(428, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,494)]);
(45, [ALookaheadInstr(false,CfgLA (1,264),46);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,45)]);
(429, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,464)]);
(46, [CompleteInstr(270)]);
(430, [EatInstr(103,465)]);
(47, [EatInstr(114,90)]);
(431, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,466)]);
(48, [EatInstr(116,91)]);
(432, [EatInstr(105,467)]);
(49, [EatInstr(111,92)]);
(433, [EatInstr(99,468)]);
(50, [EatInstr(112,93)]);
(434, [EatInstr(101,469)]);
(51, [EatInstr(115,94)]);
(435, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,470)]);
(52, [EatInstr(115,95)]);
(436, [EatInstr(112,471)]);
(53, [EatInstr(108,97);EatInstr(102,96)]);
(437, [EatInstr(115,472)]);
(54, [EatInstr(120,98)]);
(438, [AAction2Instr(__a32,236)]);
(55, [EatInstr(102,99)]);
(439, [EatInstr(111,473)]);
(56, [EatInstr(110,100)]);
(440, [EatInstr(97,474)]);
(57, [EatInstr(101,101)]);
(441, [EatInstr(105,475)]);
(58, [EatInstr(112,102)]);
(442, [AAction2Instr(__a33,597)]);
(59, [EatInstr(98,103)]);
(443, [EatInstr(45,476)]);
(60, [EatInstr(114,104)]);
(444, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,477)]);
(61, [EatInstr(97,105)]);
(445, [EatInstr(108,478)]);
(62, [EatInstr(109,106)]);
(446, [AAction2Instr(__a34,597)]);
(63, [EatInstr(115,107)]);
(447, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,479)]);
(64, [EatInstr(116,108)]);
(448, [AAction2Instr(__a35,336)]);
(65, [EatInstr(116,110);EatInstr(101,109)]);
(449, [EatInstr(110,480)]);
(66, [EatInstr(115,111)]);
(450, [EatInstr(97,481)]);
(67, [EatInstr(102,112)]);
(451, [EatInstr(97,482)]);
(68, [EatInstr(111,113)]);
(452, [EatInstr(115,483)]);
(69, [EatInstr(49,114)]);
(453, [EatInstr(98,484)]);
(70, [EatInstr(105,116);EatInstr(101,115)]);
(454, [EatInstr(97,485)]);
(71, [EatInstr(99,117)]);
(455, [EatInstr(97,486)]);
(72, [EatInstr(114,118)]);
(456, [EatInstr(121,487)]);
(73, [EatInstr(114,119)]);
(457, [EatInstr(116,488)]);
(74, [EatInstr(97,120)]);
(458, [EatInstr(110,489)]);
(75, [EatInstr(114,122);EatInstr(102,121)]);
(459, [EatInstr(103,490)]);
(76, [EatInstr(97,123)]);
(460, [AAction2Instr(__a36,577)]);
(77, [EatInstr(111,126);EatInstr(104,125);EatInstr(97,124)]);
(461, [EatInstr(100,491)]);
(78, [EatInstr(110,127)]);
(462, [EatInstr(115,492)]);
(79, [EatInstr(111,128)]);
(463, [EatInstr(108,493)]);
(80, [EatInstr(101,129)]);
(464, [AAction2Instr(__a37,236)]);
(81, [EatInstr(111,130)]);
(465, [EatInstr(117,496)]);
(82, [EatInstr(110,131)]);
(466, [AAction2Instr(__a38,236)]);
(83, [EatInstr(114,132)]);
(467, [EatInstr(115,497)]);
(84, [EatInstr(111,133)]);
(468, [EatInstr(101,498)]);
(85, [EatInstr(115,135);EatInstr(110,134)]);
(469, [EatInstr(45,499)]);
(86, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,136)]);
(470, [AAction2Instr(__a39,236)]);
(87, [AAction2Instr(__a1,137)]);
(471, [EatInstr(116,500)]);
(88, [AAction2Instr(__a4,138)]);
(472, [EatInstr(116,501)]);
(89, [CompleteInstr(267)]);
(473, [EatInstr(114,502)]);
(90, [EatInstr(111,139)]);
(474, [EatInstr(114,503)]);
(91, [EatInstr(114,140)]);
(475, [EatInstr(111,504)]);
(92, [EatInstr(115,141)]);
(476, [EatInstr(99,505)]);
(93, [EatInstr(121,142)]);
(477, [AAction2Instr(__a40,597)]);
(94, [EatInstr(117,143)]);
(478, [EatInstr(97,506)]);
(95, [EatInstr(104,144)]);
(479, [AAction2Instr(__a41,597)]);
(96, [EatInstr(101,145)]);
(480, [EatInstr(97,507)]);
(97, [EatInstr(105,146)]);
(481, [EatInstr(100,508)]);
(98, [EatInstr(101,147)]);
(482, [EatInstr(110,509)]);
(99, [EatInstr(116,148)]);
(483, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,510)]);
(100, [EatInstr(117,149)]);
(484, [EatInstr(108,511)]);
(101, [EatInstr(99,150)]);
(485, [EatInstr(110,512)]);
(102, [EatInstr(108,151)]);
(486, [EatInstr(99,513)]);
(103, [EatInstr(115,152)]);
(487, [EatInstr(112,514)]);
(104, [EatInstr(111,153)]);
(488, [EatInstr(105,515)]);
(105, [EatInstr(112,154)]);
(489, [AAction2Instr(__a42,577)]);
(106, [EatInstr(112,155)]);
(490, [EatInstr(45,516);AAction2Instr(__a43,577)]);
(107, [EatInstr(112,156)]);
(491, [EatInstr(108,518)]);
(108, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,157)]);
(492, [EatInstr(105,519)]);
(109, [EatInstr(99,158)]);
(493, [EatInstr(115,520)]);
(110, [EatInstr(114,159)]);
(494, [AAction2Instr(__a1,495);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,494)]);
(111, [EatInstr(101,160)]);
(495, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,521)]);
(112, [EatInstr(111,161)]);
(496, [EatInstr(108,522)]);
(113, [EatInstr(107,162)]);
(497, [EatInstr(116,523)]);
(114, [EatInstr(45,163)]);
(498, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,524)]);
(115, [EatInstr(99,164)]);
(499, [EatInstr(104,525)]);
(116, [EatInstr(110,165)]);
(500, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,526)]);
(117, [AAction2Instr(__a1,166)]);
(501, [EatInstr(111,527)]);
(118, [EatInstr(116,167)]);
(502, [EatInstr(121,528)]);
(119, [EatInstr(105,168)]);
(503, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,529)]);
(120, [EatInstr(110,169)]);
(504, [EatInstr(110,530)]);
(121, [EatInstr(116,170)]);
(505, [EatInstr(111,531)]);
(122, [EatInstr(114,171)]);
(506, [EatInstr(114,532)]);
(123, [EatInstr(99,172)]);
(507, [EatInstr(108,533)]);
(124, [EatInstr(115,173)]);
(508, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,534)]);
(125, [EatInstr(101,174)]);
(509, [EatInstr(97,535)]);
(126, [EatInstr(117,175)]);
(510, [AAction2Instr(__a44,205)]);
(127, [EatInstr(108,176)]);
(511, [EatInstr(101,536)]);
(128, [EatInstr(111,177)]);
(512, [EatInstr(99,537)]);
(129, [EatInstr(109,178)]);
(513, [EatInstr(116,538)]);
(130, [EatInstr(45,179)]);
(514, [EatInstr(103,539)]);
(131, [EatInstr(108,180)]);
(515, [EatInstr(111,540)]);
(132, [EatInstr(101,181)]);
(516, [EatInstr(115,541)]);
(133, [EatInstr(111,182)]);
(517, [AAction2Instr(__a45,236)]);
(134, [EatInstr(114,184);EatInstr(105,183)]);
(518, [EatInstr(101,542)]);
(135, [EatInstr(101,185)]);
(519, [EatInstr(116,543)]);
(136, [AAction2Instr(__a5,236)]);
(520, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,544)]);
(137, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,186)]);
(521, [AAction2Instr(__a46,236)]);
(138, [ALookaheadInstr(false,CfgLA (3,266),89);ACallInstr3(__default_call,11);AContInstr3(273,__g0,__binder3,88);ACallInstr3(__g0,10);ASimpleCont2Instr(274,__binder0,89)]);
(522, [EatInstr(97,545)]);
(139, [EatInstr(119,187)]);
(523, [EatInstr(111,546)]);
(140, [EatInstr(105,188)]);
(524, [AAction2Instr(__a47,236)]);
(141, [EatInstr(101,189)]);
(525, [EatInstr(105,547)]);
(142, [EatInstr(114,190)]);
(526, [AAction2Instr(__a48,236)]);
(143, [EatInstr(103,191)]);
(527, [EatInstr(114,548)]);
(144, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,192)]);
(528, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,549)]);
(145, [EatInstr(114,193)]);
(529, [AAction2Instr(__a1,550)]);
(146, [EatInstr(110,194)]);
(530, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,551)]);
(147, [EatInstr(114,195)]);
(531, [EatInstr(114,552)]);
(148, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,196)]);
(532, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,553)]);
(149, [EatInstr(115,197)]);
(533, [EatInstr(121,554)]);
(150, [EatInstr(101,198)]);
(534, [AAction2Instr(__a49,205)]);
(151, [EatInstr(97,199)]);
(535, [EatInstr(108,555)]);
(152, [EatInstr(101,200)]);
(536, [EatInstr(45,556)]);
(153, [EatInstr(108,201)]);
(537, [EatInstr(101,557)]);
(154, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,202)]);
(538, [EatInstr(105,558)]);
(155, [EatInstr(105,203)]);
(539, [EatInstr(101,559)]);
(156, [EatInstr(97,204)]);
(540, [EatInstr(110,560)]);
(157, [AAction2Instr(__a6,205)]);
(541, [EatInstr(116,561)]);
(158, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,206)]);
(542, [EatInstr(114,562)]);
(159, [EatInstr(97,207)]);
(543, [EatInstr(105,563)]);
(160, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,208)]);
(544, [AAction2Instr(__a50,236)]);
(161, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,209)]);
(545, [EatInstr(114,564)]);
(162, [EatInstr(97,210)]);
(546, [EatInstr(114,565)]);
(163, [EatInstr(108,211)]);
(547, [EatInstr(115,566)]);
(164, [EatInstr(101,212)]);
(548, [EatInstr(121,567)]);
(165, [EatInstr(116,213)]);
(549, [AAction2Instr(__a51,236)]);
(166, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,259)]);
(550, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,583)]);
(167, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,214)]);
(551, [AAction2Instr(__a52,597)]);
(168, [EatInstr(112,215)]);
(552, [EatInstr(101,568)]);
(169, [EatInstr(115,216)]);
(553, [AAction2Instr(__a53,597)]);
(170, [EatInstr(101,217)]);
(554, [EatInstr(115,569)]);
(171, [EatInstr(111,218)]);
(555, [EatInstr(121,570)]);
(172, [EatInstr(107,219)]);
(556, [EatInstr(112,571)]);
(173, [EatInstr(101,220)]);
(557, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,572)]);
(174, [EatInstr(99,221)]);
(558, [EatInstr(111,573)]);
(175, [EatInstr(110,222)]);
(559, [EatInstr(110,574)]);
(176, [EatInstr(105,223)]);
(560, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,575)]);
(177, [EatInstr(107,224)]);
(561, [EatInstr(114,576)]);
(178, [EatInstr(111,225)]);
(562, [AAction2Instr(__a54,577)]);
(179, [EatInstr(115,229);EatInstr(114,228);EatInstr(109,227);EatInstr(99,226)]);
(563, [EatInstr(118,578)]);
(180, [EatInstr(121,230)]);
(564, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,579)]);
(181, [EatInstr(102,231)]);
(565, [EatInstr(121,580)]);
(182, [EatInstr(116,232)]);
(566, [EatInstr(116,581)]);
(183, [EatInstr(116,233)]);
(567, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,582)]);
(184, [EatInstr(111,234)]);
(568, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,585)]);
(185, [EatInstr(45,235)]);
(569, [EatInstr(105,586)]);
(186, [AAction2Instr(__a7,236)]);
(570, [EatInstr(115,587)]);
(187, [EatInstr(45,237)]);
(571, [EatInstr(114,588)]);
(188, [EatInstr(98,238)]);
(572, [AAction2Instr(__a55,205)]);
(189, [EatInstr(45,239)]);
(573, [EatInstr(110,589)]);
(190, [EatInstr(117,240)]);
(574, [EatInstr(45,591);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,590)]);
(191, [EatInstr(97,241)]);
(575, [AAction2Instr(__a56,236)]);
(192, [AAction2Instr(__a8,597)]);
(576, [EatInstr(105,592)]);
(193, [EatInstr(45,242)]);
(577, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,517)]);
(194, [EatInstr(101,243)]);
(578, [EatInstr(101,593)]);
(195, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,244)]);
(579, [AAction2Instr(__a57,236)]);
(196, [AAction2Instr(__a9,597)]);
(580, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,594)]);
(197, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,245)]);
(581, [EatInstr(111,595)]);
(198, [EatInstr(100,246)]);
(582, [AAction2Instr(__a58,236)]);
(199, [EatInstr(121,247)]);
(583, [AAction2Instr(__a1,584);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,583)]);
(200, [EatInstr(116,248)]);
(584, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,596)]);
(201, [EatInstr(108,249)]);
(585, [AAction2Instr(__a59,597)]);
(202, [AAction2Instr(__a10,597)]);
(586, [EatInstr(115,598)]);
(203, [EatInstr(108,250)]);
(587, [EatInstr(105,599)]);
(204, [EatInstr(116,251)]);
(588, [EatInstr(101,600)]);
(205, [CompleteInstr(272)]);
(589, [EatInstr(115,601)]);
(206, [AAction2Instr(__a1,252)]);
(590, [AAction2Instr(__a60,205)]);
(207, [EatInstr(99,253)]);
(591, [EatInstr(115,602)]);
(208, [AAction2Instr(__a11,205)]);
(592, [EatInstr(99,603)]);
(209, [AAction2Instr(__a12,205)]);
(593, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,604)]);
(210, [EatInstr(104,254)]);
(594, [AAction2Instr(__a61,236)]);
(211, [EatInstr(111,255)]);
(595, [EatInstr(114,605)]);
(212, [EatInstr(100,256)]);
(596, [AAction2Instr(__a62,236)]);
(213, [EatInstr(45,258);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,257)]);
(597, [CompleteInstr(271)]);
(214, [AAction2Instr(__a13,205)]);
(598, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,606)]);
(215, [EatInstr(45,261)]);
(599, [EatInstr(115,607)]);
(216, [EatInstr(108,262)]);
(600, [EatInstr(100,608)]);
(217, [EatInstr(114,263)]);
(601, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,609)]);
(218, [EatInstr(119,264)]);
(602, [EatInstr(99,610)]);
(219, [EatInstr(101,265)]);
(603, [EatInstr(116,611)]);
(220, [EatInstr(45,266)]);
(604, [AAction2Instr(__a63,236)]);
(221, [EatInstr(107,267)]);
(605, [EatInstr(121,612)]);
(222, [EatInstr(116,268)]);
(606, [AAction2Instr(__a64,205)]);
(223, [EatInstr(110,269)]);
(607, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,613)]);
(224, [EatInstr(97,270)]);
(608, [EatInstr(105,614)]);
(225, [EatInstr(105,271)]);
(609, [AAction2Instr(__a65,205)]);
(226, [EatInstr(111,272)]);
(610, [EatInstr(97,615)]);
(227, [EatInstr(101,273)]);
(611, [AAction2Instr(__a66,577)]);
(228, [EatInstr(101,274)]);
(612, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,616)]);
(229, [EatInstr(107,275)]);
(613, [AAction2Instr(__a67,205)]);
(230, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,276)]);
(614, [EatInstr(99,617)]);
(231, [EatInstr(105,277)]);
(615, [EatInstr(110,618)]);
(232, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,278)]);
(616, [AAction2Instr(__a68,236)]);
(233, [EatInstr(45,279)]);
(617, [EatInstr(97,619)]);
(234, [EatInstr(108,280)]);
(618, [EatInstr(110,620)]);
(235, [EatInstr(102,281)]);
(619, [EatInstr(116,621)]);
(236, [CompleteInstr(273)]);
(620, [EatInstr(101,622)]);
(237, [EatInstr(110,282)]);
(621, [EatInstr(101,623)]);
(238, [EatInstr(117,283)]);
(622, [EatInstr(114,624)]);
(239, [EatInstr(117,284)]);
(623, [EatInstr(115,625)]);
(240, [EatInstr(108,285)]);
(624, [EatInstr(108,626)]);
(241, [EatInstr(114,286)]);
(625, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,627)]);
(242, [EatInstr(116,288)]);
(626, [EatInstr(101,628)]);
(243, [EatInstr(45,289)]);
(627, [AAction2Instr(__a69,205)]);
(244, [AAction2Instr(__a14,597)]);
(628, [EatInstr(115,629)]);
(245, [AAction2Instr(__a15,597)]);
(629, [EatInstr(115,630)]);
(246, [EatInstr(101,290)]);
(630, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,631)]);
(247, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,291)]);
(631, [AAction2Instr(__a70,205)]);
(248, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,292)]);
(249, [EatInstr(45,293)]);
(250, [EatInstr(101,294)]);
(251, [EatInstr(99,295)]);
(252, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,296)]);
(253, [EatInstr(116,297)]);
(254, [EatInstr(101,298)]);
(255, [EatInstr(111,299)]);
(256, [EatInstr(101,300)]);
(257, [AAction2Instr(__a16,205)]);
(258, [EatInstr(114,303);EatInstr(110,302);EatInstr(103,301)]);
(259, [AAction2Instr(__a1,260);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,259)]);
(260, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,304)]);
(261, [EatInstr(108,305)]);
(262, [EatInstr(97,306)]);
(263, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,307)]);
(264, [EatInstr(45,308)]);
(265, [EatInstr(110,309)]);
(266, [EatInstr(105,310)]);
(267, [EatInstr(45,311)]);
(268, [EatInstr(101,312)]);
(269, [EatInstr(101,313)]);
(270, [EatInstr(104,314)]);
(271, [EatInstr(122,315)]);
(272, [EatInstr(97,316)]);
(273, [EatInstr(109,317)]);
(274, [EatInstr(112,318)]);
(275, [EatInstr(105,319)]);
(276, [AAction2Instr(__a17,236)]);
(277, [EatInstr(120,320)]);
(278, [AAction2Instr(__a1,321)]);
(279, [EatInstr(104,322)]);
(280, [EatInstr(108,323)]);
(281, [EatInstr(115,324)]);
(282, [EatInstr(111,325)]);
(283, [EatInstr(116,326)]);
(284, [EatInstr(110,327)]);
(285, [EatInstr(101,328)]);
(286, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,329)]);
(288, [EatInstr(121,330)]);
(289, [EatInstr(114,331)]);
(290, [EatInstr(110,332)]);
(291, [AAction2Instr(__a18,597)]);
(292, [AAction2Instr(__a19,597)]);
(293, [EatInstr(115,333)]);
(294, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,334)]);
(295, [EatInstr(104,335)]);
(296, [AAction2Instr(__a20,336)]);
(297, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,337)]);
(298, [EatInstr(97,338)]);
(299, [EatInstr(107,339)]);
(300, [EatInstr(110,340)]);
(301, [EatInstr(105,341)]);
(302, [EatInstr(117,343);EatInstr(112,342)]);
(303, [EatInstr(101,344)]);
(304, [AAction2Instr(__a21,205)]);
(305, [EatInstr(97,345)]);
(306, [EatInstr(116,346)]);
(307, [AContInstr3(271,__g0,__binder4,347);ACallInstr3(__g0,8)]);
(308, [EatInstr(110,348)]);
(309, [EatInstr(100,349)]);
(310, [EatInstr(110,350)]);
(311, [EatInstr(108,351)]);
(312, [EatInstr(114,352)]);
(313, [EatInstr(45,353)]);
(314, [EatInstr(101,354)]);
(315, [EatInstr(101,355)]);
(316, [EatInstr(108,356)]);
(317, [EatInstr(111,357)]);
(318, [EatInstr(108,358)]);
(319, [EatInstr(112,359)]);
(320, [EatInstr(45,360)]);
(321, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,361)]);
(322, [EatInstr(105,362)]);
(323, [EatInstr(45,363)]);
(324, [EatInstr(116,365);EatInstr(109,364)]);
(325, [EatInstr(116,366)]);
(326, [EatInstr(101,367)]);
(327, [EatInstr(100,368)]);
(328, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,369)]);
(329, [AAction2Instr(__a22,597)]);
(330, [EatInstr(112,370)]);
(331, [EatInstr(101,371)]);
(332, [EatInstr(99,372)]);
(333, [EatInstr(116,373)]);
(334, [AAction2Instr(__a23,205)]);
(335, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,374)]);
(336, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,375)]);
(337, [AAction2Instr(__a24,205)]);
(338, [EatInstr(100,376)]);
(339, [EatInstr(97,377)]);
(340, [EatInstr(99,378)]);
(341, [EatInstr(108,379)]);
(342, [EatInstr(114,380)]);
(343, [EatInstr(108,381)]);
(344, [EatInstr(108,382)]);
(345, [EatInstr(116,383)]);
(346, [EatInstr(101,384)]);
(347, [AAction2Instr(__a25,236)]);
(348, [EatInstr(111,385)]);
(349, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,386)]);
(350, [EatInstr(115,387)]);
(351, [EatInstr(97,388)]);
(352, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,389)]);
(353, [EatInstr(114,391);EatInstr(99,390)]);
(354, [EatInstr(97,392)]);
(355, [EatInstr(45,393)]);
(356, [EatInstr(101,394)]);
(357, [EatInstr(105,395)]);
(358, [EatInstr(97,396)]);
(359, [EatInstr(45,397)]);
(360, [EatInstr(104,398)]);
(361, [AAction2Instr(__a1,399)]);
(362, [EatInstr(115,400)]);
(363, [EatInstr(115,401)]);
(364, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,402)]);
(365, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,403)]);
(366, [EatInstr(97,404)]);
(367, [EatInstr(115,405)]);
(368, [EatInstr(101,406)]);
(369, [AAction2Instr(__a26,597)]);
(370, [EatInstr(101,407)]);
(371, [EatInstr(103,408)]);
(372, [EatInstr(101,409)]);
(373, [EatInstr(97,410)]);
(374, [AAction2Instr(__a27,205)]);
(375, [AAction2Instr(__a28,205);AAction2Instr(__a1,411)]);
(376, [EatInstr(45,412)]);
(377, [EatInstr(104,413)]);
(378, [EatInstr(101,414)]);
(379, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,415)]);
(380, [EatInstr(101,416)]);
(381, [EatInstr(108,417)]);
(382, [EatInstr(101,418)]);
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
