
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
| Coroutine_cmd
| Desugar_cmd
| Desugar_gil_cmd
| Dearrow_cmd
| Dot_cmd
| Exec_cmd
| Extract_cmd
| Fuse_cmd
| Hash_cmd
| Infer_ty_cmd
| Info_cmd
| Inline_regular_cmd
| Inline_nullable_cmd
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
  type t = int * hv * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
  let memoize = true
end
module Yk_History = Yak.History.Make(Yk_Hashed)

let rec
 _r_cmd_line_args(_n,_p,ykinput) = (let c = _r_command(_n,_p,ykinput) in (); (while (match _n() with (2000) -> true | _ (*2001*) -> false) do
_r_args(_n,_p,ykinput)done)
; ();  cmd := c )
and
 _r_phases(_n,_p,ykinput) = (match _n() with
 | (2002) -> ((); ();  Attributes_cmd )
 | (2003) -> ((); ();  Close_under_core_cmd )
 | (2004) -> ((); ();  Copyrule_cmd )
 | (2005) -> ((); ();  Desugar_cmd )
 | (2006) -> ((); ();  Desugar_gil_cmd )
 | (2007) -> ((); ();  Hash_cmd )
 | (2008) -> ((); ();  Dearrow_cmd )
 | (2009) -> ((); ();  Infer_ty_cmd )
 | (2010) -> ((); ();  Inline_regular_cmd )
 | (2011) -> ((); ();  Inline_nullable_cmd )
 | (2012) -> ((); ();  Lexer_cmd )
 | (2013) -> ((); ();  Lift_cmd )
 | (2014) -> ((); ();  Minus_cmd )
 | (2015) -> ((); ();  Tx_prec_cmd )
 | (2016) -> ((); ();  Subset_cmd )
 | (2017) -> ((); ();  Unroll_star_cmd )
 | (2018) -> ((); ();  Wrap_cmd )
 | (2019) -> ((); ();  Replay_cmd )
 | _ -> raise Exit)
and
 _r_command(_n,_p,ykinput) = (match _n() with
 | (2020) -> ((let p = _r_phases(_n,_p,ykinput) in  (match p with
                                               Inline_regular_cmd -> Compileopt.inline_regular := true
                                             | Unroll_star_cmd -> if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1
                                             | _ -> ());
                                            p ))
 | (2021) -> ((); ();  Compile_cmd )
 | (2022) -> ((); ();  Coroutine_cmd )
 | (2023) -> ((); ();  Dot_cmd )
 | (2024) -> ((); (); (let _x4 = _p() in (); (let _x3 = _p() in (let f = Yak.YkBuf.get_string _x4 _x3 ykinput in (let l = (let _x8 = (let rec _x19 _x8 =
(match _n() with (2028) -> _x8 | _ (*2027*) ->
 _x19((let _x7 = (); (let _x6 = _p() in (); (let _x5 = _p() in (let x = Yak.YkBuf.get_string _x6 _x5 ykinput in x))) in _x7::_x8)))
in _x19(Yak.Util.nil)) in (List.rev _x8)) in ();  files := f::!files; exec_l := l; Exec_cmd )))))
 | (2031) -> ((); ();  Extract_cmd )
 | (2032) -> ((); ();  Compileopt.coalesce := true; Fuse_cmd )
 | (2033) -> ((); ();  Info_cmd )
 | (2034) -> ((); ();  Lookahead_analysis_cmd )
 | (2035) -> ((); ();  Lr1_lookahead_cmd )
 | (2036) -> ((); ();  Precedence_analysis_cmd )
 | (2037) -> ((); ();  Print_gul_cmd )
 | (2038) -> ((); ();  Print_gil_cmd )
 | (2039) -> ((); ();  Print_npreds_cmd )
 | (2040) -> ((); ();  Print_npreds_cmd )
 | (2041) -> ((); ();  Print_relevance_cmd )
 | (2042) -> ((); (let _x10 = _p() in (); (let _x9 = _p() in (let n = Yak.YkBuf.get_string _x10 _x9 ykinput in ();  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" ))))
 | (2045) -> ((); ();  Sort_cmd )
 | (2046) -> ((); ();  Strip_late_actions_cmd )
 | (2047) -> ((); ();  Translate_dypgen_cmd )
 | (2048) -> ((); ();  Translate_dypgen_scannerless_cmd )
 | _ -> raise Exit)
and
 _r_args(_n,_p,ykinput) = (match _n() with
 | (2049) -> ((); (); (let p = _r_phases(_n,_p,ykinput) in  after := Some p ; ()))
 | (2050) -> ((); ();  Compileopt.use_coroutines := false ; ())
 | (2051) -> ((); (); (let b = (match _n() with
 | (2052) -> ((); Fun_BE)
 | (2053) -> ((); Trans_BE)
 | (2054) -> ((); Wadler_BE)
 | (2055) -> ((); Peg_BE false)
 | (2056) -> ((); Peg_BE true)
 | _ -> raise Exit) in ();  backend := b ; ()))
 | (2057) -> ((); ();  Compileopt.case_sensitive := false ; ())
 | (2058) -> ((); ();  Compileopt.check_labels := true ; ())
 | (2059) -> ((); (); (let _x12 = _p() in (); (let _x11 = _p() in (let n = Yak.YkBuf.get_string _x12 _x11 ykinput in ();  Variables.counter := (int_of_string n) ; ()))))
 | (2062) -> ((); ();  Compileopt.inline_cs := true ; ())
 | (2063) -> ((); ();  Compileopt.inline_regular := true ; ())
 | (2064) -> ((); ();  Compileopt.lookahead := true ; ())
 | (2065) -> ((); ();  Compileopt.memoize_history := true ; ())
 | (2066) -> ((); ();  Compileopt.coalesce := false ; ())
 | (2067) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2068) -> ((); ();  Compileopt.repress_replay := true ; ())
 | (2069) -> ((); ();  Compileopt.skip_opt := false ; ())
 | (2070) -> ((); ();  only := true ; ())
 | (2071) -> ((); ();  Compileopt.postfix_history := false ; ())
 | (2072) -> ((); (); (let _x14 = _p() in (); (let _x13 = _p() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2075) -> ((); ();  Compileopt.unit_history := true ; ())
 | (2076) -> ((); (); (let _x16 = _p() in (); (let _x15 = _p() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  Compileopt.unroll_star_n := (int_of_string n) ; ()))))
 | (2079) -> ((); ();  Compileopt.use_fsm := true ; ())
 | (2080) -> ((); ();  Compileopt.use_fsm := false ; ())
 | (2081) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2082) -> ((let _x18 = _p() in (); (let _x17 = _p() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
 | _ -> raise Exit)
class ['a] rvs (labels: 'a History.enum) =
let s = ref [] in
let push x = s := x::!s in
let push_pos p = s := ( p)::!s in
let _n() = (let (_,x,_) = labels#next() in x) in
let _p() = (let (_,_,p) = labels#next() in p) in
let rec _rv_cmd_line_args() = ();();push((2001)); while (match _n() with (2000) -> true | _ (*2001*)-> false) do
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
 | (2018) -> (();();(); push((2018)))
 | (2019) -> (();();(); push((2019)))
 | _ -> raise Exit)
and _rv_command() = (match _n() with
 | (2020) -> (();_rv_phases(); push((2020)))
 | (2021) -> (();();(); push((2021)))
 | (2022) -> (();();(); push((2022)))
 | (2023) -> (();();(); push((2023)))
 | (2024) -> (();();();push((2028)); while (match _n() with (2027) -> true | _ (*2028*)-> false) do
 ();();();push_pos(_p());();push_pos(_p());(); push((2027))
done
;();push_pos(_p());();push_pos(_p());();(); push((2024)))
 | (2031) -> (();();(); push((2031)))
 | (2032) -> (();();(); push((2032)))
 | (2033) -> (();();(); push((2033)))
 | (2034) -> (();();(); push((2034)))
 | (2035) -> (();();(); push((2035)))
 | (2036) -> (();();(); push((2036)))
 | (2037) -> (();();(); push((2037)))
 | (2038) -> (();();(); push((2038)))
 | (2039) -> (();();(); push((2039)))
 | (2040) -> (();();(); push((2040)))
 | (2041) -> (();();(); push((2041)))
 | (2042) -> (();();();push_pos(_p());();push_pos(_p());(); push((2042)))
 | (2045) -> (();();(); push((2045)))
 | (2046) -> (();();(); push((2046)))
 | (2047) -> (();();(); push((2047)))
 | (2048) -> (();();(); push((2048)))
 | _ -> raise Exit)
and _rv_args() = (match _n() with
 | (2049) -> (();();_rv_phases();();(); push((2049)))
 | (2050) -> (();();();(); push((2050)))
 | (2051) -> (();();();(match _n() with
 | (2052) -> (();(); push((2052)))
 | (2053) -> (();(); push((2053)))
 | (2054) -> (();(); push((2054)))
 | (2055) -> (();(); push((2055)))
 | (2056) -> (();(); push((2056)))
 | _ -> raise Exit);();(); push((2051)))
 | (2057) -> (();();();(); push((2057)))
 | (2058) -> (();();();(); push((2058)))
 | (2059) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2059)))
 | (2062) -> (();();();(); push((2062)))
 | (2063) -> (();();();(); push((2063)))
 | (2064) -> (();();();(); push((2064)))
 | (2065) -> (();();();(); push((2065)))
 | (2066) -> (();();();(); push((2066)))
 | (2067) -> (();();();(); push((2067)))
 | (2068) -> (();();();(); push((2068)))
 | (2069) -> (();();();(); push((2069)))
 | (2070) -> (();();();(); push((2070)))
 | (2071) -> (();();();(); push((2071)))
 | (2072) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2072)))
 | (2075) -> (();();();(); push((2075)))
 | (2076) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2076)))
 | (2079) -> (();();();(); push((2079)))
 | (2080) -> (();();();(); push((2080)))
 | (2081) -> (();();();(); push((2081)))
 | (2082) -> (();();();();push_pos(_p());();push_pos(_p()); push((2082)))
 | _ -> raise Exit)
in
object (self)
method next() = (match !s with hd::tl -> (s := tl; hd) | _ -> raise Not_found)
initializer _rv_cmd_line_args()
end

let _replay_cmd_line_args ykinput h =
  let _o = new rvs (h#right_to_left) in
  let _n() = _o#next() in
  let _p() = _o#next() in
  _r_cmd_line_args(_n,_p,ykinput)
(* History constructors *)
let _e p h = h#empty p
let _p lbl hv p = (fun h->h#push p (lbl, hv, p))
let _m lbl p = (fun h1 h2-> h1#merge p (lbl,  lbl, p) h2)

(*LATE PROLOGUE*)
type _pos = int (* input positions *)
let hv_compare = Yk_History.compare
type sv = (int * hv * _pos, Yak.History.label) Yak.History.history
let sv0 = Yk_History.new_history()
let sv_compare = hv_compare
let sv_hash = Yk_History.hash
let sv_eq x y = sv_compare x y = 0
let key_eq (i,v1) (j,v2) = i = j &&  sv_eq v1 v2
let key_hash (i,v) = i lxor (sv_hash v)

(** Hashtable for top-down parsing. *)
module TDHashtable = Hashtbl.Make(struct type t = int * sv let equal = key_eq let hash = key_hash end)

let __default_call _ _ = sv0;;
let __cc_call _ x = x;;
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
let __a79 = (_p 1082 ((2055)));;
let __a61 = (_p 1078 ((2058)));;
let __a37 = (_p 1056 ((2080)));;
let __a44 = (_p 1084 ((2053)));;
let __a71 = (_p 1059 ((2078)));;
let __a8 = (_p 1047 ((2023)));;
let __a28 = (_p 1049 ((2021)));;
let __a83 = (_p 1022 ((2048)));;
let __a9 = (_p 1052 ((2082)));;
let __a36 = (_p 1057 ((2079)));;
let __a12 = (_p 1005 ((2018)));;
let __a6 = (_p 1028 ((2043)));;
let __a78 = (_p 1024 ((2046)));;
let __a68 = (_p 1086 ((2050)));;
let __a24 = (_p 1007 ((2016)));;
let __a31 = (_p 1063 ((2074)));;
let __a18 = (_p 1009 ((2014)));;
let __a62 = (_p 1061 ((2075)));;
let __a17 = (_p 1011 ((2012)));;
let __a82 = (_p 1030 ((2040)));;
let __a70 = (_p 1065 ((2071)));;
let __a64 = (_p 1013 ((2010)));;
let __a30 = (_p 1087 ((2049)));;
let __a26 = (_p 1026 ((2042)));;
let __a54 = (_p 1031 ((2039)));;
let __a49 = (_p 1014 ((2009)));;
let __a47 = (_p 1068 ((2068)));;
let __a19 = (_p 1033 ((2037)));;
let __a10 = (_p 1016 ((2007)));;
let __a60 = (_p 1035 ((2035)));;
let __a27 = (_p 1018 ((2005)));;
let __a57 = (_p 1070 ((2066)));;
let __a53 = (_p 1076 ((2061)));;
let __a15 = (_p 1037 ((2033)));;
let __a72 = (_p 1020 ((2003)));;
let __a55 = (_p 1080 ((2051)));;
let __a46 = (_p 1072 ((2064)));;
let __a29 = (_p 1039 ((2031)));;
let __a45 = (_p 1074 ((2062)));;
let __a4 = (_p 1053 ((2084)));;
let __a2 = (_p 1002 ((2001)));;
let __a43 = (fun _x0_ _x1_ -> (((_p 1041 ((2027))) _x0_) (((_p 1042 ((2030))) _x0_) _x1_)));;
let __a13 = (_p 1046 ((2025)));;
let __a52 = (_p 1081 ((2056)));;
let __a7 = (_p 1055 ((2081)));;
let __a76 = (_p 1079 ((2057)));;
let __a65 = (_p 1083 ((2054)));;
let __a25 = (fun _x0_ _x1_ -> (((_p 1044 ((2028))) _x0_) (((_p 1045 ((2026))) _x0_) _x1_)));;
let __a51 = (_p 1085 ((2052)));;
let __a38 = (_p 1048 ((2022)));;
let __a59 = (_p 1060 ((2077)));;
let __a23 = (_p 1004 ((2019)));;
let __a5 = (_p 1000 ((2000)));;
let __a20 = (_p 1027 ((2044)));;
let __a73 = (_p 1023 ((2047)));;
let __a56 = (_p 1075 ((2059)));;
let __a50 = (_p 1006 ((2017)));;
let __a3 = (_p 1050 ((2020)));;
let __a22 = (_p 1064 ((2073)));;
let __a16 = (_p 1025 ((2045)));;
let __a42 = (_p 1008 ((2015)));;
let __a67 = (_p 1029 ((2041)));;
let __a11 = (_p 1010 ((2013)));;
let __a66 = (_p 1012 ((2011)));;
let __a21 = (_p 1066 ((2070)));;
let __a75 = (_p 1058 ((2076)));;
let __a63 = (_p 1015 ((2008)));;
let __a58 = (_p 1067 ((2069)));;
let __a40 = (_p 1062 ((2072)));;
let __a39 = (_p 1032 ((2038)));;
let __g0 = (_e);;
let __a81 = (_p 1069 ((2067)));;
let __a80 = (_p 1034 ((2036)));;
let __a48 = (_p 1017 ((2006)));;
let __a77 = (_p 1036 ((2034)));;
let __a32 = (_p 1019 ((2004)));;
let __a74 = (_p 1071 ((2065)));;
let __a14 = (_p 1038 ((2032)));;
let __a35 = (_p 1077 ((2060)));;
let __a34 = (_p 1040 ((2024)));;
let __a69 = (_p 1073 ((2063)));;
let __a33 = (_p 1043 ((2029)));;
let __a41 = (_p 1021 ((2002)));;
let __a1 = (_p 1054 ((2083)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1003);;
let __binder2 = (_m 1051);;
let __binder3 = (_m 1001);;
let __binder4 = (_m 1088);;
open Yak.Pam_internal
let program = [
(383, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,422)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(101,423)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(108,424)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(101,425)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(101,426)]);
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [EatInstr(45,427)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(116,428)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,43)]);
(390, [EatInstr(119,432);EatInstr(116,431);EatInstr(112,430);EatInstr(102,429)]);
(7, [EatInstr(127,45);EatInstr(126,45);EatInstr(125,45);EatInstr(124,45);EatInstr(123,45);EatInstr(122,45);EatInstr(121,45);EatInstr(120,45);EatInstr(119,45);EatInstr(118,45);EatInstr(117,45);EatInstr(116,45);EatInstr(115,45);EatInstr(114,45);EatInstr(113,45);EatInstr(112,45);EatInstr(111,45);EatInstr(110,45);EatInstr(109,45);EatInstr(108,45);EatInstr(107,45);EatInstr(106,45);EatInstr(105,45);EatInstr(104,45);EatInstr(103,45);EatInstr(102,45);EatInstr(101,45);EatInstr(100,45);EatInstr(99,45);EatInstr(98,45);EatInstr(97,45);EatInstr(96,45);EatInstr(95,45);EatInstr(94,45);EatInstr(93,45);EatInstr(92,45);EatInstr(91,45);EatInstr(90,45);EatInstr(89,45);EatInstr(88,45);EatInstr(87,45);EatInstr(86,45);EatInstr(85,45);EatInstr(84,45);EatInstr(83,45);EatInstr(82,45);EatInstr(81,45);EatInstr(80,45);EatInstr(79,45);EatInstr(78,45);EatInstr(77,45);EatInstr(76,45);EatInstr(75,45);EatInstr(74,45);EatInstr(73,45);EatInstr(72,45);EatInstr(71,45);EatInstr(70,45);EatInstr(69,45);EatInstr(68,45);EatInstr(67,45);EatInstr(66,45);EatInstr(65,45);EatInstr(64,45);EatInstr(63,45);EatInstr(62,45);EatInstr(61,45);EatInstr(60,45);EatInstr(59,45);EatInstr(58,45);EatInstr(57,45);EatInstr(56,45);EatInstr(55,45);EatInstr(54,45);EatInstr(53,45);EatInstr(52,45);EatInstr(51,45);EatInstr(50,45);EatInstr(49,45);EatInstr(48,45);EatInstr(47,45);EatInstr(46,45);EatInstr(44,45);EatInstr(43,45);EatInstr(42,45);EatInstr(41,45);EatInstr(40,45);EatInstr(39,45);EatInstr(38,45);EatInstr(37,45);EatInstr(36,45);EatInstr(35,45);EatInstr(34,45);EatInstr(33,45);EatInstr(32,45);EatInstr(31,45);EatInstr(30,45);EatInstr(29,45);EatInstr(28,45);EatInstr(27,45);EatInstr(26,45);EatInstr(25,45);EatInstr(24,45);EatInstr(23,45);EatInstr(22,45);EatInstr(21,45);EatInstr(20,45);EatInstr(19,45);EatInstr(18,45);EatInstr(17,45);EatInstr(16,45);EatInstr(15,45);EatInstr(14,45);EatInstr(13,45);EatInstr(12,45);EatInstr(11,45);EatInstr(10,45);EatInstr(9,45);EatInstr(8,45);EatInstr(7,45);EatInstr(6,45);EatInstr(5,45);EatInstr(4,45);EatInstr(3,45);EatInstr(2,45);EatInstr(1,45)]);
(391, [EatInstr(101,433)]);
(8, [EatInstr(119,28);EatInstr(117,27);EatInstr(115,26);EatInstr(114,25);EatInstr(112,24);EatInstr(109,23);EatInstr(108,22);EatInstr(105,21);EatInstr(104,20);EatInstr(100,19);EatInstr(99,18);EatInstr(97,17)]);
(392, [EatInstr(98,434)]);
(9, [EatInstr(116,38);EatInstr(115,37);EatInstr(114,36);EatInstr(112,35);EatInstr(108,34);EatInstr(105,33);EatInstr(102,32);EatInstr(101,31);EatInstr(100,30);EatInstr(99,29);AContInstr3(271,__g0,__binder2,39);ACallInstr3(__g0,8)]);
(393, [AAction2Instr(__a35,435)]);
(10, [EatInstr(45,40);AAction2Instr(__a1,41)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),42)]);
(394, [EatInstr(115,436)]);
(395, [EatInstr(101,437)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(100,438)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(104,439)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(115,440)]);
(15, [AAction2Instr(__a2,137)]);
(399, [EatInstr(122,441)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(121,442)]);
(17, [EatInstr(116,48);EatInstr(114,47)]);
(401, [EatInstr(111,443)]);
(18, [EatInstr(111,50);EatInstr(108,49)]);
(402, [EatInstr(105,444)]);
(19, [EatInstr(101,51)]);
(403, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,445)]);
(20, [EatInstr(97,52)]);
(404, [EatInstr(116,446)]);
(21, [EatInstr(110,53)]);
(405, [EatInstr(116,447)]);
(22, [EatInstr(105,55);EatInstr(101,54)]);
(406, [AAction2Instr(__a36,235)]);
(23, [EatInstr(105,56)]);
(407, [AAction2Instr(__a37,235)]);
(24, [EatInstr(114,57)]);
(408, [EatInstr(116,448)]);
(25, [EatInstr(101,58)]);
(409, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,449)]);
(26, [EatInstr(117,59)]);
(410, [EatInstr(114,450)]);
(27, [EatInstr(110,60)]);
(411, [EatInstr(108,451)]);
(28, [EatInstr(114,61)]);
(412, [EatInstr(115,452)]);
(29, [EatInstr(111,62)]);
(413, [EatInstr(108,453)]);
(30, [EatInstr(111,63)]);
(414, [EatInstr(117,454)]);
(31, [EatInstr(120,64)]);
(415, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,455)]);
(32, [EatInstr(117,65)]);
(416, [EatInstr(114,456)]);
(33, [EatInstr(110,66)]);
(417, [AAction2Instr(__a38,204)]);
(34, [EatInstr(114,68);EatInstr(111,67)]);
(418, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,457)]);
(35, [EatInstr(114,69)]);
(419, [EatInstr(97,458)]);
(36, [EatInstr(102,70)]);
(420, [EatInstr(101,459)]);
(37, [EatInstr(116,72);EatInstr(111,71)]);
(421, [EatInstr(45,460)]);
(38, [EatInstr(114,73)]);
(422, [AAction2Instr(__a39,204)]);
(39, [AAction2Instr(__a3,204)]);
(423, [EatInstr(100,461)]);
(40, [EatInstr(118,85);EatInstr(117,84);EatInstr(114,83);EatInstr(112,82);EatInstr(111,81);EatInstr(110,80);EatInstr(109,79);EatInstr(108,78);EatInstr(105,77);EatInstr(99,76);EatInstr(98,75);EatInstr(97,74)]);
(424, [EatInstr(97,462)]);
(41, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,86)]);
(425, [EatInstr(118,463)]);
(42, [CompleteInstr(274)]);
(426, [EatInstr(45,464)]);
(43, [ALookaheadInstr(false,CfgLA (1,264),44);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,43)]);
(427, [EatInstr(100,465)]);
(44, [CompleteInstr(269)]);
(428, [EatInstr(97,466)]);
(45, [ALookaheadInstr(false,CfgLA (1,264),46);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,45)]);
(429, [EatInstr(117,467)]);
(46, [CompleteInstr(270)]);
(430, [EatInstr(101,468)]);
(47, [EatInstr(114,89)]);
(431, [EatInstr(120,469)]);
(48, [EatInstr(116,90)]);
(432, [EatInstr(97,470)]);
(49, [EatInstr(111,91)]);
(433, [EatInstr(110,471)]);
(50, [EatInstr(112,92)]);
(434, [EatInstr(101,472)]);
(51, [EatInstr(115,93)]);
(435, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,505)]);
(52, [EatInstr(115,94)]);
(436, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,473)]);
(53, [EatInstr(108,96);EatInstr(102,95)]);
(437, [EatInstr(103,474)]);
(54, [EatInstr(120,97)]);
(438, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,475)]);
(55, [EatInstr(102,98)]);
(439, [EatInstr(105,476)]);
(56, [EatInstr(110,99)]);
(440, [EatInstr(99,477)]);
(57, [EatInstr(101,100)]);
(441, [EatInstr(101,478)]);
(58, [EatInstr(112,101)]);
(442, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,479)]);
(59, [EatInstr(98,102)]);
(443, [EatInstr(112,480)]);
(60, [EatInstr(114,103)]);
(444, [EatInstr(115,481)]);
(61, [EatInstr(97,104)]);
(445, [AAction2Instr(__a40,235)]);
(62, [EatInstr(114,106);EatInstr(109,105)]);
(446, [EatInstr(111,482)]);
(63, [EatInstr(116,107)]);
(447, [EatInstr(97,483)]);
(64, [EatInstr(116,109);EatInstr(101,108)]);
(448, [EatInstr(105,484)]);
(65, [EatInstr(115,110)]);
(449, [AAction2Instr(__a41,244)]);
(66, [EatInstr(102,111)]);
(450, [EatInstr(45,485)]);
(67, [EatInstr(111,112)]);
(451, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,486)]);
(68, [EatInstr(49,113)]);
(452, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,487)]);
(69, [EatInstr(105,115);EatInstr(101,114)]);
(453, [EatInstr(97,488)]);
(70, [EatInstr(99,116)]);
(454, [EatInstr(108,489)]);
(71, [EatInstr(114,117)]);
(455, [AAction2Instr(__a42,244)]);
(72, [EatInstr(114,118)]);
(456, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,490)]);
(73, [EatInstr(97,119)]);
(457, [AAction2Instr(__a43,338)]);
(74, [EatInstr(114,121);EatInstr(102,120)]);
(458, [EatInstr(110,491)]);
(75, [EatInstr(97,122)]);
(459, [EatInstr(97,492)]);
(76, [EatInstr(111,125);EatInstr(104,124);EatInstr(97,123)]);
(460, [EatInstr(97,493)]);
(77, [EatInstr(110,126)]);
(461, [EatInstr(115,494)]);
(78, [EatInstr(111,127)]);
(462, [EatInstr(98,495)]);
(79, [EatInstr(101,128)]);
(463, [EatInstr(97,496)]);
(80, [EatInstr(111,129)]);
(464, [EatInstr(97,497)]);
(81, [EatInstr(110,130)]);
(465, [EatInstr(121,498)]);
(82, [EatInstr(114,131)]);
(466, [EatInstr(116,499)]);
(83, [EatInstr(111,132)]);
(467, [EatInstr(110,500)]);
(84, [EatInstr(115,134);EatInstr(110,133)]);
(468, [EatInstr(103,501)]);
(85, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,135)]);
(469, [AAction2Instr(__a44,592)]);
(86, [AAction2Instr(__a4,136)]);
(470, [EatInstr(100,502)]);
(87, [AAction2Instr(__a5,137)]);
(471, [EatInstr(115,503)]);
(88, [CompleteInstr(267)]);
(472, [EatInstr(108,504)]);
(89, [EatInstr(111,138)]);
(473, [AAction2Instr(__a45,235)]);
(90, [EatInstr(114,139)]);
(474, [EatInstr(117,507)]);
(91, [EatInstr(115,140)]);
(475, [AAction2Instr(__a46,235)]);
(92, [EatInstr(121,141)]);
(476, [EatInstr(115,508)]);
(93, [EatInstr(117,142)]);
(477, [EatInstr(101,509)]);
(94, [EatInstr(104,143)]);
(478, [EatInstr(45,510)]);
(95, [EatInstr(101,144)]);
(479, [AAction2Instr(__a47,235)]);
(96, [EatInstr(105,145)]);
(480, [EatInstr(116,511)]);
(97, [EatInstr(101,146)]);
(481, [EatInstr(116,512)]);
(98, [EatInstr(116,147)]);
(482, [EatInstr(114,513)]);
(99, [EatInstr(117,148)]);
(483, [EatInstr(114,514)]);
(100, [EatInstr(99,149)]);
(484, [EatInstr(111,515)]);
(101, [EatInstr(108,150)]);
(485, [EatInstr(99,516)]);
(102, [EatInstr(115,151)]);
(486, [AAction2Instr(__a48,244)]);
(103, [EatInstr(111,152)]);
(487, [AAction2Instr(__a49,244)]);
(104, [EatInstr(112,153)]);
(488, [EatInstr(98,517)]);
(105, [EatInstr(112,154)]);
(489, [EatInstr(97,518)]);
(106, [EatInstr(111,155)]);
(490, [AAction2Instr(__a50,244)]);
(107, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,156)]);
(491, [EatInstr(97,519)]);
(108, [EatInstr(99,157)]);
(492, [EatInstr(100,520)]);
(109, [EatInstr(114,158)]);
(493, [EatInstr(110,521)]);
(110, [EatInstr(101,159)]);
(494, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,522)]);
(111, [EatInstr(111,160)]);
(495, [EatInstr(108,523)]);
(112, [EatInstr(107,161)]);
(496, [EatInstr(110,524)]);
(113, [EatInstr(45,162)]);
(497, [EatInstr(99,525)]);
(114, [EatInstr(99,163)]);
(498, [EatInstr(112,526)]);
(115, [EatInstr(110,164)]);
(499, [EatInstr(105,527)]);
(116, [AAction2Instr(__a6,165)]);
(500, [AAction2Instr(__a51,592)]);
(117, [EatInstr(116,166)]);
(501, [EatInstr(45,528);AAction2Instr(__a52,592)]);
(118, [EatInstr(105,167)]);
(502, [EatInstr(108,530)]);
(119, [EatInstr(110,168)]);
(503, [EatInstr(105,531)]);
(120, [EatInstr(116,169)]);
(504, [EatInstr(115,532)]);
(121, [EatInstr(114,170)]);
(505, [AAction2Instr(__a53,506);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,505)]);
(122, [EatInstr(99,171)]);
(506, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,533)]);
(123, [EatInstr(115,172)]);
(507, [EatInstr(108,534)]);
(124, [EatInstr(101,173)]);
(508, [EatInstr(116,535)]);
(125, [EatInstr(117,174)]);
(509, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,536)]);
(126, [EatInstr(108,175)]);
(510, [EatInstr(104,537)]);
(127, [EatInstr(111,176)]);
(511, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(128, [EatInstr(109,177)]);
(512, [EatInstr(111,539)]);
(129, [EatInstr(45,178)]);
(513, [EatInstr(121,540)]);
(130, [EatInstr(108,179)]);
(514, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,541)]);
(131, [EatInstr(101,180)]);
(515, [EatInstr(110,542)]);
(132, [EatInstr(111,181)]);
(516, [EatInstr(111,543)]);
(133, [EatInstr(114,183);EatInstr(105,182)]);
(517, [EatInstr(108,544)]);
(134, [EatInstr(101,184)]);
(518, [EatInstr(114,545)]);
(135, [AAction2Instr(__a7,235)]);
(519, [EatInstr(108,546)]);
(136, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,185)]);
(520, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,547)]);
(137, [ALookaheadInstr(false,CfgLA (3,266),88);ACallInstr3(__default_call,11);AContInstr3(273,__g0,__binder3,87);ACallInstr3(__g0,10);ASimpleCont2Instr(274,__binder0,88)]);
(521, [EatInstr(97,548)]);
(138, [EatInstr(119,186)]);
(522, [AAction2Instr(__a54,204)]);
(139, [EatInstr(105,187)]);
(523, [EatInstr(101,549)]);
(140, [EatInstr(101,188)]);
(524, [EatInstr(99,550)]);
(141, [EatInstr(114,189)]);
(525, [EatInstr(116,551)]);
(142, [EatInstr(103,190)]);
(526, [EatInstr(103,552)]);
(143, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,191)]);
(527, [EatInstr(111,553)]);
(144, [EatInstr(114,192)]);
(528, [EatInstr(115,554)]);
(145, [EatInstr(110,193)]);
(529, [AAction2Instr(__a55,235)]);
(146, [EatInstr(114,194)]);
(530, [EatInstr(101,555)]);
(147, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,195)]);
(531, [EatInstr(116,556)]);
(148, [EatInstr(115,196)]);
(532, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,557)]);
(149, [EatInstr(101,197)]);
(533, [AAction2Instr(__a56,235)]);
(150, [EatInstr(97,198)]);
(534, [EatInstr(97,558)]);
(151, [EatInstr(101,199)]);
(535, [EatInstr(111,559)]);
(152, [EatInstr(108,200)]);
(536, [AAction2Instr(__a57,235)]);
(153, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,201)]);
(537, [EatInstr(105,560)]);
(154, [EatInstr(105,202)]);
(538, [AAction2Instr(__a58,235)]);
(155, [EatInstr(117,203)]);
(539, [EatInstr(114,561)]);
(156, [AAction2Instr(__a8,204)]);
(540, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,562)]);
(157, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,205)]);
(541, [AAction2Instr(__a59,563)]);
(158, [EatInstr(97,206)]);
(542, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,564)]);
(159, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,207)]);
(543, [EatInstr(114,565)]);
(160, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,208)]);
(544, [EatInstr(101,566)]);
(161, [EatInstr(97,209)]);
(545, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,567)]);
(162, [EatInstr(108,210)]);
(546, [EatInstr(121,568)]);
(163, [EatInstr(101,211)]);
(547, [AAction2Instr(__a60,204)]);
(164, [EatInstr(116,212)]);
(548, [EatInstr(108,569)]);
(165, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,259)]);
(549, [EatInstr(45,570)]);
(166, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,213)]);
(550, [EatInstr(101,571)]);
(167, [EatInstr(112,214)]);
(551, [EatInstr(105,572)]);
(168, [EatInstr(115,215)]);
(552, [EatInstr(101,573)]);
(169, [EatInstr(101,216)]);
(553, [EatInstr(110,574)]);
(170, [EatInstr(111,217)]);
(554, [EatInstr(116,575)]);
(171, [EatInstr(107,218)]);
(555, [EatInstr(114,576)]);
(172, [EatInstr(101,219)]);
(556, [EatInstr(105,577)]);
(173, [EatInstr(99,220)]);
(557, [AAction2Instr(__a61,235)]);
(174, [EatInstr(110,221)]);
(558, [EatInstr(114,578)]);
(175, [EatInstr(105,222)]);
(559, [EatInstr(114,579)]);
(176, [EatInstr(107,223)]);
(560, [EatInstr(115,580)]);
(177, [EatInstr(111,224)]);
(561, [EatInstr(121,581)]);
(178, [EatInstr(115,228);EatInstr(114,227);EatInstr(109,226);EatInstr(99,225)]);
(562, [AAction2Instr(__a62,235)]);
(179, [EatInstr(121,229)]);
(563, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,598)]);
(180, [EatInstr(102,230)]);
(564, [AAction2Instr(__a63,244)]);
(181, [EatInstr(116,231)]);
(565, [EatInstr(101,582)]);
(182, [EatInstr(116,232)]);
(566, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,583)]);
(183, [EatInstr(111,233)]);
(567, [AAction2Instr(__a64,244)]);
(184, [EatInstr(45,234)]);
(568, [EatInstr(115,584)]);
(185, [AAction2Instr(__a9,235)]);
(569, [EatInstr(121,585)]);
(186, [EatInstr(45,236)]);
(570, [EatInstr(112,586)]);
(187, [EatInstr(98,237)]);
(571, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,587)]);
(188, [EatInstr(45,238)]);
(572, [EatInstr(111,588)]);
(189, [EatInstr(117,239)]);
(573, [EatInstr(110,589)]);
(190, [EatInstr(97,240)]);
(574, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,590)]);
(191, [AAction2Instr(__a10,244)]);
(575, [EatInstr(114,591)]);
(192, [EatInstr(45,241)]);
(576, [AAction2Instr(__a65,592)]);
(193, [EatInstr(101,242)]);
(577, [EatInstr(118,593)]);
(194, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,243)]);
(578, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,594)]);
(195, [AAction2Instr(__a11,244)]);
(579, [EatInstr(121,595)]);
(196, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,245)]);
(580, [EatInstr(116,596)]);
(197, [EatInstr(100,246)]);
(581, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,597)]);
(198, [EatInstr(121,247)]);
(582, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,600)]);
(199, [EatInstr(116,248)]);
(583, [AAction2Instr(__a66,244)]);
(200, [EatInstr(108,249)]);
(584, [EatInstr(105,601)]);
(201, [AAction2Instr(__a12,244)]);
(585, [EatInstr(115,602)]);
(202, [EatInstr(108,250)]);
(586, [EatInstr(114,603)]);
(203, [EatInstr(116,251)]);
(587, [AAction2Instr(__a67,204)]);
(204, [CompleteInstr(272)]);
(588, [EatInstr(110,604)]);
(205, [AAction2Instr(__a13,252)]);
(589, [EatInstr(45,606);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,605)]);
(206, [EatInstr(99,253)]);
(590, [AAction2Instr(__a68,235)]);
(207, [AAction2Instr(__a14,204)]);
(591, [EatInstr(105,607)]);
(208, [AAction2Instr(__a15,204)]);
(592, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,529)]);
(209, [EatInstr(104,254)]);
(593, [EatInstr(101,608)]);
(210, [EatInstr(111,255)]);
(594, [AAction2Instr(__a69,235)]);
(211, [EatInstr(100,256)]);
(595, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,609)]);
(212, [EatInstr(45,258);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,257)]);
(596, [EatInstr(111,610)]);
(213, [AAction2Instr(__a16,204)]);
(597, [AAction2Instr(__a70,235)]);
(214, [EatInstr(45,261)]);
(598, [AAction2Instr(__a71,599);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,598)]);
(215, [EatInstr(108,262)]);
(599, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,611)]);
(216, [EatInstr(114,263)]);
(600, [AAction2Instr(__a72,244)]);
(217, [EatInstr(119,264)]);
(601, [EatInstr(115,612)]);
(218, [EatInstr(101,265)]);
(602, [EatInstr(105,613)]);
(219, [EatInstr(45,266)]);
(603, [EatInstr(101,614)]);
(220, [EatInstr(107,267)]);
(604, [EatInstr(115,615)]);
(221, [EatInstr(116,268)]);
(605, [AAction2Instr(__a73,204)]);
(222, [EatInstr(110,269)]);
(606, [EatInstr(115,616)]);
(223, [EatInstr(97,270)]);
(607, [EatInstr(99,617)]);
(224, [EatInstr(105,271)]);
(608, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,618)]);
(225, [EatInstr(111,272)]);
(609, [AAction2Instr(__a74,235)]);
(226, [EatInstr(101,273)]);
(610, [EatInstr(114,619)]);
(227, [EatInstr(101,274)]);
(611, [AAction2Instr(__a75,235)]);
(228, [EatInstr(107,275)]);
(612, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,620)]);
(229, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,276)]);
(613, [EatInstr(115,621)]);
(230, [EatInstr(105,277)]);
(614, [EatInstr(100,622)]);
(231, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,278)]);
(615, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,623)]);
(232, [EatInstr(45,279)]);
(616, [EatInstr(99,624)]);
(233, [EatInstr(108,280)]);
(617, [EatInstr(116,625)]);
(234, [EatInstr(102,281)]);
(618, [AAction2Instr(__a76,235)]);
(235, [CompleteInstr(273)]);
(619, [EatInstr(121,626)]);
(236, [EatInstr(110,282)]);
(620, [AAction2Instr(__a77,204)]);
(237, [EatInstr(117,283)]);
(621, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,627)]);
(238, [EatInstr(117,284)]);
(622, [EatInstr(105,628)]);
(239, [EatInstr(108,285)]);
(623, [AAction2Instr(__a78,204)]);
(240, [EatInstr(114,286)]);
(624, [EatInstr(97,629)]);
(241, [EatInstr(116,288)]);
(625, [AAction2Instr(__a79,592)]);
(242, [EatInstr(45,289)]);
(626, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,630)]);
(243, [AAction2Instr(__a17,244)]);
(627, [AAction2Instr(__a80,204)]);
(244, [CompleteInstr(271)]);
(628, [EatInstr(99,631)]);
(245, [AAction2Instr(__a18,244)]);
(629, [EatInstr(110,632)]);
(246, [EatInstr(101,290)]);
(630, [AAction2Instr(__a81,235)]);
(247, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,291)]);
(631, [EatInstr(97,633)]);
(248, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,292)]);
(632, [EatInstr(110,634)]);
(249, [EatInstr(45,293)]);
(633, [EatInstr(116,635)]);
(250, [EatInstr(101,294)]);
(634, [EatInstr(101,636)]);
(251, [EatInstr(105,295)]);
(635, [EatInstr(101,637)]);
(252, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,296)]);
(636, [EatInstr(114,638)]);
(253, [EatInstr(116,297)]);
(637, [EatInstr(115,639)]);
(254, [EatInstr(101,298)]);
(638, [EatInstr(108,640)]);
(255, [EatInstr(111,299)]);
(639, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,641)]);
(256, [EatInstr(101,300)]);
(640, [EatInstr(101,642)]);
(257, [AAction2Instr(__a19,204)]);
(641, [AAction2Instr(__a82,204)]);
(258, [EatInstr(114,303);EatInstr(110,302);EatInstr(103,301)]);
(642, [EatInstr(115,643)]);
(259, [AAction2Instr(__a20,260);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,259)]);
(643, [EatInstr(115,644)]);
(260, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,304)]);
(644, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,645)]);
(261, [EatInstr(108,305)]);
(645, [AAction2Instr(__a83,204)]);
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
(276, [AAction2Instr(__a21,235)]);
(277, [EatInstr(120,320)]);
(278, [AAction2Instr(__a22,321)]);
(279, [EatInstr(104,322)]);
(280, [EatInstr(108,323)]);
(281, [EatInstr(115,324)]);
(282, [EatInstr(111,325)]);
(283, [EatInstr(116,326)]);
(284, [EatInstr(110,327)]);
(285, [EatInstr(101,328)]);
(286, [EatInstr(45,330);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,329)]);
(288, [EatInstr(121,331)]);
(289, [EatInstr(114,333);EatInstr(110,332)]);
(290, [EatInstr(110,334)]);
(291, [AAction2Instr(__a23,244)]);
(292, [AAction2Instr(__a24,244)]);
(293, [EatInstr(115,335)]);
(294, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,336)]);
(295, [EatInstr(110,337)]);
(296, [AAction2Instr(__a25,338)]);
(297, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,339)]);
(298, [EatInstr(97,340)]);
(299, [EatInstr(107,341)]);
(300, [EatInstr(110,342)]);
(301, [EatInstr(105,343)]);
(302, [EatInstr(117,345);EatInstr(112,344)]);
(303, [EatInstr(101,346)]);
(304, [AAction2Instr(__a26,204)]);
(305, [EatInstr(97,347)]);
(306, [EatInstr(116,348)]);
(307, [AContInstr3(271,__g0,__binder4,349);ACallInstr3(__g0,8)]);
(308, [EatInstr(110,350)]);
(309, [EatInstr(100,351)]);
(310, [EatInstr(110,352)]);
(311, [EatInstr(108,353)]);
(312, [EatInstr(114,354)]);
(313, [EatInstr(45,355)]);
(314, [EatInstr(101,356)]);
(315, [EatInstr(101,357)]);
(316, [EatInstr(108,358)]);
(317, [EatInstr(111,359)]);
(318, [EatInstr(108,360)]);
(319, [EatInstr(112,361)]);
(320, [EatInstr(45,362)]);
(321, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,363)]);
(322, [EatInstr(105,364)]);
(323, [EatInstr(45,365)]);
(324, [EatInstr(116,367);EatInstr(109,366)]);
(325, [EatInstr(116,368)]);
(326, [EatInstr(101,369)]);
(327, [EatInstr(100,370)]);
(328, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,371)]);
(329, [AAction2Instr(__a27,244)]);
(330, [EatInstr(103,372)]);
(331, [EatInstr(112,373)]);
(332, [EatInstr(117,374)]);
(333, [EatInstr(101,375)]);
(334, [EatInstr(99,376)]);
(335, [EatInstr(116,377)]);
(336, [AAction2Instr(__a28,204)]);
(337, [EatInstr(101,378)]);
(338, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,379)]);
(339, [AAction2Instr(__a29,204)]);
(340, [EatInstr(100,380)]);
(341, [EatInstr(97,381)]);
(342, [EatInstr(99,382)]);
(343, [EatInstr(108,383)]);
(344, [EatInstr(114,384)]);
(345, [EatInstr(108,385)]);
(346, [EatInstr(108,386)]);
(347, [EatInstr(116,387)]);
(348, [EatInstr(101,388)]);
(349, [AAction2Instr(__a30,235)]);
(350, [EatInstr(111,389)]);
(351, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,390)]);
(352, [EatInstr(115,391)]);
(353, [EatInstr(97,392)]);
(354, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,393)]);
(355, [EatInstr(114,395);EatInstr(99,394)]);
(356, [EatInstr(97,396)]);
(357, [EatInstr(45,397)]);
(358, [EatInstr(101,398)]);
(359, [EatInstr(105,399)]);
(360, [EatInstr(97,400)]);
(361, [EatInstr(45,401)]);
(362, [EatInstr(104,402)]);
(363, [AAction2Instr(__a31,403)]);
(364, [EatInstr(115,404)]);
(365, [EatInstr(115,405)]);
(366, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,406)]);
(367, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,407)]);
(368, [EatInstr(97,408)]);
(369, [EatInstr(115,409)]);
(370, [EatInstr(101,410)]);
(371, [AAction2Instr(__a32,244)]);
(372, [EatInstr(105,411)]);
(373, [EatInstr(101,412)]);
(374, [EatInstr(108,413)]);
(375, [EatInstr(103,414)]);
(376, [EatInstr(101,415)]);
(377, [EatInstr(97,416)]);
(378, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,417)]);
(379, [AAction2Instr(__a34,204);AAction2Instr(__a33,418)]);
(380, [EatInstr(45,419)]);
(381, [EatInstr(104,420)]);
(382, [EatInstr(101,421)]);
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
