
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
| Dependency_graph_cmd
| Dot_cmd
| Exec_cmd
| Extract_cmd
| Fuse_cmd
| Generators_analysis_cmd
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
| Parse_cmd
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
 | (2023) -> ((); ();  Dependency_graph_cmd )
 | (2024) -> ((); ();  Dot_cmd )
 | (2025) -> ((); (); (let _x4 = _p() in (); (let _x3 = _p() in (let f = Yak.YkBuf.get_string _x4 _x3 ykinput in (let l = (let _x8 = (let rec _x19 _x8 =
(match _n() with (2029) -> _x8 | _ (*2028*) ->
 _x19((let _x7 = (); (let _x6 = _p() in (); (let _x5 = _p() in (let x = Yak.YkBuf.get_string _x6 _x5 ykinput in x))) in _x7::_x8)))
in _x19(Yak.Util.nil)) in (List.rev _x8)) in ();  files := f::!files; exec_l := l; Exec_cmd )))))
 | (2032) -> ((); ();  Extract_cmd )
 | (2033) -> ((); ();  Compileopt.coalesce := true; Fuse_cmd )
 | (2034) -> ((); ();  Generators_analysis_cmd )
 | (2035) -> ((); ();  Info_cmd )
 | (2036) -> ((); ();  Lookahead_analysis_cmd )
 | (2037) -> ((); ();  Lr1_lookahead_cmd )
 | (2038) -> ((); ();  Parse_cmd )
 | (2039) -> ((); ();  Precedence_analysis_cmd )
 | (2040) -> ((); ();  Print_gul_cmd )
 | (2041) -> ((); ();  Print_gil_cmd )
 | (2042) -> ((); ();  Print_npreds_cmd )
 | (2043) -> ((); ();  Print_npreds_cmd )
 | (2044) -> ((); ();  Print_relevance_cmd )
 | (2045) -> ((); (let _x10 = _p() in (); (let _x9 = _p() in (let n = Yak.YkBuf.get_string _x10 _x9 ykinput in ();  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" ))))
 | (2048) -> ((); ();  Sort_cmd )
 | (2049) -> ((); ();  Strip_late_actions_cmd )
 | (2050) -> ((); ();  Translate_dypgen_cmd )
 | (2051) -> ((); ();  Translate_dypgen_scannerless_cmd )
 | _ -> raise Exit)
and
 _r_args(_n,_p,ykinput) = (match _n() with
 | (2052) -> ((); (); (let p = _r_phases(_n,_p,ykinput) in  after := Some p ; ()))
 | (2053) -> ((); ();  Compileopt.use_wrap_and_attr := false;      
                                            Compileopt.use_coroutines    := false ; ())
 | (2054) -> ((); ();  Compileopt.use_coroutines := false ; ())
 | (2055) -> ((); (); (let b = (match _n() with
 | (2056) -> ((); Fun_BE)
 | (2057) -> ((); Trans_BE)
 | (2058) -> ((); Wadler_BE)
 | (2059) -> ((); Peg_BE false)
 | (2060) -> ((); Peg_BE true)
 | _ -> raise Exit) in ();  backend := b ; ()))
 | (2061) -> ((); ();  Compileopt.case_sensitive := false ; ())
 | (2062) -> ((); ();  Compileopt.check_labels := true ; ())
 | (2063) -> ((); (); (let _x12 = _p() in (); (let _x11 = _p() in (let n = Yak.YkBuf.get_string _x12 _x11 ykinput in ();  Variables.counter := (int_of_string n) ; ()))))
 | (2066) -> ((); ();  Compileopt.inline_cs := true ; ())
 | (2067) -> ((); ();  Compileopt.inline_regular := true ; ())
 | (2068) -> ((); ();  Compileopt.lookahead := true ; ())
 | (2069) -> ((); ();  Compileopt.memoize_history := true ; ())
 | (2070) -> ((); ();  Compileopt.coalesce := false ; ())
 | (2071) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2072) -> ((); ();  Compileopt.repress_replay := true ; ())
 | (2073) -> ((); ();  Compileopt.skip_opt := false ; ())
 | (2074) -> ((); ();  only := true ; ())
 | (2075) -> ((); ();  Compileopt.postfix_history := false ; ())
 | (2076) -> ((); (); (let _x14 = _p() in (); (let _x13 = _p() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2079) -> ((); ();  Compileopt.unit_history := true ; ())
 | (2080) -> ((); (); (let _x16 = _p() in (); (let _x15 = _p() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  Compileopt.unroll_star_n := (int_of_string n) ; ()))))
 | (2083) -> ((); ();  Compileopt.use_fsm := true ; ())
 | (2084) -> ((); ();  Compileopt.use_fsm := false ; ())
 | (2085) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2086) -> ((let _x18 = _p() in (); (let _x17 = _p() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
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
 | (2024) -> (();();(); push((2024)))
 | (2025) -> (();();();push((2029)); while (match _n() with (2028) -> true | _ (*2029*)-> false) do
 ();();();push_pos(_p());();push_pos(_p());(); push((2028))
done
;();push_pos(_p());();push_pos(_p());();(); push((2025)))
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
 | (2042) -> (();();(); push((2042)))
 | (2043) -> (();();(); push((2043)))
 | (2044) -> (();();(); push((2044)))
 | (2045) -> (();();();push_pos(_p());();push_pos(_p());(); push((2045)))
 | (2048) -> (();();(); push((2048)))
 | (2049) -> (();();(); push((2049)))
 | (2050) -> (();();(); push((2050)))
 | (2051) -> (();();(); push((2051)))
 | _ -> raise Exit)
and _rv_args() = (match _n() with
 | (2052) -> (();();_rv_phases();();(); push((2052)))
 | (2053) -> (();();();(); push((2053)))
 | (2054) -> (();();();(); push((2054)))
 | (2055) -> (();();();(match _n() with
 | (2056) -> (();(); push((2056)))
 | (2057) -> (();(); push((2057)))
 | (2058) -> (();(); push((2058)))
 | (2059) -> (();(); push((2059)))
 | (2060) -> (();(); push((2060)))
 | _ -> raise Exit);();(); push((2055)))
 | (2061) -> (();();();(); push((2061)))
 | (2062) -> (();();();(); push((2062)))
 | (2063) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2063)))
 | (2066) -> (();();();(); push((2066)))
 | (2067) -> (();();();(); push((2067)))
 | (2068) -> (();();();(); push((2068)))
 | (2069) -> (();();();(); push((2069)))
 | (2070) -> (();();();(); push((2070)))
 | (2071) -> (();();();(); push((2071)))
 | (2072) -> (();();();(); push((2072)))
 | (2073) -> (();();();(); push((2073)))
 | (2074) -> (();();();(); push((2074)))
 | (2075) -> (();();();(); push((2075)))
 | (2076) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2076)))
 | (2079) -> (();();();(); push((2079)))
 | (2080) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2080)))
 | (2083) -> (();();();(); push((2083)))
 | (2084) -> (();();();(); push((2084)))
 | (2085) -> (();();();(); push((2085)))
 | (2086) -> (();();();();push_pos(_p());();push_pos(_p()); push((2086)))
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
let __a72 = (_p 1076 ((2067)));;
let __a36 = (_p 1080 ((2064)));;
let __a86 = (_p 1030 ((2043)));;
let __a53 = (_p 1084 ((2060)));;
let __a1 = (_p 1057 ((2087)));;
let __a40 = (_p 1032 ((2041)));;
let __a80 = (_p 1082 ((2061)));;
let __a27 = (_p 1026 ((2045)));;
let __a83 = (_p 1085 ((2059)));;
let __a60 = (_p 1063 ((2081)));;
let __a38 = (_p 1059 ((2084)));;
let __a12 = (_p 1005 ((2018)));;
let __a57 = (_p 1078 ((2063)));;
let __a25 = (_p 1007 ((2016)));;
let __a45 = (_p 1087 ((2057)));;
let __a19 = (_p 1035 ((2038)));;
let __a18 = (_p 1009 ((2014)));;
let __a34 = (_p 1045 ((2030)));;
let __a81 = (_p 1037 ((2036)));;
let __a67 = (_p 1039 ((2034)));;
let __a9 = (_p 1055 ((2086)));;
let __a17 = (_p 1011 ((2012)));;
let __a66 = (_p 1013 ((2010)));;
let __a63 = (_p 1089 ((2054)));;
let __a32 = (_p 1066 ((2078)));;
let __a30 = (_p 1041 ((2032)));;
let __a64 = (_p 1064 ((2079)));;
let __a31 = (_p 1091 ((2052)));;
let __a79 = (_p 1061 ((2080)));;
let __a73 = (_p 1068 ((2075)));;
let __a50 = (_p 1014 ((2009)));;
let __a13 = (_p 1048 ((2026)));;
let __a44 = (fun _x0_ _x1_ -> (((_p 1043 ((2028))) _x0_) (((_p 1044 ((2031))) _x0_) _x1_)));;
let __a10 = (_p 1016 ((2007)));;
let __a87 = (_p 1022 ((2051)));;
let __a59 = (_p 1070 ((2073)));;
let __a28 = (_p 1018 ((2005)));;
let __a85 = (_p 1072 ((2071)));;
let __a75 = (_p 1020 ((2003)));;
let __a26 = (fun _x0_ _x1_ -> (((_p 1046 ((2029))) _x0_) (((_p 1047 ((2027))) _x0_) _x1_)));;
let __a21 = (_p 1027 ((2047)));;
let __a76 = (_p 1050 ((2023)));;
let __a2 = (_p 1002 ((2001)));;
let __a54 = (_p 1079 ((2065)));;
let __a29 = (_p 1052 ((2021)));;
let __a16 = (_p 1025 ((2048)));;
let __a56 = (_p 1083 ((2055)));;
let __a47 = (_p 1075 ((2068)));;
let __a70 = (_p 1029 ((2044)));;
let __a46 = (_p 1077 ((2066)));;
let __a4 = (_p 1056 ((2088)));;
let __a55 = (_p 1031 ((2042)));;
let __a62 = (_p 1081 ((2062)));;
let __a20 = (_p 1033 ((2040)));;
let __a74 = (_p 1062 ((2082)));;
let __a24 = (_p 1004 ((2019)));;
let __a5 = (_p 1000 ((2000)));;
let __a7 = (_p 1058 ((2085)));;
let __a68 = (_p 1086 ((2058)));;
let __a51 = (_p 1006 ((2017)));;
let __a84 = (_p 1034 ((2039)));;
let __a52 = (_p 1088 ((2056)));;
let __a37 = (_p 1060 ((2083)));;
let __a43 = (_p 1008 ((2015)));;
let __a61 = (_p 1036 ((2037)));;
let __a11 = (_p 1010 ((2013)));;
let __a15 = (_p 1038 ((2035)));;
let __a69 = (_p 1012 ((2011)));;
let __a35 = (_p 1042 ((2025)));;
let __a14 = (_p 1040 ((2033)));;
let __a23 = (_p 1067 ((2077)));;
let __a71 = (_p 1090 ((2053)));;
let __a65 = (_p 1015 ((2008)));;
let __a22 = (_p 1069 ((2074)));;
let __g0 = (_e);;
let __a49 = (_p 1017 ((2006)));;
let __a77 = (_p 1023 ((2050)));;
let __a48 = (_p 1071 ((2072)));;
let __a33 = (_p 1019 ((2004)));;
let __a58 = (_p 1073 ((2070)));;
let __a8 = (_p 1049 ((2024)));;
let __a41 = (_p 1065 ((2076)));;
let __a42 = (_p 1021 ((2002)));;
let __a6 = (_p 1028 ((2046)));;
let __a82 = (_p 1024 ((2049)));;
let __a39 = (_p 1051 ((2022)));;
let __a78 = (_p 1074 ((2069)));;
let __a3 = (_p 1053 ((2020)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1003);;
let __binder2 = (_m 1054);;
let __binder3 = (_m 1001);;
let __binder4 = (_m 1092);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(97,426)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(45,427)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(101,428)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(105,429)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(97,430)]);
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [EatInstr(45,431)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(104,432)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,44)]);
(390, [AAction2Instr(__a32,433)]);
(7, [EatInstr(127,46);EatInstr(126,46);EatInstr(125,46);EatInstr(124,46);EatInstr(123,46);EatInstr(122,46);EatInstr(121,46);EatInstr(120,46);EatInstr(119,46);EatInstr(118,46);EatInstr(117,46);EatInstr(116,46);EatInstr(115,46);EatInstr(114,46);EatInstr(113,46);EatInstr(112,46);EatInstr(111,46);EatInstr(110,46);EatInstr(109,46);EatInstr(108,46);EatInstr(107,46);EatInstr(106,46);EatInstr(105,46);EatInstr(104,46);EatInstr(103,46);EatInstr(102,46);EatInstr(101,46);EatInstr(100,46);EatInstr(99,46);EatInstr(98,46);EatInstr(97,46);EatInstr(96,46);EatInstr(95,46);EatInstr(94,46);EatInstr(93,46);EatInstr(92,46);EatInstr(91,46);EatInstr(90,46);EatInstr(89,46);EatInstr(88,46);EatInstr(87,46);EatInstr(86,46);EatInstr(85,46);EatInstr(84,46);EatInstr(83,46);EatInstr(82,46);EatInstr(81,46);EatInstr(80,46);EatInstr(79,46);EatInstr(78,46);EatInstr(77,46);EatInstr(76,46);EatInstr(75,46);EatInstr(74,46);EatInstr(73,46);EatInstr(72,46);EatInstr(71,46);EatInstr(70,46);EatInstr(69,46);EatInstr(68,46);EatInstr(67,46);EatInstr(66,46);EatInstr(65,46);EatInstr(64,46);EatInstr(63,46);EatInstr(62,46);EatInstr(61,46);EatInstr(60,46);EatInstr(59,46);EatInstr(58,46);EatInstr(57,46);EatInstr(56,46);EatInstr(55,46);EatInstr(54,46);EatInstr(53,46);EatInstr(52,46);EatInstr(51,46);EatInstr(50,46);EatInstr(49,46);EatInstr(48,46);EatInstr(47,46);EatInstr(46,46);EatInstr(44,46);EatInstr(43,46);EatInstr(42,46);EatInstr(41,46);EatInstr(40,46);EatInstr(39,46);EatInstr(38,46);EatInstr(37,46);EatInstr(36,46);EatInstr(35,46);EatInstr(34,46);EatInstr(33,46);EatInstr(32,46);EatInstr(31,46);EatInstr(30,46);EatInstr(29,46);EatInstr(28,46);EatInstr(27,46);EatInstr(26,46);EatInstr(25,46);EatInstr(24,46);EatInstr(23,46);EatInstr(22,46);EatInstr(21,46);EatInstr(20,46);EatInstr(19,46);EatInstr(18,46);EatInstr(17,46);EatInstr(16,46);EatInstr(15,46);EatInstr(14,46);EatInstr(13,46);EatInstr(12,46);EatInstr(11,46);EatInstr(10,46);EatInstr(9,46);EatInstr(8,46);EatInstr(7,46);EatInstr(6,46);EatInstr(5,46);EatInstr(4,46);EatInstr(3,46);EatInstr(2,46);EatInstr(1,46)]);
(391, [EatInstr(115,434)]);
(8, [EatInstr(119,28);EatInstr(117,27);EatInstr(115,26);EatInstr(114,25);EatInstr(112,24);EatInstr(109,23);EatInstr(108,22);EatInstr(105,21);EatInstr(104,20);EatInstr(100,19);EatInstr(99,18);EatInstr(97,17)]);
(392, [EatInstr(115,435)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(103,33);EatInstr(102,32);EatInstr(101,31);EatInstr(100,30);EatInstr(99,29);AContInstr3(271,__g0,__binder2,40);ACallInstr3(__g0,8)]);
(393, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,436)]);
(10, [EatInstr(45,41);AAction2Instr(__a1,42)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43)]);
(394, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,437)]);
(395, [EatInstr(97,438)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(115,439)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(101,440)]);
(14, [CompleteInstr(266)]);
(398, [AAction2Instr(__a33,258)]);
(15, [AAction2Instr(__a2,147)]);
(399, [EatInstr(105,441)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(101,442)]);
(17, [EatInstr(116,49);EatInstr(114,48)]);
(401, [EatInstr(108,443)]);
(18, [EatInstr(111,51);EatInstr(108,50)]);
(402, [EatInstr(103,444)]);
(19, [EatInstr(101,52)]);
(403, [EatInstr(101,445)]);
(20, [EatInstr(97,53)]);
(404, [EatInstr(97,446)]);
(21, [EatInstr(110,54)]);
(405, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,447)]);
(22, [EatInstr(105,56);EatInstr(101,55)]);
(406, [EatInstr(121,448)]);
(23, [EatInstr(105,57)]);
(407, [AAction2Instr(__a35,78);AAction2Instr(__a34,449)]);
(24, [EatInstr(114,58)]);
(408, [EatInstr(97,450)]);
(25, [EatInstr(101,59)]);
(409, [EatInstr(45,451)]);
(26, [EatInstr(117,60)]);
(410, [EatInstr(104,452)]);
(27, [EatInstr(110,61)]);
(411, [EatInstr(101,453)]);
(28, [EatInstr(114,62)]);
(412, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,454)]);
(29, [EatInstr(111,63)]);
(413, [EatInstr(101,455)]);
(30, [EatInstr(111,65);EatInstr(101,64)]);
(414, [EatInstr(108,456)]);
(31, [EatInstr(120,66)]);
(415, [EatInstr(101,457)]);
(32, [EatInstr(117,67)]);
(416, [EatInstr(101,458)]);
(33, [EatInstr(101,68)]);
(417, [EatInstr(45,459)]);
(34, [EatInstr(110,69)]);
(418, [EatInstr(116,460)]);
(35, [EatInstr(114,71);EatInstr(111,70)]);
(419, [EatInstr(119,464);EatInstr(116,463);EatInstr(112,462);EatInstr(102,461)]);
(36, [EatInstr(114,73);EatInstr(97,72)]);
(420, [EatInstr(101,465)]);
(37, [EatInstr(102,74)]);
(421, [EatInstr(98,466)]);
(38, [EatInstr(116,76);EatInstr(111,75)]);
(422, [AAction2Instr(__a36,467)]);
(39, [EatInstr(114,77)]);
(423, [EatInstr(114,468)]);
(40, [AAction2Instr(__a3,78)]);
(424, [EatInstr(115,469)]);
(41, [EatInstr(118,91);EatInstr(117,90);EatInstr(114,89);EatInstr(112,88);EatInstr(111,87);EatInstr(110,86);EatInstr(109,85);EatInstr(108,84);EatInstr(105,83);EatInstr(104,82);EatInstr(99,81);EatInstr(98,80);EatInstr(97,79)]);
(425, [EatInstr(101,470)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,92)]);
(426, [EatInstr(100,471)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(104,472)]);
(44, [ALookaheadInstr(false,CfgLA (1,264),45);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,44)]);
(428, [EatInstr(115,473)]);
(45, [CompleteInstr(269)]);
(429, [EatInstr(122,474)]);
(46, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,46)]);
(430, [EatInstr(121,475)]);
(47, [CompleteInstr(270)]);
(431, [EatInstr(111,476)]);
(48, [EatInstr(114,95)]);
(432, [EatInstr(105,477)]);
(49, [EatInstr(116,96)]);
(433, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,478)]);
(50, [EatInstr(111,97)]);
(434, [EatInstr(116,479)]);
(51, [EatInstr(112,98)]);
(435, [EatInstr(116,480)]);
(52, [EatInstr(115,99)]);
(436, [AAction2Instr(__a37,199)]);
(53, [EatInstr(115,100)]);
(437, [AAction2Instr(__a38,199)]);
(54, [EatInstr(108,102);EatInstr(102,101)]);
(438, [EatInstr(116,481)]);
(55, [EatInstr(120,103)]);
(439, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,482)]);
(56, [EatInstr(102,104)]);
(440, [EatInstr(114,483)]);
(57, [EatInstr(110,105)]);
(441, [EatInstr(108,484)]);
(58, [EatInstr(101,106)]);
(442, [EatInstr(115,485)]);
(59, [EatInstr(112,107)]);
(443, [EatInstr(108,486)]);
(60, [EatInstr(98,108)]);
(444, [EatInstr(117,487)]);
(61, [EatInstr(114,109)]);
(445, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,488)]);
(62, [EatInstr(97,110)]);
(446, [EatInstr(114,489)]);
(63, [EatInstr(114,112);EatInstr(109,111)]);
(447, [AAction2Instr(__a39,78)]);
(64, [EatInstr(112,113)]);
(448, [EatInstr(45,490)]);
(65, [EatInstr(116,114)]);
(449, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,491)]);
(66, [EatInstr(116,116);EatInstr(101,115)]);
(450, [EatInstr(116,492)]);
(67, [EatInstr(115,117)]);
(451, [EatInstr(97,493)]);
(68, [EatInstr(116,118)]);
(452, [EatInstr(101,494)]);
(69, [EatInstr(102,119)]);
(453, [EatInstr(45,495)]);
(70, [EatInstr(111,120)]);
(454, [AAction2Instr(__a40,78)]);
(71, [EatInstr(49,121)]);
(455, [EatInstr(100,496)]);
(72, [EatInstr(114,122)]);
(456, [EatInstr(97,497)]);
(73, [EatInstr(105,124);EatInstr(101,123)]);
(457, [EatInstr(118,498)]);
(74, [EatInstr(99,125)]);
(458, [EatInstr(45,499)]);
(75, [EatInstr(114,126)]);
(459, [EatInstr(100,500)]);
(76, [EatInstr(114,127)]);
(460, [EatInstr(97,501)]);
(77, [EatInstr(97,128)]);
(461, [EatInstr(117,502)]);
(78, [CompleteInstr(272)]);
(462, [EatInstr(101,503)]);
(79, [EatInstr(114,130);EatInstr(102,129)]);
(463, [EatInstr(120,504)]);
(80, [EatInstr(97,131)]);
(464, [EatInstr(97,505)]);
(81, [EatInstr(111,134);EatInstr(104,133);EatInstr(97,132)]);
(465, [EatInstr(110,506)]);
(82, [EatInstr(121,135)]);
(466, [EatInstr(101,507)]);
(83, [EatInstr(110,136)]);
(467, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,543)]);
(84, [EatInstr(111,137)]);
(468, [EatInstr(114,508)]);
(85, [EatInstr(101,138)]);
(469, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,509)]);
(86, [EatInstr(111,139)]);
(470, [EatInstr(103,510)]);
(87, [EatInstr(110,140)]);
(471, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,511)]);
(88, [EatInstr(114,141)]);
(472, [EatInstr(105,512)]);
(89, [EatInstr(111,142)]);
(473, [EatInstr(99,513)]);
(90, [EatInstr(115,144);EatInstr(110,143)]);
(474, [EatInstr(101,514)]);
(91, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,145)]);
(475, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,515)]);
(92, [AAction2Instr(__a4,146)]);
(476, [EatInstr(112,516)]);
(93, [AAction2Instr(__a5,147)]);
(477, [EatInstr(115,517)]);
(94, [CompleteInstr(267)]);
(478, [AAction2Instr(__a41,199)]);
(95, [EatInstr(111,148)]);
(479, [EatInstr(111,518)]);
(96, [EatInstr(114,149)]);
(480, [EatInstr(97,519)]);
(97, [EatInstr(115,150)]);
(481, [EatInstr(105,520)]);
(98, [EatInstr(121,151)]);
(482, [AAction2Instr(__a42,258)]);
(99, [EatInstr(117,152)]);
(483, [EatInstr(45,521)]);
(100, [EatInstr(104,153)]);
(484, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,522)]);
(101, [EatInstr(101,154)]);
(485, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,523)]);
(102, [EatInstr(105,155)]);
(486, [EatInstr(97,524)]);
(103, [EatInstr(101,156)]);
(487, [EatInstr(108,525)]);
(104, [EatInstr(116,157)]);
(488, [AAction2Instr(__a43,258)]);
(105, [EatInstr(117,158)]);
(489, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,526)]);
(106, [EatInstr(99,159)]);
(490, [EatInstr(103,527)]);
(107, [EatInstr(108,160)]);
(491, [AAction2Instr(__a44,363)]);
(108, [EatInstr(115,161)]);
(492, [EatInstr(111,528)]);
(109, [EatInstr(111,162)]);
(493, [EatInstr(110,529)]);
(110, [EatInstr(112,163)]);
(494, [EatInstr(97,530)]);
(111, [EatInstr(112,164)]);
(495, [EatInstr(97,531)]);
(112, [EatInstr(111,165)]);
(496, [EatInstr(115,532)]);
(113, [EatInstr(101,166)]);
(497, [EatInstr(98,533)]);
(114, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,167)]);
(498, [EatInstr(97,534)]);
(115, [EatInstr(99,168)]);
(499, [EatInstr(97,535)]);
(116, [EatInstr(114,169)]);
(500, [EatInstr(121,536)]);
(117, [EatInstr(101,170)]);
(501, [EatInstr(116,537)]);
(118, [EatInstr(45,171)]);
(502, [EatInstr(110,538)]);
(119, [EatInstr(111,172)]);
(503, [EatInstr(103,539)]);
(120, [EatInstr(107,173)]);
(504, [AAction2Instr(__a45,640)]);
(121, [EatInstr(45,174)]);
(505, [EatInstr(100,540)]);
(122, [EatInstr(115,175)]);
(506, [EatInstr(115,541)]);
(123, [EatInstr(99,176)]);
(507, [EatInstr(108,542)]);
(124, [EatInstr(110,177)]);
(508, [EatInstr(111,545)]);
(125, [AAction2Instr(__a6,178)]);
(509, [AAction2Instr(__a46,199)]);
(126, [EatInstr(116,179)]);
(510, [EatInstr(117,546)]);
(127, [EatInstr(105,180)]);
(511, [AAction2Instr(__a47,199)]);
(128, [EatInstr(110,181)]);
(512, [EatInstr(115,547)]);
(129, [EatInstr(116,182)]);
(513, [EatInstr(101,548)]);
(130, [EatInstr(114,183)]);
(514, [EatInstr(45,549)]);
(131, [EatInstr(99,184)]);
(515, [AAction2Instr(__a48,199)]);
(132, [EatInstr(115,185)]);
(516, [EatInstr(116,550)]);
(133, [EatInstr(101,186)]);
(517, [EatInstr(116,551)]);
(134, [EatInstr(117,187)]);
(518, [EatInstr(114,552)]);
(135, [EatInstr(98,188)]);
(519, [EatInstr(114,553)]);
(136, [EatInstr(108,189)]);
(520, [EatInstr(111,554)]);
(137, [EatInstr(111,190)]);
(521, [EatInstr(99,555)]);
(138, [EatInstr(109,191)]);
(522, [AAction2Instr(__a49,258)]);
(139, [EatInstr(45,192)]);
(523, [AAction2Instr(__a50,258)]);
(140, [EatInstr(108,193)]);
(524, [EatInstr(98,556)]);
(141, [EatInstr(101,194)]);
(525, [EatInstr(97,557)]);
(142, [EatInstr(111,195)]);
(526, [AAction2Instr(__a51,258)]);
(143, [EatInstr(114,197);EatInstr(105,196)]);
(527, [EatInstr(114,558)]);
(144, [EatInstr(101,198)]);
(528, [EatInstr(114,559)]);
(145, [AAction2Instr(__a7,199)]);
(529, [EatInstr(97,560)]);
(146, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,200)]);
(530, [EatInstr(100,561)]);
(147, [ALookaheadInstr(false,CfgLA (3,266),94);ACallInstr3(__default_call,11);AContInstr3(273,__g0,__binder3,93);ACallInstr3(__g0,10);ASimpleCont2Instr(274,__binder0,94)]);
(531, [EatInstr(110,562)]);
(148, [EatInstr(119,201)]);
(532, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,563)]);
(149, [EatInstr(105,202)]);
(533, [EatInstr(108,564)]);
(150, [EatInstr(101,203)]);
(534, [EatInstr(110,565)]);
(151, [EatInstr(114,204)]);
(535, [EatInstr(99,566)]);
(152, [EatInstr(103,205)]);
(536, [EatInstr(112,567)]);
(153, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,206)]);
(537, [EatInstr(105,568)]);
(154, [EatInstr(114,207)]);
(538, [AAction2Instr(__a52,640)]);
(155, [EatInstr(110,208)]);
(539, [EatInstr(45,569);AAction2Instr(__a53,640)]);
(156, [EatInstr(114,209)]);
(540, [EatInstr(108,571)]);
(157, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,210)]);
(541, [EatInstr(105,572)]);
(158, [EatInstr(115,211)]);
(542, [EatInstr(115,573)]);
(159, [EatInstr(101,212)]);
(543, [AAction2Instr(__a54,544);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,543)]);
(160, [EatInstr(97,213)]);
(544, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,574)]);
(161, [EatInstr(101,214)]);
(545, [EatInstr(119,575)]);
(162, [EatInstr(108,215)]);
(546, [EatInstr(108,576)]);
(163, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,216)]);
(547, [EatInstr(116,577)]);
(164, [EatInstr(105,217)]);
(548, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,578)]);
(165, [EatInstr(117,218)]);
(549, [EatInstr(104,579)]);
(166, [EatInstr(110,219)]);
(550, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,580)]);
(167, [AAction2Instr(__a8,78)]);
(551, [EatInstr(111,581)]);
(168, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,220)]);
(552, [EatInstr(121,582)]);
(169, [EatInstr(97,221)]);
(553, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,583)]);
(170, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,222)]);
(554, [EatInstr(110,584)]);
(171, [EatInstr(103,223)]);
(555, [EatInstr(111,585)]);
(172, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,224)]);
(556, [EatInstr(108,586)]);
(173, [EatInstr(97,225)]);
(557, [EatInstr(114,587)]);
(174, [EatInstr(108,226)]);
(558, [EatInstr(97,588)]);
(175, [EatInstr(101,227)]);
(559, [EatInstr(115,589)]);
(176, [EatInstr(101,228)]);
(560, [EatInstr(108,590)]);
(177, [EatInstr(116,229)]);
(561, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,591)]);
(178, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,279)]);
(562, [EatInstr(97,592)]);
(179, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,230)]);
(563, [AAction2Instr(__a55,78)]);
(180, [EatInstr(112,231)]);
(564, [EatInstr(101,593)]);
(181, [EatInstr(115,232)]);
(565, [EatInstr(99,594)]);
(182, [EatInstr(101,233)]);
(566, [EatInstr(116,595)]);
(183, [EatInstr(111,234)]);
(567, [EatInstr(103,596)]);
(184, [EatInstr(107,235)]);
(568, [EatInstr(111,597)]);
(185, [EatInstr(101,236)]);
(569, [EatInstr(115,598)]);
(186, [EatInstr(99,237)]);
(570, [AAction2Instr(__a56,199)]);
(187, [EatInstr(110,238)]);
(571, [EatInstr(101,599)]);
(188, [EatInstr(114,239)]);
(572, [EatInstr(116,600)]);
(189, [EatInstr(105,240)]);
(573, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,601)]);
(190, [EatInstr(107,241)]);
(574, [AAction2Instr(__a57,199)]);
(191, [EatInstr(111,242)]);
(575, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,602)]);
(192, [EatInstr(115,246);EatInstr(114,245);EatInstr(109,244);EatInstr(99,243)]);
(576, [EatInstr(97,603)]);
(193, [EatInstr(121,247)]);
(577, [EatInstr(111,604)]);
(194, [EatInstr(102,248)]);
(578, [AAction2Instr(__a58,199)]);
(195, [EatInstr(116,249)]);
(579, [EatInstr(105,605)]);
(196, [EatInstr(116,250)]);
(580, [AAction2Instr(__a59,199)]);
(197, [EatInstr(111,251)]);
(581, [EatInstr(114,606)]);
(198, [EatInstr(45,252)]);
(582, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,607)]);
(199, [CompleteInstr(273)]);
(583, [AAction2Instr(__a60,608)]);
(200, [AAction2Instr(__a9,199)]);
(584, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,609)]);
(201, [EatInstr(45,253)]);
(585, [EatInstr(114,610)]);
(202, [EatInstr(98,254)]);
(586, [EatInstr(101,611)]);
(203, [EatInstr(45,255)]);
(587, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,612)]);
(204, [EatInstr(117,256)]);
(588, [EatInstr(112,613)]);
(205, [EatInstr(97,257)]);
(589, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,614)]);
(206, [AAction2Instr(__a10,258)]);
(590, [EatInstr(121,615)]);
(207, [EatInstr(45,259)]);
(591, [AAction2Instr(__a61,78)]);
(208, [EatInstr(101,260)]);
(592, [EatInstr(108,616)]);
(209, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,261)]);
(593, [EatInstr(45,617)]);
(210, [AAction2Instr(__a11,258)]);
(594, [EatInstr(101,618)]);
(211, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,262)]);
(595, [EatInstr(105,619)]);
(212, [EatInstr(100,263)]);
(596, [EatInstr(101,620)]);
(213, [EatInstr(121,264)]);
(597, [EatInstr(110,621)]);
(214, [EatInstr(116,265)]);
(598, [EatInstr(116,622)]);
(215, [EatInstr(108,266)]);
(599, [EatInstr(114,623)]);
(216, [AAction2Instr(__a12,258)]);
(600, [EatInstr(105,624)]);
(217, [EatInstr(108,267)]);
(601, [AAction2Instr(__a62,199)]);
(218, [EatInstr(116,268)]);
(602, [AAction2Instr(__a63,199)]);
(219, [EatInstr(100,269)]);
(603, [EatInstr(114,625)]);
(220, [AAction2Instr(__a13,270)]);
(604, [EatInstr(114,626)]);
(221, [EatInstr(99,271)]);
(605, [EatInstr(115,627)]);
(222, [AAction2Instr(__a14,78)]);
(606, [EatInstr(121,628)]);
(223, [EatInstr(101,272)]);
(607, [AAction2Instr(__a64,199)]);
(224, [AAction2Instr(__a15,78)]);
(608, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,646)]);
(225, [EatInstr(104,273)]);
(609, [AAction2Instr(__a65,258)]);
(226, [EatInstr(111,274)]);
(610, [EatInstr(101,629)]);
(227, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,275)]);
(611, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,630)]);
(228, [EatInstr(100,276)]);
(612, [AAction2Instr(__a66,258)]);
(229, [EatInstr(45,278);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,277)]);
(613, [EatInstr(104,631)]);
(230, [AAction2Instr(__a16,78)]);
(614, [AAction2Instr(__a67,78)]);
(231, [EatInstr(45,281)]);
(615, [EatInstr(115,632)]);
(232, [EatInstr(108,282)]);
(616, [EatInstr(121,633)]);
(233, [EatInstr(114,283)]);
(617, [EatInstr(112,634)]);
(234, [EatInstr(119,284)]);
(618, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,635)]);
(235, [EatInstr(101,285)]);
(619, [EatInstr(111,636)]);
(236, [EatInstr(45,286)]);
(620, [EatInstr(110,637)]);
(237, [EatInstr(107,287)]);
(621, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,638)]);
(238, [EatInstr(116,288)]);
(622, [EatInstr(114,639)]);
(239, [EatInstr(105,289)]);
(623, [AAction2Instr(__a68,640)]);
(240, [EatInstr(110,290)]);
(624, [EatInstr(118,641)]);
(241, [EatInstr(97,291)]);
(625, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,642)]);
(242, [EatInstr(105,292)]);
(626, [EatInstr(121,643)]);
(243, [EatInstr(111,293)]);
(627, [EatInstr(116,644)]);
(244, [EatInstr(101,294)]);
(628, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,645)]);
(245, [EatInstr(101,295)]);
(629, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,648)]);
(246, [EatInstr(107,296)]);
(630, [AAction2Instr(__a69,258)]);
(247, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,297)]);
(631, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,649)]);
(248, [EatInstr(105,298)]);
(632, [EatInstr(105,650)]);
(249, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,299)]);
(633, [EatInstr(115,651)]);
(250, [EatInstr(45,300)]);
(634, [EatInstr(114,652)]);
(251, [EatInstr(108,301)]);
(635, [AAction2Instr(__a70,78)]);
(252, [EatInstr(102,302)]);
(636, [EatInstr(110,653)]);
(253, [EatInstr(110,303)]);
(637, [EatInstr(45,655);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,654)]);
(254, [EatInstr(117,304)]);
(638, [AAction2Instr(__a71,199)]);
(255, [EatInstr(117,305)]);
(639, [EatInstr(105,656)]);
(256, [EatInstr(108,306)]);
(640, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,570)]);
(257, [EatInstr(114,307)]);
(641, [EatInstr(101,657)]);
(258, [CompleteInstr(271)]);
(642, [AAction2Instr(__a72,199)]);
(259, [EatInstr(116,309)]);
(643, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,658)]);
(260, [EatInstr(45,310)]);
(644, [EatInstr(111,659)]);
(261, [AAction2Instr(__a17,258)]);
(645, [AAction2Instr(__a73,199)]);
(262, [AAction2Instr(__a18,258)]);
(646, [AAction2Instr(__a74,647);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,646)]);
(263, [EatInstr(101,311)]);
(647, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,660)]);
(264, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,312)]);
(648, [AAction2Instr(__a75,258)]);
(265, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,313)]);
(649, [AAction2Instr(__a76,78)]);
(266, [EatInstr(45,314)]);
(650, [EatInstr(115,661)]);
(267, [EatInstr(101,315)]);
(651, [EatInstr(105,662)]);
(268, [EatInstr(105,316)]);
(652, [EatInstr(101,663)]);
(269, [EatInstr(101,317)]);
(653, [EatInstr(115,664)]);
(270, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,318)]);
(654, [AAction2Instr(__a77,78)]);
(271, [EatInstr(116,319)]);
(655, [EatInstr(115,665)]);
(272, [EatInstr(110,320)]);
(656, [EatInstr(99,666)]);
(273, [EatInstr(101,321)]);
(657, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,667)]);
(274, [EatInstr(111,322)]);
(658, [AAction2Instr(__a78,199)]);
(275, [AAction2Instr(__a19,78)]);
(659, [EatInstr(114,668)]);
(276, [EatInstr(101,323)]);
(660, [AAction2Instr(__a79,199)]);
(277, [AAction2Instr(__a20,78)]);
(661, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,669)]);
(278, [EatInstr(114,326);EatInstr(110,325);EatInstr(103,324)]);
(662, [EatInstr(115,670)]);
(279, [AAction2Instr(__a21,280);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,279)]);
(663, [EatInstr(100,671)]);
(280, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,327)]);
(664, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,672)]);
(281, [EatInstr(108,328)]);
(665, [EatInstr(99,673)]);
(282, [EatInstr(97,329)]);
(666, [EatInstr(116,674)]);
(283, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,330)]);
(667, [AAction2Instr(__a80,199)]);
(284, [EatInstr(45,331)]);
(668, [EatInstr(121,675)]);
(285, [EatInstr(110,332)]);
(669, [AAction2Instr(__a81,78)]);
(286, [EatInstr(105,333)]);
(670, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,676)]);
(287, [EatInstr(45,334)]);
(671, [EatInstr(105,677)]);
(288, [EatInstr(101,335)]);
(672, [AAction2Instr(__a82,78)]);
(289, [EatInstr(100,336)]);
(673, [EatInstr(97,678)]);
(290, [EatInstr(101,337)]);
(674, [AAction2Instr(__a83,640)]);
(291, [EatInstr(104,338)]);
(675, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,679)]);
(292, [EatInstr(122,339)]);
(676, [AAction2Instr(__a84,78)]);
(293, [EatInstr(97,340)]);
(677, [EatInstr(99,680)]);
(294, [EatInstr(109,341)]);
(678, [EatInstr(110,681)]);
(295, [EatInstr(112,342)]);
(679, [AAction2Instr(__a85,199)]);
(296, [EatInstr(105,343)]);
(680, [EatInstr(97,682)]);
(297, [AAction2Instr(__a22,199)]);
(681, [EatInstr(110,683)]);
(298, [EatInstr(120,344)]);
(682, [EatInstr(116,684)]);
(299, [AAction2Instr(__a23,345)]);
(683, [EatInstr(101,685)]);
(300, [EatInstr(104,346)]);
(684, [EatInstr(101,686)]);
(301, [EatInstr(108,347)]);
(685, [EatInstr(114,687)]);
(302, [EatInstr(115,348)]);
(686, [EatInstr(115,688)]);
(303, [EatInstr(111,349)]);
(687, [EatInstr(108,689)]);
(304, [EatInstr(116,350)]);
(688, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,690)]);
(305, [EatInstr(110,351)]);
(689, [EatInstr(101,691)]);
(306, [EatInstr(101,352)]);
(690, [AAction2Instr(__a86,78)]);
(307, [EatInstr(45,354);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,353)]);
(691, [EatInstr(115,692)]);
(692, [EatInstr(115,693)]);
(309, [EatInstr(121,355)]);
(693, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,694)]);
(310, [EatInstr(114,357);EatInstr(110,356)]);
(694, [AAction2Instr(__a87,78)]);
(311, [EatInstr(110,358)]);
(312, [AAction2Instr(__a24,258)]);
(313, [AAction2Instr(__a25,258)]);
(314, [EatInstr(115,359)]);
(315, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,360)]);
(316, [EatInstr(110,361)]);
(317, [EatInstr(110,362)]);
(318, [AAction2Instr(__a26,363)]);
(319, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,364)]);
(320, [EatInstr(101,365)]);
(321, [EatInstr(97,366)]);
(322, [EatInstr(107,367)]);
(323, [EatInstr(110,368)]);
(324, [EatInstr(105,369)]);
(325, [EatInstr(117,371);EatInstr(112,370)]);
(326, [EatInstr(101,372)]);
(327, [AAction2Instr(__a27,78)]);
(328, [EatInstr(97,373)]);
(329, [EatInstr(116,374)]);
(330, [AContInstr3(271,__g0,__binder4,375);ACallInstr3(__g0,8)]);
(331, [EatInstr(110,376)]);
(332, [EatInstr(100,377)]);
(333, [EatInstr(110,378)]);
(334, [EatInstr(108,379)]);
(335, [EatInstr(114,380)]);
(336, [EatInstr(45,381)]);
(337, [EatInstr(45,382)]);
(338, [EatInstr(101,383)]);
(339, [EatInstr(101,384)]);
(340, [EatInstr(108,385)]);
(341, [EatInstr(111,386)]);
(342, [EatInstr(108,387)]);
(343, [EatInstr(112,388)]);
(344, [EatInstr(45,389)]);
(345, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,390)]);
(346, [EatInstr(105,391)]);
(347, [EatInstr(45,392)]);
(348, [EatInstr(116,394);EatInstr(109,393)]);
(349, [EatInstr(116,395)]);
(350, [EatInstr(101,396)]);
(351, [EatInstr(100,397)]);
(352, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,398)]);
(353, [AAction2Instr(__a28,258)]);
(354, [EatInstr(103,399)]);
(355, [EatInstr(112,400)]);
(356, [EatInstr(117,401)]);
(357, [EatInstr(101,402)]);
(358, [EatInstr(99,403)]);
(359, [EatInstr(116,404)]);
(360, [AAction2Instr(__a29,78)]);
(361, [EatInstr(101,405)]);
(362, [EatInstr(99,406)]);
(363, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,407)]);
(364, [AAction2Instr(__a30,78)]);
(365, [EatInstr(114,408)]);
(366, [EatInstr(100,409)]);
(367, [EatInstr(97,410)]);
(368, [EatInstr(99,411)]);
(369, [EatInstr(108,412)]);
(370, [EatInstr(114,413)]);
(371, [EatInstr(108,414)]);
(372, [EatInstr(108,415)]);
(373, [EatInstr(116,416)]);
(374, [EatInstr(101,417)]);
(375, [AAction2Instr(__a31,199)]);
(376, [EatInstr(111,418)]);
(377, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,419)]);
(378, [EatInstr(115,420)]);
(379, [EatInstr(97,421)]);
(380, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,422)]);
(381, [EatInstr(97,423)]);
(382, [EatInstr(114,425);EatInstr(99,424)]);
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
