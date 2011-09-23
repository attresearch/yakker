
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
| Pads_lift_cmd
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
 | (2038) -> ((); ();  Pads_lift_cmd )
 | (2039) -> ((); ();  Parse_cmd )
 | (2040) -> ((); ();  Precedence_analysis_cmd )
 | (2041) -> ((); ();  Print_gul_cmd )
 | (2042) -> ((); ();  Print_gil_cmd )
 | (2043) -> ((); ();  Print_npreds_cmd )
 | (2044) -> ((); ();  Print_npreds_cmd )
 | (2045) -> ((); ();  Print_relevance_cmd )
 | (2046) -> ((); (let _x10 = _p() in (); (let _x9 = _p() in (let n = Yak.YkBuf.get_string _x10 _x9 ykinput in ();  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" ))))
 | (2049) -> ((); ();  Sort_cmd )
 | (2050) -> ((); ();  Strip_late_actions_cmd )
 | (2051) -> ((); ();  Translate_dypgen_cmd )
 | (2052) -> ((); ();  Translate_dypgen_scannerless_cmd )
 | _ -> raise Exit)
and
 _r_args(_n,_p,ykinput) = (match _n() with
 | (2053) -> ((); (); (let p = _r_phases(_n,_p,ykinput) in  after := Some p ; ()))
 | (2054) -> ((); ();  Compileopt.use_wrap_and_attr := false;      
                                            Compileopt.use_coroutines    := false ; ())
 | (2055) -> ((); ();  Compileopt.use_coroutines := false ; ())
 | (2056) -> ((); (); (let b = (match _n() with
 | (2057) -> ((); Fun_BE)
 | (2058) -> ((); Trans_BE)
 | (2059) -> ((); Wadler_BE)
 | (2060) -> ((); Peg_BE false)
 | (2061) -> ((); Peg_BE true)
 | _ -> raise Exit) in ();  backend := b ; ()))
 | (2062) -> ((); ();  Compileopt.case_sensitive := false ; ())
 | (2063) -> ((); ();  Compileopt.check_labels := true ; ())
 | (2064) -> ((); (); (let _x12 = _p() in (); (let _x11 = _p() in (let n = Yak.YkBuf.get_string _x12 _x11 ykinput in ();  Variables.counter := (int_of_string n) ; ()))))
 | (2067) -> ((); ();  Compileopt.inline_cs := true ; ())
 | (2068) -> ((); ();  Compileopt.inline_regular := true ; ())
 | (2069) -> ((); ();  Compileopt.lookahead := true ; ())
 | (2070) -> ((); ();  Compileopt.memoize_history := true ; ())
 | (2071) -> ((); ();  Compileopt.coalesce := false ; ())
 | (2072) -> ((); ();  Compileopt.collapse_calls := false ; ())
 | (2073) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2074) -> ((); ();  Compileopt.repress_replay := true ; ())
 | (2075) -> ((); ();  Compileopt.skip_opt := false ; ())
 | (2076) -> ((); ();  only := true ; ())
 | (2077) -> ((); ();  Compileopt.postfix_history := false ; ())
 | (2078) -> ((); (); (let _x14 = _p() in (); (let _x13 = _p() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2081) -> ((); ();  Compileopt.unit_history := true ; ())
 | (2082) -> ((); (); (let _x16 = _p() in (); (let _x15 = _p() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  Compileopt.unroll_star_n := (int_of_string n) ; ()))))
 | (2085) -> ((); ();  Compileopt.use_fsm := true ; ())
 | (2086) -> ((); ();  Compileopt.use_fsm := false ; ())
 | (2087) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2088) -> ((let _x18 = _p() in (); (let _x17 = _p() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
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
 | (2045) -> (();();(); push((2045)))
 | (2046) -> (();();();push_pos(_p());();push_pos(_p());(); push((2046)))
 | (2049) -> (();();(); push((2049)))
 | (2050) -> (();();(); push((2050)))
 | (2051) -> (();();(); push((2051)))
 | (2052) -> (();();(); push((2052)))
 | _ -> raise Exit)
and _rv_args() = (match _n() with
 | (2053) -> (();();_rv_phases();();(); push((2053)))
 | (2054) -> (();();();(); push((2054)))
 | (2055) -> (();();();(); push((2055)))
 | (2056) -> (();();();(match _n() with
 | (2057) -> (();(); push((2057)))
 | (2058) -> (();(); push((2058)))
 | (2059) -> (();(); push((2059)))
 | (2060) -> (();(); push((2060)))
 | (2061) -> (();(); push((2061)))
 | _ -> raise Exit);();(); push((2056)))
 | (2062) -> (();();();(); push((2062)))
 | (2063) -> (();();();(); push((2063)))
 | (2064) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2064)))
 | (2067) -> (();();();(); push((2067)))
 | (2068) -> (();();();(); push((2068)))
 | (2069) -> (();();();(); push((2069)))
 | (2070) -> (();();();(); push((2070)))
 | (2071) -> (();();();(); push((2071)))
 | (2072) -> (();();();(); push((2072)))
 | (2073) -> (();();();(); push((2073)))
 | (2074) -> (();();();(); push((2074)))
 | (2075) -> (();();();(); push((2075)))
 | (2076) -> (();();();(); push((2076)))
 | (2077) -> (();();();(); push((2077)))
 | (2078) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2078)))
 | (2081) -> (();();();(); push((2081)))
 | (2082) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2082)))
 | (2085) -> (();();();(); push((2085)))
 | (2086) -> (();();();(); push((2086)))
 | (2087) -> (();();();(); push((2087)))
 | (2088) -> (();();();();push_pos(_p());();push_pos(_p()); push((2088)))
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
let __a58 = (_p 1080 ((2064)));;
let __a41 = (_p 1032 ((2042)));;
let __a86 = (_p 1034 ((2040)));;
let __a65 = (_p 1065 ((2081)));;
let __a26 = (_p 1026 ((2046)));;
let __a44 = (fun _x0_ _x1_ -> (((_p 1044 ((2028))) _x0_) (((_p 1045 ((2031))) _x0_) _x1_)));;
let __a23 = (_p 1068 ((2079)));;
let __a64 = (_p 1091 ((2055)));;
let __a19 = (_p 1035 ((2039)));;
let __a31 = (_p 1093 ((2053)));;
let __a62 = (_p 1037 ((2037)));;
let __a16 = (_p 1005 ((2018)));;
let __a27 = (_p 1007 ((2016)));;
let __a13 = (_p 1039 ((2035)));;
let __a22 = (_p 1070 ((2076)));;
let __a18 = (_p 1009 ((2014)));;
let __a35 = (_p 1043 ((2025)));;
let __a49 = (_p 1072 ((2074)));;
let __a11 = (_p 1041 ((2033)));;
let __a85 = (_p 1074 ((2072)));;
let __a17 = (_p 1011 ((2012)));;
let __a79 = (_p 1076 ((2070)));;
let __a68 = (_p 1013 ((2010)));;
let __a42 = (_p 1066 ((2078)));;
let __a4 = (_p 1057 ((2090)));;
let __a89 = (_p 1022 ((2052)));;
let __a83 = (_p 1024 ((2050)));;
let __a57 = (_p 1085 ((2056)));;
let __a55 = (_p 1081 ((2066)));;
let __a51 = (_p 1014 ((2009)));;
let __a48 = (_p 1077 ((2069)));;
let __a47 = (_p 1079 ((2067)));;
let __a12 = (_p 1016 ((2007)));;
let __a29 = (_p 1018 ((2005)));;
let __a21 = (_p 1027 ((2048)));;
let __a1 = (_p 1058 ((2089)));;
let __a84 = (_p 1087 ((2060)));;
let __a8 = (_p 1050 ((2024)));;
let __a76 = (_p 1020 ((2003)));;
let __a63 = (_p 1083 ((2063)));;
let __a39 = (_p 1052 ((2022)));;
let __a15 = (_p 1025 ((2049)));;
let __a3 = (_p 1054 ((2020)));;
let __a71 = (_p 1029 ((2045)));;
let __a61 = (_p 1064 ((2083)));;
let __a2 = (_p 1002 ((2001)));;
let __a69 = (_p 1088 ((2059)));;
let __a38 = (_p 1060 ((2086)));;
let __a56 = (_p 1031 ((2043)));;
let __a53 = (_p 1090 ((2057)));;
let __a20 = (_p 1033 ((2041)));;
let __a9 = (_p 1056 ((2088)));;
let __a72 = (_p 1092 ((2054)));;
let __a80 = (_p 1062 ((2082)));;
let __a25 = (_p 1004 ((2019)));;
let __a6 = (_p 1000 ((2000)));;
let __a34 = (_p 1046 ((2030)));;
let __a40 = (_p 1036 ((2038)));;
let __a82 = (_p 1038 ((2036)));;
let __a74 = (_p 1069 ((2077)));;
let __a52 = (_p 1006 ((2017)));;
let __a45 = (_p 1008 ((2015)));;
let __a67 = (_p 1040 ((2034)));;
let __a60 = (_p 1071 ((2075)));;
let __a87 = (_p 1073 ((2073)));;
let __a14 = (_p 1010 ((2013)));;
let __a30 = (_p 1042 ((2032)));;
let __a70 = (_p 1012 ((2011)));;
let __a59 = (_p 1075 ((2071)));;
let __a10 = (_p 1049 ((2026)));;
let __a78 = (_p 1023 ((2051)));;
let __a73 = (_p 1078 ((2068)));;
let __a66 = (_p 1015 ((2008)));;
let __a36 = (_p 1082 ((2065)));;
let __g0 = (_e);;
let __a50 = (_p 1017 ((2006)));;
let __a24 = (fun _x0_ _x1_ -> (((_p 1047 ((2029))) _x0_) (((_p 1048 ((2027))) _x0_) _x1_)));;
let __a54 = (_p 1086 ((2061)));;
let __a33 = (_p 1019 ((2004)));;
let __a5 = (_p 1028 ((2047)));;
let __a77 = (_p 1051 ((2023)));;
let __a81 = (_p 1084 ((2062)));;
let __a28 = (_p 1053 ((2021)));;
let __a43 = (_p 1021 ((2002)));;
let __a75 = (_p 1063 ((2084)));;
let __a7 = (_p 1059 ((2087)));;
let __a88 = (_p 1030 ((2044)));;
let __a32 = (_p 1067 ((2080)));;
let __a46 = (_p 1089 ((2058)));;
let __a37 = (_p 1061 ((2085)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1003);;
let __binder2 = (_m 1055);;
let __binder3 = (_m 1001);;
let __binder4 = (_m 1094);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(111,427)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,428)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(115,429)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(97,430)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,431)]);
(4, [EatInstr(119,30);EatInstr(117,29);EatInstr(116,28);EatInstr(115,27);EatInstr(114,26);EatInstr(112,25);EatInstr(109,24);EatInstr(108,23);EatInstr(105,22);EatInstr(104,21);EatInstr(103,20);EatInstr(102,19);EatInstr(101,18);EatInstr(100,17);EatInstr(99,16);EatInstr(97,15);AContInstr3(271,__g0,__binder2,32);AContInstr3(272,__g0,__binder1,31)]);
(388, [EatInstr(97,432)]);
(5, [EatInstr(0,33)]);
(389, [EatInstr(114,434);EatInstr(99,433)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,70)]);
(390, [EatInstr(97,435)]);
(7, [EatInstr(127,72);EatInstr(126,72);EatInstr(125,72);EatInstr(124,72);EatInstr(123,72);EatInstr(122,72);EatInstr(121,72);EatInstr(120,72);EatInstr(119,72);EatInstr(118,72);EatInstr(117,72);EatInstr(116,72);EatInstr(115,72);EatInstr(114,72);EatInstr(113,72);EatInstr(112,72);EatInstr(111,72);EatInstr(110,72);EatInstr(109,72);EatInstr(108,72);EatInstr(107,72);EatInstr(106,72);EatInstr(105,72);EatInstr(104,72);EatInstr(103,72);EatInstr(102,72);EatInstr(101,72);EatInstr(100,72);EatInstr(99,72);EatInstr(98,72);EatInstr(97,72);EatInstr(96,72);EatInstr(95,72);EatInstr(94,72);EatInstr(93,72);EatInstr(92,72);EatInstr(91,72);EatInstr(90,72);EatInstr(89,72);EatInstr(88,72);EatInstr(87,72);EatInstr(86,72);EatInstr(85,72);EatInstr(84,72);EatInstr(83,72);EatInstr(82,72);EatInstr(81,72);EatInstr(80,72);EatInstr(79,72);EatInstr(78,72);EatInstr(77,72);EatInstr(76,72);EatInstr(75,72);EatInstr(74,72);EatInstr(73,72);EatInstr(72,72);EatInstr(71,72);EatInstr(70,72);EatInstr(69,72);EatInstr(68,72);EatInstr(67,72);EatInstr(66,72);EatInstr(65,72);EatInstr(64,72);EatInstr(63,72);EatInstr(62,72);EatInstr(61,72);EatInstr(60,72);EatInstr(59,72);EatInstr(58,72);EatInstr(57,72);EatInstr(56,72);EatInstr(55,72);EatInstr(54,72);EatInstr(53,72);EatInstr(52,72);EatInstr(51,72);EatInstr(50,72);EatInstr(49,72);EatInstr(48,72);EatInstr(47,72);EatInstr(46,72);EatInstr(44,72);EatInstr(43,72);EatInstr(42,72);EatInstr(41,72);EatInstr(40,72);EatInstr(39,72);EatInstr(38,72);EatInstr(37,72);EatInstr(36,72);EatInstr(35,72);EatInstr(34,72);EatInstr(33,72);EatInstr(32,72);EatInstr(31,72);EatInstr(30,72);EatInstr(29,72);EatInstr(28,72);EatInstr(27,72);EatInstr(26,72);EatInstr(25,72);EatInstr(24,72);EatInstr(23,72);EatInstr(22,72);EatInstr(21,72);EatInstr(20,72);EatInstr(19,72);EatInstr(18,72);EatInstr(17,72);EatInstr(16,72);EatInstr(15,72);EatInstr(14,72);EatInstr(13,72);EatInstr(12,72);EatInstr(11,72);EatInstr(10,72);EatInstr(9,72);EatInstr(8,72);EatInstr(7,72);EatInstr(6,72);EatInstr(5,72);EatInstr(4,72);EatInstr(3,72);EatInstr(2,72);EatInstr(1,72)]);
(391, [EatInstr(45,436)]);
(8, [EatInstr(119,30);EatInstr(117,29);EatInstr(115,40);EatInstr(114,39);EatInstr(112,38);EatInstr(109,24);EatInstr(108,37);EatInstr(105,36);EatInstr(104,21);EatInstr(100,35);EatInstr(99,34);EatInstr(97,15)]);
(392, [EatInstr(101,437)]);
(9, [EatInstr(119,30);EatInstr(117,29);EatInstr(116,28);EatInstr(115,27);EatInstr(114,26);EatInstr(112,25);EatInstr(109,24);EatInstr(108,23);EatInstr(105,22);EatInstr(104,21);EatInstr(103,20);EatInstr(102,19);EatInstr(101,18);EatInstr(100,17);EatInstr(99,16);EatInstr(97,15);AContInstr3(271,__g0,__binder2,32)]);
(393, [EatInstr(97,438)]);
(10, [EatInstr(45,41);AAction2Instr(__a1,42)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43)]);
(394, [EatInstr(105,439)]);
(395, [EatInstr(97,440)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(45,441)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(104,442)]);
(14, [CompleteInstr(266)]);
(398, [AAction2Instr(__a32,443)]);
(15, [EatInstr(116,45);EatInstr(114,44)]);
(399, [EatInstr(115,444)]);
(16, [EatInstr(111,47);EatInstr(108,46)]);
(400, [EatInstr(115,445)]);
(17, [EatInstr(111,49);EatInstr(101,48)]);
(401, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,446)]);
(18, [EatInstr(120,50)]);
(402, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,447)]);
(19, [EatInstr(117,51)]);
(403, [EatInstr(97,448)]);
(20, [EatInstr(101,52)]);
(404, [EatInstr(115,449)]);
(21, [EatInstr(97,53)]);
(405, [EatInstr(101,450)]);
(22, [EatInstr(110,54)]);
(406, [AAction2Instr(__a33,270)]);
(23, [EatInstr(114,58);EatInstr(111,57);EatInstr(105,56);EatInstr(101,55)]);
(407, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,451)]);
(24, [EatInstr(105,59)]);
(408, [EatInstr(121,452)]);
(25, [EatInstr(114,61);EatInstr(97,60)]);
(409, [EatInstr(105,453)]);
(26, [EatInstr(102,63);EatInstr(101,62)]);
(410, [AAction2Instr(__a35,209);AAction2Instr(__a34,454)]);
(27, [EatInstr(117,66);EatInstr(116,65);EatInstr(111,64)]);
(411, [EatInstr(97,455)]);
(28, [EatInstr(114,67)]);
(412, [EatInstr(101,456)]);
(29, [EatInstr(110,68)]);
(413, [EatInstr(108,457)]);
(30, [EatInstr(114,69)]);
(414, [EatInstr(103,458)]);
(31, [AAction2Instr(__a2,181)]);
(415, [EatInstr(45,459)]);
(32, [AAction2Instr(__a3,209)]);
(416, [EatInstr(104,460)]);
(33, [CompleteInstr(268)]);
(417, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,461)]);
(34, [EatInstr(111,74);EatInstr(108,46)]);
(418, [EatInstr(101,462)]);
(35, [EatInstr(101,75)]);
(419, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,463)]);
(36, [EatInstr(110,76)]);
(420, [EatInstr(101,464)]);
(37, [EatInstr(105,56);EatInstr(101,55)]);
(421, [EatInstr(108,465)]);
(38, [EatInstr(114,77)]);
(422, [EatInstr(101,466)]);
(39, [EatInstr(101,62)]);
(423, [EatInstr(101,467)]);
(40, [EatInstr(117,66)]);
(424, [EatInstr(45,468)]);
(41, [EatInstr(118,90);EatInstr(117,89);EatInstr(114,88);EatInstr(112,87);EatInstr(111,86);EatInstr(110,85);EatInstr(109,84);EatInstr(108,83);EatInstr(105,82);EatInstr(104,81);EatInstr(99,80);EatInstr(98,79);EatInstr(97,78)]);
(425, [EatInstr(97,469)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,91)]);
(426, [EatInstr(101,470)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(116,471)]);
(44, [EatInstr(114,92)]);
(428, [EatInstr(119,475);EatInstr(116,474);EatInstr(112,473);EatInstr(102,472)]);
(45, [EatInstr(116,93)]);
(429, [EatInstr(101,476)]);
(46, [EatInstr(111,94)]);
(430, [EatInstr(98,477)]);
(47, [EatInstr(114,97);EatInstr(112,96);EatInstr(109,95)]);
(431, [AAction2Instr(__a36,478)]);
(48, [EatInstr(115,99);EatInstr(112,98)]);
(432, [EatInstr(114,479)]);
(49, [EatInstr(116,100)]);
(433, [EatInstr(115,480)]);
(50, [EatInstr(116,102);EatInstr(101,101)]);
(434, [EatInstr(101,481)]);
(51, [EatInstr(115,103)]);
(435, [EatInstr(100,482)]);
(52, [EatInstr(116,104)]);
(436, [EatInstr(104,483)]);
(53, [EatInstr(115,105)]);
(437, [EatInstr(115,484)]);
(54, [EatInstr(108,107);EatInstr(102,106)]);
(438, [EatInstr(112,485)]);
(55, [EatInstr(120,108)]);
(439, [EatInstr(122,486)]);
(56, [EatInstr(102,109)]);
(440, [EatInstr(121,487)]);
(57, [EatInstr(111,110)]);
(441, [EatInstr(111,488)]);
(58, [EatInstr(49,111)]);
(442, [EatInstr(105,489)]);
(59, [EatInstr(110,112)]);
(443, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,490)]);
(60, [EatInstr(114,114);EatInstr(100,113)]);
(444, [EatInstr(116,491)]);
(61, [EatInstr(105,116);EatInstr(101,115)]);
(445, [EatInstr(116,492)]);
(62, [EatInstr(112,117)]);
(446, [AAction2Instr(__a37,255)]);
(63, [EatInstr(99,118)]);
(447, [AAction2Instr(__a38,255)]);
(64, [EatInstr(114,119)]);
(448, [EatInstr(116,493)]);
(65, [EatInstr(114,120)]);
(449, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,494)]);
(66, [EatInstr(98,121)]);
(450, [EatInstr(114,495)]);
(67, [EatInstr(97,122)]);
(451, [AAction2Instr(__a39,209)]);
(68, [EatInstr(114,123)]);
(452, [EatInstr(45,496)]);
(69, [EatInstr(97,124)]);
(453, [EatInstr(108,497)]);
(70, [ALookaheadInstr(false,CfgLA (1,264),71);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,70)]);
(454, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,498)]);
(71, [CompleteInstr(269)]);
(455, [EatInstr(116,499)]);
(72, [ALookaheadInstr(false,CfgLA (1,264),73);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,72)]);
(456, [EatInstr(115,500)]);
(73, [CompleteInstr(270)]);
(457, [EatInstr(108,501)]);
(74, [EatInstr(112,96)]);
(458, [EatInstr(117,502)]);
(75, [EatInstr(115,99)]);
(459, [EatInstr(97,503)]);
(76, [EatInstr(108,107);EatInstr(102,127)]);
(460, [EatInstr(101,504)]);
(77, [EatInstr(101,128)]);
(461, [AAction2Instr(__a40,209)]);
(78, [EatInstr(114,130);EatInstr(102,129)]);
(462, [EatInstr(45,506);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,505)]);
(79, [EatInstr(97,131)]);
(463, [AAction2Instr(__a41,209)]);
(80, [EatInstr(111,134);EatInstr(104,133);EatInstr(97,132)]);
(464, [EatInstr(100,507)]);
(81, [EatInstr(121,135)]);
(465, [EatInstr(97,508)]);
(82, [EatInstr(110,136)]);
(466, [EatInstr(118,509)]);
(83, [EatInstr(111,137)]);
(467, [EatInstr(45,510)]);
(84, [EatInstr(101,138)]);
(468, [EatInstr(100,511)]);
(85, [EatInstr(111,139)]);
(469, [EatInstr(114,512)]);
(86, [EatInstr(110,140)]);
(470, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,505)]);
(87, [EatInstr(114,141)]);
(471, [EatInstr(97,513)]);
(88, [EatInstr(111,142)]);
(472, [EatInstr(117,514)]);
(89, [EatInstr(115,144);EatInstr(110,143)]);
(473, [EatInstr(101,515)]);
(90, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,145)]);
(474, [EatInstr(120,516)]);
(91, [AAction2Instr(__a4,146)]);
(475, [EatInstr(97,517)]);
(92, [EatInstr(111,147)]);
(476, [EatInstr(110,518)]);
(93, [EatInstr(114,148)]);
(477, [EatInstr(101,519)]);
(94, [EatInstr(115,149)]);
(478, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,556)]);
(95, [EatInstr(112,150)]);
(479, [EatInstr(114,520)]);
(96, [EatInstr(121,151)]);
(480, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,521)]);
(97, [EatInstr(111,152)]);
(481, [EatInstr(103,522)]);
(98, [EatInstr(101,153)]);
(482, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,523)]);
(99, [EatInstr(117,154)]);
(483, [EatInstr(105,524)]);
(100, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,155)]);
(484, [EatInstr(99,525)]);
(101, [EatInstr(99,156)]);
(485, [EatInstr(115,526)]);
(102, [EatInstr(114,157)]);
(486, [EatInstr(101,527)]);
(103, [EatInstr(101,158)]);
(487, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,528)]);
(104, [EatInstr(45,159)]);
(488, [EatInstr(112,529)]);
(105, [EatInstr(104,160)]);
(489, [EatInstr(115,530)]);
(106, [EatInstr(111,162);EatInstr(101,161)]);
(490, [AAction2Instr(__a42,255)]);
(107, [EatInstr(105,163)]);
(491, [EatInstr(111,531)]);
(108, [EatInstr(101,164)]);
(492, [EatInstr(97,532)]);
(109, [EatInstr(116,165)]);
(493, [EatInstr(105,533)]);
(110, [EatInstr(107,166)]);
(494, [AAction2Instr(__a43,270)]);
(111, [EatInstr(45,167)]);
(495, [EatInstr(45,534)]);
(112, [EatInstr(117,168)]);
(496, [EatInstr(103,535)]);
(113, [EatInstr(115,169)]);
(497, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,536)]);
(114, [EatInstr(115,170)]);
(498, [AAction2Instr(__a44,364)]);
(115, [EatInstr(99,171)]);
(499, [EatInstr(111,537)]);
(116, [EatInstr(110,172)]);
(500, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(117, [EatInstr(108,173)]);
(501, [EatInstr(97,539)]);
(118, [AAction2Instr(__a5,174)]);
(502, [EatInstr(108,540)]);
(119, [EatInstr(116,175)]);
(503, [EatInstr(110,541)]);
(120, [EatInstr(105,176)]);
(504, [EatInstr(97,542)]);
(121, [EatInstr(115,177)]);
(505, [AAction2Instr(__a45,270)]);
(122, [EatInstr(110,178)]);
(506, [EatInstr(97,543)]);
(123, [EatInstr(111,179)]);
(507, [EatInstr(115,544)]);
(124, [EatInstr(112,180)]);
(508, [EatInstr(98,545)]);
(125, [AAction2Instr(__a6,181)]);
(509, [EatInstr(97,546)]);
(126, [CompleteInstr(267)]);
(510, [EatInstr(97,547)]);
(127, [EatInstr(101,161)]);
(511, [EatInstr(121,548)]);
(128, [EatInstr(99,182)]);
(512, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,549)]);
(129, [EatInstr(116,183)]);
(513, [EatInstr(116,550)]);
(130, [EatInstr(114,184)]);
(514, [EatInstr(110,551)]);
(131, [EatInstr(99,185)]);
(515, [EatInstr(103,552)]);
(132, [EatInstr(115,186)]);
(516, [AAction2Instr(__a46,657)]);
(133, [EatInstr(101,187)]);
(517, [EatInstr(100,553)]);
(134, [EatInstr(117,188)]);
(518, [EatInstr(115,554)]);
(135, [EatInstr(98,189)]);
(519, [EatInstr(108,555)]);
(136, [EatInstr(108,190)]);
(520, [EatInstr(111,558)]);
(137, [EatInstr(111,191)]);
(521, [AAction2Instr(__a47,255)]);
(138, [EatInstr(109,192)]);
(522, [EatInstr(117,559)]);
(139, [EatInstr(45,193)]);
(523, [AAction2Instr(__a48,255)]);
(140, [EatInstr(108,194)]);
(524, [EatInstr(115,560)]);
(141, [EatInstr(101,195)]);
(525, [EatInstr(101,561)]);
(142, [EatInstr(111,196)]);
(526, [EatInstr(101,562)]);
(143, [EatInstr(114,198);EatInstr(105,197)]);
(527, [EatInstr(45,563)]);
(144, [EatInstr(101,199)]);
(528, [AAction2Instr(__a49,255)]);
(145, [AAction2Instr(__a7,255)]);
(529, [EatInstr(116,564)]);
(146, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,200)]);
(530, [EatInstr(116,565)]);
(147, [EatInstr(119,201)]);
(531, [EatInstr(114,566)]);
(148, [EatInstr(105,202)]);
(532, [EatInstr(114,567)]);
(149, [EatInstr(101,203)]);
(533, [EatInstr(111,568)]);
(150, [EatInstr(105,204)]);
(534, [EatInstr(99,569)]);
(151, [EatInstr(114,205)]);
(535, [EatInstr(114,570)]);
(152, [EatInstr(117,206)]);
(536, [AAction2Instr(__a50,270)]);
(153, [EatInstr(110,207)]);
(537, [EatInstr(114,571)]);
(154, [EatInstr(103,208)]);
(538, [AAction2Instr(__a51,270)]);
(155, [AAction2Instr(__a8,209)]);
(539, [EatInstr(98,572)]);
(156, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,210)]);
(540, [EatInstr(97,573)]);
(157, [EatInstr(97,211)]);
(541, [EatInstr(97,574)]);
(158, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,212)]);
(542, [EatInstr(100,575)]);
(159, [EatInstr(103,213)]);
(543, [EatInstr(110,576)]);
(160, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,214)]);
(544, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,577)]);
(161, [EatInstr(114,215)]);
(545, [EatInstr(108,578)]);
(162, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,216)]);
(546, [EatInstr(110,579)]);
(163, [EatInstr(110,217)]);
(547, [EatInstr(99,580)]);
(164, [EatInstr(114,218)]);
(548, [EatInstr(112,581)]);
(165, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,219)]);
(549, [AAction2Instr(__a52,270)]);
(166, [EatInstr(97,220)]);
(550, [EatInstr(105,582)]);
(167, [EatInstr(108,221)]);
(551, [AAction2Instr(__a53,657)]);
(168, [EatInstr(115,222)]);
(552, [EatInstr(45,583);AAction2Instr(__a54,657)]);
(169, [EatInstr(45,223)]);
(553, [EatInstr(108,585)]);
(170, [EatInstr(101,224)]);
(554, [EatInstr(105,586)]);
(171, [EatInstr(101,225)]);
(555, [EatInstr(115,587)]);
(172, [EatInstr(116,226)]);
(556, [AAction2Instr(__a55,557);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,556)]);
(173, [EatInstr(97,227)]);
(557, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,588)]);
(174, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,280)]);
(558, [EatInstr(119,589)]);
(175, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,228)]);
(559, [EatInstr(108,590)]);
(176, [EatInstr(112,229)]);
(560, [EatInstr(116,591)]);
(177, [EatInstr(101,230)]);
(561, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,592)]);
(178, [EatInstr(115,231)]);
(562, [EatInstr(45,593)]);
(179, [EatInstr(108,232)]);
(563, [EatInstr(104,594)]);
(180, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,233)]);
(564, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,595)]);
(181, [ALookaheadInstr(false,CfgLA (3,266),126);ACallInstr3(__default_call,11);ACallInstr3(__g0,10);AContInstr3(273,__g0,__binder3,125);ASimpleCont2Instr(274,__binder0,126)]);
(565, [EatInstr(111,596)]);
(182, [EatInstr(101,234)]);
(566, [EatInstr(121,597)]);
(183, [EatInstr(101,235)]);
(567, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,598)]);
(184, [EatInstr(111,236)]);
(568, [EatInstr(110,599)]);
(185, [EatInstr(107,237)]);
(569, [EatInstr(111,600)]);
(186, [EatInstr(101,238)]);
(570, [EatInstr(97,601)]);
(187, [EatInstr(99,239)]);
(571, [EatInstr(115,602)]);
(188, [EatInstr(110,240)]);
(572, [EatInstr(108,603)]);
(189, [EatInstr(114,241)]);
(573, [EatInstr(114,604)]);
(190, [EatInstr(105,242)]);
(574, [EatInstr(108,605)]);
(191, [EatInstr(107,243)]);
(575, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,606)]);
(192, [EatInstr(111,244)]);
(576, [EatInstr(97,607)]);
(193, [EatInstr(115,248);EatInstr(114,247);EatInstr(109,246);EatInstr(99,245)]);
(577, [AAction2Instr(__a56,209)]);
(194, [EatInstr(121,249)]);
(578, [EatInstr(101,608)]);
(195, [EatInstr(102,250)]);
(579, [EatInstr(99,609)]);
(196, [EatInstr(116,251)]);
(580, [EatInstr(116,610)]);
(197, [EatInstr(116,252)]);
(581, [EatInstr(103,611)]);
(198, [EatInstr(111,253)]);
(582, [EatInstr(111,612)]);
(199, [EatInstr(45,254)]);
(583, [EatInstr(115,613)]);
(200, [AAction2Instr(__a9,255)]);
(584, [AAction2Instr(__a57,255)]);
(201, [EatInstr(45,256)]);
(585, [EatInstr(101,614)]);
(202, [EatInstr(98,257)]);
(586, [EatInstr(116,615)]);
(203, [EatInstr(45,258)]);
(587, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,616)]);
(204, [EatInstr(108,259)]);
(588, [AAction2Instr(__a58,255)]);
(205, [EatInstr(117,260)]);
(589, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,617)]);
(206, [EatInstr(116,261)]);
(590, [EatInstr(97,618)]);
(207, [EatInstr(100,262)]);
(591, [EatInstr(111,619)]);
(208, [EatInstr(97,263)]);
(592, [AAction2Instr(__a59,255)]);
(209, [CompleteInstr(272)]);
(593, [EatInstr(99,620)]);
(210, [AAction2Instr(__a10,264)]);
(594, [EatInstr(105,621)]);
(211, [EatInstr(99,265)]);
(595, [AAction2Instr(__a60,255)]);
(212, [AAction2Instr(__a11,209)]);
(596, [EatInstr(114,622)]);
(213, [EatInstr(101,266)]);
(597, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,623)]);
(214, [AAction2Instr(__a12,270)]);
(598, [AAction2Instr(__a61,624)]);
(215, [EatInstr(45,267)]);
(599, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,625)]);
(216, [AAction2Instr(__a13,209)]);
(600, [EatInstr(114,626)]);
(217, [EatInstr(101,268)]);
(601, [EatInstr(112,627)]);
(218, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,269)]);
(602, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,628)]);
(219, [AAction2Instr(__a14,270)]);
(603, [EatInstr(101,629)]);
(220, [EatInstr(104,271)]);
(604, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,630)]);
(221, [EatInstr(111,272)]);
(605, [EatInstr(121,631)]);
(222, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,273)]);
(606, [AAction2Instr(__a62,209)]);
(223, [EatInstr(108,274)]);
(607, [EatInstr(108,632)]);
(224, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,275)]);
(608, [EatInstr(45,633)]);
(225, [EatInstr(100,276)]);
(609, [EatInstr(101,634)]);
(226, [EatInstr(45,278);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,277)]);
(610, [EatInstr(105,635)]);
(227, [EatInstr(121,279)]);
(611, [EatInstr(101,636)]);
(228, [AAction2Instr(__a15,209)]);
(612, [EatInstr(110,637)]);
(229, [EatInstr(45,282)]);
(613, [EatInstr(116,638)]);
(230, [EatInstr(116,283)]);
(614, [EatInstr(114,639)]);
(231, [EatInstr(108,284)]);
(615, [EatInstr(105,640)]);
(232, [EatInstr(108,285)]);
(616, [AAction2Instr(__a63,255)]);
(233, [AAction2Instr(__a16,270)]);
(617, [AAction2Instr(__a64,255)]);
(234, [EatInstr(100,286)]);
(618, [EatInstr(114,641)]);
(235, [EatInstr(114,287)]);
(619, [EatInstr(114,642)]);
(236, [EatInstr(119,288)]);
(620, [EatInstr(97,643)]);
(237, [EatInstr(101,289)]);
(621, [EatInstr(115,644)]);
(238, [EatInstr(45,290)]);
(622, [EatInstr(121,645)]);
(239, [EatInstr(107,291)]);
(623, [AAction2Instr(__a65,255)]);
(240, [EatInstr(116,292)]);
(624, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,664)]);
(241, [EatInstr(105,293)]);
(625, [AAction2Instr(__a66,270)]);
(242, [EatInstr(110,294)]);
(626, [EatInstr(101,646)]);
(243, [EatInstr(97,295)]);
(627, [EatInstr(104,647)]);
(244, [EatInstr(105,296)]);
(628, [AAction2Instr(__a67,209)]);
(245, [EatInstr(111,297)]);
(629, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,648)]);
(246, [EatInstr(101,298)]);
(630, [AAction2Instr(__a68,270)]);
(247, [EatInstr(101,299)]);
(631, [EatInstr(115,649)]);
(248, [EatInstr(107,300)]);
(632, [EatInstr(121,650)]);
(249, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,301)]);
(633, [EatInstr(112,651)]);
(250, [EatInstr(105,302)]);
(634, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,652)]);
(251, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,303)]);
(635, [EatInstr(111,653)]);
(252, [EatInstr(45,304)]);
(636, [EatInstr(110,654)]);
(253, [EatInstr(108,305)]);
(637, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,655)]);
(254, [EatInstr(102,306)]);
(638, [EatInstr(114,656)]);
(255, [CompleteInstr(273)]);
(639, [AAction2Instr(__a69,657)]);
(256, [EatInstr(110,307)]);
(640, [EatInstr(118,658)]);
(257, [EatInstr(117,308)]);
(641, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,659)]);
(258, [EatInstr(117,309)]);
(642, [EatInstr(121,660)]);
(259, [EatInstr(101,310)]);
(643, [EatInstr(108,661)]);
(260, [EatInstr(108,311)]);
(644, [EatInstr(116,662)]);
(261, [EatInstr(105,312)]);
(645, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,663)]);
(262, [EatInstr(101,313)]);
(646, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,666)]);
(263, [EatInstr(114,314)]);
(647, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,667)]);
(264, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,315)]);
(648, [AAction2Instr(__a70,270)]);
(265, [EatInstr(116,316)]);
(649, [EatInstr(105,668)]);
(266, [EatInstr(110,317)]);
(650, [EatInstr(115,669)]);
(267, [EatInstr(116,319)]);
(651, [EatInstr(114,670)]);
(268, [EatInstr(45,320)]);
(652, [AAction2Instr(__a71,209)]);
(269, [AAction2Instr(__a17,270)]);
(653, [EatInstr(110,671)]);
(270, [CompleteInstr(271)]);
(654, [EatInstr(45,673);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,672)]);
(271, [EatInstr(101,321)]);
(655, [AAction2Instr(__a72,255)]);
(272, [EatInstr(111,322)]);
(656, [EatInstr(105,674)]);
(273, [AAction2Instr(__a18,270)]);
(657, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,584)]);
(274, [EatInstr(105,323)]);
(658, [EatInstr(101,675)]);
(275, [AAction2Instr(__a19,209)]);
(659, [AAction2Instr(__a73,255)]);
(276, [EatInstr(101,324)]);
(660, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,676)]);
(277, [AAction2Instr(__a20,209)]);
(661, [EatInstr(108,677)]);
(278, [EatInstr(114,327);EatInstr(110,326);EatInstr(103,325)]);
(662, [EatInstr(111,678)]);
(279, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,328)]);
(663, [AAction2Instr(__a74,255)]);
(280, [AAction2Instr(__a21,281);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,280)]);
(664, [AAction2Instr(__a75,665);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,664)]);
(281, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,329)]);
(665, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,679)]);
(282, [EatInstr(108,330)]);
(666, [AAction2Instr(__a76,270)]);
(283, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,331)]);
(667, [AAction2Instr(__a77,209)]);
(284, [EatInstr(97,332)]);
(668, [EatInstr(115,680)]);
(285, [EatInstr(45,333)]);
(669, [EatInstr(105,681)]);
(286, [EatInstr(101,334)]);
(670, [EatInstr(101,682)]);
(287, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,335)]);
(671, [EatInstr(115,683)]);
(288, [EatInstr(45,336)]);
(672, [AAction2Instr(__a78,209)]);
(289, [EatInstr(110,337)]);
(673, [EatInstr(115,684)]);
(290, [EatInstr(105,338)]);
(674, [EatInstr(99,685)]);
(291, [EatInstr(45,339)]);
(675, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,686)]);
(292, [EatInstr(101,340)]);
(676, [AAction2Instr(__a79,255)]);
(293, [EatInstr(100,341)]);
(677, [EatInstr(115,687)]);
(294, [EatInstr(101,342)]);
(678, [EatInstr(114,688)]);
(295, [EatInstr(104,343)]);
(679, [AAction2Instr(__a80,255)]);
(296, [EatInstr(122,344)]);
(680, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,689)]);
(297, [EatInstr(108,346);EatInstr(97,345)]);
(681, [EatInstr(115,690)]);
(298, [EatInstr(109,347)]);
(682, [EatInstr(100,691)]);
(299, [EatInstr(112,348)]);
(683, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,692)]);
(300, [EatInstr(105,349)]);
(684, [EatInstr(99,693)]);
(301, [AAction2Instr(__a22,255)]);
(685, [EatInstr(116,694)]);
(302, [EatInstr(120,350)]);
(686, [AAction2Instr(__a81,255)]);
(303, [AAction2Instr(__a23,351)]);
(687, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,695)]);
(304, [EatInstr(104,352)]);
(688, [EatInstr(121,696)]);
(305, [EatInstr(108,353)]);
(689, [AAction2Instr(__a82,209)]);
(306, [EatInstr(115,354)]);
(690, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,697)]);
(307, [EatInstr(111,355)]);
(691, [EatInstr(105,698)]);
(308, [EatInstr(116,356)]);
(692, [AAction2Instr(__a83,209)]);
(309, [EatInstr(110,357)]);
(693, [EatInstr(97,699)]);
(310, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,358)]);
(694, [AAction2Instr(__a84,657)]);
(311, [EatInstr(101,359)]);
(695, [AAction2Instr(__a85,255)]);
(312, [EatInstr(110,360)]);
(696, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,700)]);
(313, [EatInstr(110,361)]);
(697, [AAction2Instr(__a86,209)]);
(314, [EatInstr(45,363);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,362)]);
(698, [EatInstr(99,701)]);
(315, [AAction2Instr(__a24,364)]);
(699, [EatInstr(110,702)]);
(316, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,365)]);
(700, [AAction2Instr(__a87,255)]);
(317, [EatInstr(101,366)]);
(701, [EatInstr(97,703)]);
(702, [EatInstr(110,704)]);
(319, [EatInstr(121,367)]);
(703, [EatInstr(116,705)]);
(320, [EatInstr(114,369);EatInstr(110,368)]);
(704, [EatInstr(101,706)]);
(321, [EatInstr(97,370)]);
(705, [EatInstr(101,707)]);
(322, [EatInstr(107,371)]);
(706, [EatInstr(114,708)]);
(323, [EatInstr(102,372)]);
(707, [EatInstr(115,709)]);
(324, [EatInstr(110,373)]);
(708, [EatInstr(108,710)]);
(325, [EatInstr(105,374)]);
(709, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,711)]);
(326, [EatInstr(117,376);EatInstr(112,375)]);
(710, [EatInstr(101,712)]);
(327, [EatInstr(101,377)]);
(711, [AAction2Instr(__a88,209)]);
(328, [AAction2Instr(__a25,270)]);
(712, [EatInstr(115,713)]);
(329, [AAction2Instr(__a26,209)]);
(713, [EatInstr(115,714)]);
(330, [EatInstr(97,378)]);
(714, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,715)]);
(331, [AAction2Instr(__a27,270)]);
(715, [AAction2Instr(__a89,209)]);
(332, [EatInstr(116,379)]);
(333, [EatInstr(115,380)]);
(334, [EatInstr(110,381)]);
(335, [AContInstr3(271,__g0,__binder4,382);ACallInstr3(__g0,8)]);
(336, [EatInstr(110,383)]);
(337, [EatInstr(100,384)]);
(338, [EatInstr(110,385)]);
(339, [EatInstr(108,386)]);
(340, [EatInstr(114,387)]);
(341, [EatInstr(45,388)]);
(342, [EatInstr(45,389)]);
(343, [EatInstr(101,390)]);
(344, [EatInstr(101,391)]);
(345, [EatInstr(108,392)]);
(346, [EatInstr(108,393)]);
(347, [EatInstr(111,394)]);
(348, [EatInstr(108,395)]);
(349, [EatInstr(112,396)]);
(350, [EatInstr(45,397)]);
(351, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,398)]);
(352, [EatInstr(105,399)]);
(353, [EatInstr(45,400)]);
(354, [EatInstr(116,402);EatInstr(109,401)]);
(355, [EatInstr(116,403)]);
(356, [EatInstr(101,404)]);
(357, [EatInstr(100,405)]);
(358, [AAction2Instr(__a28,209)]);
(359, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,406)]);
(360, [EatInstr(101,407)]);
(361, [EatInstr(99,408)]);
(362, [AAction2Instr(__a29,270)]);
(363, [EatInstr(103,409)]);
(364, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,410)]);
(365, [AAction2Instr(__a30,209)]);
(366, [EatInstr(114,411)]);
(367, [EatInstr(112,412)]);
(368, [EatInstr(117,413)]);
(369, [EatInstr(101,414)]);
(370, [EatInstr(100,415)]);
(371, [EatInstr(97,416)]);
(372, [EatInstr(116,417)]);
(373, [EatInstr(99,418)]);
(374, [EatInstr(108,419)]);
(375, [EatInstr(114,420)]);
(376, [EatInstr(108,421)]);
(377, [EatInstr(108,422)]);
(378, [EatInstr(116,423)]);
(379, [EatInstr(101,424)]);
(380, [EatInstr(116,425)]);
(381, [EatInstr(99,426)]);
(382, [AAction2Instr(__a31,255)]);
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
