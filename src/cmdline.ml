
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
 | (2071) -> ((); ();  Compileopt.collapse_calls := false ; ())
 | (2072) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2073) -> ((); ();  Compileopt.repress_replay := true ; ())
 | (2074) -> ((); ();  Compileopt.skip_opt := false ; ())
 | (2075) -> ((); ();  only := true ; ())
 | (2076) -> ((); ();  Compileopt.postfix_history := false ; ())
 | (2077) -> ((); (); (let _x14 = _p() in (); (let _x13 = _p() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2080) -> ((); ();  Compileopt.unit_history := true ; ())
 | (2081) -> ((); (); (let _x16 = _p() in (); (let _x15 = _p() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  Compileopt.unroll_star_n := (int_of_string n) ; ()))))
 | (2084) -> ((); ();  Compileopt.use_fsm := true ; ())
 | (2085) -> ((); ();  Compileopt.use_fsm := false ; ())
 | (2086) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2087) -> ((let _x18 = _p() in (); (let _x17 = _p() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
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
 | (2076) -> (();();();(); push((2076)))
 | (2077) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2077)))
 | (2080) -> (();();();(); push((2080)))
 | (2081) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2081)))
 | (2084) -> (();();();(); push((2084)))
 | (2085) -> (();();();(); push((2085)))
 | (2086) -> (();();();(); push((2086)))
 | (2087) -> (();();();();push_pos(_p());();push_pos(_p()); push((2087)))
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
let __a62 = (_p 1082 ((2062)));;
let __a87 = (_p 1030 ((2043)));;
let __a60 = (_p 1063 ((2082)));;
let __a40 = (_p 1032 ((2041)));;
let __a38 = (_p 1059 ((2085)));;
let __a68 = (_p 1087 ((2058)));;
let __a52 = (_p 1089 ((2056)));;
let __a26 = (_p 1026 ((2045)));;
let __a16 = (_p 1005 ((2018)));;
let __a27 = (_p 1007 ((2016)));;
let __a9 = (_p 1055 ((2087)));;
let __a19 = (_p 1035 ((2038)));;
let __a18 = (_p 1009 ((2014)));;
let __a32 = (_p 1066 ((2079)));;
let __a34 = (_p 1045 ((2030)));;
let __a81 = (_p 1037 ((2036)));;
let __a71 = (_p 1091 ((2053)));;
let __a66 = (_p 1039 ((2034)));;
let __a17 = (_p 1011 ((2012)));;
let __a79 = (_p 1061 ((2081)));;
let __a67 = (_p 1013 ((2010)));;
let __a73 = (_p 1068 ((2076)));;
let __a30 = (_p 1041 ((2032)));;
let __a59 = (_p 1070 ((2074)));;
let __a86 = (_p 1072 ((2072)));;
let __a50 = (_p 1014 ((2009)));;
let __a58 = (_p 1074 ((2070)));;
let __a10 = (_p 1048 ((2026)));;
let __a43 = (fun _x0_ _x1_ -> (((_p 1043 ((2028))) _x0_) (((_p 1044 ((2031))) _x0_) _x1_)));;
let __a12 = (_p 1016 ((2007)));;
let __a88 = (_p 1022 ((2051)));;
let __a29 = (_p 1018 ((2005)));;
let __a75 = (_p 1020 ((2003)));;
let __a78 = (_p 1075 ((2069)));;
let __a24 = (fun _x0_ _x1_ -> (((_p 1046 ((2029))) _x0_) (((_p 1047 ((2027))) _x0_) _x1_)));;
let __a21 = (_p 1027 ((2047)));;
let __a76 = (_p 1050 ((2023)));;
let __a72 = (_p 1077 ((2067)));;
let __a36 = (_p 1081 ((2064)));;
let __a2 = (_p 1002 ((2001)));;
let __a28 = (_p 1052 ((2021)));;
let __a15 = (_p 1025 ((2048)));;
let __a4 = (_p 1056 ((2089)));;
let __a53 = (_p 1085 ((2060)));;
let __a70 = (_p 1029 ((2044)));;
let __a80 = (_p 1083 ((2061)));;
let __a74 = (_p 1062 ((2083)));;
let __a55 = (_p 1031 ((2042)));;
let __a7 = (_p 1058 ((2086)));;
let __a83 = (_p 1086 ((2059)));;
let __a20 = (_p 1033 ((2040)));;
let __a57 = (_p 1079 ((2063)));;
let __a45 = (_p 1088 ((2057)));;
let __a37 = (_p 1060 ((2084)));;
let __a25 = (_p 1004 ((2019)));;
let __a6 = (_p 1000 ((2000)));;
let __a64 = (_p 1064 ((2080)));;
let __a51 = (_p 1006 ((2017)));;
let __a85 = (_p 1034 ((2039)));;
let __a44 = (_p 1008 ((2015)));;
let __a61 = (_p 1036 ((2037)));;
let __a23 = (_p 1067 ((2078)));;
let __a63 = (_p 1090 ((2054)));;
let __a14 = (_p 1010 ((2013)));;
let __a13 = (_p 1038 ((2035)));;
let __a69 = (_p 1012 ((2011)));;
let __a31 = (_p 1092 ((2052)));;
let __a35 = (_p 1042 ((2025)));;
let __a11 = (_p 1040 ((2033)));;
let __a22 = (_p 1069 ((2075)));;
let __a48 = (_p 1071 ((2073)));;
let __a84 = (_p 1073 ((2071)));;
let __a65 = (_p 1015 ((2008)));;
let __g0 = (_e);;
let __a49 = (_p 1017 ((2006)));;
let __a41 = (_p 1065 ((2077)));;
let __a77 = (_p 1023 ((2050)));;
let __a33 = (_p 1019 ((2004)));;
let __a8 = (_p 1049 ((2024)));;
let __a42 = (_p 1021 ((2002)));;
let __a56 = (_p 1084 ((2055)));;
let __a54 = (_p 1080 ((2065)));;
let __a47 = (_p 1076 ((2068)));;
let __a5 = (_p 1028 ((2046)));;
let __a82 = (_p 1024 ((2049)));;
let __a39 = (_p 1051 ((2022)));;
let __a46 = (_p 1078 ((2066)));;
let __a1 = (_p 1057 ((2088)));;
let __a3 = (_p 1053 ((2020)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1003);;
let __binder2 = (_m 1054);;
let __binder3 = (_m 1001);;
let __binder4 = (_m 1093);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(114,427);EatInstr(99,426)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(97,428)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(45,429)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(101,430)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(97,431)]);
(4, [EatInstr(119,30);EatInstr(117,29);EatInstr(116,28);EatInstr(115,27);EatInstr(114,26);EatInstr(112,25);EatInstr(109,24);EatInstr(108,23);EatInstr(105,22);EatInstr(104,21);EatInstr(103,20);EatInstr(102,19);EatInstr(101,18);EatInstr(100,17);EatInstr(99,16);EatInstr(97,15);AContInstr3(271,__g0,__binder2,32);AContInstr3(272,__g0,__binder1,31)]);
(388, [EatInstr(105,432)]);
(5, [EatInstr(0,33)]);
(389, [EatInstr(97,433)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,70)]);
(390, [EatInstr(45,434)]);
(7, [EatInstr(127,72);EatInstr(126,72);EatInstr(125,72);EatInstr(124,72);EatInstr(123,72);EatInstr(122,72);EatInstr(121,72);EatInstr(120,72);EatInstr(119,72);EatInstr(118,72);EatInstr(117,72);EatInstr(116,72);EatInstr(115,72);EatInstr(114,72);EatInstr(113,72);EatInstr(112,72);EatInstr(111,72);EatInstr(110,72);EatInstr(109,72);EatInstr(108,72);EatInstr(107,72);EatInstr(106,72);EatInstr(105,72);EatInstr(104,72);EatInstr(103,72);EatInstr(102,72);EatInstr(101,72);EatInstr(100,72);EatInstr(99,72);EatInstr(98,72);EatInstr(97,72);EatInstr(96,72);EatInstr(95,72);EatInstr(94,72);EatInstr(93,72);EatInstr(92,72);EatInstr(91,72);EatInstr(90,72);EatInstr(89,72);EatInstr(88,72);EatInstr(87,72);EatInstr(86,72);EatInstr(85,72);EatInstr(84,72);EatInstr(83,72);EatInstr(82,72);EatInstr(81,72);EatInstr(80,72);EatInstr(79,72);EatInstr(78,72);EatInstr(77,72);EatInstr(76,72);EatInstr(75,72);EatInstr(74,72);EatInstr(73,72);EatInstr(72,72);EatInstr(71,72);EatInstr(70,72);EatInstr(69,72);EatInstr(68,72);EatInstr(67,72);EatInstr(66,72);EatInstr(65,72);EatInstr(64,72);EatInstr(63,72);EatInstr(62,72);EatInstr(61,72);EatInstr(60,72);EatInstr(59,72);EatInstr(58,72);EatInstr(57,72);EatInstr(56,72);EatInstr(55,72);EatInstr(54,72);EatInstr(53,72);EatInstr(52,72);EatInstr(51,72);EatInstr(50,72);EatInstr(49,72);EatInstr(48,72);EatInstr(47,72);EatInstr(46,72);EatInstr(44,72);EatInstr(43,72);EatInstr(42,72);EatInstr(41,72);EatInstr(40,72);EatInstr(39,72);EatInstr(38,72);EatInstr(37,72);EatInstr(36,72);EatInstr(35,72);EatInstr(34,72);EatInstr(33,72);EatInstr(32,72);EatInstr(31,72);EatInstr(30,72);EatInstr(29,72);EatInstr(28,72);EatInstr(27,72);EatInstr(26,72);EatInstr(25,72);EatInstr(24,72);EatInstr(23,72);EatInstr(22,72);EatInstr(21,72);EatInstr(20,72);EatInstr(19,72);EatInstr(18,72);EatInstr(17,72);EatInstr(16,72);EatInstr(15,72);EatInstr(14,72);EatInstr(13,72);EatInstr(12,72);EatInstr(11,72);EatInstr(10,72);EatInstr(9,72);EatInstr(8,72);EatInstr(7,72);EatInstr(6,72);EatInstr(5,72);EatInstr(4,72);EatInstr(3,72);EatInstr(2,72);EatInstr(1,72)]);
(391, [EatInstr(104,435)]);
(8, [EatInstr(119,30);EatInstr(117,29);EatInstr(115,40);EatInstr(114,39);EatInstr(112,38);EatInstr(109,24);EatInstr(108,37);EatInstr(105,36);EatInstr(104,21);EatInstr(100,35);EatInstr(99,34);EatInstr(97,15)]);
(392, [AAction2Instr(__a32,436)]);
(9, [EatInstr(119,30);EatInstr(117,29);EatInstr(116,28);EatInstr(115,27);EatInstr(114,26);EatInstr(112,25);EatInstr(109,24);EatInstr(108,23);EatInstr(105,22);EatInstr(104,21);EatInstr(103,20);EatInstr(102,19);EatInstr(101,18);EatInstr(100,17);EatInstr(99,16);EatInstr(97,15);AContInstr3(271,__g0,__binder2,32)]);
(393, [EatInstr(115,437)]);
(10, [EatInstr(45,41);AAction2Instr(__a1,42)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43)]);
(394, [EatInstr(115,438)]);
(395, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,439)]);
(12, [CompleteInstr(264)]);
(396, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,440)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(97,441)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(115,442)]);
(15, [EatInstr(116,45);EatInstr(114,44)]);
(399, [EatInstr(101,443)]);
(16, [EatInstr(111,47);EatInstr(108,46)]);
(400, [AAction2Instr(__a33,267)]);
(17, [EatInstr(111,49);EatInstr(101,48)]);
(401, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,444)]);
(18, [EatInstr(120,50)]);
(402, [EatInstr(121,445)]);
(19, [EatInstr(117,51)]);
(403, [EatInstr(105,446)]);
(20, [EatInstr(101,52)]);
(404, [AAction2Instr(__a35,207);AAction2Instr(__a34,447)]);
(21, [EatInstr(97,53)]);
(405, [EatInstr(97,448)]);
(22, [EatInstr(110,54)]);
(406, [EatInstr(101,449)]);
(23, [EatInstr(114,58);EatInstr(111,57);EatInstr(105,56);EatInstr(101,55)]);
(407, [EatInstr(108,450)]);
(24, [EatInstr(105,59)]);
(408, [EatInstr(103,451)]);
(25, [EatInstr(114,61);EatInstr(97,60)]);
(409, [EatInstr(45,452)]);
(26, [EatInstr(102,63);EatInstr(101,62)]);
(410, [EatInstr(104,453)]);
(27, [EatInstr(117,66);EatInstr(116,65);EatInstr(111,64)]);
(411, [EatInstr(101,454)]);
(28, [EatInstr(114,67)]);
(412, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,455)]);
(29, [EatInstr(110,68)]);
(413, [EatInstr(101,456)]);
(30, [EatInstr(114,69)]);
(414, [EatInstr(108,457)]);
(31, [AAction2Instr(__a2,179)]);
(415, [EatInstr(101,458)]);
(32, [AAction2Instr(__a3,207)]);
(416, [EatInstr(101,459)]);
(33, [CompleteInstr(268)]);
(417, [EatInstr(45,460)]);
(34, [EatInstr(111,74);EatInstr(108,46)]);
(418, [EatInstr(97,461)]);
(35, [EatInstr(101,75)]);
(419, [EatInstr(101,462)]);
(36, [EatInstr(110,76)]);
(420, [EatInstr(116,463)]);
(37, [EatInstr(105,56);EatInstr(101,55)]);
(421, [EatInstr(119,467);EatInstr(116,466);EatInstr(112,465);EatInstr(102,464)]);
(38, [EatInstr(114,77)]);
(422, [EatInstr(101,468)]);
(39, [EatInstr(101,62)]);
(423, [EatInstr(98,469)]);
(40, [EatInstr(117,66)]);
(424, [AAction2Instr(__a36,470)]);
(41, [EatInstr(118,90);EatInstr(117,89);EatInstr(114,88);EatInstr(112,87);EatInstr(111,86);EatInstr(110,85);EatInstr(109,84);EatInstr(108,83);EatInstr(105,82);EatInstr(104,81);EatInstr(99,80);EatInstr(98,79);EatInstr(97,78)]);
(425, [EatInstr(114,471)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,91)]);
(426, [EatInstr(115,472)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(101,473)]);
(44, [EatInstr(114,92)]);
(428, [EatInstr(100,474)]);
(45, [EatInstr(116,93)]);
(429, [EatInstr(104,475)]);
(46, [EatInstr(111,94)]);
(430, [EatInstr(115,476)]);
(47, [EatInstr(114,97);EatInstr(112,96);EatInstr(109,95)]);
(431, [EatInstr(112,477)]);
(48, [EatInstr(115,99);EatInstr(112,98)]);
(432, [EatInstr(122,478)]);
(49, [EatInstr(116,100)]);
(433, [EatInstr(121,479)]);
(50, [EatInstr(116,102);EatInstr(101,101)]);
(434, [EatInstr(111,480)]);
(51, [EatInstr(115,103)]);
(435, [EatInstr(105,481)]);
(52, [EatInstr(116,104)]);
(436, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,482)]);
(53, [EatInstr(115,105)]);
(437, [EatInstr(116,483)]);
(54, [EatInstr(108,107);EatInstr(102,106)]);
(438, [EatInstr(116,484)]);
(55, [EatInstr(120,108)]);
(439, [AAction2Instr(__a37,252)]);
(56, [EatInstr(102,109)]);
(440, [AAction2Instr(__a38,252)]);
(57, [EatInstr(111,110)]);
(441, [EatInstr(116,485)]);
(58, [EatInstr(49,111)]);
(442, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,486)]);
(59, [EatInstr(110,112)]);
(443, [EatInstr(114,487)]);
(60, [EatInstr(114,113)]);
(444, [AAction2Instr(__a39,207)]);
(61, [EatInstr(105,115);EatInstr(101,114)]);
(445, [EatInstr(45,488)]);
(62, [EatInstr(112,116)]);
(446, [EatInstr(108,489)]);
(63, [EatInstr(99,117)]);
(447, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,490)]);
(64, [EatInstr(114,118)]);
(448, [EatInstr(116,491)]);
(65, [EatInstr(114,119)]);
(449, [EatInstr(115,492)]);
(66, [EatInstr(98,120)]);
(450, [EatInstr(108,493)]);
(67, [EatInstr(97,121)]);
(451, [EatInstr(117,494)]);
(68, [EatInstr(114,122)]);
(452, [EatInstr(97,495)]);
(69, [EatInstr(97,123)]);
(453, [EatInstr(101,496)]);
(70, [ALookaheadInstr(false,CfgLA (1,264),71);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,70)]);
(454, [EatInstr(45,498);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,497)]);
(71, [CompleteInstr(269)]);
(455, [AAction2Instr(__a40,207)]);
(72, [ALookaheadInstr(false,CfgLA (1,264),73);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,72)]);
(456, [EatInstr(100,499)]);
(73, [CompleteInstr(270)]);
(457, [EatInstr(97,500)]);
(74, [EatInstr(112,96)]);
(458, [EatInstr(118,501)]);
(75, [EatInstr(115,99)]);
(459, [EatInstr(45,502)]);
(76, [EatInstr(108,107);EatInstr(102,126)]);
(460, [EatInstr(100,503)]);
(77, [EatInstr(101,127)]);
(461, [EatInstr(114,504)]);
(78, [EatInstr(114,129);EatInstr(102,128)]);
(462, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,497)]);
(79, [EatInstr(97,130)]);
(463, [EatInstr(97,505)]);
(80, [EatInstr(111,133);EatInstr(104,132);EatInstr(97,131)]);
(464, [EatInstr(117,506)]);
(81, [EatInstr(121,134)]);
(465, [EatInstr(101,507)]);
(82, [EatInstr(110,135)]);
(466, [EatInstr(120,508)]);
(83, [EatInstr(111,136)]);
(467, [EatInstr(97,509)]);
(84, [EatInstr(101,137)]);
(468, [EatInstr(110,510)]);
(85, [EatInstr(111,138)]);
(469, [EatInstr(101,511)]);
(86, [EatInstr(110,139)]);
(470, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,548)]);
(87, [EatInstr(114,140)]);
(471, [EatInstr(114,512)]);
(88, [EatInstr(111,141)]);
(472, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,513)]);
(89, [EatInstr(115,143);EatInstr(110,142)]);
(473, [EatInstr(103,514)]);
(90, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,144)]);
(474, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,515)]);
(91, [AAction2Instr(__a4,145)]);
(475, [EatInstr(105,516)]);
(92, [EatInstr(111,146)]);
(476, [EatInstr(99,517)]);
(93, [EatInstr(114,147)]);
(477, [EatInstr(115,518)]);
(94, [EatInstr(115,148)]);
(478, [EatInstr(101,519)]);
(95, [EatInstr(112,149)]);
(479, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,520)]);
(96, [EatInstr(121,150)]);
(480, [EatInstr(112,521)]);
(97, [EatInstr(111,151)]);
(481, [EatInstr(115,522)]);
(98, [EatInstr(101,152)]);
(482, [AAction2Instr(__a41,252)]);
(99, [EatInstr(117,153)]);
(483, [EatInstr(111,523)]);
(100, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,154)]);
(484, [EatInstr(97,524)]);
(101, [EatInstr(99,155)]);
(485, [EatInstr(105,525)]);
(102, [EatInstr(114,156)]);
(486, [AAction2Instr(__a42,267)]);
(103, [EatInstr(101,157)]);
(487, [EatInstr(45,526)]);
(104, [EatInstr(45,158)]);
(488, [EatInstr(103,527)]);
(105, [EatInstr(104,159)]);
(489, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,528)]);
(106, [EatInstr(111,161);EatInstr(101,160)]);
(490, [AAction2Instr(__a43,359)]);
(107, [EatInstr(105,162)]);
(491, [EatInstr(111,529)]);
(108, [EatInstr(101,163)]);
(492, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,530)]);
(109, [EatInstr(116,164)]);
(493, [EatInstr(97,531)]);
(110, [EatInstr(107,165)]);
(494, [EatInstr(108,532)]);
(111, [EatInstr(45,166)]);
(495, [EatInstr(110,533)]);
(112, [EatInstr(117,167)]);
(496, [EatInstr(97,534)]);
(113, [EatInstr(115,168)]);
(497, [AAction2Instr(__a44,267)]);
(114, [EatInstr(99,169)]);
(498, [EatInstr(97,535)]);
(115, [EatInstr(110,170)]);
(499, [EatInstr(115,536)]);
(116, [EatInstr(108,171)]);
(500, [EatInstr(98,537)]);
(117, [AAction2Instr(__a5,172)]);
(501, [EatInstr(97,538)]);
(118, [EatInstr(116,173)]);
(502, [EatInstr(97,539)]);
(119, [EatInstr(105,174)]);
(503, [EatInstr(121,540)]);
(120, [EatInstr(115,175)]);
(504, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,541)]);
(121, [EatInstr(110,176)]);
(505, [EatInstr(116,542)]);
(122, [EatInstr(111,177)]);
(506, [EatInstr(110,543)]);
(123, [EatInstr(112,178)]);
(507, [EatInstr(103,544)]);
(124, [AAction2Instr(__a6,179)]);
(508, [AAction2Instr(__a45,649)]);
(125, [CompleteInstr(267)]);
(509, [EatInstr(100,545)]);
(126, [EatInstr(101,160)]);
(510, [EatInstr(115,546)]);
(127, [EatInstr(99,180)]);
(511, [EatInstr(108,547)]);
(128, [EatInstr(116,181)]);
(512, [EatInstr(111,550)]);
(129, [EatInstr(114,182)]);
(513, [AAction2Instr(__a46,252)]);
(130, [EatInstr(99,183)]);
(514, [EatInstr(117,551)]);
(131, [EatInstr(115,184)]);
(515, [AAction2Instr(__a47,252)]);
(132, [EatInstr(101,185)]);
(516, [EatInstr(115,552)]);
(133, [EatInstr(117,186)]);
(517, [EatInstr(101,553)]);
(134, [EatInstr(98,187)]);
(518, [EatInstr(101,554)]);
(135, [EatInstr(108,188)]);
(519, [EatInstr(45,555)]);
(136, [EatInstr(111,189)]);
(520, [AAction2Instr(__a48,252)]);
(137, [EatInstr(109,190)]);
(521, [EatInstr(116,556)]);
(138, [EatInstr(45,191)]);
(522, [EatInstr(116,557)]);
(139, [EatInstr(108,192)]);
(523, [EatInstr(114,558)]);
(140, [EatInstr(101,193)]);
(524, [EatInstr(114,559)]);
(141, [EatInstr(111,194)]);
(525, [EatInstr(111,560)]);
(142, [EatInstr(114,196);EatInstr(105,195)]);
(526, [EatInstr(99,561)]);
(143, [EatInstr(101,197)]);
(527, [EatInstr(114,562)]);
(144, [AAction2Instr(__a7,252)]);
(528, [AAction2Instr(__a49,267)]);
(145, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,198)]);
(529, [EatInstr(114,563)]);
(146, [EatInstr(119,199)]);
(530, [AAction2Instr(__a50,267)]);
(147, [EatInstr(105,200)]);
(531, [EatInstr(98,564)]);
(148, [EatInstr(101,201)]);
(532, [EatInstr(97,565)]);
(149, [EatInstr(105,202)]);
(533, [EatInstr(97,566)]);
(150, [EatInstr(114,203)]);
(534, [EatInstr(100,567)]);
(151, [EatInstr(117,204)]);
(535, [EatInstr(110,568)]);
(152, [EatInstr(110,205)]);
(536, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,569)]);
(153, [EatInstr(103,206)]);
(537, [EatInstr(108,570)]);
(154, [AAction2Instr(__a8,207)]);
(538, [EatInstr(110,571)]);
(155, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,208)]);
(539, [EatInstr(99,572)]);
(156, [EatInstr(97,209)]);
(540, [EatInstr(112,573)]);
(157, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,210)]);
(541, [AAction2Instr(__a51,267)]);
(158, [EatInstr(103,211)]);
(542, [EatInstr(105,574)]);
(159, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,212)]);
(543, [AAction2Instr(__a52,649)]);
(160, [EatInstr(114,213)]);
(544, [EatInstr(45,575);AAction2Instr(__a53,649)]);
(161, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,214)]);
(545, [EatInstr(108,577)]);
(162, [EatInstr(110,215)]);
(546, [EatInstr(105,578)]);
(163, [EatInstr(114,216)]);
(547, [EatInstr(115,579)]);
(164, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,217)]);
(548, [AAction2Instr(__a54,549);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,548)]);
(165, [EatInstr(97,218)]);
(549, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,580)]);
(166, [EatInstr(108,219)]);
(550, [EatInstr(119,581)]);
(167, [EatInstr(115,220)]);
(551, [EatInstr(108,582)]);
(168, [EatInstr(101,221)]);
(552, [EatInstr(116,583)]);
(169, [EatInstr(101,222)]);
(553, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,584)]);
(170, [EatInstr(116,223)]);
(554, [EatInstr(45,585)]);
(171, [EatInstr(97,224)]);
(555, [EatInstr(104,586)]);
(172, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,276)]);
(556, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,587)]);
(173, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,225)]);
(557, [EatInstr(111,588)]);
(174, [EatInstr(112,226)]);
(558, [EatInstr(121,589)]);
(175, [EatInstr(101,227)]);
(559, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,590)]);
(176, [EatInstr(115,228)]);
(560, [EatInstr(110,591)]);
(177, [EatInstr(108,229)]);
(561, [EatInstr(111,592)]);
(178, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,230)]);
(562, [EatInstr(97,593)]);
(179, [ALookaheadInstr(false,CfgLA (3,266),125);ACallInstr3(__default_call,11);ACallInstr3(__g0,10);AContInstr3(273,__g0,__binder3,124);ASimpleCont2Instr(274,__binder0,125)]);
(563, [EatInstr(115,594)]);
(180, [EatInstr(101,231)]);
(564, [EatInstr(108,595)]);
(181, [EatInstr(101,232)]);
(565, [EatInstr(114,596)]);
(182, [EatInstr(111,233)]);
(566, [EatInstr(108,597)]);
(183, [EatInstr(107,234)]);
(567, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,598)]);
(184, [EatInstr(101,235)]);
(568, [EatInstr(97,599)]);
(185, [EatInstr(99,236)]);
(569, [AAction2Instr(__a55,207)]);
(186, [EatInstr(110,237)]);
(570, [EatInstr(101,600)]);
(187, [EatInstr(114,238)]);
(571, [EatInstr(99,601)]);
(188, [EatInstr(105,239)]);
(572, [EatInstr(116,602)]);
(189, [EatInstr(107,240)]);
(573, [EatInstr(103,603)]);
(190, [EatInstr(111,241)]);
(574, [EatInstr(111,604)]);
(191, [EatInstr(115,245);EatInstr(114,244);EatInstr(109,243);EatInstr(99,242)]);
(575, [EatInstr(115,605)]);
(192, [EatInstr(121,246)]);
(576, [AAction2Instr(__a56,252)]);
(193, [EatInstr(102,247)]);
(577, [EatInstr(101,606)]);
(194, [EatInstr(116,248)]);
(578, [EatInstr(116,607)]);
(195, [EatInstr(116,249)]);
(579, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,608)]);
(196, [EatInstr(111,250)]);
(580, [AAction2Instr(__a57,252)]);
(197, [EatInstr(45,251)]);
(581, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,609)]);
(198, [AAction2Instr(__a9,252)]);
(582, [EatInstr(97,610)]);
(199, [EatInstr(45,253)]);
(583, [EatInstr(111,611)]);
(200, [EatInstr(98,254)]);
(584, [AAction2Instr(__a58,252)]);
(201, [EatInstr(45,255)]);
(585, [EatInstr(99,612)]);
(202, [EatInstr(108,256)]);
(586, [EatInstr(105,613)]);
(203, [EatInstr(117,257)]);
(587, [AAction2Instr(__a59,252)]);
(204, [EatInstr(116,258)]);
(588, [EatInstr(114,614)]);
(205, [EatInstr(100,259)]);
(589, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,615)]);
(206, [EatInstr(97,260)]);
(590, [AAction2Instr(__a60,616)]);
(207, [CompleteInstr(272)]);
(591, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,617)]);
(208, [AAction2Instr(__a10,261)]);
(592, [EatInstr(114,618)]);
(209, [EatInstr(99,262)]);
(593, [EatInstr(112,619)]);
(210, [AAction2Instr(__a11,207)]);
(594, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,620)]);
(211, [EatInstr(101,263)]);
(595, [EatInstr(101,621)]);
(212, [AAction2Instr(__a12,267)]);
(596, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,622)]);
(213, [EatInstr(45,264)]);
(597, [EatInstr(121,623)]);
(214, [AAction2Instr(__a13,207)]);
(598, [AAction2Instr(__a61,207)]);
(215, [EatInstr(101,265)]);
(599, [EatInstr(108,624)]);
(216, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,266)]);
(600, [EatInstr(45,625)]);
(217, [AAction2Instr(__a14,267)]);
(601, [EatInstr(101,626)]);
(218, [EatInstr(104,268)]);
(602, [EatInstr(105,627)]);
(219, [EatInstr(111,269)]);
(603, [EatInstr(101,628)]);
(220, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,270)]);
(604, [EatInstr(110,629)]);
(221, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,271)]);
(605, [EatInstr(116,630)]);
(222, [EatInstr(100,272)]);
(606, [EatInstr(114,631)]);
(223, [EatInstr(45,274);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,273)]);
(607, [EatInstr(105,632)]);
(224, [EatInstr(121,275)]);
(608, [AAction2Instr(__a62,252)]);
(225, [AAction2Instr(__a15,207)]);
(609, [AAction2Instr(__a63,252)]);
(226, [EatInstr(45,278)]);
(610, [EatInstr(114,633)]);
(227, [EatInstr(116,279)]);
(611, [EatInstr(114,634)]);
(228, [EatInstr(108,280)]);
(612, [EatInstr(97,635)]);
(229, [EatInstr(108,281)]);
(613, [EatInstr(115,636)]);
(230, [AAction2Instr(__a16,267)]);
(614, [EatInstr(121,637)]);
(231, [EatInstr(100,282)]);
(615, [AAction2Instr(__a64,252)]);
(232, [EatInstr(114,283)]);
(616, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,656)]);
(233, [EatInstr(119,284)]);
(617, [AAction2Instr(__a65,267)]);
(234, [EatInstr(101,285)]);
(618, [EatInstr(101,638)]);
(235, [EatInstr(45,286)]);
(619, [EatInstr(104,639)]);
(236, [EatInstr(107,287)]);
(620, [AAction2Instr(__a66,207)]);
(237, [EatInstr(116,288)]);
(621, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,640)]);
(238, [EatInstr(105,289)]);
(622, [AAction2Instr(__a67,267)]);
(239, [EatInstr(110,290)]);
(623, [EatInstr(115,641)]);
(240, [EatInstr(97,291)]);
(624, [EatInstr(121,642)]);
(241, [EatInstr(105,292)]);
(625, [EatInstr(112,643)]);
(242, [EatInstr(111,293)]);
(626, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,644)]);
(243, [EatInstr(101,294)]);
(627, [EatInstr(111,645)]);
(244, [EatInstr(101,295)]);
(628, [EatInstr(110,646)]);
(245, [EatInstr(107,296)]);
(629, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,647)]);
(246, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,297)]);
(630, [EatInstr(114,648)]);
(247, [EatInstr(105,298)]);
(631, [AAction2Instr(__a68,649)]);
(248, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,299)]);
(632, [EatInstr(118,650)]);
(249, [EatInstr(45,300)]);
(633, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,651)]);
(250, [EatInstr(108,301)]);
(634, [EatInstr(121,652)]);
(251, [EatInstr(102,302)]);
(635, [EatInstr(108,653)]);
(252, [CompleteInstr(273)]);
(636, [EatInstr(116,654)]);
(253, [EatInstr(110,303)]);
(637, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,655)]);
(254, [EatInstr(117,304)]);
(638, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,658)]);
(255, [EatInstr(117,305)]);
(639, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,659)]);
(256, [EatInstr(101,306)]);
(640, [AAction2Instr(__a69,267)]);
(257, [EatInstr(108,307)]);
(641, [EatInstr(105,660)]);
(258, [EatInstr(105,308)]);
(642, [EatInstr(115,661)]);
(259, [EatInstr(101,309)]);
(643, [EatInstr(114,662)]);
(260, [EatInstr(114,310)]);
(644, [AAction2Instr(__a70,207)]);
(261, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,311)]);
(645, [EatInstr(110,663)]);
(262, [EatInstr(116,312)]);
(646, [EatInstr(45,665);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,664)]);
(263, [EatInstr(110,313)]);
(647, [AAction2Instr(__a71,252)]);
(264, [EatInstr(116,315)]);
(648, [EatInstr(105,666)]);
(265, [EatInstr(45,316)]);
(649, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,576)]);
(266, [AAction2Instr(__a17,267)]);
(650, [EatInstr(101,667)]);
(267, [CompleteInstr(271)]);
(651, [AAction2Instr(__a72,252)]);
(268, [EatInstr(101,317)]);
(652, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,668)]);
(269, [EatInstr(111,318)]);
(653, [EatInstr(108,669)]);
(270, [AAction2Instr(__a18,267)]);
(654, [EatInstr(111,670)]);
(271, [AAction2Instr(__a19,207)]);
(655, [AAction2Instr(__a73,252)]);
(272, [EatInstr(101,319)]);
(656, [AAction2Instr(__a74,657);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,656)]);
(273, [AAction2Instr(__a20,207)]);
(657, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,671)]);
(274, [EatInstr(114,322);EatInstr(110,321);EatInstr(103,320)]);
(658, [AAction2Instr(__a75,267)]);
(275, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,323)]);
(659, [AAction2Instr(__a76,207)]);
(276, [AAction2Instr(__a21,277);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,276)]);
(660, [EatInstr(115,672)]);
(277, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,324)]);
(661, [EatInstr(105,673)]);
(278, [EatInstr(108,325)]);
(662, [EatInstr(101,674)]);
(279, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,326)]);
(663, [EatInstr(115,675)]);
(280, [EatInstr(97,327)]);
(664, [AAction2Instr(__a77,207)]);
(281, [EatInstr(45,328)]);
(665, [EatInstr(115,676)]);
(282, [EatInstr(101,329)]);
(666, [EatInstr(99,677)]);
(283, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,330)]);
(667, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,678)]);
(284, [EatInstr(45,331)]);
(668, [AAction2Instr(__a78,252)]);
(285, [EatInstr(110,332)]);
(669, [EatInstr(115,679)]);
(286, [EatInstr(105,333)]);
(670, [EatInstr(114,680)]);
(287, [EatInstr(45,334)]);
(671, [AAction2Instr(__a79,252)]);
(288, [EatInstr(101,335)]);
(672, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,681)]);
(289, [EatInstr(100,336)]);
(673, [EatInstr(115,682)]);
(290, [EatInstr(101,337)]);
(674, [EatInstr(100,683)]);
(291, [EatInstr(104,338)]);
(675, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,684)]);
(292, [EatInstr(122,339)]);
(676, [EatInstr(99,685)]);
(293, [EatInstr(108,341);EatInstr(97,340)]);
(677, [EatInstr(116,686)]);
(294, [EatInstr(109,342)]);
(678, [AAction2Instr(__a80,252)]);
(295, [EatInstr(112,343)]);
(679, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,687)]);
(296, [EatInstr(105,344)]);
(680, [EatInstr(121,688)]);
(297, [AAction2Instr(__a22,252)]);
(681, [AAction2Instr(__a81,207)]);
(298, [EatInstr(120,345)]);
(682, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,689)]);
(299, [AAction2Instr(__a23,346)]);
(683, [EatInstr(105,690)]);
(300, [EatInstr(104,347)]);
(684, [AAction2Instr(__a82,207)]);
(301, [EatInstr(108,348)]);
(685, [EatInstr(97,691)]);
(302, [EatInstr(115,349)]);
(686, [AAction2Instr(__a83,649)]);
(303, [EatInstr(111,350)]);
(687, [AAction2Instr(__a84,252)]);
(304, [EatInstr(116,351)]);
(688, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,692)]);
(305, [EatInstr(110,352)]);
(689, [AAction2Instr(__a85,207)]);
(306, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,353)]);
(690, [EatInstr(99,693)]);
(307, [EatInstr(101,354)]);
(691, [EatInstr(110,694)]);
(308, [EatInstr(110,355)]);
(692, [AAction2Instr(__a86,252)]);
(309, [EatInstr(110,356)]);
(693, [EatInstr(97,695)]);
(310, [EatInstr(45,358);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,357)]);
(694, [EatInstr(110,696)]);
(311, [AAction2Instr(__a24,359)]);
(695, [EatInstr(116,697)]);
(312, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,360)]);
(696, [EatInstr(101,698)]);
(313, [EatInstr(101,361)]);
(697, [EatInstr(101,699)]);
(698, [EatInstr(114,700)]);
(315, [EatInstr(121,362)]);
(699, [EatInstr(115,701)]);
(316, [EatInstr(114,364);EatInstr(110,363)]);
(700, [EatInstr(108,702)]);
(317, [EatInstr(97,365)]);
(701, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,703)]);
(318, [EatInstr(107,366)]);
(702, [EatInstr(101,704)]);
(319, [EatInstr(110,367)]);
(703, [AAction2Instr(__a87,207)]);
(320, [EatInstr(105,368)]);
(704, [EatInstr(115,705)]);
(321, [EatInstr(117,370);EatInstr(112,369)]);
(705, [EatInstr(115,706)]);
(322, [EatInstr(101,371)]);
(706, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,707)]);
(323, [AAction2Instr(__a25,267)]);
(707, [AAction2Instr(__a88,207)]);
(324, [AAction2Instr(__a26,207)]);
(325, [EatInstr(97,372)]);
(326, [AAction2Instr(__a27,267)]);
(327, [EatInstr(116,373)]);
(328, [EatInstr(115,374)]);
(329, [EatInstr(110,375)]);
(330, [AContInstr3(271,__g0,__binder4,376);ACallInstr3(__g0,8)]);
(331, [EatInstr(110,377)]);
(332, [EatInstr(100,378)]);
(333, [EatInstr(110,379)]);
(334, [EatInstr(108,380)]);
(335, [EatInstr(114,381)]);
(336, [EatInstr(45,382)]);
(337, [EatInstr(45,383)]);
(338, [EatInstr(101,384)]);
(339, [EatInstr(101,385)]);
(340, [EatInstr(108,386)]);
(341, [EatInstr(108,387)]);
(342, [EatInstr(111,388)]);
(343, [EatInstr(108,389)]);
(344, [EatInstr(112,390)]);
(345, [EatInstr(45,391)]);
(346, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,392)]);
(347, [EatInstr(105,393)]);
(348, [EatInstr(45,394)]);
(349, [EatInstr(116,396);EatInstr(109,395)]);
(350, [EatInstr(116,397)]);
(351, [EatInstr(101,398)]);
(352, [EatInstr(100,399)]);
(353, [AAction2Instr(__a28,207)]);
(354, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,400)]);
(355, [EatInstr(101,401)]);
(356, [EatInstr(99,402)]);
(357, [AAction2Instr(__a29,267)]);
(358, [EatInstr(103,403)]);
(359, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,404)]);
(360, [AAction2Instr(__a30,207)]);
(361, [EatInstr(114,405)]);
(362, [EatInstr(112,406)]);
(363, [EatInstr(117,407)]);
(364, [EatInstr(101,408)]);
(365, [EatInstr(100,409)]);
(366, [EatInstr(97,410)]);
(367, [EatInstr(99,411)]);
(368, [EatInstr(108,412)]);
(369, [EatInstr(114,413)]);
(370, [EatInstr(108,414)]);
(371, [EatInstr(108,415)]);
(372, [EatInstr(116,416)]);
(373, [EatInstr(101,417)]);
(374, [EatInstr(116,418)]);
(375, [EatInstr(99,419)]);
(376, [AAction2Instr(__a31,252)]);
(377, [EatInstr(111,420)]);
(378, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,421)]);
(379, [EatInstr(115,422)]);
(380, [EatInstr(97,423)]);
(381, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,424)]);
(382, [EatInstr(97,425)]);
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
