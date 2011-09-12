
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
let __a27 = (_p 1026 ((2045)));;
let __a12 = (_p 1005 ((2018)));;
let __a25 = (_p 1007 ((2016)));;
let __a9 = (_p 1055 ((2087)));;
let __a19 = (_p 1035 ((2038)));;
let __a18 = (_p 1009 ((2014)));;
let __a32 = (_p 1066 ((2079)));;
let __a34 = (_p 1045 ((2030)));;
let __a81 = (_p 1037 ((2036)));;
let __a71 = (_p 1091 ((2053)));;
let __a67 = (_p 1039 ((2034)));;
let __a17 = (_p 1011 ((2012)));;
let __a79 = (_p 1061 ((2081)));;
let __a66 = (_p 1013 ((2010)));;
let __a73 = (_p 1068 ((2076)));;
let __a30 = (_p 1041 ((2032)));;
let __a59 = (_p 1070 ((2074)));;
let __a86 = (_p 1072 ((2072)));;
let __a50 = (_p 1014 ((2009)));;
let __a58 = (_p 1074 ((2070)));;
let __a13 = (_p 1048 ((2026)));;
let __a44 = (fun _x0_ _x1_ -> (((_p 1043 ((2028))) _x0_) (((_p 1044 ((2031))) _x0_) _x1_)));;
let __a10 = (_p 1016 ((2007)));;
let __a88 = (_p 1022 ((2051)));;
let __a28 = (_p 1018 ((2005)));;
let __a75 = (_p 1020 ((2003)));;
let __a78 = (_p 1075 ((2069)));;
let __a26 = (fun _x0_ _x1_ -> (((_p 1046 ((2029))) _x0_) (((_p 1047 ((2027))) _x0_) _x1_)));;
let __a21 = (_p 1027 ((2047)));;
let __a76 = (_p 1050 ((2023)));;
let __a72 = (_p 1077 ((2067)));;
let __a36 = (_p 1081 ((2064)));;
let __a2 = (_p 1002 ((2001)));;
let __a29 = (_p 1052 ((2021)));;
let __a16 = (_p 1025 ((2048)));;
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
let __a24 = (_p 1004 ((2019)));;
let __a5 = (_p 1000 ((2000)));;
let __a64 = (_p 1064 ((2080)));;
let __a51 = (_p 1006 ((2017)));;
let __a85 = (_p 1034 ((2039)));;
let __a43 = (_p 1008 ((2015)));;
let __a61 = (_p 1036 ((2037)));;
let __a23 = (_p 1067 ((2078)));;
let __a63 = (_p 1090 ((2054)));;
let __a11 = (_p 1010 ((2013)));;
let __a15 = (_p 1038 ((2035)));;
let __a69 = (_p 1012 ((2011)));;
let __a31 = (_p 1092 ((2052)));;
let __a35 = (_p 1042 ((2025)));;
let __a14 = (_p 1040 ((2033)));;
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
let __a6 = (_p 1028 ((2046)));;
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
(4, [AContInstr3(272,__g0,__binder1,15);ACallInstr3(__g0,9)]);
(388, [EatInstr(105,432)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(97,433)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,44)]);
(390, [EatInstr(45,434)]);
(7, [EatInstr(127,46);EatInstr(126,46);EatInstr(125,46);EatInstr(124,46);EatInstr(123,46);EatInstr(122,46);EatInstr(121,46);EatInstr(120,46);EatInstr(119,46);EatInstr(118,46);EatInstr(117,46);EatInstr(116,46);EatInstr(115,46);EatInstr(114,46);EatInstr(113,46);EatInstr(112,46);EatInstr(111,46);EatInstr(110,46);EatInstr(109,46);EatInstr(108,46);EatInstr(107,46);EatInstr(106,46);EatInstr(105,46);EatInstr(104,46);EatInstr(103,46);EatInstr(102,46);EatInstr(101,46);EatInstr(100,46);EatInstr(99,46);EatInstr(98,46);EatInstr(97,46);EatInstr(96,46);EatInstr(95,46);EatInstr(94,46);EatInstr(93,46);EatInstr(92,46);EatInstr(91,46);EatInstr(90,46);EatInstr(89,46);EatInstr(88,46);EatInstr(87,46);EatInstr(86,46);EatInstr(85,46);EatInstr(84,46);EatInstr(83,46);EatInstr(82,46);EatInstr(81,46);EatInstr(80,46);EatInstr(79,46);EatInstr(78,46);EatInstr(77,46);EatInstr(76,46);EatInstr(75,46);EatInstr(74,46);EatInstr(73,46);EatInstr(72,46);EatInstr(71,46);EatInstr(70,46);EatInstr(69,46);EatInstr(68,46);EatInstr(67,46);EatInstr(66,46);EatInstr(65,46);EatInstr(64,46);EatInstr(63,46);EatInstr(62,46);EatInstr(61,46);EatInstr(60,46);EatInstr(59,46);EatInstr(58,46);EatInstr(57,46);EatInstr(56,46);EatInstr(55,46);EatInstr(54,46);EatInstr(53,46);EatInstr(52,46);EatInstr(51,46);EatInstr(50,46);EatInstr(49,46);EatInstr(48,46);EatInstr(47,46);EatInstr(46,46);EatInstr(44,46);EatInstr(43,46);EatInstr(42,46);EatInstr(41,46);EatInstr(40,46);EatInstr(39,46);EatInstr(38,46);EatInstr(37,46);EatInstr(36,46);EatInstr(35,46);EatInstr(34,46);EatInstr(33,46);EatInstr(32,46);EatInstr(31,46);EatInstr(30,46);EatInstr(29,46);EatInstr(28,46);EatInstr(27,46);EatInstr(26,46);EatInstr(25,46);EatInstr(24,46);EatInstr(23,46);EatInstr(22,46);EatInstr(21,46);EatInstr(20,46);EatInstr(19,46);EatInstr(18,46);EatInstr(17,46);EatInstr(16,46);EatInstr(15,46);EatInstr(14,46);EatInstr(13,46);EatInstr(12,46);EatInstr(11,46);EatInstr(10,46);EatInstr(9,46);EatInstr(8,46);EatInstr(7,46);EatInstr(6,46);EatInstr(5,46);EatInstr(4,46);EatInstr(3,46);EatInstr(2,46);EatInstr(1,46)]);
(391, [EatInstr(104,435)]);
(8, [EatInstr(119,28);EatInstr(117,27);EatInstr(115,26);EatInstr(114,25);EatInstr(112,24);EatInstr(109,23);EatInstr(108,22);EatInstr(105,21);EatInstr(104,20);EatInstr(100,19);EatInstr(99,18);EatInstr(97,17)]);
(392, [AAction2Instr(__a32,436)]);
(9, [EatInstr(116,39);EatInstr(115,38);EatInstr(114,37);EatInstr(112,36);EatInstr(108,35);EatInstr(105,34);EatInstr(103,33);EatInstr(102,32);EatInstr(101,31);EatInstr(100,30);EatInstr(99,29);AContInstr3(271,__g0,__binder2,40);ACallInstr3(__g0,8)]);
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
(15, [AAction2Instr(__a2,147)]);
(399, [EatInstr(101,443)]);
(16, [CompleteInstr(268)]);
(400, [AAction2Instr(__a33,258)]);
(17, [EatInstr(116,49);EatInstr(114,48)]);
(401, [EatInstr(105,444)]);
(18, [EatInstr(111,51);EatInstr(108,50)]);
(402, [EatInstr(101,445)]);
(19, [EatInstr(101,52)]);
(403, [EatInstr(108,446)]);
(20, [EatInstr(97,53)]);
(404, [EatInstr(103,447)]);
(21, [EatInstr(110,54)]);
(405, [EatInstr(101,448)]);
(22, [EatInstr(105,56);EatInstr(101,55)]);
(406, [EatInstr(97,449)]);
(23, [EatInstr(105,57)]);
(407, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,450)]);
(24, [EatInstr(114,58)]);
(408, [EatInstr(121,451)]);
(25, [EatInstr(101,59)]);
(409, [AAction2Instr(__a35,78);AAction2Instr(__a34,452)]);
(26, [EatInstr(117,60)]);
(410, [EatInstr(97,453)]);
(27, [EatInstr(110,61)]);
(411, [EatInstr(45,454)]);
(28, [EatInstr(114,62)]);
(412, [EatInstr(104,455)]);
(29, [EatInstr(111,63)]);
(413, [EatInstr(101,456)]);
(30, [EatInstr(111,65);EatInstr(101,64)]);
(414, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,457)]);
(31, [EatInstr(120,66)]);
(415, [EatInstr(101,458)]);
(32, [EatInstr(117,67)]);
(416, [EatInstr(108,459)]);
(33, [EatInstr(101,68)]);
(417, [EatInstr(101,460)]);
(34, [EatInstr(110,69)]);
(418, [EatInstr(101,461)]);
(35, [EatInstr(114,71);EatInstr(111,70)]);
(419, [EatInstr(45,462)]);
(36, [EatInstr(114,73);EatInstr(97,72)]);
(420, [EatInstr(116,463)]);
(37, [EatInstr(102,74)]);
(421, [EatInstr(119,467);EatInstr(116,466);EatInstr(112,465);EatInstr(102,464)]);
(38, [EatInstr(116,76);EatInstr(111,75)]);
(422, [EatInstr(101,468)]);
(39, [EatInstr(114,77)]);
(423, [EatInstr(98,469)]);
(40, [AAction2Instr(__a3,78)]);
(424, [AAction2Instr(__a36,470)]);
(41, [EatInstr(118,91);EatInstr(117,90);EatInstr(114,89);EatInstr(112,88);EatInstr(111,87);EatInstr(110,86);EatInstr(109,85);EatInstr(108,84);EatInstr(105,83);EatInstr(104,82);EatInstr(99,81);EatInstr(98,80);EatInstr(97,79)]);
(425, [EatInstr(114,471)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,92)]);
(426, [EatInstr(115,472)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(101,473)]);
(44, [ALookaheadInstr(false,CfgLA (1,264),45);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,44)]);
(428, [EatInstr(100,474)]);
(45, [CompleteInstr(269)]);
(429, [EatInstr(104,475)]);
(46, [ALookaheadInstr(false,CfgLA (1,264),47);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,46)]);
(430, [EatInstr(115,476)]);
(47, [CompleteInstr(270)]);
(431, [EatInstr(112,477)]);
(48, [EatInstr(114,95)]);
(432, [EatInstr(122,478)]);
(49, [EatInstr(116,96)]);
(433, [EatInstr(121,479)]);
(50, [EatInstr(111,97)]);
(434, [EatInstr(111,480)]);
(51, [EatInstr(112,98)]);
(435, [EatInstr(105,481)]);
(52, [EatInstr(115,99)]);
(436, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,482)]);
(53, [EatInstr(115,100)]);
(437, [EatInstr(116,483)]);
(54, [EatInstr(108,102);EatInstr(102,101)]);
(438, [EatInstr(116,484)]);
(55, [EatInstr(120,103)]);
(439, [AAction2Instr(__a37,199)]);
(56, [EatInstr(102,104)]);
(440, [AAction2Instr(__a38,199)]);
(57, [EatInstr(110,105)]);
(441, [EatInstr(116,485)]);
(58, [EatInstr(101,106)]);
(442, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,486)]);
(59, [EatInstr(112,107)]);
(443, [EatInstr(114,487)]);
(60, [EatInstr(98,108)]);
(444, [EatInstr(108,488)]);
(61, [EatInstr(114,109)]);
(445, [EatInstr(115,489)]);
(62, [EatInstr(97,110)]);
(446, [EatInstr(108,490)]);
(63, [EatInstr(114,112);EatInstr(109,111)]);
(447, [EatInstr(117,491)]);
(64, [EatInstr(112,113)]);
(448, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,492)]);
(65, [EatInstr(116,114)]);
(449, [EatInstr(114,493)]);
(66, [EatInstr(116,116);EatInstr(101,115)]);
(450, [AAction2Instr(__a39,78)]);
(67, [EatInstr(115,117)]);
(451, [EatInstr(45,494)]);
(68, [EatInstr(116,118)]);
(452, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,495)]);
(69, [EatInstr(102,119)]);
(453, [EatInstr(116,496)]);
(70, [EatInstr(111,120)]);
(454, [EatInstr(97,497)]);
(71, [EatInstr(49,121)]);
(455, [EatInstr(101,498)]);
(72, [EatInstr(114,122)]);
(456, [EatInstr(45,499)]);
(73, [EatInstr(105,124);EatInstr(101,123)]);
(457, [AAction2Instr(__a40,78)]);
(74, [EatInstr(99,125)]);
(458, [EatInstr(100,500)]);
(75, [EatInstr(114,126)]);
(459, [EatInstr(97,501)]);
(76, [EatInstr(114,127)]);
(460, [EatInstr(118,502)]);
(77, [EatInstr(97,128)]);
(461, [EatInstr(45,503)]);
(78, [CompleteInstr(272)]);
(462, [EatInstr(100,504)]);
(79, [EatInstr(114,130);EatInstr(102,129)]);
(463, [EatInstr(97,505)]);
(80, [EatInstr(97,131)]);
(464, [EatInstr(117,506)]);
(81, [EatInstr(111,134);EatInstr(104,133);EatInstr(97,132)]);
(465, [EatInstr(101,507)]);
(82, [EatInstr(121,135)]);
(466, [EatInstr(120,508)]);
(83, [EatInstr(110,136)]);
(467, [EatInstr(97,509)]);
(84, [EatInstr(111,137)]);
(468, [EatInstr(110,510)]);
(85, [EatInstr(101,138)]);
(469, [EatInstr(101,511)]);
(86, [EatInstr(111,139)]);
(470, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,548)]);
(87, [EatInstr(110,140)]);
(471, [EatInstr(114,512)]);
(88, [EatInstr(114,141)]);
(472, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,513)]);
(89, [EatInstr(111,142)]);
(473, [EatInstr(103,514)]);
(90, [EatInstr(115,144);EatInstr(110,143)]);
(474, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,515)]);
(91, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,145)]);
(475, [EatInstr(105,516)]);
(92, [AAction2Instr(__a4,146)]);
(476, [EatInstr(99,517)]);
(93, [AAction2Instr(__a5,147)]);
(477, [EatInstr(115,518)]);
(94, [CompleteInstr(267)]);
(478, [EatInstr(101,519)]);
(95, [EatInstr(111,148)]);
(479, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,520)]);
(96, [EatInstr(114,149)]);
(480, [EatInstr(112,521)]);
(97, [EatInstr(115,150)]);
(481, [EatInstr(115,522)]);
(98, [EatInstr(121,151)]);
(482, [AAction2Instr(__a41,199)]);
(99, [EatInstr(117,152)]);
(483, [EatInstr(111,523)]);
(100, [EatInstr(104,153)]);
(484, [EatInstr(97,524)]);
(101, [EatInstr(101,154)]);
(485, [EatInstr(105,525)]);
(102, [EatInstr(105,155)]);
(486, [AAction2Instr(__a42,258)]);
(103, [EatInstr(101,156)]);
(487, [EatInstr(45,526)]);
(104, [EatInstr(116,157)]);
(488, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,527)]);
(105, [EatInstr(117,158)]);
(489, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,528)]);
(106, [EatInstr(99,159)]);
(490, [EatInstr(97,529)]);
(107, [EatInstr(108,160)]);
(491, [EatInstr(108,530)]);
(108, [EatInstr(115,161)]);
(492, [AAction2Instr(__a43,258)]);
(109, [EatInstr(111,162)]);
(493, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,531)]);
(110, [EatInstr(112,163)]);
(494, [EatInstr(103,532)]);
(111, [EatInstr(112,164)]);
(495, [AAction2Instr(__a44,364)]);
(112, [EatInstr(111,165)]);
(496, [EatInstr(111,533)]);
(113, [EatInstr(101,166)]);
(497, [EatInstr(110,534)]);
(114, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,167)]);
(498, [EatInstr(97,535)]);
(115, [EatInstr(99,168)]);
(499, [EatInstr(97,536)]);
(116, [EatInstr(114,169)]);
(500, [EatInstr(115,537)]);
(117, [EatInstr(101,170)]);
(501, [EatInstr(98,538)]);
(118, [EatInstr(45,171)]);
(502, [EatInstr(97,539)]);
(119, [EatInstr(111,172)]);
(503, [EatInstr(97,540)]);
(120, [EatInstr(107,173)]);
(504, [EatInstr(121,541)]);
(121, [EatInstr(45,174)]);
(505, [EatInstr(116,542)]);
(122, [EatInstr(115,175)]);
(506, [EatInstr(110,543)]);
(123, [EatInstr(99,176)]);
(507, [EatInstr(103,544)]);
(124, [EatInstr(110,177)]);
(508, [AAction2Instr(__a45,649)]);
(125, [AAction2Instr(__a6,178)]);
(509, [EatInstr(100,545)]);
(126, [EatInstr(116,179)]);
(510, [EatInstr(115,546)]);
(127, [EatInstr(105,180)]);
(511, [EatInstr(108,547)]);
(128, [EatInstr(110,181)]);
(512, [EatInstr(111,550)]);
(129, [EatInstr(116,182)]);
(513, [AAction2Instr(__a46,199)]);
(130, [EatInstr(114,183)]);
(514, [EatInstr(117,551)]);
(131, [EatInstr(99,184)]);
(515, [AAction2Instr(__a47,199)]);
(132, [EatInstr(115,185)]);
(516, [EatInstr(115,552)]);
(133, [EatInstr(101,186)]);
(517, [EatInstr(101,553)]);
(134, [EatInstr(117,187)]);
(518, [EatInstr(101,554)]);
(135, [EatInstr(98,188)]);
(519, [EatInstr(45,555)]);
(136, [EatInstr(108,189)]);
(520, [AAction2Instr(__a48,199)]);
(137, [EatInstr(111,190)]);
(521, [EatInstr(116,556)]);
(138, [EatInstr(109,191)]);
(522, [EatInstr(116,557)]);
(139, [EatInstr(45,192)]);
(523, [EatInstr(114,558)]);
(140, [EatInstr(108,193)]);
(524, [EatInstr(114,559)]);
(141, [EatInstr(101,194)]);
(525, [EatInstr(111,560)]);
(142, [EatInstr(111,195)]);
(526, [EatInstr(99,561)]);
(143, [EatInstr(114,197);EatInstr(105,196)]);
(527, [AAction2Instr(__a49,258)]);
(144, [EatInstr(101,198)]);
(528, [AAction2Instr(__a50,258)]);
(145, [AAction2Instr(__a7,199)]);
(529, [EatInstr(98,562)]);
(146, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,200)]);
(530, [EatInstr(97,563)]);
(147, [ALookaheadInstr(false,CfgLA (3,266),94);ACallInstr3(__default_call,11);AContInstr3(273,__g0,__binder3,93);ACallInstr3(__g0,10);ASimpleCont2Instr(274,__binder0,94)]);
(531, [AAction2Instr(__a51,258)]);
(148, [EatInstr(119,201)]);
(532, [EatInstr(114,564)]);
(149, [EatInstr(105,202)]);
(533, [EatInstr(114,565)]);
(150, [EatInstr(101,203)]);
(534, [EatInstr(97,566)]);
(151, [EatInstr(114,204)]);
(535, [EatInstr(100,567)]);
(152, [EatInstr(103,205)]);
(536, [EatInstr(110,568)]);
(153, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,206)]);
(537, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,569)]);
(154, [EatInstr(114,207)]);
(538, [EatInstr(108,570)]);
(155, [EatInstr(110,208)]);
(539, [EatInstr(110,571)]);
(156, [EatInstr(114,209)]);
(540, [EatInstr(99,572)]);
(157, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,210)]);
(541, [EatInstr(112,573)]);
(158, [EatInstr(115,211)]);
(542, [EatInstr(105,574)]);
(159, [EatInstr(101,212)]);
(543, [AAction2Instr(__a52,649)]);
(160, [EatInstr(97,213)]);
(544, [EatInstr(45,575);AAction2Instr(__a53,649)]);
(161, [EatInstr(101,214)]);
(545, [EatInstr(108,577)]);
(162, [EatInstr(108,215)]);
(546, [EatInstr(105,578)]);
(163, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,216)]);
(547, [EatInstr(115,579)]);
(164, [EatInstr(105,217)]);
(548, [AAction2Instr(__a54,549);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,548)]);
(165, [EatInstr(117,218)]);
(549, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,580)]);
(166, [EatInstr(110,219)]);
(550, [EatInstr(119,581)]);
(167, [AAction2Instr(__a8,78)]);
(551, [EatInstr(108,582)]);
(168, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,220)]);
(552, [EatInstr(116,583)]);
(169, [EatInstr(97,221)]);
(553, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,584)]);
(170, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,222)]);
(554, [EatInstr(45,585)]);
(171, [EatInstr(103,223)]);
(555, [EatInstr(104,586)]);
(172, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,224)]);
(556, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,587)]);
(173, [EatInstr(97,225)]);
(557, [EatInstr(111,588)]);
(174, [EatInstr(108,226)]);
(558, [EatInstr(121,589)]);
(175, [EatInstr(101,227)]);
(559, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,590)]);
(176, [EatInstr(101,228)]);
(560, [EatInstr(110,591)]);
(177, [EatInstr(116,229)]);
(561, [EatInstr(111,592)]);
(178, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,279)]);
(562, [EatInstr(108,593)]);
(179, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,230)]);
(563, [EatInstr(114,594)]);
(180, [EatInstr(112,231)]);
(564, [EatInstr(97,595)]);
(181, [EatInstr(115,232)]);
(565, [EatInstr(115,596)]);
(182, [EatInstr(101,233)]);
(566, [EatInstr(108,597)]);
(183, [EatInstr(111,234)]);
(567, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,598)]);
(184, [EatInstr(107,235)]);
(568, [EatInstr(97,599)]);
(185, [EatInstr(101,236)]);
(569, [AAction2Instr(__a55,78)]);
(186, [EatInstr(99,237)]);
(570, [EatInstr(101,600)]);
(187, [EatInstr(110,238)]);
(571, [EatInstr(99,601)]);
(188, [EatInstr(114,239)]);
(572, [EatInstr(116,602)]);
(189, [EatInstr(105,240)]);
(573, [EatInstr(103,603)]);
(190, [EatInstr(107,241)]);
(574, [EatInstr(111,604)]);
(191, [EatInstr(111,242)]);
(575, [EatInstr(115,605)]);
(192, [EatInstr(115,246);EatInstr(114,245);EatInstr(109,244);EatInstr(99,243)]);
(576, [AAction2Instr(__a56,199)]);
(193, [EatInstr(121,247)]);
(577, [EatInstr(101,606)]);
(194, [EatInstr(102,248)]);
(578, [EatInstr(116,607)]);
(195, [EatInstr(116,249)]);
(579, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,608)]);
(196, [EatInstr(116,250)]);
(580, [AAction2Instr(__a57,199)]);
(197, [EatInstr(111,251)]);
(581, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,609)]);
(198, [EatInstr(45,252)]);
(582, [EatInstr(97,610)]);
(199, [CompleteInstr(273)]);
(583, [EatInstr(111,611)]);
(200, [AAction2Instr(__a9,199)]);
(584, [AAction2Instr(__a58,199)]);
(201, [EatInstr(45,253)]);
(585, [EatInstr(99,612)]);
(202, [EatInstr(98,254)]);
(586, [EatInstr(105,613)]);
(203, [EatInstr(45,255)]);
(587, [AAction2Instr(__a59,199)]);
(204, [EatInstr(117,256)]);
(588, [EatInstr(114,614)]);
(205, [EatInstr(97,257)]);
(589, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,615)]);
(206, [AAction2Instr(__a10,258)]);
(590, [AAction2Instr(__a60,616)]);
(207, [EatInstr(45,259)]);
(591, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,617)]);
(208, [EatInstr(101,260)]);
(592, [EatInstr(114,618)]);
(209, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,261)]);
(593, [EatInstr(101,619)]);
(210, [AAction2Instr(__a11,258)]);
(594, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,620)]);
(211, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,262)]);
(595, [EatInstr(112,621)]);
(212, [EatInstr(100,263)]);
(596, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,622)]);
(213, [EatInstr(121,264)]);
(597, [EatInstr(121,623)]);
(214, [EatInstr(116,265)]);
(598, [AAction2Instr(__a61,78)]);
(215, [EatInstr(108,266)]);
(599, [EatInstr(108,624)]);
(216, [AAction2Instr(__a12,258)]);
(600, [EatInstr(45,625)]);
(217, [EatInstr(108,267)]);
(601, [EatInstr(101,626)]);
(218, [EatInstr(116,268)]);
(602, [EatInstr(105,627)]);
(219, [EatInstr(100,269)]);
(603, [EatInstr(101,628)]);
(220, [AAction2Instr(__a13,270)]);
(604, [EatInstr(110,629)]);
(221, [EatInstr(99,271)]);
(605, [EatInstr(116,630)]);
(222, [AAction2Instr(__a14,78)]);
(606, [EatInstr(114,631)]);
(223, [EatInstr(101,272)]);
(607, [EatInstr(105,632)]);
(224, [AAction2Instr(__a15,78)]);
(608, [AAction2Instr(__a62,199)]);
(225, [EatInstr(104,273)]);
(609, [AAction2Instr(__a63,199)]);
(226, [EatInstr(111,274)]);
(610, [EatInstr(114,633)]);
(227, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,275)]);
(611, [EatInstr(114,634)]);
(228, [EatInstr(100,276)]);
(612, [EatInstr(97,635)]);
(229, [EatInstr(45,278);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,277)]);
(613, [EatInstr(115,636)]);
(230, [AAction2Instr(__a16,78)]);
(614, [EatInstr(121,637)]);
(231, [EatInstr(45,281)]);
(615, [AAction2Instr(__a64,199)]);
(232, [EatInstr(108,282)]);
(616, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,656)]);
(233, [EatInstr(114,283)]);
(617, [AAction2Instr(__a65,258)]);
(234, [EatInstr(119,284)]);
(618, [EatInstr(101,638)]);
(235, [EatInstr(101,285)]);
(619, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,639)]);
(236, [EatInstr(45,286)]);
(620, [AAction2Instr(__a66,258)]);
(237, [EatInstr(107,287)]);
(621, [EatInstr(104,640)]);
(238, [EatInstr(116,288)]);
(622, [AAction2Instr(__a67,78)]);
(239, [EatInstr(105,289)]);
(623, [EatInstr(115,641)]);
(240, [EatInstr(110,290)]);
(624, [EatInstr(121,642)]);
(241, [EatInstr(97,291)]);
(625, [EatInstr(112,643)]);
(242, [EatInstr(105,292)]);
(626, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,644)]);
(243, [EatInstr(111,293)]);
(627, [EatInstr(111,645)]);
(244, [EatInstr(101,294)]);
(628, [EatInstr(110,646)]);
(245, [EatInstr(101,295)]);
(629, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,647)]);
(246, [EatInstr(107,296)]);
(630, [EatInstr(114,648)]);
(247, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,297)]);
(631, [AAction2Instr(__a68,649)]);
(248, [EatInstr(105,298)]);
(632, [EatInstr(118,650)]);
(249, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,299)]);
(633, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,651)]);
(250, [EatInstr(45,300)]);
(634, [EatInstr(121,652)]);
(251, [EatInstr(108,301)]);
(635, [EatInstr(108,653)]);
(252, [EatInstr(102,302)]);
(636, [EatInstr(116,654)]);
(253, [EatInstr(110,303)]);
(637, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,655)]);
(254, [EatInstr(117,304)]);
(638, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,658)]);
(255, [EatInstr(117,305)]);
(639, [AAction2Instr(__a69,258)]);
(256, [EatInstr(108,306)]);
(640, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,659)]);
(257, [EatInstr(114,307)]);
(641, [EatInstr(105,660)]);
(258, [CompleteInstr(271)]);
(642, [EatInstr(115,661)]);
(259, [EatInstr(116,309)]);
(643, [EatInstr(114,662)]);
(260, [EatInstr(45,310)]);
(644, [AAction2Instr(__a70,78)]);
(261, [AAction2Instr(__a17,258)]);
(645, [EatInstr(110,663)]);
(262, [AAction2Instr(__a18,258)]);
(646, [EatInstr(45,665);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,664)]);
(263, [EatInstr(101,311)]);
(647, [AAction2Instr(__a71,199)]);
(264, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,312)]);
(648, [EatInstr(105,666)]);
(265, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,313)]);
(649, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,576)]);
(266, [EatInstr(45,314)]);
(650, [EatInstr(101,667)]);
(267, [EatInstr(101,315)]);
(651, [AAction2Instr(__a72,199)]);
(268, [EatInstr(105,316)]);
(652, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,668)]);
(269, [EatInstr(101,317)]);
(653, [EatInstr(108,669)]);
(270, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,318)]);
(654, [EatInstr(111,670)]);
(271, [EatInstr(116,319)]);
(655, [AAction2Instr(__a73,199)]);
(272, [EatInstr(110,320)]);
(656, [AAction2Instr(__a74,657);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,656)]);
(273, [EatInstr(101,321)]);
(657, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,671)]);
(274, [EatInstr(111,322)]);
(658, [AAction2Instr(__a75,258)]);
(275, [AAction2Instr(__a19,78)]);
(659, [AAction2Instr(__a76,78)]);
(276, [EatInstr(101,323)]);
(660, [EatInstr(115,672)]);
(277, [AAction2Instr(__a20,78)]);
(661, [EatInstr(105,673)]);
(278, [EatInstr(114,326);EatInstr(110,325);EatInstr(103,324)]);
(662, [EatInstr(101,674)]);
(279, [AAction2Instr(__a21,280);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,279)]);
(663, [EatInstr(115,675)]);
(280, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,327)]);
(664, [AAction2Instr(__a77,78)]);
(281, [EatInstr(108,328)]);
(665, [EatInstr(115,676)]);
(282, [EatInstr(97,329)]);
(666, [EatInstr(99,677)]);
(283, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,330)]);
(667, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,678)]);
(284, [EatInstr(45,331)]);
(668, [AAction2Instr(__a78,199)]);
(285, [EatInstr(110,332)]);
(669, [EatInstr(115,679)]);
(286, [EatInstr(105,333)]);
(670, [EatInstr(114,680)]);
(287, [EatInstr(45,334)]);
(671, [AAction2Instr(__a79,199)]);
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
(678, [AAction2Instr(__a80,199)]);
(295, [EatInstr(112,343)]);
(679, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,687)]);
(296, [EatInstr(105,344)]);
(680, [EatInstr(121,688)]);
(297, [AAction2Instr(__a22,199)]);
(681, [AAction2Instr(__a81,78)]);
(298, [EatInstr(120,345)]);
(682, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,689)]);
(299, [AAction2Instr(__a23,346)]);
(683, [EatInstr(105,690)]);
(300, [EatInstr(104,347)]);
(684, [AAction2Instr(__a82,78)]);
(301, [EatInstr(108,348)]);
(685, [EatInstr(97,691)]);
(302, [EatInstr(115,349)]);
(686, [AAction2Instr(__a83,649)]);
(303, [EatInstr(111,350)]);
(687, [AAction2Instr(__a84,199)]);
(304, [EatInstr(116,351)]);
(688, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,692)]);
(305, [EatInstr(110,352)]);
(689, [AAction2Instr(__a85,78)]);
(306, [EatInstr(101,353)]);
(690, [EatInstr(99,693)]);
(307, [EatInstr(45,355);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,354)]);
(691, [EatInstr(110,694)]);
(692, [AAction2Instr(__a86,199)]);
(309, [EatInstr(121,356)]);
(693, [EatInstr(97,695)]);
(310, [EatInstr(114,358);EatInstr(110,357)]);
(694, [EatInstr(110,696)]);
(311, [EatInstr(110,359)]);
(695, [EatInstr(116,697)]);
(312, [AAction2Instr(__a24,258)]);
(696, [EatInstr(101,698)]);
(313, [AAction2Instr(__a25,258)]);
(697, [EatInstr(101,699)]);
(314, [EatInstr(115,360)]);
(698, [EatInstr(114,700)]);
(315, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,361)]);
(699, [EatInstr(115,701)]);
(316, [EatInstr(110,362)]);
(700, [EatInstr(108,702)]);
(317, [EatInstr(110,363)]);
(701, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,703)]);
(318, [AAction2Instr(__a26,364)]);
(702, [EatInstr(101,704)]);
(319, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,365)]);
(703, [AAction2Instr(__a87,78)]);
(320, [EatInstr(101,366)]);
(704, [EatInstr(115,705)]);
(321, [EatInstr(97,367)]);
(705, [EatInstr(115,706)]);
(322, [EatInstr(107,368)]);
(706, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,707)]);
(323, [EatInstr(110,369)]);
(707, [AAction2Instr(__a88,78)]);
(324, [EatInstr(105,370)]);
(325, [EatInstr(117,372);EatInstr(112,371)]);
(326, [EatInstr(101,373)]);
(327, [AAction2Instr(__a27,78)]);
(328, [EatInstr(97,374)]);
(329, [EatInstr(116,375)]);
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
(353, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,400)]);
(354, [AAction2Instr(__a28,258)]);
(355, [EatInstr(103,401)]);
(356, [EatInstr(112,402)]);
(357, [EatInstr(117,403)]);
(358, [EatInstr(101,404)]);
(359, [EatInstr(99,405)]);
(360, [EatInstr(116,406)]);
(361, [AAction2Instr(__a29,78)]);
(362, [EatInstr(101,407)]);
(363, [EatInstr(99,408)]);
(364, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,409)]);
(365, [AAction2Instr(__a30,78)]);
(366, [EatInstr(114,410)]);
(367, [EatInstr(100,411)]);
(368, [EatInstr(97,412)]);
(369, [EatInstr(99,413)]);
(370, [EatInstr(108,414)]);
(371, [EatInstr(114,415)]);
(372, [EatInstr(108,416)]);
(373, [EatInstr(108,417)]);
(374, [EatInstr(116,418)]);
(375, [EatInstr(101,419)]);
(376, [AAction2Instr(__a31,199)]);
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
