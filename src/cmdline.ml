
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
 | (2043) -> ((); ();  Print_relevance_cmd )
 | (2044) -> ((); (let _x10 = _p() in (); (let _x9 = _p() in (let n = Yak.YkBuf.get_string _x10 _x9 ykinput in ();  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" ))))
 | (2047) -> ((); ();  Sort_cmd )
 | (2048) -> ((); ();  Strip_late_actions_cmd )
 | (2049) -> ((); ();  Translate_dypgen_cmd )
 | (2050) -> ((); ();  Translate_dypgen_scannerless_cmd )
 | _ -> raise Exit)
and
 _r_args(_n,_p,ykinput) = (match _n() with
 | (2051) -> ((); (); (let p = _r_phases(_n,_p,ykinput) in  after := Some p ; ()))
 | (2052) -> ((); ();  Compileopt.use_wrap_and_attr := false;      
                                            Compileopt.use_coroutines    := false ; ())
 | (2053) -> ((); ();  Compileopt.use_coroutines := false ; ())
 | (2054) -> ((); (); (let b = (match _n() with
 | (2055) -> ((); Fun_BE)
 | (2056) -> ((); Trans_BE)
 | (2057) -> ((); Wadler_BE)
 | (2058) -> ((); Peg_BE false)
 | (2059) -> ((); Peg_BE true)
 | _ -> raise Exit) in ();  backend := b ; ()))
 | (2060) -> ((); ();  Compileopt.case_sensitive := false ; ())
 | (2061) -> ((); ();  Compileopt.check_labels := true ; ())
 | (2062) -> ((); (); (let _x12 = _p() in (); (let _x11 = _p() in (let n = Yak.YkBuf.get_string _x12 _x11 ykinput in ();  Variables.counter := (int_of_string n) ; ()))))
 | (2065) -> ((); ();  Compileopt.inline_cs := true ; ())
 | (2066) -> ((); ();  Compileopt.inline_regular := true ; ())
 | (2067) -> ((); ();  Compileopt.lookahead := true ; ())
 | (2068) -> ((); ();  Compileopt.memoize_history := true ; ())
 | (2069) -> ((); ();  Compileopt.coalesce := false ; ())
 | (2070) -> ((); ();  Compileopt.collapse_calls := false ; ())
 | (2071) -> ((); ();  Compileopt.memoize_history := false ; ())
 | (2072) -> ((); ();  Compileopt.gen_optimize_pam := false ; ())
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
 | (2044) -> (();();();push_pos(_p());();push_pos(_p());(); push((2044)))
 | (2047) -> (();();(); push((2047)))
 | (2048) -> (();();(); push((2048)))
 | (2049) -> (();();(); push((2049)))
 | (2050) -> (();();(); push((2050)))
 | _ -> raise Exit)
and _rv_args() = (match _n() with
 | (2051) -> (();();_rv_phases();();(); push((2051)))
 | (2052) -> (();();();(); push((2052)))
 | (2053) -> (();();();(); push((2053)))
 | (2054) -> (();();();(match _n() with
 | (2055) -> (();(); push((2055)))
 | (2056) -> (();(); push((2056)))
 | (2057) -> (();(); push((2057)))
 | (2058) -> (();(); push((2058)))
 | (2059) -> (();(); push((2059)))
 | _ -> raise Exit);();(); push((2054)))
 | (2060) -> (();();();(); push((2060)))
 | (2061) -> (();();();(); push((2061)))
 | (2062) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2062)))
 | (2065) -> (();();();(); push((2065)))
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
let __a56 = (_p 1084 ((2054)));;
let __a55 = (_p 1080 ((2064)));;
let __a48 = (_p 1076 ((2067)));;
let __a5 = (_p 1028 ((2045)));;
let __a83 = (_p 1024 ((2048)));;
let __a47 = (_p 1078 ((2065)));;
let __a4 = (_p 1055 ((2089)));;
let __a28 = (_p 1051 ((2021)));;
let __a62 = (_p 1082 ((2061)));;
let __a74 = (_p 1061 ((2083)));;
let __a41 = (_p 1030 ((2042)));;
let __a7 = (_p 1057 ((2086)));;
let __a86 = (_p 1032 ((2040)));;
let __a54 = (_p 1085 ((2059)));;
let __a37 = (_p 1059 ((2084)));;
let __a16 = (_p 1005 ((2018)));;
let __a68 = (_p 1087 ((2057)));;
let __a27 = (_p 1007 ((2016)));;
let __a53 = (_p 1089 ((2055)));;
let __a26 = (_p 1026 ((2044)));;
let __a18 = (_p 1009 ((2014)));;
let __a64 = (_p 1063 ((2080)));;
let __a17 = (_p 1011 ((2012)));;
let __a19 = (_p 1033 ((2039)));;
let __a67 = (_p 1013 ((2010)));;
let __a61 = (_p 1035 ((2037)));;
let __a23 = (_p 1066 ((2078)));;
let __a13 = (_p 1037 ((2035)));;
let __a71 = (_p 1091 ((2052)));;
let __a11 = (_p 1039 ((2033)));;
let __a35 = (_p 1041 ((2025)));;
let __a51 = (_p 1014 ((2009)));;
let __a22 = (_p 1068 ((2075)));;
let __a12 = (_p 1016 ((2007)));;
let __a49 = (_p 1070 ((2073)));;
let __a29 = (_p 1018 ((2005)));;
let __a87 = (_p 1072 ((2071)));;
let __a75 = (_p 1020 ((2003)));;
let __a42 = (_p 1064 ((2077)));;
let __a88 = (_p 1022 ((2050)));;
let __a2 = (_p 1002 ((2001)));;
let __a8 = (_p 1048 ((2024)));;
let __a78 = (_p 1075 ((2068)));;
let __a21 = (_p 1027 ((2046)));;
let __a77 = (_p 1023 ((2049)));;
let __a72 = (_p 1077 ((2066)));;
let __a36 = (_p 1081 ((2063)));;
let __a39 = (_p 1050 ((2022)));;
let __a3 = (_p 1052 ((2020)));;
let __a15 = (_p 1025 ((2047)));;
let __a1 = (_p 1056 ((2088)));;
let __a70 = (_p 1029 ((2043)));;
let __a81 = (_p 1083 ((2060)));;
let __a60 = (_p 1062 ((2082)));;
let __a25 = (_p 1004 ((2019)));;
let __a6 = (_p 1000 ((2000)));;
let __a20 = (_p 1031 ((2041)));;
let __a38 = (_p 1058 ((2085)));;
let __a84 = (_p 1086 ((2058)));;
let __a52 = (_p 1006 ((2017)));;
let __a57 = (_p 1079 ((2062)));;
let __a46 = (_p 1088 ((2056)));;
let __a45 = (_p 1008 ((2015)));;
let __a9 = (_p 1054 ((2087)));;
let __a14 = (_p 1010 ((2013)));;
let __a69 = (_p 1012 ((2011)));;
let __a24 = (fun _x0_ _x1_ -> (((_p 1045 ((2029))) _x0_) (((_p 1046 ((2027))) _x0_) _x1_)));;
let __a32 = (_p 1065 ((2079)));;
let __a34 = (_p 1044 ((2030)));;
let __a40 = (_p 1034 ((2038)));;
let __a82 = (_p 1036 ((2036)));;
let __a66 = (_p 1038 ((2034)));;
let __a63 = (_p 1090 ((2053)));;
let __a80 = (_p 1060 ((2081)));;
let __a31 = (_p 1092 ((2051)));;
let __a73 = (_p 1067 ((2076)));;
let __a30 = (_p 1040 ((2032)));;
let __a65 = (_p 1015 ((2008)));;
let __a59 = (_p 1069 ((2074)));;
let __g0 = (_e);;
let __a50 = (_p 1017 ((2006)));;
let __a79 = (_p 1071 ((2072)));;
let __a33 = (_p 1019 ((2004)));;
let __a85 = (_p 1073 ((2070)));;
let __a10 = (_p 1047 ((2026)));;
let __a44 = (fun _x0_ _x1_ -> (((_p 1042 ((2028))) _x0_) (((_p 1043 ((2031))) _x0_) _x1_)));;
let __a43 = (_p 1021 ((2002)));;
let __a76 = (_p 1049 ((2023)));;
let __a58 = (_p 1074 ((2069)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1003);;
let __binder2 = (_m 1053);;
let __binder3 = (_m 1001);;
let __binder4 = (_m 1093);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(111,426)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,427)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(115,428)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(97,429)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,430)]);
(4, [EatInstr(119,30);EatInstr(117,29);EatInstr(116,28);EatInstr(115,27);EatInstr(114,26);EatInstr(112,25);EatInstr(109,24);EatInstr(108,23);EatInstr(105,22);EatInstr(104,21);EatInstr(103,20);EatInstr(102,19);EatInstr(101,18);EatInstr(100,17);EatInstr(99,16);EatInstr(97,15);AContInstr3(271,__g0,__binder2,32);AContInstr3(272,__g0,__binder1,31)]);
(388, [EatInstr(97,431)]);
(5, [EatInstr(0,33)]);
(389, [EatInstr(114,433);EatInstr(99,432)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,71)]);
(390, [EatInstr(97,434)]);
(7, [EatInstr(127,73);EatInstr(126,73);EatInstr(125,73);EatInstr(124,73);EatInstr(123,73);EatInstr(122,73);EatInstr(121,73);EatInstr(120,73);EatInstr(119,73);EatInstr(118,73);EatInstr(117,73);EatInstr(116,73);EatInstr(115,73);EatInstr(114,73);EatInstr(113,73);EatInstr(112,73);EatInstr(111,73);EatInstr(110,73);EatInstr(109,73);EatInstr(108,73);EatInstr(107,73);EatInstr(106,73);EatInstr(105,73);EatInstr(104,73);EatInstr(103,73);EatInstr(102,73);EatInstr(101,73);EatInstr(100,73);EatInstr(99,73);EatInstr(98,73);EatInstr(97,73);EatInstr(96,73);EatInstr(95,73);EatInstr(94,73);EatInstr(93,73);EatInstr(92,73);EatInstr(91,73);EatInstr(90,73);EatInstr(89,73);EatInstr(88,73);EatInstr(87,73);EatInstr(86,73);EatInstr(85,73);EatInstr(84,73);EatInstr(83,73);EatInstr(82,73);EatInstr(81,73);EatInstr(80,73);EatInstr(79,73);EatInstr(78,73);EatInstr(77,73);EatInstr(76,73);EatInstr(75,73);EatInstr(74,73);EatInstr(73,73);EatInstr(72,73);EatInstr(71,73);EatInstr(70,73);EatInstr(69,73);EatInstr(68,73);EatInstr(67,73);EatInstr(66,73);EatInstr(65,73);EatInstr(64,73);EatInstr(63,73);EatInstr(62,73);EatInstr(61,73);EatInstr(60,73);EatInstr(59,73);EatInstr(58,73);EatInstr(57,73);EatInstr(56,73);EatInstr(55,73);EatInstr(54,73);EatInstr(53,73);EatInstr(52,73);EatInstr(51,73);EatInstr(50,73);EatInstr(49,73);EatInstr(48,73);EatInstr(47,73);EatInstr(46,73);EatInstr(44,73);EatInstr(43,73);EatInstr(42,73);EatInstr(41,73);EatInstr(40,73);EatInstr(39,73);EatInstr(38,73);EatInstr(37,73);EatInstr(36,73);EatInstr(35,73);EatInstr(34,73);EatInstr(33,73);EatInstr(32,73);EatInstr(31,73);EatInstr(30,73);EatInstr(29,73);EatInstr(28,73);EatInstr(27,73);EatInstr(26,73);EatInstr(25,73);EatInstr(24,73);EatInstr(23,73);EatInstr(22,73);EatInstr(21,73);EatInstr(20,73);EatInstr(19,73);EatInstr(18,73);EatInstr(17,73);EatInstr(16,73);EatInstr(15,73);EatInstr(14,73);EatInstr(13,73);EatInstr(12,73);EatInstr(11,73);EatInstr(10,73);EatInstr(9,73);EatInstr(8,73);EatInstr(7,73);EatInstr(6,73);EatInstr(5,73);EatInstr(4,73);EatInstr(3,73);EatInstr(2,73);EatInstr(1,73)]);
(391, [EatInstr(45,435)]);
(8, [EatInstr(119,30);EatInstr(117,29);EatInstr(115,40);EatInstr(114,39);EatInstr(112,38);EatInstr(109,24);EatInstr(108,37);EatInstr(105,36);EatInstr(104,21);EatInstr(100,35);EatInstr(99,34);EatInstr(97,15)]);
(392, [EatInstr(101,436)]);
(9, [EatInstr(119,30);EatInstr(117,29);EatInstr(116,28);EatInstr(115,27);EatInstr(114,26);EatInstr(112,25);EatInstr(109,24);EatInstr(108,23);EatInstr(105,22);EatInstr(104,21);EatInstr(103,20);EatInstr(102,19);EatInstr(101,18);EatInstr(100,17);EatInstr(99,16);EatInstr(97,15);AContInstr3(271,__g0,__binder2,32)]);
(393, [EatInstr(97,437)]);
(10, [EatInstr(45,41);AAction2Instr(__a1,42)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),43)]);
(394, [EatInstr(105,438)]);
(395, [EatInstr(109,439)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(97,440)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(45,441)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(104,442)]);
(15, [EatInstr(116,45);EatInstr(114,44)]);
(399, [AAction2Instr(__a32,443)]);
(16, [EatInstr(111,47);EatInstr(108,46)]);
(400, [EatInstr(115,444)]);
(17, [EatInstr(111,49);EatInstr(101,48)]);
(401, [EatInstr(115,445)]);
(18, [EatInstr(120,50)]);
(402, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,446)]);
(19, [EatInstr(117,51)]);
(403, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,447)]);
(20, [EatInstr(101,52)]);
(404, [EatInstr(97,448)]);
(21, [EatInstr(97,53)]);
(405, [EatInstr(115,449)]);
(22, [EatInstr(110,54)]);
(406, [EatInstr(101,450)]);
(23, [EatInstr(114,58);EatInstr(111,57);EatInstr(105,56);EatInstr(101,55)]);
(407, [AAction2Instr(__a33,268)]);
(24, [EatInstr(105,59)]);
(408, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,451)]);
(25, [EatInstr(114,61);EatInstr(97,60)]);
(409, [EatInstr(121,452)]);
(26, [EatInstr(102,63);EatInstr(101,62)]);
(410, [EatInstr(105,453)]);
(27, [EatInstr(117,66);EatInstr(116,65);EatInstr(111,64)]);
(411, [AAction2Instr(__a35,70);AAction2Instr(__a34,454)]);
(28, [EatInstr(114,67)]);
(412, [EatInstr(97,455)]);
(29, [EatInstr(110,68)]);
(413, [EatInstr(101,456)]);
(30, [EatInstr(114,69)]);
(414, [EatInstr(108,457)]);
(31, [AAction2Instr(__a2,182)]);
(415, [EatInstr(103,458)]);
(32, [AAction2Instr(__a3,70)]);
(416, [EatInstr(45,459)]);
(33, [CompleteInstr(268)]);
(417, [EatInstr(104,460)]);
(34, [EatInstr(111,75);EatInstr(108,46)]);
(418, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,461)]);
(35, [EatInstr(101,76)]);
(419, [EatInstr(101,462)]);
(36, [EatInstr(110,77)]);
(420, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,463)]);
(37, [EatInstr(105,56);EatInstr(101,55)]);
(421, [EatInstr(101,464)]);
(38, [EatInstr(114,78)]);
(422, [EatInstr(101,465)]);
(39, [EatInstr(101,62)]);
(423, [EatInstr(45,466)]);
(40, [EatInstr(117,66)]);
(424, [EatInstr(97,467)]);
(41, [EatInstr(118,91);EatInstr(117,90);EatInstr(114,89);EatInstr(112,88);EatInstr(111,87);EatInstr(110,86);EatInstr(109,85);EatInstr(108,84);EatInstr(105,83);EatInstr(104,82);EatInstr(99,81);EatInstr(98,80);EatInstr(97,79)]);
(425, [EatInstr(101,468)]);
(42, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,92)]);
(426, [EatInstr(116,469)]);
(43, [CompleteInstr(274)]);
(427, [EatInstr(119,473);EatInstr(116,472);EatInstr(112,471);EatInstr(102,470)]);
(44, [EatInstr(114,93)]);
(428, [EatInstr(101,474)]);
(45, [EatInstr(116,94)]);
(429, [EatInstr(98,475)]);
(46, [EatInstr(111,95)]);
(430, [AAction2Instr(__a36,476)]);
(47, [EatInstr(114,98);EatInstr(112,97);EatInstr(109,96)]);
(431, [EatInstr(114,477)]);
(48, [EatInstr(115,100);EatInstr(112,99)]);
(432, [EatInstr(115,478)]);
(49, [EatInstr(116,101)]);
(433, [EatInstr(101,479)]);
(50, [EatInstr(116,103);EatInstr(101,102)]);
(434, [EatInstr(100,480)]);
(51, [EatInstr(115,104)]);
(435, [EatInstr(104,481)]);
(52, [EatInstr(116,105)]);
(436, [EatInstr(115,482)]);
(53, [EatInstr(115,106)]);
(437, [EatInstr(112,483)]);
(54, [EatInstr(108,108);EatInstr(102,107)]);
(438, [EatInstr(122,484)]);
(55, [EatInstr(120,109)]);
(439, [EatInstr(105,485)]);
(56, [EatInstr(102,110)]);
(440, [EatInstr(121,486)]);
(57, [EatInstr(111,111)]);
(441, [EatInstr(111,487)]);
(58, [EatInstr(49,112)]);
(442, [EatInstr(105,488)]);
(59, [EatInstr(110,113)]);
(443, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,489)]);
(60, [EatInstr(114,115);EatInstr(100,114)]);
(444, [EatInstr(116,490)]);
(61, [EatInstr(105,117);EatInstr(101,116)]);
(445, [EatInstr(116,491)]);
(62, [EatInstr(112,118)]);
(446, [AAction2Instr(__a37,201)]);
(63, [EatInstr(99,119)]);
(447, [AAction2Instr(__a38,201)]);
(64, [EatInstr(114,120)]);
(448, [EatInstr(116,492)]);
(65, [EatInstr(114,121)]);
(449, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,493)]);
(66, [EatInstr(98,122)]);
(450, [EatInstr(114,494)]);
(67, [EatInstr(97,123)]);
(451, [AAction2Instr(__a39,70)]);
(68, [EatInstr(114,124)]);
(452, [EatInstr(45,495)]);
(69, [EatInstr(97,125)]);
(453, [EatInstr(108,496)]);
(70, [CompleteInstr(272)]);
(454, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,497)]);
(71, [ALookaheadInstr(false,CfgLA (1,264),72);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,71)]);
(455, [EatInstr(116,498)]);
(72, [CompleteInstr(269)]);
(456, [EatInstr(115,499)]);
(73, [ALookaheadInstr(false,CfgLA (1,264),74);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,73)]);
(457, [EatInstr(108,500)]);
(74, [CompleteInstr(270)]);
(458, [EatInstr(117,501)]);
(75, [EatInstr(112,97)]);
(459, [EatInstr(97,502)]);
(76, [EatInstr(115,100)]);
(460, [EatInstr(101,503)]);
(77, [EatInstr(108,108);EatInstr(102,128)]);
(461, [AAction2Instr(__a40,70)]);
(78, [EatInstr(101,129)]);
(462, [EatInstr(45,505);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,504)]);
(79, [EatInstr(114,131);EatInstr(102,130)]);
(463, [AAction2Instr(__a41,70)]);
(80, [EatInstr(97,132)]);
(464, [EatInstr(118,506)]);
(81, [EatInstr(111,135);EatInstr(104,134);EatInstr(97,133)]);
(465, [EatInstr(45,507)]);
(82, [EatInstr(121,136)]);
(466, [EatInstr(100,508)]);
(83, [EatInstr(110,137)]);
(467, [EatInstr(114,509)]);
(84, [EatInstr(111,138)]);
(468, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,504)]);
(85, [EatInstr(101,139)]);
(469, [EatInstr(97,510)]);
(86, [EatInstr(111,140)]);
(470, [EatInstr(117,511)]);
(87, [EatInstr(110,141)]);
(471, [EatInstr(101,512)]);
(88, [EatInstr(114,142)]);
(472, [EatInstr(120,513)]);
(89, [EatInstr(111,143)]);
(473, [EatInstr(97,514)]);
(90, [EatInstr(115,145);EatInstr(110,144)]);
(474, [EatInstr(110,515)]);
(91, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,146)]);
(475, [EatInstr(101,516)]);
(92, [AAction2Instr(__a4,147)]);
(476, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,552)]);
(93, [EatInstr(111,148)]);
(477, [EatInstr(114,517)]);
(94, [EatInstr(114,149)]);
(478, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,518)]);
(95, [EatInstr(115,150)]);
(479, [EatInstr(103,519)]);
(96, [EatInstr(112,151)]);
(480, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,520)]);
(97, [EatInstr(121,152)]);
(481, [EatInstr(105,521)]);
(98, [EatInstr(111,153)]);
(482, [EatInstr(99,522)]);
(99, [EatInstr(101,154)]);
(483, [EatInstr(115,523)]);
(100, [EatInstr(117,155)]);
(484, [EatInstr(101,524)]);
(101, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,156)]);
(485, [EatInstr(122,525)]);
(102, [EatInstr(99,157)]);
(486, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,526)]);
(103, [EatInstr(114,158)]);
(487, [EatInstr(112,527)]);
(104, [EatInstr(101,159)]);
(488, [EatInstr(115,528)]);
(105, [EatInstr(45,160)]);
(489, [AAction2Instr(__a42,201)]);
(106, [EatInstr(104,161)]);
(490, [EatInstr(111,529)]);
(107, [EatInstr(111,163);EatInstr(101,162)]);
(491, [EatInstr(97,530)]);
(108, [EatInstr(105,164)]);
(492, [EatInstr(105,531)]);
(109, [EatInstr(101,165)]);
(493, [AAction2Instr(__a43,268)]);
(110, [EatInstr(116,166)]);
(494, [EatInstr(45,532)]);
(111, [EatInstr(107,167)]);
(495, [EatInstr(103,533)]);
(112, [EatInstr(45,168)]);
(496, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,534)]);
(113, [EatInstr(117,169)]);
(497, [AAction2Instr(__a44,366)]);
(114, [EatInstr(115,170)]);
(498, [EatInstr(111,535)]);
(115, [EatInstr(115,171)]);
(499, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,536)]);
(116, [EatInstr(99,172)]);
(500, [EatInstr(97,537)]);
(117, [EatInstr(110,173)]);
(501, [EatInstr(108,538)]);
(118, [EatInstr(108,174)]);
(502, [EatInstr(110,539)]);
(119, [AAction2Instr(__a5,175)]);
(503, [EatInstr(97,540)]);
(120, [EatInstr(116,176)]);
(504, [AAction2Instr(__a45,268)]);
(121, [EatInstr(105,177)]);
(505, [EatInstr(97,541)]);
(122, [EatInstr(115,178)]);
(506, [EatInstr(97,542)]);
(123, [EatInstr(110,179)]);
(507, [EatInstr(97,543)]);
(124, [EatInstr(111,180)]);
(508, [EatInstr(121,544)]);
(125, [EatInstr(112,181)]);
(509, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,545)]);
(126, [AAction2Instr(__a6,182)]);
(510, [EatInstr(116,546)]);
(127, [CompleteInstr(267)]);
(511, [EatInstr(110,547)]);
(128, [EatInstr(101,162)]);
(512, [EatInstr(103,548)]);
(129, [EatInstr(99,183)]);
(513, [AAction2Instr(__a46,652)]);
(130, [EatInstr(116,184)]);
(514, [EatInstr(100,549)]);
(131, [EatInstr(114,185)]);
(515, [EatInstr(115,550)]);
(132, [EatInstr(99,186)]);
(516, [EatInstr(108,551)]);
(133, [EatInstr(115,187)]);
(517, [EatInstr(111,554)]);
(134, [EatInstr(101,188)]);
(518, [AAction2Instr(__a47,201)]);
(135, [EatInstr(117,189)]);
(519, [EatInstr(117,555)]);
(136, [EatInstr(98,190)]);
(520, [AAction2Instr(__a48,201)]);
(137, [EatInstr(108,191)]);
(521, [EatInstr(115,556)]);
(138, [EatInstr(111,192)]);
(522, [EatInstr(101,557)]);
(139, [EatInstr(109,193)]);
(523, [EatInstr(101,558)]);
(140, [EatInstr(45,194)]);
(524, [EatInstr(45,559)]);
(141, [EatInstr(108,195)]);
(525, [EatInstr(101,560)]);
(142, [EatInstr(101,196)]);
(526, [AAction2Instr(__a49,201)]);
(143, [EatInstr(111,197)]);
(527, [EatInstr(116,561)]);
(144, [EatInstr(114,199);EatInstr(105,198)]);
(528, [EatInstr(116,562)]);
(145, [EatInstr(101,200)]);
(529, [EatInstr(114,563)]);
(146, [AAction2Instr(__a7,201)]);
(530, [EatInstr(114,564)]);
(147, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,202)]);
(531, [EatInstr(111,565)]);
(148, [EatInstr(119,203)]);
(532, [EatInstr(99,566)]);
(149, [EatInstr(105,204)]);
(533, [EatInstr(114,567)]);
(150, [EatInstr(101,205)]);
(534, [AAction2Instr(__a50,268)]);
(151, [EatInstr(105,206)]);
(535, [EatInstr(114,568)]);
(152, [EatInstr(114,207)]);
(536, [AAction2Instr(__a51,268)]);
(153, [EatInstr(117,208)]);
(537, [EatInstr(98,569)]);
(154, [EatInstr(110,209)]);
(538, [EatInstr(97,570)]);
(155, [EatInstr(103,210)]);
(539, [EatInstr(97,571)]);
(156, [AAction2Instr(__a8,70)]);
(540, [EatInstr(100,572)]);
(157, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,211)]);
(541, [EatInstr(110,573)]);
(158, [EatInstr(97,212)]);
(542, [EatInstr(110,574)]);
(159, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,213)]);
(543, [EatInstr(99,575)]);
(160, [EatInstr(103,214)]);
(544, [EatInstr(112,576)]);
(161, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,215)]);
(545, [AAction2Instr(__a52,268)]);
(162, [EatInstr(114,216)]);
(546, [EatInstr(105,577)]);
(163, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,217)]);
(547, [AAction2Instr(__a53,652)]);
(164, [EatInstr(110,218)]);
(548, [EatInstr(45,578);AAction2Instr(__a54,652)]);
(165, [EatInstr(114,219)]);
(549, [EatInstr(108,580)]);
(166, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,220)]);
(550, [EatInstr(105,581)]);
(167, [EatInstr(97,221)]);
(551, [EatInstr(115,582)]);
(168, [EatInstr(108,222)]);
(552, [AAction2Instr(__a55,553);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,552)]);
(169, [EatInstr(115,223)]);
(553, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,583)]);
(170, [EatInstr(45,224)]);
(554, [EatInstr(119,584)]);
(171, [EatInstr(101,225)]);
(555, [EatInstr(108,585)]);
(172, [EatInstr(101,226)]);
(556, [EatInstr(116,586)]);
(173, [EatInstr(116,227)]);
(557, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,587)]);
(174, [EatInstr(97,228)]);
(558, [EatInstr(45,588)]);
(175, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,281)]);
(559, [EatInstr(104,589)]);
(176, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,229)]);
(560, [EatInstr(45,590)]);
(177, [EatInstr(112,230)]);
(561, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,591)]);
(178, [EatInstr(101,231)]);
(562, [EatInstr(111,592)]);
(179, [EatInstr(115,232)]);
(563, [EatInstr(121,593)]);
(180, [EatInstr(108,233)]);
(564, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,594)]);
(181, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,234)]);
(565, [EatInstr(110,595)]);
(182, [ALookaheadInstr(false,CfgLA (3,266),127);ACallInstr3(__default_call,11);ACallInstr3(__g0,10);AContInstr3(273,__g0,__binder3,126);ASimpleCont2Instr(274,__binder0,127)]);
(566, [EatInstr(111,596)]);
(183, [EatInstr(101,235)]);
(567, [EatInstr(97,597)]);
(184, [EatInstr(101,236)]);
(568, [EatInstr(115,598)]);
(185, [EatInstr(111,237)]);
(569, [EatInstr(108,599)]);
(186, [EatInstr(107,238)]);
(570, [EatInstr(114,600)]);
(187, [EatInstr(101,239)]);
(571, [EatInstr(108,601)]);
(188, [EatInstr(99,240)]);
(572, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,602)]);
(189, [EatInstr(110,241)]);
(573, [EatInstr(97,603)]);
(190, [EatInstr(114,242)]);
(574, [EatInstr(99,604)]);
(191, [EatInstr(105,243)]);
(575, [EatInstr(116,605)]);
(192, [EatInstr(107,244)]);
(576, [EatInstr(103,606)]);
(193, [EatInstr(111,245)]);
(577, [EatInstr(111,607)]);
(194, [EatInstr(115,250);EatInstr(114,249);EatInstr(111,248);EatInstr(109,247);EatInstr(99,246)]);
(578, [EatInstr(115,608)]);
(195, [EatInstr(121,251)]);
(579, [AAction2Instr(__a56,201)]);
(196, [EatInstr(102,252)]);
(580, [EatInstr(101,609)]);
(197, [EatInstr(116,253)]);
(581, [EatInstr(116,610)]);
(198, [EatInstr(116,254)]);
(582, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,611)]);
(199, [EatInstr(111,255)]);
(583, [AAction2Instr(__a57,201)]);
(200, [EatInstr(45,256)]);
(584, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,612)]);
(201, [CompleteInstr(273)]);
(585, [EatInstr(97,613)]);
(202, [AAction2Instr(__a9,201)]);
(586, [EatInstr(111,614)]);
(203, [EatInstr(45,257)]);
(587, [AAction2Instr(__a58,201)]);
(204, [EatInstr(98,258)]);
(588, [EatInstr(99,615)]);
(205, [EatInstr(45,259)]);
(589, [EatInstr(105,616)]);
(206, [EatInstr(108,260)]);
(590, [EatInstr(112,617)]);
(207, [EatInstr(117,261)]);
(591, [AAction2Instr(__a59,201)]);
(208, [EatInstr(116,262)]);
(592, [EatInstr(114,618)]);
(209, [EatInstr(100,263)]);
(593, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,619)]);
(210, [EatInstr(97,264)]);
(594, [AAction2Instr(__a60,620)]);
(211, [AAction2Instr(__a10,265)]);
(595, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,621)]);
(212, [EatInstr(99,266)]);
(596, [EatInstr(114,622)]);
(213, [AAction2Instr(__a11,70)]);
(597, [EatInstr(112,623)]);
(214, [EatInstr(101,267)]);
(598, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,624)]);
(215, [AAction2Instr(__a12,268)]);
(599, [EatInstr(101,625)]);
(216, [EatInstr(45,269)]);
(600, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,626)]);
(217, [AAction2Instr(__a13,70)]);
(601, [EatInstr(121,627)]);
(218, [EatInstr(101,270)]);
(602, [AAction2Instr(__a61,70)]);
(219, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,271)]);
(603, [EatInstr(108,628)]);
(220, [AAction2Instr(__a14,268)]);
(604, [EatInstr(101,629)]);
(221, [EatInstr(104,272)]);
(605, [EatInstr(105,630)]);
(222, [EatInstr(111,273)]);
(606, [EatInstr(101,631)]);
(223, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,274)]);
(607, [EatInstr(110,632)]);
(224, [EatInstr(108,275)]);
(608, [EatInstr(116,633)]);
(225, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,276)]);
(609, [EatInstr(114,634)]);
(226, [EatInstr(100,277)]);
(610, [EatInstr(105,635)]);
(227, [EatInstr(45,279);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,278)]);
(611, [AAction2Instr(__a62,201)]);
(228, [EatInstr(121,280)]);
(612, [AAction2Instr(__a63,201)]);
(229, [AAction2Instr(__a15,70)]);
(613, [EatInstr(114,636)]);
(230, [EatInstr(45,283)]);
(614, [EatInstr(114,637)]);
(231, [EatInstr(116,284)]);
(615, [EatInstr(97,638)]);
(232, [EatInstr(108,285)]);
(616, [EatInstr(115,639)]);
(233, [EatInstr(108,286)]);
(617, [EatInstr(97,640)]);
(234, [AAction2Instr(__a16,268)]);
(618, [EatInstr(121,641)]);
(235, [EatInstr(100,287)]);
(619, [AAction2Instr(__a64,201)]);
(236, [EatInstr(114,288)]);
(620, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,660)]);
(237, [EatInstr(119,289)]);
(621, [AAction2Instr(__a65,268)]);
(238, [EatInstr(101,290)]);
(622, [EatInstr(101,642)]);
(239, [EatInstr(45,291)]);
(623, [EatInstr(104,643)]);
(240, [EatInstr(107,292)]);
(624, [AAction2Instr(__a66,70)]);
(241, [EatInstr(116,293)]);
(625, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,644)]);
(242, [EatInstr(105,294)]);
(626, [AAction2Instr(__a67,268)]);
(243, [EatInstr(110,295)]);
(627, [EatInstr(115,645)]);
(244, [EatInstr(97,296)]);
(628, [EatInstr(121,646)]);
(245, [EatInstr(105,297)]);
(629, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,647)]);
(246, [EatInstr(111,298)]);
(630, [EatInstr(111,648)]);
(247, [EatInstr(101,299)]);
(631, [EatInstr(110,649)]);
(248, [EatInstr(112,300)]);
(632, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,650)]);
(249, [EatInstr(101,301)]);
(633, [EatInstr(114,651)]);
(250, [EatInstr(107,302)]);
(634, [AAction2Instr(__a68,652)]);
(251, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,303)]);
(635, [EatInstr(118,653)]);
(252, [EatInstr(105,304)]);
(636, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,654)]);
(253, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,305)]);
(637, [EatInstr(121,655)]);
(254, [EatInstr(45,306)]);
(638, [EatInstr(108,656)]);
(255, [EatInstr(108,307)]);
(639, [EatInstr(116,657)]);
(256, [EatInstr(102,308)]);
(640, [EatInstr(109,658)]);
(257, [EatInstr(110,309)]);
(641, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,659)]);
(258, [EatInstr(117,310)]);
(642, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,662)]);
(259, [EatInstr(117,311)]);
(643, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,663)]);
(260, [EatInstr(101,312)]);
(644, [AAction2Instr(__a69,268)]);
(261, [EatInstr(108,313)]);
(645, [EatInstr(105,664)]);
(262, [EatInstr(105,314)]);
(646, [EatInstr(115,665)]);
(263, [EatInstr(101,315)]);
(647, [AAction2Instr(__a70,70)]);
(264, [EatInstr(114,316)]);
(648, [EatInstr(110,666)]);
(265, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,317)]);
(649, [EatInstr(45,668);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,667)]);
(266, [EatInstr(116,318)]);
(650, [AAction2Instr(__a71,201)]);
(267, [EatInstr(110,319)]);
(651, [EatInstr(105,669)]);
(268, [CompleteInstr(271)]);
(652, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,579)]);
(269, [EatInstr(116,321)]);
(653, [EatInstr(101,670)]);
(270, [EatInstr(45,322)]);
(654, [AAction2Instr(__a72,201)]);
(271, [AAction2Instr(__a17,268)]);
(655, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,671)]);
(272, [EatInstr(101,323)]);
(656, [EatInstr(108,672)]);
(273, [EatInstr(111,324)]);
(657, [EatInstr(111,673)]);
(274, [AAction2Instr(__a18,268)]);
(658, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,674)]);
(275, [EatInstr(105,325)]);
(659, [AAction2Instr(__a73,201)]);
(276, [AAction2Instr(__a19,70)]);
(660, [AAction2Instr(__a74,661);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,660)]);
(277, [EatInstr(101,326)]);
(661, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,675)]);
(278, [AAction2Instr(__a20,70)]);
(662, [AAction2Instr(__a75,268)]);
(279, [EatInstr(114,328);EatInstr(103,327)]);
(663, [AAction2Instr(__a76,70)]);
(280, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,329)]);
(664, [EatInstr(115,676)]);
(281, [AAction2Instr(__a21,282);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,281)]);
(665, [EatInstr(105,677)]);
(282, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,330)]);
(666, [EatInstr(115,678)]);
(283, [EatInstr(108,331)]);
(667, [AAction2Instr(__a77,70)]);
(284, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,332)]);
(668, [EatInstr(115,679)]);
(285, [EatInstr(97,333)]);
(669, [EatInstr(99,680)]);
(286, [EatInstr(45,334)]);
(670, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,681)]);
(287, [EatInstr(101,335)]);
(671, [AAction2Instr(__a78,201)]);
(288, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,336)]);
(672, [EatInstr(115,682)]);
(289, [EatInstr(45,337)]);
(673, [EatInstr(114,683)]);
(290, [EatInstr(110,338)]);
(674, [AAction2Instr(__a79,201)]);
(291, [EatInstr(105,339)]);
(675, [AAction2Instr(__a80,201)]);
(292, [EatInstr(45,340)]);
(676, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,684)]);
(293, [EatInstr(101,341)]);
(677, [EatInstr(115,685)]);
(294, [EatInstr(100,342)]);
(678, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,686)]);
(295, [EatInstr(101,343)]);
(679, [EatInstr(99,687)]);
(296, [EatInstr(104,344)]);
(680, [EatInstr(116,688)]);
(297, [EatInstr(122,345)]);
(681, [AAction2Instr(__a81,201)]);
(298, [EatInstr(108,347);EatInstr(97,346)]);
(682, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,689)]);
(299, [EatInstr(109,348)]);
(683, [EatInstr(121,690)]);
(300, [EatInstr(116,349)]);
(684, [AAction2Instr(__a82,70)]);
(301, [EatInstr(112,350)]);
(685, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,691)]);
(302, [EatInstr(105,351)]);
(686, [AAction2Instr(__a83,70)]);
(303, [AAction2Instr(__a22,201)]);
(687, [EatInstr(97,692)]);
(304, [EatInstr(120,352)]);
(688, [AAction2Instr(__a84,652)]);
(305, [AAction2Instr(__a23,353)]);
(689, [AAction2Instr(__a85,201)]);
(306, [EatInstr(104,354)]);
(690, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,693)]);
(307, [EatInstr(108,355)]);
(691, [AAction2Instr(__a86,70)]);
(308, [EatInstr(115,356)]);
(692, [EatInstr(110,694)]);
(309, [EatInstr(111,357)]);
(693, [AAction2Instr(__a87,201)]);
(310, [EatInstr(116,358)]);
(694, [EatInstr(110,695)]);
(311, [EatInstr(110,359)]);
(695, [EatInstr(101,696)]);
(312, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,360)]);
(696, [EatInstr(114,697)]);
(313, [EatInstr(101,361)]);
(697, [EatInstr(108,698)]);
(314, [EatInstr(110,362)]);
(698, [EatInstr(101,699)]);
(315, [EatInstr(110,363)]);
(699, [EatInstr(115,700)]);
(316, [EatInstr(45,365);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,364)]);
(700, [EatInstr(115,701)]);
(317, [AAction2Instr(__a24,366)]);
(701, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,702)]);
(318, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,367)]);
(702, [AAction2Instr(__a88,70)]);
(319, [EatInstr(101,368)]);
(321, [EatInstr(121,369)]);
(322, [EatInstr(114,371);EatInstr(110,370)]);
(323, [EatInstr(97,372)]);
(324, [EatInstr(107,373)]);
(325, [EatInstr(102,374)]);
(326, [EatInstr(110,375)]);
(327, [EatInstr(105,376)]);
(328, [EatInstr(101,377)]);
(329, [AAction2Instr(__a25,268)]);
(330, [AAction2Instr(__a26,70)]);
(331, [EatInstr(97,378)]);
(332, [AAction2Instr(__a27,268)]);
(333, [EatInstr(116,379)]);
(334, [EatInstr(115,380)]);
(335, [EatInstr(110,381)]);
(336, [AContInstr3(271,__g0,__binder4,382);ACallInstr3(__g0,8)]);
(337, [EatInstr(110,383)]);
(338, [EatInstr(100,384)]);
(339, [EatInstr(110,385)]);
(340, [EatInstr(108,386)]);
(341, [EatInstr(114,387)]);
(342, [EatInstr(45,388)]);
(343, [EatInstr(45,389)]);
(344, [EatInstr(101,390)]);
(345, [EatInstr(101,391)]);
(346, [EatInstr(108,392)]);
(347, [EatInstr(108,393)]);
(348, [EatInstr(111,394)]);
(349, [EatInstr(105,395)]);
(350, [EatInstr(108,396)]);
(351, [EatInstr(112,397)]);
(352, [EatInstr(45,398)]);
(353, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,399)]);
(354, [EatInstr(105,400)]);
(355, [EatInstr(45,401)]);
(356, [EatInstr(116,403);EatInstr(109,402)]);
(357, [EatInstr(116,404)]);
(358, [EatInstr(101,405)]);
(359, [EatInstr(100,406)]);
(360, [AAction2Instr(__a28,70)]);
(361, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,407)]);
(362, [EatInstr(101,408)]);
(363, [EatInstr(99,409)]);
(364, [AAction2Instr(__a29,268)]);
(365, [EatInstr(103,410)]);
(366, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,411)]);
(367, [AAction2Instr(__a30,70)]);
(368, [EatInstr(114,412)]);
(369, [EatInstr(112,413)]);
(370, [EatInstr(117,414)]);
(371, [EatInstr(101,415)]);
(372, [EatInstr(100,416)]);
(373, [EatInstr(97,417)]);
(374, [EatInstr(116,418)]);
(375, [EatInstr(99,419)]);
(376, [EatInstr(108,420)]);
(377, [EatInstr(108,421)]);
(378, [EatInstr(116,422)]);
(379, [EatInstr(101,423)]);
(380, [EatInstr(116,424)]);
(381, [EatInstr(99,425)]);
(382, [AAction2Instr(__a31,201)]);
]

let start_symb = get_symb_action "cmd-line-args"

module P2__ = Yak.Engine.Full_yakker (Yak.Engine.Scannerless_term_lang)
                                     (struct type t = sv let cmp = sv_compare type idata = Yk_History.Root_id_set.t
  let create_idata () = Yk_History.Root_id_set.empty
  let inspect h s = Yk_History.add_id_set h#get_root s
  let summarize_inspection s = string_of_int (Yk_History.Root_id_set.cardinal s) end)

let _wfe_data_ = Yak.PamJIT.DNELR.mk_table Yak.PamJIT.Full_opt (Yak.Pam_internal.load_internal_program program)
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
