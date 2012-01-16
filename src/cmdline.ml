
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

module O = Compileopt


(* History value type*)
type hv = int

(* History constructors *)
let _e p h = h#empty p
let _p lbl hv p = (fun h->h#push p (lbl, hv, p))
let _m lbl p = (fun h1 h2-> h1#merge p (lbl,  lbl, p) h2)


module Yk_Hashed = struct
  type t = int * hv * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
  let memoize = true
end
module Yk_History = Yak.History.Make(Yk_Hashed)

(* Replay-related functions *)

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
                                               Inline_regular_cmd -> O.inline_regular := true
                                             | Unroll_star_cmd -> if !O.unroll_star_n<1 then O.unroll_star_n := 1
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
 | (2033) -> ((); ();  O.coalesce := true; Fuse_cmd )
 | (2034) -> ((); ();  Generators_analysis_cmd )
 | (2035) -> ((); ();  Info_cmd )
 | (2036) -> ((); ();  Lookahead_analysis_cmd )
 | (2037) -> ((); ();  Lr1_lookahead_cmd )
 | (2038) -> ((); ();  Parse_cmd )
 | (2039) -> ((); ();  Precedence_analysis_cmd )
 | (2040) -> ((); ();  Print_gul_cmd )
 | (2041) -> ((); ();  Print_gil_cmd )
 | (2042) -> ((); ();  Print_relevance_cmd )
 | (2043) -> ((); (let _x10 = _p() in (); (let _x9 = _p() in (let n = Yak.YkBuf.get_string _x10 _x9 ykinput in ();  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" ))))
 | (2046) -> ((); ();  Sort_cmd )
 | (2047) -> ((); ();  Pads_lift_cmd )
 | (2048) -> ((); ();  Strip_late_actions_cmd )
 | (2049) -> ((); ();  Translate_dypgen_cmd )
 | (2050) -> ((); ();  Translate_dypgen_scannerless_cmd )
 | _ -> raise Exit)
and
 _r_args(_n,_p,ykinput) = (match _n() with
 | (2051) -> ((); (); (let p = _r_phases(_n,_p,ykinput) in  after := Some p ; ()))
 | (2052) -> ((); ();  O.use_wrap_and_attr := false;      
                                            O.use_coroutines    := false ; ())
 | (2053) -> ((); ();  O.use_coroutines := false ; ())
 | (2054) -> ((); (); (let b = (match _n() with
 | (2055) -> ((); Fun_BE)
 | (2056) -> ((); Trans_BE)
 | (2057) -> ((); Wadler_BE)
 | (2058) -> ((); Peg_BE false)
 | (2059) -> ((); Peg_BE true)
 | _ -> raise Exit) in ();  backend := b ; ()))
 | (2060) -> ((); ();  O.case_sensitive := false ; ())
 | (2061) -> ((); ();  O.check_labels := true ; ())
 | (2062) -> ((); (); (let _x12 = _p() in (); (let _x11 = _p() in (let n = Yak.YkBuf.get_string _x12 _x11 ykinput in ();  Variables.counter := (int_of_string n) ; ()))))
 | (2065) -> ((); ();  O.inline_cs := true ; ())
 | (2066) -> ((); ();  O.inline_regular := true ; ())
 | (2067) -> ((); ();  O.lookahead := true ; ())
 | (2068) -> ((); ();  O.memoize_history := true ; ())
 | (2069) -> ((); ();  O.memoize_eps_parsers := true ; ())
 | (2070) -> ((); ();  O.coalesce := false ; ())
 | (2071) -> ((); ();  O.collapse_calls := false ; ())
 | (2072) -> ((); ();  O.memoize_history := false ; ())
 | (2073) -> ((); ();  O.memoize_eps_parsers := false ; ())
 | (2074) -> ((); ();  O.gen_nullpreds := false ; ())
 | (2075) -> ((); ();  O.gen_optimize_pam := false ; ())
 | (2076) -> ((); ();  O.repress_replay := true ; ())
 | (2077) -> ((); ();  O.skip_opt := false ; ())
 | (2078) -> ((); ();  only := true ; ())
 | (2079) -> ((); ();  O.postfix_history := false ; ())
 | (2080) -> ((); (); (let _x14 = _p() in (); (let _x13 = _p() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2083) -> ((); ();  O.unit_history := true ; ())
 | (2084) -> ((); (); (let _x16 = _p() in (); (let _x15 = _p() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  O.unroll_star_n := (int_of_string n) ; ()))))
 | (2087) -> ((); ();  O.earley_ds := O.Flat_eds ; ())
 | (2088) -> ((); ();  O.earley_ds := O.Hierhash_eds ; ())
 | (2089) -> ((); ();  O.earley_ds := O.Hiermap_eds ; ())
 | (2090) -> ((); ();  O.earley_ds := O.Sparse_eds ; ())
 | (2091) -> ((); ();  O.use_fsm := true ; ())
 | (2092) -> ((); ();  O.use_fsm := false ; ())
 | (2093) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2094) -> ((let _x18 = _p() in (); (let _x17 = _p() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
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
 | (2043) -> (();();();push_pos(_p());();push_pos(_p());(); push((2043)))
 | (2046) -> (();();(); push((2046)))
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
 | (2077) -> (();();();(); push((2077)))
 | (2078) -> (();();();(); push((2078)))
 | (2079) -> (();();();(); push((2079)))
 | (2080) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2080)))
 | (2083) -> (();();();(); push((2083)))
 | (2084) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2084)))
 | (2087) -> (();();();(); push((2087)))
 | (2088) -> (();();();(); push((2088)))
 | (2089) -> (();();();(); push((2089)))
 | (2090) -> (();();();(); push((2090)))
 | (2091) -> (();();();(); push((2091)))
 | (2092) -> (();();();(); push((2092)))
 | (2093) -> (();();();(); push((2093)))
 | (2094) -> (();();();();push_pos(_p());();push_pos(_p()); push((2094)))
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


(*LATE PROLOGUE*)
type _pos = int (* input positions *)
let hv_compare = Yk_History.compare
type sv = (int * hv * _pos, Yak.History.label) Yak.History.history
let sv0 = Yk_History.new_history()
let sv_compare = hv_compare
let sv_hash = Yk_History.hash
let __default_call _ _ = sv0;;
let __cc_call _ x = x;;
let __default_ret _ v1 _ = v1;;
let num_symbols = 12

let symbol_table = function
  | 275 -> "cmd-line-args"
  | 273 -> "command"
  | 265 -> "o"
  | 267 -> "CHAR"
  | 268 -> "file"
  | 264 -> "error"
  | 271 -> "DIGIT"
  | 274 -> "args"
  | 266 -> "phases"
  | 272 -> "arg"
  | 270 -> "eof"
  | 269 -> "OCTET"
  | x -> if x < 256 then Yak.Pam_internal.default_symbol_table x else "?unknown?"

let get_symb_action = function
  | "cmd-line-args" -> 275
  | "command" -> 273
  | "o" -> 265
  | "CHAR" -> 267
  | "file" -> 268
  | "error" -> 264
  | "DIGIT" -> 271
  | "args" -> 274
  | "phases" -> 266
  | "arg" -> 272
  | "eof" -> 270
  | "OCTET" -> 269
  | _ -> raise Not_found

let get_symb_start = function
  | 275 -> 11
  | 274 -> 10
  | 273 -> 9
  | 272 -> 8
  | 271 -> 7
  | 270 -> 6
  | 269 -> 5
  | 268 -> 4
  | 267 -> 3
  | 266 -> 2
  | 265 -> 1
  | _ -> raise Not_found

module Pred3 = Yak.Pam_internal.Pred3
let __a74 = (_p 1012 ((2011)));;
let __a5 = (_p 1002 ((2001)));;
let __a72 = (_p 1062 ((2088)));;
let __a15 = (_p 1037 ((2035)));;
let __a27 = (_p 1007 ((2016)));;
let __a95 = (_p 1078 ((2072)));;
let __a11 = (_p 1054 ((2094)));;
let __a51 = (_p 1014 ((2009)));;
let __a31 = (_p 1040 ((2032)));;
let __a89 = (_p 1090 ((2060)));;
let __a12 = (_p 1010 ((2013)));;
let __a63 = (_p 1035 ((2037)));;
let __a47 = (_p 1085 ((2065)));;
let __a14 = (_p 1005 ((2018)));;
let __a6 = (_p 1055 ((2096)));;
let __a42 = (_p 1031 ((2041)));;
let __a96 = (_p 1076 ((2074)));;
let __a59 = (_p 1086 ((2062)));;
let __a55 = (_p 1096 ((2055)));;
let __a17 = (_p 1026 ((2046)));;
let __a29 = (fun _x0_ _x1_ -> (((_p 1045 ((2029))) _x0_) (((_p 1046 ((2027))) _x0_) _x1_)));;
let __a97 = (_p 1022 ((2050)));;
let __a65 = (_p 1067 ((2083)));;
let __a28 = (_p 1027 ((2043)));;
let __a56 = (_p 1087 ((2064)));;
let __a94 = (_p 1033 ((2039)));;
let __a49 = (_p 1083 ((2067)));;
let __a38 = (_p 1058 ((2092)));;
let __a43 = (_p 1068 ((2080)));;
let __a21 = (_p 1028 ((2045)));;
let __a50 = (_p 1074 ((2076)));;
let __a36 = (_p 1044 ((2030)));;
let __a71 = (_p 1094 ((2057)));;
let __a90 = (_p 1024 ((2048)));;
let __a85 = (_p 1049 ((2023)));;
let __a33 = (_p 1099 ((2051)));;
let __a35 = (_p 1019 ((2004)));;
let __a34 = (_p 1069 ((2082)));;
let __a62 = (_p 1081 ((2069)));;
let __a25 = (_p 1072 ((2078)));;
let __a4 = (_p 1052 ((2020)));;
let __a54 = (_p 1092 ((2059)));;
let __a67 = (_p 1097 ((2053)));;
let __a53 = (_p 1017 ((2006)));;
let __p0 = fun _ v -> v;;
let __a64 = (_p 1063 ((2087)));;
let __a69 = (_p 1013 ((2010)));;
let __a70 = (_p 1038 ((2034)));;
let __a45 = (_p 1008 ((2015)));;
let __a86 = (_p 1064 ((2084)));;
let __a93 = (_p 1079 ((2071)));;
let __a9 = (_p 1000 ((2000)));;
let __a41 = (_p 1050 ((2022)));;
let __a82 = (_p 1020 ((2003)));;
let __a24 = (_p 1070 ((2081)));;
let __a68 = (_p 1015 ((2008)));;
let __a79 = (_p 1065 ((2086)));;
let __a73 = (_p 1061 ((2089)));;
let __a37 = (_p 1041 ((2025)));;
let __a19 = (_p 1011 ((2012)));;
let __a46 = (fun _x0_ _x1_ -> (((_p 1042 ((2028))) _x0_) (((_p 1043 ((2031))) _x0_) _x1_)));;
let __a91 = (_p 1036 ((2036)));;
let __a52 = (_p 1006 ((2017)));;
let __a3 = (_p 1056 ((2095)));;
let __a23 = (_p 1032 ((2040)));;
let __a16 = (_p 1047 ((2026)));;
let __a80 = (_p 1077 ((2073)));;
let __g2 = (_e);;
let __p1 = fun _ _ -> false;;
let __a40 = (_p 1088 ((2063)));;
let __a22 = (_p 1034 ((2038)));;
let __a77 = (_p 1084 ((2066)));;
let __a26 = (_p 1004 ((2019)));;
let __a7 = (_p 1029 ((2044)));;
let __a39 = (_p 1059 ((2091)));;
let __a75 = (_p 1030 ((2042)));;
let __a61 = (_p 1080 ((2070)));;
let __a48 = (_p 1095 ((2056)));;
let __a83 = (_p 1025 ((2047)));;
let __a87 = (_p 1075 ((2075)));;
let __a57 = (_p 1091 ((2054)));;
let __a88 = (_p 1082 ((2068)));;
let __a8 = (_p 1057 ((2093)));;
let __a92 = (_p 1093 ((2058)));;
let __a84 = (_p 1023 ((2049)));;
let __a60 = (_p 1073 ((2077)));;
let __a10 = (_p 1048 ((2024)));;
let __a76 = (_p 1098 ((2052)));;
let __a30 = (_p 1018 ((2005)));;
let __a18 = (_p 1039 ((2033)));;
let __a66 = (_p 1089 ((2061)));;
let __a20 = (_p 1009 ((2014)));;
let __a78 = (_p 1060 ((2090)));;
let __a44 = (_p 1021 ((2002)));;
let __a81 = (_p 1071 ((2079)));;
let __a32 = (_p 1051 ((2021)));;
let __a13 = (_p 1016 ((2007)));;
let __a58 = (_p 1066 ((2085)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1053);;
let __binder2 = (_m 1003);;
let __binder3 = (_m 1001);;
let __binder4 = (_m 1100);;
open Yak.Pam_internal
let program = [
(767, [EatInstr(108,768)]);
(0, [ASimpleCont2Instr(275,__binder0,11);ASimpleCont2Instr(274,__binder0,10);ASimpleCont2Instr(273,__binder0,9);ASimpleCont2Instr(272,__binder0,8);ASimpleCont2Instr(271,__binder0,7);ASimpleCont2Instr(270,__binder0,6);ASimpleCont2Instr(269,__binder0,5);ASimpleCont2Instr(268,__binder0,4);ASimpleCont2Instr(267,__binder0,3);ASimpleCont2Instr(266,__binder0,2);ASimpleCont2Instr(265,__binder0,1)]);
(768, [EatInstr(101,769)]);
(1, [EatInstr(0,12)]);
(769, [EatInstr(115,770)]);
(2, [EatInstr(109,24);EatInstr(119,23);EatInstr(104,22);EatInstr(112,21);EatInstr(100,20);EatInstr(108,19);EatInstr(99,18);EatInstr(115,17);EatInstr(117,16);EatInstr(105,15);EatInstr(114,14);EatInstr(97,13)]);
(770, [EatInstr(115,771)]);
(3, [EatInstr(127,25);EatInstr(126,25);EatInstr(125,25);EatInstr(124,25);EatInstr(123,25);EatInstr(122,25);EatInstr(118,25);EatInstr(113,25);EatInstr(107,25);EatInstr(106,25);EatInstr(96,25);EatInstr(95,25);EatInstr(94,25);EatInstr(93,25);EatInstr(92,25);EatInstr(91,25);EatInstr(90,25);EatInstr(89,25);EatInstr(88,25);EatInstr(87,25);EatInstr(86,25);EatInstr(85,25);EatInstr(84,25);EatInstr(83,25);EatInstr(82,25);EatInstr(81,25);EatInstr(80,25);EatInstr(79,25);EatInstr(78,25);EatInstr(77,25);EatInstr(76,25);EatInstr(75,25);EatInstr(74,25);EatInstr(73,25);EatInstr(72,25);EatInstr(71,25);EatInstr(70,25);EatInstr(69,25);EatInstr(68,25);EatInstr(67,25);EatInstr(66,25);EatInstr(65,25);EatInstr(64,25);EatInstr(63,25);EatInstr(62,25);EatInstr(61,25);EatInstr(60,25);EatInstr(59,25);EatInstr(58,25);EatInstr(57,25);EatInstr(56,25);EatInstr(55,25);EatInstr(54,25);EatInstr(53,25);EatInstr(52,25);EatInstr(51,25);EatInstr(50,25);EatInstr(49,25);EatInstr(48,25);EatInstr(47,25);EatInstr(46,25);EatInstr(44,25);EatInstr(43,25);EatInstr(42,25);EatInstr(41,25);EatInstr(40,25);EatInstr(39,25);EatInstr(38,25);EatInstr(37,25);EatInstr(36,25);EatInstr(35,25);EatInstr(34,25);EatInstr(33,25);EatInstr(32,25);EatInstr(31,25);EatInstr(30,25);EatInstr(29,25);EatInstr(28,25);EatInstr(27,25);EatInstr(26,25);EatInstr(25,25);EatInstr(24,25);EatInstr(23,25);EatInstr(22,25);EatInstr(21,25);EatInstr(20,25);EatInstr(19,25);EatInstr(18,25);EatInstr(17,25);EatInstr(16,25);EatInstr(15,25);EatInstr(14,25);EatInstr(13,25);EatInstr(12,25);EatInstr(11,25);EatInstr(10,25);EatInstr(9,25);EatInstr(8,25);EatInstr(7,25);EatInstr(6,25);EatInstr(5,25);EatInstr(4,25);EatInstr(3,25);EatInstr(2,25);EatInstr(1,25);EatInstr(109,25);EatInstr(120,25);EatInstr(102,25);EatInstr(119,25);EatInstr(104,25);EatInstr(103,25);EatInstr(121,25);EatInstr(112,25);EatInstr(100,25);EatInstr(110,25);EatInstr(45,25);EatInstr(111,25);EatInstr(108,25);EatInstr(99,25);EatInstr(115,25);EatInstr(101,25);EatInstr(117,25);EatInstr(98,25);EatInstr(105,25);EatInstr(114,25);EatInstr(116,25);EatInstr(97,25)]);
(771, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,772)]);
(4, [EatInstr(127,60);EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(122,60);EatInstr(118,60);EatInstr(113,60);EatInstr(107,60);EatInstr(106,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(49,60);EatInstr(48,60);EatInstr(47,60);EatInstr(46,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(32,60);EatInstr(31,60);EatInstr(30,60);EatInstr(29,60);EatInstr(28,60);EatInstr(27,60);EatInstr(26,60);EatInstr(25,60);EatInstr(24,60);EatInstr(23,60);EatInstr(22,60);EatInstr(21,60);EatInstr(20,60);EatInstr(19,60);EatInstr(18,60);EatInstr(17,60);EatInstr(16,60);EatInstr(15,60);EatInstr(14,60);EatInstr(13,60);EatInstr(12,60);EatInstr(11,60);EatInstr(10,60);EatInstr(9,60);EatInstr(8,60);EatInstr(7,60);EatInstr(6,60);EatInstr(5,60);EatInstr(4,60);EatInstr(3,60);EatInstr(2,60);EatInstr(1,60);EatInstr(109,60);EatInstr(120,60);EatInstr(102,60);EatInstr(119,60);EatInstr(104,60);EatInstr(103,60);EatInstr(121,60);EatInstr(112,60);EatInstr(100,60);EatInstr(110,60);EatInstr(111,60);EatInstr(108,60);EatInstr(99,60);EatInstr(115,60);EatInstr(101,60);EatInstr(117,60);EatInstr(98,60);EatInstr(105,60);EatInstr(114,60);EatInstr(116,60);EatInstr(97,60)]);
(772, [AAction2Instr(__a97,80)]);
(5, [EatInstr(255,26);EatInstr(254,26);EatInstr(253,26);EatInstr(252,26);EatInstr(251,26);EatInstr(250,26);EatInstr(249,26);EatInstr(248,26);EatInstr(247,26);EatInstr(246,26);EatInstr(245,26);EatInstr(244,26);EatInstr(243,26);EatInstr(242,26);EatInstr(241,26);EatInstr(240,26);EatInstr(239,26);EatInstr(238,26);EatInstr(237,26);EatInstr(236,26);EatInstr(235,26);EatInstr(234,26);EatInstr(233,26);EatInstr(232,26);EatInstr(231,26);EatInstr(230,26);EatInstr(229,26);EatInstr(228,26);EatInstr(227,26);EatInstr(226,26);EatInstr(225,26);EatInstr(224,26);EatInstr(223,26);EatInstr(222,26);EatInstr(221,26);EatInstr(220,26);EatInstr(219,26);EatInstr(218,26);EatInstr(217,26);EatInstr(216,26);EatInstr(215,26);EatInstr(214,26);EatInstr(213,26);EatInstr(212,26);EatInstr(211,26);EatInstr(210,26);EatInstr(209,26);EatInstr(208,26);EatInstr(207,26);EatInstr(206,26);EatInstr(205,26);EatInstr(204,26);EatInstr(203,26);EatInstr(202,26);EatInstr(201,26);EatInstr(200,26);EatInstr(199,26);EatInstr(198,26);EatInstr(197,26);EatInstr(196,26);EatInstr(195,26);EatInstr(194,26);EatInstr(193,26);EatInstr(192,26);EatInstr(191,26);EatInstr(190,26);EatInstr(189,26);EatInstr(188,26);EatInstr(187,26);EatInstr(186,26);EatInstr(185,26);EatInstr(184,26);EatInstr(183,26);EatInstr(182,26);EatInstr(181,26);EatInstr(180,26);EatInstr(179,26);EatInstr(178,26);EatInstr(177,26);EatInstr(176,26);EatInstr(175,26);EatInstr(174,26);EatInstr(173,26);EatInstr(172,26);EatInstr(171,26);EatInstr(170,26);EatInstr(169,26);EatInstr(168,26);EatInstr(167,26);EatInstr(166,26);EatInstr(165,26);EatInstr(164,26);EatInstr(163,26);EatInstr(162,26);EatInstr(161,26);EatInstr(160,26);EatInstr(159,26);EatInstr(158,26);EatInstr(157,26);EatInstr(156,26);EatInstr(155,26);EatInstr(154,26);EatInstr(153,26);EatInstr(152,26);EatInstr(151,26);EatInstr(150,26);EatInstr(149,26);EatInstr(148,26);EatInstr(147,26);EatInstr(146,26);EatInstr(145,26);EatInstr(144,26);EatInstr(143,26);EatInstr(142,26);EatInstr(141,26);EatInstr(140,26);EatInstr(139,26);EatInstr(138,26);EatInstr(137,26);EatInstr(136,26);EatInstr(135,26);EatInstr(134,26);EatInstr(133,26);EatInstr(132,26);EatInstr(131,26);EatInstr(130,26);EatInstr(129,26);EatInstr(128,26);EatInstr(127,26);EatInstr(126,26);EatInstr(125,26);EatInstr(124,26);EatInstr(123,26);EatInstr(122,26);EatInstr(118,26);EatInstr(113,26);EatInstr(107,26);EatInstr(106,26);EatInstr(96,26);EatInstr(95,26);EatInstr(94,26);EatInstr(93,26);EatInstr(92,26);EatInstr(91,26);EatInstr(90,26);EatInstr(89,26);EatInstr(88,26);EatInstr(87,26);EatInstr(86,26);EatInstr(85,26);EatInstr(84,26);EatInstr(83,26);EatInstr(82,26);EatInstr(81,26);EatInstr(80,26);EatInstr(79,26);EatInstr(78,26);EatInstr(77,26);EatInstr(76,26);EatInstr(75,26);EatInstr(74,26);EatInstr(73,26);EatInstr(72,26);EatInstr(71,26);EatInstr(70,26);EatInstr(69,26);EatInstr(68,26);EatInstr(67,26);EatInstr(66,26);EatInstr(65,26);EatInstr(64,26);EatInstr(63,26);EatInstr(62,26);EatInstr(61,26);EatInstr(60,26);EatInstr(59,26);EatInstr(58,26);EatInstr(57,26);EatInstr(56,26);EatInstr(55,26);EatInstr(54,26);EatInstr(53,26);EatInstr(52,26);EatInstr(51,26);EatInstr(50,26);EatInstr(49,26);EatInstr(48,26);EatInstr(47,26);EatInstr(46,26);EatInstr(44,26);EatInstr(43,26);EatInstr(42,26);EatInstr(41,26);EatInstr(40,26);EatInstr(39,26);EatInstr(38,26);EatInstr(37,26);EatInstr(36,26);EatInstr(35,26);EatInstr(34,26);EatInstr(33,26);EatInstr(32,26);EatInstr(31,26);EatInstr(30,26);EatInstr(29,26);EatInstr(28,26);EatInstr(27,26);EatInstr(26,26);EatInstr(25,26);EatInstr(24,26);EatInstr(23,26);EatInstr(22,26);EatInstr(21,26);EatInstr(20,26);EatInstr(19,26);EatInstr(18,26);EatInstr(17,26);EatInstr(16,26);EatInstr(15,26);EatInstr(14,26);EatInstr(13,26);EatInstr(12,26);EatInstr(11,26);EatInstr(10,26);EatInstr(9,26);EatInstr(8,26);EatInstr(7,26);EatInstr(6,26);EatInstr(5,26);EatInstr(4,26);EatInstr(3,26);EatInstr(2,26);EatInstr(1,26);EatInstr(109,26);EatInstr(120,26);EatInstr(102,26);EatInstr(119,26);EatInstr(104,26);EatInstr(103,26);EatInstr(121,26);EatInstr(112,26);EatInstr(100,26);EatInstr(110,26);EatInstr(45,26);EatInstr(111,26);EatInstr(108,26);EatInstr(99,26);EatInstr(115,26);EatInstr(101,26);EatInstr(117,26);EatInstr(98,26);EatInstr(105,26);EatInstr(114,26);EatInstr(116,26);EatInstr(97,26);EatInstr(0,26)]);
(6, [AWhenInstr3(__p1,__p0,27)]);
(7, [EatInstr(57,28);EatInstr(56,28);EatInstr(55,28);EatInstr(54,28);EatInstr(53,28);EatInstr(52,28);EatInstr(51,28);EatInstr(50,28);EatInstr(49,28);EatInstr(48,28)]);
(8, [EatInstr(127,25);EatInstr(126,25);EatInstr(125,25);EatInstr(124,25);EatInstr(123,25);EatInstr(122,25);EatInstr(118,25);EatInstr(113,25);EatInstr(107,25);EatInstr(106,25);EatInstr(96,25);EatInstr(95,25);EatInstr(94,25);EatInstr(93,25);EatInstr(92,25);EatInstr(91,25);EatInstr(90,25);EatInstr(89,25);EatInstr(88,25);EatInstr(87,25);EatInstr(86,25);EatInstr(85,25);EatInstr(84,25);EatInstr(83,25);EatInstr(82,25);EatInstr(81,25);EatInstr(80,25);EatInstr(79,25);EatInstr(78,25);EatInstr(77,25);EatInstr(76,25);EatInstr(75,25);EatInstr(74,25);EatInstr(73,25);EatInstr(72,25);EatInstr(71,25);EatInstr(70,25);EatInstr(69,25);EatInstr(68,25);EatInstr(67,25);EatInstr(66,25);EatInstr(65,25);EatInstr(64,25);EatInstr(63,25);EatInstr(62,25);EatInstr(61,25);EatInstr(60,25);EatInstr(59,25);EatInstr(58,25);EatInstr(57,25);EatInstr(56,25);EatInstr(55,25);EatInstr(54,25);EatInstr(53,25);EatInstr(52,25);EatInstr(51,25);EatInstr(50,25);EatInstr(49,25);EatInstr(48,25);EatInstr(47,25);EatInstr(46,25);EatInstr(44,25);EatInstr(43,25);EatInstr(42,25);EatInstr(41,25);EatInstr(40,25);EatInstr(39,25);EatInstr(38,25);EatInstr(37,25);EatInstr(36,25);EatInstr(35,25);EatInstr(34,25);EatInstr(33,25);EatInstr(32,25);EatInstr(31,25);EatInstr(30,25);EatInstr(29,25);EatInstr(28,25);EatInstr(27,25);EatInstr(26,25);EatInstr(25,25);EatInstr(24,25);EatInstr(23,25);EatInstr(22,25);EatInstr(21,25);EatInstr(20,25);EatInstr(19,25);EatInstr(18,25);EatInstr(17,25);EatInstr(16,25);EatInstr(15,25);EatInstr(14,25);EatInstr(13,25);EatInstr(12,25);EatInstr(11,25);EatInstr(10,25);EatInstr(9,25);EatInstr(8,25);EatInstr(7,25);EatInstr(6,25);EatInstr(5,25);EatInstr(4,25);EatInstr(3,25);EatInstr(2,25);EatInstr(1,25);EatInstr(109,25);EatInstr(120,25);EatInstr(102,25);EatInstr(119,25);EatInstr(104,25);EatInstr(103,25);EatInstr(121,25);EatInstr(112,25);EatInstr(100,25);EatInstr(110,25);EatInstr(45,25);EatInstr(111,25);EatInstr(108,25);EatInstr(99,25);EatInstr(115,25);EatInstr(101,25);EatInstr(117,25);EatInstr(98,25);EatInstr(105,25);EatInstr(114,25);EatInstr(116,25);EatInstr(97,25);ASimpleCont2Instr(267,__binder0,62)]);
(9, [EatInstr(109,24);EatInstr(102,40);EatInstr(119,23);EatInstr(104,22);EatInstr(103,39);EatInstr(112,38);EatInstr(100,37);EatInstr(108,36);EatInstr(99,35);EatInstr(115,34);EatInstr(101,33);EatInstr(117,16);EatInstr(105,32);EatInstr(114,31);EatInstr(116,30);EatInstr(97,29);AContInstr3(266,__g2,__binder1,41)]);
(10, [EatInstr(45,42);AAction2Instr(__a3,43)]);
(11, [EatInstr(109,24);EatInstr(102,40);EatInstr(119,23);EatInstr(104,22);EatInstr(103,39);EatInstr(112,38);EatInstr(100,37);EatInstr(108,36);EatInstr(99,35);EatInstr(115,34);EatInstr(101,33);EatInstr(117,16);EatInstr(105,32);EatInstr(114,31);EatInstr(116,30);EatInstr(97,29);AContInstr3(273,__g2,__binder2,44);AContInstr3(266,__g2,__binder1,41)]);
(12, [CompleteInstr(265)]);
(13, [EatInstr(114,46);EatInstr(116,45)]);
(14, [EatInstr(101,47)]);
(15, [EatInstr(110,48)]);
(16, [EatInstr(110,49)]);
(17, [EatInstr(117,50)]);
(18, [EatInstr(111,52);EatInstr(108,51)]);
(19, [EatInstr(101,54);EatInstr(105,53)]);
(20, [EatInstr(101,55)]);
(21, [EatInstr(114,56)]);
(22, [EatInstr(97,57)]);
(23, [EatInstr(114,58)]);
(24, [EatInstr(105,59)]);
(25, [CompleteInstr(267)]);
(26, [CompleteInstr(269)]);
(27, [CompleteInstr(270)]);
(28, [CompleteInstr(271)]);
(29, [EatInstr(100,64);EatInstr(114,46);EatInstr(116,45)]);
(30, [EatInstr(114,65)]);
(31, [EatInstr(102,66);EatInstr(101,47)]);
(32, [EatInstr(110,67)]);
(33, [EatInstr(120,68)]);
(34, [EatInstr(111,70);EatInstr(117,50);EatInstr(116,69)]);
(35, [EatInstr(111,71);EatInstr(108,51)]);
(36, [EatInstr(111,73);EatInstr(101,54);EatInstr(105,53);EatInstr(114,72)]);
(37, [EatInstr(111,75);EatInstr(101,74)]);
(38, [EatInstr(114,77);EatInstr(97,76)]);
(39, [EatInstr(101,78)]);
(40, [EatInstr(117,79)]);
(41, [AAction2Instr(__a4,80)]);
(42, [EatInstr(118,93);EatInstr(109,92);EatInstr(104,91);EatInstr(112,90);EatInstr(110,89);EatInstr(111,88);EatInstr(108,87);EatInstr(99,86);EatInstr(117,85);EatInstr(98,84);EatInstr(105,83);EatInstr(114,82);EatInstr(97,81)]);
(43, [ACallInstr3(__default_call,4);ASimpleCont2Instr(268,__binder0,94)]);
(44, [AAction2Instr(__a5,203)]);
(45, [EatInstr(116,95)]);
(46, [EatInstr(114,96)]);
(47, [EatInstr(112,97)]);
(48, [EatInstr(102,99);EatInstr(108,98)]);
(49, [EatInstr(114,100)]);
(50, [EatInstr(98,101)]);
(51, [EatInstr(111,102)]);
(52, [EatInstr(112,103)]);
(53, [EatInstr(102,104)]);
(54, [EatInstr(120,105)]);
(55, [EatInstr(115,106)]);
(56, [EatInstr(101,107)]);
(57, [EatInstr(115,108)]);
(58, [EatInstr(97,109)]);
(59, [EatInstr(110,110)]);
(60, [ALookaheadInstr(false,CfgLA (3,267),61);ACallInstr3(__default_call,3);ASimpleCont2Instr(267,__binder0,60)]);
(61, [CompleteInstr(268)]);
(62, [ALookaheadInstr(false,CfgLA (3,267),63);ACallInstr3(__default_call,3);ASimpleCont2Instr(267,__binder0,62)]);
(63, [CompleteInstr(272)]);
(64, [EatInstr(100,111)]);
(65, [EatInstr(97,112)]);
(66, [EatInstr(99,113)]);
(67, [EatInstr(102,114);EatInstr(108,98)]);
(68, [EatInstr(101,116);EatInstr(116,115)]);
(69, [EatInstr(114,117)]);
(70, [EatInstr(114,118)]);
(71, [EatInstr(109,120);EatInstr(112,103);EatInstr(114,119)]);
(72, [EatInstr(49,121)]);
(73, [EatInstr(111,122)]);
(74, [EatInstr(112,123);EatInstr(115,106)]);
(75, [EatInstr(116,124)]);
(76, [EatInstr(114,125)]);
(77, [EatInstr(101,127);EatInstr(105,126)]);
(78, [EatInstr(116,128)]);
(79, [EatInstr(115,129)]);
(80, [CompleteInstr(273)]);
(81, [EatInstr(102,131);EatInstr(114,130)]);
(82, [EatInstr(111,132)]);
(83, [EatInstr(110,133)]);
(84, [EatInstr(97,134)]);
(85, [EatInstr(110,136);EatInstr(115,135)]);
(86, [EatInstr(104,139);EatInstr(111,138);EatInstr(97,137)]);
(87, [EatInstr(111,140)]);
(88, [EatInstr(110,141)]);
(89, [EatInstr(111,142)]);
(90, [EatInstr(114,143)]);
(91, [EatInstr(121,144)]);
(92, [EatInstr(101,145)]);
(93, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,146)]);
(94, [AAction2Instr(__a6,147)]);
(95, [EatInstr(114,150)]);
(96, [EatInstr(111,151)]);
(97, [EatInstr(108,152)]);
(98, [EatInstr(105,153)]);
(99, [EatInstr(101,154)]);
(100, [EatInstr(111,155)]);
(101, [EatInstr(115,156)]);
(102, [EatInstr(115,157)]);
(103, [EatInstr(121,158)]);
(104, [EatInstr(116,159)]);
(105, [EatInstr(101,160)]);
(106, [EatInstr(117,161)]);
(107, [EatInstr(99,162)]);
(108, [EatInstr(104,163)]);
(109, [EatInstr(112,164)]);
(110, [EatInstr(117,165)]);
(111, [EatInstr(45,166)]);
(112, [EatInstr(110,167)]);
(113, [AAction2Instr(__a7,168)]);
(114, [EatInstr(111,169);EatInstr(101,154)]);
(115, [EatInstr(114,170)]);
(116, [EatInstr(99,171)]);
(117, [EatInstr(105,172)]);
(118, [EatInstr(116,173)]);
(119, [EatInstr(111,174)]);
(120, [EatInstr(112,175)]);
(121, [EatInstr(45,176)]);
(122, [EatInstr(107,177)]);
(123, [EatInstr(101,178)]);
(124, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,179)]);
(125, [EatInstr(115,180)]);
(126, [EatInstr(110,181)]);
(127, [EatInstr(99,182)]);
(128, [EatInstr(45,183)]);
(129, [EatInstr(101,184)]);
(130, [EatInstr(114,185)]);
(131, [EatInstr(116,186)]);
(132, [EatInstr(111,187)]);
(133, [EatInstr(108,188)]);
(134, [EatInstr(99,189)]);
(135, [EatInstr(101,190)]);
(136, [EatInstr(105,192);EatInstr(114,191)]);
(137, [EatInstr(115,193)]);
(138, [EatInstr(117,194)]);
(139, [EatInstr(101,195)]);
(140, [EatInstr(111,196)]);
(141, [EatInstr(108,197)]);
(142, [EatInstr(45,198)]);
(143, [EatInstr(101,199)]);
(144, [EatInstr(98,200)]);
(145, [EatInstr(109,201)]);
(146, [AAction2Instr(__a8,692)]);
(147, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,202)]);
(148, [AAction2Instr(__a9,203)]);
(149, [CompleteInstr(275)]);
(150, [EatInstr(105,204)]);
(151, [EatInstr(119,205)]);
(152, [EatInstr(97,206)]);
(153, [EatInstr(110,207)]);
(154, [EatInstr(114,208)]);
(155, [EatInstr(108,209)]);
(156, [EatInstr(101,210)]);
(157, [EatInstr(101,211)]);
(158, [EatInstr(114,212)]);
(159, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,213)]);
(160, [EatInstr(114,214)]);
(161, [EatInstr(103,215)]);
(162, [EatInstr(101,216)]);
(163, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,217)]);
(164, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,218)]);
(165, [EatInstr(115,219)]);
(166, [EatInstr(108,220)]);
(167, [EatInstr(115,221)]);
(168, [ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,275)]);
(169, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,222)]);
(170, [EatInstr(97,223)]);
(171, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,224)]);
(172, [EatInstr(112,225)]);
(173, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,226)]);
(174, [EatInstr(117,227)]);
(175, [EatInstr(105,228)]);
(176, [EatInstr(108,229)]);
(177, [EatInstr(97,230)]);
(178, [EatInstr(110,231)]);
(179, [AAction2Instr(__a10,80)]);
(180, [EatInstr(101,232)]);
(181, [EatInstr(116,233)]);
(182, [EatInstr(101,234)]);
(183, [EatInstr(103,235)]);
(184, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,236)]);
(185, [EatInstr(111,237)]);
(186, [EatInstr(101,238)]);
(187, [EatInstr(116,239)]);
(188, [EatInstr(105,240)]);
(189, [EatInstr(107,241)]);
(190, [EatInstr(45,242)]);
(191, [EatInstr(111,243)]);
(192, [EatInstr(116,244)]);
(193, [EatInstr(101,245)]);
(194, [EatInstr(110,246)]);
(195, [EatInstr(99,247)]);
(196, [EatInstr(107,248)]);
(197, [EatInstr(121,249)]);
(198, [EatInstr(109,255);EatInstr(110,254);EatInstr(111,253);EatInstr(99,252);EatInstr(115,251);EatInstr(114,250)]);
(199, [EatInstr(102,256)]);
(200, [EatInstr(114,257)]);
(201, [EatInstr(111,258)]);
(202, [AAction2Instr(__a11,692)]);
(203, [ALookaheadInstr(false,CfgLA (5,269),149);AContInstr3(274,__g2,__binder3,148);ACallInstr3(__g2,10);ACallInstr3(__default_call,6);ASimpleCont2Instr(270,__binder0,149)]);
(204, [EatInstr(98,259)]);
(205, [EatInstr(45,260)]);
(206, [EatInstr(121,261)]);
(207, [EatInstr(101,262)]);
(208, [EatInstr(45,263)]);
(209, [EatInstr(108,264)]);
(210, [EatInstr(116,265)]);
(211, [EatInstr(45,266)]);
(212, [EatInstr(117,267)]);
(213, [AAction2Instr(__a12,268)]);
(214, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,269)]);
(215, [EatInstr(97,270)]);
(216, [EatInstr(100,271)]);
(217, [AAction2Instr(__a13,268)]);
(218, [AAction2Instr(__a14,268)]);
(219, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,272)]);
(220, [EatInstr(97,273)]);
(221, [EatInstr(108,274)]);
(222, [AAction2Instr(__a15,80)]);
(223, [EatInstr(99,277)]);
(224, [AAction2Instr(__a16,278)]);
(225, [EatInstr(45,279)]);
(226, [AAction2Instr(__a17,80)]);
(227, [EatInstr(116,280)]);
(228, [EatInstr(108,281)]);
(229, [EatInstr(111,282)]);
(230, [EatInstr(104,283)]);
(231, [EatInstr(100,284)]);
(232, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,285)]);
(233, [EatInstr(45,287);ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,286)]);
(234, [EatInstr(100,288)]);
(235, [EatInstr(101,289)]);
(236, [AAction2Instr(__a18,80)]);
(237, [EatInstr(119,290)]);
(238, [EatInstr(114,291)]);
(239, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,292)]);
(240, [EatInstr(110,293)]);
(241, [EatInstr(101,294)]);
(242, [EatInstr(102,297);EatInstr(104,296);EatInstr(115,295)]);
(243, [EatInstr(108,298)]);
(244, [EatInstr(45,299)]);
(245, [EatInstr(45,300)]);
(246, [EatInstr(116,301)]);
(247, [EatInstr(107,302)]);
(248, [EatInstr(97,303)]);
(249, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,304)]);
(250, [EatInstr(101,305)]);
(251, [EatInstr(107,306)]);
(252, [EatInstr(111,307)]);
(253, [EatInstr(112,308)]);
(254, [EatInstr(117,309)]);
(255, [EatInstr(101,310)]);
(256, [EatInstr(105,311)]);
(257, [EatInstr(105,312)]);
(258, [EatInstr(105,313)]);
(259, [EatInstr(117,314)]);
(260, [EatInstr(110,315)]);
(261, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,316)]);
(262, [EatInstr(45,317)]);
(263, [EatInstr(116,318)]);
(264, [EatInstr(45,319)]);
(265, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,320)]);
(266, [EatInstr(117,321)]);
(267, [EatInstr(108,322)]);
(268, [CompleteInstr(266)]);
(269, [AAction2Instr(__a19,268)]);
(270, [EatInstr(114,324)]);
(271, [EatInstr(101,325)]);
(272, [AAction2Instr(__a20,268)]);
(273, [EatInstr(116,326)]);
(274, [EatInstr(97,327)]);
(275, [AAction2Instr(__a21,276);ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,275)]);
(276, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,328)]);
(277, [EatInstr(116,329)]);
(278, [ACallInstr3(__default_call,8);ASimpleCont2Instr(272,__binder0,330)]);
(279, [EatInstr(108,331)]);
(280, [EatInstr(105,332)]);
(281, [EatInstr(101,333)]);
(282, [EatInstr(111,334)]);
(283, [EatInstr(101,335)]);
(284, [EatInstr(101,336)]);
(285, [AAction2Instr(__a22,80)]);
(286, [AAction2Instr(__a23,80)]);
(287, [EatInstr(103,338);EatInstr(114,337)]);
(288, [EatInstr(101,339)]);
(289, [EatInstr(110,340)]);
(290, [EatInstr(45,341)]);
(291, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,342)]);
(292, [AAction2Instr(__a24,343)]);
(293, [EatInstr(101,344)]);
(294, [EatInstr(110,345)]);
(295, [EatInstr(112,346)]);
(296, [EatInstr(105,347)]);
(297, [EatInstr(108,349);EatInstr(115,348)]);
(298, [EatInstr(108,350)]);
(299, [EatInstr(104,351)]);
(300, [EatInstr(105,352)]);
(301, [EatInstr(101,353)]);
(302, [EatInstr(45,354)]);
(303, [EatInstr(104,355)]);
(304, [AAction2Instr(__a25,692)]);
(305, [EatInstr(112,356)]);
(306, [EatInstr(105,357)]);
(307, [EatInstr(108,359);EatInstr(97,358)]);
(308, [EatInstr(116,360)]);
(309, [EatInstr(108,361)]);
(310, [EatInstr(109,362)]);
(311, [EatInstr(120,363)]);
(312, [EatInstr(100,364)]);
(313, [EatInstr(122,365)]);
(314, [EatInstr(116,366)]);
(315, [EatInstr(111,367)]);
(316, [AAction2Instr(__a26,268)]);
(317, [EatInstr(110,369);EatInstr(114,368)]);
(318, [EatInstr(121,370)]);
(319, [EatInstr(115,371)]);
(320, [AAction2Instr(__a27,268)]);
(321, [EatInstr(110,372)]);
(322, [EatInstr(101,373)]);
(324, [EatInstr(45,375);ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,374)]);
(325, [EatInstr(110,376)]);
(326, [EatInstr(101,377)]);
(327, [EatInstr(116,378)]);
(328, [AAction2Instr(__a28,80)]);
(329, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,379)]);
(330, [AAction2Instr(__a29,380)]);
(331, [EatInstr(97,381)]);
(332, [EatInstr(110,382)]);
(333, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,383)]);
(334, [EatInstr(107,384)]);
(335, [EatInstr(97,385)]);
(336, [EatInstr(110,386)]);
(337, [EatInstr(101,387)]);
(338, [EatInstr(105,388)]);
(339, [EatInstr(110,389)]);
(340, [EatInstr(101,390)]);
(341, [EatInstr(110,391)]);
(342, [ACallInstr3(__g2,2);AContInstr3(266,__g2,__binder4,392)]);
(343, [ACallInstr3(__default_call,8);ASimpleCont2Instr(272,__binder0,393)]);
(344, [EatInstr(45,394)]);
(345, [EatInstr(100,395)]);
(346, [EatInstr(97,396)]);
(347, [EatInstr(101,397)]);
(348, [EatInstr(109,399);EatInstr(116,398)]);
(349, [EatInstr(97,400)]);
(350, [EatInstr(45,401)]);
(351, [EatInstr(105,402)]);
(352, [EatInstr(110,403)]);
(353, [EatInstr(114,404)]);
(354, [EatInstr(108,405)]);
(355, [EatInstr(101,406)]);
(356, [EatInstr(108,407)]);
(357, [EatInstr(112,408)]);
(358, [EatInstr(108,409)]);
(359, [EatInstr(108,410)]);
(360, [EatInstr(105,411)]);
(361, [EatInstr(108,412)]);
(362, [EatInstr(111,413)]);
(363, [EatInstr(45,414)]);
(364, [EatInstr(45,415)]);
(365, [EatInstr(101,416)]);
(366, [EatInstr(101,417)]);
(367, [EatInstr(116,418)]);
(368, [EatInstr(101,419)]);
(369, [EatInstr(117,420)]);
(370, [EatInstr(112,421)]);
(371, [EatInstr(116,422)]);
(372, [EatInstr(100,423)]);
(373, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,424)]);
(374, [AAction2Instr(__a30,268)]);
(375, [EatInstr(103,425)]);
(376, [EatInstr(99,426)]);
(377, [EatInstr(45,427)]);
(378, [EatInstr(101,428)]);
(379, [AAction2Instr(__a31,80)]);
(380, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,429)]);
(381, [EatInstr(116,430)]);
(382, [EatInstr(101,431)]);
(383, [AAction2Instr(__a32,80)]);
(384, [EatInstr(97,432)]);
(385, [EatInstr(100,433)]);
(386, [EatInstr(99,434)]);
(387, [EatInstr(108,435)]);
(388, [EatInstr(108,436)]);
(389, [EatInstr(99,437)]);
(390, [EatInstr(114,438)]);
(391, [EatInstr(111,439)]);
(392, [AAction2Instr(__a33,692)]);
(393, [AAction2Instr(__a34,440)]);
(394, [EatInstr(99,442);EatInstr(114,441)]);
(395, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,443)]);
(396, [EatInstr(114,444)]);
(397, [EatInstr(114,445)]);
(398, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,446)]);
(399, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,447)]);
(400, [EatInstr(116,448)]);
(401, [EatInstr(115,449)]);
(402, [EatInstr(115,450)]);
(403, [EatInstr(115,451)]);
(404, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,452)]);
(405, [EatInstr(97,453)]);
(406, [EatInstr(97,454)]);
(407, [EatInstr(97,455)]);
(408, [EatInstr(45,456)]);
(409, [EatInstr(101,457)]);
(410, [EatInstr(97,458)]);
(411, [EatInstr(109,459)]);
(412, [EatInstr(97,460)]);
(413, [EatInstr(105,461)]);
(414, [EatInstr(104,462)]);
(415, [EatInstr(97,463)]);
(416, [EatInstr(45,464)]);
(417, [EatInstr(115,465)]);
(418, [EatInstr(97,466)]);
(419, [EatInstr(103,467)]);
(420, [EatInstr(108,468)]);
(421, [EatInstr(101,469)]);
(422, [EatInstr(97,470)]);
(423, [EatInstr(101,471)]);
(424, [AAction2Instr(__a35,268)]);
(425, [EatInstr(105,472)]);
(426, [EatInstr(101,473)]);
(427, [EatInstr(97,474)]);
(428, [EatInstr(45,475)]);
(429, [AAction2Instr(__a37,80);AAction2Instr(__a36,476)]);
(430, [EatInstr(101,477)]);
(431, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,478)]);
(432, [EatInstr(104,479)]);
(433, [EatInstr(45,480)]);
(434, [EatInstr(121,481)]);
(435, [EatInstr(101,482)]);
(436, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,483)]);
(437, [EatInstr(101,484)]);
(438, [EatInstr(97,485)]);
(439, [EatInstr(116,486)]);
(440, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,487)]);
(441, [EatInstr(101,488)]);
(442, [EatInstr(115,489)]);
(443, [EatInstr(102,493);EatInstr(119,492);EatInstr(112,491);EatInstr(116,490)]);
(444, [EatInstr(115,494)]);
(445, [EatInstr(109,496);EatInstr(104,495)]);
(446, [AAction2Instr(__a38,692)]);
(447, [AAction2Instr(__a39,692)]);
(448, [EatInstr(45,497)]);
(449, [EatInstr(116,498)]);
(450, [EatInstr(116,499)]);
(451, [EatInstr(101,500)]);
(452, [AAction2Instr(__a40,501)]);
(453, [EatInstr(98,502)]);
(454, [EatInstr(100,503)]);
(455, [EatInstr(121,504)]);
(456, [EatInstr(111,505)]);
(457, [EatInstr(115,506)]);
(458, [EatInstr(112,507)]);
(459, [EatInstr(105,508)]);
(460, [EatInstr(98,509)]);
(461, [EatInstr(122,510)]);
(462, [EatInstr(105,511)]);
(463, [EatInstr(114,512)]);
(464, [EatInstr(104,514);EatInstr(101,513)]);
(465, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,515)]);
(466, [EatInstr(116,516)]);
(467, [EatInstr(117,517)]);
(468, [EatInstr(108,518)]);
(469, [EatInstr(115,519)]);
(470, [EatInstr(114,520)]);
(471, [EatInstr(114,521)]);
(472, [EatInstr(108,522)]);
(473, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,523)]);
(474, [EatInstr(99,524)]);
(475, [EatInstr(100,525)]);
(476, [ACallInstr3(__default_call,8);ASimpleCont2Instr(272,__binder0,526)]);
(477, [EatInstr(45,527)]);
(478, [AAction2Instr(__a41,80)]);
(479, [EatInstr(101,528)]);
(480, [EatInstr(97,529)]);
(481, [EatInstr(45,530)]);
(482, [EatInstr(118,531)]);
(483, [AAction2Instr(__a42,80)]);
(484, [EatInstr(45,532);ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,523)]);
(485, [EatInstr(116,533)]);
(486, [EatInstr(97,534)]);
(487, [AAction2Instr(__a43,692)]);
(488, [EatInstr(103,535)]);
(489, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,536)]);
(490, [EatInstr(120,537)]);
(491, [EatInstr(101,538)]);
(492, [EatInstr(97,539)]);
(493, [EatInstr(117,540)]);
(494, [EatInstr(101,541)]);
(495, [EatInstr(45,542)]);
(496, [EatInstr(45,543)]);
(497, [EatInstr(115,544)]);
(498, [EatInstr(97,545)]);
(499, [EatInstr(111,546)]);
(500, [EatInstr(110,547)]);
(501, [ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,589)]);
(502, [EatInstr(101,548)]);
(503, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,549)]);
(504, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,550)]);
(505, [EatInstr(112,551)]);
(506, [EatInstr(99,552)]);
(507, [EatInstr(115,553)]);
(508, [EatInstr(122,554)]);
(509, [EatInstr(105,555)]);
(510, [EatInstr(101,556)]);
(511, [EatInstr(115,557)]);
(512, [EatInstr(114,558)]);
(513, [EatInstr(112,559)]);
(514, [EatInstr(105,560)]);
(515, [AAction2Instr(__a44,268)]);
(516, [EatInstr(105,561)]);
(517, [EatInstr(108,562)]);
(518, [EatInstr(97,563)]);
(519, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,564)]);
(520, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,565)]);
(521, [EatInstr(45,566)]);
(522, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,567)]);
(523, [AAction2Instr(__a45,268)]);
(524, [EatInstr(116,568)]);
(525, [EatInstr(121,569)]);
(526, [AAction2Instr(__a46,380)]);
(527, [EatInstr(97,570)]);
(528, [EatInstr(97,571)]);
(529, [EatInstr(110,572)]);
(530, [EatInstr(103,573)]);
(531, [EatInstr(97,574)]);
(532, [EatInstr(97,575)]);
(533, [EatInstr(111,576)]);
(534, [EatInstr(116,577)]);
(535, [EatInstr(117,578)]);
(536, [AAction2Instr(__a47,692)]);
(537, [AAction2Instr(__a48,713)]);
(538, [EatInstr(103,579)]);
(539, [EatInstr(100,580)]);
(540, [EatInstr(110,581)]);
(541, [EatInstr(45,582)]);
(542, [EatInstr(115,583)]);
(543, [EatInstr(115,584)]);
(544, [EatInstr(101,585)]);
(545, [EatInstr(114,586)]);
(546, [EatInstr(114,587)]);
(547, [EatInstr(115,588)]);
(548, [EatInstr(108,591)]);
(549, [AAction2Instr(__a49,692)]);
(550, [AAction2Instr(__a50,692)]);
(551, [EatInstr(116,592)]);
(552, [EatInstr(101,593)]);
(553, [EatInstr(101,594)]);
(554, [EatInstr(101,595)]);
(555, [EatInstr(108,596)]);
(556, [EatInstr(45,597)]);
(557, [EatInstr(116,598)]);
(558, [EatInstr(111,599)]);
(559, [EatInstr(115,600)]);
(560, [EatInstr(115,601)]);
(561, [EatInstr(111,602)]);
(562, [EatInstr(97,603)]);
(563, [EatInstr(98,604)]);
(564, [AAction2Instr(__a51,268)]);
(565, [AAction2Instr(__a52,268)]);
(566, [EatInstr(99,605)]);
(567, [AAction2Instr(__a53,268)]);
(568, [EatInstr(105,606)]);
(569, [EatInstr(112,607)]);
(570, [EatInstr(99,608)]);
(571, [EatInstr(100,609)]);
(572, [EatInstr(97,610)]);
(573, [EatInstr(114,611)]);
(574, [EatInstr(110,612)]);
(575, [EatInstr(110,613)]);
(576, [EatInstr(114,614)]);
(577, [EatInstr(105,615)]);
(578, [EatInstr(108,616)]);
(579, [EatInstr(45,618);AAction2Instr(__a54,713)]);
(580, [EatInstr(108,619)]);
(581, [AAction2Instr(__a55,713)]);
(582, [EatInstr(115,620)]);
(583, [EatInstr(101,621)]);
(584, [EatInstr(101,622)]);
(585, [EatInstr(116,623)]);
(586, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,624)]);
(587, [EatInstr(121,625)]);
(588, [EatInstr(105,626)]);
(589, [AAction2Instr(__a56,590);ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,589)]);
(590, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,627)]);
(591, [EatInstr(115,628)]);
(592, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,629)]);
(593, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,630)]);
(594, [EatInstr(45,631)]);
(595, [EatInstr(45,632)]);
(596, [EatInstr(105,633)]);
(597, [EatInstr(104,635);EatInstr(101,634)]);
(598, [EatInstr(111,636)]);
(599, [EatInstr(119,637)]);
(600, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,638)]);
(601, [EatInstr(116,639)]);
(602, [EatInstr(110,640)]);
(603, [EatInstr(114,641)]);
(604, [EatInstr(108,642)]);
(605, [EatInstr(111,643)]);
(606, [EatInstr(111,644)]);
(607, [EatInstr(103,645)]);
(608, [EatInstr(116,646)]);
(609, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,647)]);
(610, [EatInstr(108,648)]);
(611, [EatInstr(97,649)]);
(612, [EatInstr(99,650)]);
(613, [EatInstr(97,651)]);
(614, [EatInstr(115,652)]);
(615, [EatInstr(111,653)]);
(616, [EatInstr(97,654)]);
(617, [AAction2Instr(__a57,692)]);
(618, [EatInstr(115,655)]);
(619, [EatInstr(101,656)]);
(620, [EatInstr(101,657)]);
(621, [EatInstr(116,658)]);
(622, [EatInstr(116,659)]);
(623, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,660)]);
(624, [AAction2Instr(__a58,661)]);
(625, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,662)]);
(626, [EatInstr(116,663)]);
(627, [AAction2Instr(__a59,692)]);
(628, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,664)]);
(629, [AAction2Instr(__a60,692)]);
(630, [AAction2Instr(__a61,692)]);
(631, [EatInstr(99,665)]);
(632, [EatInstr(112,666)]);
(633, [EatInstr(116,667)]);
(634, [EatInstr(112,668)]);
(635, [EatInstr(105,669)]);
(636, [EatInstr(114,670)]);
(637, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,671)]);
(638, [AAction2Instr(__a62,692)]);
(639, [EatInstr(111,672)]);
(640, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,673)]);
(641, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,674)]);
(642, [EatInstr(101,675)]);
(643, [EatInstr(114,676)]);
(644, [EatInstr(110,677)]);
(645, [EatInstr(101,678)]);
(646, [EatInstr(105,679)]);
(647, [AAction2Instr(__a63,80)]);
(648, [EatInstr(121,680)]);
(649, [EatInstr(112,681)]);
(650, [EatInstr(101,682)]);
(651, [EatInstr(108,683)]);
(652, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,684)]);
(653, [EatInstr(110,685)]);
(654, [EatInstr(114,686)]);
(655, [EatInstr(116,687)]);
(656, [EatInstr(114,688)]);
(657, [EatInstr(116,689)]);
(658, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,690)]);
(659, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,691)]);
(660, [AAction2Instr(__a64,692)]);
(661, [ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,715)]);
(662, [AAction2Instr(__a65,692)]);
(663, [EatInstr(105,693)]);
(664, [AAction2Instr(__a66,692)]);
(665, [EatInstr(97,694)]);
(666, [EatInstr(97,695)]);
(667, [EatInstr(121,696)]);
(668, [EatInstr(115,697)]);
(669, [EatInstr(115,698)]);
(670, [EatInstr(121,699)]);
(671, [AAction2Instr(__a67,692)]);
(672, [EatInstr(114,700)]);
(673, [AAction2Instr(__a68,268)]);
(674, [AAction2Instr(__a69,268)]);
(675, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,701)]);
(676, [EatInstr(101,702)]);
(677, [EatInstr(115,703)]);
(678, [EatInstr(110,704)]);
(679, [EatInstr(111,705)]);
(680, [EatInstr(115,706)]);
(681, [EatInstr(104,707)]);
(682, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,708)]);
(683, [EatInstr(121,709)]);
(684, [AAction2Instr(__a70,80)]);
(685, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,710)]);
(686, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,711)]);
(687, [EatInstr(114,712)]);
(688, [AAction2Instr(__a71,713)]);
(689, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,714)]);
(690, [AAction2Instr(__a72,692)]);
(691, [AAction2Instr(__a73,692)]);
(692, [CompleteInstr(274)]);
(693, [EatInstr(118,717)]);
(694, [EatInstr(108,718)]);
(695, [EatInstr(109,719)]);
(696, [EatInstr(45,720)]);
(697, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,721)]);
(698, [EatInstr(116,722)]);
(699, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,723)]);
(700, [EatInstr(121,724)]);
(701, [AAction2Instr(__a74,268)]);
(702, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,725)]);
(703, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,726)]);
(704, [EatInstr(45,728);ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,727)]);
(705, [EatInstr(110,729)]);
(706, [EatInstr(105,730)]);
(707, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,731)]);
(708, [AAction2Instr(__a75,80)]);
(709, [EatInstr(115,732)]);
(710, [AAction2Instr(__a76,692)]);
(711, [AAction2Instr(__a77,692)]);
(712, [EatInstr(105,733)]);
(713, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,617)]);
(714, [AAction2Instr(__a78,692)]);
(715, [AAction2Instr(__a79,716);ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,715)]);
(716, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,734)]);
(717, [EatInstr(101,735)]);
(718, [EatInstr(108,736)]);
(719, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,737)]);
(720, [EatInstr(112,738)]);
(721, [AAction2Instr(__a80,692)]);
(722, [EatInstr(111,739)]);
(723, [AAction2Instr(__a81,692)]);
(724, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,740)]);
(725, [AAction2Instr(__a82,268)]);
(726, [AAction2Instr(__a83,80)]);
(727, [AAction2Instr(__a84,80)]);
(728, [EatInstr(115,741)]);
(729, [EatInstr(115,742)]);
(730, [EatInstr(115,743)]);
(731, [AAction2Instr(__a85,80)]);
(732, [EatInstr(105,744)]);
(733, [EatInstr(99,745)]);
(734, [AAction2Instr(__a86,692)]);
(735, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,746)]);
(736, [EatInstr(115,747)]);
(737, [AAction2Instr(__a87,692)]);
(738, [EatInstr(114,748)]);
(739, [EatInstr(114,749)]);
(740, [AAction2Instr(__a88,692)]);
(741, [EatInstr(99,750)]);
(742, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,751)]);
(743, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,752)]);
(744, [EatInstr(115,753)]);
(745, [EatInstr(116,754)]);
(746, [AAction2Instr(__a89,692)]);
(747, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,755)]);
(748, [EatInstr(101,756)]);
(749, [EatInstr(121,757)]);
(750, [EatInstr(97,758)]);
(751, [AAction2Instr(__a90,80)]);
(752, [AAction2Instr(__a91,80)]);
(753, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,759)]);
(754, [AAction2Instr(__a92,713)]);
(755, [AAction2Instr(__a93,692)]);
(756, [EatInstr(100,760)]);
(757, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,761)]);
(758, [EatInstr(110,762)]);
(759, [AAction2Instr(__a94,80)]);
(760, [EatInstr(115,763)]);
(761, [AAction2Instr(__a95,692)]);
(762, [EatInstr(110,764)]);
(763, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,765)]);
(764, [EatInstr(101,766)]);
(765, [AAction2Instr(__a96,692)]);
(766, [EatInstr(114,767)]);
]

module M = Yak.Pami.Wfe.Make(
    struct
      module Parse_engine = Yak.Engine
      module Term_language = Parse_engine.Scannerless_term_lang
      let start_symbol_name = "cmd-line-args"

      let sv0 = sv0
      module Semval =
      struct
        type t = sv
        let cmp = sv_compare
        include Yak.Pami.Wfe.History_inspector(Yk_History)
      end

      let program = program
      let get_symb_action = get_symb_action
      let get_symb_start = get_symb_start
      let min_symbol = 264
      let num_symbols = num_symbols
      let opt_mode = Yak.PamJIT.Full_opt
      let default_call = __default_call
      let default_ret = __default_ret
    end)
let parse = M.gen_parse _replay_cmd_line_args
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
