
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
 | (2069) -> ((); ();  O.coalesce := false ; ())
 | (2070) -> ((); ();  O.collapse_calls := false ; ())
 | (2071) -> ((); ();  O.memoize_history := false ; ())
 | (2072) -> ((); ();  O.gen_nullpreds := false ; ())
 | (2073) -> ((); ();  O.gen_optimize_pam := false ; ())
 | (2074) -> ((); ();  O.repress_replay := true ; ())
 | (2075) -> ((); ();  O.skip_opt := false ; ())
 | (2076) -> ((); ();  only := true ; ())
 | (2077) -> ((); ();  O.postfix_history := false ; ())
 | (2078) -> ((); (); (let _x14 = _p() in (); (let _x13 = _p() in (let x = Yak.YkBuf.get_string _x14 _x13 ykinput in ();  roots := x::!roots ; ()))))
 | (2081) -> ((); ();  O.unit_history := true ; ())
 | (2082) -> ((); (); (let _x16 = _p() in (); (let _x15 = _p() in (let n = Yak.YkBuf.get_string _x16 _x15 ykinput in ();  O.unroll_star_n := (int_of_string n) ; ()))))
 | (2085) -> ((); ();  O.earley_ds := O.Flat_eds ; ())
 | (2086) -> ((); ();  O.earley_ds := O.Hierhash_eds ; ())
 | (2087) -> ((); ();  O.earley_ds := O.Hiermap_eds ; ())
 | (2088) -> ((); ();  O.earley_ds := O.Sparse_eds ; ())
 | (2089) -> ((); ();  O.use_fsm := true ; ())
 | (2090) -> ((); ();  O.use_fsm := false ; ())
 | (2091) -> ((); ();  Yak.Logging.add_features Yak.Logging.Features.verbose ; ())
 | (2092) -> ((let _x18 = _p() in (); (let _x17 = _p() in (let f = Yak.YkBuf.get_string _x18 _x17 ykinput in ();  files := f::!files ; ()))))
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
 | (2078) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2078)))
 | (2081) -> (();();();(); push((2081)))
 | (2082) -> (();();();();push_pos(_p());();push_pos(_p());();(); push((2082)))
 | (2085) -> (();();();(); push((2085)))
 | (2086) -> (();();();(); push((2086)))
 | (2087) -> (();();();(); push((2087)))
 | (2088) -> (();();();(); push((2088)))
 | (2089) -> (();();();(); push((2089)))
 | (2090) -> (();();();(); push((2090)))
 | (2091) -> (();();();(); push((2091)))
 | (2092) -> (();();();();push_pos(_p());();push_pos(_p()); push((2092)))
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
let __a73 = (_p 1012 ((2011)));;
let __a5 = (_p 1002 ((2001)));;
let __a70 = (_p 1092 ((2057)));;
let __a25 = (_p 1072 ((2076)));;
let __a15 = (_p 1037 ((2035)));;
let __a27 = (_p 1007 ((2016)));;
let __a33 = (_p 1097 ((2051)));;
let __a63 = (_p 1063 ((2085)));;
let __a87 = (_p 1088 ((2060)));;
let __a43 = (_p 1068 ((2078)));;
let __a84 = (_p 1064 ((2082)));;
let __a51 = (_p 1014 ((2009)));;
let __a31 = (_p 1040 ((2032)));;
let __a12 = (_p 1010 ((2013)));;
let __a54 = (_p 1090 ((2059)));;
let __a78 = (_p 1065 ((2084)));;
let __a62 = (_p 1035 ((2037)));;
let __a14 = (_p 1005 ((2018)));;
let __a66 = (_p 1095 ((2053)));;
let __a72 = (_p 1061 ((2087)));;
let __a42 = (_p 1031 ((2041)));;
let __a3 = (_p 1056 ((2093)));;
let __a17 = (_p 1026 ((2046)));;
let __a29 = (fun _x0_ _x1_ -> (((_p 1045 ((2029))) _x0_) (((_p 1046 ((2027))) _x0_) _x1_)));;
let __a95 = (_p 1022 ((2050)));;
let __a28 = (_p 1027 ((2043)));;
let __a93 = (_p 1077 ((2071)));;
let __a92 = (_p 1033 ((2039)));;
let __a21 = (_p 1028 ((2045)));;
let __a36 = (_p 1044 ((2030)));;
let __a88 = (_p 1024 ((2048)));;
let __a83 = (_p 1049 ((2023)));;
let __a35 = (_p 1019 ((2004)));;
let __a61 = (_p 1079 ((2069)));;
let __a24 = (_p 1070 ((2079)));;
let __a85 = (_p 1075 ((2073)));;
let __a40 = (_p 1086 ((2063)));;
let __a76 = (_p 1082 ((2066)));;
let __a4 = (_p 1052 ((2020)));;
let __a53 = (_p 1017 ((2006)));;
let __a8 = (_p 1057 ((2091)));;
let __p0 = fun _ v -> v;;
let __a48 = (_p 1093 ((2056)));;
let __a60 = (_p 1073 ((2075)));;
let __a68 = (_p 1013 ((2010)));;
let __a69 = (_p 1038 ((2034)));;
let __a45 = (_p 1008 ((2015)));;
let __a39 = (_p 1059 ((2089)));;
let __a86 = (_p 1080 ((2068)));;
let __a9 = (_p 1000 ((2000)));;
let __a41 = (_p 1050 ((2022)));;
let __a80 = (_p 1020 ((2003)));;
let __a67 = (_p 1015 ((2008)));;
let __a90 = (_p 1091 ((2058)));;
let __a79 = (_p 1071 ((2077)));;
let __a37 = (_p 1041 ((2025)));;
let __a19 = (_p 1011 ((2012)));;
let __a46 = (fun _x0_ _x1_ -> (((_p 1042 ((2028))) _x0_) (((_p 1043 ((2031))) _x0_) _x1_)));;
let __a89 = (_p 1036 ((2036)));;
let __a52 = (_p 1006 ((2017)));;
let __a75 = (_p 1096 ((2052)));;
let __a58 = (_p 1066 ((2083)));;
let __a71 = (_p 1062 ((2086)));;
let __a23 = (_p 1032 ((2040)));;
let __a16 = (_p 1047 ((2026)));;
let __a65 = (_p 1087 ((2061)));;
let __g2 = (_e);;
let __p1 = fun _ _ -> false;;
let __a91 = (_p 1078 ((2070)));;
let __a11 = (_p 1054 ((2092)));;
let __a22 = (_p 1034 ((2038)));;
let __a26 = (_p 1004 ((2019)));;
let __a7 = (_p 1029 ((2044)));;
let __a57 = (_p 1089 ((2054)));;
let __a77 = (_p 1060 ((2088)));;
let __a74 = (_p 1030 ((2042)));;
let __a81 = (_p 1025 ((2047)));;
let __a6 = (_p 1055 ((2094)));;
let __a94 = (_p 1076 ((2072)));;
let __a64 = (_p 1067 ((2081)));;
let __a82 = (_p 1023 ((2049)));;
let __a47 = (_p 1083 ((2065)));;
let __a38 = (_p 1058 ((2090)));;
let __a10 = (_p 1048 ((2024)));;
let __a30 = (_p 1018 ((2005)));;
let __a59 = (_p 1084 ((2062)));;
let __a55 = (_p 1094 ((2055)));;
let __a50 = (_p 1074 ((2074)));;
let __a34 = (_p 1069 ((2080)));;
let __a18 = (_p 1039 ((2033)));;
let __a20 = (_p 1009 ((2014)));;
let __a56 = (_p 1085 ((2064)));;
let __a44 = (_p 1021 ((2002)));;
let __a49 = (_p 1081 ((2067)));;
let __a32 = (_p 1051 ((2021)));;
let __a13 = (_p 1016 ((2007)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1053);;
let __binder2 = (_m 1003);;
let __binder3 = (_m 1001);;
let __binder4 = (_m 1098);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(101,432)]);
(0, [ASimpleCont2Instr(275,__binder0,11);ASimpleCont2Instr(274,__binder0,10);ASimpleCont2Instr(273,__binder0,9);ASimpleCont2Instr(272,__binder0,8);ASimpleCont2Instr(271,__binder0,7);ASimpleCont2Instr(270,__binder0,6);ASimpleCont2Instr(269,__binder0,5);ASimpleCont2Instr(268,__binder0,4);ASimpleCont2Instr(267,__binder0,3);ASimpleCont2Instr(266,__binder0,2);ASimpleCont2Instr(265,__binder0,1)]);
(384, [AAction2Instr(__a32,80)]);
(1, [EatInstr(0,12)]);
(385, [EatInstr(97,433)]);
(2, [EatInstr(109,24);EatInstr(119,23);EatInstr(104,22);EatInstr(112,21);EatInstr(100,20);EatInstr(108,19);EatInstr(99,18);EatInstr(115,17);EatInstr(117,16);EatInstr(105,15);EatInstr(114,14);EatInstr(97,13)]);
(386, [EatInstr(100,434)]);
(3, [EatInstr(127,25);EatInstr(126,25);EatInstr(125,25);EatInstr(124,25);EatInstr(123,25);EatInstr(122,25);EatInstr(118,25);EatInstr(113,25);EatInstr(107,25);EatInstr(106,25);EatInstr(96,25);EatInstr(95,25);EatInstr(94,25);EatInstr(93,25);EatInstr(92,25);EatInstr(91,25);EatInstr(90,25);EatInstr(89,25);EatInstr(88,25);EatInstr(87,25);EatInstr(86,25);EatInstr(85,25);EatInstr(84,25);EatInstr(83,25);EatInstr(82,25);EatInstr(81,25);EatInstr(80,25);EatInstr(79,25);EatInstr(78,25);EatInstr(77,25);EatInstr(76,25);EatInstr(75,25);EatInstr(74,25);EatInstr(73,25);EatInstr(72,25);EatInstr(71,25);EatInstr(70,25);EatInstr(69,25);EatInstr(68,25);EatInstr(67,25);EatInstr(66,25);EatInstr(65,25);EatInstr(64,25);EatInstr(63,25);EatInstr(62,25);EatInstr(61,25);EatInstr(60,25);EatInstr(59,25);EatInstr(58,25);EatInstr(57,25);EatInstr(56,25);EatInstr(55,25);EatInstr(54,25);EatInstr(53,25);EatInstr(52,25);EatInstr(51,25);EatInstr(50,25);EatInstr(49,25);EatInstr(48,25);EatInstr(47,25);EatInstr(46,25);EatInstr(44,25);EatInstr(43,25);EatInstr(42,25);EatInstr(41,25);EatInstr(40,25);EatInstr(39,25);EatInstr(38,25);EatInstr(37,25);EatInstr(36,25);EatInstr(35,25);EatInstr(34,25);EatInstr(33,25);EatInstr(32,25);EatInstr(31,25);EatInstr(30,25);EatInstr(29,25);EatInstr(28,25);EatInstr(27,25);EatInstr(26,25);EatInstr(25,25);EatInstr(24,25);EatInstr(23,25);EatInstr(22,25);EatInstr(21,25);EatInstr(20,25);EatInstr(19,25);EatInstr(18,25);EatInstr(17,25);EatInstr(16,25);EatInstr(15,25);EatInstr(14,25);EatInstr(13,25);EatInstr(12,25);EatInstr(11,25);EatInstr(10,25);EatInstr(9,25);EatInstr(8,25);EatInstr(7,25);EatInstr(6,25);EatInstr(5,25);EatInstr(4,25);EatInstr(3,25);EatInstr(2,25);EatInstr(1,25);EatInstr(109,25);EatInstr(120,25);EatInstr(102,25);EatInstr(119,25);EatInstr(104,25);EatInstr(103,25);EatInstr(121,25);EatInstr(112,25);EatInstr(100,25);EatInstr(110,25);EatInstr(45,25);EatInstr(111,25);EatInstr(108,25);EatInstr(99,25);EatInstr(115,25);EatInstr(101,25);EatInstr(117,25);EatInstr(98,25);EatInstr(105,25);EatInstr(114,25);EatInstr(116,25);EatInstr(97,25)]);
(387, [EatInstr(99,435)]);
(4, [EatInstr(127,60);EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(122,60);EatInstr(118,60);EatInstr(113,60);EatInstr(107,60);EatInstr(106,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(49,60);EatInstr(48,60);EatInstr(47,60);EatInstr(46,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(32,60);EatInstr(31,60);EatInstr(30,60);EatInstr(29,60);EatInstr(28,60);EatInstr(27,60);EatInstr(26,60);EatInstr(25,60);EatInstr(24,60);EatInstr(23,60);EatInstr(22,60);EatInstr(21,60);EatInstr(20,60);EatInstr(19,60);EatInstr(18,60);EatInstr(17,60);EatInstr(16,60);EatInstr(15,60);EatInstr(14,60);EatInstr(13,60);EatInstr(12,60);EatInstr(11,60);EatInstr(10,60);EatInstr(9,60);EatInstr(8,60);EatInstr(7,60);EatInstr(6,60);EatInstr(5,60);EatInstr(4,60);EatInstr(3,60);EatInstr(2,60);EatInstr(1,60);EatInstr(109,60);EatInstr(120,60);EatInstr(102,60);EatInstr(119,60);EatInstr(104,60);EatInstr(103,60);EatInstr(121,60);EatInstr(112,60);EatInstr(100,60);EatInstr(110,60);EatInstr(111,60);EatInstr(108,60);EatInstr(99,60);EatInstr(115,60);EatInstr(101,60);EatInstr(117,60);EatInstr(98,60);EatInstr(105,60);EatInstr(114,60);EatInstr(116,60);EatInstr(97,60)]);
(388, [EatInstr(108,436)]);
(5, [EatInstr(255,26);EatInstr(254,26);EatInstr(253,26);EatInstr(252,26);EatInstr(251,26);EatInstr(250,26);EatInstr(249,26);EatInstr(248,26);EatInstr(247,26);EatInstr(246,26);EatInstr(245,26);EatInstr(244,26);EatInstr(243,26);EatInstr(242,26);EatInstr(241,26);EatInstr(240,26);EatInstr(239,26);EatInstr(238,26);EatInstr(237,26);EatInstr(236,26);EatInstr(235,26);EatInstr(234,26);EatInstr(233,26);EatInstr(232,26);EatInstr(231,26);EatInstr(230,26);EatInstr(229,26);EatInstr(228,26);EatInstr(227,26);EatInstr(226,26);EatInstr(225,26);EatInstr(224,26);EatInstr(223,26);EatInstr(222,26);EatInstr(221,26);EatInstr(220,26);EatInstr(219,26);EatInstr(218,26);EatInstr(217,26);EatInstr(216,26);EatInstr(215,26);EatInstr(214,26);EatInstr(213,26);EatInstr(212,26);EatInstr(211,26);EatInstr(210,26);EatInstr(209,26);EatInstr(208,26);EatInstr(207,26);EatInstr(206,26);EatInstr(205,26);EatInstr(204,26);EatInstr(203,26);EatInstr(202,26);EatInstr(201,26);EatInstr(200,26);EatInstr(199,26);EatInstr(198,26);EatInstr(197,26);EatInstr(196,26);EatInstr(195,26);EatInstr(194,26);EatInstr(193,26);EatInstr(192,26);EatInstr(191,26);EatInstr(190,26);EatInstr(189,26);EatInstr(188,26);EatInstr(187,26);EatInstr(186,26);EatInstr(185,26);EatInstr(184,26);EatInstr(183,26);EatInstr(182,26);EatInstr(181,26);EatInstr(180,26);EatInstr(179,26);EatInstr(178,26);EatInstr(177,26);EatInstr(176,26);EatInstr(175,26);EatInstr(174,26);EatInstr(173,26);EatInstr(172,26);EatInstr(171,26);EatInstr(170,26);EatInstr(169,26);EatInstr(168,26);EatInstr(167,26);EatInstr(166,26);EatInstr(165,26);EatInstr(164,26);EatInstr(163,26);EatInstr(162,26);EatInstr(161,26);EatInstr(160,26);EatInstr(159,26);EatInstr(158,26);EatInstr(157,26);EatInstr(156,26);EatInstr(155,26);EatInstr(154,26);EatInstr(153,26);EatInstr(152,26);EatInstr(151,26);EatInstr(150,26);EatInstr(149,26);EatInstr(148,26);EatInstr(147,26);EatInstr(146,26);EatInstr(145,26);EatInstr(144,26);EatInstr(143,26);EatInstr(142,26);EatInstr(141,26);EatInstr(140,26);EatInstr(139,26);EatInstr(138,26);EatInstr(137,26);EatInstr(136,26);EatInstr(135,26);EatInstr(134,26);EatInstr(133,26);EatInstr(132,26);EatInstr(131,26);EatInstr(130,26);EatInstr(129,26);EatInstr(128,26);EatInstr(127,26);EatInstr(126,26);EatInstr(125,26);EatInstr(124,26);EatInstr(123,26);EatInstr(122,26);EatInstr(118,26);EatInstr(113,26);EatInstr(107,26);EatInstr(106,26);EatInstr(96,26);EatInstr(95,26);EatInstr(94,26);EatInstr(93,26);EatInstr(92,26);EatInstr(91,26);EatInstr(90,26);EatInstr(89,26);EatInstr(88,26);EatInstr(87,26);EatInstr(86,26);EatInstr(85,26);EatInstr(84,26);EatInstr(83,26);EatInstr(82,26);EatInstr(81,26);EatInstr(80,26);EatInstr(79,26);EatInstr(78,26);EatInstr(77,26);EatInstr(76,26);EatInstr(75,26);EatInstr(74,26);EatInstr(73,26);EatInstr(72,26);EatInstr(71,26);EatInstr(70,26);EatInstr(69,26);EatInstr(68,26);EatInstr(67,26);EatInstr(66,26);EatInstr(65,26);EatInstr(64,26);EatInstr(63,26);EatInstr(62,26);EatInstr(61,26);EatInstr(60,26);EatInstr(59,26);EatInstr(58,26);EatInstr(57,26);EatInstr(56,26);EatInstr(55,26);EatInstr(54,26);EatInstr(53,26);EatInstr(52,26);EatInstr(51,26);EatInstr(50,26);EatInstr(49,26);EatInstr(48,26);EatInstr(47,26);EatInstr(46,26);EatInstr(44,26);EatInstr(43,26);EatInstr(42,26);EatInstr(41,26);EatInstr(40,26);EatInstr(39,26);EatInstr(38,26);EatInstr(37,26);EatInstr(36,26);EatInstr(35,26);EatInstr(34,26);EatInstr(33,26);EatInstr(32,26);EatInstr(31,26);EatInstr(30,26);EatInstr(29,26);EatInstr(28,26);EatInstr(27,26);EatInstr(26,26);EatInstr(25,26);EatInstr(24,26);EatInstr(23,26);EatInstr(22,26);EatInstr(21,26);EatInstr(20,26);EatInstr(19,26);EatInstr(18,26);EatInstr(17,26);EatInstr(16,26);EatInstr(15,26);EatInstr(14,26);EatInstr(13,26);EatInstr(12,26);EatInstr(11,26);EatInstr(10,26);EatInstr(9,26);EatInstr(8,26);EatInstr(7,26);EatInstr(6,26);EatInstr(5,26);EatInstr(4,26);EatInstr(3,26);EatInstr(2,26);EatInstr(1,26);EatInstr(109,26);EatInstr(120,26);EatInstr(102,26);EatInstr(119,26);EatInstr(104,26);EatInstr(103,26);EatInstr(121,26);EatInstr(112,26);EatInstr(100,26);EatInstr(110,26);EatInstr(45,26);EatInstr(111,26);EatInstr(108,26);EatInstr(99,26);EatInstr(115,26);EatInstr(101,26);EatInstr(117,26);EatInstr(98,26);EatInstr(105,26);EatInstr(114,26);EatInstr(116,26);EatInstr(97,26);EatInstr(0,26)]);
(389, [EatInstr(108,437)]);
(6, [AWhenInstr3(__p1,__p0,27)]);
(390, [EatInstr(99,438)]);
(7, [EatInstr(57,28);EatInstr(56,28);EatInstr(55,28);EatInstr(54,28);EatInstr(53,28);EatInstr(52,28);EatInstr(51,28);EatInstr(50,28);EatInstr(49,28);EatInstr(48,28)]);
(391, [EatInstr(114,439)]);
(8, [EatInstr(127,25);EatInstr(126,25);EatInstr(125,25);EatInstr(124,25);EatInstr(123,25);EatInstr(122,25);EatInstr(118,25);EatInstr(113,25);EatInstr(107,25);EatInstr(106,25);EatInstr(96,25);EatInstr(95,25);EatInstr(94,25);EatInstr(93,25);EatInstr(92,25);EatInstr(91,25);EatInstr(90,25);EatInstr(89,25);EatInstr(88,25);EatInstr(87,25);EatInstr(86,25);EatInstr(85,25);EatInstr(84,25);EatInstr(83,25);EatInstr(82,25);EatInstr(81,25);EatInstr(80,25);EatInstr(79,25);EatInstr(78,25);EatInstr(77,25);EatInstr(76,25);EatInstr(75,25);EatInstr(74,25);EatInstr(73,25);EatInstr(72,25);EatInstr(71,25);EatInstr(70,25);EatInstr(69,25);EatInstr(68,25);EatInstr(67,25);EatInstr(66,25);EatInstr(65,25);EatInstr(64,25);EatInstr(63,25);EatInstr(62,25);EatInstr(61,25);EatInstr(60,25);EatInstr(59,25);EatInstr(58,25);EatInstr(57,25);EatInstr(56,25);EatInstr(55,25);EatInstr(54,25);EatInstr(53,25);EatInstr(52,25);EatInstr(51,25);EatInstr(50,25);EatInstr(49,25);EatInstr(48,25);EatInstr(47,25);EatInstr(46,25);EatInstr(44,25);EatInstr(43,25);EatInstr(42,25);EatInstr(41,25);EatInstr(40,25);EatInstr(39,25);EatInstr(38,25);EatInstr(37,25);EatInstr(36,25);EatInstr(35,25);EatInstr(34,25);EatInstr(33,25);EatInstr(32,25);EatInstr(31,25);EatInstr(30,25);EatInstr(29,25);EatInstr(28,25);EatInstr(27,25);EatInstr(26,25);EatInstr(25,25);EatInstr(24,25);EatInstr(23,25);EatInstr(22,25);EatInstr(21,25);EatInstr(20,25);EatInstr(19,25);EatInstr(18,25);EatInstr(17,25);EatInstr(16,25);EatInstr(15,25);EatInstr(14,25);EatInstr(13,25);EatInstr(12,25);EatInstr(11,25);EatInstr(10,25);EatInstr(9,25);EatInstr(8,25);EatInstr(7,25);EatInstr(6,25);EatInstr(5,25);EatInstr(4,25);EatInstr(3,25);EatInstr(2,25);EatInstr(1,25);EatInstr(109,25);EatInstr(120,25);EatInstr(102,25);EatInstr(119,25);EatInstr(104,25);EatInstr(103,25);EatInstr(121,25);EatInstr(112,25);EatInstr(100,25);EatInstr(110,25);EatInstr(45,25);EatInstr(111,25);EatInstr(108,25);EatInstr(99,25);EatInstr(115,25);EatInstr(101,25);EatInstr(117,25);EatInstr(98,25);EatInstr(105,25);EatInstr(114,25);EatInstr(116,25);EatInstr(97,25);ASimpleCont2Instr(267,__binder0,62)]);
(392, [EatInstr(111,440)]);
(9, [EatInstr(109,24);EatInstr(102,40);EatInstr(119,23);EatInstr(104,22);EatInstr(103,39);EatInstr(112,38);EatInstr(100,37);EatInstr(108,36);EatInstr(99,35);EatInstr(115,34);EatInstr(101,33);EatInstr(117,16);EatInstr(105,32);EatInstr(114,31);EatInstr(116,30);EatInstr(97,29);AContInstr3(266,__g2,__binder1,41)]);
(393, [AAction2Instr(__a33,202)]);
(10, [EatInstr(45,42);AAction2Instr(__a3,43)]);
(394, [AAction2Instr(__a34,441)]);
(11, [EatInstr(109,24);EatInstr(102,40);EatInstr(119,23);EatInstr(104,22);EatInstr(103,39);EatInstr(112,38);EatInstr(100,37);EatInstr(108,36);EatInstr(99,35);EatInstr(115,34);EatInstr(101,33);EatInstr(117,16);EatInstr(105,32);EatInstr(114,31);EatInstr(116,30);EatInstr(97,29);AContInstr3(273,__g2,__binder2,44);AContInstr3(266,__g2,__binder1,41)]);
(395, [EatInstr(99,443);EatInstr(114,442)]);
(12, [CompleteInstr(265)]);
(396, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,444)]);
(13, [EatInstr(114,46);EatInstr(116,45)]);
(397, [EatInstr(114,445)]);
(14, [EatInstr(101,47)]);
(398, [EatInstr(114,446)]);
(15, [EatInstr(110,48)]);
(399, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,447)]);
(16, [EatInstr(110,49)]);
(400, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,448)]);
(17, [EatInstr(117,50)]);
(401, [EatInstr(116,449)]);
(18, [EatInstr(111,52);EatInstr(108,51)]);
(402, [EatInstr(115,450)]);
(19, [EatInstr(101,54);EatInstr(105,53)]);
(403, [EatInstr(115,451)]);
(20, [EatInstr(101,55)]);
(404, [EatInstr(115,452)]);
(21, [EatInstr(114,56)]);
(405, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,453)]);
(22, [EatInstr(97,57)]);
(406, [EatInstr(97,454)]);
(23, [EatInstr(114,58)]);
(407, [EatInstr(97,455)]);
(24, [EatInstr(105,59)]);
(408, [EatInstr(97,456)]);
(25, [CompleteInstr(267)]);
(409, [EatInstr(45,457)]);
(26, [CompleteInstr(269)]);
(410, [EatInstr(101,458)]);
(27, [CompleteInstr(270)]);
(411, [EatInstr(97,459)]);
(28, [CompleteInstr(271)]);
(412, [EatInstr(109,460)]);
(29, [EatInstr(100,64);EatInstr(114,46);EatInstr(116,45)]);
(413, [EatInstr(97,461)]);
(30, [EatInstr(114,65)]);
(414, [EatInstr(105,462)]);
(31, [EatInstr(102,66);EatInstr(101,47)]);
(415, [EatInstr(104,463)]);
(32, [EatInstr(110,67)]);
(416, [EatInstr(97,464)]);
(33, [EatInstr(120,68)]);
(417, [EatInstr(45,465)]);
(34, [EatInstr(111,70);EatInstr(117,50);EatInstr(116,69)]);
(418, [EatInstr(115,466)]);
(35, [EatInstr(111,71);EatInstr(108,51)]);
(419, [EatInstr(97,467)]);
(36, [EatInstr(111,73);EatInstr(101,54);EatInstr(105,53);EatInstr(114,72)]);
(420, [EatInstr(103,468)]);
(37, [EatInstr(111,75);EatInstr(101,74)]);
(421, [EatInstr(108,469)]);
(38, [EatInstr(114,77);EatInstr(97,76)]);
(422, [EatInstr(101,470)]);
(39, [EatInstr(101,78)]);
(423, [EatInstr(97,471)]);
(40, [EatInstr(117,79)]);
(424, [EatInstr(101,472)]);
(41, [AAction2Instr(__a4,80)]);
(425, [AAction2Instr(__a35,269)]);
(42, [EatInstr(118,93);EatInstr(109,92);EatInstr(104,91);EatInstr(112,90);EatInstr(110,89);EatInstr(111,88);EatInstr(108,87);EatInstr(99,86);EatInstr(117,85);EatInstr(98,84);EatInstr(105,83);EatInstr(114,82);EatInstr(97,81)]);
(426, [EatInstr(105,473)]);
(43, [ACallInstr3(__default_call,4);ASimpleCont2Instr(268,__binder0,94)]);
(427, [EatInstr(101,474)]);
(44, [AAction2Instr(__a5,204)]);
(428, [EatInstr(97,475)]);
(45, [EatInstr(116,95)]);
(429, [EatInstr(45,476)]);
(46, [EatInstr(114,96)]);
(430, [AAction2Instr(__a37,80);AAction2Instr(__a36,477)]);
(47, [EatInstr(112,97)]);
(431, [EatInstr(101,478)]);
(48, [EatInstr(102,99);EatInstr(108,98)]);
(432, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,479)]);
(49, [EatInstr(114,100)]);
(433, [EatInstr(104,480)]);
(50, [EatInstr(98,101)]);
(434, [EatInstr(45,481)]);
(51, [EatInstr(111,102)]);
(435, [EatInstr(121,482)]);
(52, [EatInstr(112,103)]);
(436, [EatInstr(101,483)]);
(53, [EatInstr(102,104)]);
(437, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,484)]);
(54, [EatInstr(120,105)]);
(438, [EatInstr(101,485)]);
(55, [EatInstr(115,106)]);
(439, [EatInstr(97,486)]);
(56, [EatInstr(101,107)]);
(440, [EatInstr(116,487)]);
(57, [EatInstr(115,108)]);
(441, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,488)]);
(58, [EatInstr(97,109)]);
(442, [EatInstr(101,489)]);
(59, [EatInstr(110,110)]);
(443, [EatInstr(115,490)]);
(60, [ALookaheadInstr(false,CfgLA (3,267),61);ACallInstr3(__default_call,3);ASimpleCont2Instr(267,__binder0,60)]);
(444, [EatInstr(102,494);EatInstr(119,493);EatInstr(112,492);EatInstr(116,491)]);
(61, [CompleteInstr(268)]);
(445, [EatInstr(115,495)]);
(62, [ALookaheadInstr(false,CfgLA (3,267),63);ACallInstr3(__default_call,3);ASimpleCont2Instr(267,__binder0,62)]);
(446, [EatInstr(109,497);EatInstr(104,496)]);
(63, [CompleteInstr(272)]);
(447, [AAction2Instr(__a38,202)]);
(64, [EatInstr(100,111)]);
(448, [AAction2Instr(__a39,202)]);
(65, [EatInstr(97,112)]);
(449, [EatInstr(45,498)]);
(66, [EatInstr(99,113)]);
(450, [EatInstr(116,499)]);
(67, [EatInstr(102,114);EatInstr(108,98)]);
(451, [EatInstr(116,500)]);
(68, [EatInstr(101,116);EatInstr(116,115)]);
(452, [EatInstr(101,501)]);
(69, [EatInstr(114,117)]);
(453, [AAction2Instr(__a40,502)]);
(70, [EatInstr(114,118)]);
(454, [EatInstr(98,503)]);
(71, [EatInstr(109,120);EatInstr(112,103);EatInstr(114,119)]);
(455, [EatInstr(100,504)]);
(72, [EatInstr(49,121)]);
(456, [EatInstr(121,505)]);
(73, [EatInstr(111,122)]);
(457, [EatInstr(111,506)]);
(74, [EatInstr(112,123);EatInstr(115,106)]);
(458, [EatInstr(115,507)]);
(75, [EatInstr(116,124)]);
(459, [EatInstr(112,508)]);
(76, [EatInstr(114,125)]);
(460, [EatInstr(105,509)]);
(77, [EatInstr(101,127);EatInstr(105,126)]);
(461, [EatInstr(98,510)]);
(78, [EatInstr(116,128)]);
(462, [EatInstr(122,511)]);
(79, [EatInstr(115,129)]);
(463, [EatInstr(105,512)]);
(80, [CompleteInstr(273)]);
(464, [EatInstr(114,513)]);
(81, [EatInstr(102,131);EatInstr(114,130)]);
(465, [EatInstr(104,514)]);
(82, [EatInstr(111,132)]);
(466, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,515)]);
(83, [EatInstr(110,133)]);
(467, [EatInstr(116,516)]);
(84, [EatInstr(97,134)]);
(468, [EatInstr(117,517)]);
(85, [EatInstr(110,136);EatInstr(115,135)]);
(469, [EatInstr(108,518)]);
(86, [EatInstr(104,139);EatInstr(111,138);EatInstr(97,137)]);
(470, [EatInstr(115,519)]);
(87, [EatInstr(111,140)]);
(471, [EatInstr(114,520)]);
(88, [EatInstr(110,141)]);
(472, [EatInstr(114,521)]);
(89, [EatInstr(111,142)]);
(473, [EatInstr(108,522)]);
(90, [EatInstr(114,143)]);
(474, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,523)]);
(91, [EatInstr(121,144)]);
(475, [EatInstr(99,524)]);
(92, [EatInstr(101,145)]);
(476, [EatInstr(100,525)]);
(93, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,146)]);
(477, [ACallInstr3(__default_call,8);ASimpleCont2Instr(272,__binder0,526)]);
(94, [AAction2Instr(__a6,147)]);
(478, [EatInstr(45,527)]);
(95, [EatInstr(114,150)]);
(479, [AAction2Instr(__a41,80)]);
(96, [EatInstr(111,151)]);
(480, [EatInstr(101,528)]);
(97, [EatInstr(108,152)]);
(481, [EatInstr(97,529)]);
(98, [EatInstr(105,153)]);
(482, [EatInstr(45,530)]);
(99, [EatInstr(101,154)]);
(483, [EatInstr(118,531)]);
(100, [EatInstr(111,155)]);
(484, [AAction2Instr(__a42,80)]);
(101, [EatInstr(115,156)]);
(485, [EatInstr(45,532);ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,523)]);
(102, [EatInstr(115,157)]);
(486, [EatInstr(116,533)]);
(103, [EatInstr(121,158)]);
(487, [EatInstr(97,534)]);
(104, [EatInstr(116,159)]);
(488, [AAction2Instr(__a43,202)]);
(105, [EatInstr(101,160)]);
(489, [EatInstr(103,535)]);
(106, [EatInstr(117,161)]);
(490, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,536)]);
(107, [EatInstr(99,162)]);
(491, [EatInstr(120,537)]);
(108, [EatInstr(104,163)]);
(492, [EatInstr(101,538)]);
(109, [EatInstr(112,164)]);
(493, [EatInstr(97,539)]);
(110, [EatInstr(117,165)]);
(494, [EatInstr(117,540)]);
(111, [EatInstr(45,166)]);
(495, [EatInstr(101,541)]);
(112, [EatInstr(110,167)]);
(496, [EatInstr(45,542)]);
(113, [AAction2Instr(__a7,168)]);
(497, [EatInstr(45,543)]);
(114, [EatInstr(111,169);EatInstr(101,154)]);
(498, [EatInstr(115,544)]);
(115, [EatInstr(114,170)]);
(499, [EatInstr(97,545)]);
(116, [EatInstr(99,171)]);
(500, [EatInstr(111,546)]);
(117, [EatInstr(105,172)]);
(501, [EatInstr(110,547)]);
(118, [EatInstr(116,173)]);
(502, [ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,588)]);
(119, [EatInstr(111,174)]);
(503, [EatInstr(101,548)]);
(120, [EatInstr(112,175)]);
(504, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,549)]);
(121, [EatInstr(45,176)]);
(505, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,550)]);
(122, [EatInstr(107,177)]);
(506, [EatInstr(112,551)]);
(123, [EatInstr(101,178)]);
(507, [EatInstr(99,552)]);
(124, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,179)]);
(508, [EatInstr(115,553)]);
(125, [EatInstr(115,180)]);
(509, [EatInstr(122,554)]);
(126, [EatInstr(110,181)]);
(510, [EatInstr(105,555)]);
(127, [EatInstr(99,182)]);
(511, [EatInstr(101,556)]);
(128, [EatInstr(45,183)]);
(512, [EatInstr(115,557)]);
(129, [EatInstr(101,184)]);
(513, [EatInstr(114,558)]);
(130, [EatInstr(114,185)]);
(514, [EatInstr(105,559)]);
(131, [EatInstr(116,186)]);
(515, [AAction2Instr(__a44,269)]);
(132, [EatInstr(111,187)]);
(516, [EatInstr(105,560)]);
(133, [EatInstr(108,188)]);
(517, [EatInstr(108,561)]);
(134, [EatInstr(99,189)]);
(518, [EatInstr(97,562)]);
(135, [EatInstr(101,190)]);
(519, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,563)]);
(136, [EatInstr(105,192);EatInstr(114,191)]);
(520, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,564)]);
(137, [EatInstr(115,193)]);
(521, [EatInstr(45,565)]);
(138, [EatInstr(117,194)]);
(522, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,566)]);
(139, [EatInstr(101,195)]);
(523, [AAction2Instr(__a45,269)]);
(140, [EatInstr(111,196)]);
(524, [EatInstr(116,567)]);
(141, [EatInstr(108,197)]);
(525, [EatInstr(121,568)]);
(142, [EatInstr(45,198)]);
(526, [AAction2Instr(__a46,381)]);
(143, [EatInstr(101,199)]);
(527, [EatInstr(97,569)]);
(144, [EatInstr(98,200)]);
(528, [EatInstr(97,570)]);
(145, [EatInstr(109,201)]);
(529, [EatInstr(110,571)]);
(146, [AAction2Instr(__a8,202)]);
(530, [EatInstr(103,572)]);
(147, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,203)]);
(531, [EatInstr(97,573)]);
(148, [AAction2Instr(__a9,204)]);
(532, [EatInstr(97,574)]);
(149, [CompleteInstr(275)]);
(533, [EatInstr(111,575)]);
(150, [EatInstr(105,205)]);
(534, [EatInstr(116,576)]);
(151, [EatInstr(119,206)]);
(535, [EatInstr(117,577)]);
(152, [EatInstr(97,207)]);
(536, [AAction2Instr(__a47,202)]);
(153, [EatInstr(110,208)]);
(537, [AAction2Instr(__a48,706)]);
(154, [EatInstr(114,209)]);
(538, [EatInstr(103,578)]);
(155, [EatInstr(108,210)]);
(539, [EatInstr(100,579)]);
(156, [EatInstr(101,211)]);
(540, [EatInstr(110,580)]);
(157, [EatInstr(101,212)]);
(541, [EatInstr(45,581)]);
(158, [EatInstr(114,213)]);
(542, [EatInstr(115,582)]);
(159, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,214)]);
(543, [EatInstr(115,583)]);
(160, [EatInstr(114,215)]);
(544, [EatInstr(101,584)]);
(161, [EatInstr(103,216)]);
(545, [EatInstr(114,585)]);
(162, [EatInstr(101,217)]);
(546, [EatInstr(114,586)]);
(163, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,218)]);
(547, [EatInstr(115,587)]);
(164, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,219)]);
(548, [EatInstr(108,590)]);
(165, [EatInstr(115,220)]);
(549, [AAction2Instr(__a49,202)]);
(166, [EatInstr(108,221)]);
(550, [AAction2Instr(__a50,202)]);
(167, [EatInstr(115,222)]);
(551, [EatInstr(116,591)]);
(168, [ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,276)]);
(552, [EatInstr(101,592)]);
(169, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,223)]);
(553, [EatInstr(101,593)]);
(170, [EatInstr(97,224)]);
(554, [EatInstr(101,594)]);
(171, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,225)]);
(555, [EatInstr(108,595)]);
(172, [EatInstr(112,226)]);
(556, [EatInstr(45,596)]);
(173, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,227)]);
(557, [EatInstr(116,597)]);
(174, [EatInstr(117,228)]);
(558, [EatInstr(111,598)]);
(175, [EatInstr(105,229)]);
(559, [EatInstr(115,599)]);
(176, [EatInstr(108,230)]);
(560, [EatInstr(111,600)]);
(177, [EatInstr(97,231)]);
(561, [EatInstr(97,601)]);
(178, [EatInstr(110,232)]);
(562, [EatInstr(98,602)]);
(179, [AAction2Instr(__a10,80)]);
(563, [AAction2Instr(__a51,269)]);
(180, [EatInstr(101,233)]);
(564, [AAction2Instr(__a52,269)]);
(181, [EatInstr(116,234)]);
(565, [EatInstr(99,603)]);
(182, [EatInstr(101,235)]);
(566, [AAction2Instr(__a53,269)]);
(183, [EatInstr(103,236)]);
(567, [EatInstr(105,604)]);
(184, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,237)]);
(568, [EatInstr(112,605)]);
(185, [EatInstr(111,238)]);
(569, [EatInstr(99,606)]);
(186, [EatInstr(101,239)]);
(570, [EatInstr(100,607)]);
(187, [EatInstr(116,240)]);
(571, [EatInstr(97,608)]);
(188, [EatInstr(105,241)]);
(572, [EatInstr(114,609)]);
(189, [EatInstr(107,242)]);
(573, [EatInstr(110,610)]);
(190, [EatInstr(45,243)]);
(574, [EatInstr(110,611)]);
(191, [EatInstr(111,244)]);
(575, [EatInstr(114,612)]);
(192, [EatInstr(116,245)]);
(576, [EatInstr(105,613)]);
(193, [EatInstr(101,246)]);
(577, [EatInstr(108,614)]);
(194, [EatInstr(110,247)]);
(578, [EatInstr(45,616);AAction2Instr(__a54,706)]);
(195, [EatInstr(99,248)]);
(579, [EatInstr(108,617)]);
(196, [EatInstr(107,249)]);
(580, [AAction2Instr(__a55,706)]);
(197, [EatInstr(121,250)]);
(581, [EatInstr(115,618)]);
(198, [EatInstr(109,256);EatInstr(110,255);EatInstr(111,254);EatInstr(99,253);EatInstr(115,252);EatInstr(114,251)]);
(582, [EatInstr(101,619)]);
(199, [EatInstr(102,257)]);
(583, [EatInstr(101,620)]);
(200, [EatInstr(114,258)]);
(584, [EatInstr(116,621)]);
(201, [EatInstr(111,259)]);
(585, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,622)]);
(202, [CompleteInstr(274)]);
(586, [EatInstr(121,623)]);
(203, [AAction2Instr(__a11,202)]);
(587, [EatInstr(105,624)]);
(204, [ALookaheadInstr(false,CfgLA (5,269),149);AContInstr3(274,__g2,__binder3,148);ACallInstr3(__g2,10);ACallInstr3(__default_call,6);ASimpleCont2Instr(270,__binder0,149)]);
(588, [AAction2Instr(__a56,589);ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,588)]);
(205, [EatInstr(98,260)]);
(589, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,625)]);
(206, [EatInstr(45,261)]);
(590, [EatInstr(115,626)]);
(207, [EatInstr(121,262)]);
(591, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,627)]);
(208, [EatInstr(101,263)]);
(592, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,628)]);
(209, [EatInstr(45,264)]);
(593, [EatInstr(45,629)]);
(210, [EatInstr(108,265)]);
(594, [EatInstr(45,630)]);
(211, [EatInstr(116,266)]);
(595, [EatInstr(105,631)]);
(212, [EatInstr(45,267)]);
(596, [EatInstr(104,632)]);
(213, [EatInstr(117,268)]);
(597, [EatInstr(111,633)]);
(214, [AAction2Instr(__a12,269)]);
(598, [EatInstr(119,634)]);
(215, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,270)]);
(599, [EatInstr(116,635)]);
(216, [EatInstr(97,271)]);
(600, [EatInstr(110,636)]);
(217, [EatInstr(100,272)]);
(601, [EatInstr(114,637)]);
(218, [AAction2Instr(__a13,269)]);
(602, [EatInstr(108,638)]);
(219, [AAction2Instr(__a14,269)]);
(603, [EatInstr(111,639)]);
(220, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,273)]);
(604, [EatInstr(111,640)]);
(221, [EatInstr(97,274)]);
(605, [EatInstr(103,641)]);
(222, [EatInstr(108,275)]);
(606, [EatInstr(116,642)]);
(223, [AAction2Instr(__a15,80)]);
(607, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,643)]);
(224, [EatInstr(99,278)]);
(608, [EatInstr(108,644)]);
(225, [AAction2Instr(__a16,279)]);
(609, [EatInstr(97,645)]);
(226, [EatInstr(45,280)]);
(610, [EatInstr(99,646)]);
(227, [AAction2Instr(__a17,80)]);
(611, [EatInstr(97,647)]);
(228, [EatInstr(116,281)]);
(612, [EatInstr(115,648)]);
(229, [EatInstr(108,282)]);
(613, [EatInstr(111,649)]);
(230, [EatInstr(111,283)]);
(614, [EatInstr(97,650)]);
(231, [EatInstr(104,284)]);
(615, [AAction2Instr(__a57,202)]);
(232, [EatInstr(100,285)]);
(616, [EatInstr(115,651)]);
(233, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,286)]);
(617, [EatInstr(101,652)]);
(234, [EatInstr(45,288);ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,287)]);
(618, [EatInstr(101,653)]);
(235, [EatInstr(100,289)]);
(619, [EatInstr(116,654)]);
(236, [EatInstr(101,290)]);
(620, [EatInstr(116,655)]);
(237, [AAction2Instr(__a18,80)]);
(621, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,656)]);
(238, [EatInstr(119,291)]);
(622, [AAction2Instr(__a58,657)]);
(239, [EatInstr(114,292)]);
(623, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,658)]);
(240, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,293)]);
(624, [EatInstr(116,659)]);
(241, [EatInstr(110,294)]);
(625, [AAction2Instr(__a59,202)]);
(242, [EatInstr(101,295)]);
(626, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,660)]);
(243, [EatInstr(102,298);EatInstr(104,297);EatInstr(115,296)]);
(627, [AAction2Instr(__a60,202)]);
(244, [EatInstr(108,299)]);
(628, [AAction2Instr(__a61,202)]);
(245, [EatInstr(45,300)]);
(629, [EatInstr(99,661)]);
(246, [EatInstr(45,301)]);
(630, [EatInstr(112,662)]);
(247, [EatInstr(116,302)]);
(631, [EatInstr(116,663)]);
(248, [EatInstr(107,303)]);
(632, [EatInstr(105,664)]);
(249, [EatInstr(97,304)]);
(633, [EatInstr(114,665)]);
(250, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,305)]);
(634, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,666)]);
(251, [EatInstr(101,306)]);
(635, [EatInstr(111,667)]);
(252, [EatInstr(107,307)]);
(636, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,668)]);
(253, [EatInstr(111,308)]);
(637, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,669)]);
(254, [EatInstr(112,309)]);
(638, [EatInstr(101,670)]);
(255, [EatInstr(117,310)]);
(639, [EatInstr(114,671)]);
(256, [EatInstr(101,311)]);
(640, [EatInstr(110,672)]);
(257, [EatInstr(105,312)]);
(641, [EatInstr(101,673)]);
(258, [EatInstr(105,313)]);
(642, [EatInstr(105,674)]);
(259, [EatInstr(105,314)]);
(643, [AAction2Instr(__a62,80)]);
(260, [EatInstr(117,315)]);
(644, [EatInstr(121,675)]);
(261, [EatInstr(110,316)]);
(645, [EatInstr(112,676)]);
(262, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,317)]);
(646, [EatInstr(101,677)]);
(263, [EatInstr(45,318)]);
(647, [EatInstr(108,678)]);
(264, [EatInstr(116,319)]);
(648, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,679)]);
(265, [EatInstr(45,320)]);
(649, [EatInstr(110,680)]);
(266, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,321)]);
(650, [EatInstr(114,681)]);
(267, [EatInstr(117,322)]);
(651, [EatInstr(116,682)]);
(268, [EatInstr(108,323)]);
(652, [EatInstr(114,683)]);
(269, [CompleteInstr(266)]);
(653, [EatInstr(116,684)]);
(270, [AAction2Instr(__a19,269)]);
(654, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,685)]);
(271, [EatInstr(114,325)]);
(655, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,686)]);
(272, [EatInstr(101,326)]);
(656, [AAction2Instr(__a63,202)]);
(273, [AAction2Instr(__a20,269)]);
(657, [ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,708)]);
(274, [EatInstr(116,327)]);
(658, [AAction2Instr(__a64,202)]);
(275, [EatInstr(97,328)]);
(659, [EatInstr(105,687)]);
(276, [AAction2Instr(__a21,277);ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,276)]);
(660, [AAction2Instr(__a65,202)]);
(277, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,329)]);
(661, [EatInstr(97,688)]);
(278, [EatInstr(116,330)]);
(662, [EatInstr(97,689)]);
(279, [ACallInstr3(__default_call,8);ASimpleCont2Instr(272,__binder0,331)]);
(663, [EatInstr(121,690)]);
(280, [EatInstr(108,332)]);
(664, [EatInstr(115,691)]);
(281, [EatInstr(105,333)]);
(665, [EatInstr(121,692)]);
(282, [EatInstr(101,334)]);
(666, [AAction2Instr(__a66,202)]);
(283, [EatInstr(111,335)]);
(667, [EatInstr(114,693)]);
(284, [EatInstr(101,336)]);
(668, [AAction2Instr(__a67,269)]);
(285, [EatInstr(101,337)]);
(669, [AAction2Instr(__a68,269)]);
(286, [AAction2Instr(__a22,80)]);
(670, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,694)]);
(287, [AAction2Instr(__a23,80)]);
(671, [EatInstr(101,695)]);
(288, [EatInstr(103,339);EatInstr(114,338)]);
(672, [EatInstr(115,696)]);
(289, [EatInstr(101,340)]);
(673, [EatInstr(110,697)]);
(290, [EatInstr(110,341)]);
(674, [EatInstr(111,698)]);
(291, [EatInstr(45,342)]);
(675, [EatInstr(115,699)]);
(292, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,343)]);
(676, [EatInstr(104,700)]);
(293, [AAction2Instr(__a24,344)]);
(677, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,701)]);
(294, [EatInstr(101,345)]);
(678, [EatInstr(121,702)]);
(295, [EatInstr(110,346)]);
(679, [AAction2Instr(__a69,80)]);
(296, [EatInstr(112,347)]);
(680, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,703)]);
(297, [EatInstr(105,348)]);
(681, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,704)]);
(298, [EatInstr(108,350);EatInstr(115,349)]);
(682, [EatInstr(114,705)]);
(299, [EatInstr(108,351)]);
(683, [AAction2Instr(__a70,706)]);
(300, [EatInstr(104,352)]);
(684, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,707)]);
(301, [EatInstr(105,353)]);
(685, [AAction2Instr(__a71,202)]);
(302, [EatInstr(101,354)]);
(686, [AAction2Instr(__a72,202)]);
(303, [EatInstr(45,355)]);
(687, [EatInstr(118,710)]);
(304, [EatInstr(104,356)]);
(688, [EatInstr(108,711)]);
(305, [AAction2Instr(__a25,202)]);
(689, [EatInstr(109,712)]);
(306, [EatInstr(112,357)]);
(690, [EatInstr(45,713)]);
(307, [EatInstr(105,358)]);
(691, [EatInstr(116,714)]);
(308, [EatInstr(108,360);EatInstr(97,359)]);
(692, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,715)]);
(309, [EatInstr(116,361)]);
(693, [EatInstr(121,716)]);
(310, [EatInstr(108,362)]);
(694, [AAction2Instr(__a73,269)]);
(311, [EatInstr(109,363)]);
(695, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,717)]);
(312, [EatInstr(120,364)]);
(696, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,718)]);
(313, [EatInstr(100,365)]);
(697, [EatInstr(45,720);ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,719)]);
(314, [EatInstr(122,366)]);
(698, [EatInstr(110,721)]);
(315, [EatInstr(116,367)]);
(699, [EatInstr(105,722)]);
(316, [EatInstr(111,368)]);
(700, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,723)]);
(317, [AAction2Instr(__a26,269)]);
(701, [AAction2Instr(__a74,80)]);
(318, [EatInstr(110,370);EatInstr(114,369)]);
(702, [EatInstr(115,724)]);
(319, [EatInstr(121,371)]);
(703, [AAction2Instr(__a75,202)]);
(320, [EatInstr(115,372)]);
(704, [AAction2Instr(__a76,202)]);
(321, [AAction2Instr(__a27,269)]);
(705, [EatInstr(105,725)]);
(322, [EatInstr(110,373)]);
(706, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,615)]);
(323, [EatInstr(101,374)]);
(707, [AAction2Instr(__a77,202)]);
(708, [AAction2Instr(__a78,709);ACallInstr3(__default_call,7);ASimpleCont2Instr(271,__binder0,708)]);
(325, [EatInstr(45,376);ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,375)]);
(709, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,726)]);
(326, [EatInstr(110,377)]);
(710, [EatInstr(101,727)]);
(327, [EatInstr(101,378)]);
(711, [EatInstr(108,728)]);
(328, [EatInstr(116,379)]);
(712, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,729)]);
(329, [AAction2Instr(__a28,80)]);
(713, [EatInstr(112,730)]);
(330, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,380)]);
(714, [EatInstr(111,731)]);
(331, [AAction2Instr(__a29,381)]);
(715, [AAction2Instr(__a79,202)]);
(332, [EatInstr(97,382)]);
(716, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,732)]);
(333, [EatInstr(110,383)]);
(717, [AAction2Instr(__a80,269)]);
(334, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,384)]);
(718, [AAction2Instr(__a81,80)]);
(335, [EatInstr(107,385)]);
(719, [AAction2Instr(__a82,80)]);
(336, [EatInstr(97,386)]);
(720, [EatInstr(115,733)]);
(337, [EatInstr(110,387)]);
(721, [EatInstr(115,734)]);
(338, [EatInstr(101,388)]);
(722, [EatInstr(115,735)]);
(339, [EatInstr(105,389)]);
(723, [AAction2Instr(__a83,80)]);
(340, [EatInstr(110,390)]);
(724, [EatInstr(105,736)]);
(341, [EatInstr(101,391)]);
(725, [EatInstr(99,737)]);
(342, [EatInstr(110,392)]);
(726, [AAction2Instr(__a84,202)]);
(343, [ACallInstr3(__g2,2);AContInstr3(266,__g2,__binder4,393)]);
(727, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,738)]);
(344, [ACallInstr3(__default_call,8);ASimpleCont2Instr(272,__binder0,394)]);
(728, [EatInstr(115,739)]);
(345, [EatInstr(45,395)]);
(729, [AAction2Instr(__a85,202)]);
(346, [EatInstr(100,396)]);
(730, [EatInstr(114,740)]);
(347, [EatInstr(97,397)]);
(731, [EatInstr(114,741)]);
(348, [EatInstr(101,398)]);
(732, [AAction2Instr(__a86,202)]);
(349, [EatInstr(109,400);EatInstr(116,399)]);
(733, [EatInstr(99,742)]);
(350, [EatInstr(97,401)]);
(734, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,743)]);
(351, [EatInstr(45,402)]);
(735, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,744)]);
(352, [EatInstr(105,403)]);
(736, [EatInstr(115,745)]);
(353, [EatInstr(110,404)]);
(737, [EatInstr(116,746)]);
(354, [EatInstr(114,405)]);
(738, [AAction2Instr(__a87,202)]);
(355, [EatInstr(108,406)]);
(739, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,747)]);
(356, [EatInstr(101,407)]);
(740, [EatInstr(101,748)]);
(357, [EatInstr(108,408)]);
(741, [EatInstr(121,749)]);
(358, [EatInstr(112,409)]);
(742, [EatInstr(97,750)]);
(359, [EatInstr(108,410)]);
(743, [AAction2Instr(__a88,80)]);
(360, [EatInstr(108,411)]);
(744, [AAction2Instr(__a89,80)]);
(361, [EatInstr(105,412)]);
(745, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,751)]);
(362, [EatInstr(108,413)]);
(746, [AAction2Instr(__a90,706)]);
(363, [EatInstr(111,414)]);
(747, [AAction2Instr(__a91,202)]);
(364, [EatInstr(45,415)]);
(748, [EatInstr(100,752)]);
(365, [EatInstr(45,416)]);
(749, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,753)]);
(366, [EatInstr(101,417)]);
(750, [EatInstr(110,754)]);
(367, [EatInstr(101,418)]);
(751, [AAction2Instr(__a92,80)]);
(368, [EatInstr(116,419)]);
(752, [EatInstr(115,755)]);
(369, [EatInstr(101,420)]);
(753, [AAction2Instr(__a93,202)]);
(370, [EatInstr(117,421)]);
(754, [EatInstr(110,756)]);
(371, [EatInstr(112,422)]);
(755, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,757)]);
(372, [EatInstr(116,423)]);
(756, [EatInstr(101,758)]);
(373, [EatInstr(100,424)]);
(757, [AAction2Instr(__a94,202)]);
(374, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,425)]);
(758, [EatInstr(114,759)]);
(375, [AAction2Instr(__a30,269)]);
(759, [EatInstr(108,760)]);
(376, [EatInstr(103,426)]);
(760, [EatInstr(101,761)]);
(377, [EatInstr(99,427)]);
(761, [EatInstr(115,762)]);
(378, [EatInstr(45,428)]);
(762, [EatInstr(115,763)]);
(379, [EatInstr(101,429)]);
(763, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,764)]);
(380, [AAction2Instr(__a31,80)]);
(764, [AAction2Instr(__a95,80)]);
(381, [ACallInstr3(__default_call,1);ASimpleCont2Instr(265,__binder0,430)]);
(382, [EatInstr(116,431)]);
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
