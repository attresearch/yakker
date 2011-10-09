
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
let __a55 = (_p 1084 ((2054)));;
let __a54 = (_p 1080 ((2064)));;
let __a47 = (_p 1076 ((2067)));;
let __a21 = (_p 1028 ((2045)));;
let __a83 = (_p 1024 ((2048)));;
let __a4 = (_p 1055 ((2089)));;
let __a28 = (_p 1051 ((2021)));;
let __a46 = (_p 1078 ((2065)));;
let __a15 = (_p 1026 ((2046)));;
let __a61 = (_p 1082 ((2061)));;
let __a73 = (_p 1061 ((2083)));;
let __a69 = (_p 1030 ((2042)));;
let __a7 = (_p 1057 ((2086)));;
let __a53 = (_p 1085 ((2059)));;
let __a20 = (_p 1032 ((2040)));;
let __a37 = (_p 1059 ((2084)));;
let __a16 = (_p 1005 ((2018)));;
let __a67 = (_p 1087 ((2057)));;
let __a27 = (_p 1007 ((2016)));;
let __a52 = (_p 1089 ((2055)));;
let __a18 = (_p 1009 ((2014)));;
let __a63 = (_p 1063 ((2080)));;
let __a86 = (_p 1033 ((2039)));;
let __a17 = (_p 1011 ((2012)));;
let __a66 = (_p 1013 ((2010)));;
let __a60 = (_p 1035 ((2037)));;
let __a23 = (_p 1066 ((2078)));;
let __a13 = (_p 1037 ((2035)));;
let __a70 = (_p 1091 ((2052)));;
let __a11 = (_p 1039 ((2033)));;
let __a35 = (_p 1041 ((2025)));;
let __a50 = (_p 1014 ((2009)));;
let __a22 = (_p 1068 ((2075)));;
let __a12 = (_p 1016 ((2007)));;
let __a48 = (_p 1070 ((2073)));;
let __a29 = (_p 1018 ((2005)));;
let __a87 = (_p 1072 ((2071)));;
let __a75 = (_p 1020 ((2003)));;
let __a41 = (_p 1064 ((2077)));;
let __a88 = (_p 1022 ((2050)));;
let __a2 = (_p 1002 ((2001)));;
let __a8 = (_p 1048 ((2024)));;
let __a78 = (_p 1075 ((2068)));;
let __a77 = (_p 1023 ((2049)));;
let __a71 = (_p 1077 ((2066)));;
let __a5 = (_p 1029 ((2044)));;
let __a36 = (_p 1081 ((2063)));;
let __a39 = (_p 1050 ((2022)));;
let __a74 = (_p 1025 ((2047)));;
let __a3 = (_p 1052 ((2020)));;
let __a1 = (_p 1056 ((2088)));;
let __a81 = (_p 1083 ((2060)));;
let __a59 = (_p 1062 ((2082)));;
let __a25 = (_p 1004 ((2019)));;
let __a6 = (_p 1000 ((2000)));;
let __a40 = (_p 1031 ((2041)));;
let __a38 = (_p 1058 ((2085)));;
let __a84 = (_p 1086 ((2058)));;
let __a51 = (_p 1006 ((2017)));;
let __a56 = (_p 1079 ((2062)));;
let __a45 = (_p 1088 ((2056)));;
let __a44 = (_p 1008 ((2015)));;
let __a26 = (_p 1027 ((2043)));;
let __a9 = (_p 1054 ((2087)));;
let __a14 = (_p 1010 ((2013)));;
let __a68 = (_p 1012 ((2011)));;
let __a24 = (fun _x0_ _x1_ -> (((_p 1045 ((2029))) _x0_) (((_p 1046 ((2027))) _x0_) _x1_)));;
let __a32 = (_p 1065 ((2079)));;
let __a34 = (_p 1044 ((2030)));;
let __a19 = (_p 1034 ((2038)));;
let __a82 = (_p 1036 ((2036)));;
let __a65 = (_p 1038 ((2034)));;
let __a62 = (_p 1090 ((2053)));;
let __a80 = (_p 1060 ((2081)));;
let __a31 = (_p 1092 ((2051)));;
let __a72 = (_p 1067 ((2076)));;
let __a30 = (_p 1040 ((2032)));;
let __a64 = (_p 1015 ((2008)));;
let __a58 = (_p 1069 ((2074)));;
let __g0 = (_e);;
let __a49 = (_p 1017 ((2006)));;
let __a79 = (_p 1071 ((2072)));;
let __a33 = (_p 1019 ((2004)));;
let __a85 = (_p 1073 ((2070)));;
let __a10 = (_p 1047 ((2026)));;
let __a43 = (fun _x0_ _x1_ -> (((_p 1042 ((2028))) _x0_) (((_p 1043 ((2031))) _x0_) _x1_)));;
let __a42 = (_p 1021 ((2002)));;
let __a76 = (_p 1049 ((2023)));;
let __a57 = (_p 1074 ((2069)));;
let __binder0 = __default_ret;;
let __binder1 = (_m 1003);;
let __binder2 = (_m 1053);;
let __binder3 = (_m 1001);;
let __binder4 = (_m 1093);;
open Yak.Pam_internal
let program = [
(383, [EatInstr(99,427)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [AAction2Instr(__a31,203)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(111,428)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,429)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(115,430)]);
(4, [EatInstr(119,30);EatInstr(117,29);EatInstr(116,28);EatInstr(115,27);EatInstr(114,26);EatInstr(112,25);EatInstr(109,24);EatInstr(108,23);EatInstr(105,22);EatInstr(104,21);EatInstr(103,20);EatInstr(102,19);EatInstr(101,18);EatInstr(100,17);EatInstr(99,16);EatInstr(97,15);AContInstr3(271,__g0,__binder2,32);AContInstr3(272,__g0,__binder1,31)]);
(388, [EatInstr(97,431)]);
(5, [EatInstr(0,33)]);
(389, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,432)]);
(6, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12);ASimpleCont2Instr(264,__binder0,73)]);
(390, [EatInstr(97,433)]);
(7, [EatInstr(127,75);EatInstr(126,75);EatInstr(125,75);EatInstr(124,75);EatInstr(123,75);EatInstr(122,75);EatInstr(121,75);EatInstr(120,75);EatInstr(119,75);EatInstr(118,75);EatInstr(117,75);EatInstr(116,75);EatInstr(115,75);EatInstr(114,75);EatInstr(113,75);EatInstr(112,75);EatInstr(111,75);EatInstr(110,75);EatInstr(109,75);EatInstr(108,75);EatInstr(107,75);EatInstr(106,75);EatInstr(105,75);EatInstr(104,75);EatInstr(103,75);EatInstr(102,75);EatInstr(101,75);EatInstr(100,75);EatInstr(99,75);EatInstr(98,75);EatInstr(97,75);EatInstr(96,75);EatInstr(95,75);EatInstr(94,75);EatInstr(93,75);EatInstr(92,75);EatInstr(91,75);EatInstr(90,75);EatInstr(89,75);EatInstr(88,75);EatInstr(87,75);EatInstr(86,75);EatInstr(85,75);EatInstr(84,75);EatInstr(83,75);EatInstr(82,75);EatInstr(81,75);EatInstr(80,75);EatInstr(79,75);EatInstr(78,75);EatInstr(77,75);EatInstr(76,75);EatInstr(75,75);EatInstr(74,75);EatInstr(73,75);EatInstr(72,75);EatInstr(71,75);EatInstr(70,75);EatInstr(69,75);EatInstr(68,75);EatInstr(67,75);EatInstr(66,75);EatInstr(65,75);EatInstr(64,75);EatInstr(63,75);EatInstr(62,75);EatInstr(61,75);EatInstr(60,75);EatInstr(59,75);EatInstr(58,75);EatInstr(57,75);EatInstr(56,75);EatInstr(55,75);EatInstr(54,75);EatInstr(53,75);EatInstr(52,75);EatInstr(51,75);EatInstr(50,75);EatInstr(49,75);EatInstr(48,75);EatInstr(47,75);EatInstr(46,75);EatInstr(44,75);EatInstr(43,75);EatInstr(42,75);EatInstr(41,75);EatInstr(40,75);EatInstr(39,75);EatInstr(38,75);EatInstr(37,75);EatInstr(36,75);EatInstr(35,75);EatInstr(34,75);EatInstr(33,75);EatInstr(32,75);EatInstr(31,75);EatInstr(30,75);EatInstr(29,75);EatInstr(28,75);EatInstr(27,75);EatInstr(26,75);EatInstr(25,75);EatInstr(24,75);EatInstr(23,75);EatInstr(22,75);EatInstr(21,75);EatInstr(20,75);EatInstr(19,75);EatInstr(18,75);EatInstr(17,75);EatInstr(16,75);EatInstr(15,75);EatInstr(14,75);EatInstr(13,75);EatInstr(12,75);EatInstr(11,75);EatInstr(10,75);EatInstr(9,75);EatInstr(8,75);EatInstr(7,75);EatInstr(6,75);EatInstr(5,75);EatInstr(4,75);EatInstr(3,75);EatInstr(2,75);EatInstr(1,75)]);
(391, [EatInstr(114,435);EatInstr(99,434)]);
(8, [EatInstr(119,30);EatInstr(117,29);EatInstr(115,41);EatInstr(114,40);EatInstr(112,39);EatInstr(109,24);EatInstr(108,38);EatInstr(105,37);EatInstr(104,21);EatInstr(100,36);EatInstr(99,35);EatInstr(97,34)]);
(392, [EatInstr(97,436)]);
(9, [EatInstr(119,30);EatInstr(117,29);EatInstr(116,28);EatInstr(115,27);EatInstr(114,26);EatInstr(112,25);EatInstr(109,24);EatInstr(108,23);EatInstr(105,22);EatInstr(104,21);EatInstr(103,20);EatInstr(102,19);EatInstr(101,18);EatInstr(100,17);EatInstr(99,16);EatInstr(97,15);AContInstr3(271,__g0,__binder2,32)]);
(393, [EatInstr(45,437)]);
(10, [EatInstr(45,42);AAction2Instr(__a1,43)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),44)]);
(394, [EatInstr(101,438)]);
(395, [EatInstr(97,439)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(105,440)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(109,441)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(97,442)]);
(15, [EatInstr(116,47);EatInstr(114,46);EatInstr(100,45)]);
(399, [EatInstr(45,443)]);
(16, [EatInstr(111,49);EatInstr(108,48)]);
(400, [EatInstr(104,444)]);
(17, [EatInstr(111,51);EatInstr(101,50)]);
(401, [AAction2Instr(__a32,445)]);
(18, [EatInstr(120,52)]);
(402, [EatInstr(115,446)]);
(19, [EatInstr(117,53)]);
(403, [EatInstr(115,447)]);
(20, [EatInstr(101,54)]);
(404, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,448)]);
(21, [EatInstr(97,55)]);
(405, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,449)]);
(22, [EatInstr(110,56)]);
(406, [EatInstr(97,450)]);
(23, [EatInstr(114,60);EatInstr(111,59);EatInstr(105,58);EatInstr(101,57)]);
(407, [EatInstr(97,451)]);
(24, [EatInstr(105,61)]);
(408, [EatInstr(115,452)]);
(25, [EatInstr(114,63);EatInstr(97,62)]);
(409, [EatInstr(101,453)]);
(26, [EatInstr(102,65);EatInstr(101,64)]);
(410, [AAction2Instr(__a33,271)]);
(27, [EatInstr(117,68);EatInstr(116,67);EatInstr(111,66)]);
(411, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,454)]);
(28, [EatInstr(114,69)]);
(412, [EatInstr(121,455)]);
(29, [EatInstr(110,70)]);
(413, [EatInstr(105,456)]);
(30, [EatInstr(114,71)]);
(414, [AAction2Instr(__a35,72);AAction2Instr(__a34,457)]);
(31, [AAction2Instr(__a2,184)]);
(415, [EatInstr(97,458)]);
(32, [AAction2Instr(__a3,72)]);
(416, [EatInstr(101,459)]);
(33, [CompleteInstr(268)]);
(417, [EatInstr(108,460)]);
(34, [EatInstr(116,47);EatInstr(114,46)]);
(418, [EatInstr(103,461)]);
(35, [EatInstr(111,77);EatInstr(108,48)]);
(419, [EatInstr(45,462)]);
(36, [EatInstr(101,78)]);
(420, [EatInstr(104,463)]);
(37, [EatInstr(110,79)]);
(421, [EatInstr(101,464)]);
(38, [EatInstr(105,58);EatInstr(101,57)]);
(422, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,465)]);
(39, [EatInstr(114,80)]);
(423, [EatInstr(101,466)]);
(40, [EatInstr(101,64)]);
(424, [EatInstr(101,467)]);
(41, [EatInstr(117,68)]);
(425, [EatInstr(45,468)]);
(42, [EatInstr(118,93);EatInstr(117,92);EatInstr(114,91);EatInstr(112,90);EatInstr(111,89);EatInstr(110,88);EatInstr(109,87);EatInstr(108,86);EatInstr(105,85);EatInstr(104,84);EatInstr(99,83);EatInstr(98,82);EatInstr(97,81)]);
(426, [EatInstr(97,469)]);
(43, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,94)]);
(427, [EatInstr(101,470)]);
(44, [CompleteInstr(274)]);
(428, [EatInstr(116,471)]);
(45, [EatInstr(100,95)]);
(429, [EatInstr(119,475);EatInstr(116,474);EatInstr(112,473);EatInstr(102,472)]);
(46, [EatInstr(114,96)]);
(430, [EatInstr(101,476)]);
(47, [EatInstr(116,97)]);
(431, [EatInstr(98,477)]);
(48, [EatInstr(111,98)]);
(432, [AAction2Instr(__a36,478)]);
(49, [EatInstr(114,101);EatInstr(112,100);EatInstr(109,99)]);
(433, [EatInstr(114,479)]);
(50, [EatInstr(115,103);EatInstr(112,102)]);
(434, [EatInstr(115,480)]);
(51, [EatInstr(116,104)]);
(435, [EatInstr(101,481)]);
(52, [EatInstr(116,106);EatInstr(101,105)]);
(436, [EatInstr(100,482)]);
(53, [EatInstr(115,107)]);
(437, [EatInstr(104,483)]);
(54, [EatInstr(116,108)]);
(438, [EatInstr(115,484)]);
(55, [EatInstr(115,109)]);
(439, [EatInstr(112,485)]);
(56, [EatInstr(108,111);EatInstr(102,110)]);
(440, [EatInstr(122,486)]);
(57, [EatInstr(120,112)]);
(441, [EatInstr(105,487)]);
(58, [EatInstr(102,113)]);
(442, [EatInstr(121,488)]);
(59, [EatInstr(111,114)]);
(443, [EatInstr(111,489)]);
(60, [EatInstr(49,115)]);
(444, [EatInstr(105,490)]);
(61, [EatInstr(110,116)]);
(445, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,491)]);
(62, [EatInstr(114,117)]);
(446, [EatInstr(116,492)]);
(63, [EatInstr(105,119);EatInstr(101,118)]);
(447, [EatInstr(116,493)]);
(64, [EatInstr(112,120)]);
(448, [AAction2Instr(__a37,203)]);
(65, [EatInstr(99,121)]);
(449, [AAction2Instr(__a38,203)]);
(66, [EatInstr(114,122)]);
(450, [EatInstr(99,494)]);
(67, [EatInstr(114,123)]);
(451, [EatInstr(116,495)]);
(68, [EatInstr(98,124)]);
(452, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,496)]);
(69, [EatInstr(97,125)]);
(453, [EatInstr(114,497)]);
(70, [EatInstr(114,126)]);
(454, [AAction2Instr(__a39,72)]);
(71, [EatInstr(97,127)]);
(455, [EatInstr(45,498)]);
(72, [CompleteInstr(272)]);
(456, [EatInstr(108,499)]);
(73, [ALookaheadInstr(false,CfgLA (1,264),74);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,73)]);
(457, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,500)]);
(74, [CompleteInstr(269)]);
(458, [EatInstr(116,501)]);
(75, [ALookaheadInstr(false,CfgLA (1,264),76);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,75)]);
(459, [EatInstr(115,502)]);
(76, [CompleteInstr(270)]);
(460, [EatInstr(108,503)]);
(77, [EatInstr(112,100)]);
(461, [EatInstr(117,504)]);
(78, [EatInstr(115,103)]);
(462, [EatInstr(97,505)]);
(79, [EatInstr(108,111);EatInstr(102,130)]);
(463, [EatInstr(101,506)]);
(80, [EatInstr(101,131)]);
(464, [EatInstr(45,508);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,507)]);
(81, [EatInstr(114,133);EatInstr(102,132)]);
(465, [AAction2Instr(__a40,72)]);
(82, [EatInstr(97,134)]);
(466, [EatInstr(118,509)]);
(83, [EatInstr(111,137);EatInstr(104,136);EatInstr(97,135)]);
(467, [EatInstr(45,510)]);
(84, [EatInstr(121,138)]);
(468, [EatInstr(100,511)]);
(85, [EatInstr(110,139)]);
(469, [EatInstr(114,512)]);
(86, [EatInstr(111,140)]);
(470, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,507)]);
(87, [EatInstr(101,141)]);
(471, [EatInstr(97,513)]);
(88, [EatInstr(111,142)]);
(472, [EatInstr(117,514)]);
(89, [EatInstr(110,143)]);
(473, [EatInstr(101,515)]);
(90, [EatInstr(114,144)]);
(474, [EatInstr(120,516)]);
(91, [EatInstr(111,145)]);
(475, [EatInstr(97,517)]);
(92, [EatInstr(115,147);EatInstr(110,146)]);
(476, [EatInstr(110,518)]);
(93, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,148)]);
(477, [EatInstr(101,519)]);
(94, [AAction2Instr(__a4,149)]);
(478, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,556)]);
(95, [EatInstr(45,150)]);
(479, [EatInstr(114,520)]);
(96, [EatInstr(111,151)]);
(480, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,521)]);
(97, [EatInstr(114,152)]);
(481, [EatInstr(103,522)]);
(98, [EatInstr(115,153)]);
(482, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,523)]);
(99, [EatInstr(112,154)]);
(483, [EatInstr(105,524)]);
(100, [EatInstr(121,155)]);
(484, [EatInstr(99,525)]);
(101, [EatInstr(111,156)]);
(485, [EatInstr(115,526)]);
(102, [EatInstr(101,157)]);
(486, [EatInstr(101,527)]);
(103, [EatInstr(117,158)]);
(487, [EatInstr(122,528)]);
(104, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,159)]);
(488, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,529)]);
(105, [EatInstr(99,160)]);
(489, [EatInstr(112,530)]);
(106, [EatInstr(114,161)]);
(490, [EatInstr(115,531)]);
(107, [EatInstr(101,162)]);
(491, [AAction2Instr(__a41,203)]);
(108, [EatInstr(45,163)]);
(492, [EatInstr(111,532)]);
(109, [EatInstr(104,164)]);
(493, [EatInstr(97,533)]);
(110, [EatInstr(111,166);EatInstr(101,165)]);
(494, [EatInstr(116,534)]);
(111, [EatInstr(105,167)]);
(495, [EatInstr(105,535)]);
(112, [EatInstr(101,168)]);
(496, [AAction2Instr(__a42,271)]);
(113, [EatInstr(116,169)]);
(497, [EatInstr(45,536)]);
(114, [EatInstr(107,170)]);
(498, [EatInstr(103,537)]);
(115, [EatInstr(45,171)]);
(499, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(116, [EatInstr(117,172)]);
(500, [AAction2Instr(__a43,369)]);
(117, [EatInstr(115,173)]);
(501, [EatInstr(111,539)]);
(118, [EatInstr(99,174)]);
(502, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,540)]);
(119, [EatInstr(110,175)]);
(503, [EatInstr(97,541)]);
(120, [EatInstr(108,176)]);
(504, [EatInstr(108,542)]);
(121, [AAction2Instr(__a5,177)]);
(505, [EatInstr(110,543)]);
(122, [EatInstr(116,178)]);
(506, [EatInstr(97,544)]);
(123, [EatInstr(105,179)]);
(507, [AAction2Instr(__a44,271)]);
(124, [EatInstr(115,180)]);
(508, [EatInstr(97,545)]);
(125, [EatInstr(110,181)]);
(509, [EatInstr(97,546)]);
(126, [EatInstr(111,182)]);
(510, [EatInstr(97,547)]);
(127, [EatInstr(112,183)]);
(511, [EatInstr(121,548)]);
(128, [AAction2Instr(__a6,184)]);
(512, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,549)]);
(129, [CompleteInstr(267)]);
(513, [EatInstr(116,550)]);
(130, [EatInstr(101,165)]);
(514, [EatInstr(110,551)]);
(131, [EatInstr(99,185)]);
(515, [EatInstr(103,552)]);
(132, [EatInstr(116,186)]);
(516, [AAction2Instr(__a45,660)]);
(133, [EatInstr(114,187)]);
(517, [EatInstr(100,553)]);
(134, [EatInstr(99,188)]);
(518, [EatInstr(115,554)]);
(135, [EatInstr(115,189)]);
(519, [EatInstr(108,555)]);
(136, [EatInstr(101,190)]);
(520, [EatInstr(111,558)]);
(137, [EatInstr(117,191)]);
(521, [AAction2Instr(__a46,203)]);
(138, [EatInstr(98,192)]);
(522, [EatInstr(117,559)]);
(139, [EatInstr(108,193)]);
(523, [AAction2Instr(__a47,203)]);
(140, [EatInstr(111,194)]);
(524, [EatInstr(115,560)]);
(141, [EatInstr(109,195)]);
(525, [EatInstr(101,561)]);
(142, [EatInstr(45,196)]);
(526, [EatInstr(101,562)]);
(143, [EatInstr(108,197)]);
(527, [EatInstr(45,563)]);
(144, [EatInstr(101,198)]);
(528, [EatInstr(101,564)]);
(145, [EatInstr(111,199)]);
(529, [AAction2Instr(__a48,203)]);
(146, [EatInstr(114,201);EatInstr(105,200)]);
(530, [EatInstr(116,565)]);
(147, [EatInstr(101,202)]);
(531, [EatInstr(116,566)]);
(148, [AAction2Instr(__a7,203)]);
(532, [EatInstr(114,567)]);
(149, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,204)]);
(533, [EatInstr(114,568)]);
(150, [EatInstr(108,205)]);
(534, [EatInstr(105,569)]);
(151, [EatInstr(119,206)]);
(535, [EatInstr(111,570)]);
(152, [EatInstr(105,207)]);
(536, [EatInstr(99,571)]);
(153, [EatInstr(101,208)]);
(537, [EatInstr(114,572)]);
(154, [EatInstr(105,209)]);
(538, [AAction2Instr(__a49,271)]);
(155, [EatInstr(114,210)]);
(539, [EatInstr(114,573)]);
(156, [EatInstr(117,211)]);
(540, [AAction2Instr(__a50,271)]);
(157, [EatInstr(110,212)]);
(541, [EatInstr(98,574)]);
(158, [EatInstr(103,213)]);
(542, [EatInstr(97,575)]);
(159, [AAction2Instr(__a8,72)]);
(543, [EatInstr(97,576)]);
(160, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,214)]);
(544, [EatInstr(100,577)]);
(161, [EatInstr(97,215)]);
(545, [EatInstr(110,578)]);
(162, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,216)]);
(546, [EatInstr(110,579)]);
(163, [EatInstr(103,217)]);
(547, [EatInstr(99,580)]);
(164, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,218)]);
(548, [EatInstr(112,581)]);
(165, [EatInstr(114,219)]);
(549, [AAction2Instr(__a51,271)]);
(166, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,220)]);
(550, [EatInstr(105,582)]);
(167, [EatInstr(110,221)]);
(551, [AAction2Instr(__a52,660)]);
(168, [EatInstr(114,222)]);
(552, [EatInstr(45,583);AAction2Instr(__a53,660)]);
(169, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,223)]);
(553, [EatInstr(108,585)]);
(170, [EatInstr(97,224)]);
(554, [EatInstr(105,586)]);
(171, [EatInstr(108,225)]);
(555, [EatInstr(115,587)]);
(172, [EatInstr(115,226)]);
(556, [AAction2Instr(__a54,557);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,556)]);
(173, [EatInstr(101,227)]);
(557, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,588)]);
(174, [EatInstr(101,228)]);
(558, [EatInstr(119,589)]);
(175, [EatInstr(116,229)]);
(559, [EatInstr(108,590)]);
(176, [EatInstr(97,230)]);
(560, [EatInstr(116,591)]);
(177, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,283)]);
(561, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,592)]);
(178, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,231)]);
(562, [EatInstr(45,593)]);
(179, [EatInstr(112,232)]);
(563, [EatInstr(104,594)]);
(180, [EatInstr(101,233)]);
(564, [EatInstr(45,595)]);
(181, [EatInstr(115,234)]);
(565, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,596)]);
(182, [EatInstr(108,235)]);
(566, [EatInstr(111,597)]);
(183, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,236)]);
(567, [EatInstr(121,598)]);
(184, [ALookaheadInstr(false,CfgLA (3,266),129);ACallInstr3(__default_call,11);ACallInstr3(__g0,10);AContInstr3(273,__g0,__binder3,128);ASimpleCont2Instr(274,__binder0,129)]);
(568, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,599)]);
(185, [EatInstr(101,237)]);
(569, [EatInstr(111,600)]);
(186, [EatInstr(101,238)]);
(570, [EatInstr(110,601)]);
(187, [EatInstr(111,239)]);
(571, [EatInstr(111,602)]);
(188, [EatInstr(107,240)]);
(572, [EatInstr(97,603)]);
(189, [EatInstr(101,241)]);
(573, [EatInstr(115,604)]);
(190, [EatInstr(99,242)]);
(574, [EatInstr(108,605)]);
(191, [EatInstr(110,243)]);
(575, [EatInstr(114,606)]);
(192, [EatInstr(114,244)]);
(576, [EatInstr(108,607)]);
(193, [EatInstr(105,245)]);
(577, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,608)]);
(194, [EatInstr(107,246)]);
(578, [EatInstr(97,609)]);
(195, [EatInstr(111,247)]);
(579, [EatInstr(99,610)]);
(196, [EatInstr(115,252);EatInstr(114,251);EatInstr(111,250);EatInstr(109,249);EatInstr(99,248)]);
(580, [EatInstr(116,611)]);
(197, [EatInstr(121,253)]);
(581, [EatInstr(103,612)]);
(198, [EatInstr(102,254)]);
(582, [EatInstr(111,613)]);
(199, [EatInstr(116,255)]);
(583, [EatInstr(115,614)]);
(200, [EatInstr(116,256)]);
(584, [AAction2Instr(__a55,203)]);
(201, [EatInstr(111,257)]);
(585, [EatInstr(101,615)]);
(202, [EatInstr(45,258)]);
(586, [EatInstr(116,616)]);
(203, [CompleteInstr(273)]);
(587, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,617)]);
(204, [AAction2Instr(__a9,203)]);
(588, [AAction2Instr(__a56,203)]);
(205, [EatInstr(97,259)]);
(589, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,618)]);
(206, [EatInstr(45,260)]);
(590, [EatInstr(97,619)]);
(207, [EatInstr(98,261)]);
(591, [EatInstr(111,620)]);
(208, [EatInstr(45,262)]);
(592, [AAction2Instr(__a57,203)]);
(209, [EatInstr(108,263)]);
(593, [EatInstr(99,621)]);
(210, [EatInstr(117,264)]);
(594, [EatInstr(105,622)]);
(211, [EatInstr(116,265)]);
(595, [EatInstr(112,623)]);
(212, [EatInstr(100,266)]);
(596, [AAction2Instr(__a58,203)]);
(213, [EatInstr(97,267)]);
(597, [EatInstr(114,624)]);
(214, [AAction2Instr(__a10,268)]);
(598, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,625)]);
(215, [EatInstr(99,269)]);
(599, [AAction2Instr(__a59,626)]);
(216, [AAction2Instr(__a11,72)]);
(600, [EatInstr(110,627)]);
(217, [EatInstr(101,270)]);
(601, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,628)]);
(218, [AAction2Instr(__a12,271)]);
(602, [EatInstr(114,629)]);
(219, [EatInstr(45,272)]);
(603, [EatInstr(112,630)]);
(220, [AAction2Instr(__a13,72)]);
(604, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,631)]);
(221, [EatInstr(101,273)]);
(605, [EatInstr(101,632)]);
(222, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,274)]);
(606, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,633)]);
(223, [AAction2Instr(__a14,271)]);
(607, [EatInstr(121,634)]);
(224, [EatInstr(104,275)]);
(608, [AAction2Instr(__a60,72)]);
(225, [EatInstr(111,276)]);
(609, [EatInstr(108,635)]);
(226, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,277)]);
(610, [EatInstr(101,636)]);
(227, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,278)]);
(611, [EatInstr(105,637)]);
(228, [EatInstr(100,279)]);
(612, [EatInstr(101,638)]);
(229, [EatInstr(45,281);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,280)]);
(613, [EatInstr(110,639)]);
(230, [EatInstr(121,282)]);
(614, [EatInstr(116,640)]);
(231, [AAction2Instr(__a15,72)]);
(615, [EatInstr(114,641)]);
(232, [EatInstr(45,285)]);
(616, [EatInstr(105,642)]);
(233, [EatInstr(116,286)]);
(617, [AAction2Instr(__a61,203)]);
(234, [EatInstr(108,287)]);
(618, [AAction2Instr(__a62,203)]);
(235, [EatInstr(108,288)]);
(619, [EatInstr(114,643)]);
(236, [AAction2Instr(__a16,271)]);
(620, [EatInstr(114,644)]);
(237, [EatInstr(100,289)]);
(621, [EatInstr(97,645)]);
(238, [EatInstr(114,290)]);
(622, [EatInstr(115,646)]);
(239, [EatInstr(119,291)]);
(623, [EatInstr(97,647)]);
(240, [EatInstr(101,292)]);
(624, [EatInstr(121,648)]);
(241, [EatInstr(45,293)]);
(625, [AAction2Instr(__a63,203)]);
(242, [EatInstr(107,294)]);
(626, [ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,668)]);
(243, [EatInstr(116,295)]);
(627, [EatInstr(115,649)]);
(244, [EatInstr(105,296)]);
(628, [AAction2Instr(__a64,271)]);
(245, [EatInstr(110,297)]);
(629, [EatInstr(101,650)]);
(246, [EatInstr(97,298)]);
(630, [EatInstr(104,651)]);
(247, [EatInstr(105,299)]);
(631, [AAction2Instr(__a65,72)]);
(248, [EatInstr(111,300)]);
(632, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,652)]);
(249, [EatInstr(101,301)]);
(633, [AAction2Instr(__a66,271)]);
(250, [EatInstr(112,302)]);
(634, [EatInstr(115,653)]);
(251, [EatInstr(101,303)]);
(635, [EatInstr(121,654)]);
(252, [EatInstr(107,304)]);
(636, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,655)]);
(253, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,305)]);
(637, [EatInstr(111,656)]);
(254, [EatInstr(105,306)]);
(638, [EatInstr(110,657)]);
(255, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,307)]);
(639, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,658)]);
(256, [EatInstr(45,308)]);
(640, [EatInstr(114,659)]);
(257, [EatInstr(108,309)]);
(641, [AAction2Instr(__a67,660)]);
(258, [EatInstr(102,310)]);
(642, [EatInstr(118,661)]);
(259, [EatInstr(116,311)]);
(643, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,662)]);
(260, [EatInstr(110,312)]);
(644, [EatInstr(121,663)]);
(261, [EatInstr(117,313)]);
(645, [EatInstr(108,664)]);
(262, [EatInstr(117,314)]);
(646, [EatInstr(116,665)]);
(263, [EatInstr(101,315)]);
(647, [EatInstr(109,666)]);
(264, [EatInstr(108,316)]);
(648, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,667)]);
(265, [EatInstr(105,317)]);
(649, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,670)]);
(266, [EatInstr(101,318)]);
(650, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,671)]);
(267, [EatInstr(114,319)]);
(651, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,672)]);
(268, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,320)]);
(652, [AAction2Instr(__a68,271)]);
(269, [EatInstr(116,321)]);
(653, [EatInstr(105,673)]);
(270, [EatInstr(110,322)]);
(654, [EatInstr(115,674)]);
(271, [CompleteInstr(271)]);
(655, [AAction2Instr(__a69,72)]);
(272, [EatInstr(116,324)]);
(656, [EatInstr(110,675)]);
(273, [EatInstr(45,325)]);
(657, [EatInstr(45,677);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,676)]);
(274, [AAction2Instr(__a17,271)]);
(658, [AAction2Instr(__a70,203)]);
(275, [EatInstr(101,326)]);
(659, [EatInstr(105,678)]);
(276, [EatInstr(111,327)]);
(660, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,584)]);
(277, [AAction2Instr(__a18,271)]);
(661, [EatInstr(101,679)]);
(278, [AAction2Instr(__a19,72)]);
(662, [AAction2Instr(__a71,203)]);
(279, [EatInstr(101,328)]);
(663, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,680)]);
(280, [AAction2Instr(__a20,72)]);
(664, [EatInstr(108,681)]);
(281, [EatInstr(114,330);EatInstr(103,329)]);
(665, [EatInstr(111,682)]);
(282, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,331)]);
(666, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,683)]);
(283, [AAction2Instr(__a21,284);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,283)]);
(667, [AAction2Instr(__a72,203)]);
(284, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,332)]);
(668, [AAction2Instr(__a73,669);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,668)]);
(285, [EatInstr(108,333)]);
(669, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,684)]);
(286, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,334)]);
(670, [AAction2Instr(__a74,72)]);
(287, [EatInstr(97,335)]);
(671, [AAction2Instr(__a75,271)]);
(288, [EatInstr(45,336)]);
(672, [AAction2Instr(__a76,72)]);
(289, [EatInstr(101,337)]);
(673, [EatInstr(115,685)]);
(290, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,338)]);
(674, [EatInstr(105,686)]);
(291, [EatInstr(45,339)]);
(675, [EatInstr(115,687)]);
(292, [EatInstr(110,340)]);
(676, [AAction2Instr(__a77,72)]);
(293, [EatInstr(105,341)]);
(677, [EatInstr(115,688)]);
(294, [EatInstr(45,342)]);
(678, [EatInstr(99,689)]);
(295, [EatInstr(101,343)]);
(679, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,690)]);
(296, [EatInstr(100,344)]);
(680, [AAction2Instr(__a78,203)]);
(297, [EatInstr(101,345)]);
(681, [EatInstr(115,691)]);
(298, [EatInstr(104,346)]);
(682, [EatInstr(114,692)]);
(299, [EatInstr(122,347)]);
(683, [AAction2Instr(__a79,203)]);
(300, [EatInstr(108,349);EatInstr(97,348)]);
(684, [AAction2Instr(__a80,203)]);
(301, [EatInstr(109,350)]);
(685, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,693)]);
(302, [EatInstr(116,351)]);
(686, [EatInstr(115,694)]);
(303, [EatInstr(112,352)]);
(687, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,695)]);
(304, [EatInstr(105,353)]);
(688, [EatInstr(99,696)]);
(305, [AAction2Instr(__a22,203)]);
(689, [EatInstr(116,697)]);
(306, [EatInstr(120,354)]);
(690, [AAction2Instr(__a81,203)]);
(307, [AAction2Instr(__a23,355)]);
(691, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,698)]);
(308, [EatInstr(104,356)]);
(692, [EatInstr(121,699)]);
(309, [EatInstr(108,357)]);
(693, [AAction2Instr(__a82,72)]);
(310, [EatInstr(115,358)]);
(694, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,700)]);
(311, [EatInstr(101,359)]);
(695, [AAction2Instr(__a83,72)]);
(312, [EatInstr(111,360)]);
(696, [EatInstr(97,701)]);
(313, [EatInstr(116,361)]);
(697, [AAction2Instr(__a84,660)]);
(314, [EatInstr(110,362)]);
(698, [AAction2Instr(__a85,203)]);
(315, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,363)]);
(699, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,702)]);
(316, [EatInstr(101,364)]);
(700, [AAction2Instr(__a86,72)]);
(317, [EatInstr(110,365)]);
(701, [EatInstr(110,703)]);
(318, [EatInstr(110,366)]);
(702, [AAction2Instr(__a87,203)]);
(319, [EatInstr(45,368);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,367)]);
(703, [EatInstr(110,704)]);
(320, [AAction2Instr(__a24,369)]);
(704, [EatInstr(101,705)]);
(321, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,370)]);
(705, [EatInstr(114,706)]);
(322, [EatInstr(101,371)]);
(706, [EatInstr(108,707)]);
(707, [EatInstr(101,708)]);
(324, [EatInstr(121,372)]);
(708, [EatInstr(115,709)]);
(325, [EatInstr(114,374);EatInstr(110,373)]);
(709, [EatInstr(115,710)]);
(326, [EatInstr(97,375)]);
(710, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,711)]);
(327, [EatInstr(107,376)]);
(711, [AAction2Instr(__a88,72)]);
(328, [EatInstr(110,377)]);
(329, [EatInstr(105,378)]);
(330, [EatInstr(101,379)]);
(331, [AAction2Instr(__a25,271)]);
(332, [AAction2Instr(__a26,72)]);
(333, [EatInstr(97,380)]);
(334, [AAction2Instr(__a27,271)]);
(335, [EatInstr(116,381)]);
(336, [EatInstr(115,382)]);
(337, [EatInstr(110,383)]);
(338, [AContInstr3(271,__g0,__binder4,384);ACallInstr3(__g0,8)]);
(339, [EatInstr(110,385)]);
(340, [EatInstr(100,386)]);
(341, [EatInstr(110,387)]);
(342, [EatInstr(108,388)]);
(343, [EatInstr(114,389)]);
(344, [EatInstr(45,390)]);
(345, [EatInstr(45,391)]);
(346, [EatInstr(101,392)]);
(347, [EatInstr(101,393)]);
(348, [EatInstr(108,394)]);
(349, [EatInstr(108,395)]);
(350, [EatInstr(111,396)]);
(351, [EatInstr(105,397)]);
(352, [EatInstr(108,398)]);
(353, [EatInstr(112,399)]);
(354, [EatInstr(45,400)]);
(355, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,401)]);
(356, [EatInstr(105,402)]);
(357, [EatInstr(45,403)]);
(358, [EatInstr(116,405);EatInstr(109,404)]);
(359, [EatInstr(45,406)]);
(360, [EatInstr(116,407)]);
(361, [EatInstr(101,408)]);
(362, [EatInstr(100,409)]);
(363, [AAction2Instr(__a28,72)]);
(364, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,410)]);
(365, [EatInstr(101,411)]);
(366, [EatInstr(99,412)]);
(367, [AAction2Instr(__a29,271)]);
(368, [EatInstr(103,413)]);
(369, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,414)]);
(370, [AAction2Instr(__a30,72)]);
(371, [EatInstr(114,415)]);
(372, [EatInstr(112,416)]);
(373, [EatInstr(117,417)]);
(374, [EatInstr(101,418)]);
(375, [EatInstr(100,419)]);
(376, [EatInstr(97,420)]);
(377, [EatInstr(99,421)]);
(378, [EatInstr(108,422)]);
(379, [EatInstr(108,423)]);
(380, [EatInstr(116,424)]);
(381, [EatInstr(101,425)]);
(382, [EatInstr(116,426)]);
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
