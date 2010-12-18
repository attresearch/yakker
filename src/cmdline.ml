
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
| Dispatch_cmd
| Dot_cmd
| Exec_cmd
| Extract_cmd
| Fuse_cmd
| Hash_cmd
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
| Rfc_cmd
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
  | Peg_BE of bool (** PEG backend. argument indicates whether
                       to use more liberal (PADS-style) star. *)
  | Trans_BE (** Transducer backend. *)

let backend = ref Trans_BE
type _yk_t =
| Yk_x1
;;
let sv0 = Yk_x1;;
type _wv = _yk_t;;
let _wv0 = Yk_x1;;
type hv = int
;;
let _l2hv x = x;; (* label to hv *)

module Yk_Hashed = struct
  type t = hv * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
end
module Yk_History = Yak.History.Make(Yk_Hashed)

(*REPLAY PROLOGUE*)
let rec
_r_cmd_line_args(_n,ykinput) = (ignore (*1000*) (_n()); 
 (let c = (ignore (*1001*) (_n()); _r_command(_n,ykinput))
  in (ignore (*1003*) (_n()); 
 (let _x27 = (ignore (*1004*) (_n()); 
 (let _x4 = (ignore (*1005*) (_n()); 
 (let rec _x29 _x4 = (match _n() with 1006 -> _x4 | _x28 -> _x29((ignore (*1007*) (_x28); 
 (let _x3 = (ignore (*1008*) (_n()); _r_args(_n,ykinput))
  in (ignore (*1010*) (_n()); _x3::_x4)
 ))
 )) in _x29(Yak.Util.nil)))
  in (ignore (*1011*) (_n()); (List.rev _x4))
 ))
  in (ignore (*1013*) (_n());  cmd := c )
 ))
 ))
 
 and
_r_phases(_n,ykinput) = 
 (match _n() with
 | (1026) -> ( Attributes_cmd )
 | (1029) -> ( Close_under_core_cmd )
 | (1032) -> ( Copyrule_cmd )
 | (1035) -> ( Desugar_cmd )
 | (1038) -> ( Hash_cmd )
 | (1041) -> ( Inline_regular_cmd )
 | (1044) -> ( Lexer_cmd )
 | (1047) -> ( Lift_cmd )
 | (1050) -> ( Minus_cmd )
 | (1053) -> ( Tx_prec_cmd )
 | (1056) -> ( Subset_cmd )
 | (1059) -> ( Unroll_star_cmd )
 | _(*1062*) -> ( Wrap_cmd )
 )
 and
_r_command(_n,ykinput) = (ignore (*1063*) (_n()); 
 (let _x25 = 
 (match _n() with
 | (1066) -> (
 (let p = (ignore (*1067*) (_n()); _r_phases(_n,ykinput))
  in (ignore (*1069*) (_n());  (match p with
                                               Inline_regular_cmd -> Compileopt.inline_regular := true
                                             | Unroll_star_cmd -> if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1
                                             | _ -> ());
                                            p )
 ))
 | (1074) -> ( Compile_cmd )
 | (1079) -> ( Dispatch_cmd )
 | (1084) -> ( Dot_cmd )
 | (1089) -> (
 (let _x7 = (ignore (*1090*) (_n()); _n())
  in (ignore (*1093*) (_n()); 
 (let _x6 = (ignore (*1094*) (_n()); _n())
  in (ignore (*1096*) (_n()); 
 (let f = (ignore (*1097*) (_n()); Yak.YkBuf.get_string _x7 _x6 ykinput)
  in (ignore (*1098*) (_n()); 
 (let l = (ignore (*1099*) (_n()); 
 (let _x11 = (ignore (*1100*) (_n()); 
 (let rec _x31 _x11 = (match _n() with 1101 -> _x11 | _x30 -> _x31((ignore (*1102*) (_x30); 
 (let _x10 = (ignore (*1104*) (_n()); 
 (let _x9 = (ignore (*1105*) (_n()); _n())
  in (ignore (*1108*) (_n()); 
 (let _x8 = (ignore (*1109*) (_n()); _n())
  in (ignore (*1111*) (_n()); 
 (let x = (ignore (*1112*) (_n()); Yak.YkBuf.get_string _x9 _x8 ykinput)
  in (ignore (*1113*) (_n()); x)
 ))
 ))
 ))
  in (ignore (*1114*) (_n()); _x10::_x11)
 ))
 )) in _x31(Yak.Util.nil)))
  in (ignore (*1115*) (_n()); (List.rev _x11))
 ))
  in (ignore (*1117*) (_n());  files := f::!files; exec_l := l; Exec_cmd )
 ))
 ))
 ))
 ))
 | (1122) -> ( Extract_cmd )
 | (1127) -> ( Compileopt.coalesce := true; Fuse_cmd )
 | (1132) -> ( Info_cmd )
 | (1137) -> ( Lookahead_analysis_cmd )
 | (1142) -> ( Lr1_lookahead_cmd )
 | (1147) -> ( Precedence_analysis_cmd )
 | (1152) -> ( Print_gul_cmd )
 | (1157) -> ( Print_gil_cmd )
 | (1162) -> ( Print_npreds_cmd )
 | (1167) -> ( Print_npreds_cmd )
 | (1172) -> ( Print_relevance_cmd )
 | (1174) -> (
 (let _x14 = (ignore (*1175*) (_n()); _n())
  in (ignore (*1186*) (_n()); 
 (let _x13 = (ignore (*1187*) (_n()); _n())
  in (ignore (*1189*) (_n()); 
 (let n = (ignore (*1190*) (_n()); Yak.YkBuf.get_string _x14 _x13 ykinput)
  in (ignore (*1192*) (_n());  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" )
 ))
 ))
 ))
 | (1199) -> ( Strip_late_actions_cmd )
 | (1206) -> ( Translate_dypgen_cmd )
 | _(*1211*) -> ( Translate_dypgen_scannerless_cmd )
 ) in (ignore (*1212*) (_n()); _x25)
 ))
 
 and
_r_args(_n,ykinput) = (ignore (*1213*) (_n()); 
 (let _x26 = 
 (match _n() with
 | (1218) -> (
 (let p = (ignore (*1219*) (_n()); _r_phases(_n,ykinput))
  in (ignore (*1221*) (_n());  after := Some p )
 ))
 | (1226) -> (
 (let b = 
 (match _n() with
 | (1228) -> (Fun_BE)
 | (1230) -> (Trans_BE)
 | (1232) -> (Peg_BE false)
 | _(*1234*) -> (Peg_BE true)
 ) in (ignore (*1236*) (_n());  backend := b )
 ))
 | (1241) -> ( Compileopt.case_sensitive := false )
 | (1246) -> ( Compileopt.check_labels := true )
 | (1249) -> (
 (let _x17 = (ignore (*1250*) (_n()); _n())
  in (ignore (*1261*) (_n()); 
 (let _x16 = (ignore (*1262*) (_n()); _n())
  in (ignore (*1264*) (_n()); 
 (let n = (ignore (*1265*) (_n()); Yak.YkBuf.get_string _x17 _x16 ykinput)
  in (ignore (*1267*) (_n());  Variables.counter := (int_of_string n) )
 ))
 ))
 ))
 | (1272) -> ( Compileopt.inline_cs := true )
 | (1277) -> ( Compileopt.inline_regular := true )
 | (1282) -> ( Compileopt.memoize_history := true )
 | (1287) -> ( Compileopt.memoize_history := false )
 | (1292) -> ( Compileopt.unit_history := true )
 | (1297) -> ( Compileopt.repress_replay := true )
 | (1302) -> ( Compileopt.lookahead := true )
 | (1307) -> ( Compileopt.coalesce := false )
 | (1312) -> ( only := true )
 | (1317) -> (
 (let _x19 = (ignore (*1318*) (_n()); _n())
  in (ignore (*1321*) (_n()); 
 (let _x18 = (ignore (*1322*) (_n()); _n())
  in (ignore (*1324*) (_n()); 
 (let x = (ignore (*1325*) (_n()); Yak.YkBuf.get_string _x19 _x18 ykinput)
  in (ignore (*1327*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | (1330) -> (
 (let _x22 = (ignore (*1331*) (_n()); _n())
  in (ignore (*1342*) (_n()); 
 (let _x21 = (ignore (*1343*) (_n()); _n())
  in (ignore (*1345*) (_n()); 
 (let n = (ignore (*1346*) (_n()); Yak.YkBuf.get_string _x22 _x21 ykinput)
  in (ignore (*1348*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1355) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | _(*1358*) -> (
 (let _x24 = (ignore (*1359*) (_n()); _n())
  in (ignore (*1362*) (_n()); 
 (let _x23 = (ignore (*1363*) (_n()); _n())
  in (ignore (*1365*) (_n()); 
 (let f = (ignore (*1366*) (_n()); Yak.YkBuf.get_string _x24 _x23 ykinput)
  in (ignore (*1368*) (_n());  files := f::!files )
 ))
 ))
 ))
 ) in (ignore (*1369*) (_n()); _x26)
 ))
 
 
(*EARLY-LATE PROLOGUE*)
(*TODO:sv,sv0,sv_compare*)
type _uid = int (* for sharing *)
type _pos = int (* input positions *)
type _lab = int (* dispatch labels *)
type 'a ev = (* early values, aka coroutines.  'a is the type of values eventually computed by the coroutines *)
  | Yk_more of _uid * (_lab -> _pos -> 'a ev)
  | Yk_box of (_pos -> Yak.YkBuf.t -> (int * 'a ev) option)
  | Yk_when of bool
  | Yk_delay of 'a ev * hv
  | Yk_bind of ('a ev -> 'a ev)
  | Yk_done of 'a
let hv_compare = Yk_History.compare
let ev_compare x y =
  match x,y with
  | Yk_more(c1,_), Yk_more(c2,_) -> compare c1 c2
  | Yk_done c1, Yk_done c2 -> compare c1 c2 (* TODO: user-supplied compare *)
  | Yk_more _, Yk_done _ -> 1
  | Yk_done _, Yk_more _  -> -1
  | _,_ -> failwith "TODO sv_compare"
let _t_count = ref 0
let _fresh_t_id () =
  let count = !_t_count in
  incr _t_count;
  count
let _t f = Yk_more(_fresh_t_id(),f)
type sv = _wv ev * (hv*_pos, Yak.History.label) Yak.History.history
let sv0 = (Yk_done _wv0, Yk_History.new_history())
let sv_compare (x1,x2) (y1,y2) =
  (match ev_compare x1 y1 with
  | 0 -> hv_compare x2 y2
  | z -> z)
let _ev_to_string = function
  | Yk_more   _ -> "Yk_more"
  | Yk_box    _ -> "Yk_box"
  | Yk_when   _ -> "Yk_when"
  | Yk_delay  _ -> "Yk_delay"
  | Yk_bind   _ -> "Yk_bind"
  | Yk_done   _ -> "Yk_done"

let sv_hash (x,h) =
  let hash_h = Yk_History.hash h in
  (Hashtbl.hash x) lxor hash_h

(* Coroutine transformers *)

(*TJIM: we need multiple dispatch functions in a typed language, hence
  the many _d* variants.  The first argument x is the label.  Some _d*
  functions take a second argument p, which is the current input
  position.  The p is used by for history manipulations to distinguish
  two history nodes with the same label.  Some _d* functions that have
  a p argument use the p, and some do not use it.  Example: _d needs p
  even though p is unused in its body because it is used in
  Gil.Action, and there are some other Gil.Actions that need the p,
  e.g., _p and _ddelay. *)

let _d x p = function
    (Yk_more(_,t),h) -> (t x p,h)
  | (ev,_) -> failwith (Printf.sprintf "_d(%s)" (_ev_to_string ev))
let _darg x p = function (*TJIM: same as _d without p*)
    (Yk_more(_,t),h) -> (t x p,h#empty p)
  | _ -> failwith "_darg"
let _dbox x = function
    (Yk_more(_,t),h) ->
      (fun p ->
        (match t x p with
          Yk_box b ->
            (fun ykb -> (* painful! *)
              (match b p ykb with None -> None
              | Some(p2,a) -> Some(p2,(a,h))))
        | _ -> failwith "_dbox"))
  | _ -> failwith "_dbox"
let _dwhen x p = function
    (Yk_more(_,t),h) -> (match t x p with Yk_when b -> b | _ -> failwith "_dwhen")
  | _ -> failwith "_dwhen"
let _ddelay x p =
  (function
    | (Yk_more(_,t),h) -> (match t x p with Yk_delay(v,hv) -> (v,(h#push p ((x),p))#push p (hv,p)) | _ -> failwith "_ddelay1")
    | _ -> failwith "_ddelay2")
let _dret x p =
  (function
    | (Yk_more(_,t),h) ->
        (fun (r,_) ->
          match t x p with
          | Yk_bind(f) -> (f r,h)
          | _ -> failwith "_dret1")
    | _ -> failwith "_dret2")
let _dmerge x p =
  (function
    | (Yk_more(_,t),h1) ->
        (fun (r,h2) ->
          match t x p with
          | Yk_bind(f) -> (f r,h1#merge p ((x),p) h2)
          | _ -> failwith "_dmerge1")
    | _ -> failwith "_dmerge3")
let _d_and_push x p = function
    (Yk_more(_,t),h) -> (t x p,h#push p ((x),p))
  | _ -> failwith "_d_and_push"
let _dnext x p = function (*TJIM: same as _d without p *)
    (Yk_more(_,t),h) -> (t x p,h)
  | _ -> failwith "_dnext"
(* History transformers *)
let _p x p = (fun(v,h)->(v,h#push p ((x),p)))
let _p_pos x p = (fun(v,h)->(v,(h#push p ((x),p))#push p ((p),p)))
let _m x p = (fun(v1,h1)->fun(_,h2)-> (v1,h1#merge p ((x),p) h2))

let sv_eq x y = sv_compare x y = 0
let key_eq (i,v1) (j,v2) = i = j &&  sv_eq v1 v2
let key_hash (i,v) = i lxor (sv_hash v)

(** Hashtable for top-down parsing. *)
module TDHashtable = Hashtbl.Make(struct type t = int * sv let equal = key_eq let hash = key_hash end)

let _x35 =
 (fun _(*pos*) (_,_x32)(*arg of arg*) -> (_t(fun _(*1016*) pos_ -> let _x33 _x5  = _t(function
 | 1022 ->
 (fun pos_ -> Yk_when(_x5>=1))
 | _(*1023*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1017*) pos_ -> let rec _x34 _x5  = _t(function
 | 1018 ->
 (fun pos_ -> _x33 (_x5) )
 | _(*1020*) ->
 (fun pos_ -> _x34 (_x5+1) )) in _x34 (0) )),_x32))
let _x43 =
 (fun _(*pos*) (_,_x36)(*arg of command*) -> (_t(function
 | 1065 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1071 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1076 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1081 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1086 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1119 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1124 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1129 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1134 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1139 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1144 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1149 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1154 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1159 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1164 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1169 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1178 ->
 (fun pos_ -> let _x41 _x12  = _t(function
 | 1184 ->
 (fun pos_ -> Yk_when(_x12>=1))
 | _(*1185*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1179*) pos_ -> let rec _x42 _x12  = _t(function
 | 1180 ->
 (fun pos_ -> _x41 (_x12) )
 | _(*1182*) ->
 (fun pos_ -> _x42 (_x12+1) )) in _x42 (0) ))
 | _(*1193*) ->
 (fun pos_ -> let _x38 _x37  = _t(function
 | 1196 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1200*) ->
 (fun pos_ -> let _x40 _x39  = _t(function
 | 1203 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1208*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1201*) pos_ -> _x40 (()) ))) in _t(fun _(*1194*) pos_ -> _x38 (()) ))),_x36))
let _x51 =
 (fun _(*pos*) (_,_x44)(*arg of args*) -> (_t(function
 | 1215 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1223 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1238 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1243 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1253 ->
 (fun pos_ -> let _x49 _x15  = _t(function
 | 1259 ->
 (fun pos_ -> Yk_when(_x15>=1))
 | _(*1260*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1254*) pos_ -> let rec _x50 _x15  = _t(function
 | 1255 ->
 (fun pos_ -> _x49 (_x15) )
 | _(*1257*) ->
 (fun pos_ -> _x50 (_x15+1) )) in _x50 (0) ))
 | 1269 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1274 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1279 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1284 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1289 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1294 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1299 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1304 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1309 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1314 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1334 ->
 (fun pos_ -> let _x47 _x20  = _t(function
 | 1340 ->
 (fun pos_ -> Yk_when(_x20>=1))
 | _(*1341*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1335*) pos_ -> let rec _x48 _x20  = _t(function
 | 1336 ->
 (fun pos_ -> _x47 (_x20) )
 | _(*1338*) ->
 (fun pos_ -> _x48 (_x20+1) )) in _x48 (0) ))
 | _(*1349*) ->
 (fun pos_ -> let _x46 _x45  = _t(function
 | 1352 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1357*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1350*) pos_ -> _x46 (()) ))),_x44))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a101 = fun p v -> _p 1114 p (_p 1113 p (_p 1112 p (_p 1111 p (_p_pos 1109 p (_p 1108 p (v))))));;
let __a11 = fun p v -> _d 1139 p (_p 1063 p (_x43 p (v)));;
let __a133 = _p 1211;;
let __a109 = _p 1041;;
let __a10 = fun p v -> _d 1134 p (_p 1063 p (_x43 p (v)));;
let __a48 = _p 1212;;
let __a69 = _p 1152;;
let __a78 = _p 1102;;
let __a27 = fun p v -> _d 1269 p (_p 1213 p (_x51 p (v)));;
let __a26 = fun p v -> _p 1213 p (_x51 p (v));;
let __a60 = _p 1044;;
let __a13 = fun p v -> _d 1149 p (_p 1063 p (_x43 p (v)));;
let __a62 = fun p v -> _p_pos 1090 p (_p 1089 p (v));;
let __a23 = fun p v -> _d 1223 p (_p 1213 p (_x51 p (v)));;
let __p117 = _dnext 1341;;
let __a113 = _p 1267;;
let __a93 = _p 1327;;
let __a91 = _p 1157;;
let __a55 = _p 1047;;
let __a39 = _d 1018;;
let __a24 = fun p v -> _d 1238 p (_p 1213 p (_x51 p (v)));;
let __a49 = fun p v -> _d 1179 p (_d 1178 p (_p_pos 1175 p (_p 1174 p (v))));;
let __a70 = fun p v -> _p 1190 p (_p 1189 p (_p_pos 1187 p (_p 1186 p (v))));;
let __a3 = fun p v -> _d 1071 p (_p 1063 p (_x43 p (v)));;
let __a6 = fun p v -> _d 1086 p (_p 1063 p (_x43 p (v)));;
let __a28 = fun p v -> _d 1274 p (_p 1213 p (_x51 p (v)));;
let __p118 = _dwhen 1340;;
let __a14 = fun p v -> _d 1154 p (_p 1063 p (_x43 p (v)));;
let __a61 = _p 1050;;
let __a58 = _d 1180;;
let __a41 = _d 1020;;
let __a84 = _p 1221;;
let __a31 = fun p v -> _d 1289 p (_p 1213 p (_x51 p (v)));;
let __a104 = _p 1162;;
let __a98 = _p 1272;;
let __a17 = fun p v -> _d 1169 p (_p 1063 p (_x43 p (v)));;
let __a34 = fun p v -> _d 1304 p (_p 1213 p (_x51 p (v)));;
let __a65 = _d 1182;;
let __a25 = fun p v -> _d 1243 p (_p 1213 p (_x51 p (v)));;
let __a88 = _p 1053;;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a2 = fun p v -> _p 1067 p (_p 1066 p (_d 1065 p (_p 1063 p (_x43 p (v)))));;
let __a68 = _p 1056;;
let __a85 = _p 1226;;
let __a132 = _p 1167;;
let __a122 = _p 1277;;
let __a90 = _p 1117;;
let __a105 = _p 1228;;
let __a94 = _p 1059;;
let __p42 = _dnext 1023;;
let __p66 = _dnext 1185;;
let __a38 = fun p v -> _p_pos 1359 p (_p 1358 p (_d 1357 p (_d 1350 p (_d 1349 p (_p 1213 p (_x51 p (v)))))));;
let __a22 = fun p v -> _d 1215 p (_p 1213 p (_x51 p (v)));;
let __a9 = fun p v -> _d 1129 p (_p 1063 p (_x43 p (v)));;
let __a37 = fun p v -> _d 1352 p (_d 1350 p (_d 1349 p (_p 1213 p (_x51 p (v)))));;
let __a102 = _p 1230;;
let __a52 = _p 1010;;
let __p43 = _dwhen 1022;;
let __a130 = _p 1232;;
let __a124 = _p 1282;;
let __a120 = _p 1172;;
let __a80 = _p 1122;;
let __a56 = _p 1062;;
let __p67 = _dwhen 1184;;
let __a72 = fun p v -> _p_pos 1318 p (_p 1317 p (v));;
let __a53 = _p 1013;;
let __a106 = _p 1234;;
let __a92 = _d 1255;;
let __a111 = _p 1236;;
let __a107 = fun p v -> _d 1335 p (_d 1334 p (_p_pos 1331 p (_p 1330 p (v))));;
let __a131 = _p 1287;;
let __a95 = _d 1257;;
let __a63 = _p 1127;;
let __a126 = _p 1348;;
let __a75 = fun p v -> _p 1219 p (_p 1218 p (v));;
let __a44 = _p 1069;;
let __a74 = fun p v -> _p 1100 p (_p 1099 p (_p 1098 p (_p 1097 p (_p 1096 p (_p_pos 1094 p (_p 1093 p (v)))))));;
let __a20 = fun p v -> _d 1203 p (_d 1201 p (_d 1200 p (_d 1194 p (_d 1193 p (_p 1063 p (_x43 p (v)))))));;
let __a47 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x35 p (v)));;
let __a19 = fun p v -> _d 1196 p (_d 1194 p (_d 1193 p (_p 1063 p (_x43 p (v)))));;
let __a103 = fun p v -> _p 1265 p (_p 1264 p (_p_pos 1262 p (_p 1261 p (v))));;
let __a121 = fun p v -> _p 1346 p (_p 1345 p (_p_pos 1343 p (_p 1342 p (v))));;
let __a4 = fun p v -> _d 1076 p (_p 1063 p (_x43 p (v)));;
let __a12 = fun p v -> _d 1144 p (_p 1063 p (_x43 p (v)));;
let __a125 = _p 1241;;
let __a46 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a115 = _p 1292;;
let __a100 = _p 1302;;
let __a29 = fun p v -> _d 1279 p (_p 1213 p (_x51 p (v)));;
let __a18 = fun p v -> _p 1063 p (_x43 p (v));;
let __a64 = _p 1132;;
let __a15 = fun p v -> _d 1159 p (_p 1063 p (_x43 p (v)));;
let __a77 = _p 1074;;
let __a50 = _p 1355;;
let __a112 = _p 1246;;
let __p96 = _dnext 1260;;
let __a87 = _p 1026;;
let __a35 = fun p v -> _d 1309 p (_p 1213 p (_x51 p (v)));;
let __a127 = _p 1137;;
let __a108 = _p 1307;;
let __a99 = _p 1297;;
let __a89 = fun p v -> _p_pos 1105 p (_p 1104 p (v));;
let __p97 = _dwhen 1259;;
let __a119 = _p 1029;;
let __a83 = _p 1079;;
let __a21 = fun p v -> _d 1208 p (_d 1201 p (_d 1200 p (_d 1194 p (_d 1193 p (_p 1063 p (_x43 p (v)))))));;
let __a5 = fun p v -> _d 1081 p (_p 1063 p (_x43 p (v)));;
let __a40 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a45 = fun p v -> _p 1366 p (_p 1365 p (_p_pos 1363 p (_p 1362 p (v))));;
let __a82 = fun p v -> _p 1325 p (_p 1324 p (_p_pos 1322 p (_p 1321 p (v))));;
let __a30 = fun p v -> _d 1284 p (_p 1213 p (_x51 p (v)));;
let __a16 = fun p v -> _d 1164 p (_p 1063 p (_x43 p (v)));;
let __a7 = fun p v -> _d 1119 p (_p 1063 p (_x43 p (v)));;
let __a33 = fun p v -> _d 1299 p (_p 1213 p (_x51 p (v)));;
let __a36 = fun p v -> _d 1314 p (_p 1213 p (_x51 p (v)));;
let __a110 = _p 1142;;
let __a76 = _p 1032;;
let __a71 = _p 1312;;
let __a81 = _p 1192;;
let __a79 = fun p v -> _p 1115 p (_p 1101 p (v));;
let __a32 = fun p v -> _d 1294 p (_p 1213 p (_x51 p (v)));;
let __a57 = _p 1084;;
let __a73 = _p 1035;;
let __a123 = _p 1206;;
let __a114 = _d 1336;;
let __a86 = fun p v -> _d 1254 p (_d 1253 p (_p_pos 1250 p (_p 1249 p (v))));;
let __a129 = _p 1147;;
let __a116 = _d 1338;;
let __a51 = _p 1368;;
let __a54 = _p 1038;;
let __a128 = _p 1199;;
let __a59 = _p 1369;;
let __a8 = fun p v -> _d 1124 p (_p 1063 p (_x43 p (v)));;
let __binder0 = __default_ret;;
let __binder1 = _m 1002;;
let __binder2 = _m 1068;;
let __binder3 = _m 1009;;
let __binder4 = _m 1220;;
let binders : (sv -> sv -> sv) array = [| |]
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

open Yak.Pam_internal
module SV_hashtbl = Hashtbl.Make(struct
                          type t = sv
                          let equal a b = sv_compare a b = 0
                          let hash = Hashtbl.hash end)
module Pred = Pred3
let rec nullable_cmd_line_args __lookahead _p0_ _x0_ = None

and nullable_command __lookahead _p0_ _x0_ = None

and nullable_o __lookahead _p0_ _x0_ = None

and nullable_CHAR __lookahead _p0_ _x0_ = None

and nullable_file __lookahead _p0_ _x0_ = None

and nullable_DIGIT __lookahead _p0_ _x0_ = None

and nullable_args __lookahead _p0_ _x0_ = None

and nullable_phases __lookahead _p0_ _x0_ = None

and nullable_arg __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1022 and n = _dnext 1023 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (Pred.full_lookaheadc false 264 1)) __lookahead) _p0_) (((_d 1018) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1017 p (_d 1016 p (_x35 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_eof __lookahead _p0_ _x0_ = ((((Pred.full_lookaheadc false 266 3) __lookahead) _p0_) _x0_)

and nullable_OCTET __lookahead _p0_ _x0_ = None

let program : (int * sv instruction list) list = [
(383, [EatInstr(105,419)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(112,420)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(117,421)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(101,422)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,423)]);
(4, [AAction2Instr(__a0,15)]);
(388, [EatInstr(97,424)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(116,425)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,17)]);
(390, [EatInstr(116,426)]);
(7, [EatInstr(127,19);EatInstr(126,19);EatInstr(125,19);EatInstr(124,19);EatInstr(123,19);EatInstr(122,19);EatInstr(121,19);EatInstr(120,19);EatInstr(119,19);EatInstr(118,19);EatInstr(117,19);EatInstr(116,19);EatInstr(115,19);EatInstr(114,19);EatInstr(113,19);EatInstr(112,19);EatInstr(111,19);EatInstr(110,19);EatInstr(109,19);EatInstr(108,19);EatInstr(107,19);EatInstr(106,19);EatInstr(105,19);EatInstr(104,19);EatInstr(103,19);EatInstr(102,19);EatInstr(101,19);EatInstr(100,19);EatInstr(99,19);EatInstr(98,19);EatInstr(97,19);EatInstr(96,19);EatInstr(95,19);EatInstr(94,19);EatInstr(93,19);EatInstr(92,19);EatInstr(91,19);EatInstr(90,19);EatInstr(89,19);EatInstr(88,19);EatInstr(87,19);EatInstr(86,19);EatInstr(85,19);EatInstr(84,19);EatInstr(83,19);EatInstr(82,19);EatInstr(81,19);EatInstr(80,19);EatInstr(79,19);EatInstr(78,19);EatInstr(77,19);EatInstr(76,19);EatInstr(75,19);EatInstr(74,19);EatInstr(73,19);EatInstr(72,19);EatInstr(71,19);EatInstr(70,19);EatInstr(69,19);EatInstr(68,19);EatInstr(67,19);EatInstr(66,19);EatInstr(65,19);EatInstr(64,19);EatInstr(63,19);EatInstr(62,19);EatInstr(61,19);EatInstr(60,19);EatInstr(59,19);EatInstr(58,19);EatInstr(57,19);EatInstr(56,19);EatInstr(55,19);EatInstr(54,19);EatInstr(53,19);EatInstr(52,19);EatInstr(51,19);EatInstr(50,19);EatInstr(49,19);EatInstr(48,19);EatInstr(47,19);EatInstr(46,19);EatInstr(44,19);EatInstr(43,19);EatInstr(42,19);EatInstr(41,19);EatInstr(40,19);EatInstr(39,19);EatInstr(38,19);EatInstr(37,19);EatInstr(36,19);EatInstr(35,19);EatInstr(34,19);EatInstr(33,19);EatInstr(32,19);EatInstr(31,19);EatInstr(30,19);EatInstr(29,19);EatInstr(28,19);EatInstr(27,19);EatInstr(26,19);EatInstr(25,19);EatInstr(24,19);EatInstr(23,19);EatInstr(22,19);EatInstr(21,19);EatInstr(20,19);EatInstr(19,19);EatInstr(18,19);EatInstr(17,19);EatInstr(16,19);EatInstr(15,19);EatInstr(14,19);EatInstr(13,19);EatInstr(12,19);EatInstr(11,19);EatInstr(10,19);EatInstr(9,19);EatInstr(8,19);EatInstr(7,19);EatInstr(6,19);EatInstr(5,19);EatInstr(4,19);EatInstr(3,19);EatInstr(2,19);EatInstr(1,19)]);
(391, [AAction2Instr(__a75,427)]);
(8, [EatInstr(119,30);EatInstr(117,29);EatInstr(115,28);EatInstr(112,27);EatInstr(109,26);EatInstr(108,25);EatInstr(105,24);EatInstr(104,23);EatInstr(100,22);EatInstr(99,21);EatInstr(97,20)]);
(392, [EatInstr(100,428)]);
(9, [AAction2Instr(__a21,50);AAction2Instr(__a20,49);AAction2Instr(__a19,48);AAction2Instr(__a18,47);AAction2Instr(__a17,46);AAction2Instr(__a16,45);AAction2Instr(__a15,44);AAction2Instr(__a14,43);AAction2Instr(__a13,42);AAction2Instr(__a12,41);AAction2Instr(__a11,40);AAction2Instr(__a10,39);AAction2Instr(__a9,38);AAction2Instr(__a8,37);AAction2Instr(__a7,36);AAction2Instr(__a6,35);AAction2Instr(__a5,34);AAction2Instr(__a4,33);AAction2Instr(__a3,32);AAction2Instr(__a2,31)]);
(393, [EatInstr(110,429)]);
(10, [AAction2Instr(__a38,67);AAction2Instr(__a37,66);AAction2Instr(__a36,65);AAction2Instr(__a35,64);AAction2Instr(__a34,63);AAction2Instr(__a33,62);AAction2Instr(__a32,61);AAction2Instr(__a31,60);AAction2Instr(__a30,59);AAction2Instr(__a29,58);AAction2Instr(__a28,57);AAction2Instr(__a27,56);AAction2Instr(__a26,55);AAction2Instr(__a25,54);AAction2Instr(__a24,53);AAction2Instr(__a23,52);AAction2Instr(__a22,51)]);
(394, [EatInstr(108,430)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),68);RCompleteInstr2(274,nullable_eof)]);
(395, [EatInstr(114,431)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(45,432)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(45,433)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(45,434)]);
(15, [ACallInstr3(__default_call,9);ASimpleCont2Instr(272,__binder1,69)]);
(399, [EatInstr(101,435)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(111,436)]);
(17, [AAction2Instr(__a39,71);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,70)]);
(401, [EatInstr(105,437)]);
(402, [EatInstr(108,438)]);
(19, [ALookaheadInstr(false,CfgLA (1,264),72);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,19)]);
(403, [EatInstr(101,439)]);
(20, [EatInstr(116,73)]);
(404, [EatInstr(108,440)]);
(21, [EatInstr(111,75);EatInstr(108,74)]);
(405, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,441)]);
(22, [EatInstr(101,76)]);
(406, [EatInstr(115,442)]);
(23, [EatInstr(97,77)]);
(407, [EatInstr(101,443)]);
(24, [EatInstr(110,78)]);
(408, [AAction2Instr(__a76,286)]);
(25, [EatInstr(105,80);EatInstr(101,79)]);
(409, [EatInstr(103,444)]);
(26, [EatInstr(105,81)]);
(410, [EatInstr(101,445)]);
(27, [EatInstr(114,82)]);
(411, [EatInstr(97,446)]);
(28, [EatInstr(117,83)]);
(412, [AAction2Instr(__a77,138)]);
(29, [EatInstr(110,84)]);
(413, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,447)]);
(30, [EatInstr(114,85)]);
(414, [AAction2Instr(__a79,449);AAction2Instr(__a78,448)]);
(31, [ASimpleCont2Instr(271,__binder2,86);ACallInstr3(__default_call,8)]);
(415, [AAction2Instr(__a80,138)]);
(32, [EatInstr(99,87)]);
(416, [EatInstr(100,450)]);
(33, [EatInstr(100,88)]);
(417, [EatInstr(97,451)]);
(34, [EatInstr(100,89)]);
(418, [EatInstr(99,452)]);
(35, [EatInstr(101,90)]);
(419, [EatInstr(108,453)]);
(36, [EatInstr(101,91)]);
(420, [EatInstr(114,454)]);
(37, [EatInstr(102,92)]);
(421, [EatInstr(108,455)]);
(38, [EatInstr(105,93)]);
(422, [EatInstr(108,456)]);
(39, [EatInstr(108,94)]);
(423, [AAction2Instr(__a81,138)]);
(40, [EatInstr(108,95)]);
(424, [EatInstr(116,457)]);
(41, [EatInstr(112,96)]);
(425, [EatInstr(101,458)]);
(42, [EatInstr(112,97)]);
(426, [EatInstr(101,459)]);
(43, [EatInstr(112,98)]);
(427, [ASimpleCont2Instr(271,__binder4,460);ACallInstr3(__default_call,8)]);
(44, [EatInstr(112,99)]);
(428, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,461)]);
(45, [EatInstr(112,100)]);
(429, [EatInstr(115,462)]);
(46, [EatInstr(112,101)]);
(430, [EatInstr(97,463)]);
(47, [EatInstr(114,102)]);
(431, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,464)]);
(48, [EatInstr(115,103)]);
(432, [EatInstr(115,465)]);
(49, [EatInstr(116,104)]);
(433, [EatInstr(99,466)]);
(50, [EatInstr(116,105)]);
(434, [EatInstr(114,467)]);
(51, [EatInstr(45,106)]);
(435, [EatInstr(45,468)]);
(52, [EatInstr(45,107)]);
(436, [EatInstr(105,469)]);
(53, [EatInstr(45,108)]);
(437, [EatInstr(115,470)]);
(54, [EatInstr(45,109)]);
(438, [EatInstr(97,471)]);
(55, [EatInstr(45,110)]);
(439, [EatInstr(97,472)]);
(56, [EatInstr(45,111)]);
(440, [EatInstr(101,473)]);
(57, [EatInstr(45,112)]);
(441, [AAction2Instr(__a82,474)]);
(58, [EatInstr(45,113)]);
(442, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,475)]);
(59, [EatInstr(45,114)]);
(443, [EatInstr(114,476)]);
(60, [EatInstr(45,115)]);
(444, [EatInstr(117,477)]);
(61, [EatInstr(45,116)]);
(445, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,478)]);
(62, [EatInstr(45,117)]);
(446, [EatInstr(114,479)]);
(63, [EatInstr(45,118)]);
(447, [AAction2Instr(__a83,138)]);
(64, [EatInstr(45,119)]);
(448, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,480)]);
(65, [EatInstr(45,120)]);
(449, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,481)]);
(66, [EatInstr(45,121)]);
(450, [EatInstr(45,482)]);
(67, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,122)]);
(451, [EatInstr(104,483)]);
(68, [CompleteInstr(274)]);
(452, [EatInstr(101,484)]);
(69, [AAction2Instr(__a40,123)]);
(453, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,485)]);
(70, [AAction2Instr(__a41,17)]);
(454, [EatInstr(101,486)]);
(71, [AWhenInstr3(__p43,__p42,124)]);
(455, [EatInstr(108,487)]);
(72, [CompleteInstr(270)]);
(456, [EatInstr(101,488)]);
(73, [EatInstr(116,125)]);
(457, [EatInstr(101,489)]);
(74, [EatInstr(111,126)]);
(458, [EatInstr(45,490)]);
(75, [EatInstr(112,127)]);
(459, [EatInstr(45,491)]);
(76, [EatInstr(115,128)]);
(460, [AAction2Instr(__a84,280)]);
(77, [EatInstr(115,129)]);
(461, [AAction2Instr(__a85,492)]);
(78, [EatInstr(108,130)]);
(462, [EatInstr(101,493)]);
(79, [EatInstr(120,131)]);
(463, [EatInstr(98,494)]);
(80, [EatInstr(102,132)]);
(464, [AAction2Instr(__a86,495)]);
(81, [EatInstr(110,133)]);
(465, [EatInstr(116,496)]);
(82, [EatInstr(101,134)]);
(466, [EatInstr(115,497)]);
(83, [EatInstr(98,135)]);
(467, [EatInstr(101,498)]);
(84, [EatInstr(114,136)]);
(468, [EatInstr(104,499)]);
(85, [EatInstr(97,137)]);
(469, [EatInstr(122,500)]);
(86, [AAction2Instr(__a44,138)]);
(470, [EatInstr(116,501)]);
(87, [EatInstr(111,139)]);
(471, [EatInstr(121,502)]);
(88, [EatInstr(105,140)]);
(472, [EatInstr(100,503)]);
(89, [EatInstr(111,141)]);
(473, [EatInstr(115,504)]);
(90, [EatInstr(120,142)]);
(474, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,505)]);
(91, [EatInstr(120,143)]);
(475, [AAction2Instr(__a87,286)]);
(92, [EatInstr(117,144)]);
(476, [EatInstr(45,506)]);
(93, [EatInstr(110,145)]);
(477, [EatInstr(108,507)]);
(94, [EatInstr(111,146)]);
(478, [AAction2Instr(__a88,286)]);
(95, [EatInstr(114,147)]);
(479, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(96, [EatInstr(114,148)]);
(480, [AAction2Instr(__a89,509)]);
(97, [EatInstr(114,149)]);
(481, [AAction2Instr(__a90,138)]);
(98, [EatInstr(114,150)]);
(482, [EatInstr(97,510)]);
(99, [EatInstr(114,151)]);
(483, [EatInstr(101,511)]);
(100, [EatInstr(114,152)]);
(484, [EatInstr(45,512)]);
(101, [EatInstr(114,153)]);
(485, [AAction2Instr(__a91,138)]);
(102, [EatInstr(102,154)]);
(486, [EatInstr(100,513)]);
(103, [EatInstr(116,155)]);
(487, [EatInstr(97,514)]);
(104, [EatInstr(114,156)]);
(488, [EatInstr(118,515)]);
(105, [EatInstr(114,157)]);
(489, [EatInstr(45,516)]);
(106, [EatInstr(97,158)]);
(490, [EatInstr(100,517)]);
(107, [EatInstr(98,159)]);
(491, [EatInstr(100,518)]);
(108, [EatInstr(99,160)]);
(492, [EatInstr(116,521);EatInstr(112,520);EatInstr(102,519)]);
(109, [EatInstr(99,161)]);
(493, [EatInstr(110,522)]);
(110, [EatInstr(117,163);EatInstr(99,162)]);
(494, [EatInstr(101,523)]);
(111, [EatInstr(105,164)]);
(495, [AAction2Instr(__a92,525);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,524)]);
(112, [EatInstr(105,165)]);
(496, [EatInstr(97,526)]);
(113, [EatInstr(109,166)]);
(497, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,527)]);
(114, [EatInstr(110,167)]);
(498, [EatInstr(103,528)]);
(115, [EatInstr(117,168)]);
(499, [EatInstr(105,529)]);
(116, [EatInstr(110,169)]);
(500, [EatInstr(101,530)]);
(117, [EatInstr(108,170)]);
(501, [EatInstr(111,531)]);
(118, [EatInstr(110,171)]);
(502, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,532)]);
(119, [EatInstr(111,172)]);
(503, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,533)]);
(120, [EatInstr(114,173)]);
(504, [EatInstr(99,534)]);
(121, [EatInstr(118,174)]);
(505, [AAction2Instr(__a93,280)]);
(122, [AAction2Instr(__a45,175)]);
(506, [EatInstr(99,535)]);
(123, [AAction2Instr(__a47,177);AAction2Instr(__a46,176)]);
(124, [ALookaheadInstr(false,CfgLA (1,264),178)]);
(507, [EatInstr(97,536)]);
(508, [AAction2Instr(__a94,286)]);
(125, [EatInstr(114,179)]);
(509, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,537)]);
(126, [EatInstr(115,180)]);
(510, [EatInstr(110,538)]);
(127, [EatInstr(121,181)]);
(511, [EatInstr(97,539)]);
(128, [EatInstr(117,182)]);
(512, [EatInstr(97,540)]);
(129, [EatInstr(104,183)]);
(513, [EatInstr(115,541)]);
(130, [EatInstr(105,184)]);
(514, [EatInstr(98,542)]);
(131, [EatInstr(101,185)]);
(515, [EatInstr(97,543)]);
(132, [EatInstr(116,186)]);
(516, [EatInstr(97,544)]);
(133, [EatInstr(117,187)]);
(517, [EatInstr(121,545)]);
(134, [EatInstr(99,188)]);
(518, [EatInstr(121,546)]);
(135, [EatInstr(115,189)]);
(519, [EatInstr(117,547)]);
(136, [EatInstr(111,190)]);
(520, [EatInstr(101,548)]);
(137, [EatInstr(112,191)]);
(521, [EatInstr(120,549)]);
(138, [AAction2Instr(__a48,192)]);
(522, [EatInstr(115,550)]);
(139, [EatInstr(109,193)]);
(523, [EatInstr(108,551)]);
(140, [EatInstr(115,194)]);
(524, [AAction2Instr(__a95,495)]);
(141, [EatInstr(116,195)]);
(525, [AWhenInstr3(__p97,__p96,552)]);
(142, [EatInstr(101,196)]);
(526, [EatInstr(114,553)]);
(143, [EatInstr(116,197)]);
(527, [AAction2Instr(__a98,280)]);
(144, [EatInstr(115,198)]);
(528, [EatInstr(117,554)]);
(145, [EatInstr(102,199)]);
(529, [EatInstr(115,555)]);
(146, [EatInstr(111,200)]);
(530, [EatInstr(45,556)]);
(147, [EatInstr(49,201)]);
(531, [EatInstr(114,557)]);
(148, [EatInstr(101,202)]);
(532, [AAction2Instr(__a99,280)]);
(149, [EatInstr(105,203)]);
(533, [AAction2Instr(__a100,280)]);
(150, [EatInstr(105,204)]);
(534, [EatInstr(101,558)]);
(151, [EatInstr(105,205)]);
(535, [EatInstr(111,559)]);
(152, [EatInstr(105,206)]);
(536, [EatInstr(114,560)]);
(153, [EatInstr(105,207)]);
(537, [AAction2Instr(__a101,414)]);
(154, [EatInstr(99,208)]);
(538, [EatInstr(97,561)]);
(155, [EatInstr(114,209)]);
(539, [EatInstr(100,562)]);
(156, [EatInstr(97,210)]);
(540, [EatInstr(110,563)]);
(157, [EatInstr(97,211)]);
(541, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,564)]);
(158, [EatInstr(102,212)]);
(542, [EatInstr(108,565)]);
(159, [EatInstr(97,213)]);
(543, [EatInstr(110,566)]);
(160, [EatInstr(97,214)]);
(544, [EatInstr(99,567)]);
(161, [EatInstr(104,215)]);
(545, [EatInstr(112,568)]);
(162, [EatInstr(111,216)]);
(546, [EatInstr(112,569)]);
(163, [EatInstr(110,217)]);
(547, [EatInstr(110,570)]);
(164, [EatInstr(110,218)]);
(548, [EatInstr(103,571)]);
(165, [EatInstr(110,219)]);
(549, [AAction2Instr(__a102,572)]);
(166, [EatInstr(101,220)]);
(550, [EatInstr(105,573)]);
(167, [EatInstr(111,221)]);
(551, [EatInstr(115,574)]);
(168, [EatInstr(110,222)]);
(552, [AAction2Instr(__a103,575)]);
(169, [EatInstr(111,223)]);
(553, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,576)]);
(170, [EatInstr(111,224)]);
(554, [EatInstr(108,577)]);
(171, [EatInstr(111,225)]);
(555, [EatInstr(116,578)]);
(172, [EatInstr(110,226)]);
(556, [EatInstr(104,579)]);
(173, [EatInstr(111,227)]);
(557, [EatInstr(121,580)]);
(174, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,228)]);
(558, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,581)]);
(175, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,229)]);
(559, [EatInstr(114,582)]);
(176, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder3,230)]);
(560, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,583)]);
(177, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,231)]);
(561, [EatInstr(108,584)]);
(178, [CompleteInstr(269)]);
(562, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,585)]);
(179, [EatInstr(105,232)]);
(563, [EatInstr(97,586)]);
(180, [EatInstr(101,233)]);
(564, [AAction2Instr(__a104,138)]);
(181, [EatInstr(114,234)]);
(565, [EatInstr(101,587)]);
(182, [EatInstr(103,235)]);
(566, [EatInstr(99,588)]);
(183, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,236)]);
(567, [EatInstr(116,589)]);
(184, [EatInstr(110,237)]);
(568, [EatInstr(103,590)]);
(185, [EatInstr(114,238)]);
(569, [EatInstr(103,591)]);
(186, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,239)]);
(570, [AAction2Instr(__a105,572)]);
(187, [EatInstr(115,240)]);
(571, [EatInstr(45,592);AAction2Instr(__a106,572)]);
(188, [EatInstr(101,241)]);
(572, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,593)]);
(189, [EatInstr(101,242)]);
(573, [EatInstr(116,594)]);
(190, [EatInstr(108,243)]);
(574, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,595)]);
(191, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,244)]);
(575, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,596)]);
(192, [CompleteInstr(272)]);
(576, [AAction2Instr(__a107,597)]);
(193, [EatInstr(112,245)]);
(577, [EatInstr(97,598)]);
(194, [EatInstr(112,246)]);
(578, [EatInstr(111,599)]);
(195, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,247)]);
(579, [EatInstr(105,600)]);
(196, [EatInstr(99,248)]);
(580, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,601)]);
(197, [EatInstr(114,249)]);
(581, [AAction2Instr(__a108,280)]);
(198, [EatInstr(101,250)]);
(582, [EatInstr(101,602)]);
(199, [EatInstr(111,251)]);
(583, [AAction2Instr(__a109,286)]);
(200, [EatInstr(107,252)]);
(584, [EatInstr(121,603)]);
(201, [EatInstr(45,253)]);
(585, [AAction2Instr(__a110,138)]);
(202, [EatInstr(99,254)]);
(586, [EatInstr(108,604)]);
(203, [EatInstr(110,255)]);
(587, [EatInstr(45,605)]);
(204, [EatInstr(110,256)]);
(588, [EatInstr(101,606)]);
(205, [EatInstr(110,257)]);
(589, [EatInstr(105,607)]);
(206, [EatInstr(110,258)]);
(590, [EatInstr(101,608)]);
(207, [EatInstr(110,259)]);
(591, [EatInstr(101,609)]);
(208, [AAction2Instr(__a49,260)]);
(592, [EatInstr(115,610)]);
(209, [EatInstr(105,261)]);
(593, [AAction2Instr(__a111,280)]);
(210, [EatInstr(110,262)]);
(594, [EatInstr(105,611)]);
(211, [EatInstr(110,263)]);
(595, [AAction2Instr(__a112,280)]);
(212, [EatInstr(116,264)]);
(596, [AAction2Instr(__a113,280)]);
(213, [EatInstr(99,265)]);
(597, [AAction2Instr(__a114,613);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,612)]);
(214, [EatInstr(115,266)]);
(598, [EatInstr(114,614)]);
(215, [EatInstr(101,267)]);
(599, [EatInstr(114,615)]);
(216, [EatInstr(117,268)]);
(600, [EatInstr(115,616)]);
(217, [EatInstr(114,269)]);
(601, [AAction2Instr(__a115,280)]);
(218, [EatInstr(108,270)]);
(602, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,617)]);
(219, [EatInstr(108,271)]);
(603, [EatInstr(115,618)]);
(220, [EatInstr(109,272)]);
(604, [EatInstr(121,619)]);
(221, [EatInstr(45,273)]);
(605, [EatInstr(112,620)]);
(222, [EatInstr(105,274)]);
(606, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,621)]);
(223, [EatInstr(45,275)]);
(607, [EatInstr(111,622)]);
(224, [EatInstr(111,276)]);
(608, [EatInstr(110,623)]);
(225, [EatInstr(45,277)]);
(609, [EatInstr(110,624)]);
(226, [EatInstr(108,278)]);
(610, [EatInstr(116,625)]);
(227, [EatInstr(111,279)]);
(611, [EatInstr(118,626)]);
(228, [AAction2Instr(__a50,280)]);
(612, [AAction2Instr(__a116,597)]);
(229, [AAction2Instr(__a51,280)]);
(613, [AWhenInstr3(__p118,__p117,627)]);
(230, [AAction2Instr(__a52,123)]);
(614, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,628)]);
(231, [AAction2Instr(__a53,281)]);
(615, [EatInstr(121,629)]);
(232, [EatInstr(98,282)]);
(616, [EatInstr(116,630)]);
(233, [EatInstr(45,283)]);
(617, [AAction2Instr(__a119,286)]);
(234, [EatInstr(117,284)]);
(618, [EatInstr(105,631)]);
(235, [EatInstr(97,285)]);
(619, [EatInstr(115,632)]);
(236, [AAction2Instr(__a54,286)]);
(620, [EatInstr(114,633)]);
(237, [EatInstr(101,287)]);
(621, [AAction2Instr(__a120,138)]);
(238, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,288)]);
(622, [EatInstr(110,634)]);
(239, [AAction2Instr(__a55,286)]);
(623, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,635)]);
(240, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,289)]);
(624, [EatInstr(45,636)]);
(241, [EatInstr(100,290)]);
(625, [EatInstr(114,637)]);
(242, [EatInstr(116,291)]);
(626, [EatInstr(101,638)]);
(243, [EatInstr(108,292)]);
(627, [AAction2Instr(__a121,639)]);
(244, [AAction2Instr(__a56,286)]);
(628, [AAction2Instr(__a122,280)]);
(245, [EatInstr(105,293)]);
(629, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,640)]);
(246, [EatInstr(97,294)]);
(630, [EatInstr(111,641)]);
(247, [AAction2Instr(__a57,138)]);
(631, [EatInstr(115,642)]);
(248, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,295)]);
(632, [EatInstr(105,643)]);
(249, [EatInstr(97,296)]);
(633, [EatInstr(101,644)]);
(250, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,297)]);
(634, [EatInstr(115,645)]);
(251, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,298)]);
(635, [AAction2Instr(__a123,138)]);
(252, [EatInstr(97,299)]);
(636, [EatInstr(115,646)]);
(253, [EatInstr(108,300)]);
(637, [EatInstr(105,647)]);
(254, [EatInstr(101,301)]);
(638, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,648)]);
(255, [EatInstr(116,302)]);
(639, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,649)]);
(256, [EatInstr(116,303)]);
(640, [AAction2Instr(__a124,280)]);
(257, [EatInstr(116,304)]);
(641, [EatInstr(114,650)]);
(258, [EatInstr(116,305)]);
(642, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,651)]);
(259, [EatInstr(116,306)]);
(643, [EatInstr(115,652)]);
(260, [AAction2Instr(__a58,308);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,307)]);
(644, [EatInstr(100,653)]);
(261, [EatInstr(112,309)]);
(645, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,654)]);
(262, [EatInstr(115,310)]);
(646, [EatInstr(99,655)]);
(263, [EatInstr(115,311)]);
(647, [EatInstr(99,656)]);
(264, [EatInstr(101,312)]);
(648, [AAction2Instr(__a125,280)]);
(265, [EatInstr(107,313)]);
(649, [AAction2Instr(__a126,280)]);
(266, [EatInstr(101,314)]);
(650, [EatInstr(121,657)]);
(267, [EatInstr(99,315)]);
(651, [AAction2Instr(__a127,138)]);
(268, [EatInstr(110,316)]);
(652, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,658)]);
(269, [EatInstr(111,317)]);
(653, [EatInstr(105,659)]);
(270, [EatInstr(105,318)]);
(654, [AAction2Instr(__a128,138)]);
(271, [EatInstr(105,319)]);
(655, [EatInstr(97,660)]);
(272, [EatInstr(111,320)]);
(656, [EatInstr(116,661)]);
(273, [EatInstr(109,321)]);
(657, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,662)]);
(274, [EatInstr(116,322)]);
(658, [AAction2Instr(__a129,138)]);
(275, [EatInstr(114,323)]);
(659, [EatInstr(99,663)]);
(276, [EatInstr(107,324)]);
(660, [EatInstr(110,664)]);
(277, [EatInstr(99,325)]);
(661, [AAction2Instr(__a130,572)]);
(278, [EatInstr(121,326)]);
(662, [AAction2Instr(__a131,280)]);
(279, [EatInstr(116,327)]);
(663, [EatInstr(97,665)]);
(280, [AAction2Instr(__a59,328)]);
(664, [EatInstr(110,666)]);
(281, [CompleteInstr(267)]);
(665, [EatInstr(116,667)]);
(282, [EatInstr(117,329)]);
(666, [EatInstr(101,668)]);
(283, [EatInstr(117,330)]);
(667, [EatInstr(101,669)]);
(284, [EatInstr(108,331)]);
(668, [EatInstr(114,670)]);
(285, [EatInstr(114,332)]);
(669, [EatInstr(115,671)]);
(286, [CompleteInstr(271)]);
(670, [EatInstr(108,672)]);
(287, [EatInstr(45,333)]);
(671, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,673)]);
(288, [AAction2Instr(__a60,286)]);
(672, [EatInstr(101,674)]);
(289, [AAction2Instr(__a61,286)]);
(673, [AAction2Instr(__a132,138)]);
(290, [EatInstr(101,334)]);
(674, [EatInstr(115,675)]);
(291, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,335)]);
(675, [EatInstr(115,676)]);
(292, [EatInstr(45,336)]);
(676, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,677)]);
(293, [EatInstr(108,337)]);
(677, [AAction2Instr(__a133,138)]);
(294, [EatInstr(116,338)]);
(295, [AAction2Instr(__a62,339)]);
(296, [EatInstr(99,340)]);
(297, [AAction2Instr(__a63,138)]);
(298, [AAction2Instr(__a64,138)]);
(299, [EatInstr(104,341)]);
(300, [EatInstr(111,342)]);
(301, [EatInstr(100,343)]);
(302, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,344)]);
(303, [EatInstr(45,345)]);
(304, [EatInstr(45,346)]);
(305, [EatInstr(45,347)]);
(306, [EatInstr(45,348)]);
(307, [AAction2Instr(__a65,260)]);
(308, [AWhenInstr3(__p67,__p66,349)]);
(309, [EatInstr(45,350)]);
(310, [EatInstr(108,351)]);
(311, [EatInstr(108,352)]);
(312, [EatInstr(114,353)]);
(313, [EatInstr(101,354)]);
(314, [EatInstr(45,355)]);
(315, [EatInstr(107,356)]);
(316, [EatInstr(116,357)]);
(317, [EatInstr(108,358)]);
(318, [EatInstr(110,359)]);
(319, [EatInstr(110,360)]);
(320, [EatInstr(105,361)]);
(321, [EatInstr(101,362)]);
(322, [EatInstr(45,363)]);
(323, [EatInstr(101,364)]);
(324, [EatInstr(97,365)]);
(325, [EatInstr(111,366)]);
(326, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,367)]);
(327, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,368)]);
(328, [CompleteInstr(273)]);
(329, [EatInstr(116,369)]);
(330, [EatInstr(110,370)]);
(331, [EatInstr(101,371)]);
(332, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,372)]);
(333, [EatInstr(114,373)]);
(334, [EatInstr(110,374)]);
(335, [AAction2Instr(__a68,286)]);
(336, [EatInstr(115,375)]);
(337, [EatInstr(101,376)]);
(338, [EatInstr(99,377)]);
(339, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,378)]);
(340, [EatInstr(116,379)]);
(341, [EatInstr(101,380)]);
(342, [EatInstr(111,381)]);
(343, [EatInstr(101,382)]);
(344, [AAction2Instr(__a69,138)]);
(345, [EatInstr(103,383)]);
(346, [EatInstr(110,384)]);
(347, [EatInstr(110,385)]);
(348, [EatInstr(114,386)]);
(349, [AAction2Instr(__a70,387)]);
(350, [EatInstr(108,388)]);
(351, [EatInstr(97,389)]);
(352, [EatInstr(97,390)]);
(353, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,391)]);
(354, [EatInstr(110,392)]);
(355, [EatInstr(105,393)]);
(356, [EatInstr(45,394)]);
(357, [EatInstr(101,395)]);
(358, [EatInstr(108,396)]);
(359, [EatInstr(101,397)]);
(360, [EatInstr(101,398)]);
(361, [EatInstr(122,399)]);
(362, [EatInstr(109,400)]);
(363, [EatInstr(104,401)]);
(364, [EatInstr(112,402)]);
(365, [EatInstr(104,403)]);
(366, [EatInstr(97,404)]);
(367, [AAction2Instr(__a71,280)]);
(368, [AAction2Instr(__a72,405)]);
(369, [EatInstr(101,406)]);
(370, [EatInstr(100,407)]);
(371, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,408)]);
(372, [AAction2Instr(__a73,286)]);
(373, [EatInstr(101,409)]);
(374, [EatInstr(99,410)]);
(375, [EatInstr(116,411)]);
(376, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,412)]);
(377, [EatInstr(104,413)]);
(378, [AAction2Instr(__a74,414)]);
(379, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,415)]);
(380, [EatInstr(97,416)]);
(381, [EatInstr(107,417)]);
(382, [EatInstr(110,418)]);
]

let start_symb = get_symb_action "cmd-line-args"

module P2__ = Yak.Engine.Full_yakker(struct type t = sv let cmp = sv_compare
                                            type idata = Yk_History.Root_id_set.t
  let create_idata () = Yk_History.Root_id_set.empty
  let inspect (_,h) s = Yk_History.add_id_set h#get_root s
  let summarize_inspection s = string_of_int (Yk_History.Root_id_set.cardinal s) end)

let _wfe_data_ = Yak.PamJIT.DNELR.to_table (Yak.Pam_internal.load_internal_program program)
  start_symb (get_symb_start start_symb) 264 num_symbols
  __default_call __default_ret

let parse = Yak.Pami.Wfe.mk_parse P2__.parse _wfe_data_ sv0 
    (fun ykinput (_,h) ->
      let _o = (h#traverse_postfix) in
      let _n() = (let (x,_) = _o#next() in x) in
      _r_cmd_line_args(_n,ykinput)
    )

let visualize = parse
let visualize_file = Yak.Pami.Simple.parse_file visualize
let visualize_string = Yak.Pami.Simple.parse_string visualize

let parse_file = Yak.Pami.Simple.parse_file parse
let parse_string = Yak.Pami.Simple.parse_string parse
;;
Yk_History.memoize := false;;


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
