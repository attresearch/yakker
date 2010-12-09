
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
 (let _x41 = (ignore (*1004*) (_n()); 
 (let _x4 = (ignore (*1005*) (_n()); 
 (let rec _x43 _x4 = (match _n() with 1006 -> _x4 | _x42 -> _x43((ignore (*1007*) (_x42); 
 (let _x3 = (ignore (*1008*) (_n()); _r_args(_n,ykinput))
  in (ignore (*1010*) (_n()); _x3::_x4)
 ))
 )) in _x43([])))
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
 (let _x39 = 
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
 | (1094) -> (
 (let _x9 = (ignore (*1095*) (_n()); _n())
  in (ignore (*1096*) (_n()); 
 (let _x8 = (ignore (*1097*) (_n()); _n())
  in (ignore (*1098*) (_n()); 
 (let f = (ignore (*1099*) (_n()); Yak.YkBuf.get_string _x9 _x8 ykinput)
  in (ignore (*1100*) (_n()); 
 (let l = (ignore (*1101*) (_n()); 
 (let _x15 = (ignore (*1102*) (_n()); 
 (let rec _x45 _x15 = (match _n() with 1103 -> _x15 | _x44 -> _x45((ignore (*1104*) (_x44); 
 (let _x14 = (ignore (*1113*) (_n()); 
 (let _x13 = (ignore (*1114*) (_n()); _n())
  in (ignore (*1115*) (_n()); 
 (let _x12 = (ignore (*1116*) (_n()); _n())
  in (ignore (*1117*) (_n()); 
 (let x = (ignore (*1118*) (_n()); Yak.YkBuf.get_string _x13 _x12 ykinput)
  in (ignore (*1119*) (_n()); x)
 ))
 ))
 ))
  in (ignore (*1120*) (_n()); _x14::_x15)
 ))
 )) in _x45([])))
  in (ignore (*1121*) (_n()); (List.rev _x15))
 ))
  in (ignore (*1123*) (_n());  files := f::!files; exec_l := l; Exec_cmd )
 ))
 ))
 ))
 ))
 | (1128) -> ( Extract_cmd )
 | (1133) -> ( Compileopt.coalesce := true; Fuse_cmd )
 | (1138) -> ( Info_cmd )
 | (1143) -> ( Lookahead_analysis_cmd )
 | (1148) -> ( Lr1_lookahead_cmd )
 | (1153) -> ( Precedence_analysis_cmd )
 | (1158) -> ( Print_gul_cmd )
 | (1163) -> ( Print_gil_cmd )
 | (1168) -> ( Print_npreds_cmd )
 | (1173) -> ( Print_npreds_cmd )
 | (1178) -> ( Print_relevance_cmd )
 | (1195) -> (
 (let _x20 = (ignore (*1196*) (_n()); _n())
  in (ignore (*1197*) (_n()); 
 (let _x19 = (ignore (*1198*) (_n()); _n())
  in (ignore (*1199*) (_n()); 
 (let n = (ignore (*1200*) (_n()); Yak.YkBuf.get_string _x20 _x19 ykinput)
  in (ignore (*1202*) (_n());  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" )
 ))
 ))
 ))
 | (1209) -> ( Strip_late_actions_cmd )
 | (1216) -> ( Translate_dypgen_cmd )
 | _(*1221*) -> ( Translate_dypgen_scannerless_cmd )
 ) in (ignore (*1222*) (_n()); _x39)
 ))
 
 and
_r_args(_n,ykinput) = (ignore (*1223*) (_n()); 
 (let _x40 = 
 (match _n() with
 | (1228) -> (
 (let p = (ignore (*1229*) (_n()); _r_phases(_n,ykinput))
  in (ignore (*1231*) (_n());  after := Some p )
 ))
 | (1236) -> (
 (let b = 
 (match _n() with
 | (1238) -> (Fun_BE)
 | (1240) -> (Trans_BE)
 | (1242) -> (Peg_BE false)
 | _(*1244*) -> (Peg_BE true)
 ) in (ignore (*1246*) (_n());  backend := b )
 ))
 | (1251) -> ( Compileopt.case_sensitive := false )
 | (1256) -> ( Compileopt.check_labels := true )
 | (1274) -> (
 (let _x25 = (ignore (*1275*) (_n()); _n())
  in (ignore (*1276*) (_n()); 
 (let _x24 = (ignore (*1277*) (_n()); _n())
  in (ignore (*1278*) (_n()); 
 (let n = (ignore (*1279*) (_n()); Yak.YkBuf.get_string _x25 _x24 ykinput)
  in (ignore (*1281*) (_n());  Variables.counter := (int_of_string n) )
 ))
 ))
 ))
 | (1286) -> ( Compileopt.inline_cs := true )
 | (1291) -> ( Compileopt.inline_regular := true )
 | (1296) -> ( Compileopt.memoize_history := true )
 | (1301) -> ( Compileopt.memoize_history := false )
 | (1306) -> ( Compileopt.unit_history := true )
 | (1311) -> ( Compileopt.repress_replay := true )
 | (1316) -> ( Compileopt.lookahead := true )
 | (1321) -> ( Compileopt.coalesce := false )
 | (1326) -> ( only := true )
 | (1336) -> (
 (let _x29 = (ignore (*1337*) (_n()); _n())
  in (ignore (*1338*) (_n()); 
 (let _x28 = (ignore (*1339*) (_n()); _n())
  in (ignore (*1340*) (_n()); 
 (let x = (ignore (*1341*) (_n()); Yak.YkBuf.get_string _x29 _x28 ykinput)
  in (ignore (*1343*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | (1361) -> (
 (let _x34 = (ignore (*1362*) (_n()); _n())
  in (ignore (*1363*) (_n()); 
 (let _x33 = (ignore (*1364*) (_n()); _n())
  in (ignore (*1365*) (_n()); 
 (let n = (ignore (*1366*) (_n()); Yak.YkBuf.get_string _x34 _x33 ykinput)
  in (ignore (*1368*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1373) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | _(*1381*) -> (
 (let _x38 = (ignore (*1382*) (_n()); _n())
  in (ignore (*1383*) (_n()); 
 (let _x37 = (ignore (*1384*) (_n()); _n())
  in (ignore (*1385*) (_n()); 
 (let f = (ignore (*1386*) (_n()); Yak.YkBuf.get_string _x38 _x37 ykinput)
  in (ignore (*1388*) (_n());  files := f::!files )
 ))
 ))
 ))
 ) in (ignore (*1389*) (_n()); _x40)
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
let _m x p = (fun(v1,h1)->fun(_,h2)-> (v1,h1#merge p ((x),p) h2))

let sv_eq x y = sv_compare x y = 0
let key_eq (i,v1) (j,v2) = i = j &&  sv_eq v1 v2
let key_hash (i,v) = i lxor (sv_hash v)

(** Hashtable for top-down parsing. *)
module TDHashtable = Hashtbl.Make(struct type t = int * sv let equal = key_eq let hash = key_hash end)

let _x49 =
 (fun _(*pos*) (_,_x46)(*arg of arg*) -> (_t(fun _(*1016*) pos_ -> let _x47 _x5  = _t(function
 | 1022 ->
 (fun pos_ -> Yk_when(_x5>=1))
 | _(*1023*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1017*) pos_ -> let rec _x48 _x5  = _t(function
 | 1018 ->
 (fun pos_ -> _x47 (_x5) )
 | _(*1020*) ->
 (fun pos_ -> _x48 (_x5+1) )) in _x48 (0) )),_x46))
let _x75 =
 (fun _(*pos*) (_,_x50)(*arg of command*) -> (_t(function
 | 1065 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1071 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1076 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1081 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1087 ->
 (fun pos_ -> let _x63 _x7  = _t(fun _(*1091*) pos_ -> let _x64 _x6  = _t(fun _(*1094*) pos_ -> let _x66 _x65  = _t(fun _(*1096*) pos_ -> let _x68 _x67  = _t(fun _(*1102*) pos_ -> let rec _x70 _x69  = _t(function
 | 1103 ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore(_x69);_wv0);_wv0);_wv0))
 | _(*1106*) ->
 (fun pos_ -> let _x71 _x11  = _t(fun _(*1110*) pos_ -> let _x72 _x10  = _t(fun _(*1113*) pos_ -> let _x74 _x73  = _t(fun _(*1116*) pos_ -> Yk_delay(_x70 (ignore(ignore((_wv0));_wv0);_wv0) ,_x10)) in _t(fun _(*1114*) pos_ -> Yk_delay(_x74 ((_wv0)) ,_x11))) in _t(fun _(*1111*) pos_ -> _x72 (pos_) )) in _t(fun _(*1107*) pos_ -> _x71 (pos_) ))) in _x70 (_wv0) ) in _t(fun _(*1097*) pos_ -> Yk_delay(_x68 ((_wv0)) ,_x6))) in _t(fun _(*1095*) pos_ -> Yk_delay(_x66 ((_wv0)) ,_x7))) in _t(fun _(*1092*) pos_ -> _x64 (pos_) )) in _t(fun _(*1088*) pos_ -> _x63 (pos_) ))
 | 1125 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1130 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1135 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1140 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1145 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1150 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1155 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1160 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1165 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1170 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1175 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1180 ->
 (fun pos_ -> let _x55 _x18  = _t(fun _(*1183*) pos_ -> let _x57 _x56  = _t(fun _(*1192*) pos_ -> let _x60 _x17  = _t(fun _(*1195*) pos_ -> let _x62 _x61  = _t(fun _(*1198*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x17)) in _t(fun _(*1196*) pos_ -> Yk_delay(_x62 ((_wv0)) ,_x18))) in _t(fun _(*1193*) pos_ -> _x60 (pos_) )) in _t(fun _(*1184*) pos_ -> let _x58 _x16  = _t(function
 | 1190 ->
 (fun pos_ -> Yk_when(_x16>=1))
 | _(*1191*) ->
 (fun pos_ -> _x57 (ignore((_wv0));_wv0) )) in _t(fun _(*1185*) pos_ -> let rec _x59 _x16  = _t(function
 | 1186 ->
 (fun pos_ -> _x58 (_x16) )
 | _(*1188*) ->
 (fun pos_ -> _x59 (_x16+1) )) in _x59 (0) ))) in _t(fun _(*1181*) pos_ -> _x55 (pos_) ))
 | _(*1203*) ->
 (fun pos_ -> let _x52 _x51  = _t(function
 | 1206 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1210*) ->
 (fun pos_ -> let _x54 _x53  = _t(function
 | 1213 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1218*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1211*) pos_ -> _x54 (()) ))) in _t(fun _(*1204*) pos_ -> _x52 (()) ))),_x50))
let _x101 =
 (fun _(*pos*) (_,_x76)(*arg of args*) -> (_t(function
 | 1225 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1233 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1248 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1253 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1259 ->
 (fun pos_ -> let _x93 _x23  = _t(fun _(*1262*) pos_ -> let _x95 _x94  = _t(fun _(*1271*) pos_ -> let _x98 _x22  = _t(fun _(*1274*) pos_ -> let _x100 _x99  = _t(fun _(*1277*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x22)) in _t(fun _(*1275*) pos_ -> Yk_delay(_x100 ((_wv0)) ,_x23))) in _t(fun _(*1272*) pos_ -> _x98 (pos_) )) in _t(fun _(*1263*) pos_ -> let _x96 _x21  = _t(function
 | 1269 ->
 (fun pos_ -> Yk_when(_x21>=1))
 | _(*1270*) ->
 (fun pos_ -> _x95 (ignore((_wv0));_wv0) )) in _t(fun _(*1264*) pos_ -> let rec _x97 _x21  = _t(function
 | 1265 ->
 (fun pos_ -> _x96 (_x21) )
 | _(*1267*) ->
 (fun pos_ -> _x97 (_x21+1) )) in _x97 (0) ))) in _t(fun _(*1260*) pos_ -> _x93 (pos_) ))
 | 1283 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1288 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1293 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1298 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1303 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1308 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1313 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1318 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1323 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1329 ->
 (fun pos_ -> let _x89 _x27  = _t(fun _(*1333*) pos_ -> let _x90 _x26  = _t(fun _(*1336*) pos_ -> let _x92 _x91  = _t(fun _(*1339*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x26)) in _t(fun _(*1337*) pos_ -> Yk_delay(_x92 ((_wv0)) ,_x27))) in _t(fun _(*1334*) pos_ -> _x90 (pos_) )) in _t(fun _(*1330*) pos_ -> _x89 (pos_) ))
 | 1346 ->
 (fun pos_ -> let _x81 _x32  = _t(fun _(*1349*) pos_ -> let _x83 _x82  = _t(fun _(*1358*) pos_ -> let _x86 _x31  = _t(fun _(*1361*) pos_ -> let _x88 _x87  = _t(fun _(*1364*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x31)) in _t(fun _(*1362*) pos_ -> Yk_delay(_x88 ((_wv0)) ,_x32))) in _t(fun _(*1359*) pos_ -> _x86 (pos_) )) in _t(fun _(*1350*) pos_ -> let _x84 _x30  = _t(function
 | 1356 ->
 (fun pos_ -> Yk_when(_x30>=1))
 | _(*1357*) ->
 (fun pos_ -> _x83 (ignore((_wv0));_wv0) )) in _t(fun _(*1351*) pos_ -> let rec _x85 _x30  = _t(function
 | 1352 ->
 (fun pos_ -> _x84 (_x30) )
 | _(*1354*) ->
 (fun pos_ -> _x85 (_x30+1) )) in _x85 (0) ))) in _t(fun _(*1347*) pos_ -> _x81 (pos_) ))
 | 1370 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1374*) ->
 (fun pos_ -> let _x77 _x36  = _t(fun _(*1378*) pos_ -> let _x78 _x35  = _t(fun _(*1381*) pos_ -> let _x80 _x79  = _t(fun _(*1384*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x35)) in _t(fun _(*1382*) pos_ -> Yk_delay(_x80 ((_wv0)) ,_x36))) in _t(fun _(*1379*) pos_ -> _x78 (pos_) )) in _t(fun _(*1375*) pos_ -> _x77 (pos_) ))),_x76))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a23 = fun p v -> _d 1248 p (_p 1223 p (_x101 p (v)));;
let __a18 = fun p v -> _d 1206 p (_d 1204 p (_d 1203 p (_p 1063 p (_x75 p (v)))));;
let __a107 = _p 1041;;
let __a106 = _p 1321;;
let __a32 = fun p v -> _d 1313 p (_p 1223 p (_x101 p (v)));;
let __a127 = _p 1153;;
let __a48 = _p 1373;;
let __a20 = fun p v -> _d 1218 p (_d 1211 p (_d 1210 p (_d 1204 p (_d 1203 p (_p 1063 p (_x75 p (v)))))));;
let __a76 = _p 1104;;
let __a58 = _p 1044;;
let __a121 = _p 1216;;
let __a70 = _p 1326;;
let __a16 = fun p v -> _d 1170 p (_p 1063 p (_x75 p (v)));;
let __a53 = _p 1047;;
let __a37 = _d 1018;;
let __a7 = fun p v -> _d 1125 p (_p 1063 p (_x75 p (v)));;
let __a68 = _p 1158;;
let __a3 = fun p v -> _d 1071 p (_p 1063 p (_x75 p (v)));;
let __a2 = fun p v -> _p 1067 p (_p 1066 p (_d 1065 p (_p 1063 p (_x75 p (v)))));;
let __a6 = fun p v -> _p 1063 p (_x75 p (v));;
let __a35 = fun p v -> _d 1370 p (_p 1223 p (_x101 p (v)));;
let __a43 = fun p v -> _p 1386 p (_p 1385 p (_ddelay 1384 p (_p 1383 p (_ddelay 1382 p (_d_and_push 1381 p (_d 1379 p (_d 1378 p (v))))))));;
let __a99 = fun p v -> _p 1120 p (_p 1119 p (_p 1118 p (_p 1117 p (_ddelay 1116 p (_p 1115 p (_ddelay 1114 p (_d_and_push 1113 p (_d 1111 p (_d 1110 p (v))))))))));;
let __a26 = fun p v -> _d 1283 p (_p 1223 p (_x101 p (v)));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x49 p (v)));;
let __a59 = _p 1050;;
let __a39 = _d 1020;;
let __a131 = _p 1221;;
let __a112 = _d 1352;;
let __a46 = _p 1222;;
let __a24 = fun p v -> _d 1253 p (_p 1223 p (_x101 p (v)));;
let __a89 = _p 1163;;
let __a86 = _p 1053;;
let __a8 = fun p v -> _d 1130 p (_p 1063 p (_x75 p (v)));;
let __a114 = _d 1354;;
let __a47 = fun p v -> _d 1185 p (_d 1184 p (_d 1183 p (_d 1181 p (_d 1180 p (v)))));;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a11 = fun p v -> _d 1145 p (_p 1063 p (_x75 p (v)));;
let __a66 = _p 1056;;
let __a29 = fun p v -> _d 1298 p (_p 1223 p (_x101 p (v)));;
let __a56 = _d 1186;;
let __a102 = _p 1168;;
let __a61 = _d 1188;;
let __a49 = _p 1388;;
let __a92 = _p 1059;;
let __a57 = _p 1389;;
let __p40 = _dnext 1023;;
let __a36 = fun p v -> _d 1375 p (_d 1374 p (_p 1223 p (_x101 p (v))));;
let __p115 = _dnext 1357;;
let __a69 = fun p v -> _d 1330 p (_d 1329 p (v));;
let __a13 = fun p v -> _d 1155 p (_p 1063 p (_x75 p (v)));;
let __a25 = fun p v -> _p 1223 p (_x101 p (v));;
let __a30 = fun p v -> _d 1303 p (_p 1223 p (_x101 p (v)));;
let __a50 = _p 1010;;
let __a111 = _p 1281;;
let __p41 = _dwhen 1022;;
let __a82 = _p 1231;;
let __a54 = _p 1062;;
let __a130 = _p 1173;;
let __a119 = fun p v -> _p 1366 p (_p 1365 p (_ddelay 1364 p (_p 1363 p (_ddelay 1362 p (_d_and_push 1361 p (_d 1359 p (_d 1358 p (v))))))));;
let __a91 = _p 1343;;
let __a88 = _p 1123;;
let __a51 = _p 1013;;
let __a67 = fun p v -> _p 1200 p (_p 1199 p (_ddelay 1198 p (_p 1197 p (_ddelay 1196 p (_d_and_push 1195 p (_d 1193 p (_d 1192 p (v))))))));;
let __a33 = fun p v -> _d 1318 p (_p 1223 p (_x101 p (v)));;
let __p116 = _dwhen 1356;;
let __a96 = _p 1286;;
let __a83 = _p 1236;;
let __p62 = _dnext 1191;;
let __a77 = fun p v -> _p 1121 p (_d_and_push 1103 p (v));;
let __a118 = _p 1178;;
let __a103 = _p 1238;;
let __a79 = _p 1128;;
let __a14 = fun p v -> _d 1160 p (_p 1063 p (_x75 p (v)));;
let __a42 = _p 1069;;
let __a45 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a17 = fun p v -> _d 1175 p (_p 1063 p (_x75 p (v)));;
let __a4 = fun p v -> _d 1076 p (_p 1063 p (_x75 p (v)));;
let __p63 = _dwhen 1190;;
let __a100 = _p 1240;;
let __a129 = _p 1301;;
let __a120 = _p 1291;;
let __a105 = fun p v -> _d 1351 p (_d 1350 p (_d 1349 p (_d 1347 p (_d 1346 p (v)))));;
let __a44 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a27 = fun p v -> _d 1288 p (_p 1223 p (_x101 p (v)));;
let __a128 = _p 1242;;
let __a80 = fun p v -> _p 1341 p (_p 1340 p (_ddelay 1339 p (_p 1338 p (_ddelay 1337 p (_d_and_push 1336 p (_d 1334 p (_d 1333 p (v))))))));;
let __a64 = _p 1133;;
let __a104 = _p 1244;;
let __a75 = _p 1074;;
let __a101 = fun p v -> _p 1279 p (_p 1278 p (_ddelay 1277 p (_p 1276 p (_ddelay 1275 p (_d_and_push 1274 p (_d 1272 p (_d 1271 p (v))))))));;
let __a90 = _d 1265;;
let __a122 = _p 1296;;
let __a113 = _p 1306;;
let __a109 = _p 1246;;
let __a34 = fun p v -> _d 1323 p (_p 1223 p (_x101 p (v)));;
let __a85 = _p 1026;;
let __a73 = fun p v -> _p 1229 p (_p 1228 p (v));;
let __a9 = fun p v -> _d 1135 p (_p 1063 p (_x75 p (v)));;
let __a93 = _d 1267;;
let __a65 = _p 1138;;
let __a84 = fun p v -> _d 1264 p (_d 1263 p (_d 1262 p (_d 1260 p (_d 1259 p (v)))));;
let __a117 = _p 1029;;
let __a81 = _p 1079;;
let __a5 = fun p v -> _d 1081 p (_p 1063 p (_x75 p (v)));;
let __a72 = fun p v -> _d_and_push 1102 p (_p 1101 p (_p 1100 p (_p 1099 p (_p 1098 p (_ddelay 1097 p (_d_and_push 1096 p (_ddelay 1095 p (_d_and_push 1094 p (_d 1092 p (_d 1091 p (v)))))))))));;
let __a38 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a31 = fun p v -> _d 1308 p (_p 1223 p (_x101 p (v)));;
let __a123 = _p 1251;;
let __a97 = _p 1311;;
let __a87 = fun p v -> _d 1107 p (_d 1106 p (v));;
let __a74 = _p 1032;;
let __a10 = fun p v -> _d 1140 p (_p 1063 p (_x75 p (v)));;
let __a78 = _p 1202;;
let __a125 = _p 1143;;
let __a28 = fun p v -> _d 1293 p (_p 1223 p (_x101 p (v)));;
let __a60 = fun p v -> _d 1088 p (_d 1087 p (v));;
let __a19 = fun p v -> _d 1213 p (_d 1211 p (_d 1210 p (_d 1204 p (_d 1203 p (_p 1063 p (_x75 p (v)))))));;
let __a55 = _p 1084;;
let __a71 = _p 1035;;
let __a110 = _p 1256;;
let __a98 = _p 1316;;
let __p94 = _dnext 1270;;
let __a124 = _p 1368;;
let __a108 = _p 1148;;
let __p95 = _dwhen 1269;;
let __a52 = _p 1038;;
let __a126 = _p 1209;;
let __a12 = fun p v -> _d 1150 p (_p 1063 p (_x75 p (v)));;
let __a22 = fun p v -> _d 1233 p (_p 1223 p (_x101 p (v)));;
let __a21 = fun p v -> _d 1225 p (_p 1223 p (_x101 p (v)));;
let __a15 = fun p v -> _d 1165 p (_p 1063 p (_x75 p (v)));;
let __binder0 = __default_ret;;
let __binder1 = _m 1002;;
let __binder2 = _m 1068;;
let __binder3 = _m 1009;;
let __binder4 = _m 1230;;
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

and nullable_arg __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1022 and n = _dnext 1023 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (Pred.full_lookaheadc false 264 1)) __lookahead) _p0_) (((_d 1018) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1017 p (_d 1016 p (_x49 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_eof __lookahead _p0_ _x0_ = ((((Pred.full_lookaheadc false 266 3) __lookahead) _p0_) _x0_)

and nullable_OCTET __lookahead _p0_ _x0_ = None

let program : (int * sv instruction list) list = [
(383, [EatInstr(117,419)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(101,420)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [EatInstr(97,421)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(116,422)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(116,423)]);
(4, [AAction2Instr(__a0,15)]);
(388, [AAction2Instr(__a73,424)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(100,425)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,17)]);
(390, [EatInstr(110,426)]);
(7, [EatInstr(127,19);EatInstr(126,19);EatInstr(125,19);EatInstr(124,19);EatInstr(123,19);EatInstr(122,19);EatInstr(121,19);EatInstr(120,19);EatInstr(119,19);EatInstr(118,19);EatInstr(117,19);EatInstr(116,19);EatInstr(115,19);EatInstr(114,19);EatInstr(113,19);EatInstr(112,19);EatInstr(111,19);EatInstr(110,19);EatInstr(109,19);EatInstr(108,19);EatInstr(107,19);EatInstr(106,19);EatInstr(105,19);EatInstr(104,19);EatInstr(103,19);EatInstr(102,19);EatInstr(101,19);EatInstr(100,19);EatInstr(99,19);EatInstr(98,19);EatInstr(97,19);EatInstr(96,19);EatInstr(95,19);EatInstr(94,19);EatInstr(93,19);EatInstr(92,19);EatInstr(91,19);EatInstr(90,19);EatInstr(89,19);EatInstr(88,19);EatInstr(87,19);EatInstr(86,19);EatInstr(85,19);EatInstr(84,19);EatInstr(83,19);EatInstr(82,19);EatInstr(81,19);EatInstr(80,19);EatInstr(79,19);EatInstr(78,19);EatInstr(77,19);EatInstr(76,19);EatInstr(75,19);EatInstr(74,19);EatInstr(73,19);EatInstr(72,19);EatInstr(71,19);EatInstr(70,19);EatInstr(69,19);EatInstr(68,19);EatInstr(67,19);EatInstr(66,19);EatInstr(65,19);EatInstr(64,19);EatInstr(63,19);EatInstr(62,19);EatInstr(61,19);EatInstr(60,19);EatInstr(59,19);EatInstr(58,19);EatInstr(57,19);EatInstr(56,19);EatInstr(55,19);EatInstr(54,19);EatInstr(53,19);EatInstr(52,19);EatInstr(51,19);EatInstr(50,19);EatInstr(49,19);EatInstr(48,19);EatInstr(47,19);EatInstr(46,19);EatInstr(44,19);EatInstr(43,19);EatInstr(42,19);EatInstr(41,19);EatInstr(40,19);EatInstr(39,19);EatInstr(38,19);EatInstr(37,19);EatInstr(36,19);EatInstr(35,19);EatInstr(34,19);EatInstr(33,19);EatInstr(32,19);EatInstr(31,19);EatInstr(30,19);EatInstr(29,19);EatInstr(28,19);EatInstr(27,19);EatInstr(26,19);EatInstr(25,19);EatInstr(24,19);EatInstr(23,19);EatInstr(22,19);EatInstr(21,19);EatInstr(20,19);EatInstr(19,19);EatInstr(18,19);EatInstr(17,19);EatInstr(16,19);EatInstr(15,19);EatInstr(14,19);EatInstr(13,19);EatInstr(12,19);EatInstr(11,19);EatInstr(10,19);EatInstr(9,19);EatInstr(8,19);EatInstr(7,19);EatInstr(6,19);EatInstr(5,19);EatInstr(4,19);EatInstr(3,19);EatInstr(2,19);EatInstr(1,19)]);
(391, [EatInstr(108,427)]);
(8, [EatInstr(119,30);EatInstr(117,29);EatInstr(115,28);EatInstr(112,27);EatInstr(109,26);EatInstr(108,25);EatInstr(105,24);EatInstr(104,23);EatInstr(100,22);EatInstr(99,21);EatInstr(97,20)]);
(392, [EatInstr(114,428)]);
(9, [AAction2Instr(__a20,49);AAction2Instr(__a19,48);AAction2Instr(__a18,47);AAction2Instr(__a17,46);AAction2Instr(__a16,45);AAction2Instr(__a15,44);AAction2Instr(__a14,43);AAction2Instr(__a13,42);AAction2Instr(__a12,41);AAction2Instr(__a11,40);AAction2Instr(__a10,39);AAction2Instr(__a9,38);AAction2Instr(__a8,37);AAction2Instr(__a7,36);AAction2Instr(__a6,35);AAction2Instr(__a5,34);AAction2Instr(__a4,33);AAction2Instr(__a3,32);AAction2Instr(__a2,31)]);
(393, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,429)]);
(10, [AAction2Instr(__a36,65);AAction2Instr(__a35,64);AAction2Instr(__a34,63);AAction2Instr(__a33,62);AAction2Instr(__a32,61);AAction2Instr(__a31,60);AAction2Instr(__a30,59);AAction2Instr(__a29,58);AAction2Instr(__a28,57);AAction2Instr(__a27,56);AAction2Instr(__a26,55);AAction2Instr(__a25,54);AAction2Instr(__a24,53);AAction2Instr(__a23,52);AAction2Instr(__a22,51);AAction2Instr(__a21,50)]);
(394, [EatInstr(45,430)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),66);RCompleteInstr2(274,nullable_eof)]);
(395, [EatInstr(45,431)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(45,432)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(101,433)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(111,434)]);
(15, [ACallInstr3(__default_call,9);ASimpleCont2Instr(272,__binder1,67)]);
(399, [EatInstr(105,435)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(108,436)]);
(17, [AAction2Instr(__a37,69);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,68)]);
(401, [EatInstr(101,437)]);
(402, [EatInstr(108,438)]);
(19, [ALookaheadInstr(false,CfgLA (1,264),70);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,19)]);
(403, [EatInstr(115,439)]);
(20, [EatInstr(116,71)]);
(404, [EatInstr(101,440)]);
(21, [EatInstr(111,73);EatInstr(108,72)]);
(405, [AAction2Instr(__a74,283)]);
(22, [EatInstr(101,74)]);
(406, [EatInstr(103,441)]);
(23, [EatInstr(97,75)]);
(407, [EatInstr(101,442)]);
(24, [EatInstr(110,76)]);
(408, [EatInstr(97,443)]);
(25, [EatInstr(105,78);EatInstr(101,77)]);
(409, [AAction2Instr(__a75,135)]);
(26, [EatInstr(105,79)]);
(410, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,444)]);
(27, [EatInstr(114,80)]);
(411, [AAction2Instr(__a77,446);AAction2Instr(__a76,445)]);
(28, [EatInstr(117,81)]);
(412, [AAction2Instr(__a78,135)]);
(29, [EatInstr(110,82)]);
(413, [AAction2Instr(__a79,135)]);
(30, [EatInstr(114,83)]);
(414, [EatInstr(100,447)]);
(31, [ASimpleCont2Instr(271,__binder2,84);ACallInstr3(__default_call,8)]);
(415, [EatInstr(97,448)]);
(32, [EatInstr(99,85)]);
(416, [EatInstr(99,449)]);
(33, [EatInstr(100,86)]);
(417, [EatInstr(108,450)]);
(34, [EatInstr(100,87)]);
(418, [EatInstr(114,451)]);
(35, [EatInstr(114,89);EatInstr(101,88)]);
(419, [EatInstr(108,452)]);
(36, [EatInstr(101,90)]);
(420, [EatInstr(108,453)]);
(37, [EatInstr(102,91)]);
(421, [EatInstr(116,454)]);
(38, [EatInstr(105,92)]);
(422, [EatInstr(101,455)]);
(39, [EatInstr(108,93)]);
(423, [EatInstr(101,456)]);
(40, [EatInstr(108,94)]);
(424, [ASimpleCont2Instr(271,__binder4,457);ACallInstr3(__default_call,8)]);
(41, [EatInstr(112,95)]);
(425, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,458)]);
(42, [EatInstr(112,96)]);
(426, [EatInstr(115,459)]);
(43, [EatInstr(112,97)]);
(427, [EatInstr(97,460)]);
(44, [EatInstr(112,98)]);
(428, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,461)]);
(45, [EatInstr(112,99)]);
(429, [AAction2Instr(__a80,462)]);
(46, [EatInstr(112,100)]);
(430, [EatInstr(115,463)]);
(47, [EatInstr(115,101)]);
(431, [EatInstr(99,464)]);
(48, [EatInstr(116,102)]);
(432, [EatInstr(114,465)]);
(49, [EatInstr(116,103)]);
(433, [EatInstr(45,466)]);
(50, [EatInstr(45,104)]);
(434, [EatInstr(105,467)]);
(51, [EatInstr(45,105)]);
(435, [EatInstr(115,468)]);
(52, [EatInstr(45,106)]);
(436, [EatInstr(97,469)]);
(53, [EatInstr(45,107)]);
(437, [EatInstr(97,470)]);
(54, [EatInstr(45,108)]);
(438, [EatInstr(101,471)]);
(55, [EatInstr(45,109)]);
(439, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,472)]);
(56, [EatInstr(45,110)]);
(440, [EatInstr(114,473)]);
(57, [EatInstr(45,111)]);
(441, [EatInstr(117,474)]);
(58, [EatInstr(45,112)]);
(442, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,475)]);
(59, [EatInstr(45,113)]);
(443, [EatInstr(114,476)]);
(60, [EatInstr(45,114)]);
(444, [AAction2Instr(__a81,135)]);
(61, [EatInstr(45,115)]);
(445, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,477)]);
(62, [EatInstr(45,116)]);
(446, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,478)]);
(63, [EatInstr(45,117)]);
(447, [EatInstr(45,479)]);
(64, [EatInstr(45,118)]);
(448, [EatInstr(104,480)]);
(65, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,119)]);
(449, [EatInstr(101,481)]);
(66, [CompleteInstr(274)]);
(450, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,482)]);
(67, [AAction2Instr(__a38,120)]);
(451, [EatInstr(101,483)]);
(68, [AAction2Instr(__a39,17)]);
(452, [EatInstr(108,484)]);
(69, [AWhenInstr3(__p41,__p40,121)]);
(453, [EatInstr(101,485)]);
(70, [CompleteInstr(270)]);
(454, [EatInstr(101,486)]);
(71, [EatInstr(116,122)]);
(455, [EatInstr(45,487)]);
(72, [EatInstr(111,123)]);
(456, [EatInstr(45,488)]);
(73, [EatInstr(112,124)]);
(457, [AAction2Instr(__a82,277)]);
(74, [EatInstr(115,125)]);
(458, [AAction2Instr(__a83,489)]);
(75, [EatInstr(115,126)]);
(459, [EatInstr(101,490)]);
(76, [EatInstr(108,127)]);
(460, [EatInstr(98,491)]);
(77, [EatInstr(120,128)]);
(461, [AAction2Instr(__a84,492)]);
(78, [EatInstr(102,129)]);
(462, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,493)]);
(79, [EatInstr(110,130)]);
(463, [EatInstr(116,494)]);
(80, [EatInstr(101,131)]);
(464, [EatInstr(115,495)]);
(81, [EatInstr(98,132)]);
(465, [EatInstr(101,496)]);
(82, [EatInstr(114,133)]);
(466, [EatInstr(104,497)]);
(83, [EatInstr(97,134)]);
(467, [EatInstr(122,498)]);
(84, [AAction2Instr(__a42,135)]);
(468, [EatInstr(116,499)]);
(85, [EatInstr(111,136)]);
(469, [EatInstr(121,500)]);
(86, [EatInstr(105,137)]);
(470, [EatInstr(100,501)]);
(87, [EatInstr(111,138)]);
(471, [EatInstr(115,502)]);
(88, [EatInstr(120,139)]);
(472, [AAction2Instr(__a85,283)]);
(89, [EatInstr(102,140)]);
(473, [EatInstr(45,503)]);
(90, [EatInstr(120,141)]);
(474, [EatInstr(108,504)]);
(91, [EatInstr(117,142)]);
(475, [AAction2Instr(__a86,283)]);
(92, [EatInstr(110,143)]);
(476, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,505)]);
(93, [EatInstr(111,144)]);
(477, [AAction2Instr(__a87,506)]);
(94, [EatInstr(114,145)]);
(478, [AAction2Instr(__a88,135)]);
(95, [EatInstr(114,146)]);
(479, [EatInstr(97,507)]);
(96, [EatInstr(114,147)]);
(480, [EatInstr(101,508)]);
(97, [EatInstr(114,148)]);
(481, [EatInstr(45,509)]);
(98, [EatInstr(114,149)]);
(482, [AAction2Instr(__a89,135)]);
(99, [EatInstr(114,150)]);
(483, [EatInstr(100,510)]);
(100, [EatInstr(114,151)]);
(484, [EatInstr(97,511)]);
(101, [EatInstr(116,152)]);
(485, [EatInstr(118,512)]);
(102, [EatInstr(114,153)]);
(486, [EatInstr(45,513)]);
(103, [EatInstr(114,154)]);
(487, [EatInstr(100,514)]);
(104, [EatInstr(97,155)]);
(488, [EatInstr(100,515)]);
(105, [EatInstr(98,156)]);
(489, [EatInstr(116,518);EatInstr(112,517);EatInstr(102,516)]);
(106, [EatInstr(99,157)]);
(490, [EatInstr(110,519)]);
(107, [EatInstr(99,158)]);
(491, [EatInstr(101,520)]);
(108, [EatInstr(117,161);EatInstr(114,160);EatInstr(99,159)]);
(492, [AAction2Instr(__a90,522);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,521)]);
(109, [EatInstr(105,162)]);
(493, [AAction2Instr(__a91,277)]);
(110, [EatInstr(105,163)]);
(494, [EatInstr(97,523)]);
(111, [EatInstr(109,164)]);
(495, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,524)]);
(112, [EatInstr(110,165)]);
(496, [EatInstr(103,525)]);
(113, [EatInstr(117,166)]);
(497, [EatInstr(105,526)]);
(114, [EatInstr(110,167)]);
(498, [EatInstr(101,527)]);
(115, [EatInstr(108,168)]);
(499, [EatInstr(111,528)]);
(116, [EatInstr(110,169)]);
(500, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,529)]);
(117, [EatInstr(111,170)]);
(501, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,530)]);
(118, [EatInstr(118,171)]);
(502, [EatInstr(99,531)]);
(119, [AAction2Instr(__a43,172)]);
(503, [EatInstr(99,532)]);
(120, [AAction2Instr(__a45,174);AAction2Instr(__a44,173)]);
(121, [ALookaheadInstr(false,CfgLA (1,264),175)]);
(504, [EatInstr(97,533)]);
(505, [AAction2Instr(__a92,283)]);
(122, [EatInstr(114,176)]);
(506, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,534)]);
(123, [EatInstr(115,177)]);
(507, [EatInstr(110,535)]);
(124, [EatInstr(121,178)]);
(508, [EatInstr(97,536)]);
(125, [EatInstr(117,179)]);
(509, [EatInstr(97,537)]);
(126, [EatInstr(104,180)]);
(510, [EatInstr(115,538)]);
(127, [EatInstr(105,181)]);
(511, [EatInstr(98,539)]);
(128, [EatInstr(101,182)]);
(512, [EatInstr(97,540)]);
(129, [EatInstr(116,183)]);
(513, [EatInstr(97,541)]);
(130, [EatInstr(117,184)]);
(514, [EatInstr(121,542)]);
(131, [EatInstr(99,185)]);
(515, [EatInstr(121,543)]);
(132, [EatInstr(115,186)]);
(516, [EatInstr(117,544)]);
(133, [EatInstr(111,187)]);
(517, [EatInstr(101,545)]);
(134, [EatInstr(112,188)]);
(518, [EatInstr(120,546)]);
(135, [AAction2Instr(__a46,189)]);
(519, [EatInstr(115,547)]);
(136, [EatInstr(109,190)]);
(520, [EatInstr(108,548)]);
(137, [EatInstr(115,191)]);
(521, [AAction2Instr(__a93,492)]);
(138, [EatInstr(116,192)]);
(522, [AWhenInstr3(__p95,__p94,549)]);
(139, [EatInstr(101,193)]);
(523, [EatInstr(114,550)]);
(140, [EatInstr(99,194)]);
(524, [AAction2Instr(__a96,277)]);
(141, [EatInstr(116,195)]);
(525, [EatInstr(117,551)]);
(142, [EatInstr(115,196)]);
(526, [EatInstr(115,552)]);
(143, [EatInstr(102,197)]);
(527, [EatInstr(45,553)]);
(144, [EatInstr(111,198)]);
(528, [EatInstr(114,554)]);
(145, [EatInstr(49,199)]);
(529, [AAction2Instr(__a97,277)]);
(146, [EatInstr(101,200)]);
(530, [AAction2Instr(__a98,277)]);
(147, [EatInstr(105,201)]);
(531, [EatInstr(101,555)]);
(148, [EatInstr(105,202)]);
(532, [EatInstr(111,556)]);
(149, [EatInstr(105,203)]);
(533, [EatInstr(114,557)]);
(150, [EatInstr(105,204)]);
(534, [AAction2Instr(__a99,411)]);
(151, [EatInstr(105,205)]);
(535, [EatInstr(97,558)]);
(152, [EatInstr(114,206)]);
(536, [EatInstr(100,559)]);
(153, [EatInstr(97,207)]);
(537, [EatInstr(110,560)]);
(154, [EatInstr(97,208)]);
(538, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,561)]);
(155, [EatInstr(102,209)]);
(539, [EatInstr(108,562)]);
(156, [EatInstr(97,210)]);
(540, [EatInstr(110,563)]);
(157, [EatInstr(97,211)]);
(541, [EatInstr(99,564)]);
(158, [EatInstr(104,212)]);
(542, [EatInstr(112,565)]);
(159, [EatInstr(111,213)]);
(543, [EatInstr(112,566)]);
(160, [EatInstr(111,214)]);
(544, [EatInstr(110,567)]);
(161, [EatInstr(110,215)]);
(545, [EatInstr(103,568)]);
(162, [EatInstr(110,216)]);
(546, [AAction2Instr(__a100,569)]);
(163, [EatInstr(110,217)]);
(547, [EatInstr(105,570)]);
(164, [EatInstr(101,218)]);
(548, [EatInstr(115,571)]);
(165, [EatInstr(111,219)]);
(549, [AAction2Instr(__a101,572)]);
(166, [EatInstr(110,220)]);
(550, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,573)]);
(167, [EatInstr(111,221)]);
(551, [EatInstr(108,574)]);
(168, [EatInstr(111,222)]);
(552, [EatInstr(116,575)]);
(169, [EatInstr(111,223)]);
(553, [EatInstr(104,576)]);
(170, [EatInstr(110,224)]);
(554, [EatInstr(121,577)]);
(171, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,225)]);
(555, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,578)]);
(172, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,226)]);
(556, [EatInstr(114,579)]);
(173, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder3,227)]);
(557, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,580)]);
(174, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,228)]);
(558, [EatInstr(108,581)]);
(175, [CompleteInstr(269)]);
(559, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,582)]);
(176, [EatInstr(105,229)]);
(560, [EatInstr(97,583)]);
(177, [EatInstr(101,230)]);
(561, [AAction2Instr(__a102,135)]);
(178, [EatInstr(114,231)]);
(562, [EatInstr(101,584)]);
(179, [EatInstr(103,232)]);
(563, [EatInstr(99,585)]);
(180, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,233)]);
(564, [EatInstr(116,586)]);
(181, [EatInstr(110,234)]);
(565, [EatInstr(103,587)]);
(182, [EatInstr(114,235)]);
(566, [EatInstr(103,588)]);
(183, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,236)]);
(567, [AAction2Instr(__a103,569)]);
(184, [EatInstr(115,237)]);
(568, [EatInstr(45,589);AAction2Instr(__a104,569)]);
(185, [EatInstr(101,238)]);
(569, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,590)]);
(186, [EatInstr(101,239)]);
(570, [EatInstr(116,591)]);
(187, [EatInstr(108,240)]);
(571, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,592)]);
(188, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,241)]);
(572, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,593)]);
(189, [CompleteInstr(272)]);
(573, [AAction2Instr(__a105,594)]);
(190, [EatInstr(112,242)]);
(574, [EatInstr(97,595)]);
(191, [EatInstr(112,243)]);
(575, [EatInstr(111,596)]);
(192, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,244)]);
(576, [EatInstr(105,597)]);
(193, [EatInstr(99,245)]);
(577, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,598)]);
(194, [AAction2Instr(__a47,246)]);
(578, [AAction2Instr(__a106,277)]);
(195, [EatInstr(114,247)]);
(579, [EatInstr(101,599)]);
(196, [EatInstr(101,248)]);
(580, [AAction2Instr(__a107,283)]);
(197, [EatInstr(111,249)]);
(581, [EatInstr(121,600)]);
(198, [EatInstr(107,250)]);
(582, [AAction2Instr(__a108,135)]);
(199, [EatInstr(45,251)]);
(583, [EatInstr(108,601)]);
(200, [EatInstr(99,252)]);
(584, [EatInstr(45,602)]);
(201, [EatInstr(110,253)]);
(585, [EatInstr(101,603)]);
(202, [EatInstr(110,254)]);
(586, [EatInstr(105,604)]);
(203, [EatInstr(110,255)]);
(587, [EatInstr(101,605)]);
(204, [EatInstr(110,256)]);
(588, [EatInstr(101,606)]);
(205, [EatInstr(110,257)]);
(589, [EatInstr(115,607)]);
(206, [EatInstr(105,258)]);
(590, [AAction2Instr(__a109,277)]);
(207, [EatInstr(110,259)]);
(591, [EatInstr(105,608)]);
(208, [EatInstr(110,260)]);
(592, [AAction2Instr(__a110,277)]);
(209, [EatInstr(116,261)]);
(593, [AAction2Instr(__a111,277)]);
(210, [EatInstr(99,262)]);
(594, [AAction2Instr(__a112,610);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,609)]);
(211, [EatInstr(115,263)]);
(595, [EatInstr(114,611)]);
(212, [EatInstr(101,264)]);
(596, [EatInstr(114,612)]);
(213, [EatInstr(117,265)]);
(597, [EatInstr(115,613)]);
(214, [EatInstr(111,266)]);
(598, [AAction2Instr(__a113,277)]);
(215, [EatInstr(114,267)]);
(599, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,614)]);
(216, [EatInstr(108,268)]);
(600, [EatInstr(115,615)]);
(217, [EatInstr(108,269)]);
(601, [EatInstr(121,616)]);
(218, [EatInstr(109,270)]);
(602, [EatInstr(112,617)]);
(219, [EatInstr(45,271)]);
(603, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,618)]);
(220, [EatInstr(105,272)]);
(604, [EatInstr(111,619)]);
(221, [EatInstr(45,273)]);
(605, [EatInstr(110,620)]);
(222, [EatInstr(111,274)]);
(606, [EatInstr(110,621)]);
(223, [EatInstr(45,275)]);
(607, [EatInstr(116,622)]);
(224, [EatInstr(108,276)]);
(608, [EatInstr(118,623)]);
(225, [AAction2Instr(__a48,277)]);
(609, [AAction2Instr(__a114,594)]);
(226, [AAction2Instr(__a49,277)]);
(610, [AWhenInstr3(__p116,__p115,624)]);
(227, [AAction2Instr(__a50,120)]);
(611, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,625)]);
(228, [AAction2Instr(__a51,278)]);
(612, [EatInstr(121,626)]);
(229, [EatInstr(98,279)]);
(613, [EatInstr(116,627)]);
(230, [EatInstr(45,280)]);
(614, [AAction2Instr(__a117,283)]);
(231, [EatInstr(117,281)]);
(615, [EatInstr(105,628)]);
(232, [EatInstr(97,282)]);
(616, [EatInstr(115,629)]);
(233, [AAction2Instr(__a52,283)]);
(617, [EatInstr(114,630)]);
(234, [EatInstr(101,284)]);
(618, [AAction2Instr(__a118,135)]);
(235, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,285)]);
(619, [EatInstr(110,631)]);
(236, [AAction2Instr(__a53,283)]);
(620, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,632)]);
(237, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,286)]);
(621, [EatInstr(45,633)]);
(238, [EatInstr(100,287)]);
(622, [EatInstr(114,634)]);
(239, [EatInstr(116,288)]);
(623, [EatInstr(101,635)]);
(240, [EatInstr(108,289)]);
(624, [AAction2Instr(__a119,636)]);
(241, [AAction2Instr(__a54,283)]);
(625, [AAction2Instr(__a120,277)]);
(242, [EatInstr(105,290)]);
(626, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,637)]);
(243, [EatInstr(97,291)]);
(627, [EatInstr(111,638)]);
(244, [AAction2Instr(__a55,135)]);
(628, [EatInstr(115,639)]);
(245, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,292)]);
(629, [EatInstr(105,640)]);
(246, [AAction2Instr(__a56,294);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,293)]);
(630, [EatInstr(101,641)]);
(247, [EatInstr(97,295)]);
(631, [EatInstr(115,642)]);
(248, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,296)]);
(632, [AAction2Instr(__a121,135)]);
(249, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,297)]);
(633, [EatInstr(115,643)]);
(250, [EatInstr(97,298)]);
(634, [EatInstr(105,644)]);
(251, [EatInstr(108,299)]);
(635, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,645)]);
(252, [EatInstr(101,300)]);
(636, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,646)]);
(253, [EatInstr(116,301)]);
(637, [AAction2Instr(__a122,277)]);
(254, [EatInstr(116,302)]);
(638, [EatInstr(114,647)]);
(255, [EatInstr(116,303)]);
(639, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,648)]);
(256, [EatInstr(116,304)]);
(640, [EatInstr(115,649)]);
(257, [EatInstr(116,305)]);
(641, [EatInstr(100,650)]);
(258, [EatInstr(112,306)]);
(642, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,651)]);
(259, [EatInstr(115,307)]);
(643, [EatInstr(99,652)]);
(260, [EatInstr(115,308)]);
(644, [EatInstr(99,653)]);
(261, [EatInstr(101,309)]);
(645, [AAction2Instr(__a123,277)]);
(262, [EatInstr(107,310)]);
(646, [AAction2Instr(__a124,277)]);
(263, [EatInstr(101,311)]);
(647, [EatInstr(121,654)]);
(264, [EatInstr(99,312)]);
(648, [AAction2Instr(__a125,135)]);
(265, [EatInstr(110,313)]);
(649, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,655)]);
(266, [EatInstr(116,314)]);
(650, [EatInstr(105,656)]);
(267, [EatInstr(111,315)]);
(651, [AAction2Instr(__a126,135)]);
(268, [EatInstr(105,316)]);
(652, [EatInstr(97,657)]);
(269, [EatInstr(105,317)]);
(653, [EatInstr(116,658)]);
(270, [EatInstr(111,318)]);
(654, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,659)]);
(271, [EatInstr(109,319)]);
(655, [AAction2Instr(__a127,135)]);
(272, [EatInstr(116,320)]);
(656, [EatInstr(99,660)]);
(273, [EatInstr(114,321)]);
(657, [EatInstr(110,661)]);
(274, [EatInstr(107,322)]);
(658, [AAction2Instr(__a128,569)]);
(275, [EatInstr(99,323)]);
(659, [AAction2Instr(__a129,277)]);
(276, [EatInstr(121,324)]);
(660, [EatInstr(97,662)]);
(277, [AAction2Instr(__a57,325)]);
(661, [EatInstr(110,663)]);
(278, [CompleteInstr(267)]);
(662, [EatInstr(116,664)]);
(279, [EatInstr(117,326)]);
(663, [EatInstr(101,665)]);
(280, [EatInstr(117,327)]);
(664, [EatInstr(101,666)]);
(281, [EatInstr(108,328)]);
(665, [EatInstr(114,667)]);
(282, [EatInstr(114,329)]);
(666, [EatInstr(115,668)]);
(283, [CompleteInstr(271)]);
(667, [EatInstr(108,669)]);
(284, [EatInstr(45,330)]);
(668, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,670)]);
(285, [AAction2Instr(__a58,283)]);
(669, [EatInstr(101,671)]);
(286, [AAction2Instr(__a59,283)]);
(670, [AAction2Instr(__a130,135)]);
(287, [EatInstr(101,331)]);
(671, [EatInstr(115,672)]);
(288, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,332)]);
(672, [EatInstr(115,673)]);
(289, [EatInstr(45,333)]);
(673, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,674)]);
(290, [EatInstr(108,334)]);
(674, [AAction2Instr(__a131,135)]);
(291, [EatInstr(116,335)]);
(292, [AAction2Instr(__a60,336)]);
(293, [AAction2Instr(__a61,246)]);
(294, [AWhenInstr3(__p63,__p62,337)]);
(295, [EatInstr(99,338)]);
(296, [AAction2Instr(__a64,135)]);
(297, [AAction2Instr(__a65,135)]);
(298, [EatInstr(104,339)]);
(299, [EatInstr(111,340)]);
(300, [EatInstr(100,341)]);
(301, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,342)]);
(302, [EatInstr(45,343)]);
(303, [EatInstr(45,344)]);
(304, [EatInstr(45,345)]);
(305, [EatInstr(45,346)]);
(306, [EatInstr(45,347)]);
(307, [EatInstr(108,348)]);
(308, [EatInstr(108,349)]);
(309, [EatInstr(114,350)]);
(310, [EatInstr(101,351)]);
(311, [EatInstr(45,352)]);
(312, [EatInstr(107,353)]);
(313, [EatInstr(116,354)]);
(314, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,355)]);
(315, [EatInstr(108,356)]);
(316, [EatInstr(110,357)]);
(317, [EatInstr(110,358)]);
(318, [EatInstr(105,359)]);
(319, [EatInstr(101,360)]);
(320, [EatInstr(45,361)]);
(321, [EatInstr(101,362)]);
(322, [EatInstr(97,363)]);
(323, [EatInstr(111,364)]);
(324, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,365)]);
(325, [CompleteInstr(273)]);
(326, [EatInstr(116,366)]);
(327, [EatInstr(110,367)]);
(328, [EatInstr(101,368)]);
(329, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,369)]);
(330, [EatInstr(114,370)]);
(331, [EatInstr(110,371)]);
(332, [AAction2Instr(__a66,283)]);
(333, [EatInstr(115,372)]);
(334, [EatInstr(101,373)]);
(335, [EatInstr(99,374)]);
(336, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,375)]);
(337, [AAction2Instr(__a67,376)]);
(338, [EatInstr(116,377)]);
(339, [EatInstr(101,378)]);
(340, [EatInstr(111,379)]);
(341, [EatInstr(101,380)]);
(342, [AAction2Instr(__a68,135)]);
(343, [EatInstr(103,381)]);
(344, [EatInstr(110,382)]);
(345, [EatInstr(110,383)]);
(346, [EatInstr(114,384)]);
(347, [EatInstr(108,385)]);
(348, [EatInstr(97,386)]);
(349, [EatInstr(97,387)]);
(350, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,388)]);
(351, [EatInstr(110,389)]);
(352, [EatInstr(105,390)]);
(353, [EatInstr(45,391)]);
(354, [EatInstr(101,392)]);
(355, [AAction2Instr(__a69,393)]);
(356, [EatInstr(108,394)]);
(357, [EatInstr(101,395)]);
(358, [EatInstr(101,396)]);
(359, [EatInstr(122,397)]);
(360, [EatInstr(109,398)]);
(361, [EatInstr(104,399)]);
(362, [EatInstr(112,400)]);
(363, [EatInstr(104,401)]);
(364, [EatInstr(97,402)]);
(365, [AAction2Instr(__a70,277)]);
(366, [EatInstr(101,403)]);
(367, [EatInstr(100,404)]);
(368, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,405)]);
(369, [AAction2Instr(__a71,283)]);
(370, [EatInstr(101,406)]);
(371, [EatInstr(99,407)]);
(372, [EatInstr(116,408)]);
(373, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,409)]);
(374, [EatInstr(104,410)]);
(375, [AAction2Instr(__a72,411)]);
(376, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,412)]);
(377, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,413)]);
(378, [EatInstr(97,414)]);
(379, [EatInstr(107,415)]);
(380, [EatInstr(110,416)]);
(381, [EatInstr(105,417)]);
(382, [EatInstr(112,418)]);
]

let start_symb = get_symb_action "cmd-line-args"

module P2__ = Yak.Engine.Full_yakker(struct type t = sv let cmp = sv_compare end)

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
