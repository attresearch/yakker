
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
type plugin =
  | Dypgen_PI of bool (** flag indicating whether the generated grammar should be faux-scannerless. *)

type transformation = Inline_regular_Tx | Add_LR1_lookahead_Tx

type analyses = A_precedence_sets

type command =
| Analyze_cmd
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
| Minus_cmd
| Print_gil_cmd
| Print_gul_cmd
| Print_npreds_cmd
| Print_relevance_cmd
| Rfc_cmd
| Strip_late_actions_cmd
| Subset_cmd
| Transform_cmd
| Translate_cmd
| Tx_prec_cmd
| Unroll_star_cmd
| Wrap_cmd

let cmd = ref Print_gul_cmd
let files = ref []
let roots = ref []
let exec_l = ref []
let rfc_num = ref 0
let translate_plugin = ref (Dypgen_PI false)
let transforms = ref []
let analysis = ref A_precedence_sets
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
 (let _x43 = (ignore (*1004*) (_n()); 
 (let _x4 = (ignore (*1005*) (_n()); 
 (let rec _x45 _x4 = (match _n() with 1006 -> _x4 | _x44 -> _x45((ignore (*1007*) (_x44); 
 (let _x3 = (ignore (*1008*) (_n()); _r_args(_n,ykinput))
  in (ignore (*1010*) (_n()); _x3::_x4)
 ))
 )) in _x45([])))
  in (ignore (*1011*) (_n()); (List.rev _x4))
 ))
  in (ignore (*1013*) (_n());  cmd := c )
 ))
 ))
 
 and
_r_transformation(_n,ykinput) = 
 (match _n() with
 | (1025) -> ( Inline_regular_Tx )
 | _(*1027*) -> ( Add_LR1_lookahead_Tx )
 )
 and
_r_analysis(_n,ykinput) = (ignore (*1029*) (_n()); A_precedence_sets)
 
 and
_r_command(_n,ykinput) = (ignore (*1030*) (_n()); 
 (let _x41 = 
 (match _n() with
 | (1035) -> (
 (let a = (ignore (*1036*) (_n()); _r_analysis(_n,ykinput))
  in (ignore (*1039*) (_n());  analysis := a; Analyze_cmd )
 ))
 | (1044) -> ( Attributes_cmd )
 | (1049) -> ( Close_under_core_cmd )
 | (1054) -> ( Compile_cmd )
 | (1059) -> ( Desugar_cmd )
 | (1064) -> ( Dispatch_cmd )
 | (1069) -> ( Dot_cmd )
 | (1079) -> (
 (let _x9 = (ignore (*1080*) (_n()); _n())
  in (ignore (*1081*) (_n()); 
 (let _x8 = (ignore (*1082*) (_n()); _n())
  in (ignore (*1083*) (_n()); 
 (let f = (ignore (*1084*) (_n()); Yak.YkBuf.get_string _x9 _x8 ykinput)
  in (ignore (*1085*) (_n()); 
 (let l = (ignore (*1086*) (_n()); 
 (let _x15 = (ignore (*1087*) (_n()); 
 (let rec _x49 _x15 = (match _n() with 1088 -> _x15 | _x48 -> _x49((ignore (*1089*) (_x48); 
 (let _x14 = (ignore (*1098*) (_n()); 
 (let _x13 = (ignore (*1099*) (_n()); _n())
  in (ignore (*1100*) (_n()); 
 (let _x12 = (ignore (*1101*) (_n()); _n())
  in (ignore (*1102*) (_n()); 
 (let x = (ignore (*1103*) (_n()); Yak.YkBuf.get_string _x13 _x12 ykinput)
  in (ignore (*1104*) (_n()); x)
 ))
 ))
 ))
  in (ignore (*1105*) (_n()); _x14::_x15)
 ))
 )) in _x49([])))
  in (ignore (*1106*) (_n()); (List.rev _x15))
 ))
  in (ignore (*1108*) (_n());  files := f::!files; exec_l := l; Exec_cmd )
 ))
 ))
 ))
 ))
 | (1113) -> ( Extract_cmd )
 | (1118) -> ( Fuse_cmd )
 | (1123) -> ( Hash_cmd )
 | (1128) -> ( Info_cmd )
 | (1133) -> ( Inline_regular_cmd )
 | (1138) -> ( Lexer_cmd )
 | (1143) -> ( Lift_cmd )
 | (1148) -> ( Lookahead_analysis_cmd )
 | (1153) -> ( Minus_cmd )
 | (1158) -> ( Tx_prec_cmd )
 | (1163) -> ( Print_gul_cmd )
 | (1168) -> ( Print_gil_cmd )
 | (1173) -> ( Print_npreds_cmd )
 | (1178) -> ( Print_npreds_cmd )
 | (1183) -> ( Print_relevance_cmd )
 | (1200) -> (
 (let _x20 = (ignore (*1201*) (_n()); _n())
  in (ignore (*1202*) (_n()); 
 (let _x19 = (ignore (*1203*) (_n()); _n())
  in (ignore (*1204*) (_n()); 
 (let n = (ignore (*1205*) (_n()); Yak.YkBuf.get_string _x20 _x19 ykinput)
  in (ignore (*1207*) (_n());  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" )
 ))
 ))
 ))
 | (1214) -> ( Strip_late_actions_cmd )
 | (1221) -> ( Subset_cmd )
 | (1228) -> (
 (let tx = (ignore (*1229*) (_n()); _r_transformation(_n,ykinput))
  in (ignore (*1231*) (_n()); 
 (let txs = (ignore (*1232*) (_n()); 
 (let _x22 = (ignore (*1233*) (_n()); 
 (let rec _x47 _x22 = (match _n() with 1234 -> _x22 | _x46 -> _x47((ignore (*1235*) (_x46); 
 (let _x21 = (ignore (*1237*) (_n()); 
 (let x = (ignore (*1238*) (_n()); _r_transformation(_n,ykinput))
  in (ignore (*1240*) (_n()); x)
 ))
  in (ignore (*1241*) (_n()); _x21::_x22)
 ))
 )) in _x47([])))
  in (ignore (*1242*) (_n()); (List.rev _x22))
 ))
  in (ignore (*1244*) (_n());  transforms := (tx::txs); Transform_cmd )
 ))
 ))
 | (1251) -> (
 (let t = 
 (match _n() with
 | (1253) -> (Dypgen_PI false)
 | _(*1255*) -> (Dypgen_PI true)
 ) in (ignore (*1257*) (_n());  translate_plugin := t; Translate_cmd )
 ))
 | (1264) -> ( Unroll_star_cmd )
 | _(*1269*) -> ( Wrap_cmd )
 ) in (ignore (*1270*) (_n()); _x41)
 ))
 
 and
_r_args(_n,ykinput) = (ignore (*1271*) (_n()); 
 (let _x42 = 
 (match _n() with
 | (1276) -> (
 (let b = 
 (match _n() with
 | (1278) -> (Fun_BE)
 | (1280) -> (Trans_BE)
 | (1282) -> (Peg_BE false)
 | _(*1284*) -> (Peg_BE true)
 ) in (ignore (*1286*) (_n());  backend := b )
 ))
 | (1291) -> ( Compileopt.case_sensitive := false )
 | (1296) -> ( Compileopt.check_labels := true )
 | (1314) -> (
 (let _x27 = (ignore (*1315*) (_n()); _n())
  in (ignore (*1316*) (_n()); 
 (let _x26 = (ignore (*1317*) (_n()); _n())
  in (ignore (*1318*) (_n()); 
 (let n = (ignore (*1319*) (_n()); Yak.YkBuf.get_string _x27 _x26 ykinput)
  in (ignore (*1321*) (_n());  Variables.counter := (int_of_string n) )
 ))
 ))
 ))
 | (1326) -> ( Compileopt.inline_cs := true )
 | (1331) -> ( Compileopt.inline_regular := true )
 | (1336) -> ( Compileopt.lookahead := true )
 | (1341) -> ( Yak.Pami.new_engine_flag := true )
 | (1346) -> ( Compileopt.coalesce := false )
 | (1351) -> ( only := true )
 | (1361) -> (
 (let _x31 = (ignore (*1362*) (_n()); _n())
  in (ignore (*1363*) (_n()); 
 (let _x30 = (ignore (*1364*) (_n()); _n())
  in (ignore (*1365*) (_n()); 
 (let x = (ignore (*1366*) (_n()); Yak.YkBuf.get_string _x31 _x30 ykinput)
  in (ignore (*1368*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | (1386) -> (
 (let _x36 = (ignore (*1387*) (_n()); _n())
  in (ignore (*1388*) (_n()); 
 (let _x35 = (ignore (*1389*) (_n()); _n())
  in (ignore (*1390*) (_n()); 
 (let n = (ignore (*1391*) (_n()); Yak.YkBuf.get_string _x36 _x35 ykinput)
  in (ignore (*1393*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1398) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | _(*1406*) -> (
 (let _x40 = (ignore (*1407*) (_n()); _n())
  in (ignore (*1408*) (_n()); 
 (let _x39 = (ignore (*1409*) (_n()); _n())
  in (ignore (*1410*) (_n()); 
 (let f = (ignore (*1411*) (_n()); Yak.YkBuf.get_string _x40 _x39 ykinput)
  in (ignore (*1413*) (_n());  files := f::!files )
 ))
 ))
 ))
 ) in (ignore (*1414*) (_n()); _x42)
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

let _x53 =
 (fun _(*pos*) (_,_x50)(*arg of arg*) -> (_t(fun _(*1016*) pos_ -> let _x51 _x5  = _t(function
 | 1022 ->
 (fun pos_ -> Yk_when(_x5>=1))
 | _(*1023*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1017*) pos_ -> let rec _x52 _x5  = _t(function
 | 1018 ->
 (fun pos_ -> _x51 (_x5) )
 | _(*1020*) ->
 (fun pos_ -> _x52 (_x5+1) )) in _x52 (0) )),_x50))
let _x85 =
 (fun _(*pos*) (_,_x54)(*arg of command*) -> (_t(function
 | 1032 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1041 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1046 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1051 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1056 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1061 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1066 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1072 ->
 (fun pos_ -> let _x73 _x7  = _t(fun _(*1076*) pos_ -> let _x74 _x6  = _t(fun _(*1079*) pos_ -> let _x76 _x75  = _t(fun _(*1081*) pos_ -> let _x78 _x77  = _t(fun _(*1087*) pos_ -> let rec _x80 _x79  = _t(function
 | 1088 ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore(_x79);_wv0);_wv0);_wv0))
 | _(*1091*) ->
 (fun pos_ -> let _x81 _x11  = _t(fun _(*1095*) pos_ -> let _x82 _x10  = _t(fun _(*1098*) pos_ -> let _x84 _x83  = _t(fun _(*1101*) pos_ -> Yk_delay(_x80 (ignore(ignore((_wv0));_wv0);_wv0) ,_x10)) in _t(fun _(*1099*) pos_ -> Yk_delay(_x84 ((_wv0)) ,_x11))) in _t(fun _(*1096*) pos_ -> _x82 (pos_) )) in _t(fun _(*1092*) pos_ -> _x81 (pos_) ))) in _x80 (_wv0) ) in _t(fun _(*1082*) pos_ -> Yk_delay(_x78 ((_wv0)) ,_x6))) in _t(fun _(*1080*) pos_ -> Yk_delay(_x76 ((_wv0)) ,_x7))) in _t(fun _(*1077*) pos_ -> _x74 (pos_) )) in _t(fun _(*1073*) pos_ -> _x73 (pos_) ))
 | 1110 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1115 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1120 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
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
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1185 ->
 (fun pos_ -> let _x65 _x18  = _t(fun _(*1188*) pos_ -> let _x67 _x66  = _t(fun _(*1197*) pos_ -> let _x70 _x17  = _t(fun _(*1200*) pos_ -> let _x72 _x71  = _t(fun _(*1203*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x17)) in _t(fun _(*1201*) pos_ -> Yk_delay(_x72 ((_wv0)) ,_x18))) in _t(fun _(*1198*) pos_ -> _x70 (pos_) )) in _t(fun _(*1189*) pos_ -> let _x68 _x16  = _t(function
 | 1195 ->
 (fun pos_ -> Yk_when(_x16>=1))
 | _(*1196*) ->
 (fun pos_ -> _x67 (ignore((_wv0));_wv0) )) in _t(fun _(*1190*) pos_ -> let rec _x69 _x16  = _t(function
 | 1191 ->
 (fun pos_ -> _x68 (_x16) )
 | _(*1193*) ->
 (fun pos_ -> _x69 (_x16+1) )) in _x69 (0) ))) in _t(fun _(*1186*) pos_ -> _x65 (pos_) ))
 | _(*1208*) ->
 (fun pos_ -> let _x56 _x55  = _t(function
 | 1211 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1215*) ->
 (fun pos_ -> let _x58 _x57  = _t(function
 | 1218 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1222*) ->
 (fun pos_ -> let _x60 _x59  = _t(function
 | 1225 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1245*) ->
 (fun pos_ -> let _x62 _x61  = _t(function
 | 1248 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1258*) ->
 (fun pos_ -> let _x64 _x63  = _t(function
 | 1261 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1266*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1259*) pos_ -> _x64 (()) ))) in _t(fun _(*1246*) pos_ -> _x62 (()) ))) in _t(fun _(*1223*) pos_ -> _x60 (()) ))) in _t(fun _(*1216*) pos_ -> _x58 (()) ))) in _t(fun _(*1209*) pos_ -> _x56 (()) ))),_x54))
let _x111 =
 (fun _(*pos*) (_,_x86)(*arg of args*) -> (_t(function
 | 1273 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1288 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1293 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1299 ->
 (fun pos_ -> let _x103 _x25  = _t(fun _(*1302*) pos_ -> let _x105 _x104  = _t(fun _(*1311*) pos_ -> let _x108 _x24  = _t(fun _(*1314*) pos_ -> let _x110 _x109  = _t(fun _(*1317*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x24)) in _t(fun _(*1315*) pos_ -> Yk_delay(_x110 ((_wv0)) ,_x25))) in _t(fun _(*1312*) pos_ -> _x108 (pos_) )) in _t(fun _(*1303*) pos_ -> let _x106 _x23  = _t(function
 | 1309 ->
 (fun pos_ -> Yk_when(_x23>=1))
 | _(*1310*) ->
 (fun pos_ -> _x105 (ignore((_wv0));_wv0) )) in _t(fun _(*1304*) pos_ -> let rec _x107 _x23  = _t(function
 | 1305 ->
 (fun pos_ -> _x106 (_x23) )
 | _(*1307*) ->
 (fun pos_ -> _x107 (_x23+1) )) in _x107 (0) ))) in _t(fun _(*1300*) pos_ -> _x103 (pos_) ))
 | 1323 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1328 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1333 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1338 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1343 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1348 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1354 ->
 (fun pos_ -> let _x99 _x29  = _t(fun _(*1358*) pos_ -> let _x100 _x28  = _t(fun _(*1361*) pos_ -> let _x102 _x101  = _t(fun _(*1364*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x28)) in _t(fun _(*1362*) pos_ -> Yk_delay(_x102 ((_wv0)) ,_x29))) in _t(fun _(*1359*) pos_ -> _x100 (pos_) )) in _t(fun _(*1355*) pos_ -> _x99 (pos_) ))
 | 1371 ->
 (fun pos_ -> let _x91 _x34  = _t(fun _(*1374*) pos_ -> let _x93 _x92  = _t(fun _(*1383*) pos_ -> let _x96 _x33  = _t(fun _(*1386*) pos_ -> let _x98 _x97  = _t(fun _(*1389*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x33)) in _t(fun _(*1387*) pos_ -> Yk_delay(_x98 ((_wv0)) ,_x34))) in _t(fun _(*1384*) pos_ -> _x96 (pos_) )) in _t(fun _(*1375*) pos_ -> let _x94 _x32  = _t(function
 | 1381 ->
 (fun pos_ -> Yk_when(_x32>=1))
 | _(*1382*) ->
 (fun pos_ -> _x93 (ignore((_wv0));_wv0) )) in _t(fun _(*1376*) pos_ -> let rec _x95 _x32  = _t(function
 | 1377 ->
 (fun pos_ -> _x94 (_x32) )
 | _(*1379*) ->
 (fun pos_ -> _x95 (_x32+1) )) in _x95 (0) ))) in _t(fun _(*1372*) pos_ -> _x91 (pos_) ))
 | 1395 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1399*) ->
 (fun pos_ -> let _x87 _x38  = _t(fun _(*1403*) pos_ -> let _x88 _x37  = _t(fun _(*1406*) pos_ -> let _x90 _x89  = _t(fun _(*1409*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x37)) in _t(fun _(*1407*) pos_ -> Yk_delay(_x90 ((_wv0)) ,_x38))) in _t(fun _(*1404*) pos_ -> _x88 (pos_) )) in _t(fun _(*1400*) pos_ -> _x87 (pos_) ))),_x86))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a2 = fun p v -> _p 1030 p (_x85 p (v));;
let __p123 = _dwhen 1381;;
let __a24 = _d 1170;;
let __a14 = _d 1120;;
let __a115 = _p 1321;;
let __a10 = _d 1061;;
let __a35 = _d 1343;;
let __a77 = _p 1153;;
let __a141 = _p 1214;;
let __a135 = _p 1264;;
let __a104 = _p 1044;;
let __a44 = fun p v -> _d 1216 p (_d 1215 p (v));;
let __a25 = _d 1175;;
let __a15 = _d 1125;;
let __a37 = _d 1395;;
let __a110 = fun p v -> _d 1376 p (_d 1375 p (_d 1374 p (_d 1372 p (_d 1371 p (v)))));;
let __a107 = _p 1326;;
let __a11 = _d 1066;;
let __a105 = _p 1158;;
let __a29 = _d 1288;;
let __a4 = _d 1018;;
let __a91 = _p 1108;;
let __a36 = _d 1348;;
let __a134 = _p 1049;;
let __a98 = _p 1269;;
let __a60 = fun p v -> _d 1073 p (_d 1072 p (v));;
let __a137 = fun p v -> _p 1238 p (_p 1237 p (v));;
let __a69 = _p 1270;;
let __a26 = _d 1180;;
let __a16 = _d 1130;;
let __a40 = _d 1020;;
let __a133 = _p 1331;;
let __a93 = _p 1221;;
let __a132 = fun p v -> _p 1242 p (_p 1234 p (v));;
let __a78 = _p 1163;;
let __a88 = _p 1113;;
let __a30 = _d 1293;;
let __a86 = _p 1054;;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a94 = _d 1305;;
let __a17 = _d 1135;;
let __a108 = _p 1336;;
let __a96 = _p 1276;;
let __a99 = _d 1307;;
let __a117 = _p 1278;;
let __a97 = _p 1168;;
let __a70 = _p 1118;;
let __a56 = _d 1248;;
let __a87 = _p 1059;;
let __p41 = _dnext 1023;;
let __a68 = fun p v -> _p 1205 p (_p 1204 p (_ddelay 1203 p (_p 1202 p (_ddelay 1201 p (_d_and_push 1200 p (_d 1198 p (_d 1197 p (v))))))));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x53 p (v)));;
let __a90 = fun p v -> _d 1092 p (_d 1091 p (v));;
let __a89 = fun p v -> _d 1304 p (_d 1303 p (_d 1302 p (_d 1300 p (_d 1299 p (v)))));;
let __a111 = _p 1280;;
let __a18 = _d 1140;;
let __a53 = _p 1010;;
let __a112 = _p 1341;;
let __a55 = _d 1191;;
let __p42 = _dwhen 1022;;
let __a143 = _p 1282;;
let __a5 = _d 1032;;
let __a136 = _p 1393;;
let __a113 = _p 1173;;
let __a83 = fun p v -> _p 1366 p (_p 1365 p (_ddelay 1364 p (_p 1363 p (_ddelay 1362 p (_d_and_push 1361 p (_d 1359 p (_d 1358 p (v))))))));;
let __a61 = _d 1193;;
let __a71 = _p 1123;;
let __a54 = _p 1013;;
let __a118 = _p 1284;;
let __a92 = _p 1064;;
let __a131 = _p 1235;;
let __a19 = _d 1145;;
let __a124 = _p 1286;;
let __a119 = _p 1346;;
let __p100 = _dnext 1310;;
let __a49 = fun p v -> _p 1411 p (_p 1410 p (_ddelay 1409 p (_p 1408 p (_ddelay 1407 p (_d_and_push 1406 p (_d 1404 p (_d 1403 p (v))))))));;
let __a146 = _p 1178;;
let __p101 = _dwhen 1309;;
let __a58 = _p 1398;;
let __a72 = _p 1128;;
let __a64 = _p 1069;;
let __a46 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a142 = fun p v -> _p 1241 p (_p 1240 p (v));;
let __p62 = _dnext 1196;;
let __a50 = fun p v -> _d 1190 p (_d 1189 p (_d 1188 p (_d 1186 p (_d 1185 p (v)))));;
let __a38 = fun p v -> _d 1400 p (_d 1399 p (v));;
let __a102 = fun p v -> _p 1105 p (_p 1104 p (_p 1103 p (_p 1102 p (_ddelay 1101 p (_p 1100 p (_ddelay 1099 p (_d_and_push 1098 p (_d 1096 p (_d 1095 p (v))))))))));;
let __a20 = _d 1150;;
let __a139 = _p 1291;;
let __a6 = _d 1041;;
let __a79 = _p 1351;;
let __a45 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a65 = _d 1261;;
let __a3 = fun p v -> _p 1271 p (_x111 p (v));;
let __a43 = _d 1211;;
let __a81 = fun p v -> _p 1106 p (_d_and_push 1088 p (v));;
let __a130 = _p 1183;;
let __a127 = _p 1133;;
let __a106 = fun p v -> _p 1319 p (_p 1318 p (_ddelay 1317 p (_p 1316 p (_ddelay 1315 p (_d_and_push 1314 p (_d 1312 p (_d 1311 p (v))))))));;
let __a59 = _p 1413;;
let __a31 = _d 1323;;
let __a138 = _p 1244;;
let __a67 = _p 1414;;
let __a52 = fun p v -> _d 1246 p (_d 1245 p (v));;
let __p63 = _dwhen 1195;;
let __a109 = _p 1025;;
let __a21 = _d 1155;;
let __a125 = _p 1296;;
let __a7 = _d 1046;;
let __a48 = fun p v -> _d 1223 p (_d 1222 p (v));;
let __a66 = _d 1266;;
let __a126 = _p 1027;;
let __a116 = _d 1377;;
let __a114 = fun p v -> _p 1229 p (_p 1228 p (v));;
let __a128 = fun p v -> _p 1233 p (_p 1232 p (_p 1231 p (v)));;
let __a76 = _p 1138;;
let __a32 = _d 1328;;
let __a47 = _d 1218;;
let __a129 = fun p v -> _p 1391 p (_p 1390 p (_ddelay 1389 p (_p 1388 p (_ddelay 1387 p (_d_and_push 1386 p (_d 1384 p (_d 1383 p (v))))))));;
let __a121 = _d 1379;;
let __a84 = _p 1029;;
let __a39 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a75 = fun p v -> _d_and_push 1087 p (_p 1086 p (_p 1085 p (_p 1084 p (_p 1083 p (_ddelay 1082 p (_d_and_push 1081 p (_ddelay 1080 p (_d_and_push 1079 p (_d 1077 p (_d 1076 p (v)))))))))));;
let __a22 = _d 1160;;
let __a12 = _d 1110;;
let __a120 = _p 1251;;
let __a85 = fun p v -> _p 1036 p (_p 1035 p (v));;
let __a8 = _d 1051;;
let __a144 = _p 1253;;
let __a33 = _d 1333;;
let __a28 = _d 1273;;
let __a73 = _p 1143;;
let __a147 = _p 1255;;
let __a51 = _d 1225;;
let __a23 = _d 1165;;
let __a13 = _d 1115;;
let __a9 = _d 1056;;
let __a145 = _p 1257;;
let __a74 = fun p v -> _d 1355 p (_d 1354 p (v));;
let __a82 = _p 1207;;
let __a140 = _p 1148;;
let __p122 = _dnext 1382;;
let __a95 = _p 1368;;
let __a34 = _d 1338;;
let __a103 = _p 1039;;
let __a80 = _p 1089;;
let __a57 = fun p v -> _d 1259 p (_d 1258 p (v));;
let __a27 = fun p v -> _d 1209 p (_d 1208 p (v));;
let __binder0 = __default_ret;;
let __binder1 = _m 1002;;
let __binder2 = _m 1009;;
let __binder3 = _m 1037;;
let __binder4 = _m 1230;;
let __binder5 = _m 1239;;
let binders : (sv -> sv -> sv) array = [| |]
let num_symbols = 12

let symbol_table = function
  | 267 -> "cmd-line-args"
  | 273 -> "command"
  | 268 -> "o"
  | 264 -> "CHAR"
  | 272 -> "analysis"
  | 271 -> "transformation"
  | 270 -> "file"
  | 265 -> "DIGIT"
  | 274 -> "args"
  | 275 -> "eof"
  | 269 -> "arg"
  | 266 -> "OCTET"
  | x -> if x < 256 then Yak.Pam_internal.default_symbol_table x else "?unknown?"

let get_symb_action = function
  | "cmd-line-args" -> 267
  | "command" -> 273
  | "o" -> 268
  | "CHAR" -> 264
  | "analysis" -> 272
  | "transformation" -> 271
  | "file" -> 270
  | "DIGIT" -> 265
  | "args" -> 274
  | "eof" -> 275
  | "arg" -> 269
  | "OCTET" -> 266
  | _ -> raise Not_found

let get_symb_start = function
  | 275 -> 12
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

and nullable_analysis __lookahead _p0_ _x0_ = None

and nullable_file __lookahead _p0_ _x0_ = None

and nullable_transformation __lookahead _p0_ _x0_ = None

and nullable_DIGIT __lookahead _p0_ _x0_ = None

and nullable_args __lookahead _p0_ _x0_ = None

and nullable_arg __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1022 and n = _dnext 1023 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (fun _x1_ _x2_ _x3_ -> ((((Pred.andc (Pred.full_lookaheadc false 264 1) (fun _x4_ _x5_ _x6_ -> (Some _x6_))) _x1_) _x2_) _x3_))) __lookahead) _p0_) (((_d 1018) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1017 p (_d 1016 p (_x53 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_eof __lookahead _p0_ _x0_ = ((((Pred.full_lookaheadc false 266 3) __lookahead) _p0_) _x0_)

and nullable_OCTET __lookahead _p0_ _x0_ = None

let program : (int * sv instruction list) list = [
(383, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,419)]);
(0, [ASimpleCont2Instr(275,__binder0,12);ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,420)]);
(1, [EatInstr(127,13);EatInstr(126,13);EatInstr(125,13);EatInstr(124,13);EatInstr(123,13);EatInstr(122,13);EatInstr(121,13);EatInstr(120,13);EatInstr(119,13);EatInstr(118,13);EatInstr(117,13);EatInstr(116,13);EatInstr(115,13);EatInstr(114,13);EatInstr(113,13);EatInstr(112,13);EatInstr(111,13);EatInstr(110,13);EatInstr(109,13);EatInstr(108,13);EatInstr(107,13);EatInstr(106,13);EatInstr(105,13);EatInstr(104,13);EatInstr(103,13);EatInstr(102,13);EatInstr(101,13);EatInstr(100,13);EatInstr(99,13);EatInstr(98,13);EatInstr(97,13);EatInstr(96,13);EatInstr(95,13);EatInstr(94,13);EatInstr(93,13);EatInstr(92,13);EatInstr(91,13);EatInstr(90,13);EatInstr(89,13);EatInstr(88,13);EatInstr(87,13);EatInstr(86,13);EatInstr(85,13);EatInstr(84,13);EatInstr(83,13);EatInstr(82,13);EatInstr(81,13);EatInstr(80,13);EatInstr(79,13);EatInstr(78,13);EatInstr(77,13);EatInstr(76,13);EatInstr(75,13);EatInstr(74,13);EatInstr(73,13);EatInstr(72,13);EatInstr(71,13);EatInstr(70,13);EatInstr(69,13);EatInstr(68,13);EatInstr(67,13);EatInstr(66,13);EatInstr(65,13);EatInstr(64,13);EatInstr(63,13);EatInstr(62,13);EatInstr(61,13);EatInstr(60,13);EatInstr(59,13);EatInstr(58,13);EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13);EatInstr(47,13);EatInstr(46,13);EatInstr(45,13);EatInstr(44,13);EatInstr(43,13);EatInstr(42,13);EatInstr(41,13);EatInstr(40,13);EatInstr(39,13);EatInstr(38,13);EatInstr(37,13);EatInstr(36,13);EatInstr(35,13);EatInstr(34,13);EatInstr(33,13);EatInstr(32,13);EatInstr(31,13);EatInstr(30,13);EatInstr(29,13);EatInstr(28,13);EatInstr(27,13);EatInstr(26,13);EatInstr(25,13);EatInstr(24,13);EatInstr(23,13);EatInstr(22,13);EatInstr(21,13);EatInstr(20,13);EatInstr(19,13);EatInstr(18,13);EatInstr(17,13);EatInstr(16,13);EatInstr(15,13);EatInstr(14,13);EatInstr(13,13);EatInstr(12,13);EatInstr(11,13);EatInstr(10,13);EatInstr(9,13);EatInstr(8,13);EatInstr(7,13);EatInstr(6,13);EatInstr(5,13);EatInstr(4,13);EatInstr(3,13);EatInstr(2,13);EatInstr(1,13)]);
(385, [AAction2Instr(__a85,421)]);
(2, [EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14)]);
(386, [EatInstr(101,422)]);
(3, [EatInstr(255,15);EatInstr(254,15);EatInstr(253,15);EatInstr(252,15);EatInstr(251,15);EatInstr(250,15);EatInstr(249,15);EatInstr(248,15);EatInstr(247,15);EatInstr(246,15);EatInstr(245,15);EatInstr(244,15);EatInstr(243,15);EatInstr(242,15);EatInstr(241,15);EatInstr(240,15);EatInstr(239,15);EatInstr(238,15);EatInstr(237,15);EatInstr(236,15);EatInstr(235,15);EatInstr(234,15);EatInstr(233,15);EatInstr(232,15);EatInstr(231,15);EatInstr(230,15);EatInstr(229,15);EatInstr(228,15);EatInstr(227,15);EatInstr(226,15);EatInstr(225,15);EatInstr(224,15);EatInstr(223,15);EatInstr(222,15);EatInstr(221,15);EatInstr(220,15);EatInstr(219,15);EatInstr(218,15);EatInstr(217,15);EatInstr(216,15);EatInstr(215,15);EatInstr(214,15);EatInstr(213,15);EatInstr(212,15);EatInstr(211,15);EatInstr(210,15);EatInstr(209,15);EatInstr(208,15);EatInstr(207,15);EatInstr(206,15);EatInstr(205,15);EatInstr(204,15);EatInstr(203,15);EatInstr(202,15);EatInstr(201,15);EatInstr(200,15);EatInstr(199,15);EatInstr(198,15);EatInstr(197,15);EatInstr(196,15);EatInstr(195,15);EatInstr(194,15);EatInstr(193,15);EatInstr(192,15);EatInstr(191,15);EatInstr(190,15);EatInstr(189,15);EatInstr(188,15);EatInstr(187,15);EatInstr(186,15);EatInstr(185,15);EatInstr(184,15);EatInstr(183,15);EatInstr(182,15);EatInstr(181,15);EatInstr(180,15);EatInstr(179,15);EatInstr(178,15);EatInstr(177,15);EatInstr(176,15);EatInstr(175,15);EatInstr(174,15);EatInstr(173,15);EatInstr(172,15);EatInstr(171,15);EatInstr(170,15);EatInstr(169,15);EatInstr(168,15);EatInstr(167,15);EatInstr(166,15);EatInstr(165,15);EatInstr(164,15);EatInstr(163,15);EatInstr(162,15);EatInstr(161,15);EatInstr(160,15);EatInstr(159,15);EatInstr(158,15);EatInstr(157,15);EatInstr(156,15);EatInstr(155,15);EatInstr(154,15);EatInstr(153,15);EatInstr(152,15);EatInstr(151,15);EatInstr(150,15);EatInstr(149,15);EatInstr(148,15);EatInstr(147,15);EatInstr(146,15);EatInstr(145,15);EatInstr(144,15);EatInstr(143,15);EatInstr(142,15);EatInstr(141,15);EatInstr(140,15);EatInstr(139,15);EatInstr(138,15);EatInstr(137,15);EatInstr(136,15);EatInstr(135,15);EatInstr(134,15);EatInstr(133,15);EatInstr(132,15);EatInstr(131,15);EatInstr(130,15);EatInstr(129,15);EatInstr(128,15);EatInstr(0,15);EatInstr(127,15);EatInstr(126,15);EatInstr(125,15);EatInstr(124,15);EatInstr(123,15);EatInstr(122,15);EatInstr(121,15);EatInstr(120,15);EatInstr(119,15);EatInstr(118,15);EatInstr(117,15);EatInstr(116,15);EatInstr(115,15);EatInstr(114,15);EatInstr(113,15);EatInstr(112,15);EatInstr(111,15);EatInstr(110,15);EatInstr(109,15);EatInstr(108,15);EatInstr(107,15);EatInstr(106,15);EatInstr(105,15);EatInstr(104,15);EatInstr(103,15);EatInstr(102,15);EatInstr(101,15);EatInstr(100,15);EatInstr(99,15);EatInstr(98,15);EatInstr(97,15);EatInstr(96,15);EatInstr(95,15);EatInstr(94,15);EatInstr(93,15);EatInstr(92,15);EatInstr(91,15);EatInstr(90,15);EatInstr(89,15);EatInstr(88,15);EatInstr(87,15);EatInstr(86,15);EatInstr(85,15);EatInstr(84,15);EatInstr(83,15);EatInstr(82,15);EatInstr(81,15);EatInstr(80,15);EatInstr(79,15);EatInstr(78,15);EatInstr(77,15);EatInstr(76,15);EatInstr(75,15);EatInstr(74,15);EatInstr(73,15);EatInstr(72,15);EatInstr(71,15);EatInstr(70,15);EatInstr(69,15);EatInstr(68,15);EatInstr(67,15);EatInstr(66,15);EatInstr(65,15);EatInstr(64,15);EatInstr(63,15);EatInstr(62,15);EatInstr(61,15);EatInstr(60,15);EatInstr(59,15);EatInstr(58,15);EatInstr(57,15);EatInstr(56,15);EatInstr(55,15);EatInstr(54,15);EatInstr(53,15);EatInstr(52,15);EatInstr(51,15);EatInstr(50,15);EatInstr(49,15);EatInstr(48,15);EatInstr(47,15);EatInstr(46,15);EatInstr(45,15);EatInstr(44,15);EatInstr(43,15);EatInstr(42,15);EatInstr(41,15);EatInstr(40,15);EatInstr(39,15);EatInstr(38,15);EatInstr(37,15);EatInstr(36,15);EatInstr(35,15);EatInstr(34,15);EatInstr(33,15);EatInstr(32,15);EatInstr(31,15);EatInstr(30,15);EatInstr(29,15);EatInstr(28,15);EatInstr(27,15);EatInstr(26,15);EatInstr(25,15);EatInstr(24,15);EatInstr(23,15);EatInstr(22,15);EatInstr(21,15);EatInstr(20,15);EatInstr(19,15);EatInstr(18,15);EatInstr(17,15);EatInstr(16,15);EatInstr(15,15);EatInstr(14,15);EatInstr(13,15);EatInstr(12,15);EatInstr(11,15);EatInstr(10,15);EatInstr(9,15);EatInstr(8,15);EatInstr(7,15);EatInstr(6,15);EatInstr(5,15);EatInstr(4,15);EatInstr(3,15);EatInstr(2,15);EatInstr(1,15)]);
(387, [EatInstr(100,423)]);
(4, [AAction2Instr(__a0,16)]);
(388, [AAction2Instr(__a86,268)]);
(5, [EatInstr(0,17)]);
(389, [AAction2Instr(__a87,268)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,18)]);
(390, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,424)]);
(7, [EatInstr(127,20);EatInstr(126,20);EatInstr(125,20);EatInstr(124,20);EatInstr(123,20);EatInstr(122,20);EatInstr(121,20);EatInstr(120,20);EatInstr(119,20);EatInstr(118,20);EatInstr(117,20);EatInstr(116,20);EatInstr(115,20);EatInstr(114,20);EatInstr(113,20);EatInstr(112,20);EatInstr(111,20);EatInstr(110,20);EatInstr(109,20);EatInstr(108,20);EatInstr(107,20);EatInstr(106,20);EatInstr(105,20);EatInstr(104,20);EatInstr(103,20);EatInstr(102,20);EatInstr(101,20);EatInstr(100,20);EatInstr(99,20);EatInstr(98,20);EatInstr(97,20);EatInstr(96,20);EatInstr(95,20);EatInstr(94,20);EatInstr(93,20);EatInstr(92,20);EatInstr(91,20);EatInstr(90,20);EatInstr(89,20);EatInstr(88,20);EatInstr(87,20);EatInstr(86,20);EatInstr(85,20);EatInstr(84,20);EatInstr(83,20);EatInstr(82,20);EatInstr(81,20);EatInstr(80,20);EatInstr(79,20);EatInstr(78,20);EatInstr(77,20);EatInstr(76,20);EatInstr(75,20);EatInstr(74,20);EatInstr(73,20);EatInstr(72,20);EatInstr(71,20);EatInstr(70,20);EatInstr(69,20);EatInstr(68,20);EatInstr(67,20);EatInstr(66,20);EatInstr(65,20);EatInstr(64,20);EatInstr(63,20);EatInstr(62,20);EatInstr(61,20);EatInstr(60,20);EatInstr(59,20);EatInstr(58,20);EatInstr(57,20);EatInstr(56,20);EatInstr(55,20);EatInstr(54,20);EatInstr(53,20);EatInstr(52,20);EatInstr(51,20);EatInstr(50,20);EatInstr(49,20);EatInstr(48,20);EatInstr(47,20);EatInstr(46,20);EatInstr(44,20);EatInstr(43,20);EatInstr(42,20);EatInstr(41,20);EatInstr(40,20);EatInstr(39,20);EatInstr(38,20);EatInstr(37,20);EatInstr(36,20);EatInstr(35,20);EatInstr(34,20);EatInstr(33,20);EatInstr(32,20);EatInstr(31,20);EatInstr(30,20);EatInstr(29,20);EatInstr(28,20);EatInstr(27,20);EatInstr(26,20);EatInstr(25,20);EatInstr(24,20);EatInstr(23,20);EatInstr(22,20);EatInstr(21,20);EatInstr(20,20);EatInstr(19,20);EatInstr(18,20);EatInstr(17,20);EatInstr(16,20);EatInstr(15,20);EatInstr(14,20);EatInstr(13,20);EatInstr(12,20);EatInstr(11,20);EatInstr(10,20);EatInstr(9,20);EatInstr(8,20);EatInstr(7,20);EatInstr(6,20);EatInstr(5,20);EatInstr(4,20);EatInstr(3,20);EatInstr(2,20);EatInstr(1,20)]);
(391, [AAction2Instr(__a88,268)]);
(8, [EatInstr(105,22);EatInstr(97,21)]);
(392, [EatInstr(101,425)]);
(9, [EatInstr(112,23)]);
(393, [EatInstr(100,426)]);
(10, [AAction2Instr(__a2,24)]);
(394, [EatInstr(99,427)]);
(11, [AAction2Instr(__a3,25)]);
(395, [EatInstr(108,428)]);
(12, [ALookaheadInstr(false,CfgLA (3,266),26);RCompleteInstr2(275,nullable_eof)]);
(396, [EatInstr(114,429)]);
(13, [CompleteInstr(264)]);
(397, [EatInstr(108,430)]);
(14, [CompleteInstr(265)]);
(398, [EatInstr(108,431)]);
(15, [CompleteInstr(266)]);
(399, [EatInstr(97,432)]);
(16, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder1,27)]);
(400, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,433)]);
(17, [CompleteInstr(268)]);
(401, [EatInstr(102,434)]);
(18, [AAction2Instr(__a4,29);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,28)]);
(402, [EatInstr(115,435)]);
(403, [EatInstr(111,436)]);
(20, [ALookaheadInstr(false,CfgLA (1,264),30);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,20)]);
(404, [EatInstr(112,437)]);
(21, [EatInstr(100,31)]);
(405, [AAction2Instr(__a89,438)]);
(22, [EatInstr(110,32)]);
(406, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,439)]);
(23, [EatInstr(114,33)]);
(407, [EatInstr(116,440)]);
(24, [EatInstr(114,35);EatInstr(101,34);AAction2Instr(__a27,58);AAction2Instr(__a26,57);AAction2Instr(__a25,56);AAction2Instr(__a24,55);AAction2Instr(__a23,54);AAction2Instr(__a22,53);AAction2Instr(__a21,52);AAction2Instr(__a20,51);AAction2Instr(__a19,50);AAction2Instr(__a18,49);AAction2Instr(__a17,48);AAction2Instr(__a16,47);AAction2Instr(__a15,46);AAction2Instr(__a14,45);AAction2Instr(__a13,44);AAction2Instr(__a12,43);AAction2Instr(__a11,42);AAction2Instr(__a10,41);AAction2Instr(__a9,40);AAction2Instr(__a8,39);AAction2Instr(__a7,38);AAction2Instr(__a6,37);AAction2Instr(__a5,36)]);
(408, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,441)]);
(25, [EatInstr(45,59);AAction2Instr(__a38,70);AAction2Instr(__a37,69);AAction2Instr(__a36,68);AAction2Instr(__a35,67);AAction2Instr(__a34,66);AAction2Instr(__a33,65);AAction2Instr(__a32,64);AAction2Instr(__a31,63);AAction2Instr(__a30,62);AAction2Instr(__a29,61);AAction2Instr(__a28,60)]);
(409, [EatInstr(115,442)]);
(26, [CompleteInstr(275)]);
(410, [EatInstr(97,443)]);
(27, [AAction2Instr(__a39,71)]);
(411, [EatInstr(99,444)]);
(28, [AAction2Instr(__a40,18)]);
(412, [EatInstr(114,445)]);
(29, [AWhenInstr3(__p42,__p41,72)]);
(413, [EatInstr(97,446)]);
(30, [CompleteInstr(270)]);
(414, [EatInstr(105,447)]);
(31, [EatInstr(100,73)]);
(415, [EatInstr(101,448)]);
(32, [EatInstr(108,74)]);
(416, [EatInstr(107,449)]);
(33, [EatInstr(101,75)]);
(417, [EatInstr(108,450)]);
(34, [EatInstr(120,76)]);
(418, [CompleteInstr(272)]);
(35, [EatInstr(102,77)]);
(419, [AAction2Instr(__a90,451)]);
(36, [EatInstr(97,78)]);
(420, [AAction2Instr(__a91,268)]);
(37, [EatInstr(97,79)]);
(421, [ASimpleCont2Instr(272,__binder3,452);ACallInstr3(__default_call,9)]);
(38, [EatInstr(99,80)]);
(422, [EatInstr(115,453)]);
(39, [EatInstr(99,81)]);
(423, [EatInstr(101,454)]);
(40, [EatInstr(100,82)]);
(424, [AAction2Instr(__a92,268)]);
(41, [EatInstr(100,83)]);
(425, [EatInstr(103,455)]);
(42, [EatInstr(100,84)]);
(426, [EatInstr(45,456)]);
(43, [EatInstr(101,85)]);
(427, [EatInstr(101,457)]);
(44, [EatInstr(102,86)]);
(428, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,458)]);
(45, [EatInstr(104,87)]);
(429, [EatInstr(101,459)]);
(46, [EatInstr(105,88)]);
(430, [EatInstr(108,460)]);
(47, [EatInstr(105,89)]);
(431, [EatInstr(101,461)]);
(48, [EatInstr(108,90)]);
(432, [EatInstr(116,462)]);
(49, [EatInstr(108,91)]);
(433, [AAction2Instr(__a93,268)]);
(50, [EatInstr(108,92)]);
(434, [EatInstr(111,463)]);
(51, [EatInstr(109,93)]);
(435, [EatInstr(108,464)]);
(52, [EatInstr(112,94)]);
(436, [EatInstr(108,465)]);
(53, [EatInstr(112,95)]);
(437, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,466)]);
(54, [EatInstr(112,96)]);
(438, [AAction2Instr(__a94,468);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,467)]);
(55, [EatInstr(112,97)]);
(439, [AAction2Instr(__a95,256)]);
(56, [EatInstr(112,98)]);
(440, [EatInstr(97,469)]);
(57, [EatInstr(112,99)]);
(441, [AAction2Instr(__a96,470)]);
(58, [AAction2Instr(__a44,101);AAction2Instr(__a43,100)]);
(442, [EatInstr(101,471)]);
(59, [EatInstr(117,104);EatInstr(114,103);EatInstr(99,102)]);
(443, [EatInstr(98,472)]);
(60, [EatInstr(45,105)]);
(444, [EatInstr(115,473)]);
(61, [EatInstr(45,106)]);
(445, [EatInstr(101,474)]);
(62, [EatInstr(45,107)]);
(446, [EatInstr(100,475)]);
(63, [EatInstr(45,108)]);
(447, [EatInstr(110,476)]);
(64, [EatInstr(45,109)]);
(448, [EatInstr(115,477)]);
(65, [EatInstr(45,110)]);
(449, [EatInstr(97,478)]);
(66, [EatInstr(45,111)]);
(450, [EatInstr(97,479)]);
(67, [EatInstr(45,112)]);
(451, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,480)]);
(68, [EatInstr(45,113)]);
(452, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,481)]);
(69, [EatInstr(45,114)]);
(453, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,482)]);
(70, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,115)]);
(454, [EatInstr(114,483)]);
(71, [AAction2Instr(__a46,117);AAction2Instr(__a45,116)]);
(72, [ALookaheadInstr(false,CfgLA (1,264),118)]);
(455, [EatInstr(117,484)]);
(456, [EatInstr(97,485)]);
(73, [EatInstr(45,119)]);
(457, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,486)]);
(74, [EatInstr(105,120)]);
(458, [AAction2Instr(__a97,268)]);
(75, [EatInstr(99,121)]);
(459, [EatInstr(100,487)]);
(76, [EatInstr(101,122)]);
(460, [EatInstr(97,488)]);
(77, [EatInstr(99,123)]);
(461, [EatInstr(118,489)]);
(78, [EatInstr(110,124)]);
(462, [EatInstr(101,490)]);
(79, [EatInstr(116,125)]);
(463, [EatInstr(114,491)]);
(80, [EatInstr(108,126)]);
(464, [EatInstr(97,492)]);
(81, [EatInstr(111,127)]);
(465, [EatInstr(108,493)]);
(82, [EatInstr(101,128)]);
(466, [AAction2Instr(__a98,268)]);
(83, [EatInstr(105,129)]);
(467, [AAction2Instr(__a99,438)]);
(84, [EatInstr(111,130)]);
(468, [AWhenInstr3(__p101,__p100,494)]);
(85, [EatInstr(120,131)]);
(469, [EatInstr(114,495)]);
(86, [EatInstr(117,132)]);
(470, [EatInstr(116,498);EatInstr(112,497);EatInstr(102,496)]);
(87, [EatInstr(97,133)]);
(471, [EatInstr(110,499)]);
(88, [EatInstr(110,134)]);
(472, [EatInstr(101,500)]);
(89, [EatInstr(110,135)]);
(473, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,501)]);
(90, [EatInstr(101,136)]);
(474, [EatInstr(103,502)]);
(91, [EatInstr(105,137)]);
(475, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,503)]);
(92, [EatInstr(111,138)]);
(476, [EatInstr(101,504)]);
(93, [EatInstr(105,139)]);
(477, [EatInstr(99,505)]);
(94, [EatInstr(114,140)]);
(478, [EatInstr(104,506)]);
(95, [EatInstr(114,141)]);
(479, [EatInstr(114,507)]);
(96, [EatInstr(114,142)]);
(480, [AAction2Instr(__a102,347)]);
(97, [EatInstr(114,143)]);
(481, [AAction2Instr(__a103,268)]);
(98, [EatInstr(114,144)]);
(482, [AAction2Instr(__a104,268)]);
(99, [EatInstr(114,145)]);
(483, [EatInstr(45,508)]);
(100, [EatInstr(115,146)]);
(484, [EatInstr(108,509)]);
(101, [AAction2Instr(__a48,148);AAction2Instr(__a47,147)]);
(485, [EatInstr(110,510)]);
(102, [EatInstr(111,149)]);
(486, [AAction2Instr(__a105,268)]);
(103, [EatInstr(111,150)]);
(487, [EatInstr(115,511)]);
(104, [EatInstr(110,151)]);
(488, [EatInstr(98,512)]);
(105, [EatInstr(98,152)]);
(489, [EatInstr(97,513)]);
(106, [EatInstr(99,153)]);
(490, [EatInstr(45,514)]);
(107, [EatInstr(99,154)]);
(491, [EatInstr(109,515)]);
(108, [EatInstr(105,155)]);
(492, [EatInstr(116,516)]);
(109, [EatInstr(105,156)]);
(493, [EatInstr(45,517)]);
(110, [EatInstr(108,157)]);
(494, [AAction2Instr(__a106,518)]);
(111, [EatInstr(110,158)]);
(495, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,519)]);
(112, [EatInstr(110,159)]);
(496, [EatInstr(117,520)]);
(113, [EatInstr(111,160)]);
(497, [EatInstr(101,521)]);
(114, [EatInstr(118,161)]);
(498, [EatInstr(120,522)]);
(115, [AAction2Instr(__a49,162)]);
(499, [EatInstr(115,523)]);
(116, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder2,163)]);
(500, [EatInstr(108,524)]);
(117, [ACallInstr3(__default_call,12);ASimpleCont2Instr(275,__binder0,164)]);
(501, [AAction2Instr(__a107,256)]);
(118, [CompleteInstr(269)]);
(502, [EatInstr(117,525)]);
(119, [EatInstr(108,165)]);
(503, [AAction2Instr(__a108,256)]);
(120, [EatInstr(110,166)]);
(504, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,526)]);
(121, [EatInstr(101,167)]);
(505, [EatInstr(101,527)]);
(122, [EatInstr(99,168)]);
(506, [EatInstr(101,528)]);
(123, [AAction2Instr(__a50,169)]);
(507, [AAction2Instr(__a109,529)]);
(124, [EatInstr(97,170)]);
(508, [EatInstr(99,530)]);
(125, [EatInstr(116,171)]);
(509, [EatInstr(97,531)]);
(126, [EatInstr(111,172)]);
(510, [EatInstr(97,532)]);
(127, [EatInstr(109,173)]);
(511, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,533)]);
(128, [EatInstr(115,174)]);
(512, [EatInstr(108,534)]);
(129, [EatInstr(115,175)]);
(513, [EatInstr(110,535)]);
(130, [EatInstr(116,176)]);
(514, [EatInstr(97,536)]);
(131, [EatInstr(116,177)]);
(515, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,537)]);
(132, [EatInstr(115,178)]);
(516, [EatInstr(101,538)]);
(133, [EatInstr(115,179)]);
(517, [EatInstr(115,539)]);
(134, [EatInstr(102,180)]);
(518, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,540)]);
(135, [EatInstr(108,181)]);
(519, [AAction2Instr(__a110,541)]);
(136, [EatInstr(120,182)]);
(520, [EatInstr(110,542)]);
(137, [EatInstr(102,183)]);
(521, [EatInstr(103,543)]);
(138, [EatInstr(111,184)]);
(522, [AAction2Instr(__a111,544)]);
(139, [EatInstr(110,185)]);
(523, [EatInstr(105,545)]);
(140, [EatInstr(101,186)]);
(524, [EatInstr(115,546)]);
(141, [EatInstr(105,187)]);
(525, [EatInstr(108,547)]);
(142, [EatInstr(105,188)]);
(526, [AAction2Instr(__a112,256)]);
(143, [EatInstr(105,189)]);
(527, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,548)]);
(144, [EatInstr(105,190)]);
(528, [EatInstr(97,549)]);
(145, [EatInstr(105,191)]);
(529, [CompleteInstr(271)]);
(146, [EatInstr(116,192)]);
(530, [EatInstr(111,550)]);
(147, [EatInstr(115,193)]);
(531, [EatInstr(114,551)]);
(148, [AAction2Instr(__a52,195);AAction2Instr(__a51,194)]);
(532, [EatInstr(108,552)]);
(149, [EatInstr(117,196)]);
(533, [AAction2Instr(__a113,268)]);
(150, [EatInstr(111,197)]);
(534, [EatInstr(101,553)]);
(151, [EatInstr(114,198)]);
(535, [EatInstr(99,554)]);
(152, [EatInstr(97,199)]);
(536, [EatInstr(99,555)]);
(153, [EatInstr(97,200)]);
(537, [AAction2Instr(__a114,556)]);
(154, [EatInstr(104,201)]);
(538, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,557)]);
(155, [EatInstr(110,202)]);
(539, [EatInstr(116,558)]);
(156, [EatInstr(110,203)]);
(540, [AAction2Instr(__a115,256)]);
(157, [EatInstr(111,204)]);
(541, [AAction2Instr(__a116,560);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,559)]);
(158, [EatInstr(101,205)]);
(542, [AAction2Instr(__a117,544)]);
(159, [EatInstr(111,206)]);
(543, [EatInstr(45,561);AAction2Instr(__a118,544)]);
(160, [EatInstr(110,207)]);
(544, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,562)]);
(161, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,208)]);
(545, [EatInstr(116,563)]);
(162, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,209)]);
(546, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,564)]);
(163, [AAction2Instr(__a53,71)]);
(547, [EatInstr(97,565)]);
(164, [AAction2Instr(__a54,210)]);
(548, [AAction2Instr(__a119,256)]);
(165, [EatInstr(114,211)]);
(549, [EatInstr(100,566)]);
(166, [EatInstr(101,212)]);
(550, [EatInstr(114,567)]);
(167, [EatInstr(100,213)]);
(551, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,568)]);
(168, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,214)]);
(552, [EatInstr(121,569)]);
(169, [AAction2Instr(__a55,216);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,215)]);
(553, [EatInstr(45,570)]);
(170, [EatInstr(108,217)]);
(554, [EatInstr(101,571)]);
(171, [EatInstr(114,218)]);
(555, [EatInstr(116,572)]);
(172, [EatInstr(115,219)]);
(556, [ASimpleCont2Instr(271,__binder4,573);ACallInstr3(__default_call,8)]);
(173, [EatInstr(112,220)]);
(557, [AAction2Instr(__a120,574)]);
(174, [EatInstr(117,221)]);
(558, [EatInstr(97,575)]);
(175, [EatInstr(112,222)]);
(559, [AAction2Instr(__a121,541)]);
(176, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,223)]);
(560, [AWhenInstr3(__p123,__p122,576)]);
(177, [EatInstr(114,224)]);
(561, [EatInstr(115,577)]);
(178, [EatInstr(101,225)]);
(562, [AAction2Instr(__a124,256)]);
(179, [EatInstr(104,226)]);
(563, [EatInstr(105,578)]);
(180, [EatInstr(111,227)]);
(564, [AAction2Instr(__a125,256)]);
(181, [EatInstr(105,228)]);
(565, [EatInstr(114,579)]);
(182, [EatInstr(101,229)]);
(566, [AAction2Instr(__a126,529)]);
(183, [EatInstr(116,230)]);
(567, [EatInstr(101,580)]);
(184, [EatInstr(107,231)]);
(568, [AAction2Instr(__a127,268)]);
(185, [EatInstr(117,232)]);
(569, [EatInstr(115,581)]);
(186, [EatInstr(99,233)]);
(570, [EatInstr(112,582)]);
(187, [EatInstr(110,234)]);
(571, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,583)]);
(188, [EatInstr(110,235)]);
(572, [EatInstr(105,584)]);
(189, [EatInstr(110,236)]);
(573, [AAction2Instr(__a128,585)]);
(190, [EatInstr(110,237)]);
(574, [EatInstr(100,586)]);
(191, [EatInstr(110,238)]);
(575, [EatInstr(114,587)]);
(192, [EatInstr(114,239)]);
(576, [AAction2Instr(__a129,588)]);
(193, [EatInstr(117,240)]);
(577, [EatInstr(116,589)]);
(194, [EatInstr(116,241)]);
(578, [EatInstr(118,590)]);
(195, [AAction2Instr(__a57,243);AAction2Instr(__a56,242)]);
(579, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,591)]);
(196, [EatInstr(110,244)]);
(580, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,592)]);
(197, [EatInstr(116,245)]);
(581, [EatInstr(105,593)]);
(198, [EatInstr(111,246)]);
(582, [EatInstr(114,594)]);
(199, [EatInstr(99,247)]);
(583, [AAction2Instr(__a130,268)]);
(200, [EatInstr(115,248)]);
(584, [EatInstr(111,595)]);
(201, [EatInstr(101,249)]);
(585, [AAction2Instr(__a132,597);AAction2Instr(__a131,596)]);
(202, [EatInstr(108,250)]);
(586, [EatInstr(121,598)]);
(203, [EatInstr(108,251)]);
(587, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,599)]);
(204, [EatInstr(111,252)]);
(588, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,600)]);
(205, [EatInstr(119,253)]);
(589, [EatInstr(114,601)]);
(206, [EatInstr(45,254)]);
(590, [EatInstr(101,602)]);
(207, [EatInstr(108,255)]);
(591, [AAction2Instr(__a133,256)]);
(208, [AAction2Instr(__a58,256)]);
(592, [AAction2Instr(__a134,268)]);
(209, [AAction2Instr(__a59,256)]);
(593, [EatInstr(115,603)]);
(210, [CompleteInstr(267)]);
(594, [EatInstr(101,604)]);
(211, [EatInstr(49,257)]);
(595, [EatInstr(110,605)]);
(212, [EatInstr(45,258)]);
(596, [EatInstr(44,606)]);
(213, [EatInstr(101,259)]);
(597, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,607)]);
(214, [AAction2Instr(__a60,260)]);
(598, [EatInstr(112,608)]);
(215, [AAction2Instr(__a61,169)]);
(599, [AAction2Instr(__a135,268)]);
(216, [AWhenInstr3(__p63,__p62,261)]);
(600, [AAction2Instr(__a136,256)]);
(217, [EatInstr(121,262)]);
(601, [EatInstr(105,609)]);
(218, [EatInstr(105,263)]);
(602, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,610)]);
(219, [EatInstr(101,264)]);
(603, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,611)]);
(220, [EatInstr(105,265)]);
(604, [EatInstr(100,612)]);
(221, [EatInstr(103,266)]);
(605, [EatInstr(115,613)]);
(222, [EatInstr(97,267)]);
(606, [AAction2Instr(__a137,614)]);
(223, [AAction2Instr(__a64,268)]);
(607, [AAction2Instr(__a138,268)]);
(224, [EatInstr(97,269)]);
(608, [EatInstr(103,615)]);
(225, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,270)]);
(609, [EatInstr(99,616)]);
(226, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,271)]);
(610, [AAction2Instr(__a139,256)]);
(227, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,272)]);
(611, [AAction2Instr(__a140,268)]);
(228, [EatInstr(110,273)]);
(612, [EatInstr(105,617)]);
(229, [EatInstr(114,274)]);
(613, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,618)]);
(230, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,275)]);
(614, [ASimpleCont2Instr(271,__binder5,619);ACallInstr3(__default_call,8)]);
(231, [EatInstr(97,276)]);
(615, [EatInstr(101,620)]);
(232, [EatInstr(115,277)]);
(616, [EatInstr(116,621)]);
(233, [EatInstr(101,278)]);
(617, [EatInstr(99,622)]);
(234, [EatInstr(116,279)]);
(618, [AAction2Instr(__a141,268)]);
(235, [EatInstr(116,280)]);
(619, [AAction2Instr(__a142,585)]);
(236, [EatInstr(116,281)]);
(620, [EatInstr(110,623)]);
(237, [EatInstr(116,282)]);
(621, [AAction2Instr(__a143,544)]);
(238, [EatInstr(116,283)]);
(622, [EatInstr(97,624)]);
(239, [EatInstr(105,284)]);
(623, [EatInstr(45,625);AAction2Instr(__a144,626)]);
(240, [EatInstr(98,285)]);
(624, [EatInstr(116,627)]);
(241, [EatInstr(114,286)]);
(625, [EatInstr(115,628)]);
(242, [EatInstr(116,287)]);
(626, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,629)]);
(243, [AAction2Instr(__a66,289);AAction2Instr(__a65,288)]);
(627, [EatInstr(101,630)]);
(244, [EatInstr(116,290)]);
(628, [EatInstr(99,631)]);
(245, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,291)]);
(629, [AAction2Instr(__a145,268)]);
(246, [EatInstr(108,292)]);
(630, [EatInstr(115,632)]);
(247, [EatInstr(107,293)]);
(631, [EatInstr(97,633)]);
(248, [EatInstr(101,294)]);
(632, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,634)]);
(249, [EatInstr(99,295)]);
(633, [EatInstr(110,635)]);
(250, [EatInstr(105,296)]);
(634, [AAction2Instr(__a146,268)]);
(251, [EatInstr(105,297)]);
(635, [EatInstr(110,636)]);
(252, [EatInstr(107,298)]);
(636, [EatInstr(101,637)]);
(253, [EatInstr(45,299)]);
(637, [EatInstr(114,638)]);
(254, [EatInstr(99,300)]);
(638, [EatInstr(108,639)]);
(255, [EatInstr(121,301)]);
(639, [EatInstr(101,640)]);
(256, [AAction2Instr(__a67,302)]);
(640, [EatInstr(115,641)]);
(257, [EatInstr(45,303)]);
(641, [EatInstr(115,642)]);
(258, [EatInstr(114,304)]);
(642, [AAction2Instr(__a147,626)]);
(259, [EatInstr(110,305)]);
(260, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,306)]);
(261, [AAction2Instr(__a68,307)]);
(262, [EatInstr(122,308)]);
(263, [EatInstr(98,309)]);
(264, [EatInstr(45,310)]);
(265, [EatInstr(108,311)]);
(266, [EatInstr(97,312)]);
(267, [EatInstr(116,313)]);
(268, [AAction2Instr(__a69,314)]);
(269, [EatInstr(99,315)]);
(270, [AAction2Instr(__a70,268)]);
(271, [AAction2Instr(__a71,268)]);
(272, [AAction2Instr(__a72,268)]);
(273, [EatInstr(101,316)]);
(274, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,317)]);
(275, [AAction2Instr(__a73,268)]);
(276, [EatInstr(104,318)]);
(277, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,319)]);
(278, [EatInstr(100,320)]);
(279, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,321)]);
(280, [EatInstr(45,322)]);
(281, [EatInstr(45,323)]);
(282, [EatInstr(45,324)]);
(283, [EatInstr(45,325)]);
(284, [EatInstr(112,326)]);
(285, [EatInstr(115,327)]);
(286, [EatInstr(97,328)]);
(287, [EatInstr(114,329)]);
(288, [EatInstr(117,330)]);
(289, [EatInstr(119,331)]);
(290, [EatInstr(101,332)]);
(291, [AAction2Instr(__a74,333)]);
(292, [EatInstr(108,334)]);
(293, [EatInstr(101,335)]);
(294, [EatInstr(45,336)]);
(295, [EatInstr(107,337)]);
(296, [EatInstr(110,338)]);
(297, [EatInstr(110,339)]);
(298, [EatInstr(97,340)]);
(299, [EatInstr(101,341)]);
(300, [EatInstr(111,342)]);
(301, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,343)]);
(302, [CompleteInstr(274)]);
(303, [EatInstr(108,344)]);
(304, [EatInstr(101,345)]);
(305, [EatInstr(99,346)]);
(306, [AAction2Instr(__a75,347)]);
(307, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,348)]);
(308, [EatInstr(101,349)]);
(309, [EatInstr(117,350)]);
(310, [EatInstr(117,351)]);
(311, [EatInstr(101,352)]);
(312, [EatInstr(114,353)]);
(313, [EatInstr(99,354)]);
(314, [CompleteInstr(273)]);
(315, [EatInstr(116,355)]);
(316, [EatInstr(45,356)]);
(317, [AAction2Instr(__a76,268)]);
(318, [EatInstr(101,357)]);
(319, [AAction2Instr(__a77,268)]);
(320, [EatInstr(101,358)]);
(321, [AAction2Instr(__a78,268)]);
(322, [EatInstr(103,359)]);
(323, [EatInstr(110,360)]);
(324, [EatInstr(110,361)]);
(325, [EatInstr(114,362)]);
(326, [EatInstr(45,363)]);
(327, [EatInstr(101,364)]);
(328, [EatInstr(110,365)]);
(329, [EatInstr(97,366)]);
(330, [EatInstr(110,367)]);
(331, [EatInstr(114,368)]);
(332, [EatInstr(114,369)]);
(333, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,370)]);
(334, [EatInstr(45,371)]);
(335, [EatInstr(110,372)]);
(336, [EatInstr(105,373)]);
(337, [EatInstr(45,374)]);
(338, [EatInstr(101,375)]);
(339, [EatInstr(101,376)]);
(340, [EatInstr(104,377)]);
(341, [EatInstr(110,378)]);
(342, [EatInstr(97,379)]);
(343, [AAction2Instr(__a79,256)]);
(344, [EatInstr(111,380)]);
(345, [EatInstr(103,381)]);
(346, [EatInstr(101,382)]);
(347, [AAction2Instr(__a81,384);AAction2Instr(__a80,383)]);
(348, [AAction2Instr(__a82,268)]);
(349, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,385)]);
(350, [EatInstr(116,386)]);
(351, [EatInstr(110,387)]);
(352, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,388)]);
(353, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,389)]);
(354, [EatInstr(104,390)]);
(355, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,391)]);
(356, [EatInstr(114,392)]);
(357, [EatInstr(97,393)]);
(358, [EatInstr(110,394)]);
(359, [EatInstr(105,395)]);
(360, [EatInstr(112,396)]);
(361, [EatInstr(117,397)]);
(362, [EatInstr(101,398)]);
(363, [EatInstr(108,399)]);
(364, [EatInstr(116,400)]);
(365, [EatInstr(115,401)]);
(366, [EatInstr(110,402)]);
(367, [EatInstr(114,403)]);
(368, [EatInstr(97,404)]);
(369, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,405)]);
(370, [AAction2Instr(__a83,406)]);
(371, [EatInstr(115,407)]);
(372, [EatInstr(100,408)]);
(373, [EatInstr(110,409)]);
(374, [EatInstr(108,410)]);
(375, [EatInstr(45,411)]);
(376, [EatInstr(45,412)]);
(377, [EatInstr(101,413)]);
(378, [EatInstr(103,414)]);
(379, [EatInstr(108,415)]);
(380, [EatInstr(111,416)]);
(381, [EatInstr(117,417)]);
(382, [AAction2Instr(__a84,418)]);
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
