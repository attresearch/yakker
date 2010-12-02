
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

type transformation = Add_LR1_lookahead_Tx

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
| Minus_cmd
| Precedence_analysis_cmd
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
_r_transformation(_n,ykinput) = (ignore (*1025*) (_n());  Add_LR1_lookahead_Tx )
 
 and
_r_command(_n,ykinput) = (ignore (*1026*) (_n()); 
 (let _x41 = 
 (match _n() with
 | (1031) -> ( Attributes_cmd )
 | (1036) -> ( Close_under_core_cmd )
 | (1041) -> ( Compile_cmd )
 | (1046) -> ( Desugar_cmd )
 | (1051) -> ( Dispatch_cmd )
 | (1056) -> ( Dot_cmd )
 | (1066) -> (
 (let _x9 = (ignore (*1067*) (_n()); _n())
  in (ignore (*1068*) (_n()); 
 (let _x8 = (ignore (*1069*) (_n()); _n())
  in (ignore (*1070*) (_n()); 
 (let f = (ignore (*1071*) (_n()); Yak.YkBuf.get_string _x9 _x8 ykinput)
  in (ignore (*1072*) (_n()); 
 (let l = (ignore (*1073*) (_n()); 
 (let _x15 = (ignore (*1074*) (_n()); 
 (let rec _x49 _x15 = (match _n() with 1075 -> _x15 | _x48 -> _x49((ignore (*1076*) (_x48); 
 (let _x14 = (ignore (*1085*) (_n()); 
 (let _x13 = (ignore (*1086*) (_n()); _n())
  in (ignore (*1087*) (_n()); 
 (let _x12 = (ignore (*1088*) (_n()); _n())
  in (ignore (*1089*) (_n()); 
 (let x = (ignore (*1090*) (_n()); Yak.YkBuf.get_string _x13 _x12 ykinput)
  in (ignore (*1091*) (_n()); x)
 ))
 ))
 ))
  in (ignore (*1092*) (_n()); _x14::_x15)
 ))
 )) in _x49([])))
  in (ignore (*1093*) (_n()); (List.rev _x15))
 ))
  in (ignore (*1095*) (_n());  files := f::!files; exec_l := l; Exec_cmd )
 ))
 ))
 ))
 ))
 | (1100) -> ( Extract_cmd )
 | (1105) -> ( Compileopt.coalesce := true; Fuse_cmd )
 | (1110) -> ( Hash_cmd )
 | (1115) -> ( Info_cmd )
 | (1120) -> ( Compileopt.inline_regular := true; Inline_regular_cmd )
 | (1125) -> ( Lexer_cmd )
 | (1130) -> ( Lift_cmd )
 | (1135) -> ( Lookahead_analysis_cmd )
 | (1140) -> ( Minus_cmd )
 | (1145) -> ( Tx_prec_cmd )
 | (1150) -> ( Precedence_analysis_cmd )
 | (1155) -> ( Print_gul_cmd )
 | (1160) -> ( Print_gil_cmd )
 | (1165) -> ( Print_npreds_cmd )
 | (1170) -> ( Print_npreds_cmd )
 | (1175) -> ( Print_relevance_cmd )
 | (1192) -> (
 (let _x20 = (ignore (*1193*) (_n()); _n())
  in (ignore (*1194*) (_n()); 
 (let _x19 = (ignore (*1195*) (_n()); _n())
  in (ignore (*1196*) (_n()); 
 (let n = (ignore (*1197*) (_n()); Yak.YkBuf.get_string _x20 _x19 ykinput)
  in (ignore (*1199*) (_n());  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" )
 ))
 ))
 ))
 | (1206) -> ( Strip_late_actions_cmd )
 | (1213) -> ( Subset_cmd )
 | (1220) -> (
 (let tx = (ignore (*1221*) (_n()); _r_transformation(_n,ykinput))
  in (ignore (*1223*) (_n()); 
 (let txs = (ignore (*1224*) (_n()); 
 (let _x22 = (ignore (*1225*) (_n()); 
 (let rec _x47 _x22 = (match _n() with 1226 -> _x22 | _x46 -> _x47((ignore (*1227*) (_x46); 
 (let _x21 = (ignore (*1229*) (_n()); 
 (let x = (ignore (*1230*) (_n()); _r_transformation(_n,ykinput))
  in (ignore (*1232*) (_n()); x)
 ))
  in (ignore (*1233*) (_n()); _x21::_x22)
 ))
 )) in _x47([])))
  in (ignore (*1234*) (_n()); (List.rev _x22))
 ))
  in (ignore (*1236*) (_n());  transforms := (tx::txs); Transform_cmd )
 ))
 ))
 | (1243) -> (
 (let t = 
 (match _n() with
 | (1245) -> (Dypgen_PI false)
 | _(*1247*) -> (Dypgen_PI true)
 ) in (ignore (*1249*) (_n());  translate_plugin := t; Translate_cmd )
 ))
 | (1256) -> ( if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1; Unroll_star_cmd )
 | _(*1261*) -> ( Wrap_cmd )
 ) in (ignore (*1262*) (_n()); _x41)
 ))
 
 and
_r_args(_n,ykinput) = (ignore (*1263*) (_n()); 
 (let _x42 = 
 (match _n() with
 | (1268) -> (
 (let b = 
 (match _n() with
 | (1270) -> (Fun_BE)
 | (1272) -> (Trans_BE)
 | (1274) -> (Peg_BE false)
 | _(*1276*) -> (Peg_BE true)
 ) in (ignore (*1278*) (_n());  backend := b )
 ))
 | (1283) -> ( Compileopt.case_sensitive := false )
 | (1288) -> ( Compileopt.check_labels := true )
 | (1306) -> (
 (let _x27 = (ignore (*1307*) (_n()); _n())
  in (ignore (*1308*) (_n()); 
 (let _x26 = (ignore (*1309*) (_n()); _n())
  in (ignore (*1310*) (_n()); 
 (let n = (ignore (*1311*) (_n()); Yak.YkBuf.get_string _x27 _x26 ykinput)
  in (ignore (*1313*) (_n());  Variables.counter := (int_of_string n) )
 ))
 ))
 ))
 | (1318) -> ( Compileopt.inline_cs := true )
 | (1323) -> ( Compileopt.inline_regular := true )
 | (1328) -> ( Compileopt.lookahead := true )
 | (1333) -> ( Yak.Pami.new_engine_flag := true )
 | (1338) -> ( Compileopt.coalesce := false )
 | (1343) -> ( only := true )
 | (1353) -> (
 (let _x31 = (ignore (*1354*) (_n()); _n())
  in (ignore (*1355*) (_n()); 
 (let _x30 = (ignore (*1356*) (_n()); _n())
  in (ignore (*1357*) (_n()); 
 (let x = (ignore (*1358*) (_n()); Yak.YkBuf.get_string _x31 _x30 ykinput)
  in (ignore (*1360*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | (1378) -> (
 (let _x36 = (ignore (*1379*) (_n()); _n())
  in (ignore (*1380*) (_n()); 
 (let _x35 = (ignore (*1381*) (_n()); _n())
  in (ignore (*1382*) (_n()); 
 (let n = (ignore (*1383*) (_n()); Yak.YkBuf.get_string _x36 _x35 ykinput)
  in (ignore (*1385*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1390) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | _(*1398*) -> (
 (let _x40 = (ignore (*1399*) (_n()); _n())
  in (ignore (*1400*) (_n()); 
 (let _x39 = (ignore (*1401*) (_n()); _n())
  in (ignore (*1402*) (_n()); 
 (let f = (ignore (*1403*) (_n()); Yak.YkBuf.get_string _x40 _x39 ykinput)
  in (ignore (*1405*) (_n());  files := f::!files )
 ))
 ))
 ))
 ) in (ignore (*1406*) (_n()); _x42)
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
 | 1028 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1033 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1038 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1043 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1048 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1053 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1059 ->
 (fun pos_ -> let _x73 _x7  = _t(fun _(*1063*) pos_ -> let _x74 _x6  = _t(fun _(*1066*) pos_ -> let _x76 _x75  = _t(fun _(*1068*) pos_ -> let _x78 _x77  = _t(fun _(*1074*) pos_ -> let rec _x80 _x79  = _t(function
 | 1075 ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore(_x79);_wv0);_wv0);_wv0))
 | _(*1078*) ->
 (fun pos_ -> let _x81 _x11  = _t(fun _(*1082*) pos_ -> let _x82 _x10  = _t(fun _(*1085*) pos_ -> let _x84 _x83  = _t(fun _(*1088*) pos_ -> Yk_delay(_x80 (ignore(ignore((_wv0));_wv0);_wv0) ,_x10)) in _t(fun _(*1086*) pos_ -> Yk_delay(_x84 ((_wv0)) ,_x11))) in _t(fun _(*1083*) pos_ -> _x82 (pos_) )) in _t(fun _(*1079*) pos_ -> _x81 (pos_) ))) in _x80 (_wv0) ) in _t(fun _(*1069*) pos_ -> Yk_delay(_x78 ((_wv0)) ,_x6))) in _t(fun _(*1067*) pos_ -> Yk_delay(_x76 ((_wv0)) ,_x7))) in _t(fun _(*1064*) pos_ -> _x74 (pos_) )) in _t(fun _(*1060*) pos_ -> _x73 (pos_) ))
 | 1097 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1102 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1107 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1112 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1117 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1122 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1127 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1132 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1137 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1142 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1147 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1152 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1157 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1162 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1167 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1172 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1177 ->
 (fun pos_ -> let _x65 _x18  = _t(fun _(*1180*) pos_ -> let _x67 _x66  = _t(fun _(*1189*) pos_ -> let _x70 _x17  = _t(fun _(*1192*) pos_ -> let _x72 _x71  = _t(fun _(*1195*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x17)) in _t(fun _(*1193*) pos_ -> Yk_delay(_x72 ((_wv0)) ,_x18))) in _t(fun _(*1190*) pos_ -> _x70 (pos_) )) in _t(fun _(*1181*) pos_ -> let _x68 _x16  = _t(function
 | 1187 ->
 (fun pos_ -> Yk_when(_x16>=1))
 | _(*1188*) ->
 (fun pos_ -> _x67 (ignore((_wv0));_wv0) )) in _t(fun _(*1182*) pos_ -> let rec _x69 _x16  = _t(function
 | 1183 ->
 (fun pos_ -> _x68 (_x16) )
 | _(*1185*) ->
 (fun pos_ -> _x69 (_x16+1) )) in _x69 (0) ))) in _t(fun _(*1178*) pos_ -> _x65 (pos_) ))
 | _(*1200*) ->
 (fun pos_ -> let _x56 _x55  = _t(function
 | 1203 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1207*) ->
 (fun pos_ -> let _x58 _x57  = _t(function
 | 1210 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1214*) ->
 (fun pos_ -> let _x60 _x59  = _t(function
 | 1217 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1237*) ->
 (fun pos_ -> let _x62 _x61  = _t(function
 | 1240 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1250*) ->
 (fun pos_ -> let _x64 _x63  = _t(function
 | 1253 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1258*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1251*) pos_ -> _x64 (()) ))) in _t(fun _(*1238*) pos_ -> _x62 (()) ))) in _t(fun _(*1215*) pos_ -> _x60 (()) ))) in _t(fun _(*1208*) pos_ -> _x58 (()) ))) in _t(fun _(*1201*) pos_ -> _x56 (()) ))),_x54))
let _x111 =
 (fun _(*pos*) (_,_x86)(*arg of args*) -> (_t(function
 | 1265 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1280 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1285 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1291 ->
 (fun pos_ -> let _x103 _x25  = _t(fun _(*1294*) pos_ -> let _x105 _x104  = _t(fun _(*1303*) pos_ -> let _x108 _x24  = _t(fun _(*1306*) pos_ -> let _x110 _x109  = _t(fun _(*1309*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x24)) in _t(fun _(*1307*) pos_ -> Yk_delay(_x110 ((_wv0)) ,_x25))) in _t(fun _(*1304*) pos_ -> _x108 (pos_) )) in _t(fun _(*1295*) pos_ -> let _x106 _x23  = _t(function
 | 1301 ->
 (fun pos_ -> Yk_when(_x23>=1))
 | _(*1302*) ->
 (fun pos_ -> _x105 (ignore((_wv0));_wv0) )) in _t(fun _(*1296*) pos_ -> let rec _x107 _x23  = _t(function
 | 1297 ->
 (fun pos_ -> _x106 (_x23) )
 | _(*1299*) ->
 (fun pos_ -> _x107 (_x23+1) )) in _x107 (0) ))) in _t(fun _(*1292*) pos_ -> _x103 (pos_) ))
 | 1315 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1320 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1325 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1330 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1335 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1340 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1346 ->
 (fun pos_ -> let _x99 _x29  = _t(fun _(*1350*) pos_ -> let _x100 _x28  = _t(fun _(*1353*) pos_ -> let _x102 _x101  = _t(fun _(*1356*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x28)) in _t(fun _(*1354*) pos_ -> Yk_delay(_x102 ((_wv0)) ,_x29))) in _t(fun _(*1351*) pos_ -> _x100 (pos_) )) in _t(fun _(*1347*) pos_ -> _x99 (pos_) ))
 | 1363 ->
 (fun pos_ -> let _x91 _x34  = _t(fun _(*1366*) pos_ -> let _x93 _x92  = _t(fun _(*1375*) pos_ -> let _x96 _x33  = _t(fun _(*1378*) pos_ -> let _x98 _x97  = _t(fun _(*1381*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x33)) in _t(fun _(*1379*) pos_ -> Yk_delay(_x98 ((_wv0)) ,_x34))) in _t(fun _(*1376*) pos_ -> _x96 (pos_) )) in _t(fun _(*1367*) pos_ -> let _x94 _x32  = _t(function
 | 1373 ->
 (fun pos_ -> Yk_when(_x32>=1))
 | _(*1374*) ->
 (fun pos_ -> _x93 (ignore((_wv0));_wv0) )) in _t(fun _(*1368*) pos_ -> let rec _x95 _x32  = _t(function
 | 1369 ->
 (fun pos_ -> _x94 (_x32) )
 | _(*1371*) ->
 (fun pos_ -> _x95 (_x32+1) )) in _x95 (0) ))) in _t(fun _(*1364*) pos_ -> _x91 (pos_) ))
 | 1387 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1391*) ->
 (fun pos_ -> let _x87 _x38  = _t(fun _(*1395*) pos_ -> let _x88 _x37  = _t(fun _(*1398*) pos_ -> let _x90 _x89  = _t(fun _(*1401*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x37)) in _t(fun _(*1399*) pos_ -> Yk_delay(_x90 ((_wv0)) ,_x38))) in _t(fun _(*1396*) pos_ -> _x88 (pos_) )) in _t(fun _(*1392*) pos_ -> _x87 (pos_) ))),_x86))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a124 = fun p v -> _p 1225 p (_p 1224 p (_p 1223 p (v)));;
let __a137 = _p 1150;;
let __a36 = _d 1340;;
let __a29 = _d 1280;;
let __a86 = _p 1100;;
let __a106 = fun p v -> _d 1368 p (_d 1367 p (_d 1366 p (_d 1364 p (_d 1363 p (v)))));;
let __a96 = _p 1261;;
let __a84 = _p 1041;;
let __a26 = _d 1172;;
let __a16 = _d 1122;;
let __a69 = _p 1262;;
let __a129 = _p 1323;;
let __a75 = fun p v -> _d_and_push 1074 p (_p 1073 p (_p 1072 p (_p 1071 p (_p 1070 p (_ddelay 1069 p (_d_and_push 1068 p (_ddelay 1067 p (_d_and_push 1066 p (_d 1064 p (_d 1063 p (v)))))))))));;
let __a91 = _p 1213;;
let __a30 = _d 1285;;
let __a78 = _p 1155;;
let __a70 = _p 1105;;
let __a89 = _p 1095;;
let __a85 = _p 1046;;
let __a17 = _d 1127;;
let __a81 = fun p v -> _p 1093 p (_d_and_push 1075 p (v));;
let __a105 = _p 1328;;
let __a4 = _d 1018;;
let __a94 = _p 1268;;
let __a113 = _p 1270;;
let __a95 = _p 1160;;
let __a71 = _p 1110;;
let __a56 = _d 1240;;
let __a40 = _d 1020;;
let __a139 = fun p v -> _p 1233 p (_p 1232 p (v));;
let __a90 = _p 1051;;
let __a107 = _p 1272;;
let __a18 = _d 1132;;
let __a108 = _p 1333;;
let __a55 = _d 1183;;
let __a140 = _p 1274;;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a132 = _p 1385;;
let __a109 = _p 1165;;
let __a72 = _p 1115;;
let __a3 = fun p v -> _p 1263 p (_x111 p (v));;
let __a61 = _d 1185;;
let __a114 = _p 1276;;
let __a64 = _p 1056;;
let __a127 = _p 1227;;
let __a103 = fun p v -> _p 1311 p (_p 1310 p (_ddelay 1309 p (_p 1308 p (_ddelay 1307 p (_d_and_push 1306 p (_d 1304 p (_d 1303 p (v))))))));;
let __a92 = _d 1297;;
let __a19 = _d 1137;;
let __a120 = _p 1278;;
let __a115 = _p 1338;;
let __p98 = _dnext 1302;;
let __a5 = _d 1028;;
let __a97 = _d 1299;;
let __p41 = _dnext 1023;;
let __a68 = fun p v -> _p 1197 p (_p 1196 p (_ddelay 1195 p (_p 1194 p (_ddelay 1193 p (_d_and_push 1192 p (_d 1190 p (_d 1189 p (v))))))));;
let __a88 = fun p v -> _d 1079 p (_d 1078 p (v));;
let __a52 = fun p v -> _d 1238 p (_d 1237 p (v));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x53 p (v)));;
let __p62 = _dnext 1188;;
let __a48 = fun p v -> _d 1215 p (_d 1214 p (v));;
let __a143 = _p 1170;;
let __a128 = fun p v -> _p 1234 p (_p 1226 p (v));;
let __a123 = _p 1120;;
let __p99 = _dwhen 1301;;
let __a58 = _p 1390;;
let __a53 = _p 1010;;
let __p42 = _dwhen 1022;;
let __a20 = _d 1142;;
let __a49 = fun p v -> _p 1403 p (_p 1402 p (_ddelay 1401 p (_p 1400 p (_ddelay 1399 p (_d_and_push 1398 p (_d 1396 p (_d 1395 p (v))))))));;
let __a135 = _p 1283;;
let __a65 = _d 1253;;
let __a43 = _d 1203;;
let __a6 = _d 1033;;
let __a79 = _p 1343;;
let __a54 = _p 1013;;
let __a126 = _p 1175;;
let __a31 = _d 1315;;
let __a59 = _p 1405;;
let __a76 = _p 1125;;
let __a134 = _p 1236;;
let __p63 = _dwhen 1187;;
let __a2 = fun p v -> _p 1026 p (_x85 p (v));;
let __a67 = _p 1406;;
let __a21 = _d 1147;;
let __a121 = _p 1288;;
let __a87 = fun p v -> _d 1296 p (_d 1295 p (_d 1294 p (_d 1292 p (_d 1291 p (v)))));;
let __a66 = _d 1258;;
let __a7 = _d 1038;;
let __a112 = _d 1369;;
let __a46 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a50 = fun p v -> _d 1182 p (_d 1181 p (_d 1180 p (_d 1178 p (_d 1177 p (v)))));;
let __a74 = fun p v -> _d 1347 p (_d 1346 p (v));;
let __a38 = fun p v -> _d 1392 p (_d 1391 p (v));;
let __a47 = _d 1210;;
let __a73 = _p 1130;;
let __a32 = _d 1320;;
let __a117 = _d 1371;;
let __a45 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a110 = fun p v -> _p 1221 p (_p 1220 p (v));;
let __a22 = _d 1152;;
let __a12 = _d 1102;;
let __a116 = _p 1243;;
let __a8 = _d 1043;;
let __a141 = _p 1245;;
let __a136 = _p 1135;;
let __a122 = _p 1025;;
let __a100 = fun p v -> _p 1092 p (_p 1091 p (_p 1090 p (_p 1089 p (_ddelay 1088 p (_p 1087 p (_ddelay 1086 p (_d_and_push 1085 p (_d 1083 p (_d 1082 p (v))))))))));;
let __a33 = _d 1325;;
let __a28 = _d 1265;;
let __a80 = _p 1076;;
let __a144 = _p 1247;;
let __a51 = _d 1217;;
let __a23 = _d 1157;;
let __a13 = _d 1107;;
let __a11 = _d 1097;;
let __a9 = _d 1048;;
let __a142 = _p 1249;;
let __a60 = fun p v -> _d 1060 p (_d 1059 p (v));;
let __p118 = _dnext 1374;;
let __a125 = fun p v -> _p 1383 p (_p 1382 p (_ddelay 1381 p (_p 1380 p (_ddelay 1379 p (_d_and_push 1378 p (_d 1376 p (_d 1375 p (v))))))));;
let __a44 = fun p v -> _d 1208 p (_d 1207 p (v));;
let __a39 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a34 = _d 1330;;
let __a93 = _p 1360;;
let __a77 = _p 1140;;
let __a101 = _p 1031;;
let __p119 = _dwhen 1373;;
let __a24 = _d 1162;;
let __a14 = _d 1112;;
let __a111 = _p 1313;;
let __a10 = _d 1053;;
let __a102 = _p 1145;;
let __a35 = _d 1335;;
let __a83 = fun p v -> _p 1358 p (_p 1357 p (_ddelay 1356 p (_p 1355 p (_ddelay 1354 p (_d_and_push 1353 p (_d 1351 p (_d 1350 p (v))))))));;
let __a138 = _p 1206;;
let __a131 = _p 1256;;
let __a130 = _p 1036;;
let __a57 = fun p v -> _d 1251 p (_d 1250 p (v));;
let __a37 = _d 1387;;
let __a25 = _d 1167;;
let __a15 = _d 1117;;
let __a104 = _p 1318;;
let __a27 = fun p v -> _d 1201 p (_d 1200 p (v));;
let __a82 = _p 1199;;
let __a133 = fun p v -> _p 1230 p (_p 1229 p (v));;
let __binder0 = __default_ret;;
let __binder1 = _m 1002;;
let __binder2 = _m 1009;;
let __binder3 = _m 1222;;
let __binder4 = _m 1231;;
let binders : (sv -> sv -> sv) array = [| |]
let num_symbols = 11

let symbol_table = function
  | 267 -> "cmd-line-args"
  | 272 -> "command"
  | 268 -> "o"
  | 264 -> "CHAR"
  | 271 -> "transformation"
  | 270 -> "file"
  | 265 -> "DIGIT"
  | 273 -> "args"
  | 274 -> "eof"
  | 269 -> "arg"
  | 266 -> "OCTET"
  | x -> if x < 256 then Yak.Pam_internal.default_symbol_table x else "?unknown?"

let get_symb_action = function
  | "cmd-line-args" -> 267
  | "command" -> 272
  | "o" -> 268
  | "CHAR" -> 264
  | "transformation" -> 271
  | "file" -> 270
  | "DIGIT" -> 265
  | "args" -> 273
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

and nullable_transformation __lookahead _p0_ _x0_ = None

and nullable_DIGIT __lookahead _p0_ _x0_ = None

and nullable_args __lookahead _p0_ _x0_ = None

and nullable_arg __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1022 and n = _dnext 1023 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (fun _x1_ _x2_ _x3_ -> ((((Pred.andc (Pred.full_lookaheadc false 264 1) (fun _x4_ _x5_ _x6_ -> (Some _x6_))) _x1_) _x2_) _x3_))) __lookahead) _p0_) (((_d 1018) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1017 p (_d 1016 p (_x53 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_eof __lookahead _p0_ _x0_ = ((((Pred.full_lookaheadc false 266 3) __lookahead) _p0_) _x0_)

and nullable_OCTET __lookahead _p0_ _x0_ = None

let program : (int * sv instruction list) list = [
(383, [EatInstr(112,414)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [AAction2Instr(__a87,415)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,416)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(116,417)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,418)]);
(4, [AAction2Instr(__a0,15)]);
(388, [EatInstr(115,419)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(97,420)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,17)]);
(390, [EatInstr(99,421)]);
(7, [EatInstr(127,19);EatInstr(126,19);EatInstr(125,19);EatInstr(124,19);EatInstr(123,19);EatInstr(122,19);EatInstr(121,19);EatInstr(120,19);EatInstr(119,19);EatInstr(118,19);EatInstr(117,19);EatInstr(116,19);EatInstr(115,19);EatInstr(114,19);EatInstr(113,19);EatInstr(112,19);EatInstr(111,19);EatInstr(110,19);EatInstr(109,19);EatInstr(108,19);EatInstr(107,19);EatInstr(106,19);EatInstr(105,19);EatInstr(104,19);EatInstr(103,19);EatInstr(102,19);EatInstr(101,19);EatInstr(100,19);EatInstr(99,19);EatInstr(98,19);EatInstr(97,19);EatInstr(96,19);EatInstr(95,19);EatInstr(94,19);EatInstr(93,19);EatInstr(92,19);EatInstr(91,19);EatInstr(90,19);EatInstr(89,19);EatInstr(88,19);EatInstr(87,19);EatInstr(86,19);EatInstr(85,19);EatInstr(84,19);EatInstr(83,19);EatInstr(82,19);EatInstr(81,19);EatInstr(80,19);EatInstr(79,19);EatInstr(78,19);EatInstr(77,19);EatInstr(76,19);EatInstr(75,19);EatInstr(74,19);EatInstr(73,19);EatInstr(72,19);EatInstr(71,19);EatInstr(70,19);EatInstr(69,19);EatInstr(68,19);EatInstr(67,19);EatInstr(66,19);EatInstr(65,19);EatInstr(64,19);EatInstr(63,19);EatInstr(62,19);EatInstr(61,19);EatInstr(60,19);EatInstr(59,19);EatInstr(58,19);EatInstr(57,19);EatInstr(56,19);EatInstr(55,19);EatInstr(54,19);EatInstr(53,19);EatInstr(52,19);EatInstr(51,19);EatInstr(50,19);EatInstr(49,19);EatInstr(48,19);EatInstr(47,19);EatInstr(46,19);EatInstr(44,19);EatInstr(43,19);EatInstr(42,19);EatInstr(41,19);EatInstr(40,19);EatInstr(39,19);EatInstr(38,19);EatInstr(37,19);EatInstr(36,19);EatInstr(35,19);EatInstr(34,19);EatInstr(33,19);EatInstr(32,19);EatInstr(31,19);EatInstr(30,19);EatInstr(29,19);EatInstr(28,19);EatInstr(27,19);EatInstr(26,19);EatInstr(25,19);EatInstr(24,19);EatInstr(23,19);EatInstr(22,19);EatInstr(21,19);EatInstr(20,19);EatInstr(19,19);EatInstr(18,19);EatInstr(17,19);EatInstr(16,19);EatInstr(15,19);EatInstr(14,19);EatInstr(13,19);EatInstr(12,19);EatInstr(11,19);EatInstr(10,19);EatInstr(9,19);EatInstr(8,19);EatInstr(7,19);EatInstr(6,19);EatInstr(5,19);EatInstr(4,19);EatInstr(3,19);EatInstr(2,19);EatInstr(1,19)]);
(391, [EatInstr(114,422)]);
(8, [EatInstr(97,20)]);
(392, [EatInstr(97,423)]);
(9, [AAction2Instr(__a2,21)]);
(393, [EatInstr(105,424)]);
(10, [AAction2Instr(__a3,22)]);
(394, [EatInstr(101,425)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),23);RCompleteInstr2(274,nullable_eof)]);
(395, [EatInstr(107,426)]);
(12, [CompleteInstr(264)]);
(396, [AAction2Instr(__a88,427)]);
(13, [CompleteInstr(265)]);
(397, [AAction2Instr(__a89,252)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(115,428)]);
(15, [ACallInstr3(__default_call,9);ASimpleCont2Instr(272,__binder1,24)]);
(399, [EatInstr(101,429)]);
(16, [CompleteInstr(268)]);
(400, [AAction2Instr(__a90,252)]);
(17, [AAction2Instr(__a4,26);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,25)]);
(401, [EatInstr(103,430)]);
(402, [EatInstr(45,431)]);
(19, [ALookaheadInstr(false,CfgLA (1,264),27);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,19)]);
(403, [EatInstr(101,432)]);
(20, [EatInstr(100,28)]);
(404, [EatInstr(101,433)]);
(21, [EatInstr(114,30);EatInstr(101,29);AAction2Instr(__a27,53);AAction2Instr(__a26,52);AAction2Instr(__a25,51);AAction2Instr(__a24,50);AAction2Instr(__a23,49);AAction2Instr(__a22,48);AAction2Instr(__a21,47);AAction2Instr(__a20,46);AAction2Instr(__a19,45);AAction2Instr(__a18,44);AAction2Instr(__a17,43);AAction2Instr(__a16,42);AAction2Instr(__a15,41);AAction2Instr(__a14,40);AAction2Instr(__a13,39);AAction2Instr(__a12,38);AAction2Instr(__a11,37);AAction2Instr(__a10,36);AAction2Instr(__a9,35);AAction2Instr(__a8,34);AAction2Instr(__a7,33);AAction2Instr(__a6,32);AAction2Instr(__a5,31)]);
(405, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,434)]);
(22, [EatInstr(45,54);AAction2Instr(__a38,65);AAction2Instr(__a37,64);AAction2Instr(__a36,63);AAction2Instr(__a35,62);AAction2Instr(__a34,61);AAction2Instr(__a33,60);AAction2Instr(__a32,59);AAction2Instr(__a31,58);AAction2Instr(__a30,57);AAction2Instr(__a29,56);AAction2Instr(__a28,55)]);
(406, [EatInstr(101,435)]);
(23, [CompleteInstr(274)]);
(407, [EatInstr(108,436)]);
(24, [AAction2Instr(__a39,66)]);
(408, [EatInstr(101,437)]);
(25, [AAction2Instr(__a40,17)]);
(409, [EatInstr(116,438)]);
(26, [AWhenInstr3(__p42,__p41,67)]);
(410, [AAction2Instr(__a91,252)]);
(27, [CompleteInstr(270)]);
(411, [EatInstr(111,439)]);
(28, [EatInstr(100,68)]);
(412, [EatInstr(108,440)]);
(29, [EatInstr(120,69)]);
(413, [EatInstr(108,441)]);
(30, [EatInstr(102,70)]);
(414, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,442)]);
(31, [EatInstr(97,71)]);
(415, [AAction2Instr(__a92,444);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,443)]);
(32, [EatInstr(99,72)]);
(416, [AAction2Instr(__a93,243)]);
(33, [EatInstr(99,73)]);
(417, [EatInstr(97,445)]);
(34, [EatInstr(100,74)]);
(418, [AAction2Instr(__a94,446)]);
(35, [EatInstr(100,75)]);
(419, [EatInstr(101,447)]);
(36, [EatInstr(100,76)]);
(420, [EatInstr(98,448)]);
(37, [EatInstr(101,77)]);
(421, [EatInstr(115,449)]);
(38, [EatInstr(102,78)]);
(422, [EatInstr(101,450)]);
(39, [EatInstr(104,79)]);
(423, [EatInstr(100,451)]);
(40, [EatInstr(105,80)]);
(424, [EatInstr(110,452)]);
(41, [EatInstr(105,81)]);
(425, [EatInstr(115,453)]);
(42, [EatInstr(108,82)]);
(426, [EatInstr(97,454)]);
(43, [EatInstr(108,83)]);
(427, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,455)]);
(44, [EatInstr(108,84)]);
(428, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,456)]);
(45, [EatInstr(109,85)]);
(429, [EatInstr(114,457)]);
(46, [EatInstr(112,86)]);
(430, [EatInstr(117,458)]);
(47, [EatInstr(112,87)]);
(431, [EatInstr(97,459)]);
(48, [EatInstr(112,88)]);
(432, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,460)]);
(49, [EatInstr(112,89)]);
(433, [EatInstr(45,461)]);
(50, [EatInstr(112,90)]);
(434, [AAction2Instr(__a95,252)]);
(51, [EatInstr(112,91)]);
(435, [EatInstr(100,462)]);
(52, [EatInstr(112,92)]);
(436, [EatInstr(97,463)]);
(53, [AAction2Instr(__a44,94);AAction2Instr(__a43,93)]);
(437, [EatInstr(118,464)]);
(54, [EatInstr(117,97);EatInstr(114,96);EatInstr(99,95)]);
(438, [EatInstr(101,465)]);
(55, [EatInstr(45,98)]);
(439, [EatInstr(114,466)]);
(56, [EatInstr(45,99)]);
(440, [EatInstr(97,467)]);
(57, [EatInstr(45,100)]);
(441, [EatInstr(108,468)]);
(58, [EatInstr(45,101)]);
(442, [AAction2Instr(__a96,252)]);
(59, [EatInstr(45,102)]);
(443, [AAction2Instr(__a97,415)]);
(60, [EatInstr(45,103)]);
(444, [AWhenInstr3(__p99,__p98,469)]);
(61, [EatInstr(45,104)]);
(445, [EatInstr(114,470)]);
(62, [EatInstr(45,105)]);
(446, [EatInstr(116,473);EatInstr(112,472);EatInstr(102,471)]);
(63, [EatInstr(45,106)]);
(447, [EatInstr(110,474)]);
(64, [EatInstr(45,107)]);
(448, [EatInstr(101,475)]);
(65, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,108)]);
(449, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,476)]);
(66, [AAction2Instr(__a46,110);AAction2Instr(__a45,109)]);
(67, [ALookaheadInstr(false,CfgLA (1,264),111)]);
(450, [EatInstr(103,477)]);
(451, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,478)]);
(68, [EatInstr(45,112)]);
(452, [EatInstr(101,479)]);
(69, [EatInstr(101,113)]);
(453, [EatInstr(99,480)]);
(70, [EatInstr(99,114)]);
(454, [EatInstr(104,481)]);
(71, [EatInstr(116,115)]);
(455, [AAction2Instr(__a100,328)]);
(72, [EatInstr(108,116)]);
(456, [AAction2Instr(__a101,252)]);
(73, [EatInstr(111,117)]);
(457, [EatInstr(45,482)]);
(74, [EatInstr(101,118)]);
(458, [EatInstr(108,483)]);
(75, [EatInstr(105,119)]);
(459, [EatInstr(110,484)]);
(76, [EatInstr(111,120)]);
(460, [AAction2Instr(__a102,252)]);
(77, [EatInstr(120,121)]);
(461, [EatInstr(97,485)]);
(78, [EatInstr(117,122)]);
(462, [EatInstr(115,486)]);
(79, [EatInstr(97,123)]);
(463, [EatInstr(98,487)]);
(80, [EatInstr(110,124)]);
(464, [EatInstr(97,488)]);
(81, [EatInstr(110,125)]);
(465, [EatInstr(45,489)]);
(82, [EatInstr(101,126)]);
(466, [EatInstr(109,490)]);
(83, [EatInstr(105,127)]);
(467, [EatInstr(116,491)]);
(84, [EatInstr(111,128)]);
(468, [EatInstr(45,492)]);
(85, [EatInstr(105,129)]);
(469, [AAction2Instr(__a103,493)]);
(86, [EatInstr(114,130)]);
(470, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,494)]);
(87, [EatInstr(114,131)]);
(471, [EatInstr(117,495)]);
(88, [EatInstr(114,132)]);
(472, [EatInstr(101,496)]);
(89, [EatInstr(114,133)]);
(473, [EatInstr(120,497)]);
(90, [EatInstr(114,134)]);
(474, [EatInstr(115,498)]);
(91, [EatInstr(114,135)]);
(475, [EatInstr(108,499)]);
(92, [EatInstr(114,136)]);
(476, [AAction2Instr(__a104,243)]);
(93, [EatInstr(115,137)]);
(477, [EatInstr(117,500)]);
(94, [AAction2Instr(__a48,139);AAction2Instr(__a47,138)]);
(478, [AAction2Instr(__a105,243)]);
(95, [EatInstr(111,140)]);
(479, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,501)]);
(96, [EatInstr(111,141)]);
(480, [EatInstr(101,502)]);
(97, [EatInstr(110,142)]);
(481, [EatInstr(101,503)]);
(98, [EatInstr(98,143)]);
(482, [EatInstr(99,504)]);
(99, [EatInstr(99,144)]);
(483, [EatInstr(97,505)]);
(100, [EatInstr(99,145)]);
(484, [EatInstr(97,506)]);
(101, [EatInstr(105,146)]);
(485, [EatInstr(110,507)]);
(102, [EatInstr(105,147)]);
(486, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(103, [EatInstr(108,148)]);
(487, [EatInstr(108,509)]);
(104, [EatInstr(110,149)]);
(488, [EatInstr(110,510)]);
(105, [EatInstr(110,150)]);
(489, [EatInstr(97,511)]);
(106, [EatInstr(111,151)]);
(490, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,512)]);
(107, [EatInstr(118,152)]);
(491, [EatInstr(101,513)]);
(108, [AAction2Instr(__a49,153)]);
(492, [EatInstr(115,514)]);
(109, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder2,154)]);
(493, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,515)]);
(110, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,155)]);
(494, [AAction2Instr(__a106,516)]);
(111, [CompleteInstr(269)]);
(495, [EatInstr(110,517)]);
(112, [EatInstr(108,156)]);
(496, [EatInstr(103,518)]);
(113, [EatInstr(99,157)]);
(497, [AAction2Instr(__a107,519)]);
(114, [AAction2Instr(__a50,158)]);
(498, [EatInstr(105,520)]);
(115, [EatInstr(116,159)]);
(499, [EatInstr(115,521)]);
(116, [EatInstr(111,160)]);
(500, [EatInstr(108,522)]);
(117, [EatInstr(109,161)]);
(501, [AAction2Instr(__a108,243)]);
(118, [EatInstr(115,162)]);
(502, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,523)]);
(119, [EatInstr(115,163)]);
(503, [EatInstr(97,524)]);
(120, [EatInstr(116,164)]);
(504, [EatInstr(111,525)]);
(121, [EatInstr(116,165)]);
(505, [EatInstr(114,526)]);
(122, [EatInstr(115,166)]);
(506, [EatInstr(108,527)]);
(123, [EatInstr(115,167)]);
(507, [EatInstr(97,528)]);
(124, [EatInstr(102,168)]);
(508, [AAction2Instr(__a109,252)]);
(125, [EatInstr(108,169)]);
(509, [EatInstr(101,529)]);
(126, [EatInstr(120,170)]);
(510, [EatInstr(99,530)]);
(127, [EatInstr(102,171)]);
(511, [EatInstr(99,531)]);
(128, [EatInstr(111,172)]);
(512, [AAction2Instr(__a110,532)]);
(129, [EatInstr(110,173)]);
(513, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,533)]);
(130, [EatInstr(101,174)]);
(514, [EatInstr(116,534)]);
(131, [EatInstr(101,175)]);
(515, [AAction2Instr(__a111,243)]);
(132, [EatInstr(105,176)]);
(516, [AAction2Instr(__a112,536);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,535)]);
(133, [EatInstr(105,177)]);
(517, [AAction2Instr(__a113,519)]);
(134, [EatInstr(105,178)]);
(518, [EatInstr(45,537);AAction2Instr(__a114,519)]);
(135, [EatInstr(105,179)]);
(519, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(136, [EatInstr(105,180)]);
(520, [EatInstr(116,539)]);
(137, [EatInstr(116,181)]);
(521, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,540)]);
(138, [EatInstr(115,182)]);
(522, [EatInstr(97,541)]);
(139, [AAction2Instr(__a52,184);AAction2Instr(__a51,183)]);
(523, [AAction2Instr(__a115,243)]);
(140, [EatInstr(117,185)]);
(524, [EatInstr(100,542)]);
(141, [EatInstr(111,186)]);
(525, [EatInstr(114,543)]);
(142, [EatInstr(114,187)]);
(526, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,544)]);
(143, [EatInstr(97,188)]);
(527, [EatInstr(121,545)]);
(144, [EatInstr(97,189)]);
(528, [EatInstr(108,546)]);
(145, [EatInstr(104,190)]);
(529, [EatInstr(45,547)]);
(146, [EatInstr(110,191)]);
(530, [EatInstr(101,548)]);
(147, [EatInstr(110,192)]);
(531, [EatInstr(116,549)]);
(148, [EatInstr(111,193)]);
(532, [ASimpleCont2Instr(271,__binder3,550);ACallInstr3(__default_call,8)]);
(149, [EatInstr(101,194)]);
(533, [AAction2Instr(__a116,551)]);
(150, [EatInstr(111,195)]);
(534, [EatInstr(97,552)]);
(151, [EatInstr(110,196)]);
(535, [AAction2Instr(__a117,516)]);
(152, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,197)]);
(536, [AWhenInstr3(__p119,__p118,553)]);
(153, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,198)]);
(537, [EatInstr(115,554)]);
(154, [AAction2Instr(__a53,66)]);
(538, [AAction2Instr(__a120,243)]);
(155, [AAction2Instr(__a54,199)]);
(539, [EatInstr(105,555)]);
(156, [EatInstr(114,200)]);
(540, [AAction2Instr(__a121,243)]);
(157, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,201)]);
(541, [EatInstr(114,556)]);
(158, [AAction2Instr(__a55,203);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,202)]);
(542, [AAction2Instr(__a122,557)]);
(159, [EatInstr(114,204)]);
(543, [EatInstr(101,558)]);
(160, [EatInstr(115,205)]);
(544, [AAction2Instr(__a123,252)]);
(161, [EatInstr(112,206)]);
(545, [EatInstr(115,559)]);
(162, [EatInstr(117,207)]);
(546, [EatInstr(121,560)]);
(163, [EatInstr(112,208)]);
(547, [EatInstr(112,561)]);
(164, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,209)]);
(548, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,562)]);
(165, [EatInstr(114,210)]);
(549, [EatInstr(105,563)]);
(166, [EatInstr(101,211)]);
(550, [AAction2Instr(__a124,564)]);
(167, [EatInstr(104,212)]);
(551, [EatInstr(100,565)]);
(168, [EatInstr(111,213)]);
(552, [EatInstr(114,566)]);
(169, [EatInstr(105,214)]);
(553, [AAction2Instr(__a125,567)]);
(170, [EatInstr(101,215)]);
(554, [EatInstr(116,568)]);
(171, [EatInstr(116,216)]);
(555, [EatInstr(118,569)]);
(172, [EatInstr(107,217)]);
(556, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,570)]);
(173, [EatInstr(117,218)]);
(557, [CompleteInstr(271)]);
(174, [EatInstr(99,219)]);
(558, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,571)]);
(175, [EatInstr(99,220)]);
(559, [EatInstr(105,572)]);
(176, [EatInstr(110,221)]);
(560, [EatInstr(115,573)]);
(177, [EatInstr(110,222)]);
(561, [EatInstr(114,574)]);
(178, [EatInstr(110,223)]);
(562, [AAction2Instr(__a126,252)]);
(179, [EatInstr(110,224)]);
(563, [EatInstr(111,575)]);
(180, [EatInstr(110,225)]);
(564, [AAction2Instr(__a128,577);AAction2Instr(__a127,576)]);
(181, [EatInstr(114,226)]);
(565, [EatInstr(121,578)]);
(182, [EatInstr(117,227)]);
(566, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,579)]);
(183, [EatInstr(116,228)]);
(567, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,580)]);
(184, [AAction2Instr(__a57,230);AAction2Instr(__a56,229)]);
(568, [EatInstr(114,581)]);
(185, [EatInstr(110,231)]);
(569, [EatInstr(101,582)]);
(186, [EatInstr(116,232)]);
(570, [AAction2Instr(__a129,243)]);
(187, [EatInstr(111,233)]);
(571, [AAction2Instr(__a130,252)]);
(188, [EatInstr(99,234)]);
(572, [EatInstr(115,583)]);
(189, [EatInstr(115,235)]);
(573, [EatInstr(105,584)]);
(190, [EatInstr(101,236)]);
(574, [EatInstr(101,585)]);
(191, [EatInstr(108,237)]);
(575, [EatInstr(110,586)]);
(192, [EatInstr(108,238)]);
(576, [EatInstr(44,587)]);
(193, [EatInstr(111,239)]);
(577, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,588)]);
(194, [EatInstr(119,240)]);
(578, [EatInstr(112,589)]);
(195, [EatInstr(45,241)]);
(579, [AAction2Instr(__a131,252)]);
(196, [EatInstr(108,242)]);
(580, [AAction2Instr(__a132,243)]);
(197, [AAction2Instr(__a58,243)]);
(581, [EatInstr(105,590)]);
(198, [AAction2Instr(__a59,243)]);
(582, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,591)]);
(199, [CompleteInstr(267)]);
(583, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,592)]);
(200, [EatInstr(49,244)]);
(584, [EatInstr(115,593)]);
(201, [AAction2Instr(__a60,245)]);
(585, [EatInstr(100,594)]);
(202, [AAction2Instr(__a61,158)]);
(586, [EatInstr(115,595)]);
(203, [AWhenInstr3(__p63,__p62,246)]);
(587, [AAction2Instr(__a133,596)]);
(204, [EatInstr(105,247)]);
(588, [AAction2Instr(__a134,252)]);
(205, [EatInstr(101,248)]);
(589, [EatInstr(103,597)]);
(206, [EatInstr(105,249)]);
(590, [EatInstr(99,598)]);
(207, [EatInstr(103,250)]);
(591, [AAction2Instr(__a135,243)]);
(208, [EatInstr(97,251)]);
(592, [AAction2Instr(__a136,252)]);
(209, [AAction2Instr(__a64,252)]);
(593, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,599)]);
(210, [EatInstr(97,253)]);
(594, [EatInstr(105,600)]);
(211, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,254)]);
(595, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,601)]);
(212, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,255)]);
(596, [ASimpleCont2Instr(271,__binder4,602);ACallInstr3(__default_call,8)]);
(213, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,256)]);
(597, [EatInstr(101,603)]);
(214, [EatInstr(110,257)]);
(598, [EatInstr(116,604)]);
(215, [EatInstr(114,258)]);
(599, [AAction2Instr(__a137,252)]);
(216, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,259)]);
(600, [EatInstr(99,605)]);
(217, [EatInstr(97,260)]);
(601, [AAction2Instr(__a138,252)]);
(218, [EatInstr(115,261)]);
(602, [AAction2Instr(__a139,564)]);
(219, [EatInstr(101,262)]);
(603, [EatInstr(110,606)]);
(220, [EatInstr(101,263)]);
(604, [AAction2Instr(__a140,519)]);
(221, [EatInstr(116,264)]);
(605, [EatInstr(97,607)]);
(222, [EatInstr(116,265)]);
(606, [EatInstr(45,608);AAction2Instr(__a141,609)]);
(223, [EatInstr(116,266)]);
(607, [EatInstr(116,610)]);
(224, [EatInstr(116,267)]);
(608, [EatInstr(115,611)]);
(225, [EatInstr(116,268)]);
(609, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,612)]);
(226, [EatInstr(105,269)]);
(610, [EatInstr(101,613)]);
(227, [EatInstr(98,270)]);
(611, [EatInstr(99,614)]);
(228, [EatInstr(114,271)]);
(612, [AAction2Instr(__a142,252)]);
(229, [EatInstr(116,272)]);
(613, [EatInstr(115,615)]);
(230, [AAction2Instr(__a66,274);AAction2Instr(__a65,273)]);
(614, [EatInstr(97,616)]);
(231, [EatInstr(116,275)]);
(615, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,617)]);
(232, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,276)]);
(616, [EatInstr(110,618)]);
(233, [EatInstr(108,277)]);
(617, [AAction2Instr(__a143,252)]);
(234, [EatInstr(107,278)]);
(618, [EatInstr(110,619)]);
(235, [EatInstr(101,279)]);
(619, [EatInstr(101,620)]);
(236, [EatInstr(99,280)]);
(620, [EatInstr(114,621)]);
(237, [EatInstr(105,281)]);
(621, [EatInstr(108,622)]);
(238, [EatInstr(105,282)]);
(622, [EatInstr(101,623)]);
(239, [EatInstr(107,283)]);
(623, [EatInstr(115,624)]);
(240, [EatInstr(45,284)]);
(624, [EatInstr(115,625)]);
(241, [EatInstr(99,285)]);
(625, [AAction2Instr(__a144,609)]);
(242, [EatInstr(121,286)]);
(243, [AAction2Instr(__a67,287)]);
(244, [EatInstr(45,288)]);
(245, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,289)]);
(246, [AAction2Instr(__a68,290)]);
(247, [EatInstr(98,291)]);
(248, [EatInstr(45,292)]);
(249, [EatInstr(108,293)]);
(250, [EatInstr(97,294)]);
(251, [EatInstr(116,295)]);
(252, [AAction2Instr(__a69,296)]);
(253, [EatInstr(99,297)]);
(254, [AAction2Instr(__a70,252)]);
(255, [AAction2Instr(__a71,252)]);
(256, [AAction2Instr(__a72,252)]);
(257, [EatInstr(101,298)]);
(258, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,299)]);
(259, [AAction2Instr(__a73,252)]);
(260, [EatInstr(104,300)]);
(261, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,301)]);
(262, [EatInstr(100,302)]);
(263, [EatInstr(100,303)]);
(264, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,304)]);
(265, [EatInstr(45,305)]);
(266, [EatInstr(45,306)]);
(267, [EatInstr(45,307)]);
(268, [EatInstr(45,308)]);
(269, [EatInstr(112,309)]);
(270, [EatInstr(115,310)]);
(271, [EatInstr(97,311)]);
(272, [EatInstr(114,312)]);
(273, [EatInstr(117,313)]);
(274, [EatInstr(119,314)]);
(275, [EatInstr(101,315)]);
(276, [AAction2Instr(__a74,316)]);
(277, [EatInstr(108,317)]);
(278, [EatInstr(101,318)]);
(279, [EatInstr(45,319)]);
(280, [EatInstr(107,320)]);
(281, [EatInstr(110,321)]);
(282, [EatInstr(110,322)]);
(283, [EatInstr(97,323)]);
(284, [EatInstr(101,324)]);
(285, [EatInstr(111,325)]);
(286, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,326)]);
(287, [CompleteInstr(273)]);
(288, [EatInstr(108,327)]);
(289, [AAction2Instr(__a75,328)]);
(290, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,329)]);
(291, [EatInstr(117,330)]);
(292, [EatInstr(117,331)]);
(293, [EatInstr(101,332)]);
(294, [EatInstr(114,333)]);
(295, [EatInstr(99,334)]);
(296, [CompleteInstr(272)]);
(297, [EatInstr(116,335)]);
(298, [EatInstr(45,336)]);
(299, [AAction2Instr(__a76,252)]);
(300, [EatInstr(101,337)]);
(301, [AAction2Instr(__a77,252)]);
(302, [EatInstr(101,338)]);
(303, [EatInstr(101,339)]);
(304, [AAction2Instr(__a78,252)]);
(305, [EatInstr(103,340)]);
(306, [EatInstr(110,341)]);
(307, [EatInstr(110,342)]);
(308, [EatInstr(114,343)]);
(309, [EatInstr(45,344)]);
(310, [EatInstr(101,345)]);
(311, [EatInstr(110,346)]);
(312, [EatInstr(97,347)]);
(313, [EatInstr(110,348)]);
(314, [EatInstr(114,349)]);
(315, [EatInstr(114,350)]);
(316, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,351)]);
(317, [EatInstr(45,352)]);
(318, [EatInstr(110,353)]);
(319, [EatInstr(105,354)]);
(320, [EatInstr(45,355)]);
(321, [EatInstr(101,356)]);
(322, [EatInstr(101,357)]);
(323, [EatInstr(104,358)]);
(324, [EatInstr(110,359)]);
(325, [EatInstr(97,360)]);
(326, [AAction2Instr(__a79,243)]);
(327, [EatInstr(111,361)]);
(328, [AAction2Instr(__a81,363);AAction2Instr(__a80,362)]);
(329, [AAction2Instr(__a82,252)]);
(330, [EatInstr(116,364)]);
(331, [EatInstr(110,365)]);
(332, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,366)]);
(333, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,367)]);
(334, [EatInstr(104,368)]);
(335, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,369)]);
(336, [EatInstr(114,370)]);
(337, [EatInstr(97,371)]);
(338, [EatInstr(110,372)]);
(339, [EatInstr(110,373)]);
(340, [EatInstr(105,374)]);
(341, [EatInstr(112,375)]);
(342, [EatInstr(117,376)]);
(343, [EatInstr(101,377)]);
(344, [EatInstr(108,378)]);
(345, [EatInstr(116,379)]);
(346, [EatInstr(115,380)]);
(347, [EatInstr(110,381)]);
(348, [EatInstr(114,382)]);
(349, [EatInstr(97,383)]);
(350, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,384)]);
(351, [AAction2Instr(__a83,385)]);
(352, [EatInstr(115,386)]);
(353, [EatInstr(100,387)]);
(354, [EatInstr(110,388)]);
(355, [EatInstr(108,389)]);
(356, [EatInstr(45,390)]);
(357, [EatInstr(45,391)]);
(358, [EatInstr(101,392)]);
(359, [EatInstr(103,393)]);
(360, [EatInstr(108,394)]);
(361, [EatInstr(111,395)]);
(362, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,396)]);
(363, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,397)]);
(364, [EatInstr(101,398)]);
(365, [EatInstr(100,399)]);
(366, [AAction2Instr(__a84,252)]);
(367, [AAction2Instr(__a85,252)]);
(368, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,400)]);
(369, [AAction2Instr(__a86,252)]);
(370, [EatInstr(101,401)]);
(371, [EatInstr(100,402)]);
(372, [EatInstr(99,403)]);
(373, [EatInstr(99,404)]);
(374, [EatInstr(108,405)]);
(375, [EatInstr(114,406)]);
(376, [EatInstr(108,407)]);
(377, [EatInstr(108,408)]);
(378, [EatInstr(97,409)]);
(379, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,410)]);
(380, [EatInstr(102,411)]);
(381, [EatInstr(115,412)]);
(382, [EatInstr(111,413)]);
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
