
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
| Exec_cmd of string * string list
| Compile_cmd
| Translate_cmd of plugin

| Close_under_core_cmd
| Dispatch_cmd
| Wrap_cmd
| Attributes_cmd
| Lift_cmd
| Desugar_cmd
| Fuse_cmd
| Tx_prec_cmd

| Compile_only_cmd
| Dispatch_only_cmd
| Wrap_only_cmd
| Attributes_only_cmd
| Lift_only_cmd
| Desugar_only_cmd

| Print_gil_cmd
| Print_gul_cmd
| Print_npreds_cmd
| Extract_cmd
| Rfc_cmd of int
| Dot_cmd
| Strip_late_actions_cmd

| Info_cmd (* display internal information about the compiler. *)

| Transform_cmd of transformation list

| Analyze_cmd of analyses (** Perform a grammar analysis. *)

let cmd = ref Print_gul_cmd
let files = ref []
let roots = ref []

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
 | (1035) -> ( Dot_cmd )
 | (1040) -> ( Lift_only_cmd )
 | (1045) -> ( Attributes_only_cmd )
 | (1050) -> ( Wrap_only_cmd )
 | (1055) -> ( Dispatch_only_cmd )
 | (1060) -> ( Compile_only_cmd )
 | (1065) -> ( Tx_prec_cmd )
 | (1070) -> ( Desugar_cmd )
 | (1075) -> ( Lift_cmd )
 | (1080) -> ( Attributes_cmd )
 | (1085) -> ( Wrap_cmd )
 | (1090) -> ( Dispatch_cmd )
 | (1095) -> ( Compile_cmd )
 | (1100) -> (
 (let t = 
 (match _n() with
 | (1102) -> (Dypgen_PI false)
 | _(*1104*) -> (Dypgen_PI true)
 ) in (ignore (*1106*) (_n());  Translate_cmd t )
 ))
 | (1111) -> ( Print_gil_cmd )
 | (1116) -> ( Print_gul_cmd )
 | (1121) -> ( Strip_late_actions_cmd )
 | (1126) -> ( Extract_cmd )
 | (1143) -> (
 (let _x10 = (ignore (*1144*) (_n()); _n())
  in (ignore (*1145*) (_n()); 
 (let _x9 = (ignore (*1146*) (_n()); _n())
  in (ignore (*1147*) (_n()); 
 (let n = (ignore (*1148*) (_n()); Yak.Yakker.get_string _x10 _x9 ykinput)
  in (ignore (*1150*) (_n());  try Rfc_cmd(int_of_string n) with _ -> raise(Failure "Invalid RFC number") )
 ))
 ))
 ))
 | (1155) -> ( Print_npreds_cmd )
 | (1165) -> (
 (let _x14 = (ignore (*1166*) (_n()); _n())
  in (ignore (*1167*) (_n()); 
 (let _x13 = (ignore (*1168*) (_n()); _n())
  in (ignore (*1169*) (_n()); 
 (let f = (ignore (*1170*) (_n()); Yak.Yakker.get_string _x14 _x13 ykinput)
  in (ignore (*1171*) (_n()); 
 (let l = (ignore (*1172*) (_n()); 
 (let _x20 = (ignore (*1173*) (_n()); 
 (let rec _x49 _x20 = (match _n() with 1174 -> _x20 | _x48 -> _x49((ignore (*1175*) (_x48); 
 (let _x19 = (ignore (*1184*) (_n()); 
 (let _x18 = (ignore (*1185*) (_n()); _n())
  in (ignore (*1186*) (_n()); 
 (let _x17 = (ignore (*1187*) (_n()); _n())
  in (ignore (*1188*) (_n()); 
 (let x = (ignore (*1189*) (_n()); Yak.Yakker.get_string _x18 _x17 ykinput)
  in (ignore (*1190*) (_n()); x)
 ))
 ))
 ))
  in (ignore (*1191*) (_n()); _x19::_x20)
 ))
 )) in _x49([])))
  in (ignore (*1192*) (_n()); (List.rev _x20))
 ))
  in (ignore (*1194*) (_n());  Exec_cmd(f,l) )
 ))
 ))
 ))
 ))
 | (1201) -> ( Info_cmd )
 | (1208) -> (
 (let tx = (ignore (*1209*) (_n()); _r_transformation(_n,ykinput))
  in (ignore (*1211*) (_n()); 
 (let txs = (ignore (*1212*) (_n()); 
 (let _x22 = (ignore (*1213*) (_n()); 
 (let rec _x47 _x22 = (match _n() with 1214 -> _x22 | _x46 -> _x47((ignore (*1215*) (_x46); 
 (let _x21 = (ignore (*1217*) (_n()); 
 (let x = (ignore (*1218*) (_n()); _r_transformation(_n,ykinput))
  in (ignore (*1220*) (_n()); x)
 ))
  in (ignore (*1221*) (_n()); _x21::_x22)
 ))
 )) in _x47([])))
  in (ignore (*1222*) (_n()); (List.rev _x22))
 ))
  in (ignore (*1224*) (_n());  Transform_cmd (tx::txs) )
 ))
 ))
 | _(*1229*) -> (
 (let a = (ignore (*1230*) (_n()); _r_analysis(_n,ykinput))
  in (ignore (*1233*) (_n());  Analyze_cmd a )
 ))
 ) in (ignore (*1234*) (_n()); _x41)
 ))
 
 and
_r_args(_n,ykinput) = (ignore (*1235*) (_n()); 
 (let _x42 = 
 (match _n() with
 | (1240) -> ( Compileopt.case_sensitive := false )
 | (1245) -> (
 (let b = 
 (match _n() with
 | (1247) -> (Fun_BE)
 | (1249) -> (Trans_BE)
 | (1251) -> (Peg_BE false)
 | _(*1253*) -> (Peg_BE true)
 ) in (ignore (*1255*) (_n());  backend := b )
 ))
 | (1260) -> ( Compileopt.inline_cs := true )
 | (1265) -> ( Compileopt.inline_regular := true )
 | (1283) -> (
 (let _x27 = (ignore (*1284*) (_n()); _n())
  in (ignore (*1285*) (_n()); 
 (let _x26 = (ignore (*1286*) (_n()); _n())
  in (ignore (*1287*) (_n()); 
 (let n = (ignore (*1288*) (_n()); Yak.Yakker.get_string _x27 _x26 ykinput)
  in (ignore (*1290*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1308) -> (
 (let _x32 = (ignore (*1309*) (_n()); _n())
  in (ignore (*1310*) (_n()); 
 (let _x31 = (ignore (*1311*) (_n()); _n())
  in (ignore (*1312*) (_n()); 
 (let n = (ignore (*1313*) (_n()); Yak.Yakker.get_string _x32 _x31 ykinput)
  in (ignore (*1315*) (_n());  Variables.counter := (int_of_string n) )
 ))
 ))
 ))
 | (1320) -> ( Compileopt.lookahead := true )
 | (1325) -> ( Compileopt.coalesce := false )
 | (1330) -> ( Yak.Pami.new_engine_flag := true )
 | (1335) -> ( Compileopt.check_labels := true )
 | (1340) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | (1350) -> (
 (let _x36 = (ignore (*1351*) (_n()); _n())
  in (ignore (*1352*) (_n()); 
 (let _x35 = (ignore (*1353*) (_n()); _n())
  in (ignore (*1354*) (_n()); 
 (let x = (ignore (*1355*) (_n()); Yak.Yakker.get_string _x36 _x35 ykinput)
  in (ignore (*1357*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | _(*1365*) -> (
 (let _x40 = (ignore (*1366*) (_n()); _n())
  in (ignore (*1367*) (_n()); 
 (let _x39 = (ignore (*1368*) (_n()); _n())
  in (ignore (*1369*) (_n()); 
 (let f = (ignore (*1370*) (_n()); Yak.Yakker.get_string _x40 _x39 ykinput)
  in (ignore (*1372*) (_n());  files := f::!files )
 ))
 ))
 ))
 ) in (ignore (*1373*) (_n()); _x42)
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
type sv = _wv ev * (hv*_pos) Yak.History.history
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
    (Yk_more(_,t),h) -> (t x p,h#empty)
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
    | (Yk_more(_,t),h) -> (match t x p with Yk_delay(v,hv) -> (v,(h#push((x),p))#push(hv,p)) | _ -> failwith "_ddelay1")
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
	  | Yk_bind(f) -> (f r,h1#merge((x),p) h2)
	  | _ -> failwith "_dmerge1")
    | _ -> failwith "_dmerge3")
let _d_and_push x p = function
    (Yk_more(_,t),h) -> (t x p,h#push((x),p))
  | _ -> failwith "_d_and_push"
let _dnext x p = function (*TJIM: same as _d without p *)
    (Yk_more(_,t),h) -> (t x p,h)
  | _ -> failwith "_dnext"
(* History transformers *)
let _p x p = (fun(v,h)->(v,h#push((x),p)))
let _m x p = (fun(v1,h1)->fun(_,h2)-> (v1,h1#merge((x),p) h2))

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
let _x79 =
 (fun _(*pos*) (_,_x54)(*arg of command*) -> (_t(function
 | 1032 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1037 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1042 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1047 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1052 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1057 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1062 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1067 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1072 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1077 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1082 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1087 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1092 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1097 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1108 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1113 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1118 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1123 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1128 ->
 (fun pos_ -> let _x71 _x8  = _t(fun _(*1131*) pos_ -> let _x73 _x72  = _t(fun _(*1140*) pos_ -> let _x76 _x7  = _t(fun _(*1143*) pos_ -> let _x78 _x77  = _t(fun _(*1146*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x7)) in _t(fun _(*1144*) pos_ -> Yk_delay(_x78 ((_wv0)) ,_x8))) in _t(fun _(*1141*) pos_ -> _x76 (pos_) )) in _t(fun _(*1132*) pos_ -> let _x74 _x6  = _t(function
 | 1138 ->
 (fun pos_ -> Yk_when(_x6>=1))
 | _(*1139*) ->
 (fun pos_ -> _x73 (ignore((_wv0));_wv0) )) in _t(fun _(*1133*) pos_ -> let rec _x75 _x6  = _t(function
 | 1134 ->
 (fun pos_ -> _x74 (_x6) )
 | _(*1136*) ->
 (fun pos_ -> _x75 (_x6+1) )) in _x75 (0) ))) in _t(fun _(*1129*) pos_ -> _x71 (pos_) ))
 | 1152 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1158 ->
 (fun pos_ -> let _x59 _x12  = _t(fun _(*1162*) pos_ -> let _x60 _x11  = _t(fun _(*1165*) pos_ -> let _x62 _x61  = _t(fun _(*1167*) pos_ -> let _x64 _x63  = _t(fun _(*1173*) pos_ -> let rec _x66 _x65  = _t(function
 | 1174 ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore(_x65);_wv0);_wv0);_wv0))
 | _(*1177*) ->
 (fun pos_ -> let _x67 _x16  = _t(fun _(*1181*) pos_ -> let _x68 _x15  = _t(fun _(*1184*) pos_ -> let _x70 _x69  = _t(fun _(*1187*) pos_ -> Yk_delay(_x66 (ignore(ignore((_wv0));_wv0);_wv0) ,_x15)) in _t(fun _(*1185*) pos_ -> Yk_delay(_x70 ((_wv0)) ,_x16))) in _t(fun _(*1182*) pos_ -> _x68 (pos_) )) in _t(fun _(*1178*) pos_ -> _x67 (pos_) ))) in _x66 (_wv0) ) in _t(fun _(*1168*) pos_ -> Yk_delay(_x64 ((_wv0)) ,_x11))) in _t(fun _(*1166*) pos_ -> Yk_delay(_x62 ((_wv0)) ,_x12))) in _t(fun _(*1163*) pos_ -> _x60 (pos_) )) in _t(fun _(*1159*) pos_ -> _x59 (pos_) ))
 | _(*1195*) ->
 (fun pos_ -> let _x56 _x55  = _t(function
 | 1198 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1202*) ->
 (fun pos_ -> let _x58 _x57  = _t(function
 | 1205 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1226*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1203*) pos_ -> _x58 (()) ))) in _t(fun _(*1196*) pos_ -> _x56 (()) ))),_x54))
let _x105 =
 (fun _(*pos*) (_,_x80)(*arg of args*) -> (_t(function
 | 1237 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1242 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1257 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1262 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1268 ->
 (fun pos_ -> let _x97 _x25  = _t(fun _(*1271*) pos_ -> let _x99 _x98  = _t(fun _(*1280*) pos_ -> let _x102 _x24  = _t(fun _(*1283*) pos_ -> let _x104 _x103  = _t(fun _(*1286*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x24)) in _t(fun _(*1284*) pos_ -> Yk_delay(_x104 ((_wv0)) ,_x25))) in _t(fun _(*1281*) pos_ -> _x102 (pos_) )) in _t(fun _(*1272*) pos_ -> let _x100 _x23  = _t(function
 | 1278 ->
 (fun pos_ -> Yk_when(_x23>=1))
 | _(*1279*) ->
 (fun pos_ -> _x99 (ignore((_wv0));_wv0) )) in _t(fun _(*1273*) pos_ -> let rec _x101 _x23  = _t(function
 | 1274 ->
 (fun pos_ -> _x100 (_x23) )
 | _(*1276*) ->
 (fun pos_ -> _x101 (_x23+1) )) in _x101 (0) ))) in _t(fun _(*1269*) pos_ -> _x97 (pos_) ))
 | 1293 ->
 (fun pos_ -> let _x89 _x30  = _t(fun _(*1296*) pos_ -> let _x91 _x90  = _t(fun _(*1305*) pos_ -> let _x94 _x29  = _t(fun _(*1308*) pos_ -> let _x96 _x95  = _t(fun _(*1311*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x29)) in _t(fun _(*1309*) pos_ -> Yk_delay(_x96 ((_wv0)) ,_x30))) in _t(fun _(*1306*) pos_ -> _x94 (pos_) )) in _t(fun _(*1297*) pos_ -> let _x92 _x28  = _t(function
 | 1303 ->
 (fun pos_ -> Yk_when(_x28>=1))
 | _(*1304*) ->
 (fun pos_ -> _x91 (ignore((_wv0));_wv0) )) in _t(fun _(*1298*) pos_ -> let rec _x93 _x28  = _t(function
 | 1299 ->
 (fun pos_ -> _x92 (_x28) )
 | _(*1301*) ->
 (fun pos_ -> _x93 (_x28+1) )) in _x93 (0) ))) in _t(fun _(*1294*) pos_ -> _x89 (pos_) ))
 | 1317 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1322 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1327 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1332 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1337 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1343 ->
 (fun pos_ -> let _x85 _x34  = _t(fun _(*1347*) pos_ -> let _x86 _x33  = _t(fun _(*1350*) pos_ -> let _x88 _x87  = _t(fun _(*1353*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x33)) in _t(fun _(*1351*) pos_ -> Yk_delay(_x88 ((_wv0)) ,_x34))) in _t(fun _(*1348*) pos_ -> _x86 (pos_) )) in _t(fun _(*1344*) pos_ -> _x85 (pos_) ))
 | _(*1358*) ->
 (fun pos_ -> let _x81 _x38  = _t(fun _(*1362*) pos_ -> let _x82 _x37  = _t(fun _(*1365*) pos_ -> let _x84 _x83  = _t(fun _(*1368*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x37)) in _t(fun _(*1366*) pos_ -> Yk_delay(_x84 ((_wv0)) ,_x38))) in _t(fun _(*1363*) pos_ -> _x82 (pos_) )) in _t(fun _(*1359*) pos_ -> _x81 (pos_) ))),_x80))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __p109 = _dnext 1279;;
let __a91 = _p 1320;;
let __a90 = _p 1260;;
let __a65 = _p 1150;;
let __a79 = _p 1100;;
let __a74 = _p 1090;;
let __a77 = _p 1040;;
let __a115 = fun p v -> _p 1288 p (_p 1287 p (_ddelay 1286 p (_p 1285 p (_ddelay 1284 p (_d_and_push 1283 p (_d 1281 p (_d 1280 p (v))))))));;
let __a85 = fun p v -> _p 1191 p (_p 1190 p (_p 1189 p (_p 1188 p (_ddelay 1187 p (_p 1186 p (_ddelay 1185 p (_d_and_push 1184 p (_d 1182 p (_d 1181 p (v))))))))));;
let __a118 = _p 1102;;
let __a10 = _d 1062;;
let __a48 = _p 1372;;
let __a126 = _p 1373;;
let __a21 = _d 1123;;
let __a130 = _p 1104;;
let __a127 = _p 1215;;
let __a117 = _p 1265;;
let __a116 = _p 1045;;
let __a105 = _p 1325;;
let __a100 = _p 1155;;
let __a44 = fun p v -> _d 1133 p (_d 1132 p (_d 1131 p (_d 1129 p (_d 1128 p (v)))));;
let __a69 = _p 1095;;
let __a123 = _p 1106;;
let __p110 = _dwhen 1278;;
let __a24 = _d 1237;;
let __a11 = _d 1067;;
let __a40 = _d 1018;;
let __a33 = fun p v -> _d 1359 p (_d 1358 p (v));;
let __a2 = fun p v -> _p 1030 p (_x79 p (v));;
let __a98 = _p 1330;;
let __a78 = _p 1050;;
let __a71 = fun p v -> _d 1298 p (_d 1297 p (_d 1296 p (_d 1294 p (_d 1293 p (v)))));;
let __a35 = _d 1020;;
let __a80 = _p 1111;;
let __a82 = _d 1301;;
let __a25 = _d 1242;;
let __a12 = _d 1072;;
let __a120 = _p 1224;;
let __a96 = fun p v -> _d 1273 p (_d 1272 p (_d 1271 p (_d 1269 p (_d 1268 p (v)))));;
let __a56 = _d 1134;;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a112 = _p 1335;;
let __a106 = _p 1055;;
let __a63 = _p 1116;;
let __a52 = _d 1136;;
let __a13 = _d 1077;;
let __a119 = fun p v -> _p 1218 p (_p 1217 p (v));;
let __a88 = _d 1299;;
let __p36 = _dnext 1023;;
let __p83 = _dnext 1304;;
let __a128 = fun p v -> _p 1222 p (_p 1214 p (v));;
let __a107 = fun p v -> _p 1213 p (_p 1212 p (_p 1211 p (v)));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x53 p (v)));;
let __p53 = _dnext 1139;;
let __a23 = fun p v -> _d 1196 p (_d 1195 p (v));;
let __a99 = _p 1060;;
let __a47 = _p 1340;;
let __a45 = _p 1010;;
let __a124 = _p 1121;;
let __p37 = _dwhen 1022;;
let __p84 = _dwhen 1303;;
let __a14 = _d 1082;;
let __a4 = _d 1032;;
let __a101 = _p 1233;;
let __a46 = _p 1013;;
let __a58 = _p 1234;;
let __a41 = _d 1205;;
let __a93 = _p 1175;;
let __a86 = _p 1065;;
let __a70 = _p 1126;;
let __a28 = _d 1317;;
let __a26 = _d 1257;;
let __p54 = _dwhen 1138;;
let __a15 = _d 1087;;
let __a5 = _d 1037;;
let __a38 = _d 1198;;
let __a61 = fun p v -> _d 1344 p (_d 1343 p (v));;
let __a50 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a66 = fun p v -> _p 1355 p (_p 1354 p (_ddelay 1353 p (_p 1352 p (_ddelay 1351 p (_d_and_push 1350 p (_d 1348 p (_d 1347 p (v))))))));;
let __a122 = _p 1240;;
let __a121 = _p 1290;;
let __a68 = _p 1070;;
let __a49 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a125 = fun p v -> _p 1221 p (_p 1220 p (v));;
let __a22 = _d 1152;;
let __a16 = _d 1092;;
let __a6 = _d 1042;;
let __a29 = _d 1322;;
let __a27 = _d 1262;;
let __a76 = _p 1245;;
let __a59 = _p 1075;;
let __a92 = _p 1025;;
let __a113 = _p 1027;;
let __a103 = _p 1247;;
let __a7 = _d 1047;;
let __a75 = _p 1357;;
let __a51 = fun p v -> _d 1159 p (_d 1158 p (v));;
let __a30 = _d 1327;;
let __a17 = _d 1097;;
let __a18 = _d 1108;;
let __a97 = _p 1249;;
let __a67 = _p 1029;;
let __a43 = fun p v -> _p 1370 p (_p 1369 p (_ddelay 1368 p (_p 1367 p (_ddelay 1366 p (_d_and_push 1365 p (_d 1363 p (_d 1362 p (v))))))));;
let __a34 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a62 = fun p v -> _d_and_push 1173 p (_p 1172 p (_p 1171 p (_p 1170 p (_p 1169 p (_ddelay 1168 p (_d_and_push 1167 p (_ddelay 1166 p (_d_and_push 1165 p (_d 1163 p (_d 1162 p (v)))))))))));;
let __a39 = fun p v -> _d 1203 p (_d 1202 p (v));;
let __a95 = fun p v -> _p 1209 p (_p 1208 p (v));;
let __a87 = _p 1080;;
let __a129 = _p 1251;;
let __a94 = fun p v -> _p 1192 p (_d_and_push 1174 p (v));;
let __a64 = _p 1201;;
let __a31 = _d 1332;;
let __a8 = _d 1052;;
let __a104 = _p 1253;;
let __a19 = _d 1113;;
let __a114 = _d 1274;;
let __a73 = _p 1194;;
let __a111 = _p 1255;;
let __a102 = _p 1315;;
let __a60 = _p 1085;;
let __a55 = _p 1035;;
let __a108 = _d 1276;;
let __a42 = _d 1226;;
let __a32 = _d 1337;;
let __a9 = _d 1057;;
let __a20 = _d 1118;;
let __a57 = fun p v -> _p 1148 p (_p 1147 p (_ddelay 1146 p (_p 1145 p (_ddelay 1144 p (_d_and_push 1143 p (_d 1141 p (_d 1140 p (v))))))));;
let __a72 = fun p v -> _d 1178 p (_d 1177 p (v));;
let __a3 = fun p v -> _p 1235 p (_x105 p (v));;
let __a89 = fun p v -> _p 1313 p (_p 1312 p (_ddelay 1311 p (_p 1310 p (_ddelay 1309 p (_d_and_push 1308 p (_d 1306 p (_d 1305 p (v))))))));;
let __a81 = fun p v -> _p 1230 p (_p 1229 p (v));;
let __binder0 = __default_ret;;
let __binder1 = _m 1002;;
let __binder2 = _m 1009;;
let __binder3 = _m 1231;;
let __binder4 = _m 1210;;
let __binder5 = _m 1219;;
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
(383, [EatInstr(115,410)]);
(0, [ASimpleCont2Instr(275,__binder0,12);ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [AAction2Instr(__a74,237)]);
(1, [EatInstr(127,13);EatInstr(126,13);EatInstr(125,13);EatInstr(124,13);EatInstr(123,13);EatInstr(122,13);EatInstr(121,13);EatInstr(120,13);EatInstr(119,13);EatInstr(118,13);EatInstr(117,13);EatInstr(116,13);EatInstr(115,13);EatInstr(114,13);EatInstr(113,13);EatInstr(112,13);EatInstr(111,13);EatInstr(110,13);EatInstr(109,13);EatInstr(108,13);EatInstr(107,13);EatInstr(106,13);EatInstr(105,13);EatInstr(104,13);EatInstr(103,13);EatInstr(102,13);EatInstr(101,13);EatInstr(100,13);EatInstr(99,13);EatInstr(98,13);EatInstr(97,13);EatInstr(96,13);EatInstr(95,13);EatInstr(94,13);EatInstr(93,13);EatInstr(92,13);EatInstr(91,13);EatInstr(90,13);EatInstr(89,13);EatInstr(88,13);EatInstr(87,13);EatInstr(86,13);EatInstr(85,13);EatInstr(84,13);EatInstr(83,13);EatInstr(82,13);EatInstr(81,13);EatInstr(80,13);EatInstr(79,13);EatInstr(78,13);EatInstr(77,13);EatInstr(76,13);EatInstr(75,13);EatInstr(74,13);EatInstr(73,13);EatInstr(72,13);EatInstr(71,13);EatInstr(70,13);EatInstr(69,13);EatInstr(68,13);EatInstr(67,13);EatInstr(66,13);EatInstr(65,13);EatInstr(64,13);EatInstr(63,13);EatInstr(62,13);EatInstr(61,13);EatInstr(60,13);EatInstr(59,13);EatInstr(58,13);EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13);EatInstr(47,13);EatInstr(46,13);EatInstr(45,13);EatInstr(44,13);EatInstr(43,13);EatInstr(42,13);EatInstr(41,13);EatInstr(40,13);EatInstr(39,13);EatInstr(38,13);EatInstr(37,13);EatInstr(36,13);EatInstr(35,13);EatInstr(34,13);EatInstr(33,13);EatInstr(32,13);EatInstr(31,13);EatInstr(30,13);EatInstr(29,13);EatInstr(28,13);EatInstr(27,13);EatInstr(26,13);EatInstr(25,13);EatInstr(24,13);EatInstr(23,13);EatInstr(22,13);EatInstr(21,13);EatInstr(20,13);EatInstr(19,13);EatInstr(18,13);EatInstr(17,13);EatInstr(16,13);EatInstr(15,13);EatInstr(14,13);EatInstr(13,13);EatInstr(12,13);EatInstr(11,13);EatInstr(10,13);EatInstr(9,13);EatInstr(8,13);EatInstr(7,13);EatInstr(6,13);EatInstr(5,13);EatInstr(4,13);EatInstr(3,13);EatInstr(2,13);EatInstr(1,13)]);
(385, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,411)]);
(2, [EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14)]);
(386, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,412)]);
(3, [EatInstr(255,15);EatInstr(254,15);EatInstr(253,15);EatInstr(252,15);EatInstr(251,15);EatInstr(250,15);EatInstr(249,15);EatInstr(248,15);EatInstr(247,15);EatInstr(246,15);EatInstr(245,15);EatInstr(244,15);EatInstr(243,15);EatInstr(242,15);EatInstr(241,15);EatInstr(240,15);EatInstr(239,15);EatInstr(238,15);EatInstr(237,15);EatInstr(236,15);EatInstr(235,15);EatInstr(234,15);EatInstr(233,15);EatInstr(232,15);EatInstr(231,15);EatInstr(230,15);EatInstr(229,15);EatInstr(228,15);EatInstr(227,15);EatInstr(226,15);EatInstr(225,15);EatInstr(224,15);EatInstr(223,15);EatInstr(222,15);EatInstr(221,15);EatInstr(220,15);EatInstr(219,15);EatInstr(218,15);EatInstr(217,15);EatInstr(216,15);EatInstr(215,15);EatInstr(214,15);EatInstr(213,15);EatInstr(212,15);EatInstr(211,15);EatInstr(210,15);EatInstr(209,15);EatInstr(208,15);EatInstr(207,15);EatInstr(206,15);EatInstr(205,15);EatInstr(204,15);EatInstr(203,15);EatInstr(202,15);EatInstr(201,15);EatInstr(200,15);EatInstr(199,15);EatInstr(198,15);EatInstr(197,15);EatInstr(196,15);EatInstr(195,15);EatInstr(194,15);EatInstr(193,15);EatInstr(192,15);EatInstr(191,15);EatInstr(190,15);EatInstr(189,15);EatInstr(188,15);EatInstr(187,15);EatInstr(186,15);EatInstr(185,15);EatInstr(184,15);EatInstr(183,15);EatInstr(182,15);EatInstr(181,15);EatInstr(180,15);EatInstr(179,15);EatInstr(178,15);EatInstr(177,15);EatInstr(176,15);EatInstr(175,15);EatInstr(174,15);EatInstr(173,15);EatInstr(172,15);EatInstr(171,15);EatInstr(170,15);EatInstr(169,15);EatInstr(168,15);EatInstr(167,15);EatInstr(166,15);EatInstr(165,15);EatInstr(164,15);EatInstr(163,15);EatInstr(162,15);EatInstr(161,15);EatInstr(160,15);EatInstr(159,15);EatInstr(158,15);EatInstr(157,15);EatInstr(156,15);EatInstr(155,15);EatInstr(154,15);EatInstr(153,15);EatInstr(152,15);EatInstr(151,15);EatInstr(150,15);EatInstr(149,15);EatInstr(148,15);EatInstr(147,15);EatInstr(146,15);EatInstr(145,15);EatInstr(144,15);EatInstr(143,15);EatInstr(142,15);EatInstr(141,15);EatInstr(140,15);EatInstr(139,15);EatInstr(138,15);EatInstr(137,15);EatInstr(136,15);EatInstr(135,15);EatInstr(134,15);EatInstr(133,15);EatInstr(132,15);EatInstr(131,15);EatInstr(130,15);EatInstr(129,15);EatInstr(128,15);EatInstr(0,15);EatInstr(127,15);EatInstr(126,15);EatInstr(125,15);EatInstr(124,15);EatInstr(123,15);EatInstr(122,15);EatInstr(121,15);EatInstr(120,15);EatInstr(119,15);EatInstr(118,15);EatInstr(117,15);EatInstr(116,15);EatInstr(115,15);EatInstr(114,15);EatInstr(113,15);EatInstr(112,15);EatInstr(111,15);EatInstr(110,15);EatInstr(109,15);EatInstr(108,15);EatInstr(107,15);EatInstr(106,15);EatInstr(105,15);EatInstr(104,15);EatInstr(103,15);EatInstr(102,15);EatInstr(101,15);EatInstr(100,15);EatInstr(99,15);EatInstr(98,15);EatInstr(97,15);EatInstr(96,15);EatInstr(95,15);EatInstr(94,15);EatInstr(93,15);EatInstr(92,15);EatInstr(91,15);EatInstr(90,15);EatInstr(89,15);EatInstr(88,15);EatInstr(87,15);EatInstr(86,15);EatInstr(85,15);EatInstr(84,15);EatInstr(83,15);EatInstr(82,15);EatInstr(81,15);EatInstr(80,15);EatInstr(79,15);EatInstr(78,15);EatInstr(77,15);EatInstr(76,15);EatInstr(75,15);EatInstr(74,15);EatInstr(73,15);EatInstr(72,15);EatInstr(71,15);EatInstr(70,15);EatInstr(69,15);EatInstr(68,15);EatInstr(67,15);EatInstr(66,15);EatInstr(65,15);EatInstr(64,15);EatInstr(63,15);EatInstr(62,15);EatInstr(61,15);EatInstr(60,15);EatInstr(59,15);EatInstr(58,15);EatInstr(57,15);EatInstr(56,15);EatInstr(55,15);EatInstr(54,15);EatInstr(53,15);EatInstr(52,15);EatInstr(51,15);EatInstr(50,15);EatInstr(49,15);EatInstr(48,15);EatInstr(47,15);EatInstr(46,15);EatInstr(45,15);EatInstr(44,15);EatInstr(43,15);EatInstr(42,15);EatInstr(41,15);EatInstr(40,15);EatInstr(39,15);EatInstr(38,15);EatInstr(37,15);EatInstr(36,15);EatInstr(35,15);EatInstr(34,15);EatInstr(33,15);EatInstr(32,15);EatInstr(31,15);EatInstr(30,15);EatInstr(29,15);EatInstr(28,15);EatInstr(27,15);EatInstr(26,15);EatInstr(25,15);EatInstr(24,15);EatInstr(23,15);EatInstr(22,15);EatInstr(21,15);EatInstr(20,15);EatInstr(19,15);EatInstr(18,15);EatInstr(17,15);EatInstr(16,15);EatInstr(15,15);EatInstr(14,15);EatInstr(13,15);EatInstr(12,15);EatInstr(11,15);EatInstr(10,15);EatInstr(9,15);EatInstr(8,15);EatInstr(7,15);EatInstr(6,15);EatInstr(5,15);EatInstr(4,15);EatInstr(3,15);EatInstr(2,15);EatInstr(1,15)]);
(387, [EatInstr(101,413)]);
(4, [AAction2Instr(__a0,16)]);
(388, [EatInstr(101,414)]);
(5, [EatInstr(0,17)]);
(389, [EatInstr(114,415)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,65)]);
(390, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,416)]);
(7, [EatInstr(127,27);EatInstr(126,27);EatInstr(125,27);EatInstr(124,27);EatInstr(123,27);EatInstr(122,27);EatInstr(121,27);EatInstr(120,27);EatInstr(119,27);EatInstr(118,27);EatInstr(117,27);EatInstr(116,27);EatInstr(115,27);EatInstr(114,27);EatInstr(113,27);EatInstr(112,27);EatInstr(111,27);EatInstr(110,27);EatInstr(109,27);EatInstr(108,27);EatInstr(107,27);EatInstr(106,27);EatInstr(105,27);EatInstr(104,27);EatInstr(103,27);EatInstr(102,27);EatInstr(101,27);EatInstr(100,27);EatInstr(99,27);EatInstr(98,27);EatInstr(97,27);EatInstr(96,27);EatInstr(95,27);EatInstr(94,27);EatInstr(93,27);EatInstr(92,27);EatInstr(91,27);EatInstr(90,27);EatInstr(89,27);EatInstr(88,27);EatInstr(87,27);EatInstr(86,27);EatInstr(85,27);EatInstr(84,27);EatInstr(83,27);EatInstr(82,27);EatInstr(81,27);EatInstr(80,27);EatInstr(79,27);EatInstr(78,27);EatInstr(77,27);EatInstr(76,27);EatInstr(75,27);EatInstr(74,27);EatInstr(73,27);EatInstr(72,27);EatInstr(71,27);EatInstr(70,27);EatInstr(69,27);EatInstr(68,27);EatInstr(67,27);EatInstr(66,27);EatInstr(65,27);EatInstr(64,27);EatInstr(63,27);EatInstr(62,27);EatInstr(61,27);EatInstr(60,27);EatInstr(59,27);EatInstr(58,27);EatInstr(57,27);EatInstr(56,27);EatInstr(55,27);EatInstr(54,27);EatInstr(53,27);EatInstr(52,27);EatInstr(51,27);EatInstr(50,27);EatInstr(49,27);EatInstr(48,27);EatInstr(47,27);EatInstr(46,27);EatInstr(44,27);EatInstr(43,27);EatInstr(42,27);EatInstr(41,27);EatInstr(40,27);EatInstr(39,27);EatInstr(38,27);EatInstr(37,27);EatInstr(36,27);EatInstr(35,27);EatInstr(34,27);EatInstr(33,27);EatInstr(32,27);EatInstr(31,27);EatInstr(30,27);EatInstr(29,27);EatInstr(28,27);EatInstr(27,27);EatInstr(26,27);EatInstr(25,27);EatInstr(24,27);EatInstr(23,27);EatInstr(22,27);EatInstr(21,27);EatInstr(20,27);EatInstr(19,27);EatInstr(18,27);EatInstr(17,27);EatInstr(16,27);EatInstr(15,27);EatInstr(14,27);EatInstr(13,27);EatInstr(12,27);EatInstr(11,27);EatInstr(10,27);EatInstr(9,27);EatInstr(8,27);EatInstr(7,27);EatInstr(6,27);EatInstr(5,27);EatInstr(4,27);EatInstr(3,27);EatInstr(2,27);EatInstr(1,27)]);
(391, [AAction2Instr(__a75,540)]);
(8, [EatInstr(105,19);EatInstr(97,18)]);
(392, [EatInstr(97,419)]);
(9, [EatInstr(112,20)]);
(393, [EatInstr(101,420)]);
(10, [AAction2Instr(__a2,21)]);
(394, [AAction2Instr(__a76,421)]);
(11, [AAction2Instr(__a3,22)]);
(395, [EatInstr(115,422)]);
(12, [ALookaheadInstr(false,CfgLA (3,266),23);RCompleteInstr2(275,nullable_eof)]);
(396, [EatInstr(101,423)]);
(13, [CompleteInstr(264)]);
(397, [EatInstr(100,424)]);
(14, [CompleteInstr(265)]);
(398, [EatInstr(115,425)]);
(15, [CompleteInstr(266)]);
(399, [EatInstr(110,426)]);
(16, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder1,24)]);
(400, [EatInstr(98,427)]);
(17, [CompleteInstr(268)]);
(401, [EatInstr(97,428)]);
(18, [EatInstr(100,29)]);
(402, [EatInstr(97,429)]);
(19, [EatInstr(110,30)]);
(403, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,430)]);
(20, [EatInstr(114,31)]);
(404, [AAction2Instr(__a77,237)]);
(21, [EatInstr(114,33);EatInstr(101,32);AAction2Instr(__a23,53);AAction2Instr(__a22,52);AAction2Instr(__a21,51);AAction2Instr(__a20,50);AAction2Instr(__a19,49);AAction2Instr(__a18,48);AAction2Instr(__a17,47);AAction2Instr(__a16,46);AAction2Instr(__a15,45);AAction2Instr(__a14,44);AAction2Instr(__a13,43);AAction2Instr(__a12,42);AAction2Instr(__a11,41);AAction2Instr(__a10,40);AAction2Instr(__a9,39);AAction2Instr(__a8,38);AAction2Instr(__a7,37);AAction2Instr(__a6,36);AAction2Instr(__a5,35);AAction2Instr(__a4,34)]);
(405, [EatInstr(45,431)]);
(22, [EatInstr(45,54);AAction2Instr(__a33,64);AAction2Instr(__a32,63);AAction2Instr(__a31,62);AAction2Instr(__a30,61);AAction2Instr(__a29,60);AAction2Instr(__a28,59);AAction2Instr(__a27,58);AAction2Instr(__a26,57);AAction2Instr(__a25,56);AAction2Instr(__a24,55)]);
(406, [AAction2Instr(__a78,237)]);
(23, [CompleteInstr(275)]);
(407, [EatInstr(110,432)]);
(24, [AAction2Instr(__a34,190)]);
(408, [EatInstr(108,433)]);
(25, [AAction2Instr(__a35,65)]);
(409, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,434)]);
(26, [AWhenInstr3(__p37,__p36,66)]);
(410, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,435)]);
(27, [ALookaheadInstr(false,CfgLA (1,264),28);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,27)]);
(411, [AAction2Instr(__a79,436)]);
(28, [CompleteInstr(270)]);
(412, [AAction2Instr(__a80,237)]);
(29, [EatInstr(100,67)]);
(413, [EatInstr(45,437)]);
(30, [EatInstr(108,68)]);
(414, [EatInstr(100,438)]);
(31, [EatInstr(101,69)]);
(415, [EatInstr(109,439)]);
(32, [EatInstr(120,70)]);
(416, [AAction2Instr(__a81,440)]);
(33, [EatInstr(102,71)]);
(417, [AAction2Instr(__a82,441)]);
(34, [EatInstr(100,72)]);
(418, [AWhenInstr3(__p84,__p83,442)]);
(35, [EatInstr(108,73)]);
(419, [EatInstr(114,443)]);
(36, [EatInstr(97,74)]);
(420, [EatInstr(110,444)]);
(37, [EatInstr(119,75)]);
(421, [EatInstr(116,447);EatInstr(112,446);EatInstr(102,445)]);
(38, [EatInstr(100,76)]);
(422, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,448)]);
(39, [EatInstr(99,77)]);
(423, [EatInstr(103,449)]);
(40, [EatInstr(112,78)]);
(424, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,450)]);
(41, [EatInstr(100,79)]);
(425, [EatInstr(99,451)]);
(42, [EatInstr(108,80)]);
(426, [EatInstr(101,452)]);
(43, [EatInstr(97,81)]);
(427, [EatInstr(101,453)]);
(44, [EatInstr(119,82)]);
(428, [EatInstr(104,454)]);
(45, [EatInstr(100,83)]);
(429, [EatInstr(114,455)]);
(46, [EatInstr(99,84)]);
(430, [AAction2Instr(__a85,456)]);
(47, [EatInstr(116,85)]);
(431, [EatInstr(111,457)]);
(48, [EatInstr(112,86)]);
(432, [EatInstr(108,458)]);
(49, [EatInstr(112,87)]);
(433, [EatInstr(121,459)]);
(50, [EatInstr(115,88)]);
(434, [AAction2Instr(__a86,237)]);
(51, [EatInstr(101,89)]);
(435, [AAction2Instr(__a87,237)]);
(52, [EatInstr(112,90)]);
(436, [EatInstr(100,460)]);
(53, [AAction2Instr(__a39,92);AAction2Instr(__a38,91)]);
(437, [EatInstr(97,461)]);
(54, [EatInstr(117,95);EatInstr(114,94);EatInstr(99,93)]);
(438, [EatInstr(115,462)]);
(55, [EatInstr(45,96)]);
(439, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,463)]);
(56, [EatInstr(45,97)]);
(440, [ASimpleCont2Instr(272,__binder3,464);ACallInstr3(__default_call,9)]);
(57, [EatInstr(45,98)]);
(441, [AAction2Instr(__a88,418);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,417)]);
(58, [EatInstr(45,99)]);
(442, [AAction2Instr(__a89,465)]);
(59, [EatInstr(45,100)]);
(443, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,466)]);
(60, [EatInstr(45,101)]);
(444, [EatInstr(115,467)]);
(61, [EatInstr(45,102)]);
(445, [EatInstr(117,468)]);
(62, [EatInstr(45,103)]);
(446, [EatInstr(101,469)]);
(63, [EatInstr(45,104)]);
(447, [EatInstr(120,470)]);
(64, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,105)]);
(448, [AAction2Instr(__a90,540)]);
(65, [AAction2Instr(__a40,26);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,25)]);
(66, [ALookaheadInstr(false,CfgLA (1,264),108)]);
(449, [EatInstr(117,471)]);
(450, [AAction2Instr(__a91,540)]);
(67, [EatInstr(45,109)]);
(451, [EatInstr(101,472)]);
(68, [EatInstr(105,110)]);
(452, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,473)]);
(69, [EatInstr(99,111)]);
(453, [EatInstr(108,474)]);
(70, [EatInstr(101,112)]);
(454, [EatInstr(101,475)]);
(71, [EatInstr(99,113)]);
(455, [AAction2Instr(__a92,514)]);
(72, [EatInstr(111,114)]);
(456, [AAction2Instr(__a94,343);AAction2Instr(__a93,342)]);
(73, [EatInstr(105,115)]);
(457, [EatInstr(110,476)]);
(74, [EatInstr(116,116)]);
(458, [EatInstr(121,477)]);
(75, [EatInstr(114,117)]);
(459, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,478)]);
(76, [EatInstr(105,118)]);
(460, [EatInstr(121,479)]);
(77, [EatInstr(111,119)]);
(461, [EatInstr(99,480)]);
(78, [EatInstr(114,120)]);
(462, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,481)]);
(79, [EatInstr(101,121)]);
(463, [AAction2Instr(__a95,482)]);
(80, [EatInstr(105,122)]);
(464, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,483)]);
(81, [EatInstr(116,123)]);
(465, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,484)]);
(82, [EatInstr(114,124)]);
(466, [AAction2Instr(__a96,509)]);
(83, [EatInstr(105,125)]);
(467, [EatInstr(105,485)]);
(84, [EatInstr(111,126)]);
(468, [EatInstr(110,486)]);
(85, [EatInstr(114,127)]);
(469, [EatInstr(103,487)]);
(86, [EatInstr(114,128)]);
(470, [AAction2Instr(__a97,546)]);
(87, [EatInstr(114,129)]);
(471, [EatInstr(108,488)]);
(88, [EatInstr(116,130)]);
(472, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,489)]);
(89, [EatInstr(120,131)]);
(473, [AAction2Instr(__a98,540)]);
(90, [EatInstr(114,132)]);
(474, [EatInstr(115,490)]);
(91, [EatInstr(105,133)]);
(475, [EatInstr(97,491)]);
(92, [AAction2Instr(__a42,135);AAction2Instr(__a41,134)]);
(476, [EatInstr(108,493)]);
(93, [EatInstr(111,136)]);
(477, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,494)]);
(94, [EatInstr(111,137)]);
(478, [AAction2Instr(__a99,237)]);
(95, [EatInstr(110,138)]);
(479, [EatInstr(112,495)]);
(96, [EatInstr(99,139)]);
(480, [EatInstr(116,496)]);
(97, [EatInstr(98,140)]);
(481, [AAction2Instr(__a100,237)]);
(98, [EatInstr(105,141)]);
(482, [ASimpleCont2Instr(271,__binder4,497);ACallInstr3(__default_call,8)]);
(99, [EatInstr(105,142)]);
(483, [AAction2Instr(__a101,237)]);
(100, [EatInstr(108,143)]);
(484, [AAction2Instr(__a102,540)]);
(101, [EatInstr(110,144)]);
(485, [EatInstr(116,500)]);
(102, [EatInstr(110,145)]);
(486, [AAction2Instr(__a103,546)]);
(103, [EatInstr(99,146)]);
(487, [EatInstr(45,501);AAction2Instr(__a104,546)]);
(104, [EatInstr(118,147)]);
(488, [EatInstr(97,503)]);
(105, [AAction2Instr(__a43,148)]);
(489, [AAction2Instr(__a105,540)]);
(106, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder2,149)]);
(490, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,504)]);
(107, [ACallInstr3(__default_call,12);ASimpleCont2Instr(275,__binder0,150)]);
(491, [EatInstr(100,505)]);
(108, [CompleteInstr(269)]);
(109, [EatInstr(108,151)]);
(493, [EatInstr(121,506)]);
(110, [EatInstr(110,152)]);
(494, [AAction2Instr(__a106,237)]);
(111, [EatInstr(101,153)]);
(495, [EatInstr(103,507)]);
(112, [EatInstr(99,154)]);
(496, [EatInstr(105,508)]);
(113, [AAction2Instr(__a44,235)]);
(497, [AAction2Instr(__a107,543)]);
(114, [EatInstr(116,155)]);
(498, [AAction2Instr(__a108,509)]);
(115, [EatInstr(102,156)]);
(499, [AWhenInstr3(__p110,__p109,510)]);
(116, [EatInstr(116,157)]);
(500, [EatInstr(105,511)]);
(117, [EatInstr(97,158)]);
(501, [EatInstr(115,512)]);
(118, [EatInstr(115,159)]);
(502, [AAction2Instr(__a111,540)]);
(119, [EatInstr(109,160)]);
(503, [EatInstr(114,513)]);
(120, [EatInstr(101,161)]);
(504, [AAction2Instr(__a112,540)]);
(121, [EatInstr(115,162)]);
(505, [AAction2Instr(__a113,514)]);
(122, [EatInstr(102,163)]);
(506, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,515)]);
(123, [EatInstr(116,164)]);
(507, [EatInstr(101,516)]);
(124, [EatInstr(97,165)]);
(508, [EatInstr(111,517)]);
(125, [EatInstr(115,166)]);
(509, [AAction2Instr(__a114,499);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,498)]);
(126, [EatInstr(109,167)]);
(510, [AAction2Instr(__a115,520)]);
(127, [EatInstr(97,168)]);
(511, [EatInstr(118,521)]);
(128, [EatInstr(105,169)]);
(512, [EatInstr(116,522)]);
(129, [EatInstr(105,170)]);
(513, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,523)]);
(130, [EatInstr(114,171)]);
(514, [CompleteInstr(271)]);
(131, [EatInstr(116,172)]);
(515, [AAction2Instr(__a116,237)]);
(132, [EatInstr(105,173)]);
(516, [EatInstr(110,524)]);
(133, [EatInstr(110,174)]);
(517, [EatInstr(110,525)]);
(134, [EatInstr(116,175)]);
(518, [EatInstr(44,526)]);
(135, [EatInstr(97,176)]);
(519, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,527)]);
(136, [EatInstr(117,177)]);
(520, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,528)]);
(137, [EatInstr(111,178)]);
(521, [EatInstr(101,529)]);
(138, [EatInstr(114,179)]);
(522, [EatInstr(114,530)]);
(139, [EatInstr(97,180)]);
(523, [AAction2Instr(__a117,540)]);
(140, [EatInstr(97,181)]);
(524, [EatInstr(45,531);AAction2Instr(__a118,555)]);
(141, [EatInstr(110,182)]);
(525, [EatInstr(115,532)]);
(142, [EatInstr(110,183)]);
(526, [AAction2Instr(__a119,533)]);
(143, [EatInstr(111,184)]);
(527, [AAction2Instr(__a120,237)]);
(144, [EatInstr(111,185)]);
(528, [AAction2Instr(__a121,540)]);
(145, [EatInstr(101,186)]);
(529, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,534)]);
(146, [EatInstr(104,187)]);
(530, [EatInstr(105,535)]);
(147, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,188)]);
(531, [EatInstr(115,536)]);
(148, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,189)]);
(532, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(149, [AAction2Instr(__a45,190)]);
(533, [ASimpleCont2Instr(271,__binder5,539);ACallInstr3(__default_call,8)]);
(150, [AAction2Instr(__a46,191)]);
(534, [AAction2Instr(__a122,540)]);
(151, [EatInstr(114,192)]);
(535, [EatInstr(99,541)]);
(152, [EatInstr(101,193)]);
(536, [EatInstr(99,542)]);
(153, [EatInstr(100,194)]);
(537, [AAction2Instr(__a123,237)]);
(154, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,195)]);
(538, [AAction2Instr(__a124,237)]);
(155, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,198)]);
(539, [AAction2Instr(__a125,543)]);
(156, [EatInstr(116,199)]);
(540, [AAction2Instr(__a126,270)]);
(157, [EatInstr(114,200)]);
(541, [EatInstr(116,544)]);
(158, [EatInstr(112,201)]);
(542, [EatInstr(97,545)]);
(159, [EatInstr(112,202)]);
(543, [AAction2Instr(__a128,519);AAction2Instr(__a127,518)]);
(160, [EatInstr(112,203)]);
(544, [AAction2Instr(__a129,546)]);
(161, [EatInstr(99,204)]);
(545, [EatInstr(110,547)]);
(162, [EatInstr(117,205)]);
(546, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,502)]);
(163, [EatInstr(116,206)]);
(547, [EatInstr(110,548)]);
(164, [EatInstr(114,207)]);
(548, [EatInstr(101,549)]);
(165, [EatInstr(112,208)]);
(549, [EatInstr(114,550)]);
(166, [EatInstr(112,209)]);
(550, [EatInstr(108,551)]);
(167, [EatInstr(112,210)]);
(551, [EatInstr(101,552)]);
(168, [EatInstr(110,211)]);
(552, [EatInstr(115,553)]);
(169, [EatInstr(110,212)]);
(553, [EatInstr(115,554)]);
(170, [EatInstr(110,213)]);
(554, [AAction2Instr(__a130,555)]);
(171, [EatInstr(105,214)]);
(555, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,537)]);
(172, [EatInstr(114,215)]);
(173, [EatInstr(110,216)]);
(174, [EatInstr(102,217)]);
(175, [EatInstr(114,218)]);
(176, [EatInstr(110,219)]);
(177, [EatInstr(110,220)]);
(178, [EatInstr(116,221)]);
(179, [EatInstr(111,222)]);
(180, [EatInstr(115,223)]);
(181, [EatInstr(99,224)]);
(182, [EatInstr(108,225)]);
(183, [EatInstr(108,226)]);
(184, [EatInstr(111,227)]);
(185, [EatInstr(45,228)]);
(186, [EatInstr(119,229)]);
(187, [EatInstr(101,230)]);
(188, [AAction2Instr(__a47,540)]);
(189, [AAction2Instr(__a48,540)]);
(190, [AAction2Instr(__a50,107);AAction2Instr(__a49,106)]);
(191, [CompleteInstr(267)]);
(192, [EatInstr(49,231)]);
(193, [EatInstr(45,232)]);
(194, [EatInstr(101,233)]);
(195, [AAction2Instr(__a51,234)]);
(196, [AAction2Instr(__a52,235)]);
(197, [AWhenInstr3(__p54,__p53,236)]);
(198, [AAction2Instr(__a55,237)]);
(199, [EatInstr(45,238)]);
(200, [EatInstr(105,239)]);
(201, [EatInstr(45,240)]);
(202, [EatInstr(97,241)]);
(203, [EatInstr(105,242)]);
(204, [EatInstr(101,243)]);
(205, [EatInstr(103,244)]);
(206, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,245)]);
(207, [EatInstr(105,246)]);
(208, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,247)]);
(209, [EatInstr(97,248)]);
(210, [EatInstr(105,249)]);
(211, [EatInstr(115,250)]);
(212, [EatInstr(116,251)]);
(213, [EatInstr(116,252)]);
(214, [EatInstr(112,253)]);
(215, [EatInstr(97,254)]);
(216, [EatInstr(116,255)]);
(217, [EatInstr(111,256)]);
(218, [EatInstr(97,257)]);
(219, [EatInstr(97,258)]);
(220, [EatInstr(116,259)]);
(221, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,260)]);
(222, [EatInstr(108,261)]);
(223, [EatInstr(101,262)]);
(224, [EatInstr(107,263)]);
(225, [EatInstr(105,264)]);
(226, [EatInstr(105,265)]);
(227, [EatInstr(107,266)]);
(228, [EatInstr(99,267)]);
(229, [EatInstr(45,268)]);
(230, [EatInstr(99,269)]);
(231, [EatInstr(45,271)]);
(232, [EatInstr(114,272)]);
(233, [EatInstr(110,273)]);
(234, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,274)]);
(235, [AAction2Instr(__a56,197);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,196)]);
(236, [AAction2Instr(__a57,275)]);
(237, [AAction2Instr(__a58,276)]);
(238, [EatInstr(111,277)]);
(239, [EatInstr(98,278)]);
(240, [EatInstr(111,279)]);
(241, [EatInstr(116,280)]);
(242, [EatInstr(108,281)]);
(243, [EatInstr(100,282)]);
(244, [EatInstr(97,283)]);
(245, [AAction2Instr(__a59,237)]);
(246, [EatInstr(98,284)]);
(247, [AAction2Instr(__a60,237)]);
(248, [EatInstr(116,285)]);
(249, [EatInstr(108,286)]);
(250, [EatInstr(108,287)]);
(251, [EatInstr(45,288)]);
(252, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,289)]);
(253, [EatInstr(45,290)]);
(254, [EatInstr(99,291)]);
(255, [EatInstr(45,292)]);
(256, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,293)]);
(257, [EatInstr(110,294)]);
(258, [EatInstr(108,295)]);
(259, [EatInstr(101,296)]);
(260, [AAction2Instr(__a61,297)]);
(261, [EatInstr(108,298)]);
(262, [EatInstr(45,299)]);
(263, [EatInstr(101,300)]);
(264, [EatInstr(110,301)]);
(265, [EatInstr(110,302)]);
(266, [EatInstr(97,303)]);
(267, [EatInstr(111,304)]);
(268, [EatInstr(101,305)]);
(269, [EatInstr(107,306)]);
(270, [CompleteInstr(274)]);
(271, [EatInstr(108,307)]);
(272, [EatInstr(101,308)]);
(273, [EatInstr(99,309)]);
(274, [AAction2Instr(__a62,456)]);
(275, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,310)]);
(276, [CompleteInstr(273)]);
(277, [EatInstr(110,311)]);
(278, [EatInstr(117,312)]);
(279, [EatInstr(110,313)]);
(280, [EatInstr(99,314)]);
(281, [EatInstr(101,315)]);
(282, [EatInstr(101,316)]);
(283, [EatInstr(114,317)]);
(284, [EatInstr(117,318)]);
(285, [EatInstr(99,319)]);
(286, [EatInstr(101,320)]);
(287, [EatInstr(97,321)]);
(288, [EatInstr(103,322)]);
(289, [AAction2Instr(__a63,237)]);
(290, [EatInstr(108,323)]);
(291, [EatInstr(116,324)]);
(292, [EatInstr(110,325)]);
(293, [AAction2Instr(__a64,237)]);
(294, [EatInstr(115,326)]);
(295, [EatInstr(121,327)]);
(296, [EatInstr(114,328)]);
(297, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,329)]);
(298, [EatInstr(45,330)]);
(299, [EatInstr(105,331)]);
(300, [EatInstr(110,332)]);
(301, [EatInstr(101,333)]);
(302, [EatInstr(101,334)]);
(303, [EatInstr(104,335)]);
(304, [EatInstr(97,336)]);
(305, [EatInstr(110,337)]);
(306, [EatInstr(45,338)]);
(307, [EatInstr(111,339)]);
(308, [EatInstr(103,340)]);
(309, [EatInstr(101,341)]);
(310, [AAction2Instr(__a65,237)]);
(311, [EatInstr(108,344)]);
(312, [EatInstr(116,345)]);
(313, [EatInstr(108,346)]);
(314, [EatInstr(104,347)]);
(315, [EatInstr(45,348)]);
(316, [EatInstr(110,349)]);
(317, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,350)]);
(318, [EatInstr(116,351)]);
(319, [EatInstr(104,352)]);
(320, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,353)]);
(321, [EatInstr(116,354)]);
(322, [EatInstr(105,355)]);
(323, [EatInstr(97,356)]);
(324, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,357)]);
(325, [EatInstr(112,358)]);
(326, [EatInstr(102,359)]);
(327, [EatInstr(122,360)]);
(328, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,361)]);
(329, [AAction2Instr(__a66,362)]);
(330, [EatInstr(115,363)]);
(331, [EatInstr(110,364)]);
(332, [EatInstr(100,365)]);
(333, [EatInstr(45,366)]);
(334, [EatInstr(45,367)]);
(335, [EatInstr(101,368)]);
(336, [EatInstr(108,369)]);
(337, [EatInstr(103,370)]);
(338, [EatInstr(108,371)]);
(339, [EatInstr(111,372)]);
(340, [EatInstr(117,373)]);
(341, [AAction2Instr(__a67,374)]);
(342, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,375)]);
(343, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,376)]);
(344, [EatInstr(121,377)]);
(345, [EatInstr(101,378)]);
(346, [EatInstr(121,379)]);
(347, [EatInstr(45,380)]);
(348, [EatInstr(111,381)]);
(349, [EatInstr(99,382)]);
(350, [AAction2Instr(__a68,237)]);
(351, [EatInstr(101,383)]);
(352, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,384)]);
(353, [AAction2Instr(__a69,237)]);
(354, [EatInstr(101,385)]);
(355, [EatInstr(108,386)]);
(356, [EatInstr(116,387)]);
(357, [AAction2Instr(__a70,237)]);
(358, [EatInstr(114,388)]);
(359, [EatInstr(111,389)]);
(360, [EatInstr(101,390)]);
(361, [AAction2Instr(__a71,441)]);
(362, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,391)]);
(363, [EatInstr(116,392)]);
(364, [EatInstr(115,393)]);
(365, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,394)]);
(366, [EatInstr(99,395)]);
(367, [EatInstr(114,396)]);
(368, [EatInstr(97,397)]);
(369, [EatInstr(101,398)]);
(370, [EatInstr(105,399)]);
(371, [EatInstr(97,400)]);
(372, [EatInstr(107,401)]);
(373, [EatInstr(108,402)]);
(374, [CompleteInstr(272)]);
(375, [AAction2Instr(__a72,403)]);
(376, [AAction2Instr(__a73,237)]);
(377, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,404)]);
(378, [EatInstr(115,405)]);
(379, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,406)]);
(380, [EatInstr(111,407)]);
(381, [EatInstr(110,408)]);
(382, [EatInstr(101,409)]);
]

let start_symb = get_symb_action "cmd-line-args"

module P2__ = Yak.Engine.Full_yakker(struct type t = sv let cmp = sv_compare end)

let _wfe_data_ = Yak.PamJIT.DNELR.to_table (Yak.Pam_internal.load_internal_program program)
  start_symb (get_symb_start start_symb) 264 num_symbols
  __default_call __default_ret

let parse = Yak.Pami.Wfe.mk_parse P2__.parse _wfe_data_ sv0 
    (fun ykinput (_,h) ->
      let _o = (new Yak.History.postfix h) in
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
