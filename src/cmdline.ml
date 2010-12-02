
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
| Exec_cmd
| Compile_cmd
| Translate_cmd
| Close_under_core_cmd
| Dispatch_cmd
| Wrap_cmd
| Attributes_cmd
| Lift_cmd
| Desugar_cmd
| Fuse_cmd
| Tx_prec_cmd
| Print_gil_cmd
| Print_gul_cmd
| Print_npreds_cmd
| Extract_cmd
| Lexer_cmd
| Subset_cmd
| Hash_cmd
| Minus_cmd
| Inline_regular_cmd
| Unroll_star_cmd
| Lookahead_analysis_cmd
| Copyrule_cmd
| Print_relevance_cmd
| Rfc_cmd
| Dot_cmd
| Strip_late_actions_cmd
| Info_cmd (* display internal information about the compiler. *)
| Transform_cmd
| Analyze_cmd (** Perform a grammar analysis. *)

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
 | (1035) -> ( Dot_cmd )
 | (1040) -> ( Tx_prec_cmd )
 | (1045) -> ( Desugar_cmd )
 | (1050) -> ( Lift_cmd )
 | (1055) -> ( Attributes_cmd )
 | (1060) -> ( Wrap_cmd )
 | (1065) -> ( Dispatch_cmd )
 | (1070) -> ( Compile_cmd )
 | (1075) -> (
 (let t = 
 (match _n() with
 | (1077) -> (Dypgen_PI false)
 | _(*1079*) -> (Dypgen_PI true)
 ) in (ignore (*1081*) (_n());  translate_plugin := t; Translate_cmd )
 ))
 | (1086) -> ( Print_gil_cmd )
 | (1091) -> ( Print_gul_cmd )
 | (1096) -> ( Strip_late_actions_cmd )
 | (1101) -> ( Extract_cmd )
 | (1118) -> (
 (let _x10 = (ignore (*1119*) (_n()); _n())
  in (ignore (*1120*) (_n()); 
 (let _x9 = (ignore (*1121*) (_n()); _n())
  in (ignore (*1122*) (_n()); 
 (let n = (ignore (*1123*) (_n()); Yak.YkBuf.get_string _x10 _x9 ykinput)
  in (ignore (*1125*) (_n());  try rfc_num := int_of_string n; Rfc_cmd with _ -> failwith "Invalid RFC number" )
 ))
 ))
 ))
 | (1130) -> ( Print_npreds_cmd )
 | (1140) -> (
 (let _x14 = (ignore (*1141*) (_n()); _n())
  in (ignore (*1142*) (_n()); 
 (let _x13 = (ignore (*1143*) (_n()); _n())
  in (ignore (*1144*) (_n()); 
 (let f = (ignore (*1145*) (_n()); Yak.YkBuf.get_string _x14 _x13 ykinput)
  in (ignore (*1146*) (_n()); 
 (let l = (ignore (*1147*) (_n()); 
 (let _x20 = (ignore (*1148*) (_n()); 
 (let rec _x49 _x20 = (match _n() with 1149 -> _x20 | _x48 -> _x49((ignore (*1150*) (_x48); 
 (let _x19 = (ignore (*1159*) (_n()); 
 (let _x18 = (ignore (*1160*) (_n()); _n())
  in (ignore (*1161*) (_n()); 
 (let _x17 = (ignore (*1162*) (_n()); _n())
  in (ignore (*1163*) (_n()); 
 (let x = (ignore (*1164*) (_n()); Yak.YkBuf.get_string _x18 _x17 ykinput)
  in (ignore (*1165*) (_n()); x)
 ))
 ))
 ))
  in (ignore (*1166*) (_n()); _x19::_x20)
 ))
 )) in _x49([])))
  in (ignore (*1167*) (_n()); (List.rev _x20))
 ))
  in (ignore (*1169*) (_n());  files := f::!files; exec_l := l; Exec_cmd )
 ))
 ))
 ))
 ))
 | (1176) -> ( Info_cmd )
 | (1183) -> (
 (let tx = (ignore (*1184*) (_n()); _r_transformation(_n,ykinput))
  in (ignore (*1186*) (_n()); 
 (let txs = (ignore (*1187*) (_n()); 
 (let _x22 = (ignore (*1188*) (_n()); 
 (let rec _x47 _x22 = (match _n() with 1189 -> _x22 | _x46 -> _x47((ignore (*1190*) (_x46); 
 (let _x21 = (ignore (*1192*) (_n()); 
 (let x = (ignore (*1193*) (_n()); _r_transformation(_n,ykinput))
  in (ignore (*1195*) (_n()); x)
 ))
  in (ignore (*1196*) (_n()); _x21::_x22)
 ))
 )) in _x47([])))
  in (ignore (*1197*) (_n()); (List.rev _x22))
 ))
  in (ignore (*1199*) (_n());  transforms := (tx::txs); Transform_cmd )
 ))
 ))
 | _(*1204*) -> (
 (let a = (ignore (*1205*) (_n()); _r_analysis(_n,ykinput))
  in (ignore (*1208*) (_n());  analysis := a; Analyze_cmd )
 ))
 ) in (ignore (*1209*) (_n()); _x41)
 ))
 
 and
_r_args(_n,ykinput) = (ignore (*1210*) (_n()); 
 (let _x42 = 
 (match _n() with
 | (1215) -> ( Compileopt.case_sensitive := false )
 | (1220) -> (
 (let b = 
 (match _n() with
 | (1222) -> (Fun_BE)
 | (1224) -> (Trans_BE)
 | (1226) -> (Peg_BE false)
 | _(*1228*) -> (Peg_BE true)
 ) in (ignore (*1230*) (_n());  backend := b )
 ))
 | (1235) -> ( only := true )
 | (1240) -> ( Compileopt.inline_cs := true )
 | (1245) -> ( Compileopt.inline_regular := true )
 | (1263) -> (
 (let _x27 = (ignore (*1264*) (_n()); _n())
  in (ignore (*1265*) (_n()); 
 (let _x26 = (ignore (*1266*) (_n()); _n())
  in (ignore (*1267*) (_n()); 
 (let n = (ignore (*1268*) (_n()); Yak.YkBuf.get_string _x27 _x26 ykinput)
  in (ignore (*1270*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1288) -> (
 (let _x32 = (ignore (*1289*) (_n()); _n())
  in (ignore (*1290*) (_n()); 
 (let _x31 = (ignore (*1291*) (_n()); _n())
  in (ignore (*1292*) (_n()); 
 (let n = (ignore (*1293*) (_n()); Yak.YkBuf.get_string _x32 _x31 ykinput)
  in (ignore (*1295*) (_n());  Variables.counter := (int_of_string n) )
 ))
 ))
 ))
 | (1300) -> ( Compileopt.lookahead := true )
 | (1305) -> ( Compileopt.coalesce := false )
 | (1310) -> ( Yak.Pami.new_engine_flag := true )
 | (1315) -> ( Compileopt.check_labels := true )
 | (1320) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | (1330) -> (
 (let _x36 = (ignore (*1331*) (_n()); _n())
  in (ignore (*1332*) (_n()); 
 (let _x35 = (ignore (*1333*) (_n()); _n())
  in (ignore (*1334*) (_n()); 
 (let x = (ignore (*1335*) (_n()); Yak.YkBuf.get_string _x36 _x35 ykinput)
  in (ignore (*1337*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | _(*1345*) -> (
 (let _x40 = (ignore (*1346*) (_n()); _n())
  in (ignore (*1347*) (_n()); 
 (let _x39 = (ignore (*1348*) (_n()); _n())
  in (ignore (*1349*) (_n()); 
 (let f = (ignore (*1350*) (_n()); Yak.YkBuf.get_string _x40 _x39 ykinput)
  in (ignore (*1352*) (_n());  files := f::!files )
 ))
 ))
 ))
 ) in (ignore (*1353*) (_n()); _x42)
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
 | 1083 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1088 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1093 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1098 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1103 ->
 (fun pos_ -> let _x71 _x8  = _t(fun _(*1106*) pos_ -> let _x73 _x72  = _t(fun _(*1115*) pos_ -> let _x76 _x7  = _t(fun _(*1118*) pos_ -> let _x78 _x77  = _t(fun _(*1121*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x7)) in _t(fun _(*1119*) pos_ -> Yk_delay(_x78 ((_wv0)) ,_x8))) in _t(fun _(*1116*) pos_ -> _x76 (pos_) )) in _t(fun _(*1107*) pos_ -> let _x74 _x6  = _t(function
 | 1113 ->
 (fun pos_ -> Yk_when(_x6>=1))
 | _(*1114*) ->
 (fun pos_ -> _x73 (ignore((_wv0));_wv0) )) in _t(fun _(*1108*) pos_ -> let rec _x75 _x6  = _t(function
 | 1109 ->
 (fun pos_ -> _x74 (_x6) )
 | _(*1111*) ->
 (fun pos_ -> _x75 (_x6+1) )) in _x75 (0) ))) in _t(fun _(*1104*) pos_ -> _x71 (pos_) ))
 | 1127 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1133 ->
 (fun pos_ -> let _x59 _x12  = _t(fun _(*1137*) pos_ -> let _x60 _x11  = _t(fun _(*1140*) pos_ -> let _x62 _x61  = _t(fun _(*1142*) pos_ -> let _x64 _x63  = _t(fun _(*1148*) pos_ -> let rec _x66 _x65  = _t(function
 | 1149 ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore(_x65);_wv0);_wv0);_wv0))
 | _(*1152*) ->
 (fun pos_ -> let _x67 _x16  = _t(fun _(*1156*) pos_ -> let _x68 _x15  = _t(fun _(*1159*) pos_ -> let _x70 _x69  = _t(fun _(*1162*) pos_ -> Yk_delay(_x66 (ignore(ignore((_wv0));_wv0);_wv0) ,_x15)) in _t(fun _(*1160*) pos_ -> Yk_delay(_x70 ((_wv0)) ,_x16))) in _t(fun _(*1157*) pos_ -> _x68 (pos_) )) in _t(fun _(*1153*) pos_ -> _x67 (pos_) ))) in _x66 (_wv0) ) in _t(fun _(*1143*) pos_ -> Yk_delay(_x64 ((_wv0)) ,_x11))) in _t(fun _(*1141*) pos_ -> Yk_delay(_x62 ((_wv0)) ,_x12))) in _t(fun _(*1138*) pos_ -> _x60 (pos_) )) in _t(fun _(*1134*) pos_ -> _x59 (pos_) ))
 | _(*1170*) ->
 (fun pos_ -> let _x56 _x55  = _t(function
 | 1173 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1177*) ->
 (fun pos_ -> let _x58 _x57  = _t(function
 | 1180 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1201*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1178*) pos_ -> _x58 (()) ))) in _t(fun _(*1171*) pos_ -> _x56 (()) ))),_x54))
let _x105 =
 (fun _(*pos*) (_,_x80)(*arg of args*) -> (_t(function
 | 1212 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1217 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1232 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1237 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1242 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1248 ->
 (fun pos_ -> let _x97 _x25  = _t(fun _(*1251*) pos_ -> let _x99 _x98  = _t(fun _(*1260*) pos_ -> let _x102 _x24  = _t(fun _(*1263*) pos_ -> let _x104 _x103  = _t(fun _(*1266*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x24)) in _t(fun _(*1264*) pos_ -> Yk_delay(_x104 ((_wv0)) ,_x25))) in _t(fun _(*1261*) pos_ -> _x102 (pos_) )) in _t(fun _(*1252*) pos_ -> let _x100 _x23  = _t(function
 | 1258 ->
 (fun pos_ -> Yk_when(_x23>=1))
 | _(*1259*) ->
 (fun pos_ -> _x99 (ignore((_wv0));_wv0) )) in _t(fun _(*1253*) pos_ -> let rec _x101 _x23  = _t(function
 | 1254 ->
 (fun pos_ -> _x100 (_x23) )
 | _(*1256*) ->
 (fun pos_ -> _x101 (_x23+1) )) in _x101 (0) ))) in _t(fun _(*1249*) pos_ -> _x97 (pos_) ))
 | 1273 ->
 (fun pos_ -> let _x89 _x30  = _t(fun _(*1276*) pos_ -> let _x91 _x90  = _t(fun _(*1285*) pos_ -> let _x94 _x29  = _t(fun _(*1288*) pos_ -> let _x96 _x95  = _t(fun _(*1291*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x29)) in _t(fun _(*1289*) pos_ -> Yk_delay(_x96 ((_wv0)) ,_x30))) in _t(fun _(*1286*) pos_ -> _x94 (pos_) )) in _t(fun _(*1277*) pos_ -> let _x92 _x28  = _t(function
 | 1283 ->
 (fun pos_ -> Yk_when(_x28>=1))
 | _(*1284*) ->
 (fun pos_ -> _x91 (ignore((_wv0));_wv0) )) in _t(fun _(*1278*) pos_ -> let rec _x93 _x28  = _t(function
 | 1279 ->
 (fun pos_ -> _x92 (_x28) )
 | _(*1281*) ->
 (fun pos_ -> _x93 (_x28+1) )) in _x93 (0) ))) in _t(fun _(*1274*) pos_ -> _x89 (pos_) ))
 | 1297 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1302 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1307 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1312 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1317 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1323 ->
 (fun pos_ -> let _x85 _x34  = _t(fun _(*1327*) pos_ -> let _x86 _x33  = _t(fun _(*1330*) pos_ -> let _x88 _x87  = _t(fun _(*1333*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x33)) in _t(fun _(*1331*) pos_ -> Yk_delay(_x88 ((_wv0)) ,_x34))) in _t(fun _(*1328*) pos_ -> _x86 (pos_) )) in _t(fun _(*1324*) pos_ -> _x85 (pos_) ))
 | _(*1338*) ->
 (fun pos_ -> let _x81 _x38  = _t(fun _(*1342*) pos_ -> let _x82 _x37  = _t(fun _(*1345*) pos_ -> let _x84 _x83  = _t(fun _(*1348*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x37)) in _t(fun _(*1346*) pos_ -> Yk_delay(_x84 ((_wv0)) ,_x38))) in _t(fun _(*1343*) pos_ -> _x82 (pos_) )) in _t(fun _(*1339*) pos_ -> _x81 (pos_) ))),_x80))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a59 = fun p v -> _d_and_push 1148 p (_p 1147 p (_p 1146 p (_p 1145 p (_p 1144 p (_ddelay 1143 p (_d_and_push 1142 p (_ddelay 1141 p (_d_and_push 1140 p (_d 1138 p (_d 1137 p (v)))))))))));;
let __a41 = fun p v -> _p 1350 p (_p 1349 p (_ddelay 1348 p (_p 1347 p (_ddelay 1346 p (_d_and_push 1345 p (_d 1343 p (_d 1342 p (v))))))));;
let __a46 = _p 1320;;
let __a63 = _p 1150;;
let __a85 = _p 1040;;
let __a81 = _d 1281;;
let __a70 = _p 1101;;
let __a60 = _p 1091;;
let __a22 = _d 1232;;
let __p51 = _dwhen 1113;;
let __a11 = _d 1062;;
let __a35 = _d 1173;;
let __a117 = _p 1215;;
let __a68 = _p 1045;;
let __a119 = _p 1096;;
let __a23 = _d 1237;;
let __a18 = _d 1127;;
let __a12 = _d 1067;;
let __a4 = _d 1018;;
let __p82 = _dnext 1284;;
let __a114 = fun p v -> _p 1193 p (_p 1192 p (v));;
let __a91 = fun p v -> _p 1184 p (_p 1183 p (v));;
let __a87 = fun p v -> _p 1293 p (_p 1292 p (_ddelay 1291 p (_p 1290 p (_ddelay 1289 p (_d_and_push 1288 p (_d 1286 p (_d 1285 p (v))))))));;
let __a102 = fun p v -> _p 1188 p (_p 1187 p (_p 1186 p (v)));;
let __a2 = fun p v -> _p 1030 p (_x79 p (v));;
let __a116 = _p 1270;;
let __a77 = _p 1220;;
let __a56 = _p 1050;;
let __a39 = _d 1180;;
let __a32 = _d 1020;;
let __a120 = fun p v -> _p 1196 p (_p 1195 p (v));;
let __a99 = _p 1222;;
let __p83 = _dwhen 1283;;
let __a26 = _d 1302;;
let __a24 = _d 1242;;
let __a13 = _d 1072;;
let __a93 = _p 1224;;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a86 = _p 1055;;
let __a121 = _p 1226;;
let __a30 = fun p v -> _d 1339 p (_d 1338 p (v));;
let __a3 = fun p v -> _p 1210 p (_x105 p (v));;
let __a64 = fun p v -> _p 1167 p (_d_and_push 1149 p (v));;
let __a27 = _d 1307;;
let __a25 = _d 1297;;
let __a76 = _p 1337;;
let __a111 = fun p v -> _p 1268 p (_p 1267 p (_ddelay 1266 p (_p 1265 p (_ddelay 1264 p (_d_and_push 1263 p (_d 1261 p (_d 1260 p (v))))))));;
let __a100 = _p 1228;;
let __p33 = _dnext 1023;;
let __a73 = _p 1169;;
let __a66 = fun p v -> _p 1335 p (_p 1334 p (_ddelay 1333 p (_p 1332 p (_ddelay 1331 p (_d_and_push 1330 p (_d 1328 p (_d 1327 p (v))))))));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x53 p (v)));;
let __a106 = _p 1230;;
let __a57 = _p 1060;;
let __a43 = _p 1010;;
let __a40 = _d 1201;;
let __p34 = _dwhen 1022;;
let __a28 = _d 1312;;
let __a5 = _d 1032;;
let __a14 = _d 1083;;
let __a44 = _p 1013;;
let __a98 = _d 1254;;
let __a62 = _p 1235;;
let __a65 = _p 1125;;
let __a74 = _p 1065;;
let __a103 = _d 1256;;
let __a61 = _p 1176;;
let __a29 = _d 1317;;
let __a6 = _d 1037;;
let __a15 = _d 1088;;
let __a80 = fun p v -> _p 1205 p (_p 1204 p (v));;
let __a72 = fun p v -> _d 1153 p (_d 1152 p (v));;
let __a38 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a19 = fun p v -> _d 1171 p (_d 1170 p (v));;
let __p104 = _dnext 1259;;
let __a58 = fun p v -> _d 1324 p (_d 1323 p (v));;
let __a95 = _p 1130;;
let __a89 = _p 1300;;
let __a88 = _p 1240;;
let __a69 = _p 1070;;
let __a37 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a7 = _d 1042;;
let __a47 = _p 1352;;
let __a20 = _d 1212;;
let __a16 = _d 1093;;
let __a53 = _p 1353;;
let __a112 = _p 1245;;
let __a101 = _p 1305;;
let __a97 = _p 1295;;
let __a78 = _p 1075;;
let __a90 = _p 1025;;
let __a42 = fun p v -> _d 1108 p (_d 1107 p (_d 1106 p (_d 1104 p (_d 1103 p (v)))));;
let __a71 = fun p v -> _d 1278 p (_d 1277 p (_d 1276 p (_d 1274 p (_d 1273 p (v)))));;
let __a113 = _p 1077;;
let __a108 = _p 1027;;
let __p105 = _dwhen 1258;;
let __a8 = _d 1047;;
let __a54 = fun p v -> _p 1123 p (_p 1122 p (_ddelay 1121 p (_p 1120 p (_ddelay 1119 p (_d_and_push 1118 p (_d 1116 p (_d 1115 p (v))))))));;
let __a21 = _d 1217;;
let __a17 = _d 1098;;
let __a122 = _p 1079;;
let __a67 = _p 1029;;
let __a45 = _d 1109;;
let __a92 = fun p v -> _d 1253 p (_d 1252 p (_d 1251 p (_d 1249 p (_d 1248 p (v)))));;
let __a31 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a109 = _p 1190;;
let __a94 = _p 1310;;
let __a118 = _p 1081;;
let __a48 = fun p v -> _d 1134 p (_d 1133 p (v));;
let __a49 = _d 1111;;
let __a9 = _d 1052;;
let __a107 = _p 1315;;
let __a52 = _p 1035;;
let __a79 = _p 1086;;
let __a10 = _d 1057;;
let __a96 = _p 1208;;
let __a36 = fun p v -> _d 1178 p (_d 1177 p (v));;
let __a115 = _p 1199;;
let __a75 = _d 1279;;
let __a55 = _p 1209;;
let __a84 = fun p v -> _p 1166 p (_p 1165 p (_p 1164 p (_p 1163 p (_ddelay 1162 p (_p 1161 p (_ddelay 1160 p (_d_and_push 1159 p (_d 1157 p (_d 1156 p (v))))))))));;
let __p50 = _dnext 1114;;
let __a110 = fun p v -> _p 1197 p (_p 1189 p (v));;
let __binder0 = __default_ret;;
let __binder1 = _m 1002;;
let __binder2 = _m 1009;;
let __binder3 = _m 1206;;
let __binder4 = _m 1185;;
let __binder5 = _m 1194;;
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
(383, [EatInstr(101,405)]);
(0, [ASimpleCont2Instr(275,__binder0,12);ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(104,406)]);
(1, [EatInstr(127,13);EatInstr(126,13);EatInstr(125,13);EatInstr(124,13);EatInstr(123,13);EatInstr(122,13);EatInstr(121,13);EatInstr(120,13);EatInstr(119,13);EatInstr(118,13);EatInstr(117,13);EatInstr(116,13);EatInstr(115,13);EatInstr(114,13);EatInstr(113,13);EatInstr(112,13);EatInstr(111,13);EatInstr(110,13);EatInstr(109,13);EatInstr(108,13);EatInstr(107,13);EatInstr(106,13);EatInstr(105,13);EatInstr(104,13);EatInstr(103,13);EatInstr(102,13);EatInstr(101,13);EatInstr(100,13);EatInstr(99,13);EatInstr(98,13);EatInstr(97,13);EatInstr(96,13);EatInstr(95,13);EatInstr(94,13);EatInstr(93,13);EatInstr(92,13);EatInstr(91,13);EatInstr(90,13);EatInstr(89,13);EatInstr(88,13);EatInstr(87,13);EatInstr(86,13);EatInstr(85,13);EatInstr(84,13);EatInstr(83,13);EatInstr(82,13);EatInstr(81,13);EatInstr(80,13);EatInstr(79,13);EatInstr(78,13);EatInstr(77,13);EatInstr(76,13);EatInstr(75,13);EatInstr(74,13);EatInstr(73,13);EatInstr(72,13);EatInstr(71,13);EatInstr(70,13);EatInstr(69,13);EatInstr(68,13);EatInstr(67,13);EatInstr(66,13);EatInstr(65,13);EatInstr(64,13);EatInstr(63,13);EatInstr(62,13);EatInstr(61,13);EatInstr(60,13);EatInstr(59,13);EatInstr(58,13);EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13);EatInstr(47,13);EatInstr(46,13);EatInstr(45,13);EatInstr(44,13);EatInstr(43,13);EatInstr(42,13);EatInstr(41,13);EatInstr(40,13);EatInstr(39,13);EatInstr(38,13);EatInstr(37,13);EatInstr(36,13);EatInstr(35,13);EatInstr(34,13);EatInstr(33,13);EatInstr(32,13);EatInstr(31,13);EatInstr(30,13);EatInstr(29,13);EatInstr(28,13);EatInstr(27,13);EatInstr(26,13);EatInstr(25,13);EatInstr(24,13);EatInstr(23,13);EatInstr(22,13);EatInstr(21,13);EatInstr(20,13);EatInstr(19,13);EatInstr(18,13);EatInstr(17,13);EatInstr(16,13);EatInstr(15,13);EatInstr(14,13);EatInstr(13,13);EatInstr(12,13);EatInstr(11,13);EatInstr(10,13);EatInstr(9,13);EatInstr(8,13);EatInstr(7,13);EatInstr(6,13);EatInstr(5,13);EatInstr(4,13);EatInstr(3,13);EatInstr(2,13);EatInstr(1,13)]);
(385, [EatInstr(114,407)]);
(2, [EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14)]);
(386, [AAction2Instr(__a84,284)]);
(3, [EatInstr(255,15);EatInstr(254,15);EatInstr(253,15);EatInstr(252,15);EatInstr(251,15);EatInstr(250,15);EatInstr(249,15);EatInstr(248,15);EatInstr(247,15);EatInstr(246,15);EatInstr(245,15);EatInstr(244,15);EatInstr(243,15);EatInstr(242,15);EatInstr(241,15);EatInstr(240,15);EatInstr(239,15);EatInstr(238,15);EatInstr(237,15);EatInstr(236,15);EatInstr(235,15);EatInstr(234,15);EatInstr(233,15);EatInstr(232,15);EatInstr(231,15);EatInstr(230,15);EatInstr(229,15);EatInstr(228,15);EatInstr(227,15);EatInstr(226,15);EatInstr(225,15);EatInstr(224,15);EatInstr(223,15);EatInstr(222,15);EatInstr(221,15);EatInstr(220,15);EatInstr(219,15);EatInstr(218,15);EatInstr(217,15);EatInstr(216,15);EatInstr(215,15);EatInstr(214,15);EatInstr(213,15);EatInstr(212,15);EatInstr(211,15);EatInstr(210,15);EatInstr(209,15);EatInstr(208,15);EatInstr(207,15);EatInstr(206,15);EatInstr(205,15);EatInstr(204,15);EatInstr(203,15);EatInstr(202,15);EatInstr(201,15);EatInstr(200,15);EatInstr(199,15);EatInstr(198,15);EatInstr(197,15);EatInstr(196,15);EatInstr(195,15);EatInstr(194,15);EatInstr(193,15);EatInstr(192,15);EatInstr(191,15);EatInstr(190,15);EatInstr(189,15);EatInstr(188,15);EatInstr(187,15);EatInstr(186,15);EatInstr(185,15);EatInstr(184,15);EatInstr(183,15);EatInstr(182,15);EatInstr(181,15);EatInstr(180,15);EatInstr(179,15);EatInstr(178,15);EatInstr(177,15);EatInstr(176,15);EatInstr(175,15);EatInstr(174,15);EatInstr(173,15);EatInstr(172,15);EatInstr(171,15);EatInstr(170,15);EatInstr(169,15);EatInstr(168,15);EatInstr(167,15);EatInstr(166,15);EatInstr(165,15);EatInstr(164,15);EatInstr(163,15);EatInstr(162,15);EatInstr(161,15);EatInstr(160,15);EatInstr(159,15);EatInstr(158,15);EatInstr(157,15);EatInstr(156,15);EatInstr(155,15);EatInstr(154,15);EatInstr(153,15);EatInstr(152,15);EatInstr(151,15);EatInstr(150,15);EatInstr(149,15);EatInstr(148,15);EatInstr(147,15);EatInstr(146,15);EatInstr(145,15);EatInstr(144,15);EatInstr(143,15);EatInstr(142,15);EatInstr(141,15);EatInstr(140,15);EatInstr(139,15);EatInstr(138,15);EatInstr(137,15);EatInstr(136,15);EatInstr(135,15);EatInstr(134,15);EatInstr(133,15);EatInstr(132,15);EatInstr(131,15);EatInstr(130,15);EatInstr(129,15);EatInstr(128,15);EatInstr(0,15);EatInstr(127,15);EatInstr(126,15);EatInstr(125,15);EatInstr(124,15);EatInstr(123,15);EatInstr(122,15);EatInstr(121,15);EatInstr(120,15);EatInstr(119,15);EatInstr(118,15);EatInstr(117,15);EatInstr(116,15);EatInstr(115,15);EatInstr(114,15);EatInstr(113,15);EatInstr(112,15);EatInstr(111,15);EatInstr(110,15);EatInstr(109,15);EatInstr(108,15);EatInstr(107,15);EatInstr(106,15);EatInstr(105,15);EatInstr(104,15);EatInstr(103,15);EatInstr(102,15);EatInstr(101,15);EatInstr(100,15);EatInstr(99,15);EatInstr(98,15);EatInstr(97,15);EatInstr(96,15);EatInstr(95,15);EatInstr(94,15);EatInstr(93,15);EatInstr(92,15);EatInstr(91,15);EatInstr(90,15);EatInstr(89,15);EatInstr(88,15);EatInstr(87,15);EatInstr(86,15);EatInstr(85,15);EatInstr(84,15);EatInstr(83,15);EatInstr(82,15);EatInstr(81,15);EatInstr(80,15);EatInstr(79,15);EatInstr(78,15);EatInstr(77,15);EatInstr(76,15);EatInstr(75,15);EatInstr(74,15);EatInstr(73,15);EatInstr(72,15);EatInstr(71,15);EatInstr(70,15);EatInstr(69,15);EatInstr(68,15);EatInstr(67,15);EatInstr(66,15);EatInstr(65,15);EatInstr(64,15);EatInstr(63,15);EatInstr(62,15);EatInstr(61,15);EatInstr(60,15);EatInstr(59,15);EatInstr(58,15);EatInstr(57,15);EatInstr(56,15);EatInstr(55,15);EatInstr(54,15);EatInstr(53,15);EatInstr(52,15);EatInstr(51,15);EatInstr(50,15);EatInstr(49,15);EatInstr(48,15);EatInstr(47,15);EatInstr(46,15);EatInstr(45,15);EatInstr(44,15);EatInstr(43,15);EatInstr(42,15);EatInstr(41,15);EatInstr(40,15);EatInstr(39,15);EatInstr(38,15);EatInstr(37,15);EatInstr(36,15);EatInstr(35,15);EatInstr(34,15);EatInstr(33,15);EatInstr(32,15);EatInstr(31,15);EatInstr(30,15);EatInstr(29,15);EatInstr(28,15);EatInstr(27,15);EatInstr(26,15);EatInstr(25,15);EatInstr(24,15);EatInstr(23,15);EatInstr(22,15);EatInstr(21,15);EatInstr(20,15);EatInstr(19,15);EatInstr(18,15);EatInstr(17,15);EatInstr(16,15);EatInstr(15,15);EatInstr(14,15);EatInstr(13,15);EatInstr(12,15);EatInstr(11,15);EatInstr(10,15);EatInstr(9,15);EatInstr(8,15);EatInstr(7,15);EatInstr(6,15);EatInstr(5,15);EatInstr(4,15);EatInstr(3,15);EatInstr(2,15);EatInstr(1,15)]);
(387, [AAction2Instr(__a85,219)]);
(4, [AAction2Instr(__a0,16)]);
(388, [AAction2Instr(__a86,219)]);
(5, [EatInstr(0,17)]);
(389, [EatInstr(100,408)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,18)]);
(390, [EatInstr(97,409)]);
(7, [EatInstr(127,20);EatInstr(126,20);EatInstr(125,20);EatInstr(124,20);EatInstr(123,20);EatInstr(122,20);EatInstr(121,20);EatInstr(120,20);EatInstr(119,20);EatInstr(118,20);EatInstr(117,20);EatInstr(116,20);EatInstr(115,20);EatInstr(114,20);EatInstr(113,20);EatInstr(112,20);EatInstr(111,20);EatInstr(110,20);EatInstr(109,20);EatInstr(108,20);EatInstr(107,20);EatInstr(106,20);EatInstr(105,20);EatInstr(104,20);EatInstr(103,20);EatInstr(102,20);EatInstr(101,20);EatInstr(100,20);EatInstr(99,20);EatInstr(98,20);EatInstr(97,20);EatInstr(96,20);EatInstr(95,20);EatInstr(94,20);EatInstr(93,20);EatInstr(92,20);EatInstr(91,20);EatInstr(90,20);EatInstr(89,20);EatInstr(88,20);EatInstr(87,20);EatInstr(86,20);EatInstr(85,20);EatInstr(84,20);EatInstr(83,20);EatInstr(82,20);EatInstr(81,20);EatInstr(80,20);EatInstr(79,20);EatInstr(78,20);EatInstr(77,20);EatInstr(76,20);EatInstr(75,20);EatInstr(74,20);EatInstr(73,20);EatInstr(72,20);EatInstr(71,20);EatInstr(70,20);EatInstr(69,20);EatInstr(68,20);EatInstr(67,20);EatInstr(66,20);EatInstr(65,20);EatInstr(64,20);EatInstr(63,20);EatInstr(62,20);EatInstr(61,20);EatInstr(60,20);EatInstr(59,20);EatInstr(58,20);EatInstr(57,20);EatInstr(56,20);EatInstr(55,20);EatInstr(54,20);EatInstr(53,20);EatInstr(52,20);EatInstr(51,20);EatInstr(50,20);EatInstr(49,20);EatInstr(48,20);EatInstr(47,20);EatInstr(46,20);EatInstr(44,20);EatInstr(43,20);EatInstr(42,20);EatInstr(41,20);EatInstr(40,20);EatInstr(39,20);EatInstr(38,20);EatInstr(37,20);EatInstr(36,20);EatInstr(35,20);EatInstr(34,20);EatInstr(33,20);EatInstr(32,20);EatInstr(31,20);EatInstr(30,20);EatInstr(29,20);EatInstr(28,20);EatInstr(27,20);EatInstr(26,20);EatInstr(25,20);EatInstr(24,20);EatInstr(23,20);EatInstr(22,20);EatInstr(21,20);EatInstr(20,20);EatInstr(19,20);EatInstr(18,20);EatInstr(17,20);EatInstr(16,20);EatInstr(15,20);EatInstr(14,20);EatInstr(13,20);EatInstr(12,20);EatInstr(11,20);EatInstr(10,20);EatInstr(9,20);EatInstr(8,20);EatInstr(7,20);EatInstr(6,20);EatInstr(5,20);EatInstr(4,20);EatInstr(3,20);EatInstr(2,20);EatInstr(1,20)]);
(391, [EatInstr(115,410)]);
(8, [EatInstr(105,22);EatInstr(97,21)]);
(392, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,411)]);
(9, [EatInstr(112,23)]);
(393, [ASimpleCont2Instr(272,__binder3,412);ACallInstr3(__default_call,9)]);
(10, [AAction2Instr(__a2,24)]);
(394, [AAction2Instr(__a87,413)]);
(11, [AAction2Instr(__a3,25)]);
(395, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,414)]);
(12, [ALookaheadInstr(false,CfgLA (3,266),26);RCompleteInstr2(275,nullable_eof)]);
(396, [EatInstr(115,415)]);
(13, [CompleteInstr(264)]);
(397, [EatInstr(117,416)]);
(14, [CompleteInstr(265)]);
(398, [EatInstr(101,417)]);
(15, [CompleteInstr(266)]);
(399, [EatInstr(120,418)]);
(16, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder1,27)]);
(400, [AAction2Instr(__a88,213)]);
(17, [CompleteInstr(268)]);
(401, [EatInstr(117,419)]);
(18, [AAction2Instr(__a4,29);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,28)]);
(402, [AAction2Instr(__a89,213)]);
(403, [EatInstr(101,420)]);
(20, [ALookaheadInstr(false,CfgLA (1,264),30);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,20)]);
(404, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,421)]);
(21, [EatInstr(100,31)]);
(405, [EatInstr(108,422)]);
(22, [EatInstr(110,32)]);
(406, [EatInstr(101,423)]);
(23, [EatInstr(114,33)]);
(407, [AAction2Instr(__a90,424)]);
(24, [EatInstr(114,35);EatInstr(101,34);AAction2Instr(__a19,50);AAction2Instr(__a18,49);AAction2Instr(__a17,48);AAction2Instr(__a16,47);AAction2Instr(__a15,46);AAction2Instr(__a14,45);AAction2Instr(__a13,44);AAction2Instr(__a12,43);AAction2Instr(__a11,42);AAction2Instr(__a10,41);AAction2Instr(__a9,40);AAction2Instr(__a8,39);AAction2Instr(__a7,38);AAction2Instr(__a6,37);AAction2Instr(__a5,36)]);
(408, [EatInstr(121,425)]);
(25, [EatInstr(45,51);AAction2Instr(__a30,62);AAction2Instr(__a29,61);AAction2Instr(__a28,60);AAction2Instr(__a27,59);AAction2Instr(__a26,58);AAction2Instr(__a25,57);AAction2Instr(__a24,56);AAction2Instr(__a23,55);AAction2Instr(__a22,54);AAction2Instr(__a21,53);AAction2Instr(__a20,52)]);
(409, [EatInstr(99,426)]);
(26, [CompleteInstr(275)]);
(410, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,427)]);
(27, [AAction2Instr(__a31,63)]);
(411, [AAction2Instr(__a91,428)]);
(28, [AAction2Instr(__a32,18)]);
(412, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,429)]);
(29, [AWhenInstr3(__p34,__p33,64)]);
(413, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,430)]);
(30, [CompleteInstr(270)]);
(414, [AAction2Instr(__a92,431)]);
(31, [EatInstr(100,65)]);
(415, [EatInstr(105,432)]);
(32, [EatInstr(108,66)]);
(416, [EatInstr(110,433)]);
(33, [EatInstr(101,67)]);
(417, [EatInstr(103,434)]);
(34, [EatInstr(120,68)]);
(418, [AAction2Instr(__a93,435)]);
(35, [EatInstr(102,69)]);
(419, [EatInstr(108,436)]);
(36, [EatInstr(100,70)]);
(420, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,437)]);
(37, [EatInstr(112,71)]);
(421, [AAction2Instr(__a94,213)]);
(38, [EatInstr(100,72)]);
(422, [EatInstr(115,438)]);
(39, [EatInstr(108,73)]);
(423, [EatInstr(97,439)]);
(40, [EatInstr(97,74)]);
(424, [CompleteInstr(271)]);
(41, [EatInstr(119,75)]);
(425, [EatInstr(112,440)]);
(42, [EatInstr(100,76)]);
(426, [EatInstr(116,441)]);
(43, [EatInstr(99,77)]);
(427, [AAction2Instr(__a95,219)]);
(44, [EatInstr(116,78)]);
(428, [ASimpleCont2Instr(271,__binder4,442);ACallInstr3(__default_call,8)]);
(45, [EatInstr(112,79)]);
(429, [AAction2Instr(__a96,219)]);
(46, [EatInstr(112,80)]);
(430, [AAction2Instr(__a97,213)]);
(47, [EatInstr(115,81)]);
(431, [AAction2Instr(__a98,444);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,443)]);
(48, [EatInstr(101,82)]);
(432, [EatInstr(116,445)]);
(49, [EatInstr(112,83)]);
(433, [AAction2Instr(__a99,435)]);
(50, [AAction2Instr(__a36,85);AAction2Instr(__a35,84)]);
(434, [EatInstr(45,446);AAction2Instr(__a100,435)]);
(51, [EatInstr(117,88);EatInstr(114,87);EatInstr(99,86)]);
(435, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,447)]);
(52, [EatInstr(45,89)]);
(436, [EatInstr(97,448)]);
(53, [EatInstr(45,90)]);
(437, [AAction2Instr(__a101,213)]);
(54, [EatInstr(45,91)]);
(438, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,449)]);
(55, [EatInstr(45,92)]);
(439, [EatInstr(100,450)]);
(56, [EatInstr(45,93)]);
(440, [EatInstr(103,451)]);
(57, [EatInstr(45,94)]);
(441, [EatInstr(105,452)]);
(58, [EatInstr(45,95)]);
(442, [AAction2Instr(__a102,453)]);
(59, [EatInstr(45,96)]);
(443, [AAction2Instr(__a103,431)]);
(60, [EatInstr(45,97)]);
(444, [AWhenInstr3(__p105,__p104,454)]);
(61, [EatInstr(45,98)]);
(445, [EatInstr(105,455)]);
(62, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,99)]);
(446, [EatInstr(115,456)]);
(63, [AAction2Instr(__a38,101);AAction2Instr(__a37,100)]);
(64, [ALookaheadInstr(false,CfgLA (1,264),102)]);
(447, [AAction2Instr(__a106,213)]);
(448, [EatInstr(114,457)]);
(65, [EatInstr(45,103)]);
(449, [AAction2Instr(__a107,213)]);
(66, [EatInstr(105,104)]);
(450, [AAction2Instr(__a108,424)]);
(67, [EatInstr(99,105)]);
(451, [EatInstr(101,458)]);
(68, [EatInstr(101,106)]);
(452, [EatInstr(111,459)]);
(69, [EatInstr(99,107)]);
(453, [AAction2Instr(__a110,461);AAction2Instr(__a109,460)]);
(70, [EatInstr(111,108)]);
(454, [AAction2Instr(__a111,462)]);
(71, [EatInstr(114,109)]);
(455, [EatInstr(118,463)]);
(72, [EatInstr(101,110)]);
(456, [EatInstr(116,464)]);
(73, [EatInstr(105,111)]);
(457, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,465)]);
(74, [EatInstr(116,112)]);
(458, [EatInstr(110,466)]);
(75, [EatInstr(114,113)]);
(459, [EatInstr(110,467)]);
(76, [EatInstr(105,114)]);
(460, [EatInstr(44,468)]);
(77, [EatInstr(111,115)]);
(461, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,469)]);
(78, [EatInstr(114,116)]);
(462, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,470)]);
(79, [EatInstr(114,117)]);
(463, [EatInstr(101,471)]);
(80, [EatInstr(114,118)]);
(464, [EatInstr(114,472)]);
(81, [EatInstr(116,119)]);
(465, [AAction2Instr(__a112,213)]);
(82, [EatInstr(120,120)]);
(466, [EatInstr(45,473);AAction2Instr(__a113,474)]);
(83, [EatInstr(114,121)]);
(467, [EatInstr(115,475)]);
(84, [EatInstr(105,122)]);
(468, [AAction2Instr(__a114,476)]);
(85, [AAction2Instr(__a40,124);AAction2Instr(__a39,123)]);
(469, [AAction2Instr(__a115,219)]);
(86, [EatInstr(111,125)]);
(470, [AAction2Instr(__a116,213)]);
(87, [EatInstr(111,126)]);
(471, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,477)]);
(88, [EatInstr(110,127)]);
(472, [EatInstr(105,478)]);
(89, [EatInstr(99,128)]);
(473, [EatInstr(115,479)]);
(90, [EatInstr(98,129)]);
(474, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,480)]);
(91, [EatInstr(111,130)]);
(475, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,481)]);
(92, [EatInstr(105,131)]);
(476, [ASimpleCont2Instr(271,__binder5,482);ACallInstr3(__default_call,8)]);
(93, [EatInstr(105,132)]);
(477, [AAction2Instr(__a117,213)]);
(94, [EatInstr(108,133)]);
(478, [EatInstr(99,483)]);
(95, [EatInstr(110,134)]);
(479, [EatInstr(99,484)]);
(96, [EatInstr(110,135)]);
(480, [AAction2Instr(__a118,219)]);
(97, [EatInstr(99,136)]);
(481, [AAction2Instr(__a119,219)]);
(98, [EatInstr(118,137)]);
(482, [AAction2Instr(__a120,453)]);
(99, [AAction2Instr(__a41,138)]);
(483, [EatInstr(116,485)]);
(100, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder2,139)]);
(484, [EatInstr(97,486)]);
(101, [ACallInstr3(__default_call,12);ASimpleCont2Instr(275,__binder0,140)]);
(485, [AAction2Instr(__a121,435)]);
(102, [CompleteInstr(269)]);
(486, [EatInstr(110,487)]);
(103, [EatInstr(108,141)]);
(487, [EatInstr(110,488)]);
(104, [EatInstr(110,142)]);
(488, [EatInstr(101,489)]);
(105, [EatInstr(101,143)]);
(489, [EatInstr(114,490)]);
(106, [EatInstr(99,144)]);
(490, [EatInstr(108,491)]);
(107, [AAction2Instr(__a42,145)]);
(491, [EatInstr(101,492)]);
(108, [EatInstr(116,146)]);
(492, [EatInstr(115,493)]);
(109, [EatInstr(101,147)]);
(493, [EatInstr(115,494)]);
(110, [EatInstr(115,148)]);
(494, [AAction2Instr(__a122,474)]);
(111, [EatInstr(102,149)]);
(112, [EatInstr(116,150)]);
(113, [EatInstr(97,151)]);
(114, [EatInstr(115,152)]);
(115, [EatInstr(109,153)]);
(116, [EatInstr(97,154)]);
(117, [EatInstr(105,155)]);
(118, [EatInstr(105,156)]);
(119, [EatInstr(114,157)]);
(120, [EatInstr(116,158)]);
(121, [EatInstr(105,159)]);
(122, [EatInstr(110,160)]);
(123, [EatInstr(116,161)]);
(124, [EatInstr(97,162)]);
(125, [EatInstr(117,163)]);
(126, [EatInstr(111,164)]);
(127, [EatInstr(114,165)]);
(128, [EatInstr(97,166)]);
(129, [EatInstr(97,167)]);
(130, [EatInstr(110,168)]);
(131, [EatInstr(110,169)]);
(132, [EatInstr(110,170)]);
(133, [EatInstr(111,171)]);
(134, [EatInstr(111,172)]);
(135, [EatInstr(101,173)]);
(136, [EatInstr(104,174)]);
(137, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,175)]);
(138, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,176)]);
(139, [AAction2Instr(__a43,63)]);
(140, [AAction2Instr(__a44,177)]);
(141, [EatInstr(114,178)]);
(142, [EatInstr(101,179)]);
(143, [EatInstr(100,180)]);
(144, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,181)]);
(145, [AAction2Instr(__a45,183);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,182)]);
(146, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,184)]);
(147, [EatInstr(99,185)]);
(148, [EatInstr(117,186)]);
(149, [EatInstr(116,187)]);
(150, [EatInstr(114,188)]);
(151, [EatInstr(112,189)]);
(152, [EatInstr(112,190)]);
(153, [EatInstr(112,191)]);
(154, [EatInstr(110,192)]);
(155, [EatInstr(110,193)]);
(156, [EatInstr(110,194)]);
(157, [EatInstr(105,195)]);
(158, [EatInstr(114,196)]);
(159, [EatInstr(110,197)]);
(160, [EatInstr(102,198)]);
(161, [EatInstr(114,199)]);
(162, [EatInstr(110,200)]);
(163, [EatInstr(110,201)]);
(164, [EatInstr(116,202)]);
(165, [EatInstr(111,203)]);
(166, [EatInstr(115,204)]);
(167, [EatInstr(99,205)]);
(168, [EatInstr(108,206)]);
(169, [EatInstr(108,207)]);
(170, [EatInstr(108,208)]);
(171, [EatInstr(111,209)]);
(172, [EatInstr(45,210)]);
(173, [EatInstr(119,211)]);
(174, [EatInstr(101,212)]);
(175, [AAction2Instr(__a46,213)]);
(176, [AAction2Instr(__a47,213)]);
(177, [CompleteInstr(267)]);
(178, [EatInstr(49,214)]);
(179, [EatInstr(45,215)]);
(180, [EatInstr(101,216)]);
(181, [AAction2Instr(__a48,217)]);
(182, [AAction2Instr(__a49,145)]);
(183, [AWhenInstr3(__p51,__p50,218)]);
(184, [AAction2Instr(__a52,219)]);
(185, [EatInstr(101,220)]);
(186, [EatInstr(103,221)]);
(187, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,222)]);
(188, [EatInstr(105,223)]);
(189, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,224)]);
(190, [EatInstr(97,225)]);
(191, [EatInstr(105,226)]);
(192, [EatInstr(115,227)]);
(193, [EatInstr(116,228)]);
(194, [EatInstr(116,229)]);
(195, [EatInstr(112,230)]);
(196, [EatInstr(97,231)]);
(197, [EatInstr(116,232)]);
(198, [EatInstr(111,233)]);
(199, [EatInstr(97,234)]);
(200, [EatInstr(97,235)]);
(201, [EatInstr(116,236)]);
(202, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,237)]);
(203, [EatInstr(108,238)]);
(204, [EatInstr(101,239)]);
(205, [EatInstr(107,240)]);
(206, [EatInstr(121,241)]);
(207, [EatInstr(105,242)]);
(208, [EatInstr(105,243)]);
(209, [EatInstr(107,244)]);
(210, [EatInstr(99,245)]);
(211, [EatInstr(45,246)]);
(212, [EatInstr(99,247)]);
(213, [AAction2Instr(__a53,248)]);
(214, [EatInstr(45,249)]);
(215, [EatInstr(114,250)]);
(216, [EatInstr(110,251)]);
(217, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,252)]);
(218, [AAction2Instr(__a54,253)]);
(219, [AAction2Instr(__a55,254)]);
(220, [EatInstr(100,255)]);
(221, [EatInstr(97,256)]);
(222, [AAction2Instr(__a56,219)]);
(223, [EatInstr(98,257)]);
(224, [AAction2Instr(__a57,219)]);
(225, [EatInstr(116,258)]);
(226, [EatInstr(108,259)]);
(227, [EatInstr(108,260)]);
(228, [EatInstr(45,261)]);
(229, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,262)]);
(230, [EatInstr(45,263)]);
(231, [EatInstr(99,264)]);
(232, [EatInstr(45,265)]);
(233, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,266)]);
(234, [EatInstr(110,267)]);
(235, [EatInstr(108,268)]);
(236, [EatInstr(101,269)]);
(237, [AAction2Instr(__a58,270)]);
(238, [EatInstr(108,271)]);
(239, [EatInstr(45,272)]);
(240, [EatInstr(101,273)]);
(241, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,274)]);
(242, [EatInstr(110,275)]);
(243, [EatInstr(110,276)]);
(244, [EatInstr(97,277)]);
(245, [EatInstr(111,278)]);
(246, [EatInstr(101,279)]);
(247, [EatInstr(107,280)]);
(248, [CompleteInstr(274)]);
(249, [EatInstr(108,281)]);
(250, [EatInstr(101,282)]);
(251, [EatInstr(99,283)]);
(252, [AAction2Instr(__a59,284)]);
(253, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,285)]);
(254, [CompleteInstr(273)]);
(255, [EatInstr(101,286)]);
(256, [EatInstr(114,287)]);
(257, [EatInstr(117,288)]);
(258, [EatInstr(99,289)]);
(259, [EatInstr(101,290)]);
(260, [EatInstr(97,291)]);
(261, [EatInstr(103,292)]);
(262, [AAction2Instr(__a60,219)]);
(263, [EatInstr(108,293)]);
(264, [EatInstr(116,294)]);
(265, [EatInstr(110,295)]);
(266, [AAction2Instr(__a61,219)]);
(267, [EatInstr(115,296)]);
(268, [EatInstr(121,297)]);
(269, [EatInstr(114,298)]);
(270, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,299)]);
(271, [EatInstr(45,300)]);
(272, [EatInstr(105,301)]);
(273, [EatInstr(110,302)]);
(274, [AAction2Instr(__a62,213)]);
(275, [EatInstr(101,303)]);
(276, [EatInstr(101,304)]);
(277, [EatInstr(104,305)]);
(278, [EatInstr(97,306)]);
(279, [EatInstr(110,307)]);
(280, [EatInstr(45,308)]);
(281, [EatInstr(111,309)]);
(282, [EatInstr(103,310)]);
(283, [EatInstr(101,311)]);
(284, [AAction2Instr(__a64,313);AAction2Instr(__a63,312)]);
(285, [AAction2Instr(__a65,219)]);
(286, [EatInstr(110,314)]);
(287, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,315)]);
(288, [EatInstr(116,316)]);
(289, [EatInstr(104,317)]);
(290, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,318)]);
(291, [EatInstr(116,319)]);
(292, [EatInstr(105,320)]);
(293, [EatInstr(97,321)]);
(294, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,322)]);
(295, [EatInstr(112,323)]);
(296, [EatInstr(102,324)]);
(297, [EatInstr(122,325)]);
(298, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,326)]);
(299, [AAction2Instr(__a66,327)]);
(300, [EatInstr(115,328)]);
(301, [EatInstr(110,329)]);
(302, [EatInstr(100,330)]);
(303, [EatInstr(45,331)]);
(304, [EatInstr(45,332)]);
(305, [EatInstr(101,333)]);
(306, [EatInstr(108,334)]);
(307, [EatInstr(103,335)]);
(308, [EatInstr(108,336)]);
(309, [EatInstr(111,337)]);
(310, [EatInstr(117,338)]);
(311, [AAction2Instr(__a67,339)]);
(312, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,340)]);
(313, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,341)]);
(314, [EatInstr(99,342)]);
(315, [AAction2Instr(__a68,219)]);
(316, [EatInstr(101,343)]);
(317, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,344)]);
(318, [AAction2Instr(__a69,219)]);
(319, [EatInstr(101,345)]);
(320, [EatInstr(108,346)]);
(321, [EatInstr(116,347)]);
(322, [AAction2Instr(__a70,219)]);
(323, [EatInstr(114,348)]);
(324, [EatInstr(111,349)]);
(325, [EatInstr(101,350)]);
(326, [AAction2Instr(__a71,351)]);
(327, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,352)]);
(328, [EatInstr(116,353)]);
(329, [EatInstr(115,354)]);
(330, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,355)]);
(331, [EatInstr(99,356)]);
(332, [EatInstr(114,357)]);
(333, [EatInstr(97,358)]);
(334, [EatInstr(101,359)]);
(335, [EatInstr(105,360)]);
(336, [EatInstr(97,361)]);
(337, [EatInstr(107,362)]);
(338, [EatInstr(108,363)]);
(339, [CompleteInstr(272)]);
(340, [AAction2Instr(__a72,364)]);
(341, [AAction2Instr(__a73,219)]);
(342, [EatInstr(101,365)]);
(343, [EatInstr(115,366)]);
(344, [AAction2Instr(__a74,219)]);
(345, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,367)]);
(346, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,368)]);
(347, [EatInstr(101,369)]);
(348, [EatInstr(101,370)]);
(349, [EatInstr(114,371)]);
(350, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,372)]);
(351, [AAction2Instr(__a75,374);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,373)]);
(352, [AAction2Instr(__a76,213)]);
(353, [EatInstr(97,375)]);
(354, [EatInstr(101,376)]);
(355, [AAction2Instr(__a77,377)]);
(356, [EatInstr(115,378)]);
(357, [EatInstr(101,379)]);
(358, [EatInstr(100,380)]);
(359, [EatInstr(115,381)]);
(360, [EatInstr(110,382)]);
(361, [EatInstr(98,383)]);
(362, [EatInstr(97,384)]);
(363, [EatInstr(97,385)]);
(364, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,386)]);
(365, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,387)]);
(366, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,388)]);
(367, [AAction2Instr(__a78,389)]);
(368, [AAction2Instr(__a79,219)]);
(369, [EatInstr(45,390)]);
(370, [EatInstr(100,391)]);
(371, [EatInstr(109,392)]);
(372, [AAction2Instr(__a80,393)]);
(373, [AAction2Instr(__a81,351)]);
(374, [AWhenInstr3(__p83,__p82,394)]);
(375, [EatInstr(114,395)]);
(376, [EatInstr(110,396)]);
(377, [EatInstr(116,399);EatInstr(112,398);EatInstr(102,397)]);
(378, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,400)]);
(379, [EatInstr(103,401)]);
(380, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,402)]);
(381, [EatInstr(99,403)]);
(382, [EatInstr(101,404)]);
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
