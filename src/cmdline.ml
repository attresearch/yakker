
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
_r_command(_n,ykinput) = (ignore (*1024*) (_n()); 
 (let _x39 = 
 (match _n() with
 | (1029) -> ( Attributes_cmd )
 | (1034) -> ( Close_under_core_cmd )
 | (1039) -> ( Compile_cmd )
 | (1044) -> ( Desugar_cmd )
 | (1049) -> ( Dispatch_cmd )
 | (1054) -> ( Dot_cmd )
 | (1064) -> (
 (let _x9 = (ignore (*1065*) (_n()); _n())
  in (ignore (*1066*) (_n()); 
 (let _x8 = (ignore (*1067*) (_n()); _n())
  in (ignore (*1068*) (_n()); 
 (let f = (ignore (*1069*) (_n()); Yak.YkBuf.get_string _x9 _x8 ykinput)
  in (ignore (*1070*) (_n()); 
 (let l = (ignore (*1071*) (_n()); 
 (let _x15 = (ignore (*1072*) (_n()); 
 (let rec _x45 _x15 = (match _n() with 1073 -> _x15 | _x44 -> _x45((ignore (*1074*) (_x44); 
 (let _x14 = (ignore (*1083*) (_n()); 
 (let _x13 = (ignore (*1084*) (_n()); _n())
  in (ignore (*1085*) (_n()); 
 (let _x12 = (ignore (*1086*) (_n()); _n())
  in (ignore (*1087*) (_n()); 
 (let x = (ignore (*1088*) (_n()); Yak.YkBuf.get_string _x13 _x12 ykinput)
  in (ignore (*1089*) (_n()); x)
 ))
 ))
 ))
  in (ignore (*1090*) (_n()); _x14::_x15)
 ))
 )) in _x45([])))
  in (ignore (*1091*) (_n()); (List.rev _x15))
 ))
  in (ignore (*1093*) (_n());  files := f::!files; exec_l := l; Exec_cmd )
 ))
 ))
 ))
 ))
 | (1098) -> ( Extract_cmd )
 | (1103) -> ( Compileopt.coalesce := true; Fuse_cmd )
 | (1108) -> ( Hash_cmd )
 | (1113) -> ( Info_cmd )
 | (1118) -> ( Compileopt.inline_regular := true; Inline_regular_cmd )
 | (1123) -> ( Lexer_cmd )
 | (1128) -> ( Lift_cmd )
 | (1133) -> ( Lookahead_analysis_cmd )
 | (1138) -> ( Lr1_lookahead_cmd )
 | (1143) -> ( Minus_cmd )
 | (1148) -> ( Tx_prec_cmd )
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
 | (1216) -> ( Subset_cmd )
 | (1223) -> ( Translate_dypgen_cmd )
 | (1230) -> ( Translate_dypgen_scannerless_cmd )
 | (1237) -> ( if !Compileopt.unroll_star_n<1 then Compileopt.unroll_star_n := 1; Unroll_star_cmd )
 | _(*1242*) -> ( Wrap_cmd )
 ) in (ignore (*1243*) (_n()); _x39)
 ))
 
 and
_r_args(_n,ykinput) = (ignore (*1244*) (_n()); 
 (let _x40 = 
 (match _n() with
 | (1249) -> (
 (let b = 
 (match _n() with
 | (1251) -> (Fun_BE)
 | (1253) -> (Trans_BE)
 | (1255) -> (Peg_BE false)
 | _(*1257*) -> (Peg_BE true)
 ) in (ignore (*1259*) (_n());  backend := b )
 ))
 | (1264) -> ( Compileopt.case_sensitive := false )
 | (1269) -> ( Compileopt.check_labels := true )
 | (1287) -> (
 (let _x25 = (ignore (*1288*) (_n()); _n())
  in (ignore (*1289*) (_n()); 
 (let _x24 = (ignore (*1290*) (_n()); _n())
  in (ignore (*1291*) (_n()); 
 (let n = (ignore (*1292*) (_n()); Yak.YkBuf.get_string _x25 _x24 ykinput)
  in (ignore (*1294*) (_n());  Variables.counter := (int_of_string n) )
 ))
 ))
 ))
 | (1299) -> ( Compileopt.inline_cs := true )
 | (1304) -> ( Compileopt.inline_regular := true )
 | (1309) -> ( Compileopt.lookahead := true )
 | (1314) -> ( Yak.Pami.new_engine_flag := true )
 | (1319) -> ( Compileopt.coalesce := false )
 | (1324) -> ( only := true )
 | (1334) -> (
 (let _x29 = (ignore (*1335*) (_n()); _n())
  in (ignore (*1336*) (_n()); 
 (let _x28 = (ignore (*1337*) (_n()); _n())
  in (ignore (*1338*) (_n()); 
 (let x = (ignore (*1339*) (_n()); Yak.YkBuf.get_string _x29 _x28 ykinput)
  in (ignore (*1341*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | (1359) -> (
 (let _x34 = (ignore (*1360*) (_n()); _n())
  in (ignore (*1361*) (_n()); 
 (let _x33 = (ignore (*1362*) (_n()); _n())
  in (ignore (*1363*) (_n()); 
 (let n = (ignore (*1364*) (_n()); Yak.YkBuf.get_string _x34 _x33 ykinput)
  in (ignore (*1366*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1371) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | _(*1379*) -> (
 (let _x38 = (ignore (*1380*) (_n()); _n())
  in (ignore (*1381*) (_n()); 
 (let _x37 = (ignore (*1382*) (_n()); _n())
  in (ignore (*1383*) (_n()); 
 (let f = (ignore (*1384*) (_n()); Yak.YkBuf.get_string _x38 _x37 ykinput)
  in (ignore (*1386*) (_n());  files := f::!files )
 ))
 ))
 ))
 ) in (ignore (*1387*) (_n()); _x40)
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
let _x81 =
 (fun _(*pos*) (_,_x50)(*arg of command*) -> (_t(function
 | 1026 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1031 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1036 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1041 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1046 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1051 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1057 ->
 (fun pos_ -> let _x69 _x7  = _t(fun _(*1061*) pos_ -> let _x70 _x6  = _t(fun _(*1064*) pos_ -> let _x72 _x71  = _t(fun _(*1066*) pos_ -> let _x74 _x73  = _t(fun _(*1072*) pos_ -> let rec _x76 _x75  = _t(function
 | 1073 ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore(_x75);_wv0);_wv0);_wv0))
 | _(*1076*) ->
 (fun pos_ -> let _x77 _x11  = _t(fun _(*1080*) pos_ -> let _x78 _x10  = _t(fun _(*1083*) pos_ -> let _x80 _x79  = _t(fun _(*1086*) pos_ -> Yk_delay(_x76 (ignore(ignore((_wv0));_wv0);_wv0) ,_x10)) in _t(fun _(*1084*) pos_ -> Yk_delay(_x80 ((_wv0)) ,_x11))) in _t(fun _(*1081*) pos_ -> _x78 (pos_) )) in _t(fun _(*1077*) pos_ -> _x77 (pos_) ))) in _x76 (_wv0) ) in _t(fun _(*1067*) pos_ -> Yk_delay(_x74 ((_wv0)) ,_x6))) in _t(fun _(*1065*) pos_ -> Yk_delay(_x72 ((_wv0)) ,_x7))) in _t(fun _(*1062*) pos_ -> _x70 (pos_) )) in _t(fun _(*1058*) pos_ -> _x69 (pos_) ))
 | 1095 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1100 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1105 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
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
 (fun pos_ -> let _x61 _x18  = _t(fun _(*1183*) pos_ -> let _x63 _x62  = _t(fun _(*1192*) pos_ -> let _x66 _x17  = _t(fun _(*1195*) pos_ -> let _x68 _x67  = _t(fun _(*1198*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x17)) in _t(fun _(*1196*) pos_ -> Yk_delay(_x68 ((_wv0)) ,_x18))) in _t(fun _(*1193*) pos_ -> _x66 (pos_) )) in _t(fun _(*1184*) pos_ -> let _x64 _x16  = _t(function
 | 1190 ->
 (fun pos_ -> Yk_when(_x16>=1))
 | _(*1191*) ->
 (fun pos_ -> _x63 (ignore((_wv0));_wv0) )) in _t(fun _(*1185*) pos_ -> let rec _x65 _x16  = _t(function
 | 1186 ->
 (fun pos_ -> _x64 (_x16) )
 | _(*1188*) ->
 (fun pos_ -> _x65 (_x16+1) )) in _x65 (0) ))) in _t(fun _(*1181*) pos_ -> _x61 (pos_) ))
 | _(*1203*) ->
 (fun pos_ -> let _x52 _x51  = _t(function
 | 1206 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1210*) ->
 (fun pos_ -> let _x54 _x53  = _t(function
 | 1213 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1217*) ->
 (fun pos_ -> let _x56 _x55  = _t(function
 | 1220 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1224*) ->
 (fun pos_ -> let _x58 _x57  = _t(function
 | 1227 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1231*) ->
 (fun pos_ -> let _x60 _x59  = _t(function
 | 1234 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1239*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1232*) pos_ -> _x60 (()) ))) in _t(fun _(*1225*) pos_ -> _x58 (()) ))) in _t(fun _(*1218*) pos_ -> _x56 (()) ))) in _t(fun _(*1211*) pos_ -> _x54 (()) ))) in _t(fun _(*1204*) pos_ -> _x52 (()) ))),_x50))
let _x107 =
 (fun _(*pos*) (_,_x82)(*arg of args*) -> (_t(function
 | 1246 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1261 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1266 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1272 ->
 (fun pos_ -> let _x99 _x23  = _t(fun _(*1275*) pos_ -> let _x101 _x100  = _t(fun _(*1284*) pos_ -> let _x104 _x22  = _t(fun _(*1287*) pos_ -> let _x106 _x105  = _t(fun _(*1290*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x22)) in _t(fun _(*1288*) pos_ -> Yk_delay(_x106 ((_wv0)) ,_x23))) in _t(fun _(*1285*) pos_ -> _x104 (pos_) )) in _t(fun _(*1276*) pos_ -> let _x102 _x21  = _t(function
 | 1282 ->
 (fun pos_ -> Yk_when(_x21>=1))
 | _(*1283*) ->
 (fun pos_ -> _x101 (ignore((_wv0));_wv0) )) in _t(fun _(*1277*) pos_ -> let rec _x103 _x21  = _t(function
 | 1278 ->
 (fun pos_ -> _x102 (_x21) )
 | _(*1280*) ->
 (fun pos_ -> _x103 (_x21+1) )) in _x103 (0) ))) in _t(fun _(*1273*) pos_ -> _x99 (pos_) ))
 | 1296 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1301 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1306 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1311 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1316 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1321 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1327 ->
 (fun pos_ -> let _x95 _x27  = _t(fun _(*1331*) pos_ -> let _x96 _x26  = _t(fun _(*1334*) pos_ -> let _x98 _x97  = _t(fun _(*1337*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x26)) in _t(fun _(*1335*) pos_ -> Yk_delay(_x98 ((_wv0)) ,_x27))) in _t(fun _(*1332*) pos_ -> _x96 (pos_) )) in _t(fun _(*1328*) pos_ -> _x95 (pos_) ))
 | 1344 ->
 (fun pos_ -> let _x87 _x32  = _t(fun _(*1347*) pos_ -> let _x89 _x88  = _t(fun _(*1356*) pos_ -> let _x92 _x31  = _t(fun _(*1359*) pos_ -> let _x94 _x93  = _t(fun _(*1362*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x31)) in _t(fun _(*1360*) pos_ -> Yk_delay(_x94 ((_wv0)) ,_x32))) in _t(fun _(*1357*) pos_ -> _x92 (pos_) )) in _t(fun _(*1348*) pos_ -> let _x90 _x30  = _t(function
 | 1354 ->
 (fun pos_ -> Yk_when(_x30>=1))
 | _(*1355*) ->
 (fun pos_ -> _x89 (ignore((_wv0));_wv0) )) in _t(fun _(*1349*) pos_ -> let rec _x91 _x30  = _t(function
 | 1350 ->
 (fun pos_ -> _x90 (_x30) )
 | _(*1352*) ->
 (fun pos_ -> _x91 (_x30+1) )) in _x91 (0) ))) in _t(fun _(*1345*) pos_ -> _x87 (pos_) ))
 | 1368 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1372*) ->
 (fun pos_ -> let _x83 _x36  = _t(fun _(*1376*) pos_ -> let _x84 _x35  = _t(fun _(*1379*) pos_ -> let _x86 _x85  = _t(fun _(*1382*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x35)) in _t(fun _(*1380*) pos_ -> Yk_delay(_x86 ((_wv0)) ,_x36))) in _t(fun _(*1377*) pos_ -> _x84 (pos_) )) in _t(fun _(*1373*) pos_ -> _x83 (pos_) ))),_x82))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a28 = fun p v -> _d 1204 p (_d 1203 p (v));;
let __a98 = _d 1280;;
let __a26 = _d 1170;;
let __a16 = _d 1120;;
let __a59 = _p 1371;;
let __a131 = _p 1153;;
let __a71 = _p 1103;;
let __a90 = _p 1093;;
let __a129 = _p 1264;;
let __a66 = _d 1234;;
let __a80 = _p 1324;;
let __a86 = _p 1044;;
let __a27 = _d 1175;;
let __a17 = _d 1125;;
let __a92 = _p 1216;;
let __a45 = fun p v -> _d 1211 p (_d 1210 p (v));;
let __a4 = _d 1018;;
let __a79 = _p 1158;;
let __a72 = _p 1108;;
let __a87 = _p 1098;;
let __a121 = _p 1269;;
let __a104 = fun p v -> _p 1292 p (_p 1291 p (_ddelay 1290 p (_p 1289 p (_ddelay 1288 p (_d_and_push 1287 p (_d 1285 p (_d 1284 p (v))))))));;
let __p99 = _dnext 1283;;
let __a67 = _d 1239;;
let __a91 = _p 1049;;
let __a58 = fun p v -> _d 1232 p (_d 1231 p (v));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x49 p (v)));;
let __a112 = _d 1350;;
let __a101 = fun p v -> _p 1090 p (_p 1089 p (_p 1088 p (_p 1087 p (_ddelay 1086 p (_p 1085 p (_ddelay 1084 p (_d_and_push 1083 p (_d 1081 p (_d 1080 p (v))))))))));;
let __a18 = _d 1130;;
let __a41 = _d 1020;;
let __p100 = _dwhen 1282;;
let __a33 = _d 1301;;
let __a117 = _d 1352;;
let __a133 = _p 1223;;
let __a96 = _p 1163;;
let __a73 = _p 1113;;
let __a65 = _p 1054;;
let __a51 = fun p v -> _d 1185 p (_d 1184 p (_d 1183 p (_d 1181 p (_d 1180 p (v)))));;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a19 = _d 1135;;
let __a34 = _d 1306;;
let __a32 = _d 1296;;
let __a29 = _d 1246;;
let __a56 = _d 1186;;
let __a5 = _d 1026;;
let __a60 = _p 1386;;
let __a68 = _p 1387;;
let __a122 = _p 1118;;
let __a110 = _p 1168;;
let __a62 = _d 1188;;
let __p42 = _dnext 1023;;
let __a88 = fun p v -> _d 1277 p (_d 1276 p (_d 1275 p (_d 1273 p (_d 1272 p (v)))));;
let __p118 = _dnext 1355;;
let __a82 = fun p v -> _p 1091 p (_d_and_push 1073 p (v));;
let __a136 = _p 1230;;
let __a20 = _d 1140;;
let __a54 = _p 1010;;
let __a35 = _d 1311;;
let __p43 = _dwhen 1022;;
let __a6 = _d 1031;;
let __a94 = _p 1341;;
let __a135 = _p 1173;;
let __p119 = _dwhen 1354;;
let __a77 = _p 1123;;
let __a55 = _p 1013;;
let __a69 = fun p v -> _p 1200 p (_p 1199 p (_ddelay 1198 p (_p 1197 p (_ddelay 1196 p (_d_and_push 1195 p (_d 1193 p (_d 1192 p (v))))))));;
let __a49 = fun p v -> _d 1218 p (_d 1217 p (v));;
let __a76 = fun p v -> _d_and_push 1072 p (_p 1071 p (_p 1070 p (_p 1069 p (_p 1068 p (_ddelay 1067 p (_d_and_push 1066 p (_ddelay 1065 p (_d_and_push 1064 p (_d 1062 p (_d 1061 p (v)))))))))));;
let __a89 = fun p v -> _d 1077 p (_d 1076 p (v));;
let __a21 = _d 1145;;
let __a36 = _d 1316;;
let __a44 = _d 1206;;
let __a7 = _d 1036;;
let __a127 = _p 1237;;
let __p63 = _dnext 1191;;
let __a124 = _p 1178;;
let __a38 = _d 1368;;
let __a74 = _p 1128;;
let __a47 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a53 = fun p v -> _d 1225 p (_d 1224 p (v));;
let __a2 = fun p v -> _p 1024 p (_x81 p (v));;
let __a50 = fun p v -> _p 1384 p (_p 1383 p (_ddelay 1382 p (_p 1381 p (_ddelay 1380 p (_d_and_push 1379 p (_d 1377 p (_d 1376 p (v))))))));;
let __p64 = _dwhen 1190;;
let __a22 = _d 1150;;
let __a12 = _d 1100;;
let __a8 = _d 1041;;
let __a46 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a37 = _d 1321;;
let __a30 = _d 1261;;
let __a97 = _p 1242;;
let __a130 = _p 1133;;
let __a70 = _p 1243;;
let __a48 = _d 1213;;
let __a125 = _p 1304;;
let __a111 = _p 1294;;
let __a3 = fun p v -> _p 1244 p (_x107 p (v));;
let __a81 = _p 1074;;
let __a11 = _d 1095;;
let __a23 = _d 1155;;
let __a13 = _d 1105;;
let __a9 = _d 1046;;
let __a31 = _d 1266;;
let __a116 = _p 1138;;
let __a106 = _p 1309;;
let __a105 = _p 1299;;
let __a102 = _p 1029;;
let __a95 = _p 1249;;
let __a40 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a61 = fun p v -> _d 1058 p (_d 1057 p (v));;
let __a52 = _d 1220;;
let __a24 = _d 1160;;
let __a14 = _d 1110;;
let __a113 = _p 1251;;
let __a10 = _d 1051;;
let __a83 = _p 1202;;
let __a108 = _p 1253;;
let __a107 = fun p v -> _d 1349 p (_d 1348 p (_d 1347 p (_d 1345 p (_d 1344 p (v)))));;
let __a78 = _p 1143;;
let __a126 = _p 1034;;
let __a123 = fun p v -> _p 1364 p (_p 1363 p (_ddelay 1362 p (_p 1361 p (_ddelay 1360 p (_d_and_push 1359 p (_d 1357 p (_d 1356 p (v))))))));;
let __a109 = _p 1314;;
let __a134 = _p 1255;;
let __a25 = _d 1165;;
let __a15 = _d 1115;;
let __a128 = _p 1366;;
let __a114 = _p 1257;;
let __a57 = _d 1227;;
let __a75 = fun p v -> _d 1328 p (_d 1327 p (v));;
let __a84 = fun p v -> _p 1339 p (_p 1338 p (_ddelay 1337 p (_p 1336 p (_ddelay 1335 p (_d_and_push 1334 p (_d 1332 p (_d 1331 p (v))))))));;
let __a103 = _p 1148;;
let __a93 = _d 1278;;
let __a39 = fun p v -> _d 1373 p (_d 1372 p (v));;
let __a132 = _p 1209;;
let __a120 = _p 1259;;
let __a115 = _p 1319;;
let __a85 = _p 1039;;
let __binder0 = __default_ret;;
let __binder1 = _m 1002;;
let __binder2 = _m 1009;;
let binders : (sv -> sv -> sv) array = [| |]
let num_symbols = 10

let symbol_table = function
  | 267 -> "cmd-line-args"
  | 271 -> "command"
  | 268 -> "o"
  | 264 -> "CHAR"
  | 270 -> "file"
  | 265 -> "DIGIT"
  | 272 -> "args"
  | 273 -> "eof"
  | 269 -> "arg"
  | 266 -> "OCTET"
  | x -> if x < 256 then Yak.Pam_internal.default_symbol_table x else "?unknown?"

let get_symb_action = function
  | "cmd-line-args" -> 267
  | "command" -> 271
  | "o" -> 268
  | "CHAR" -> 264
  | "file" -> 270
  | "DIGIT" -> 265
  | "args" -> 272
  | "eof" -> 273
  | "arg" -> 269
  | "OCTET" -> 266
  | _ -> raise Not_found

let get_symb_start = function
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

and nullable_arg __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1022 and n = _dnext 1023 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (fun _x1_ _x2_ _x3_ -> ((((Pred.andc (Pred.full_lookaheadc false 264 1) (fun _x4_ _x5_ _x6_ -> (Some _x6_))) _x1_) _x2_) _x3_))) __lookahead) _p0_) (((_d 1018) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1017 p (_d 1016 p (_x49 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_eof __lookahead _p0_ _x0_ = ((((Pred.full_lookaheadc false 266 3) __lookahead) _p0_) _x0_)

and nullable_OCTET __lookahead _p0_ _x0_ = None

let program : (int * sv instruction list) list = [
(383, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,414)]);
(0, [ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(116,415)]);
(1, [EatInstr(127,11);EatInstr(126,11);EatInstr(125,11);EatInstr(124,11);EatInstr(123,11);EatInstr(122,11);EatInstr(121,11);EatInstr(120,11);EatInstr(119,11);EatInstr(118,11);EatInstr(117,11);EatInstr(116,11);EatInstr(115,11);EatInstr(114,11);EatInstr(113,11);EatInstr(112,11);EatInstr(111,11);EatInstr(110,11);EatInstr(109,11);EatInstr(108,11);EatInstr(107,11);EatInstr(106,11);EatInstr(105,11);EatInstr(104,11);EatInstr(103,11);EatInstr(102,11);EatInstr(101,11);EatInstr(100,11);EatInstr(99,11);EatInstr(98,11);EatInstr(97,11);EatInstr(96,11);EatInstr(95,11);EatInstr(94,11);EatInstr(93,11);EatInstr(92,11);EatInstr(91,11);EatInstr(90,11);EatInstr(89,11);EatInstr(88,11);EatInstr(87,11);EatInstr(86,11);EatInstr(85,11);EatInstr(84,11);EatInstr(83,11);EatInstr(82,11);EatInstr(81,11);EatInstr(80,11);EatInstr(79,11);EatInstr(78,11);EatInstr(77,11);EatInstr(76,11);EatInstr(75,11);EatInstr(74,11);EatInstr(73,11);EatInstr(72,11);EatInstr(71,11);EatInstr(70,11);EatInstr(69,11);EatInstr(68,11);EatInstr(67,11);EatInstr(66,11);EatInstr(65,11);EatInstr(64,11);EatInstr(63,11);EatInstr(62,11);EatInstr(61,11);EatInstr(60,11);EatInstr(59,11);EatInstr(58,11);EatInstr(57,11);EatInstr(56,11);EatInstr(55,11);EatInstr(54,11);EatInstr(53,11);EatInstr(52,11);EatInstr(51,11);EatInstr(50,11);EatInstr(49,11);EatInstr(48,11);EatInstr(47,11);EatInstr(46,11);EatInstr(45,11);EatInstr(44,11);EatInstr(43,11);EatInstr(42,11);EatInstr(41,11);EatInstr(40,11);EatInstr(39,11);EatInstr(38,11);EatInstr(37,11);EatInstr(36,11);EatInstr(35,11);EatInstr(34,11);EatInstr(33,11);EatInstr(32,11);EatInstr(31,11);EatInstr(30,11);EatInstr(29,11);EatInstr(28,11);EatInstr(27,11);EatInstr(26,11);EatInstr(25,11);EatInstr(24,11);EatInstr(23,11);EatInstr(22,11);EatInstr(21,11);EatInstr(20,11);EatInstr(19,11);EatInstr(18,11);EatInstr(17,11);EatInstr(16,11);EatInstr(15,11);EatInstr(14,11);EatInstr(13,11);EatInstr(12,11);EatInstr(11,11);EatInstr(10,11);EatInstr(9,11);EatInstr(8,11);EatInstr(7,11);EatInstr(6,11);EatInstr(5,11);EatInstr(4,11);EatInstr(3,11);EatInstr(2,11);EatInstr(1,11)]);
(385, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,416)]);
(2, [EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12)]);
(386, [EatInstr(115,417)]);
(3, [EatInstr(255,13);EatInstr(254,13);EatInstr(253,13);EatInstr(252,13);EatInstr(251,13);EatInstr(250,13);EatInstr(249,13);EatInstr(248,13);EatInstr(247,13);EatInstr(246,13);EatInstr(245,13);EatInstr(244,13);EatInstr(243,13);EatInstr(242,13);EatInstr(241,13);EatInstr(240,13);EatInstr(239,13);EatInstr(238,13);EatInstr(237,13);EatInstr(236,13);EatInstr(235,13);EatInstr(234,13);EatInstr(233,13);EatInstr(232,13);EatInstr(231,13);EatInstr(230,13);EatInstr(229,13);EatInstr(228,13);EatInstr(227,13);EatInstr(226,13);EatInstr(225,13);EatInstr(224,13);EatInstr(223,13);EatInstr(222,13);EatInstr(221,13);EatInstr(220,13);EatInstr(219,13);EatInstr(218,13);EatInstr(217,13);EatInstr(216,13);EatInstr(215,13);EatInstr(214,13);EatInstr(213,13);EatInstr(212,13);EatInstr(211,13);EatInstr(210,13);EatInstr(209,13);EatInstr(208,13);EatInstr(207,13);EatInstr(206,13);EatInstr(205,13);EatInstr(204,13);EatInstr(203,13);EatInstr(202,13);EatInstr(201,13);EatInstr(200,13);EatInstr(199,13);EatInstr(198,13);EatInstr(197,13);EatInstr(196,13);EatInstr(195,13);EatInstr(194,13);EatInstr(193,13);EatInstr(192,13);EatInstr(191,13);EatInstr(190,13);EatInstr(189,13);EatInstr(188,13);EatInstr(187,13);EatInstr(186,13);EatInstr(185,13);EatInstr(184,13);EatInstr(183,13);EatInstr(182,13);EatInstr(181,13);EatInstr(180,13);EatInstr(179,13);EatInstr(178,13);EatInstr(177,13);EatInstr(176,13);EatInstr(175,13);EatInstr(174,13);EatInstr(173,13);EatInstr(172,13);EatInstr(171,13);EatInstr(170,13);EatInstr(169,13);EatInstr(168,13);EatInstr(167,13);EatInstr(166,13);EatInstr(165,13);EatInstr(164,13);EatInstr(163,13);EatInstr(162,13);EatInstr(161,13);EatInstr(160,13);EatInstr(159,13);EatInstr(158,13);EatInstr(157,13);EatInstr(156,13);EatInstr(155,13);EatInstr(154,13);EatInstr(153,13);EatInstr(152,13);EatInstr(151,13);EatInstr(150,13);EatInstr(149,13);EatInstr(148,13);EatInstr(147,13);EatInstr(146,13);EatInstr(145,13);EatInstr(144,13);EatInstr(143,13);EatInstr(142,13);EatInstr(141,13);EatInstr(140,13);EatInstr(139,13);EatInstr(138,13);EatInstr(137,13);EatInstr(136,13);EatInstr(135,13);EatInstr(134,13);EatInstr(133,13);EatInstr(132,13);EatInstr(131,13);EatInstr(130,13);EatInstr(129,13);EatInstr(128,13);EatInstr(0,13);EatInstr(127,13);EatInstr(126,13);EatInstr(125,13);EatInstr(124,13);EatInstr(123,13);EatInstr(122,13);EatInstr(121,13);EatInstr(120,13);EatInstr(119,13);EatInstr(118,13);EatInstr(117,13);EatInstr(116,13);EatInstr(115,13);EatInstr(114,13);EatInstr(113,13);EatInstr(112,13);EatInstr(111,13);EatInstr(110,13);EatInstr(109,13);EatInstr(108,13);EatInstr(107,13);EatInstr(106,13);EatInstr(105,13);EatInstr(104,13);EatInstr(103,13);EatInstr(102,13);EatInstr(101,13);EatInstr(100,13);EatInstr(99,13);EatInstr(98,13);EatInstr(97,13);EatInstr(96,13);EatInstr(95,13);EatInstr(94,13);EatInstr(93,13);EatInstr(92,13);EatInstr(91,13);EatInstr(90,13);EatInstr(89,13);EatInstr(88,13);EatInstr(87,13);EatInstr(86,13);EatInstr(85,13);EatInstr(84,13);EatInstr(83,13);EatInstr(82,13);EatInstr(81,13);EatInstr(80,13);EatInstr(79,13);EatInstr(78,13);EatInstr(77,13);EatInstr(76,13);EatInstr(75,13);EatInstr(74,13);EatInstr(73,13);EatInstr(72,13);EatInstr(71,13);EatInstr(70,13);EatInstr(69,13);EatInstr(68,13);EatInstr(67,13);EatInstr(66,13);EatInstr(65,13);EatInstr(64,13);EatInstr(63,13);EatInstr(62,13);EatInstr(61,13);EatInstr(60,13);EatInstr(59,13);EatInstr(58,13);EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13);EatInstr(47,13);EatInstr(46,13);EatInstr(45,13);EatInstr(44,13);EatInstr(43,13);EatInstr(42,13);EatInstr(41,13);EatInstr(40,13);EatInstr(39,13);EatInstr(38,13);EatInstr(37,13);EatInstr(36,13);EatInstr(35,13);EatInstr(34,13);EatInstr(33,13);EatInstr(32,13);EatInstr(31,13);EatInstr(30,13);EatInstr(29,13);EatInstr(28,13);EatInstr(27,13);EatInstr(26,13);EatInstr(25,13);EatInstr(24,13);EatInstr(23,13);EatInstr(22,13);EatInstr(21,13);EatInstr(20,13);EatInstr(19,13);EatInstr(18,13);EatInstr(17,13);EatInstr(16,13);EatInstr(15,13);EatInstr(14,13);EatInstr(13,13);EatInstr(12,13);EatInstr(11,13);EatInstr(10,13);EatInstr(9,13);EatInstr(8,13);EatInstr(7,13);EatInstr(6,13);EatInstr(5,13);EatInstr(4,13);EatInstr(3,13);EatInstr(2,13);EatInstr(1,13)]);
(387, [EatInstr(97,418)]);
(4, [AAction2Instr(__a0,14)]);
(388, [EatInstr(99,419)]);
(5, [EatInstr(0,15)]);
(389, [EatInstr(114,420)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,16)]);
(390, [EatInstr(97,421)]);
(7, [EatInstr(127,18);EatInstr(126,18);EatInstr(125,18);EatInstr(124,18);EatInstr(123,18);EatInstr(122,18);EatInstr(121,18);EatInstr(120,18);EatInstr(119,18);EatInstr(118,18);EatInstr(117,18);EatInstr(116,18);EatInstr(115,18);EatInstr(114,18);EatInstr(113,18);EatInstr(112,18);EatInstr(111,18);EatInstr(110,18);EatInstr(109,18);EatInstr(108,18);EatInstr(107,18);EatInstr(106,18);EatInstr(105,18);EatInstr(104,18);EatInstr(103,18);EatInstr(102,18);EatInstr(101,18);EatInstr(100,18);EatInstr(99,18);EatInstr(98,18);EatInstr(97,18);EatInstr(96,18);EatInstr(95,18);EatInstr(94,18);EatInstr(93,18);EatInstr(92,18);EatInstr(91,18);EatInstr(90,18);EatInstr(89,18);EatInstr(88,18);EatInstr(87,18);EatInstr(86,18);EatInstr(85,18);EatInstr(84,18);EatInstr(83,18);EatInstr(82,18);EatInstr(81,18);EatInstr(80,18);EatInstr(79,18);EatInstr(78,18);EatInstr(77,18);EatInstr(76,18);EatInstr(75,18);EatInstr(74,18);EatInstr(73,18);EatInstr(72,18);EatInstr(71,18);EatInstr(70,18);EatInstr(69,18);EatInstr(68,18);EatInstr(67,18);EatInstr(66,18);EatInstr(65,18);EatInstr(64,18);EatInstr(63,18);EatInstr(62,18);EatInstr(61,18);EatInstr(60,18);EatInstr(59,18);EatInstr(58,18);EatInstr(57,18);EatInstr(56,18);EatInstr(55,18);EatInstr(54,18);EatInstr(53,18);EatInstr(52,18);EatInstr(51,18);EatInstr(50,18);EatInstr(49,18);EatInstr(48,18);EatInstr(47,18);EatInstr(46,18);EatInstr(44,18);EatInstr(43,18);EatInstr(42,18);EatInstr(41,18);EatInstr(40,18);EatInstr(39,18);EatInstr(38,18);EatInstr(37,18);EatInstr(36,18);EatInstr(35,18);EatInstr(34,18);EatInstr(33,18);EatInstr(32,18);EatInstr(31,18);EatInstr(30,18);EatInstr(29,18);EatInstr(28,18);EatInstr(27,18);EatInstr(26,18);EatInstr(25,18);EatInstr(24,18);EatInstr(23,18);EatInstr(22,18);EatInstr(21,18);EatInstr(20,18);EatInstr(19,18);EatInstr(18,18);EatInstr(17,18);EatInstr(16,18);EatInstr(15,18);EatInstr(14,18);EatInstr(13,18);EatInstr(12,18);EatInstr(11,18);EatInstr(10,18);EatInstr(9,18);EatInstr(8,18);EatInstr(7,18);EatInstr(6,18);EatInstr(5,18);EatInstr(4,18);EatInstr(3,18);EatInstr(2,18);EatInstr(1,18)]);
(391, [EatInstr(105,422)]);
(8, [AAction2Instr(__a2,19)]);
(392, [EatInstr(101,423)]);
(9, [AAction2Instr(__a3,20)]);
(393, [AAction2Instr(__a89,424)]);
(10, [ALookaheadInstr(false,CfgLA (3,266),21);RCompleteInstr2(273,nullable_eof)]);
(394, [AAction2Instr(__a90,249)]);
(11, [CompleteInstr(264)]);
(395, [EatInstr(115,425)]);
(12, [CompleteInstr(265)]);
(396, [EatInstr(101,426)]);
(13, [CompleteInstr(266)]);
(397, [AAction2Instr(__a91,249)]);
(14, [ACallInstr3(__default_call,8);ASimpleCont2Instr(271,__binder1,22)]);
(398, [EatInstr(103,427)]);
(15, [CompleteInstr(268)]);
(399, [EatInstr(45,428)]);
(16, [AAction2Instr(__a4,24);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,23)]);
(400, [EatInstr(104,429)]);
(401, [EatInstr(101,430)]);
(18, [ALookaheadInstr(false,CfgLA (1,264),25);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,18)]);
(402, [EatInstr(101,431)]);
(19, [EatInstr(114,27);EatInstr(101,26);AAction2Instr(__a28,51);AAction2Instr(__a27,50);AAction2Instr(__a26,49);AAction2Instr(__a25,48);AAction2Instr(__a24,47);AAction2Instr(__a23,46);AAction2Instr(__a22,45);AAction2Instr(__a21,44);AAction2Instr(__a20,43);AAction2Instr(__a19,42);AAction2Instr(__a18,41);AAction2Instr(__a17,40);AAction2Instr(__a16,39);AAction2Instr(__a15,38);AAction2Instr(__a14,37);AAction2Instr(__a13,36);AAction2Instr(__a12,35);AAction2Instr(__a11,34);AAction2Instr(__a10,33);AAction2Instr(__a9,32);AAction2Instr(__a8,31);AAction2Instr(__a7,30);AAction2Instr(__a6,29);AAction2Instr(__a5,28)]);
(403, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,432)]);
(20, [EatInstr(45,52);AAction2Instr(__a39,63);AAction2Instr(__a38,62);AAction2Instr(__a37,61);AAction2Instr(__a36,60);AAction2Instr(__a35,59);AAction2Instr(__a34,58);AAction2Instr(__a33,57);AAction2Instr(__a32,56);AAction2Instr(__a31,55);AAction2Instr(__a30,54);AAction2Instr(__a29,53)]);
(404, [EatInstr(101,433)]);
(21, [CompleteInstr(273)]);
(405, [EatInstr(108,434)]);
(22, [AAction2Instr(__a40,64)]);
(406, [EatInstr(101,435)]);
(23, [AAction2Instr(__a41,16)]);
(407, [EatInstr(116,436)]);
(24, [AWhenInstr3(__p43,__p42,65)]);
(408, [AAction2Instr(__a92,249)]);
(25, [CompleteInstr(270)]);
(409, [EatInstr(97,437)]);
(26, [EatInstr(120,66)]);
(410, [EatInstr(108,438)]);
(27, [EatInstr(102,67)]);
(411, [EatInstr(108,439)]);
(28, [EatInstr(97,68)]);
(412, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,440)]);
(29, [EatInstr(99,69)]);
(413, [AAction2Instr(__a93,442);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,441)]);
(30, [EatInstr(99,70)]);
(414, [AAction2Instr(__a94,241)]);
(31, [EatInstr(100,71)]);
(415, [EatInstr(97,443)]);
(32, [EatInstr(100,72)]);
(416, [AAction2Instr(__a95,444)]);
(33, [EatInstr(100,73)]);
(417, [EatInstr(101,445)]);
(34, [EatInstr(101,74)]);
(418, [EatInstr(98,446)]);
(35, [EatInstr(102,75)]);
(419, [EatInstr(115,447)]);
(36, [EatInstr(104,76)]);
(420, [EatInstr(101,448)]);
(37, [EatInstr(105,77)]);
(421, [EatInstr(100,449)]);
(38, [EatInstr(105,78)]);
(422, [EatInstr(110,450)]);
(39, [EatInstr(108,79)]);
(423, [EatInstr(115,451)]);
(40, [EatInstr(108,80)]);
(424, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,452)]);
(41, [EatInstr(108,81)]);
(425, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,453)]);
(42, [EatInstr(108,82)]);
(426, [EatInstr(114,454)]);
(43, [EatInstr(109,83)]);
(427, [EatInstr(117,455)]);
(44, [EatInstr(112,84)]);
(428, [EatInstr(97,456)]);
(45, [EatInstr(112,85)]);
(429, [EatInstr(101,457)]);
(46, [EatInstr(112,86)]);
(430, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,458)]);
(47, [EatInstr(112,87)]);
(431, [EatInstr(45,459)]);
(48, [EatInstr(112,88)]);
(432, [AAction2Instr(__a96,249)]);
(49, [EatInstr(112,89)]);
(433, [EatInstr(100,460)]);
(50, [EatInstr(112,90)]);
(434, [EatInstr(97,461)]);
(51, [AAction2Instr(__a45,92);AAction2Instr(__a44,91)]);
(435, [EatInstr(118,462)]);
(52, [EatInstr(117,95);EatInstr(114,94);EatInstr(99,93)]);
(436, [EatInstr(101,463)]);
(53, [EatInstr(45,96)]);
(437, [EatInstr(116,464)]);
(54, [EatInstr(45,97)]);
(438, [EatInstr(97,465)]);
(55, [EatInstr(45,98)]);
(439, [EatInstr(108,466)]);
(56, [EatInstr(45,99)]);
(440, [AAction2Instr(__a97,249)]);
(57, [EatInstr(45,100)]);
(441, [AAction2Instr(__a98,413)]);
(58, [EatInstr(45,101)]);
(442, [AWhenInstr3(__p100,__p99,467)]);
(59, [EatInstr(45,102)]);
(443, [EatInstr(114,468)]);
(60, [EatInstr(45,103)]);
(444, [EatInstr(116,471);EatInstr(112,470);EatInstr(102,469)]);
(61, [EatInstr(45,104)]);
(445, [EatInstr(110,472)]);
(62, [EatInstr(45,105)]);
(446, [EatInstr(101,473)]);
(63, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,106)]);
(447, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,474)]);
(64, [AAction2Instr(__a47,108);AAction2Instr(__a46,107)]);
(65, [ALookaheadInstr(false,CfgLA (1,264),109)]);
(448, [EatInstr(103,475)]);
(449, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,476)]);
(66, [EatInstr(101,110)]);
(450, [EatInstr(101,477)]);
(67, [EatInstr(99,111)]);
(451, [EatInstr(99,478)]);
(68, [EatInstr(116,112)]);
(452, [AAction2Instr(__a101,325)]);
(69, [EatInstr(108,113)]);
(453, [AAction2Instr(__a102,249)]);
(70, [EatInstr(111,114)]);
(454, [EatInstr(45,479)]);
(71, [EatInstr(101,115)]);
(455, [EatInstr(108,480)]);
(72, [EatInstr(105,116)]);
(456, [EatInstr(110,481)]);
(73, [EatInstr(111,117)]);
(457, [EatInstr(97,482)]);
(74, [EatInstr(120,118)]);
(458, [AAction2Instr(__a103,249)]);
(75, [EatInstr(117,119)]);
(459, [EatInstr(97,483)]);
(76, [EatInstr(97,120)]);
(460, [EatInstr(115,484)]);
(77, [EatInstr(110,121)]);
(461, [EatInstr(98,485)]);
(78, [EatInstr(110,122)]);
(462, [EatInstr(97,486)]);
(79, [EatInstr(101,123)]);
(463, [EatInstr(45,487)]);
(80, [EatInstr(105,124)]);
(464, [EatInstr(101,488)]);
(81, [EatInstr(111,125)]);
(465, [EatInstr(116,489)]);
(82, [EatInstr(114,126)]);
(466, [EatInstr(45,490)]);
(83, [EatInstr(105,127)]);
(467, [AAction2Instr(__a104,491)]);
(84, [EatInstr(114,128)]);
(468, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,492)]);
(85, [EatInstr(114,129)]);
(469, [EatInstr(117,493)]);
(86, [EatInstr(114,130)]);
(470, [EatInstr(101,494)]);
(87, [EatInstr(114,131)]);
(471, [EatInstr(120,495)]);
(88, [EatInstr(114,132)]);
(472, [EatInstr(115,496)]);
(89, [EatInstr(114,133)]);
(473, [EatInstr(108,497)]);
(90, [EatInstr(114,134)]);
(474, [AAction2Instr(__a105,241)]);
(91, [EatInstr(115,135)]);
(475, [EatInstr(117,498)]);
(92, [AAction2Instr(__a49,137);AAction2Instr(__a48,136)]);
(476, [AAction2Instr(__a106,241)]);
(93, [EatInstr(111,138)]);
(477, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,499)]);
(94, [EatInstr(111,139)]);
(478, [EatInstr(101,500)]);
(95, [EatInstr(110,140)]);
(479, [EatInstr(99,501)]);
(96, [EatInstr(98,141)]);
(480, [EatInstr(97,502)]);
(97, [EatInstr(99,142)]);
(481, [EatInstr(97,503)]);
(98, [EatInstr(99,143)]);
(482, [EatInstr(100,504)]);
(99, [EatInstr(105,144)]);
(483, [EatInstr(110,505)]);
(100, [EatInstr(105,145)]);
(484, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,506)]);
(101, [EatInstr(108,146)]);
(485, [EatInstr(108,507)]);
(102, [EatInstr(110,147)]);
(486, [EatInstr(110,508)]);
(103, [EatInstr(110,148)]);
(487, [EatInstr(97,509)]);
(104, [EatInstr(111,149)]);
(488, [EatInstr(45,510)]);
(105, [EatInstr(118,150)]);
(489, [EatInstr(101,511)]);
(106, [AAction2Instr(__a50,151)]);
(490, [EatInstr(115,512)]);
(107, [ACallInstr3(__default_call,9);ASimpleCont2Instr(272,__binder2,152)]);
(491, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,513)]);
(108, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder0,153)]);
(492, [AAction2Instr(__a107,514)]);
(109, [CompleteInstr(269)]);
(493, [EatInstr(110,515)]);
(110, [EatInstr(99,154)]);
(494, [EatInstr(103,516)]);
(111, [AAction2Instr(__a51,155)]);
(495, [AAction2Instr(__a108,517)]);
(112, [EatInstr(116,156)]);
(496, [EatInstr(105,518)]);
(113, [EatInstr(111,157)]);
(497, [EatInstr(115,519)]);
(114, [EatInstr(109,158)]);
(498, [EatInstr(108,520)]);
(115, [EatInstr(115,159)]);
(499, [AAction2Instr(__a109,241)]);
(116, [EatInstr(115,160)]);
(500, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,521)]);
(117, [EatInstr(116,161)]);
(501, [EatInstr(111,522)]);
(118, [EatInstr(116,162)]);
(502, [EatInstr(114,523)]);
(119, [EatInstr(115,163)]);
(503, [EatInstr(108,524)]);
(120, [EatInstr(115,164)]);
(504, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(121, [EatInstr(102,165)]);
(505, [EatInstr(97,526)]);
(122, [EatInstr(108,166)]);
(506, [AAction2Instr(__a110,249)]);
(123, [EatInstr(120,167)]);
(507, [EatInstr(101,527)]);
(124, [EatInstr(102,168)]);
(508, [EatInstr(99,528)]);
(125, [EatInstr(111,169)]);
(509, [EatInstr(99,529)]);
(126, [EatInstr(49,170)]);
(510, [EatInstr(100,530)]);
(127, [EatInstr(110,171)]);
(511, [EatInstr(45,531)]);
(128, [EatInstr(101,172)]);
(512, [EatInstr(116,532)]);
(129, [EatInstr(101,173)]);
(513, [AAction2Instr(__a111,241)]);
(130, [EatInstr(105,174)]);
(514, [AAction2Instr(__a112,534);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,533)]);
(131, [EatInstr(105,175)]);
(515, [AAction2Instr(__a113,517)]);
(132, [EatInstr(105,176)]);
(516, [EatInstr(45,535);AAction2Instr(__a114,517)]);
(133, [EatInstr(105,177)]);
(517, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,536)]);
(134, [EatInstr(105,178)]);
(518, [EatInstr(116,537)]);
(135, [EatInstr(116,179)]);
(519, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(136, [EatInstr(115,180)]);
(520, [EatInstr(97,539)]);
(137, [AAction2Instr(__a53,182);AAction2Instr(__a52,181)]);
(521, [AAction2Instr(__a115,241)]);
(138, [EatInstr(117,183)]);
(522, [EatInstr(114,540)]);
(139, [EatInstr(111,184)]);
(523, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,541)]);
(140, [EatInstr(114,185)]);
(524, [EatInstr(121,542)]);
(141, [EatInstr(97,186)]);
(525, [AAction2Instr(__a116,249)]);
(142, [EatInstr(97,187)]);
(526, [EatInstr(108,543)]);
(143, [EatInstr(104,188)]);
(527, [EatInstr(45,544)]);
(144, [EatInstr(110,189)]);
(528, [EatInstr(101,545)]);
(145, [EatInstr(110,190)]);
(529, [EatInstr(116,546)]);
(146, [EatInstr(111,191)]);
(530, [EatInstr(121,547)]);
(147, [EatInstr(101,192)]);
(531, [EatInstr(100,548)]);
(148, [EatInstr(111,193)]);
(532, [EatInstr(97,549)]);
(149, [EatInstr(110,194)]);
(533, [AAction2Instr(__a117,514)]);
(150, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,195)]);
(534, [AWhenInstr3(__p119,__p118,550)]);
(151, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,196)]);
(535, [EatInstr(115,551)]);
(152, [AAction2Instr(__a54,64)]);
(536, [AAction2Instr(__a120,241)]);
(153, [AAction2Instr(__a55,197)]);
(537, [EatInstr(105,552)]);
(154, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,198)]);
(538, [AAction2Instr(__a121,241)]);
(155, [AAction2Instr(__a56,200);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,199)]);
(539, [EatInstr(114,553)]);
(156, [EatInstr(114,201)]);
(540, [EatInstr(101,554)]);
(157, [EatInstr(115,202)]);
(541, [AAction2Instr(__a122,249)]);
(158, [EatInstr(112,203)]);
(542, [EatInstr(115,555)]);
(159, [EatInstr(117,204)]);
(543, [EatInstr(121,556)]);
(160, [EatInstr(112,205)]);
(544, [EatInstr(112,557)]);
(161, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,206)]);
(545, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,558)]);
(162, [EatInstr(114,207)]);
(546, [EatInstr(105,559)]);
(163, [EatInstr(101,208)]);
(547, [EatInstr(112,560)]);
(164, [EatInstr(104,209)]);
(548, [EatInstr(121,561)]);
(165, [EatInstr(111,210)]);
(549, [EatInstr(114,562)]);
(166, [EatInstr(105,211)]);
(550, [AAction2Instr(__a123,563)]);
(167, [EatInstr(101,212)]);
(551, [EatInstr(116,564)]);
(168, [EatInstr(116,213)]);
(552, [EatInstr(118,565)]);
(169, [EatInstr(107,214)]);
(553, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,566)]);
(170, [EatInstr(45,215)]);
(554, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,567)]);
(171, [EatInstr(117,216)]);
(555, [EatInstr(105,568)]);
(172, [EatInstr(99,217)]);
(556, [EatInstr(115,569)]);
(173, [EatInstr(99,218)]);
(557, [EatInstr(114,570)]);
(174, [EatInstr(110,219)]);
(558, [AAction2Instr(__a124,249)]);
(175, [EatInstr(110,220)]);
(559, [EatInstr(111,571)]);
(176, [EatInstr(110,221)]);
(560, [EatInstr(103,572)]);
(177, [EatInstr(110,222)]);
(561, [EatInstr(112,573)]);
(178, [EatInstr(110,223)]);
(562, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,574)]);
(179, [EatInstr(114,224)]);
(563, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,575)]);
(180, [EatInstr(117,225)]);
(564, [EatInstr(114,576)]);
(181, [EatInstr(116,226)]);
(565, [EatInstr(101,577)]);
(182, [AAction2Instr(__a58,228);AAction2Instr(__a57,227)]);
(566, [AAction2Instr(__a125,241)]);
(183, [EatInstr(110,229)]);
(567, [AAction2Instr(__a126,249)]);
(184, [EatInstr(116,230)]);
(568, [EatInstr(115,578)]);
(185, [EatInstr(111,231)]);
(569, [EatInstr(105,579)]);
(186, [EatInstr(99,232)]);
(570, [EatInstr(101,580)]);
(187, [EatInstr(115,233)]);
(571, [EatInstr(110,581)]);
(188, [EatInstr(101,234)]);
(572, [EatInstr(101,582)]);
(189, [EatInstr(108,235)]);
(573, [EatInstr(103,583)]);
(190, [EatInstr(108,236)]);
(574, [AAction2Instr(__a127,249)]);
(191, [EatInstr(111,237)]);
(575, [AAction2Instr(__a128,241)]);
(192, [EatInstr(119,238)]);
(576, [EatInstr(105,584)]);
(193, [EatInstr(45,239)]);
(577, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,585)]);
(194, [EatInstr(108,240)]);
(578, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,586)]);
(195, [AAction2Instr(__a59,241)]);
(579, [EatInstr(115,587)]);
(196, [AAction2Instr(__a60,241)]);
(580, [EatInstr(100,588)]);
(197, [CompleteInstr(267)]);
(581, [EatInstr(115,589)]);
(198, [AAction2Instr(__a61,242)]);
(582, [EatInstr(110,590)]);
(199, [AAction2Instr(__a62,155)]);
(583, [EatInstr(101,591)]);
(200, [AWhenInstr3(__p64,__p63,243)]);
(584, [EatInstr(99,592)]);
(201, [EatInstr(105,244)]);
(585, [AAction2Instr(__a129,241)]);
(202, [EatInstr(101,245)]);
(586, [AAction2Instr(__a130,249)]);
(203, [EatInstr(105,246)]);
(587, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,593)]);
(204, [EatInstr(103,247)]);
(588, [EatInstr(105,594)]);
(205, [EatInstr(97,248)]);
(589, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,595)]);
(206, [AAction2Instr(__a65,249)]);
(590, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,596)]);
(207, [EatInstr(97,250)]);
(591, [EatInstr(110,597)]);
(208, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,251)]);
(592, [EatInstr(116,598)]);
(209, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,252)]);
(593, [AAction2Instr(__a131,249)]);
(210, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,253)]);
(594, [EatInstr(99,599)]);
(211, [EatInstr(110,254)]);
(595, [AAction2Instr(__a132,249)]);
(212, [EatInstr(114,255)]);
(596, [AAction2Instr(__a133,249)]);
(213, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,256)]);
(597, [EatInstr(45,600)]);
(214, [EatInstr(97,257)]);
(598, [AAction2Instr(__a134,517)]);
(215, [EatInstr(108,258)]);
(599, [EatInstr(97,601)]);
(216, [EatInstr(115,259)]);
(600, [EatInstr(115,602)]);
(217, [EatInstr(101,260)]);
(601, [EatInstr(116,603)]);
(218, [EatInstr(101,261)]);
(602, [EatInstr(99,604)]);
(219, [EatInstr(116,262)]);
(603, [EatInstr(101,605)]);
(220, [EatInstr(116,263)]);
(604, [EatInstr(97,606)]);
(221, [EatInstr(116,264)]);
(605, [EatInstr(115,607)]);
(222, [EatInstr(116,265)]);
(606, [EatInstr(110,608)]);
(223, [EatInstr(116,266)]);
(607, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,609)]);
(224, [EatInstr(105,267)]);
(608, [EatInstr(110,610)]);
(225, [EatInstr(98,268)]);
(609, [AAction2Instr(__a135,249)]);
(226, [EatInstr(114,269)]);
(610, [EatInstr(101,611)]);
(227, [EatInstr(116,270)]);
(611, [EatInstr(114,612)]);
(228, [AAction2Instr(__a67,272);AAction2Instr(__a66,271)]);
(612, [EatInstr(108,613)]);
(229, [EatInstr(116,273)]);
(613, [EatInstr(101,614)]);
(230, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,274)]);
(614, [EatInstr(115,615)]);
(231, [EatInstr(108,275)]);
(615, [EatInstr(115,616)]);
(232, [EatInstr(107,276)]);
(616, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,617)]);
(233, [EatInstr(101,277)]);
(617, [AAction2Instr(__a136,249)]);
(234, [EatInstr(99,278)]);
(235, [EatInstr(105,279)]);
(236, [EatInstr(105,280)]);
(237, [EatInstr(107,281)]);
(238, [EatInstr(45,282)]);
(239, [EatInstr(99,283)]);
(240, [EatInstr(121,284)]);
(241, [AAction2Instr(__a68,285)]);
(242, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,286)]);
(243, [AAction2Instr(__a69,287)]);
(244, [EatInstr(98,288)]);
(245, [EatInstr(45,289)]);
(246, [EatInstr(108,290)]);
(247, [EatInstr(97,291)]);
(248, [EatInstr(116,292)]);
(249, [AAction2Instr(__a70,293)]);
(250, [EatInstr(99,294)]);
(251, [AAction2Instr(__a71,249)]);
(252, [AAction2Instr(__a72,249)]);
(253, [AAction2Instr(__a73,249)]);
(254, [EatInstr(101,295)]);
(255, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,296)]);
(256, [AAction2Instr(__a74,249)]);
(257, [EatInstr(104,297)]);
(258, [EatInstr(111,298)]);
(259, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,299)]);
(260, [EatInstr(100,300)]);
(261, [EatInstr(100,301)]);
(262, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,302)]);
(263, [EatInstr(45,303)]);
(264, [EatInstr(45,304)]);
(265, [EatInstr(45,305)]);
(266, [EatInstr(45,306)]);
(267, [EatInstr(112,307)]);
(268, [EatInstr(115,308)]);
(269, [EatInstr(97,309)]);
(270, [EatInstr(114,310)]);
(271, [EatInstr(117,311)]);
(272, [EatInstr(119,312)]);
(273, [EatInstr(101,313)]);
(274, [AAction2Instr(__a75,314)]);
(275, [EatInstr(108,315)]);
(276, [EatInstr(101,316)]);
(277, [EatInstr(45,317)]);
(278, [EatInstr(107,318)]);
(279, [EatInstr(110,319)]);
(280, [EatInstr(110,320)]);
(281, [EatInstr(97,321)]);
(282, [EatInstr(101,322)]);
(283, [EatInstr(111,323)]);
(284, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,324)]);
(285, [CompleteInstr(272)]);
(286, [AAction2Instr(__a76,325)]);
(287, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,326)]);
(288, [EatInstr(117,327)]);
(289, [EatInstr(117,328)]);
(290, [EatInstr(101,329)]);
(291, [EatInstr(114,330)]);
(292, [EatInstr(99,331)]);
(293, [CompleteInstr(271)]);
(294, [EatInstr(116,332)]);
(295, [EatInstr(45,333)]);
(296, [AAction2Instr(__a77,249)]);
(297, [EatInstr(101,334)]);
(298, [EatInstr(111,335)]);
(299, [AAction2Instr(__a78,249)]);
(300, [EatInstr(101,336)]);
(301, [EatInstr(101,337)]);
(302, [AAction2Instr(__a79,249)]);
(303, [EatInstr(103,338)]);
(304, [EatInstr(110,339)]);
(305, [EatInstr(110,340)]);
(306, [EatInstr(114,341)]);
(307, [EatInstr(45,342)]);
(308, [EatInstr(101,343)]);
(309, [EatInstr(110,344)]);
(310, [EatInstr(97,345)]);
(311, [EatInstr(110,346)]);
(312, [EatInstr(114,347)]);
(313, [EatInstr(114,348)]);
(314, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,349)]);
(315, [EatInstr(45,350)]);
(316, [EatInstr(110,351)]);
(317, [EatInstr(105,352)]);
(318, [EatInstr(45,353)]);
(319, [EatInstr(101,354)]);
(320, [EatInstr(101,355)]);
(321, [EatInstr(104,356)]);
(322, [EatInstr(110,357)]);
(323, [EatInstr(97,358)]);
(324, [AAction2Instr(__a80,241)]);
(325, [AAction2Instr(__a82,360);AAction2Instr(__a81,359)]);
(326, [AAction2Instr(__a83,249)]);
(327, [EatInstr(116,361)]);
(328, [EatInstr(110,362)]);
(329, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,363)]);
(330, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,364)]);
(331, [EatInstr(104,365)]);
(332, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,366)]);
(333, [EatInstr(114,367)]);
(334, [EatInstr(97,368)]);
(335, [EatInstr(107,369)]);
(336, [EatInstr(110,370)]);
(337, [EatInstr(110,371)]);
(338, [EatInstr(105,372)]);
(339, [EatInstr(112,373)]);
(340, [EatInstr(117,374)]);
(341, [EatInstr(101,375)]);
(342, [EatInstr(108,376)]);
(343, [EatInstr(116,377)]);
(344, [EatInstr(115,378)]);
(345, [EatInstr(110,379)]);
(346, [EatInstr(114,380)]);
(347, [EatInstr(97,381)]);
(348, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,382)]);
(349, [AAction2Instr(__a84,383)]);
(350, [EatInstr(115,384)]);
(351, [EatInstr(100,385)]);
(352, [EatInstr(110,386)]);
(353, [EatInstr(108,387)]);
(354, [EatInstr(45,388)]);
(355, [EatInstr(45,389)]);
(356, [EatInstr(101,390)]);
(357, [EatInstr(103,391)]);
(358, [EatInstr(108,392)]);
(359, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,393)]);
(360, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,394)]);
(361, [EatInstr(101,395)]);
(362, [EatInstr(100,396)]);
(363, [AAction2Instr(__a85,249)]);
(364, [AAction2Instr(__a86,249)]);
(365, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,397)]);
(366, [AAction2Instr(__a87,249)]);
(367, [EatInstr(101,398)]);
(368, [EatInstr(100,399)]);
(369, [EatInstr(97,400)]);
(370, [EatInstr(99,401)]);
(371, [EatInstr(99,402)]);
(372, [EatInstr(108,403)]);
(373, [EatInstr(114,404)]);
(374, [EatInstr(108,405)]);
(375, [EatInstr(108,406)]);
(376, [EatInstr(97,407)]);
(377, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,408)]);
(378, [EatInstr(108,409)]);
(379, [EatInstr(115,410)]);
(380, [EatInstr(111,411)]);
(381, [EatInstr(112,412)]);
(382, [AAction2Instr(__a88,413)]);
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
