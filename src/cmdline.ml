
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
 | (1314) -> ( Compileopt.coalesce := false )
 | (1319) -> ( only := true )
 | (1329) -> (
 (let _x29 = (ignore (*1330*) (_n()); _n())
  in (ignore (*1331*) (_n()); 
 (let _x28 = (ignore (*1332*) (_n()); _n())
  in (ignore (*1333*) (_n()); 
 (let x = (ignore (*1334*) (_n()); Yak.YkBuf.get_string _x29 _x28 ykinput)
  in (ignore (*1336*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | (1354) -> (
 (let _x34 = (ignore (*1355*) (_n()); _n())
  in (ignore (*1356*) (_n()); 
 (let _x33 = (ignore (*1357*) (_n()); _n())
  in (ignore (*1358*) (_n()); 
 (let n = (ignore (*1359*) (_n()); Yak.YkBuf.get_string _x34 _x33 ykinput)
  in (ignore (*1361*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1366) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | _(*1374*) -> (
 (let _x38 = (ignore (*1375*) (_n()); _n())
  in (ignore (*1376*) (_n()); 
 (let _x37 = (ignore (*1377*) (_n()); _n())
  in (ignore (*1378*) (_n()); 
 (let f = (ignore (*1379*) (_n()); Yak.YkBuf.get_string _x38 _x37 ykinput)
  in (ignore (*1381*) (_n());  files := f::!files )
 ))
 ))
 ))
 ) in (ignore (*1382*) (_n()); _x40)
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
 | 1322 ->
 (fun pos_ -> let _x95 _x27  = _t(fun _(*1326*) pos_ -> let _x96 _x26  = _t(fun _(*1329*) pos_ -> let _x98 _x97  = _t(fun _(*1332*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x26)) in _t(fun _(*1330*) pos_ -> Yk_delay(_x98 ((_wv0)) ,_x27))) in _t(fun _(*1327*) pos_ -> _x96 (pos_) )) in _t(fun _(*1323*) pos_ -> _x95 (pos_) ))
 | 1339 ->
 (fun pos_ -> let _x87 _x32  = _t(fun _(*1342*) pos_ -> let _x89 _x88  = _t(fun _(*1351*) pos_ -> let _x92 _x31  = _t(fun _(*1354*) pos_ -> let _x94 _x93  = _t(fun _(*1357*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x31)) in _t(fun _(*1355*) pos_ -> Yk_delay(_x94 ((_wv0)) ,_x32))) in _t(fun _(*1352*) pos_ -> _x92 (pos_) )) in _t(fun _(*1343*) pos_ -> let _x90 _x30  = _t(function
 | 1349 ->
 (fun pos_ -> Yk_when(_x30>=1))
 | _(*1350*) ->
 (fun pos_ -> _x89 (ignore((_wv0));_wv0) )) in _t(fun _(*1344*) pos_ -> let rec _x91 _x30  = _t(function
 | 1345 ->
 (fun pos_ -> _x90 (_x30) )
 | _(*1347*) ->
 (fun pos_ -> _x91 (_x30+1) )) in _x91 (0) ))) in _t(fun _(*1340*) pos_ -> _x87 (pos_) ))
 | 1363 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1367*) ->
 (fun pos_ -> let _x83 _x36  = _t(fun _(*1371*) pos_ -> let _x84 _x35  = _t(fun _(*1374*) pos_ -> let _x86 _x85  = _t(fun _(*1377*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x35)) in _t(fun _(*1375*) pos_ -> Yk_delay(_x86 ((_wv0)) ,_x36))) in _t(fun _(*1372*) pos_ -> _x84 (pos_) )) in _t(fun _(*1368*) pos_ -> _x83 (pos_) ))),_x82))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a28 = fun p v -> _d 1204 p (_d 1203 p (v));;
let __a97 = _d 1280;;
let __a26 = _d 1170;;
let __a16 = _d 1120;;
let __a129 = _p 1153;;
let __a70 = _p 1103;;
let __a89 = _p 1093;;
let __a127 = _p 1264;;
let __a65 = _d 1234;;
let __a85 = _p 1044;;
let __a110 = _d 1345;;
let __a27 = _d 1175;;
let __a17 = _d 1125;;
let __a91 = _p 1216;;
let __a115 = _d 1347;;
let __a44 = fun p v -> _d 1211 p (_d 1210 p (v));;
let __a4 = _d 1018;;
let __a78 = _p 1158;;
let __a71 = _p 1108;;
let __a86 = _p 1098;;
let __a119 = _p 1269;;
let __a103 = fun p v -> _p 1292 p (_p 1291 p (_ddelay 1290 p (_p 1289 p (_ddelay 1288 p (_d_and_push 1287 p (_d 1285 p (_d 1284 p (v))))))));;
let __p98 = _dnext 1283;;
let __a66 = _d 1239;;
let __a90 = _p 1049;;
let __a57 = fun p v -> _d 1232 p (_d 1231 p (v));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x49 p (v)));;
let __a100 = fun p v -> _p 1090 p (_p 1089 p (_p 1088 p (_p 1087 p (_ddelay 1086 p (_p 1085 p (_ddelay 1084 p (_d_and_push 1083 p (_d 1081 p (_d 1080 p (v))))))))));;
let __a18 = _d 1130;;
let __a40 = _d 1020;;
let __p99 = _dwhen 1282;;
let __a33 = _d 1301;;
let __a59 = _p 1381;;
let __a67 = _p 1382;;
let __a131 = _p 1223;;
let __a95 = _p 1163;;
let __a72 = _p 1113;;
let __a64 = _p 1054;;
let __a50 = fun p v -> _d 1185 p (_d 1184 p (_d 1183 p (_d 1181 p (_d 1180 p (v)))));;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a19 = _d 1135;;
let __p116 = _dnext 1350;;
let __a106 = fun p v -> _d 1344 p (_d 1343 p (_d 1342 p (_d 1340 p (_d 1339 p (v)))));;
let __a34 = _d 1306;;
let __a32 = _d 1296;;
let __a29 = _d 1246;;
let __a55 = _d 1186;;
let __a5 = _d 1026;;
let __a93 = _p 1336;;
let __a120 = _p 1118;;
let __p117 = _dwhen 1349;;
let __a108 = _p 1168;;
let __a61 = _d 1188;;
let __p41 = _dnext 1023;;
let __a87 = fun p v -> _d 1277 p (_d 1276 p (_d 1275 p (_d 1273 p (_d 1272 p (v)))));;
let __a81 = fun p v -> _p 1091 p (_d_and_push 1073 p (v));;
let __a49 = fun p v -> _p 1379 p (_p 1378 p (_ddelay 1377 p (_p 1376 p (_ddelay 1375 p (_d_and_push 1374 p (_d 1372 p (_d 1371 p (v))))))));;
let __a134 = _p 1230;;
let __a20 = _d 1140;;
let __a53 = _p 1010;;
let __a35 = _d 1311;;
let __p42 = _dwhen 1022;;
let __a6 = _d 1031;;
let __a133 = _p 1173;;
let __a37 = _d 1363;;
let __a76 = _p 1123;;
let __a54 = _p 1013;;
let __a68 = fun p v -> _p 1200 p (_p 1199 p (_ddelay 1198 p (_p 1197 p (_ddelay 1196 p (_d_and_push 1195 p (_d 1193 p (_d 1192 p (v))))))));;
let __a48 = fun p v -> _d 1218 p (_d 1217 p (v));;
let __a75 = fun p v -> _d_and_push 1072 p (_p 1071 p (_p 1070 p (_p 1069 p (_p 1068 p (_ddelay 1067 p (_d_and_push 1066 p (_ddelay 1065 p (_d_and_push 1064 p (_d 1062 p (_d 1061 p (v)))))))))));;
let __a88 = fun p v -> _d 1077 p (_d 1076 p (v));;
let __a21 = _d 1145;;
let __a36 = _d 1316;;
let __a43 = _d 1206;;
let __a7 = _d 1036;;
let __a125 = _p 1237;;
let __p62 = _dnext 1191;;
let __a122 = _p 1178;;
let __a73 = _p 1128;;
let __a83 = fun p v -> _p 1334 p (_p 1333 p (_ddelay 1332 p (_p 1331 p (_ddelay 1330 p (_d_and_push 1329 p (_d 1327 p (_d 1326 p (v))))))));;
let __a46 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __a52 = fun p v -> _d 1225 p (_d 1224 p (v));;
let __a2 = fun p v -> _p 1024 p (_x81 p (v));;
let __p63 = _dwhen 1190;;
let __a121 = fun p v -> _p 1359 p (_p 1358 p (_ddelay 1357 p (_p 1356 p (_ddelay 1355 p (_d_and_push 1354 p (_d 1352 p (_d 1351 p (v))))))));;
let __a22 = _d 1150;;
let __a12 = _d 1100;;
let __a8 = _d 1041;;
let __a45 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a30 = _d 1261;;
let __a96 = _p 1242;;
let __a128 = _p 1133;;
let __a69 = _p 1243;;
let __a47 = _d 1213;;
let __a123 = _p 1304;;
let __a109 = _p 1294;;
let __a3 = fun p v -> _p 1244 p (_x107 p (v));;
let __a80 = _p 1074;;
let __a11 = _d 1095;;
let __a23 = _d 1155;;
let __a13 = _d 1105;;
let __a9 = _d 1046;;
let __a38 = fun p v -> _d 1368 p (_d 1367 p (v));;
let __a31 = _d 1266;;
let __a114 = _p 1138;;
let __a105 = _p 1309;;
let __a104 = _p 1299;;
let __a101 = _p 1029;;
let __a94 = _p 1249;;
let __a39 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a60 = fun p v -> _d 1058 p (_d 1057 p (v));;
let __a51 = _d 1220;;
let __a24 = _d 1160;;
let __a14 = _d 1110;;
let __a126 = _p 1361;;
let __a111 = _p 1251;;
let __a10 = _d 1051;;
let __a82 = _p 1202;;
let __a107 = _p 1253;;
let __a77 = _p 1143;;
let __a124 = _p 1034;;
let __a113 = _p 1314;;
let __a132 = _p 1255;;
let __a25 = _d 1165;;
let __a15 = _d 1115;;
let __a58 = _p 1366;;
let __a112 = _p 1257;;
let __a56 = _d 1227;;
let __a102 = _p 1148;;
let __a92 = _d 1278;;
let __a130 = _p 1209;;
let __a118 = _p 1259;;
let __a79 = _p 1319;;
let __a84 = _p 1039;;
let __a74 = fun p v -> _d 1323 p (_d 1322 p (v));;
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
(383, [EatInstr(101,413)]);
(0, [ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [AAction2Instr(__a88,414)]);
(1, [EatInstr(127,11);EatInstr(126,11);EatInstr(125,11);EatInstr(124,11);EatInstr(123,11);EatInstr(122,11);EatInstr(121,11);EatInstr(120,11);EatInstr(119,11);EatInstr(118,11);EatInstr(117,11);EatInstr(116,11);EatInstr(115,11);EatInstr(114,11);EatInstr(113,11);EatInstr(112,11);EatInstr(111,11);EatInstr(110,11);EatInstr(109,11);EatInstr(108,11);EatInstr(107,11);EatInstr(106,11);EatInstr(105,11);EatInstr(104,11);EatInstr(103,11);EatInstr(102,11);EatInstr(101,11);EatInstr(100,11);EatInstr(99,11);EatInstr(98,11);EatInstr(97,11);EatInstr(96,11);EatInstr(95,11);EatInstr(94,11);EatInstr(93,11);EatInstr(92,11);EatInstr(91,11);EatInstr(90,11);EatInstr(89,11);EatInstr(88,11);EatInstr(87,11);EatInstr(86,11);EatInstr(85,11);EatInstr(84,11);EatInstr(83,11);EatInstr(82,11);EatInstr(81,11);EatInstr(80,11);EatInstr(79,11);EatInstr(78,11);EatInstr(77,11);EatInstr(76,11);EatInstr(75,11);EatInstr(74,11);EatInstr(73,11);EatInstr(72,11);EatInstr(71,11);EatInstr(70,11);EatInstr(69,11);EatInstr(68,11);EatInstr(67,11);EatInstr(66,11);EatInstr(65,11);EatInstr(64,11);EatInstr(63,11);EatInstr(62,11);EatInstr(61,11);EatInstr(60,11);EatInstr(59,11);EatInstr(58,11);EatInstr(57,11);EatInstr(56,11);EatInstr(55,11);EatInstr(54,11);EatInstr(53,11);EatInstr(52,11);EatInstr(51,11);EatInstr(50,11);EatInstr(49,11);EatInstr(48,11);EatInstr(47,11);EatInstr(46,11);EatInstr(45,11);EatInstr(44,11);EatInstr(43,11);EatInstr(42,11);EatInstr(41,11);EatInstr(40,11);EatInstr(39,11);EatInstr(38,11);EatInstr(37,11);EatInstr(36,11);EatInstr(35,11);EatInstr(34,11);EatInstr(33,11);EatInstr(32,11);EatInstr(31,11);EatInstr(30,11);EatInstr(29,11);EatInstr(28,11);EatInstr(27,11);EatInstr(26,11);EatInstr(25,11);EatInstr(24,11);EatInstr(23,11);EatInstr(22,11);EatInstr(21,11);EatInstr(20,11);EatInstr(19,11);EatInstr(18,11);EatInstr(17,11);EatInstr(16,11);EatInstr(15,11);EatInstr(14,11);EatInstr(13,11);EatInstr(12,11);EatInstr(11,11);EatInstr(10,11);EatInstr(9,11);EatInstr(8,11);EatInstr(7,11);EatInstr(6,11);EatInstr(5,11);EatInstr(4,11);EatInstr(3,11);EatInstr(2,11);EatInstr(1,11)]);
(385, [AAction2Instr(__a89,244)]);
(2, [EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12)]);
(386, [EatInstr(115,415)]);
(3, [EatInstr(255,13);EatInstr(254,13);EatInstr(253,13);EatInstr(252,13);EatInstr(251,13);EatInstr(250,13);EatInstr(249,13);EatInstr(248,13);EatInstr(247,13);EatInstr(246,13);EatInstr(245,13);EatInstr(244,13);EatInstr(243,13);EatInstr(242,13);EatInstr(241,13);EatInstr(240,13);EatInstr(239,13);EatInstr(238,13);EatInstr(237,13);EatInstr(236,13);EatInstr(235,13);EatInstr(234,13);EatInstr(233,13);EatInstr(232,13);EatInstr(231,13);EatInstr(230,13);EatInstr(229,13);EatInstr(228,13);EatInstr(227,13);EatInstr(226,13);EatInstr(225,13);EatInstr(224,13);EatInstr(223,13);EatInstr(222,13);EatInstr(221,13);EatInstr(220,13);EatInstr(219,13);EatInstr(218,13);EatInstr(217,13);EatInstr(216,13);EatInstr(215,13);EatInstr(214,13);EatInstr(213,13);EatInstr(212,13);EatInstr(211,13);EatInstr(210,13);EatInstr(209,13);EatInstr(208,13);EatInstr(207,13);EatInstr(206,13);EatInstr(205,13);EatInstr(204,13);EatInstr(203,13);EatInstr(202,13);EatInstr(201,13);EatInstr(200,13);EatInstr(199,13);EatInstr(198,13);EatInstr(197,13);EatInstr(196,13);EatInstr(195,13);EatInstr(194,13);EatInstr(193,13);EatInstr(192,13);EatInstr(191,13);EatInstr(190,13);EatInstr(189,13);EatInstr(188,13);EatInstr(187,13);EatInstr(186,13);EatInstr(185,13);EatInstr(184,13);EatInstr(183,13);EatInstr(182,13);EatInstr(181,13);EatInstr(180,13);EatInstr(179,13);EatInstr(178,13);EatInstr(177,13);EatInstr(176,13);EatInstr(175,13);EatInstr(174,13);EatInstr(173,13);EatInstr(172,13);EatInstr(171,13);EatInstr(170,13);EatInstr(169,13);EatInstr(168,13);EatInstr(167,13);EatInstr(166,13);EatInstr(165,13);EatInstr(164,13);EatInstr(163,13);EatInstr(162,13);EatInstr(161,13);EatInstr(160,13);EatInstr(159,13);EatInstr(158,13);EatInstr(157,13);EatInstr(156,13);EatInstr(155,13);EatInstr(154,13);EatInstr(153,13);EatInstr(152,13);EatInstr(151,13);EatInstr(150,13);EatInstr(149,13);EatInstr(148,13);EatInstr(147,13);EatInstr(146,13);EatInstr(145,13);EatInstr(144,13);EatInstr(143,13);EatInstr(142,13);EatInstr(141,13);EatInstr(140,13);EatInstr(139,13);EatInstr(138,13);EatInstr(137,13);EatInstr(136,13);EatInstr(135,13);EatInstr(134,13);EatInstr(133,13);EatInstr(132,13);EatInstr(131,13);EatInstr(130,13);EatInstr(129,13);EatInstr(128,13);EatInstr(0,13);EatInstr(127,13);EatInstr(126,13);EatInstr(125,13);EatInstr(124,13);EatInstr(123,13);EatInstr(122,13);EatInstr(121,13);EatInstr(120,13);EatInstr(119,13);EatInstr(118,13);EatInstr(117,13);EatInstr(116,13);EatInstr(115,13);EatInstr(114,13);EatInstr(113,13);EatInstr(112,13);EatInstr(111,13);EatInstr(110,13);EatInstr(109,13);EatInstr(108,13);EatInstr(107,13);EatInstr(106,13);EatInstr(105,13);EatInstr(104,13);EatInstr(103,13);EatInstr(102,13);EatInstr(101,13);EatInstr(100,13);EatInstr(99,13);EatInstr(98,13);EatInstr(97,13);EatInstr(96,13);EatInstr(95,13);EatInstr(94,13);EatInstr(93,13);EatInstr(92,13);EatInstr(91,13);EatInstr(90,13);EatInstr(89,13);EatInstr(88,13);EatInstr(87,13);EatInstr(86,13);EatInstr(85,13);EatInstr(84,13);EatInstr(83,13);EatInstr(82,13);EatInstr(81,13);EatInstr(80,13);EatInstr(79,13);EatInstr(78,13);EatInstr(77,13);EatInstr(76,13);EatInstr(75,13);EatInstr(74,13);EatInstr(73,13);EatInstr(72,13);EatInstr(71,13);EatInstr(70,13);EatInstr(69,13);EatInstr(68,13);EatInstr(67,13);EatInstr(66,13);EatInstr(65,13);EatInstr(64,13);EatInstr(63,13);EatInstr(62,13);EatInstr(61,13);EatInstr(60,13);EatInstr(59,13);EatInstr(58,13);EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13);EatInstr(47,13);EatInstr(46,13);EatInstr(45,13);EatInstr(44,13);EatInstr(43,13);EatInstr(42,13);EatInstr(41,13);EatInstr(40,13);EatInstr(39,13);EatInstr(38,13);EatInstr(37,13);EatInstr(36,13);EatInstr(35,13);EatInstr(34,13);EatInstr(33,13);EatInstr(32,13);EatInstr(31,13);EatInstr(30,13);EatInstr(29,13);EatInstr(28,13);EatInstr(27,13);EatInstr(26,13);EatInstr(25,13);EatInstr(24,13);EatInstr(23,13);EatInstr(22,13);EatInstr(21,13);EatInstr(20,13);EatInstr(19,13);EatInstr(18,13);EatInstr(17,13);EatInstr(16,13);EatInstr(15,13);EatInstr(14,13);EatInstr(13,13);EatInstr(12,13);EatInstr(11,13);EatInstr(10,13);EatInstr(9,13);EatInstr(8,13);EatInstr(7,13);EatInstr(6,13);EatInstr(5,13);EatInstr(4,13);EatInstr(3,13);EatInstr(2,13);EatInstr(1,13)]);
(387, [EatInstr(101,416)]);
(4, [AAction2Instr(__a0,14)]);
(388, [AAction2Instr(__a90,244)]);
(5, [EatInstr(0,15)]);
(389, [EatInstr(103,417)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,16)]);
(390, [EatInstr(45,418)]);
(7, [EatInstr(127,18);EatInstr(126,18);EatInstr(125,18);EatInstr(124,18);EatInstr(123,18);EatInstr(122,18);EatInstr(121,18);EatInstr(120,18);EatInstr(119,18);EatInstr(118,18);EatInstr(117,18);EatInstr(116,18);EatInstr(115,18);EatInstr(114,18);EatInstr(113,18);EatInstr(112,18);EatInstr(111,18);EatInstr(110,18);EatInstr(109,18);EatInstr(108,18);EatInstr(107,18);EatInstr(106,18);EatInstr(105,18);EatInstr(104,18);EatInstr(103,18);EatInstr(102,18);EatInstr(101,18);EatInstr(100,18);EatInstr(99,18);EatInstr(98,18);EatInstr(97,18);EatInstr(96,18);EatInstr(95,18);EatInstr(94,18);EatInstr(93,18);EatInstr(92,18);EatInstr(91,18);EatInstr(90,18);EatInstr(89,18);EatInstr(88,18);EatInstr(87,18);EatInstr(86,18);EatInstr(85,18);EatInstr(84,18);EatInstr(83,18);EatInstr(82,18);EatInstr(81,18);EatInstr(80,18);EatInstr(79,18);EatInstr(78,18);EatInstr(77,18);EatInstr(76,18);EatInstr(75,18);EatInstr(74,18);EatInstr(73,18);EatInstr(72,18);EatInstr(71,18);EatInstr(70,18);EatInstr(69,18);EatInstr(68,18);EatInstr(67,18);EatInstr(66,18);EatInstr(65,18);EatInstr(64,18);EatInstr(63,18);EatInstr(62,18);EatInstr(61,18);EatInstr(60,18);EatInstr(59,18);EatInstr(58,18);EatInstr(57,18);EatInstr(56,18);EatInstr(55,18);EatInstr(54,18);EatInstr(53,18);EatInstr(52,18);EatInstr(51,18);EatInstr(50,18);EatInstr(49,18);EatInstr(48,18);EatInstr(47,18);EatInstr(46,18);EatInstr(44,18);EatInstr(43,18);EatInstr(42,18);EatInstr(41,18);EatInstr(40,18);EatInstr(39,18);EatInstr(38,18);EatInstr(37,18);EatInstr(36,18);EatInstr(35,18);EatInstr(34,18);EatInstr(33,18);EatInstr(32,18);EatInstr(31,18);EatInstr(30,18);EatInstr(29,18);EatInstr(28,18);EatInstr(27,18);EatInstr(26,18);EatInstr(25,18);EatInstr(24,18);EatInstr(23,18);EatInstr(22,18);EatInstr(21,18);EatInstr(20,18);EatInstr(19,18);EatInstr(18,18);EatInstr(17,18);EatInstr(16,18);EatInstr(15,18);EatInstr(14,18);EatInstr(13,18);EatInstr(12,18);EatInstr(11,18);EatInstr(10,18);EatInstr(9,18);EatInstr(8,18);EatInstr(7,18);EatInstr(6,18);EatInstr(5,18);EatInstr(4,18);EatInstr(3,18);EatInstr(2,18);EatInstr(1,18)]);
(391, [EatInstr(104,419)]);
(8, [AAction2Instr(__a2,19)]);
(392, [EatInstr(101,420)]);
(9, [AAction2Instr(__a3,20)]);
(393, [EatInstr(101,421)]);
(10, [ALookaheadInstr(false,CfgLA (3,266),21);RCompleteInstr2(273,nullable_eof)]);
(394, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,422)]);
(11, [CompleteInstr(264)]);
(395, [EatInstr(101,423)]);
(12, [CompleteInstr(265)]);
(396, [EatInstr(108,424)]);
(13, [CompleteInstr(266)]);
(397, [EatInstr(101,425)]);
(14, [ACallInstr3(__default_call,8);ASimpleCont2Instr(271,__binder1,22)]);
(398, [EatInstr(116,426)]);
(15, [CompleteInstr(268)]);
(399, [AAction2Instr(__a91,244)]);
(16, [AAction2Instr(__a4,24);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,23)]);
(400, [EatInstr(97,427)]);
(401, [EatInstr(108,428)]);
(18, [ALookaheadInstr(false,CfgLA (1,264),25);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,18)]);
(402, [EatInstr(108,429)]);
(19, [EatInstr(114,27);EatInstr(101,26);AAction2Instr(__a28,51);AAction2Instr(__a27,50);AAction2Instr(__a26,49);AAction2Instr(__a25,48);AAction2Instr(__a24,47);AAction2Instr(__a23,46);AAction2Instr(__a22,45);AAction2Instr(__a21,44);AAction2Instr(__a20,43);AAction2Instr(__a19,42);AAction2Instr(__a18,41);AAction2Instr(__a17,40);AAction2Instr(__a16,39);AAction2Instr(__a15,38);AAction2Instr(__a14,37);AAction2Instr(__a13,36);AAction2Instr(__a12,35);AAction2Instr(__a11,34);AAction2Instr(__a10,33);AAction2Instr(__a9,32);AAction2Instr(__a8,31);AAction2Instr(__a7,30);AAction2Instr(__a6,29);AAction2Instr(__a5,28)]);
(403, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,430)]);
(20, [EatInstr(45,52);AAction2Instr(__a38,62);AAction2Instr(__a37,61);AAction2Instr(__a36,60);AAction2Instr(__a35,59);AAction2Instr(__a34,58);AAction2Instr(__a33,57);AAction2Instr(__a32,56);AAction2Instr(__a31,55);AAction2Instr(__a30,54);AAction2Instr(__a29,53)]);
(404, [AAction2Instr(__a92,432);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,431)]);
(21, [CompleteInstr(273)]);
(405, [AAction2Instr(__a93,236)]);
(22, [AAction2Instr(__a39,63)]);
(406, [EatInstr(97,433)]);
(23, [AAction2Instr(__a40,16)]);
(407, [AAction2Instr(__a94,434)]);
(24, [AWhenInstr3(__p42,__p41,64)]);
(408, [EatInstr(101,435)]);
(25, [CompleteInstr(270)]);
(409, [EatInstr(98,436)]);
(26, [EatInstr(120,65)]);
(410, [EatInstr(115,437)]);
(27, [EatInstr(102,66)]);
(411, [EatInstr(101,438)]);
(28, [EatInstr(97,67)]);
(412, [EatInstr(100,439)]);
(29, [EatInstr(99,68)]);
(413, [EatInstr(115,440)]);
(30, [EatInstr(99,69)]);
(414, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,441)]);
(31, [EatInstr(100,70)]);
(415, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,442)]);
(32, [EatInstr(100,71)]);
(416, [EatInstr(114,443)]);
(33, [EatInstr(100,72)]);
(417, [EatInstr(117,444)]);
(34, [EatInstr(101,73)]);
(418, [EatInstr(97,445)]);
(35, [EatInstr(102,74)]);
(419, [EatInstr(101,446)]);
(36, [EatInstr(104,75)]);
(420, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,447)]);
(37, [EatInstr(105,76)]);
(421, [EatInstr(45,448)]);
(38, [EatInstr(105,77)]);
(422, [AAction2Instr(__a95,244)]);
(39, [EatInstr(108,78)]);
(423, [EatInstr(100,449)]);
(40, [EatInstr(108,79)]);
(424, [EatInstr(97,450)]);
(41, [EatInstr(108,80)]);
(425, [EatInstr(118,451)]);
(42, [EatInstr(108,81)]);
(426, [EatInstr(101,452)]);
(43, [EatInstr(109,82)]);
(427, [EatInstr(116,453)]);
(44, [EatInstr(112,83)]);
(428, [EatInstr(97,454)]);
(45, [EatInstr(112,84)]);
(429, [EatInstr(108,455)]);
(46, [EatInstr(112,85)]);
(430, [AAction2Instr(__a96,244)]);
(47, [EatInstr(112,86)]);
(431, [AAction2Instr(__a97,404)]);
(48, [EatInstr(112,87)]);
(432, [AWhenInstr3(__p99,__p98,456)]);
(49, [EatInstr(112,88)]);
(433, [EatInstr(114,457)]);
(50, [EatInstr(112,89)]);
(434, [EatInstr(116,460);EatInstr(112,459);EatInstr(102,458)]);
(51, [AAction2Instr(__a44,91);AAction2Instr(__a43,90)]);
(435, [EatInstr(110,461)]);
(52, [EatInstr(117,94);EatInstr(114,93);EatInstr(99,92)]);
(436, [EatInstr(101,462)]);
(53, [EatInstr(45,95)]);
(437, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,463)]);
(54, [EatInstr(45,96)]);
(438, [EatInstr(103,464)]);
(55, [EatInstr(45,97)]);
(439, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,465)]);
(56, [EatInstr(45,98)]);
(440, [EatInstr(99,466)]);
(57, [EatInstr(45,99)]);
(441, [AAction2Instr(__a100,318)]);
(58, [EatInstr(45,100)]);
(442, [AAction2Instr(__a101,244)]);
(59, [EatInstr(45,101)]);
(443, [EatInstr(45,467)]);
(60, [EatInstr(45,102)]);
(444, [EatInstr(108,468)]);
(61, [EatInstr(45,103)]);
(445, [EatInstr(110,469)]);
(62, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,104)]);
(446, [EatInstr(97,470)]);
(63, [AAction2Instr(__a46,106);AAction2Instr(__a45,105)]);
(64, [ALookaheadInstr(false,CfgLA (1,264),107)]);
(447, [AAction2Instr(__a102,244)]);
(448, [EatInstr(97,471)]);
(65, [EatInstr(101,108)]);
(449, [EatInstr(115,472)]);
(66, [EatInstr(99,109)]);
(450, [EatInstr(98,473)]);
(67, [EatInstr(116,110)]);
(451, [EatInstr(97,474)]);
(68, [EatInstr(108,111)]);
(452, [EatInstr(45,475)]);
(69, [EatInstr(111,112)]);
(453, [EatInstr(101,476)]);
(70, [EatInstr(101,113)]);
(454, [EatInstr(116,477)]);
(71, [EatInstr(105,114)]);
(455, [EatInstr(45,478)]);
(72, [EatInstr(111,115)]);
(456, [AAction2Instr(__a103,479)]);
(73, [EatInstr(120,116)]);
(457, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,480)]);
(74, [EatInstr(117,117)]);
(458, [EatInstr(117,481)]);
(75, [EatInstr(97,118)]);
(459, [EatInstr(101,482)]);
(76, [EatInstr(110,119)]);
(460, [EatInstr(120,483)]);
(77, [EatInstr(110,120)]);
(461, [EatInstr(115,484)]);
(78, [EatInstr(101,121)]);
(462, [EatInstr(108,485)]);
(79, [EatInstr(105,122)]);
(463, [AAction2Instr(__a104,236)]);
(80, [EatInstr(111,123)]);
(464, [EatInstr(117,486)]);
(81, [EatInstr(114,124)]);
(465, [AAction2Instr(__a105,236)]);
(82, [EatInstr(105,125)]);
(466, [EatInstr(101,487)]);
(83, [EatInstr(114,126)]);
(467, [EatInstr(99,488)]);
(84, [EatInstr(114,127)]);
(468, [EatInstr(97,489)]);
(85, [EatInstr(114,128)]);
(469, [EatInstr(97,490)]);
(86, [EatInstr(114,129)]);
(470, [EatInstr(100,491)]);
(87, [EatInstr(114,130)]);
(471, [EatInstr(110,492)]);
(88, [EatInstr(114,131)]);
(472, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,493)]);
(89, [EatInstr(114,132)]);
(473, [EatInstr(108,494)]);
(90, [EatInstr(115,133)]);
(474, [EatInstr(110,495)]);
(91, [AAction2Instr(__a48,135);AAction2Instr(__a47,134)]);
(475, [EatInstr(97,496)]);
(92, [EatInstr(111,136)]);
(476, [EatInstr(45,497)]);
(93, [EatInstr(111,137)]);
(477, [EatInstr(101,498)]);
(94, [EatInstr(110,138)]);
(478, [EatInstr(115,499)]);
(95, [EatInstr(98,139)]);
(479, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,500)]);
(96, [EatInstr(99,140)]);
(480, [AAction2Instr(__a106,501)]);
(97, [EatInstr(99,141)]);
(481, [EatInstr(110,502)]);
(98, [EatInstr(105,142)]);
(482, [EatInstr(103,503)]);
(99, [EatInstr(105,143)]);
(483, [AAction2Instr(__a107,504)]);
(100, [EatInstr(108,144)]);
(484, [EatInstr(105,505)]);
(101, [EatInstr(110,145)]);
(485, [EatInstr(115,506)]);
(102, [EatInstr(111,146)]);
(486, [EatInstr(108,507)]);
(103, [EatInstr(118,147)]);
(487, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,508)]);
(104, [AAction2Instr(__a49,148)]);
(488, [EatInstr(111,509)]);
(105, [ACallInstr3(__default_call,9);ASimpleCont2Instr(272,__binder2,149)]);
(489, [EatInstr(114,510)]);
(106, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder0,150)]);
(490, [EatInstr(108,511)]);
(107, [CompleteInstr(269)]);
(491, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,512)]);
(108, [EatInstr(99,151)]);
(492, [EatInstr(97,513)]);
(109, [AAction2Instr(__a50,152)]);
(493, [AAction2Instr(__a108,244)]);
(110, [EatInstr(116,153)]);
(494, [EatInstr(101,514)]);
(111, [EatInstr(111,154)]);
(495, [EatInstr(99,515)]);
(112, [EatInstr(109,155)]);
(496, [EatInstr(99,516)]);
(113, [EatInstr(115,156)]);
(497, [EatInstr(100,517)]);
(114, [EatInstr(115,157)]);
(498, [EatInstr(45,518)]);
(115, [EatInstr(116,158)]);
(499, [EatInstr(116,519)]);
(116, [EatInstr(116,159)]);
(500, [AAction2Instr(__a109,236)]);
(117, [EatInstr(115,160)]);
(501, [AAction2Instr(__a110,521);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,520)]);
(118, [EatInstr(115,161)]);
(502, [AAction2Instr(__a111,504)]);
(119, [EatInstr(102,162)]);
(503, [EatInstr(45,522);AAction2Instr(__a112,504)]);
(120, [EatInstr(108,163)]);
(504, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,523)]);
(121, [EatInstr(120,164)]);
(505, [EatInstr(116,524)]);
(122, [EatInstr(102,165)]);
(506, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(123, [EatInstr(111,166)]);
(507, [EatInstr(97,526)]);
(124, [EatInstr(49,167)]);
(508, [AAction2Instr(__a113,236)]);
(125, [EatInstr(110,168)]);
(509, [EatInstr(114,527)]);
(126, [EatInstr(101,169)]);
(510, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,528)]);
(127, [EatInstr(101,170)]);
(511, [EatInstr(121,529)]);
(128, [EatInstr(105,171)]);
(512, [AAction2Instr(__a114,244)]);
(129, [EatInstr(105,172)]);
(513, [EatInstr(108,530)]);
(130, [EatInstr(105,173)]);
(514, [EatInstr(45,531)]);
(131, [EatInstr(105,174)]);
(515, [EatInstr(101,532)]);
(132, [EatInstr(105,175)]);
(516, [EatInstr(116,533)]);
(133, [EatInstr(116,176)]);
(517, [EatInstr(121,534)]);
(134, [EatInstr(115,177)]);
(518, [EatInstr(100,535)]);
(135, [AAction2Instr(__a52,179);AAction2Instr(__a51,178)]);
(519, [EatInstr(97,536)]);
(136, [EatInstr(117,180)]);
(520, [AAction2Instr(__a115,501)]);
(137, [EatInstr(111,181)]);
(521, [AWhenInstr3(__p117,__p116,537)]);
(138, [EatInstr(114,182)]);
(522, [EatInstr(115,538)]);
(139, [EatInstr(97,183)]);
(523, [AAction2Instr(__a118,236)]);
(140, [EatInstr(97,184)]);
(524, [EatInstr(105,539)]);
(141, [EatInstr(104,185)]);
(525, [AAction2Instr(__a119,236)]);
(142, [EatInstr(110,186)]);
(526, [EatInstr(114,540)]);
(143, [EatInstr(110,187)]);
(527, [EatInstr(101,541)]);
(144, [EatInstr(111,188)]);
(528, [AAction2Instr(__a120,244)]);
(145, [EatInstr(111,189)]);
(529, [EatInstr(115,542)]);
(146, [EatInstr(110,190)]);
(530, [EatInstr(121,543)]);
(147, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,191)]);
(531, [EatInstr(112,544)]);
(148, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,192)]);
(532, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,545)]);
(149, [AAction2Instr(__a53,63)]);
(533, [EatInstr(105,546)]);
(150, [AAction2Instr(__a54,193)]);
(534, [EatInstr(112,547)]);
(151, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,194)]);
(535, [EatInstr(121,548)]);
(152, [AAction2Instr(__a55,196);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,195)]);
(536, [EatInstr(114,549)]);
(153, [EatInstr(114,197)]);
(537, [AAction2Instr(__a121,550)]);
(154, [EatInstr(115,198)]);
(538, [EatInstr(116,551)]);
(155, [EatInstr(112,199)]);
(539, [EatInstr(118,552)]);
(156, [EatInstr(117,200)]);
(540, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,553)]);
(157, [EatInstr(112,201)]);
(541, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,554)]);
(158, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,202)]);
(542, [EatInstr(105,555)]);
(159, [EatInstr(114,203)]);
(543, [EatInstr(115,556)]);
(160, [EatInstr(101,204)]);
(544, [EatInstr(114,557)]);
(161, [EatInstr(104,205)]);
(545, [AAction2Instr(__a122,244)]);
(162, [EatInstr(111,206)]);
(546, [EatInstr(111,558)]);
(163, [EatInstr(105,207)]);
(547, [EatInstr(103,559)]);
(164, [EatInstr(101,208)]);
(548, [EatInstr(112,560)]);
(165, [EatInstr(116,209)]);
(549, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,561)]);
(166, [EatInstr(107,210)]);
(550, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,562)]);
(167, [EatInstr(45,211)]);
(551, [EatInstr(114,563)]);
(168, [EatInstr(117,212)]);
(552, [EatInstr(101,564)]);
(169, [EatInstr(99,213)]);
(553, [AAction2Instr(__a123,236)]);
(170, [EatInstr(99,214)]);
(554, [AAction2Instr(__a124,244)]);
(171, [EatInstr(110,215)]);
(555, [EatInstr(115,565)]);
(172, [EatInstr(110,216)]);
(556, [EatInstr(105,566)]);
(173, [EatInstr(110,217)]);
(557, [EatInstr(101,567)]);
(174, [EatInstr(110,218)]);
(558, [EatInstr(110,568)]);
(175, [EatInstr(110,219)]);
(559, [EatInstr(101,569)]);
(176, [EatInstr(114,220)]);
(560, [EatInstr(103,570)]);
(177, [EatInstr(117,221)]);
(561, [AAction2Instr(__a125,244)]);
(178, [EatInstr(116,222)]);
(562, [AAction2Instr(__a126,236)]);
(179, [AAction2Instr(__a57,224);AAction2Instr(__a56,223)]);
(563, [EatInstr(105,571)]);
(180, [EatInstr(110,225)]);
(564, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,572)]);
(181, [EatInstr(116,226)]);
(565, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,573)]);
(182, [EatInstr(111,227)]);
(566, [EatInstr(115,574)]);
(183, [EatInstr(99,228)]);
(567, [EatInstr(100,575)]);
(184, [EatInstr(115,229)]);
(568, [EatInstr(115,576)]);
(185, [EatInstr(101,230)]);
(569, [EatInstr(110,577)]);
(186, [EatInstr(108,231)]);
(570, [EatInstr(101,578)]);
(187, [EatInstr(108,232)]);
(571, [EatInstr(99,579)]);
(188, [EatInstr(111,233)]);
(572, [AAction2Instr(__a127,236)]);
(189, [EatInstr(45,234)]);
(573, [AAction2Instr(__a128,244)]);
(190, [EatInstr(108,235)]);
(574, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,580)]);
(191, [AAction2Instr(__a58,236)]);
(575, [EatInstr(105,581)]);
(192, [AAction2Instr(__a59,236)]);
(576, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,582)]);
(193, [CompleteInstr(267)]);
(577, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,583)]);
(194, [AAction2Instr(__a60,237)]);
(578, [EatInstr(110,584)]);
(195, [AAction2Instr(__a61,152)]);
(579, [EatInstr(116,585)]);
(196, [AWhenInstr3(__p63,__p62,238)]);
(580, [AAction2Instr(__a129,244)]);
(197, [EatInstr(105,239)]);
(581, [EatInstr(99,586)]);
(198, [EatInstr(101,240)]);
(582, [AAction2Instr(__a130,244)]);
(199, [EatInstr(105,241)]);
(583, [AAction2Instr(__a131,244)]);
(200, [EatInstr(103,242)]);
(584, [EatInstr(45,587)]);
(201, [EatInstr(97,243)]);
(585, [AAction2Instr(__a132,504)]);
(202, [AAction2Instr(__a64,244)]);
(586, [EatInstr(97,588)]);
(203, [EatInstr(97,245)]);
(587, [EatInstr(115,589)]);
(204, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,246)]);
(588, [EatInstr(116,590)]);
(205, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,247)]);
(589, [EatInstr(99,591)]);
(206, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,248)]);
(590, [EatInstr(101,592)]);
(207, [EatInstr(110,249)]);
(591, [EatInstr(97,593)]);
(208, [EatInstr(114,250)]);
(592, [EatInstr(115,594)]);
(209, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,251)]);
(593, [EatInstr(110,595)]);
(210, [EatInstr(97,252)]);
(594, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,596)]);
(211, [EatInstr(108,253)]);
(595, [EatInstr(110,597)]);
(212, [EatInstr(115,254)]);
(596, [AAction2Instr(__a133,244)]);
(213, [EatInstr(101,255)]);
(597, [EatInstr(101,598)]);
(214, [EatInstr(101,256)]);
(598, [EatInstr(114,599)]);
(215, [EatInstr(116,257)]);
(599, [EatInstr(108,600)]);
(216, [EatInstr(116,258)]);
(600, [EatInstr(101,601)]);
(217, [EatInstr(116,259)]);
(601, [EatInstr(115,602)]);
(218, [EatInstr(116,260)]);
(602, [EatInstr(115,603)]);
(219, [EatInstr(116,261)]);
(603, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,604)]);
(220, [EatInstr(105,262)]);
(604, [AAction2Instr(__a134,244)]);
(221, [EatInstr(98,263)]);
(222, [EatInstr(114,264)]);
(223, [EatInstr(116,265)]);
(224, [AAction2Instr(__a66,267);AAction2Instr(__a65,266)]);
(225, [EatInstr(116,268)]);
(226, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,269)]);
(227, [EatInstr(108,270)]);
(228, [EatInstr(107,271)]);
(229, [EatInstr(101,272)]);
(230, [EatInstr(99,273)]);
(231, [EatInstr(105,274)]);
(232, [EatInstr(105,275)]);
(233, [EatInstr(107,276)]);
(234, [EatInstr(99,277)]);
(235, [EatInstr(121,278)]);
(236, [AAction2Instr(__a67,279)]);
(237, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,280)]);
(238, [AAction2Instr(__a68,281)]);
(239, [EatInstr(98,282)]);
(240, [EatInstr(45,283)]);
(241, [EatInstr(108,284)]);
(242, [EatInstr(97,285)]);
(243, [EatInstr(116,286)]);
(244, [AAction2Instr(__a69,287)]);
(245, [EatInstr(99,288)]);
(246, [AAction2Instr(__a70,244)]);
(247, [AAction2Instr(__a71,244)]);
(248, [AAction2Instr(__a72,244)]);
(249, [EatInstr(101,289)]);
(250, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,290)]);
(251, [AAction2Instr(__a73,244)]);
(252, [EatInstr(104,291)]);
(253, [EatInstr(111,292)]);
(254, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,293)]);
(255, [EatInstr(100,294)]);
(256, [EatInstr(100,295)]);
(257, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,296)]);
(258, [EatInstr(45,297)]);
(259, [EatInstr(45,298)]);
(260, [EatInstr(45,299)]);
(261, [EatInstr(45,300)]);
(262, [EatInstr(112,301)]);
(263, [EatInstr(115,302)]);
(264, [EatInstr(97,303)]);
(265, [EatInstr(114,304)]);
(266, [EatInstr(117,305)]);
(267, [EatInstr(119,306)]);
(268, [EatInstr(101,307)]);
(269, [AAction2Instr(__a74,308)]);
(270, [EatInstr(108,309)]);
(271, [EatInstr(101,310)]);
(272, [EatInstr(45,311)]);
(273, [EatInstr(107,312)]);
(274, [EatInstr(110,313)]);
(275, [EatInstr(110,314)]);
(276, [EatInstr(97,315)]);
(277, [EatInstr(111,316)]);
(278, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,317)]);
(279, [CompleteInstr(272)]);
(280, [AAction2Instr(__a75,318)]);
(281, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,319)]);
(282, [EatInstr(117,320)]);
(283, [EatInstr(117,321)]);
(284, [EatInstr(101,322)]);
(285, [EatInstr(114,323)]);
(286, [EatInstr(99,324)]);
(287, [CompleteInstr(271)]);
(288, [EatInstr(116,325)]);
(289, [EatInstr(45,326)]);
(290, [AAction2Instr(__a76,244)]);
(291, [EatInstr(101,327)]);
(292, [EatInstr(111,328)]);
(293, [AAction2Instr(__a77,244)]);
(294, [EatInstr(101,329)]);
(295, [EatInstr(101,330)]);
(296, [AAction2Instr(__a78,244)]);
(297, [EatInstr(103,331)]);
(298, [EatInstr(110,332)]);
(299, [EatInstr(110,333)]);
(300, [EatInstr(114,334)]);
(301, [EatInstr(45,335)]);
(302, [EatInstr(101,336)]);
(303, [EatInstr(110,337)]);
(304, [EatInstr(97,338)]);
(305, [EatInstr(110,339)]);
(306, [EatInstr(114,340)]);
(307, [EatInstr(114,341)]);
(308, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,342)]);
(309, [EatInstr(45,343)]);
(310, [EatInstr(110,344)]);
(311, [EatInstr(105,345)]);
(312, [EatInstr(45,346)]);
(313, [EatInstr(101,347)]);
(314, [EatInstr(101,348)]);
(315, [EatInstr(104,349)]);
(316, [EatInstr(97,350)]);
(317, [AAction2Instr(__a79,236)]);
(318, [AAction2Instr(__a81,352);AAction2Instr(__a80,351)]);
(319, [AAction2Instr(__a82,244)]);
(320, [EatInstr(116,353)]);
(321, [EatInstr(110,354)]);
(322, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,355)]);
(323, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,356)]);
(324, [EatInstr(104,357)]);
(325, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,358)]);
(326, [EatInstr(114,359)]);
(327, [EatInstr(97,360)]);
(328, [EatInstr(107,361)]);
(329, [EatInstr(110,362)]);
(330, [EatInstr(110,363)]);
(331, [EatInstr(105,364)]);
(332, [EatInstr(112,365)]);
(333, [EatInstr(117,366)]);
(334, [EatInstr(101,367)]);
(335, [EatInstr(108,368)]);
(336, [EatInstr(116,369)]);
(337, [EatInstr(115,370)]);
(338, [EatInstr(110,371)]);
(339, [EatInstr(114,372)]);
(340, [EatInstr(97,373)]);
(341, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,374)]);
(342, [AAction2Instr(__a83,375)]);
(343, [EatInstr(115,376)]);
(344, [EatInstr(100,377)]);
(345, [EatInstr(110,378)]);
(346, [EatInstr(108,379)]);
(347, [EatInstr(45,380)]);
(348, [EatInstr(45,381)]);
(349, [EatInstr(101,382)]);
(350, [EatInstr(108,383)]);
(351, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,384)]);
(352, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,385)]);
(353, [EatInstr(101,386)]);
(354, [EatInstr(100,387)]);
(355, [AAction2Instr(__a84,244)]);
(356, [AAction2Instr(__a85,244)]);
(357, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,388)]);
(358, [AAction2Instr(__a86,244)]);
(359, [EatInstr(101,389)]);
(360, [EatInstr(100,390)]);
(361, [EatInstr(97,391)]);
(362, [EatInstr(99,392)]);
(363, [EatInstr(99,393)]);
(364, [EatInstr(108,394)]);
(365, [EatInstr(114,395)]);
(366, [EatInstr(108,396)]);
(367, [EatInstr(108,397)]);
(368, [EatInstr(97,398)]);
(369, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,399)]);
(370, [EatInstr(108,400)]);
(371, [EatInstr(115,401)]);
(372, [EatInstr(111,402)]);
(373, [EatInstr(112,403)]);
(374, [AAction2Instr(__a87,404)]);
(375, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,405)]);
(376, [EatInstr(116,406)]);
(377, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,407)]);
(378, [EatInstr(115,408)]);
(379, [EatInstr(97,409)]);
(380, [EatInstr(99,410)]);
(381, [EatInstr(114,411)]);
(382, [EatInstr(97,412)]);
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
