
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
let __a20 = fun p v -> _d 1204 p (_d 1203 p (v));;
let __a18 = _d 1170;;
let __a111 = _p 1321;;
let __a105 = _p 1041;;
let __a128 = _p 1153;;
let __a25 = _d 1283;;
let __a22 = _d 1233;;
let __a56 = _p 1373;;
let __a76 = _p 1104;;
let __a58 = _p 1044;;
let __a19 = _d 1175;;
let __a9 = _d 1125;;
let __a127 = _p 1216;;
let __a74 = _p 1326;;
let __a53 = _p 1047;;
let __a41 = fun p v -> _d 1211 p (_d 1210 p (v));;
let __a26 = _d 1288;;
let __a4 = _d 1018;;
let __a73 = _p 1158;;
let __a2 = fun p v -> _p 1063 p (_x75 p (v));;
let __a5 = fun p v -> _p 1067 p (_p 1066 p (_d 1065 p (v)));;
let __a47 = fun p v -> _p 1386 p (_p 1385 p (_ddelay 1384 p (_p 1383 p (_ddelay 1382 p (_d_and_push 1381 p (_d 1379 p (_d 1378 p (v))))))));;
let __a98 = fun p v -> _p 1120 p (_p 1119 p (_p 1118 p (_p 1117 p (_ddelay 1116 p (_p 1115 p (_ddelay 1114 p (_d_and_push 1113 p (_d 1111 p (_d 1110 p (v))))))))));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x49 p (v)));;
let __a59 = _p 1050;;
let __a10 = _d 1130;;
let __a37 = _d 1020;;
let __a133 = _p 1221;;
let __a6 = _d 1071;;
let __a108 = _d 1352;;
let __a49 = _p 1222;;
let __a94 = _p 1163;;
let __a85 = _p 1053;;
let __a29 = _d 1303;;
let __a27 = _d 1293;;
let __a113 = _d 1354;;
let __a48 = fun p v -> _d 1185 p (_d 1184 p (_d 1183 p (_d 1181 p (_d 1180 p (v)))));;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a11 = _d 1135;;
let __a66 = _p 1056;;
let __a55 = _d 1186;;
let __a7 = _d 1076;;
let __a106 = _p 1168;;
let __a30 = _d 1308;;
let __a28 = _d 1298;;
let __a23 = _d 1248;;
let __a61 = _d 1188;;
let __a57 = _p 1388;;
let __a93 = _p 1059;;
let __a65 = _p 1389;;
let __p38 = _dnext 1023;;
let __p114 = _dnext 1357;;
let __a70 = fun p v -> _d 1330 p (_d 1329 p (v));;
let __a3 = fun p v -> _p 1223 p (_x101 p (v));;
let __a12 = _d 1140;;
let __a50 = _p 1010;;
let __a107 = _p 1281;;
let __p39 = _dwhen 1022;;
let __a8 = _d 1081;;
let __a91 = _p 1231;;
let __a54 = _p 1062;;
let __a132 = _p 1173;;
let __a120 = fun p v -> _p 1366 p (_p 1365 p (_ddelay 1364 p (_p 1363 p (_ddelay 1362 p (_d_and_push 1361 p (_d 1359 p (_d 1358 p (v))))))));;
let __a31 = _d 1313;;
let __a24 = _d 1253;;
let __a90 = _p 1343;;
let __a87 = _p 1123;;
let __a51 = _p 1013;;
let __a67 = fun p v -> _p 1200 p (_p 1199 p (_ddelay 1198 p (_p 1197 p (_ddelay 1196 p (_d_and_push 1195 p (_d 1193 p (_d 1192 p (v))))))));;
let __p115 = _dwhen 1356;;
let __a13 = _d 1145;;
let __a100 = _p 1286;;
let __a40 = _d 1206;;
let __a92 = _p 1236;;
let __p62 = _dnext 1191;;
let __a77 = fun p v -> _p 1121 p (_d_and_push 1103 p (v));;
let __a121 = _p 1178;;
let __a109 = _p 1238;;
let __a32 = _d 1318;;
let __a82 = _p 1128;;
let __a44 = _p 1069;;
let __a43 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __p63 = _dwhen 1190;;
let __a104 = _p 1240;;
let __a14 = _d 1150;;
let __a34 = _d 1370;;
let __a131 = _p 1301;;
let __a122 = _p 1291;;
let __a103 = fun p v -> _d 1351 p (_d 1350 p (_d 1349 p (_d 1347 p (_d 1346 p (v)))));;
let __a42 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a130 = _p 1242;;
let __a79 = fun p v -> _p 1341 p (_p 1340 p (_ddelay 1339 p (_p 1338 p (_ddelay 1337 p (_d_and_push 1336 p (_d 1334 p (_d 1333 p (v))))))));;
let __a68 = _p 1133;;
let __a33 = _d 1323;;
let __a45 = _d 1213;;
let __a110 = _p 1244;;
let __a81 = _p 1074;;
let __a99 = fun p v -> _p 1279 p (_p 1278 p (_ddelay 1277 p (_p 1276 p (_ddelay 1275 p (_d_and_push 1274 p (_d 1272 p (_d 1271 p (v))))))));;
let __a89 = _d 1265;;
let __a15 = _d 1155;;
let __a124 = _p 1296;;
let __a118 = _p 1306;;
let __a116 = _p 1246;;
let __a84 = _p 1026;;
let __a95 = _d 1267;;
let __a80 = fun p v -> _p 1229 p (_p 1228 p (v));;
let __a69 = _p 1138;;
let __a83 = fun p v -> _d 1264 p (_d 1263 p (_d 1262 p (_d 1260 p (_d 1259 p (v)))));;
let __a46 = _d 1218;;
let __a119 = _p 1029;;
let __a88 = _p 1079;;
let __a72 = fun p v -> _d_and_push 1102 p (_p 1101 p (_p 1100 p (_p 1099 p (_p 1098 p (_ddelay 1097 p (_d_and_push 1096 p (_ddelay 1095 p (_d_and_push 1094 p (_d 1092 p (_d 1091 p (v)))))))))));;
let __a36 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a35 = fun p v -> _d 1375 p (_d 1374 p (v));;
let __a16 = _d 1160;;
let __a125 = _p 1251;;
let __a101 = _p 1311;;
let __a86 = fun p v -> _d 1107 p (_d 1106 p (v));;
let __a75 = _p 1032;;
let __a78 = _p 1202;;
let __a126 = _p 1143;;
let __a60 = fun p v -> _d 1088 p (_d 1087 p (v));;
let __a64 = _p 1084;;
let __a21 = _d 1225;;
let __a17 = _d 1165;;
let __a71 = _p 1035;;
let __a117 = _p 1256;;
let __a102 = _p 1316;;
let __p96 = _dnext 1270;;
let __a123 = _p 1368;;
let __a112 = _p 1148;;
let __p97 = _dwhen 1269;;
let __a52 = _p 1038;;
let __a129 = _p 1209;;
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

and nullable_arg __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1022 and n = _dnext 1023 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (fun _x1_ _x2_ _x3_ -> ((((Pred.andc (Pred.full_lookaheadc false 264 1) (fun _x4_ _x5_ _x6_ -> (Some _x6_))) _x1_) _x2_) _x3_))) __lookahead) _p0_) (((_d 1018) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1017 p (_d 1016 p (_x49 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_eof __lookahead _p0_ _x0_ = ((((Pred.full_lookaheadc false 266 3) __lookahead) _p0_) _x0_)

and nullable_OCTET __lookahead _p0_ _x0_ = None

let program : (int * sv instruction list) list = [
(383, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,418)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [EatInstr(104,419)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,420)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [EatInstr(97,421)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [EatInstr(107,422)]);
(4, [AAction2Instr(__a0,15)]);
(388, [EatInstr(110,423)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(105,424)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,17)]);
(390, [EatInstr(112,425)]);
(7, [EatInstr(127,19);EatInstr(126,19);EatInstr(125,19);EatInstr(124,19);EatInstr(123,19);EatInstr(122,19);EatInstr(121,19);EatInstr(120,19);EatInstr(119,19);EatInstr(118,19);EatInstr(117,19);EatInstr(116,19);EatInstr(115,19);EatInstr(114,19);EatInstr(113,19);EatInstr(112,19);EatInstr(111,19);EatInstr(110,19);EatInstr(109,19);EatInstr(108,19);EatInstr(107,19);EatInstr(106,19);EatInstr(105,19);EatInstr(104,19);EatInstr(103,19);EatInstr(102,19);EatInstr(101,19);EatInstr(100,19);EatInstr(99,19);EatInstr(98,19);EatInstr(97,19);EatInstr(96,19);EatInstr(95,19);EatInstr(94,19);EatInstr(93,19);EatInstr(92,19);EatInstr(91,19);EatInstr(90,19);EatInstr(89,19);EatInstr(88,19);EatInstr(87,19);EatInstr(86,19);EatInstr(85,19);EatInstr(84,19);EatInstr(83,19);EatInstr(82,19);EatInstr(81,19);EatInstr(80,19);EatInstr(79,19);EatInstr(78,19);EatInstr(77,19);EatInstr(76,19);EatInstr(75,19);EatInstr(74,19);EatInstr(73,19);EatInstr(72,19);EatInstr(71,19);EatInstr(70,19);EatInstr(69,19);EatInstr(68,19);EatInstr(67,19);EatInstr(66,19);EatInstr(65,19);EatInstr(64,19);EatInstr(63,19);EatInstr(62,19);EatInstr(61,19);EatInstr(60,19);EatInstr(59,19);EatInstr(58,19);EatInstr(57,19);EatInstr(56,19);EatInstr(55,19);EatInstr(54,19);EatInstr(53,19);EatInstr(52,19);EatInstr(51,19);EatInstr(50,19);EatInstr(49,19);EatInstr(48,19);EatInstr(47,19);EatInstr(46,19);EatInstr(44,19);EatInstr(43,19);EatInstr(42,19);EatInstr(41,19);EatInstr(40,19);EatInstr(39,19);EatInstr(38,19);EatInstr(37,19);EatInstr(36,19);EatInstr(35,19);EatInstr(34,19);EatInstr(33,19);EatInstr(32,19);EatInstr(31,19);EatInstr(30,19);EatInstr(29,19);EatInstr(28,19);EatInstr(27,19);EatInstr(26,19);EatInstr(25,19);EatInstr(24,19);EatInstr(23,19);EatInstr(22,19);EatInstr(21,19);EatInstr(20,19);EatInstr(19,19);EatInstr(18,19);EatInstr(17,19);EatInstr(16,19);EatInstr(15,19);EatInstr(14,19);EatInstr(13,19);EatInstr(12,19);EatInstr(11,19);EatInstr(10,19);EatInstr(9,19);EatInstr(8,19);EatInstr(7,19);EatInstr(6,19);EatInstr(5,19);EatInstr(4,19);EatInstr(3,19);EatInstr(2,19);EatInstr(1,19)]);
(391, [EatInstr(117,426)]);
(8, [EatInstr(119,30);EatInstr(117,29);EatInstr(115,28);EatInstr(112,27);EatInstr(109,26);EatInstr(108,25);EatInstr(105,24);EatInstr(104,23);EatInstr(100,22);EatInstr(99,21);EatInstr(97,20)]);
(392, [EatInstr(101,427)]);
(9, [AAction2Instr(__a2,31)]);
(393, [EatInstr(108,428)]);
(10, [AAction2Instr(__a3,32)]);
(394, [EatInstr(108,429)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),33);RCompleteInstr2(274,nullable_eof)]);
(395, [EatInstr(108,430)]);
(12, [CompleteInstr(264)]);
(396, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,431)]);
(13, [CompleteInstr(265)]);
(397, [AAction2Instr(__a79,432)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(115,433)]);
(15, [ACallInstr3(__default_call,9);ASimpleCont2Instr(272,__binder1,34)]);
(399, [AAction2Instr(__a80,434)]);
(16, [CompleteInstr(268)]);
(400, [EatInstr(100,435)]);
(17, [AAction2Instr(__a4,36);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,35)]);
(401, [EatInstr(110,436)]);
(402, [EatInstr(108,437)]);
(19, [ALookaheadInstr(false,CfgLA (1,264),37);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,19)]);
(403, [EatInstr(45,438)]);
(20, [EatInstr(116,38)]);
(404, [EatInstr(45,439)]);
(21, [EatInstr(111,40);EatInstr(108,39)]);
(405, [EatInstr(101,440)]);
(22, [EatInstr(101,41)]);
(406, [EatInstr(111,441)]);
(23, [EatInstr(97,42)]);
(407, [EatInstr(105,442)]);
(24, [EatInstr(110,43)]);
(408, [EatInstr(108,443)]);
(25, [EatInstr(105,45);EatInstr(101,44)]);
(409, [EatInstr(101,444)]);
(26, [EatInstr(105,46)]);
(410, [EatInstr(108,445)]);
(27, [EatInstr(114,47)]);
(411, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,446)]);
(28, [EatInstr(117,48)]);
(412, [EatInstr(114,447)]);
(29, [EatInstr(110,49)]);
(413, [EatInstr(117,448)]);
(30, [EatInstr(114,50)]);
(414, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,449)]);
(31, [EatInstr(114,52);EatInstr(101,51);AAction2Instr(__a20,68);AAction2Instr(__a19,67);AAction2Instr(__a18,66);AAction2Instr(__a17,65);AAction2Instr(__a16,64);AAction2Instr(__a15,63);AAction2Instr(__a14,62);AAction2Instr(__a13,61);AAction2Instr(__a12,60);AAction2Instr(__a11,59);AAction2Instr(__a10,58);AAction2Instr(__a9,57);AAction2Instr(__a8,56);AAction2Instr(__a7,55);AAction2Instr(__a6,54);AAction2Instr(__a5,53)]);
(415, [EatInstr(114,450)]);
(32, [EatInstr(45,69);AAction2Instr(__a35,84);AAction2Instr(__a34,83);AAction2Instr(__a33,82);AAction2Instr(__a32,81);AAction2Instr(__a31,80);AAction2Instr(__a30,79);AAction2Instr(__a29,78);AAction2Instr(__a28,77);AAction2Instr(__a27,76);AAction2Instr(__a26,75);AAction2Instr(__a25,74);AAction2Instr(__a24,73);AAction2Instr(__a23,72);AAction2Instr(__a22,71);AAction2Instr(__a21,70)]);
(416, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,451)]);
(33, [CompleteInstr(274)]);
(417, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,452)]);
(34, [AAction2Instr(__a36,85)]);
(418, [AAction2Instr(__a81,155)]);
(35, [AAction2Instr(__a37,17)]);
(419, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,453)]);
(36, [AWhenInstr3(__p39,__p38,86)]);
(420, [AAction2Instr(__a82,155)]);
(37, [CompleteInstr(270)]);
(421, [EatInstr(100,454)]);
(38, [EatInstr(116,87)]);
(422, [EatInstr(97,455)]);
(39, [EatInstr(111,88)]);
(423, [EatInstr(99,456)]);
(40, [EatInstr(112,89)]);
(424, [EatInstr(108,457)]);
(41, [EatInstr(115,90)]);
(425, [EatInstr(114,458)]);
(42, [EatInstr(115,91)]);
(426, [EatInstr(108,459)]);
(43, [EatInstr(108,92)]);
(427, [EatInstr(108,460)]);
(44, [EatInstr(120,93)]);
(428, [EatInstr(97,461)]);
(45, [EatInstr(102,94)]);
(429, [EatInstr(97,462)]);
(46, [EatInstr(110,95)]);
(430, [EatInstr(97,463)]);
(47, [EatInstr(101,96)]);
(431, [AAction2Instr(__a83,464)]);
(48, [EatInstr(98,97)]);
(432, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,465)]);
(49, [EatInstr(114,98)]);
(433, [EatInstr(116,466)]);
(50, [EatInstr(97,99)]);
(434, [ASimpleCont2Instr(271,__binder4,467);ACallInstr3(__default_call,8)]);
(51, [EatInstr(120,100)]);
(435, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,468)]);
(52, [EatInstr(102,101)]);
(436, [EatInstr(115,469)]);
(53, [ASimpleCont2Instr(271,__binder2,102);ACallInstr3(__default_call,8)]);
(437, [EatInstr(97,470)]);
(54, [EatInstr(99,103)]);
(438, [EatInstr(99,471)]);
(55, [EatInstr(100,104)]);
(439, [EatInstr(114,472)]);
(56, [EatInstr(100,105)]);
(440, [EatInstr(45,473)]);
(57, [EatInstr(101,106)]);
(441, [EatInstr(105,474)]);
(58, [EatInstr(102,107)]);
(442, [EatInstr(115,475)]);
(59, [EatInstr(105,108)]);
(443, [EatInstr(97,476)]);
(60, [EatInstr(108,109)]);
(444, [EatInstr(97,477)]);
(61, [EatInstr(108,110)]);
(445, [EatInstr(101,478)]);
(62, [EatInstr(112,111)]);
(446, [AAction2Instr(__a84,249)]);
(63, [EatInstr(112,112)]);
(447, [EatInstr(45,479)]);
(64, [EatInstr(112,113)]);
(448, [EatInstr(108,480)]);
(65, [EatInstr(112,114)]);
(449, [AAction2Instr(__a85,249)]);
(66, [EatInstr(112,115)]);
(450, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,481)]);
(67, [EatInstr(112,116)]);
(451, [AAction2Instr(__a86,482)]);
(68, [AAction2Instr(__a41,118);AAction2Instr(__a40,117)]);
(452, [AAction2Instr(__a87,155)]);
(69, [EatInstr(117,121);EatInstr(114,120);EatInstr(99,119)]);
(453, [AAction2Instr(__a88,155)]);
(70, [EatInstr(45,122)]);
(454, [EatInstr(45,483)]);
(71, [EatInstr(45,123)]);
(455, [EatInstr(104,484)]);
(72, [EatInstr(45,124)]);
(456, [EatInstr(101,485)]);
(73, [EatInstr(45,125)]);
(457, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,486)]);
(74, [EatInstr(45,126)]);
(458, [EatInstr(101,487)]);
(75, [EatInstr(45,127)]);
(459, [EatInstr(108,488)]);
(76, [EatInstr(45,128)]);
(460, [EatInstr(101,489)]);
(77, [EatInstr(45,129)]);
(461, [EatInstr(116,490)]);
(78, [EatInstr(45,130)]);
(462, [EatInstr(116,491)]);
(79, [EatInstr(45,131)]);
(463, [EatInstr(116,492)]);
(80, [EatInstr(45,132)]);
(464, [AAction2Instr(__a89,494);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,493)]);
(81, [EatInstr(45,133)]);
(465, [AAction2Instr(__a90,292)]);
(82, [EatInstr(45,134)]);
(466, [EatInstr(97,495)]);
(83, [EatInstr(45,135)]);
(467, [AAction2Instr(__a91,292)]);
(84, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,136)]);
(468, [AAction2Instr(__a92,496)]);
(85, [AAction2Instr(__a43,138);AAction2Instr(__a42,137)]);
(86, [ALookaheadInstr(false,CfgLA (1,264),139)]);
(469, [EatInstr(101,497)]);
(470, [EatInstr(98,498)]);
(87, [EatInstr(114,140)]);
(471, [EatInstr(115,499)]);
(88, [EatInstr(115,141)]);
(472, [EatInstr(101,500)]);
(89, [EatInstr(121,142)]);
(473, [EatInstr(104,501)]);
(90, [EatInstr(117,143)]);
(474, [EatInstr(122,502)]);
(91, [EatInstr(104,144)]);
(475, [EatInstr(116,503)]);
(92, [EatInstr(105,145)]);
(476, [EatInstr(121,504)]);
(93, [EatInstr(101,146)]);
(477, [EatInstr(100,505)]);
(94, [EatInstr(116,147)]);
(478, [EatInstr(115,506)]);
(95, [EatInstr(117,148)]);
(479, [EatInstr(99,507)]);
(96, [EatInstr(99,149)]);
(480, [EatInstr(97,508)]);
(97, [EatInstr(115,150)]);
(481, [AAction2Instr(__a93,249)]);
(98, [EatInstr(111,151)]);
(482, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,509)]);
(99, [EatInstr(112,152)]);
(483, [EatInstr(97,510)]);
(100, [EatInstr(101,153)]);
(484, [EatInstr(101,511)]);
(101, [EatInstr(99,154)]);
(485, [EatInstr(45,512)]);
(102, [AAction2Instr(__a44,155)]);
(486, [AAction2Instr(__a94,155)]);
(103, [EatInstr(111,156)]);
(487, [EatInstr(100,513)]);
(104, [EatInstr(105,157)]);
(488, [EatInstr(97,514)]);
(105, [EatInstr(111,158)]);
(489, [EatInstr(118,515)]);
(106, [EatInstr(120,159)]);
(490, [EatInstr(101,516)]);
(107, [EatInstr(117,160)]);
(491, [EatInstr(101,517)]);
(108, [EatInstr(110,161)]);
(492, [EatInstr(101,518)]);
(109, [EatInstr(111,162)]);
(493, [AAction2Instr(__a95,464)]);
(110, [EatInstr(114,163)]);
(494, [AWhenInstr3(__p97,__p96,519)]);
(111, [EatInstr(114,164)]);
(495, [EatInstr(114,520)]);
(112, [EatInstr(114,165)]);
(496, [EatInstr(116,523);EatInstr(112,522);EatInstr(102,521)]);
(113, [EatInstr(114,166)]);
(497, [EatInstr(110,524)]);
(114, [EatInstr(114,167)]);
(498, [EatInstr(101,525)]);
(115, [EatInstr(114,168)]);
(499, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,526)]);
(116, [EatInstr(114,169)]);
(500, [EatInstr(103,527)]);
(117, [EatInstr(115,170)]);
(501, [EatInstr(105,528)]);
(118, [AAction2Instr(__a46,172);AAction2Instr(__a45,171)]);
(502, [EatInstr(101,529)]);
(119, [EatInstr(111,173)]);
(503, [EatInstr(111,530)]);
(120, [EatInstr(111,174)]);
(504, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,531)]);
(121, [EatInstr(110,175)]);
(505, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,532)]);
(122, [EatInstr(97,176)]);
(506, [EatInstr(99,533)]);
(123, [EatInstr(98,177)]);
(507, [EatInstr(111,534)]);
(124, [EatInstr(99,178)]);
(508, [EatInstr(114,535)]);
(125, [EatInstr(99,179)]);
(509, [AAction2Instr(__a98,381)]);
(126, [EatInstr(105,180)]);
(510, [EatInstr(110,536)]);
(127, [EatInstr(105,181)]);
(511, [EatInstr(97,537)]);
(128, [EatInstr(109,182)]);
(512, [EatInstr(97,538)]);
(129, [EatInstr(110,183)]);
(513, [EatInstr(115,539)]);
(130, [EatInstr(117,184)]);
(514, [EatInstr(98,540)]);
(131, [EatInstr(110,185)]);
(515, [EatInstr(97,541)]);
(132, [EatInstr(108,186)]);
(516, [EatInstr(45,542)]);
(133, [EatInstr(110,187)]);
(517, [EatInstr(45,543)]);
(134, [EatInstr(111,188)]);
(518, [EatInstr(45,544)]);
(135, [EatInstr(118,189)]);
(519, [AAction2Instr(__a99,545)]);
(136, [AAction2Instr(__a47,190)]);
(520, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,546)]);
(137, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder3,191)]);
(521, [EatInstr(117,547)]);
(138, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,192)]);
(522, [EatInstr(101,548)]);
(139, [CompleteInstr(269)]);
(523, [EatInstr(120,549)]);
(140, [EatInstr(105,193)]);
(524, [EatInstr(115,550)]);
(141, [EatInstr(101,194)]);
(525, [EatInstr(108,551)]);
(142, [EatInstr(114,195)]);
(526, [AAction2Instr(__a100,292)]);
(143, [EatInstr(103,196)]);
(527, [EatInstr(117,552)]);
(144, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,197)]);
(528, [EatInstr(115,553)]);
(145, [EatInstr(110,198)]);
(529, [EatInstr(45,554)]);
(146, [EatInstr(114,199)]);
(530, [EatInstr(114,555)]);
(147, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,200)]);
(531, [AAction2Instr(__a101,292)]);
(148, [EatInstr(115,201)]);
(532, [AAction2Instr(__a102,292)]);
(149, [EatInstr(101,202)]);
(533, [EatInstr(101,556)]);
(150, [EatInstr(101,203)]);
(534, [EatInstr(114,557)]);
(151, [EatInstr(108,204)]);
(535, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,558)]);
(152, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,205)]);
(536, [EatInstr(97,559)]);
(153, [EatInstr(99,206)]);
(537, [EatInstr(100,560)]);
(154, [AAction2Instr(__a48,207)]);
(538, [EatInstr(110,561)]);
(155, [AAction2Instr(__a49,208)]);
(539, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,562)]);
(156, [EatInstr(109,209)]);
(540, [EatInstr(108,563)]);
(157, [EatInstr(115,210)]);
(541, [EatInstr(110,564)]);
(158, [EatInstr(116,211)]);
(542, [EatInstr(97,565)]);
(159, [EatInstr(116,212)]);
(543, [EatInstr(100,566)]);
(160, [EatInstr(115,213)]);
(544, [EatInstr(100,567)]);
(161, [EatInstr(102,214)]);
(545, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,568)]);
(162, [EatInstr(111,215)]);
(546, [AAction2Instr(__a103,569)]);
(163, [EatInstr(49,216)]);
(547, [EatInstr(110,570)]);
(164, [EatInstr(101,217)]);
(548, [EatInstr(103,571)]);
(165, [EatInstr(105,218)]);
(549, [AAction2Instr(__a104,572)]);
(166, [EatInstr(105,219)]);
(550, [EatInstr(105,573)]);
(167, [EatInstr(105,220)]);
(551, [EatInstr(115,574)]);
(168, [EatInstr(105,221)]);
(552, [EatInstr(108,575)]);
(169, [EatInstr(105,222)]);
(553, [EatInstr(116,576)]);
(170, [EatInstr(116,223)]);
(554, [EatInstr(104,577)]);
(171, [EatInstr(116,224)]);
(555, [EatInstr(121,578)]);
(172, [EatInstr(116,225)]);
(556, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,579)]);
(173, [EatInstr(117,226)]);
(557, [EatInstr(101,580)]);
(174, [EatInstr(111,227)]);
(558, [AAction2Instr(__a105,249)]);
(175, [EatInstr(114,228)]);
(559, [EatInstr(108,581)]);
(176, [EatInstr(102,229)]);
(560, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,582)]);
(177, [EatInstr(97,230)]);
(561, [EatInstr(97,583)]);
(178, [EatInstr(97,231)]);
(562, [AAction2Instr(__a106,155)]);
(179, [EatInstr(104,232)]);
(563, [EatInstr(101,584)]);
(180, [EatInstr(110,233)]);
(564, [EatInstr(99,585)]);
(181, [EatInstr(110,234)]);
(565, [EatInstr(99,586)]);
(182, [EatInstr(101,235)]);
(566, [EatInstr(121,587)]);
(183, [EatInstr(111,236)]);
(567, [EatInstr(121,588)]);
(184, [EatInstr(110,237)]);
(568, [AAction2Instr(__a107,292)]);
(185, [EatInstr(111,238)]);
(569, [AAction2Instr(__a108,590);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,589)]);
(186, [EatInstr(111,239)]);
(570, [AAction2Instr(__a109,572)]);
(187, [EatInstr(111,240)]);
(571, [EatInstr(45,591);AAction2Instr(__a110,572)]);
(188, [EatInstr(110,241)]);
(572, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,592)]);
(189, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,242)]);
(573, [EatInstr(116,593)]);
(190, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,243)]);
(574, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,594)]);
(191, [AAction2Instr(__a50,85)]);
(575, [EatInstr(97,595)]);
(192, [AAction2Instr(__a51,244)]);
(576, [EatInstr(111,596)]);
(193, [EatInstr(98,245)]);
(577, [EatInstr(105,597)]);
(194, [EatInstr(45,246)]);
(578, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,598)]);
(195, [EatInstr(117,247)]);
(579, [AAction2Instr(__a111,292)]);
(196, [EatInstr(97,248)]);
(580, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,599)]);
(197, [AAction2Instr(__a52,249)]);
(581, [EatInstr(121,600)]);
(198, [EatInstr(101,250)]);
(582, [AAction2Instr(__a112,155)]);
(199, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,251)]);
(583, [EatInstr(108,601)]);
(200, [AAction2Instr(__a53,249)]);
(584, [EatInstr(45,602)]);
(201, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,252)]);
(585, [EatInstr(101,603)]);
(202, [EatInstr(100,253)]);
(586, [EatInstr(116,604)]);
(203, [EatInstr(116,254)]);
(587, [EatInstr(112,605)]);
(204, [EatInstr(108,255)]);
(588, [EatInstr(112,606)]);
(205, [AAction2Instr(__a54,249)]);
(589, [AAction2Instr(__a113,569)]);
(206, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,256)]);
(590, [AWhenInstr3(__p115,__p114,607)]);
(207, [AAction2Instr(__a55,258);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,257)]);
(591, [EatInstr(115,608)]);
(208, [CompleteInstr(272)]);
(592, [AAction2Instr(__a116,292)]);
(209, [EatInstr(112,259)]);
(593, [EatInstr(105,609)]);
(210, [EatInstr(112,260)]);
(594, [AAction2Instr(__a117,292)]);
(211, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,261)]);
(595, [EatInstr(114,610)]);
(212, [EatInstr(114,262)]);
(596, [EatInstr(114,611)]);
(213, [EatInstr(101,263)]);
(597, [EatInstr(115,612)]);
(214, [EatInstr(111,264)]);
(598, [AAction2Instr(__a118,292)]);
(215, [EatInstr(107,265)]);
(599, [AAction2Instr(__a119,249)]);
(216, [EatInstr(45,266)]);
(600, [EatInstr(115,613)]);
(217, [EatInstr(99,267)]);
(601, [EatInstr(121,614)]);
(218, [EatInstr(110,268)]);
(602, [EatInstr(112,615)]);
(219, [EatInstr(110,269)]);
(603, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,616)]);
(220, [EatInstr(110,270)]);
(604, [EatInstr(105,617)]);
(221, [EatInstr(110,271)]);
(605, [EatInstr(103,618)]);
(222, [EatInstr(110,272)]);
(606, [EatInstr(103,619)]);
(223, [EatInstr(114,273)]);
(607, [AAction2Instr(__a120,620)]);
(224, [EatInstr(114,274)]);
(608, [EatInstr(116,621)]);
(225, [EatInstr(114,275)]);
(609, [EatInstr(118,622)]);
(226, [EatInstr(110,276)]);
(610, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,623)]);
(227, [EatInstr(116,277)]);
(611, [EatInstr(121,624)]);
(228, [EatInstr(111,278)]);
(612, [EatInstr(116,625)]);
(229, [EatInstr(116,279)]);
(613, [EatInstr(105,626)]);
(230, [EatInstr(99,280)]);
(614, [EatInstr(115,627)]);
(231, [EatInstr(115,281)]);
(615, [EatInstr(114,628)]);
(232, [EatInstr(101,282)]);
(616, [AAction2Instr(__a121,155)]);
(233, [EatInstr(108,283)]);
(617, [EatInstr(111,629)]);
(234, [EatInstr(108,284)]);
(618, [EatInstr(101,630)]);
(235, [EatInstr(109,285)]);
(619, [EatInstr(101,631)]);
(236, [EatInstr(45,286)]);
(620, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,632)]);
(237, [EatInstr(105,287)]);
(621, [EatInstr(114,633)]);
(238, [EatInstr(45,288)]);
(622, [EatInstr(101,634)]);
(239, [EatInstr(111,289)]);
(623, [AAction2Instr(__a122,292)]);
(240, [EatInstr(45,290)]);
(624, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,635)]);
(241, [EatInstr(108,291)]);
(625, [EatInstr(111,636)]);
(242, [AAction2Instr(__a56,292)]);
(626, [EatInstr(115,637)]);
(243, [AAction2Instr(__a57,292)]);
(627, [EatInstr(105,638)]);
(244, [CompleteInstr(267)]);
(628, [EatInstr(101,639)]);
(245, [EatInstr(117,293)]);
(629, [EatInstr(110,640)]);
(246, [EatInstr(117,294)]);
(630, [EatInstr(110,641)]);
(247, [EatInstr(108,295)]);
(631, [EatInstr(110,642)]);
(248, [EatInstr(114,296)]);
(632, [AAction2Instr(__a123,292)]);
(249, [CompleteInstr(271)]);
(633, [EatInstr(105,643)]);
(250, [EatInstr(45,297)]);
(634, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,644)]);
(251, [AAction2Instr(__a58,249)]);
(635, [AAction2Instr(__a124,292)]);
(252, [AAction2Instr(__a59,249)]);
(636, [EatInstr(114,645)]);
(253, [EatInstr(101,298)]);
(637, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,646)]);
(254, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,299)]);
(638, [EatInstr(115,647)]);
(255, [EatInstr(45,300)]);
(639, [EatInstr(100,648)]);
(256, [AAction2Instr(__a60,301)]);
(640, [EatInstr(115,649)]);
(257, [AAction2Instr(__a61,207)]);
(641, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,650)]);
(258, [AWhenInstr3(__p63,__p62,302)]);
(642, [EatInstr(45,651)]);
(259, [EatInstr(105,303)]);
(643, [EatInstr(99,652)]);
(260, [EatInstr(97,304)]);
(644, [AAction2Instr(__a125,292)]);
(261, [AAction2Instr(__a64,155)]);
(645, [EatInstr(121,653)]);
(262, [EatInstr(97,305)]);
(646, [AAction2Instr(__a126,155)]);
(263, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,306)]);
(647, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,654)]);
(264, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,307)]);
(648, [EatInstr(105,655)]);
(265, [EatInstr(97,308)]);
(649, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,656)]);
(266, [EatInstr(108,309)]);
(650, [AAction2Instr(__a127,155)]);
(267, [EatInstr(101,310)]);
(651, [EatInstr(115,657)]);
(268, [EatInstr(116,311)]);
(652, [EatInstr(116,658)]);
(269, [EatInstr(116,312)]);
(653, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,659)]);
(270, [EatInstr(116,313)]);
(654, [AAction2Instr(__a128,155)]);
(271, [EatInstr(116,314)]);
(655, [EatInstr(99,660)]);
(272, [EatInstr(116,315)]);
(656, [AAction2Instr(__a129,155)]);
(273, [EatInstr(105,316)]);
(657, [EatInstr(99,661)]);
(274, [EatInstr(97,317)]);
(658, [AAction2Instr(__a130,572)]);
(275, [EatInstr(97,318)]);
(659, [AAction2Instr(__a131,292)]);
(276, [EatInstr(116,319)]);
(660, [EatInstr(97,662)]);
(277, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,320)]);
(661, [EatInstr(97,663)]);
(278, [EatInstr(108,321)]);
(662, [EatInstr(116,664)]);
(279, [EatInstr(101,322)]);
(663, [EatInstr(110,665)]);
(280, [EatInstr(107,323)]);
(664, [EatInstr(101,666)]);
(281, [EatInstr(101,324)]);
(665, [EatInstr(110,667)]);
(282, [EatInstr(99,325)]);
(666, [EatInstr(115,668)]);
(283, [EatInstr(105,326)]);
(667, [EatInstr(101,669)]);
(284, [EatInstr(105,327)]);
(668, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,670)]);
(285, [EatInstr(111,328)]);
(669, [EatInstr(114,671)]);
(286, [EatInstr(109,329)]);
(670, [AAction2Instr(__a132,155)]);
(287, [EatInstr(116,330)]);
(671, [EatInstr(108,672)]);
(288, [EatInstr(114,331)]);
(672, [EatInstr(101,673)]);
(289, [EatInstr(107,332)]);
(673, [EatInstr(115,674)]);
(290, [EatInstr(99,333)]);
(674, [EatInstr(115,675)]);
(291, [EatInstr(121,334)]);
(675, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,676)]);
(292, [AAction2Instr(__a65,335)]);
(676, [AAction2Instr(__a133,155)]);
(293, [EatInstr(116,336)]);
(294, [EatInstr(110,337)]);
(295, [EatInstr(101,338)]);
(296, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,339)]);
(297, [EatInstr(114,340)]);
(298, [EatInstr(110,341)]);
(299, [AAction2Instr(__a66,249)]);
(300, [EatInstr(115,342)]);
(301, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,343)]);
(302, [AAction2Instr(__a67,344)]);
(303, [EatInstr(108,345)]);
(304, [EatInstr(116,346)]);
(305, [EatInstr(99,347)]);
(306, [AAction2Instr(__a68,155)]);
(307, [AAction2Instr(__a69,155)]);
(308, [EatInstr(104,348)]);
(309, [EatInstr(111,349)]);
(310, [EatInstr(100,350)]);
(311, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,351)]);
(312, [EatInstr(45,352)]);
(313, [EatInstr(45,353)]);
(314, [EatInstr(45,354)]);
(315, [EatInstr(45,355)]);
(316, [EatInstr(112,356)]);
(317, [EatInstr(110,357)]);
(318, [EatInstr(110,358)]);
(319, [EatInstr(101,359)]);
(320, [AAction2Instr(__a70,360)]);
(321, [EatInstr(108,361)]);
(322, [EatInstr(114,362)]);
(323, [EatInstr(101,363)]);
(324, [EatInstr(45,364)]);
(325, [EatInstr(107,365)]);
(326, [EatInstr(110,366)]);
(327, [EatInstr(110,367)]);
(328, [EatInstr(105,368)]);
(329, [EatInstr(101,369)]);
(330, [EatInstr(45,370)]);
(331, [EatInstr(101,371)]);
(332, [EatInstr(97,372)]);
(333, [EatInstr(111,373)]);
(334, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,374)]);
(335, [CompleteInstr(273)]);
(336, [EatInstr(101,375)]);
(337, [EatInstr(100,376)]);
(338, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,377)]);
(339, [AAction2Instr(__a71,249)]);
(340, [EatInstr(101,378)]);
(341, [EatInstr(99,379)]);
(342, [EatInstr(116,380)]);
(343, [AAction2Instr(__a72,381)]);
(344, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,382)]);
(345, [EatInstr(101,383)]);
(346, [EatInstr(99,384)]);
(347, [EatInstr(116,385)]);
(348, [EatInstr(101,386)]);
(349, [EatInstr(111,387)]);
(350, [EatInstr(101,388)]);
(351, [AAction2Instr(__a73,155)]);
(352, [EatInstr(103,389)]);
(353, [EatInstr(110,390)]);
(354, [EatInstr(110,391)]);
(355, [EatInstr(114,392)]);
(356, [EatInstr(45,393)]);
(357, [EatInstr(115,394)]);
(358, [EatInstr(115,395)]);
(359, [EatInstr(114,396)]);
(360, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,397)]);
(361, [EatInstr(45,398)]);
(362, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,399)]);
(363, [EatInstr(110,400)]);
(364, [EatInstr(105,401)]);
(365, [EatInstr(45,402)]);
(366, [EatInstr(101,403)]);
(367, [EatInstr(101,404)]);
(368, [EatInstr(122,405)]);
(369, [EatInstr(109,406)]);
(370, [EatInstr(104,407)]);
(371, [EatInstr(112,408)]);
(372, [EatInstr(104,409)]);
(373, [EatInstr(97,410)]);
(374, [AAction2Instr(__a74,292)]);
(375, [EatInstr(115,411)]);
(376, [EatInstr(101,412)]);
(377, [AAction2Instr(__a75,249)]);
(378, [EatInstr(103,413)]);
(379, [EatInstr(101,414)]);
(380, [EatInstr(97,415)]);
(381, [AAction2Instr(__a77,417);AAction2Instr(__a76,416)]);
(382, [AAction2Instr(__a78,155)]);
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
