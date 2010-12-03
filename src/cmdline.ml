
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
 | (1296) -> ( Compileopt.lookahead := true )
 | (1301) -> ( Compileopt.coalesce := false )
 | (1306) -> ( only := true )
 | (1316) -> (
 (let _x29 = (ignore (*1317*) (_n()); _n())
  in (ignore (*1318*) (_n()); 
 (let _x28 = (ignore (*1319*) (_n()); _n())
  in (ignore (*1320*) (_n()); 
 (let x = (ignore (*1321*) (_n()); Yak.YkBuf.get_string _x29 _x28 ykinput)
  in (ignore (*1323*) (_n());  roots := x::!roots )
 ))
 ))
 ))
 | (1341) -> (
 (let _x34 = (ignore (*1342*) (_n()); _n())
  in (ignore (*1343*) (_n()); 
 (let _x33 = (ignore (*1344*) (_n()); _n())
  in (ignore (*1345*) (_n()); 
 (let n = (ignore (*1346*) (_n()); Yak.YkBuf.get_string _x34 _x33 ykinput)
  in (ignore (*1348*) (_n());  Compileopt.unroll_star_n := (int_of_string n) )
 ))
 ))
 ))
 | (1353) -> ( Yak.Logging.add_features Yak.Logging.Features.verbose )
 | _(*1361*) -> (
 (let _x38 = (ignore (*1362*) (_n()); _n())
  in (ignore (*1363*) (_n()); 
 (let _x37 = (ignore (*1364*) (_n()); _n())
  in (ignore (*1365*) (_n()); 
 (let f = (ignore (*1366*) (_n()); Yak.YkBuf.get_string _x38 _x37 ykinput)
  in (ignore (*1368*) (_n());  files := f::!files )
 ))
 ))
 ))
 ) in (ignore (*1369*) (_n()); _x40)
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
 | 1309 ->
 (fun pos_ -> let _x89 _x27  = _t(fun _(*1313*) pos_ -> let _x90 _x26  = _t(fun _(*1316*) pos_ -> let _x92 _x91  = _t(fun _(*1319*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x26)) in _t(fun _(*1317*) pos_ -> Yk_delay(_x92 ((_wv0)) ,_x27))) in _t(fun _(*1314*) pos_ -> _x90 (pos_) )) in _t(fun _(*1310*) pos_ -> _x89 (pos_) ))
 | 1326 ->
 (fun pos_ -> let _x81 _x32  = _t(fun _(*1329*) pos_ -> let _x83 _x82  = _t(fun _(*1338*) pos_ -> let _x86 _x31  = _t(fun _(*1341*) pos_ -> let _x88 _x87  = _t(fun _(*1344*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x31)) in _t(fun _(*1342*) pos_ -> Yk_delay(_x88 ((_wv0)) ,_x32))) in _t(fun _(*1339*) pos_ -> _x86 (pos_) )) in _t(fun _(*1330*) pos_ -> let _x84 _x30  = _t(function
 | 1336 ->
 (fun pos_ -> Yk_when(_x30>=1))
 | _(*1337*) ->
 (fun pos_ -> _x83 (ignore((_wv0));_wv0) )) in _t(fun _(*1331*) pos_ -> let rec _x85 _x30  = _t(function
 | 1332 ->
 (fun pos_ -> _x84 (_x30) )
 | _(*1334*) ->
 (fun pos_ -> _x85 (_x30+1) )) in _x85 (0) ))) in _t(fun _(*1327*) pos_ -> _x81 (pos_) ))
 | 1350 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1354*) ->
 (fun pos_ -> let _x77 _x36  = _t(fun _(*1358*) pos_ -> let _x78 _x35  = _t(fun _(*1361*) pos_ -> let _x80 _x79  = _t(fun _(*1364*) pos_ -> Yk_delay(Yk_done(ignore(ignore((_wv0));_wv0);_wv0),_x35)) in _t(fun _(*1362*) pos_ -> Yk_delay(_x80 ((_wv0)) ,_x36))) in _t(fun _(*1359*) pos_ -> _x78 (pos_) )) in _t(fun _(*1355*) pos_ -> _x77 (pos_) ))),_x76))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a20 = fun p v -> _d 1204 p (_d 1203 p (v));;
let __a18 = _d 1170;;
let __a100 = _p 1041;;
let __a121 = _p 1153;;
let __a25 = _d 1283;;
let __a22 = _d 1233;;
let __a86 = _p 1323;;
let __a72 = _p 1104;;
let __a54 = _p 1044;;
let __p110 = _dwhen 1336;;
let __a19 = _d 1175;;
let __a9 = _d 1125;;
let __a120 = _p 1216;;
let __a98 = fun p v -> _d 1331 p (_d 1330 p (_d 1329 p (_d 1327 p (_d 1326 p (v)))));;
let __a49 = _p 1047;;
let __a37 = fun p v -> _d 1211 p (_d 1210 p (v));;
let __a26 = _d 1288;;
let __a4 = _d 1018;;
let __a69 = _p 1158;;
let __a2 = fun p v -> _p 1063 p (_x75 p (v));;
let __a5 = fun p v -> _p 1067 p (_p 1066 p (_d 1065 p (v)));;
let __a94 = fun p v -> _p 1120 p (_p 1119 p (_p 1118 p (_p 1117 p (_ddelay 1116 p (_p 1115 p (_ddelay 1114 p (_d_and_push 1113 p (_d 1111 p (_d 1110 p (v))))))))));;
let __a1 = fun p v -> _d 1017 p (_d 1016 p (_x49 p (v)));;
let __a55 = _p 1050;;
let __a30 = _d 1350;;
let __a10 = _d 1130;;
let __a33 = _d 1020;;
let __a125 = _p 1221;;
let __a6 = _d 1071;;
let __a45 = _p 1222;;
let __a90 = _p 1163;;
let __a81 = _p 1053;;
let __a29 = _d 1303;;
let __a27 = _d 1293;;
let __a44 = fun p v -> _d 1185 p (_d 1184 p (_d 1183 p (_d 1181 p (_d 1180 p (v)))));;
let __a0 = fun p v -> _p 1001 p (_p 1000 p (v));;
let __a11 = _d 1135;;
let __a62 = _p 1056;;
let __a51 = _d 1186;;
let __a7 = _d 1076;;
let __a101 = _p 1168;;
let __a28 = _d 1298;;
let __a23 = _d 1248;;
let __a57 = _d 1188;;
let __a89 = _p 1059;;
let __p34 = _dnext 1023;;
let __a3 = fun p v -> _p 1223 p (_x101 p (v));;
let __a12 = _d 1140;;
let __a46 = _p 1010;;
let __a102 = _p 1281;;
let __p35 = _dwhen 1022;;
let __a8 = _d 1081;;
let __a87 = _p 1231;;
let __a50 = _p 1062;;
let __a124 = _p 1173;;
let __a43 = fun p v -> _p 1366 p (_p 1365 p (_ddelay 1364 p (_p 1363 p (_ddelay 1362 p (_d_and_push 1361 p (_d 1359 p (_d 1358 p (v))))))));;
let __a24 = _d 1253;;
let __a83 = _p 1123;;
let __a47 = _p 1013;;
let __a63 = fun p v -> _p 1200 p (_p 1199 p (_ddelay 1198 p (_p 1197 p (_ddelay 1196 p (_d_and_push 1195 p (_d 1193 p (_d 1192 p (v))))))));;
let __a66 = fun p v -> _d 1310 p (_d 1309 p (v));;
let __a13 = _d 1145;;
let __a96 = _p 1286;;
let __a36 = _d 1206;;
let __a88 = _p 1236;;
let __p58 = _dnext 1191;;
let __a73 = fun p v -> _p 1121 p (_d_and_push 1103 p (v));;
let __a117 = _p 1348;;
let __a115 = _p 1178;;
let __a104 = _p 1238;;
let __a78 = _p 1128;;
let __a40 = _p 1069;;
let __a39 = fun p v -> _p 1011 p (_p 1006 p (v));;
let __p59 = _dwhen 1190;;
let __a99 = _p 1240;;
let __a14 = _d 1150;;
let __a116 = _p 1291;;
let __a106 = _p 1301;;
let __a38 = fun p v -> _p 1008 p (_p 1007 p (v));;
let __a123 = _p 1242;;
let __a52 = _p 1353;;
let __a64 = _p 1133;;
let __a41 = _d 1213;;
let __a105 = _p 1244;;
let __a77 = _p 1074;;
let __a95 = fun p v -> _p 1279 p (_p 1278 p (_ddelay 1277 p (_p 1276 p (_ddelay 1275 p (_d_and_push 1274 p (_d 1272 p (_d 1271 p (v))))))));;
let __a85 = _d 1265;;
let __a15 = _d 1155;;
let __a111 = _p 1246;;
let __a97 = _p 1296;;
let __a70 = _p 1306;;
let __a80 = _p 1026;;
let __a76 = fun p v -> _p 1229 p (_p 1228 p (v));;
let __a91 = _d 1267;;
let __a65 = _p 1138;;
let __a79 = fun p v -> _d 1264 p (_d 1263 p (_d 1262 p (_d 1260 p (_d 1259 p (v)))));;
let __a42 = _d 1218;;
let __a113 = _p 1029;;
let __a84 = _p 1079;;
let __a114 = fun p v -> _p 1346 p (_p 1345 p (_ddelay 1344 p (_p 1343 p (_ddelay 1342 p (_d_and_push 1341 p (_d 1339 p (_d 1338 p (v))))))));;
let __a68 = fun p v -> _d_and_push 1102 p (_p 1101 p (_p 1100 p (_p 1099 p (_p 1098 p (_ddelay 1097 p (_d_and_push 1096 p (_ddelay 1095 p (_d_and_push 1094 p (_d 1092 p (_d 1091 p (v)))))))))));;
let __a32 = fun p v -> _p 1005 p (_p 1004 p (_p 1003 p (v)));;
let __a16 = _d 1160;;
let __a118 = _p 1251;;
let __a82 = fun p v -> _d 1107 p (_d 1106 p (v));;
let __a103 = _d 1332;;
let __a71 = _p 1032;;
let __a74 = _p 1202;;
let __a119 = _p 1143;;
let __a56 = fun p v -> _d 1088 p (_d 1087 p (v));;
let __a108 = _d 1334;;
let __a60 = _p 1084;;
let __a21 = _d 1225;;
let __a17 = _d 1165;;
let __a67 = _p 1035;;
let __a112 = _p 1256;;
let __p92 = _dnext 1270;;
let __a31 = fun p v -> _d 1355 p (_d 1354 p (v));;
let __a107 = _p 1148;;
let __p93 = _dwhen 1269;;
let __a53 = _p 1368;;
let __a48 = _p 1038;;
let __a122 = _p 1209;;
let __a75 = fun p v -> _p 1321 p (_p 1320 p (_ddelay 1319 p (_p 1318 p (_ddelay 1317 p (_d_and_push 1316 p (_d 1314 p (_d 1313 p (v))))))));;
let __a61 = _p 1369;;
let __p109 = _dnext 1337;;
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
(383, [EatInstr(114,414)]);
(0, [ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(384, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,415)]);
(1, [EatInstr(127,12);EatInstr(126,12);EatInstr(125,12);EatInstr(124,12);EatInstr(123,12);EatInstr(122,12);EatInstr(121,12);EatInstr(120,12);EatInstr(119,12);EatInstr(118,12);EatInstr(117,12);EatInstr(116,12);EatInstr(115,12);EatInstr(114,12);EatInstr(113,12);EatInstr(112,12);EatInstr(111,12);EatInstr(110,12);EatInstr(109,12);EatInstr(108,12);EatInstr(107,12);EatInstr(106,12);EatInstr(105,12);EatInstr(104,12);EatInstr(103,12);EatInstr(102,12);EatInstr(101,12);EatInstr(100,12);EatInstr(99,12);EatInstr(98,12);EatInstr(97,12);EatInstr(96,12);EatInstr(95,12);EatInstr(94,12);EatInstr(93,12);EatInstr(92,12);EatInstr(91,12);EatInstr(90,12);EatInstr(89,12);EatInstr(88,12);EatInstr(87,12);EatInstr(86,12);EatInstr(85,12);EatInstr(84,12);EatInstr(83,12);EatInstr(82,12);EatInstr(81,12);EatInstr(80,12);EatInstr(79,12);EatInstr(78,12);EatInstr(77,12);EatInstr(76,12);EatInstr(75,12);EatInstr(74,12);EatInstr(73,12);EatInstr(72,12);EatInstr(71,12);EatInstr(70,12);EatInstr(69,12);EatInstr(68,12);EatInstr(67,12);EatInstr(66,12);EatInstr(65,12);EatInstr(64,12);EatInstr(63,12);EatInstr(62,12);EatInstr(61,12);EatInstr(60,12);EatInstr(59,12);EatInstr(58,12);EatInstr(57,12);EatInstr(56,12);EatInstr(55,12);EatInstr(54,12);EatInstr(53,12);EatInstr(52,12);EatInstr(51,12);EatInstr(50,12);EatInstr(49,12);EatInstr(48,12);EatInstr(47,12);EatInstr(46,12);EatInstr(45,12);EatInstr(44,12);EatInstr(43,12);EatInstr(42,12);EatInstr(41,12);EatInstr(40,12);EatInstr(39,12);EatInstr(38,12);EatInstr(37,12);EatInstr(36,12);EatInstr(35,12);EatInstr(34,12);EatInstr(33,12);EatInstr(32,12);EatInstr(31,12);EatInstr(30,12);EatInstr(29,12);EatInstr(28,12);EatInstr(27,12);EatInstr(26,12);EatInstr(25,12);EatInstr(24,12);EatInstr(23,12);EatInstr(22,12);EatInstr(21,12);EatInstr(20,12);EatInstr(19,12);EatInstr(18,12);EatInstr(17,12);EatInstr(16,12);EatInstr(15,12);EatInstr(14,12);EatInstr(13,12);EatInstr(12,12);EatInstr(11,12);EatInstr(10,12);EatInstr(9,12);EatInstr(8,12);EatInstr(7,12);EatInstr(6,12);EatInstr(5,12);EatInstr(4,12);EatInstr(3,12);EatInstr(2,12);EatInstr(1,12)]);
(385, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,416)]);
(2, [EatInstr(57,13);EatInstr(56,13);EatInstr(55,13);EatInstr(54,13);EatInstr(53,13);EatInstr(52,13);EatInstr(51,13);EatInstr(50,13);EatInstr(49,13);EatInstr(48,13)]);
(386, [AAction2Instr(__a77,147)]);
(3, [EatInstr(255,14);EatInstr(254,14);EatInstr(253,14);EatInstr(252,14);EatInstr(251,14);EatInstr(250,14);EatInstr(249,14);EatInstr(248,14);EatInstr(247,14);EatInstr(246,14);EatInstr(245,14);EatInstr(244,14);EatInstr(243,14);EatInstr(242,14);EatInstr(241,14);EatInstr(240,14);EatInstr(239,14);EatInstr(238,14);EatInstr(237,14);EatInstr(236,14);EatInstr(235,14);EatInstr(234,14);EatInstr(233,14);EatInstr(232,14);EatInstr(231,14);EatInstr(230,14);EatInstr(229,14);EatInstr(228,14);EatInstr(227,14);EatInstr(226,14);EatInstr(225,14);EatInstr(224,14);EatInstr(223,14);EatInstr(222,14);EatInstr(221,14);EatInstr(220,14);EatInstr(219,14);EatInstr(218,14);EatInstr(217,14);EatInstr(216,14);EatInstr(215,14);EatInstr(214,14);EatInstr(213,14);EatInstr(212,14);EatInstr(211,14);EatInstr(210,14);EatInstr(209,14);EatInstr(208,14);EatInstr(207,14);EatInstr(206,14);EatInstr(205,14);EatInstr(204,14);EatInstr(203,14);EatInstr(202,14);EatInstr(201,14);EatInstr(200,14);EatInstr(199,14);EatInstr(198,14);EatInstr(197,14);EatInstr(196,14);EatInstr(195,14);EatInstr(194,14);EatInstr(193,14);EatInstr(192,14);EatInstr(191,14);EatInstr(190,14);EatInstr(189,14);EatInstr(188,14);EatInstr(187,14);EatInstr(186,14);EatInstr(185,14);EatInstr(184,14);EatInstr(183,14);EatInstr(182,14);EatInstr(181,14);EatInstr(180,14);EatInstr(179,14);EatInstr(178,14);EatInstr(177,14);EatInstr(176,14);EatInstr(175,14);EatInstr(174,14);EatInstr(173,14);EatInstr(172,14);EatInstr(171,14);EatInstr(170,14);EatInstr(169,14);EatInstr(168,14);EatInstr(167,14);EatInstr(166,14);EatInstr(165,14);EatInstr(164,14);EatInstr(163,14);EatInstr(162,14);EatInstr(161,14);EatInstr(160,14);EatInstr(159,14);EatInstr(158,14);EatInstr(157,14);EatInstr(156,14);EatInstr(155,14);EatInstr(154,14);EatInstr(153,14);EatInstr(152,14);EatInstr(151,14);EatInstr(150,14);EatInstr(149,14);EatInstr(148,14);EatInstr(147,14);EatInstr(146,14);EatInstr(145,14);EatInstr(144,14);EatInstr(143,14);EatInstr(142,14);EatInstr(141,14);EatInstr(140,14);EatInstr(139,14);EatInstr(138,14);EatInstr(137,14);EatInstr(136,14);EatInstr(135,14);EatInstr(134,14);EatInstr(133,14);EatInstr(132,14);EatInstr(131,14);EatInstr(130,14);EatInstr(129,14);EatInstr(128,14);EatInstr(0,14);EatInstr(127,14);EatInstr(126,14);EatInstr(125,14);EatInstr(124,14);EatInstr(123,14);EatInstr(122,14);EatInstr(121,14);EatInstr(120,14);EatInstr(119,14);EatInstr(118,14);EatInstr(117,14);EatInstr(116,14);EatInstr(115,14);EatInstr(114,14);EatInstr(113,14);EatInstr(112,14);EatInstr(111,14);EatInstr(110,14);EatInstr(109,14);EatInstr(108,14);EatInstr(107,14);EatInstr(106,14);EatInstr(105,14);EatInstr(104,14);EatInstr(103,14);EatInstr(102,14);EatInstr(101,14);EatInstr(100,14);EatInstr(99,14);EatInstr(98,14);EatInstr(97,14);EatInstr(96,14);EatInstr(95,14);EatInstr(94,14);EatInstr(93,14);EatInstr(92,14);EatInstr(91,14);EatInstr(90,14);EatInstr(89,14);EatInstr(88,14);EatInstr(87,14);EatInstr(86,14);EatInstr(85,14);EatInstr(84,14);EatInstr(83,14);EatInstr(82,14);EatInstr(81,14);EatInstr(80,14);EatInstr(79,14);EatInstr(78,14);EatInstr(77,14);EatInstr(76,14);EatInstr(75,14);EatInstr(74,14);EatInstr(73,14);EatInstr(72,14);EatInstr(71,14);EatInstr(70,14);EatInstr(69,14);EatInstr(68,14);EatInstr(67,14);EatInstr(66,14);EatInstr(65,14);EatInstr(64,14);EatInstr(63,14);EatInstr(62,14);EatInstr(61,14);EatInstr(60,14);EatInstr(59,14);EatInstr(58,14);EatInstr(57,14);EatInstr(56,14);EatInstr(55,14);EatInstr(54,14);EatInstr(53,14);EatInstr(52,14);EatInstr(51,14);EatInstr(50,14);EatInstr(49,14);EatInstr(48,14);EatInstr(47,14);EatInstr(46,14);EatInstr(45,14);EatInstr(44,14);EatInstr(43,14);EatInstr(42,14);EatInstr(41,14);EatInstr(40,14);EatInstr(39,14);EatInstr(38,14);EatInstr(37,14);EatInstr(36,14);EatInstr(35,14);EatInstr(34,14);EatInstr(33,14);EatInstr(32,14);EatInstr(31,14);EatInstr(30,14);EatInstr(29,14);EatInstr(28,14);EatInstr(27,14);EatInstr(26,14);EatInstr(25,14);EatInstr(24,14);EatInstr(23,14);EatInstr(22,14);EatInstr(21,14);EatInstr(20,14);EatInstr(19,14);EatInstr(18,14);EatInstr(17,14);EatInstr(16,14);EatInstr(15,14);EatInstr(14,14);EatInstr(13,14);EatInstr(12,14);EatInstr(11,14);EatInstr(10,14);EatInstr(9,14);EatInstr(8,14);EatInstr(7,14);EatInstr(6,14);EatInstr(5,14);EatInstr(4,14);EatInstr(3,14);EatInstr(2,14);EatInstr(1,14)]);
(387, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,417)]);
(4, [AAction2Instr(__a0,15)]);
(388, [AAction2Instr(__a78,147)]);
(5, [EatInstr(0,16)]);
(389, [EatInstr(100,418)]);
(6, [RCompleteInstr2(269,nullable_arg);AAction2Instr(__a1,17)]);
(390, [EatInstr(97,419)]);
(7, [EatInstr(127,19);EatInstr(126,19);EatInstr(125,19);EatInstr(124,19);EatInstr(123,19);EatInstr(122,19);EatInstr(121,19);EatInstr(120,19);EatInstr(119,19);EatInstr(118,19);EatInstr(117,19);EatInstr(116,19);EatInstr(115,19);EatInstr(114,19);EatInstr(113,19);EatInstr(112,19);EatInstr(111,19);EatInstr(110,19);EatInstr(109,19);EatInstr(108,19);EatInstr(107,19);EatInstr(106,19);EatInstr(105,19);EatInstr(104,19);EatInstr(103,19);EatInstr(102,19);EatInstr(101,19);EatInstr(100,19);EatInstr(99,19);EatInstr(98,19);EatInstr(97,19);EatInstr(96,19);EatInstr(95,19);EatInstr(94,19);EatInstr(93,19);EatInstr(92,19);EatInstr(91,19);EatInstr(90,19);EatInstr(89,19);EatInstr(88,19);EatInstr(87,19);EatInstr(86,19);EatInstr(85,19);EatInstr(84,19);EatInstr(83,19);EatInstr(82,19);EatInstr(81,19);EatInstr(80,19);EatInstr(79,19);EatInstr(78,19);EatInstr(77,19);EatInstr(76,19);EatInstr(75,19);EatInstr(74,19);EatInstr(73,19);EatInstr(72,19);EatInstr(71,19);EatInstr(70,19);EatInstr(69,19);EatInstr(68,19);EatInstr(67,19);EatInstr(66,19);EatInstr(65,19);EatInstr(64,19);EatInstr(63,19);EatInstr(62,19);EatInstr(61,19);EatInstr(60,19);EatInstr(59,19);EatInstr(58,19);EatInstr(57,19);EatInstr(56,19);EatInstr(55,19);EatInstr(54,19);EatInstr(53,19);EatInstr(52,19);EatInstr(51,19);EatInstr(50,19);EatInstr(49,19);EatInstr(48,19);EatInstr(47,19);EatInstr(46,19);EatInstr(44,19);EatInstr(43,19);EatInstr(42,19);EatInstr(41,19);EatInstr(40,19);EatInstr(39,19);EatInstr(38,19);EatInstr(37,19);EatInstr(36,19);EatInstr(35,19);EatInstr(34,19);EatInstr(33,19);EatInstr(32,19);EatInstr(31,19);EatInstr(30,19);EatInstr(29,19);EatInstr(28,19);EatInstr(27,19);EatInstr(26,19);EatInstr(25,19);EatInstr(24,19);EatInstr(23,19);EatInstr(22,19);EatInstr(21,19);EatInstr(20,19);EatInstr(19,19);EatInstr(18,19);EatInstr(17,19);EatInstr(16,19);EatInstr(15,19);EatInstr(14,19);EatInstr(13,19);EatInstr(12,19);EatInstr(11,19);EatInstr(10,19);EatInstr(9,19);EatInstr(8,19);EatInstr(7,19);EatInstr(6,19);EatInstr(5,19);EatInstr(4,19);EatInstr(3,19);EatInstr(2,19);EatInstr(1,19)]);
(391, [EatInstr(99,420)]);
(8, [EatInstr(119,30);EatInstr(117,29);EatInstr(115,28);EatInstr(112,27);EatInstr(109,26);EatInstr(108,25);EatInstr(105,24);EatInstr(104,23);EatInstr(100,22);EatInstr(99,21);EatInstr(97,20)]);
(392, [EatInstr(108,421)]);
(9, [AAction2Instr(__a2,31)]);
(393, [EatInstr(114,422)]);
(10, [AAction2Instr(__a3,32)]);
(394, [EatInstr(108,423)]);
(11, [ALookaheadInstr(false,CfgLA (3,266),33);RCompleteInstr2(274,nullable_eof)]);
(395, [EatInstr(108,424)]);
(12, [CompleteInstr(264)]);
(396, [EatInstr(97,425)]);
(13, [CompleteInstr(265)]);
(397, [EatInstr(97,426)]);
(14, [CompleteInstr(266)]);
(398, [EatInstr(97,427)]);
(15, [ACallInstr3(__default_call,9);ASimpleCont2Instr(272,__binder1,34)]);
(399, [AAction2Instr(__a79,428)]);
(16, [CompleteInstr(268)]);
(400, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,429)]);
(17, [AAction2Instr(__a4,36);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,35)]);
(401, [EatInstr(116,430)]);
(402, [ASimpleCont2Instr(271,__binder4,431);ACallInstr3(__default_call,8)]);
(19, [ALookaheadInstr(false,CfgLA (1,264),37);ACallInstr3(__default_call,1);ASimpleCont2Instr(264,__binder0,19)]);
(403, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,432)]);
(20, [EatInstr(116,38)]);
(404, [EatInstr(115,433)]);
(21, [EatInstr(111,40);EatInstr(108,39)]);
(405, [EatInstr(97,434)]);
(22, [EatInstr(101,41)]);
(406, [EatInstr(99,435)]);
(23, [EatInstr(97,42)]);
(407, [EatInstr(114,436)]);
(24, [EatInstr(110,43)]);
(408, [EatInstr(97,437)]);
(25, [EatInstr(105,45);EatInstr(101,44)]);
(409, [EatInstr(101,438)]);
(26, [EatInstr(105,46)]);
(410, [AAction2Instr(__a80,233)]);
(27, [EatInstr(114,47)]);
(411, [EatInstr(45,439)]);
(28, [EatInstr(117,48)]);
(412, [EatInstr(108,440)]);
(29, [EatInstr(110,49)]);
(413, [AAction2Instr(__a81,233)]);
(30, [EatInstr(114,50)]);
(414, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,441)]);
(31, [EatInstr(114,52);EatInstr(101,51);AAction2Instr(__a20,68);AAction2Instr(__a19,67);AAction2Instr(__a18,66);AAction2Instr(__a17,65);AAction2Instr(__a16,64);AAction2Instr(__a15,63);AAction2Instr(__a14,62);AAction2Instr(__a13,61);AAction2Instr(__a12,60);AAction2Instr(__a11,59);AAction2Instr(__a10,58);AAction2Instr(__a9,57);AAction2Instr(__a8,56);AAction2Instr(__a7,55);AAction2Instr(__a6,54);AAction2Instr(__a5,53)]);
(415, [AAction2Instr(__a82,442)]);
(32, [EatInstr(45,69);AAction2Instr(__a31,80);AAction2Instr(__a30,79);AAction2Instr(__a29,78);AAction2Instr(__a28,77);AAction2Instr(__a27,76);AAction2Instr(__a26,75);AAction2Instr(__a25,74);AAction2Instr(__a24,73);AAction2Instr(__a23,72);AAction2Instr(__a22,71);AAction2Instr(__a21,70)]);
(416, [AAction2Instr(__a83,147)]);
(33, [CompleteInstr(274)]);
(417, [AAction2Instr(__a84,147)]);
(34, [AAction2Instr(__a32,81)]);
(418, [EatInstr(45,443)]);
(35, [AAction2Instr(__a33,17)]);
(419, [EatInstr(104,444)]);
(36, [AWhenInstr3(__p35,__p34,82)]);
(420, [EatInstr(101,445)]);
(37, [CompleteInstr(270)]);
(421, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,446)]);
(38, [EatInstr(116,83)]);
(422, [EatInstr(101,447)]);
(39, [EatInstr(111,84)]);
(423, [EatInstr(108,448)]);
(40, [EatInstr(112,85)]);
(424, [EatInstr(101,449)]);
(41, [EatInstr(115,86)]);
(425, [EatInstr(116,450)]);
(42, [EatInstr(115,87)]);
(426, [EatInstr(116,451)]);
(43, [EatInstr(108,88)]);
(427, [EatInstr(116,452)]);
(44, [EatInstr(120,89)]);
(428, [AAction2Instr(__a85,454);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,453)]);
(45, [EatInstr(102,90)]);
(429, [AAction2Instr(__a86,272)]);
(46, [EatInstr(110,91)]);
(430, [EatInstr(97,455)]);
(47, [EatInstr(101,92)]);
(431, [AAction2Instr(__a87,272)]);
(48, [EatInstr(98,93)]);
(432, [AAction2Instr(__a88,456)]);
(49, [EatInstr(114,94)]);
(433, [EatInstr(101,457)]);
(50, [EatInstr(97,95)]);
(434, [EatInstr(98,458)]);
(51, [EatInstr(120,96)]);
(435, [EatInstr(115,459)]);
(52, [EatInstr(102,97)]);
(436, [EatInstr(101,460)]);
(53, [ASimpleCont2Instr(271,__binder2,98);ACallInstr3(__default_call,8)]);
(437, [EatInstr(100,461)]);
(54, [EatInstr(99,99)]);
(438, [EatInstr(115,462)]);
(55, [EatInstr(100,100)]);
(439, [EatInstr(99,463)]);
(56, [EatInstr(100,101)]);
(440, [EatInstr(97,464)]);
(57, [EatInstr(101,102)]);
(441, [AAction2Instr(__a89,233)]);
(58, [EatInstr(102,103)]);
(442, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,465)]);
(59, [EatInstr(105,104)]);
(443, [EatInstr(97,466)]);
(60, [EatInstr(108,105)]);
(444, [EatInstr(101,467)]);
(61, [EatInstr(108,106)]);
(445, [EatInstr(45,468)]);
(62, [EatInstr(112,107)]);
(446, [AAction2Instr(__a90,147)]);
(63, [EatInstr(112,108)]);
(447, [EatInstr(100,469)]);
(64, [EatInstr(112,109)]);
(448, [EatInstr(97,470)]);
(65, [EatInstr(112,110)]);
(449, [EatInstr(118,471)]);
(66, [EatInstr(112,111)]);
(450, [EatInstr(101,472)]);
(67, [EatInstr(112,112)]);
(451, [EatInstr(101,473)]);
(68, [AAction2Instr(__a37,114);AAction2Instr(__a36,113)]);
(452, [EatInstr(101,474)]);
(69, [EatInstr(117,117);EatInstr(114,116);EatInstr(99,115)]);
(453, [AAction2Instr(__a91,428)]);
(70, [EatInstr(45,118)]);
(454, [AWhenInstr3(__p93,__p92,475)]);
(71, [EatInstr(45,119)]);
(455, [EatInstr(114,476)]);
(72, [EatInstr(45,120)]);
(456, [EatInstr(116,479);EatInstr(112,478);EatInstr(102,477)]);
(73, [EatInstr(45,121)]);
(457, [EatInstr(110,480)]);
(74, [EatInstr(45,122)]);
(458, [EatInstr(101,481)]);
(75, [EatInstr(45,123)]);
(459, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,482)]);
(76, [EatInstr(45,124)]);
(460, [EatInstr(103,483)]);
(77, [EatInstr(45,125)]);
(461, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,484)]);
(78, [EatInstr(45,126)]);
(462, [EatInstr(99,485)]);
(79, [EatInstr(45,127)]);
(463, [EatInstr(111,486)]);
(80, [ACallInstr3(__default_call,7);ASimpleCont2Instr(270,__binder0,128)]);
(464, [EatInstr(114,487)]);
(81, [AAction2Instr(__a39,130);AAction2Instr(__a38,129)]);
(82, [ALookaheadInstr(false,CfgLA (1,264),131)]);
(465, [AAction2Instr(__a94,353)]);
(466, [EatInstr(110,488)]);
(83, [EatInstr(114,132)]);
(467, [EatInstr(97,489)]);
(84, [EatInstr(115,133)]);
(468, [EatInstr(97,490)]);
(85, [EatInstr(121,134)]);
(469, [EatInstr(115,491)]);
(86, [EatInstr(117,135)]);
(470, [EatInstr(98,492)]);
(87, [EatInstr(104,136)]);
(471, [EatInstr(97,493)]);
(88, [EatInstr(105,137)]);
(472, [EatInstr(45,494)]);
(89, [EatInstr(101,138)]);
(473, [EatInstr(45,495)]);
(90, [EatInstr(116,139)]);
(474, [EatInstr(45,496)]);
(91, [EatInstr(117,140)]);
(475, [AAction2Instr(__a95,497)]);
(92, [EatInstr(99,141)]);
(476, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,498)]);
(93, [EatInstr(115,142)]);
(477, [EatInstr(117,499)]);
(94, [EatInstr(111,143)]);
(478, [EatInstr(101,500)]);
(95, [EatInstr(112,144)]);
(479, [EatInstr(120,501)]);
(96, [EatInstr(101,145)]);
(480, [EatInstr(115,502)]);
(97, [EatInstr(99,146)]);
(481, [EatInstr(108,503)]);
(98, [AAction2Instr(__a40,147)]);
(482, [AAction2Instr(__a96,272)]);
(99, [EatInstr(111,148)]);
(483, [EatInstr(117,504)]);
(100, [EatInstr(105,149)]);
(484, [AAction2Instr(__a97,272)]);
(101, [EatInstr(111,150)]);
(485, [EatInstr(101,505)]);
(102, [EatInstr(120,151)]);
(486, [EatInstr(114,506)]);
(103, [EatInstr(117,152)]);
(487, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,507)]);
(104, [EatInstr(110,153)]);
(488, [EatInstr(97,508)]);
(105, [EatInstr(111,154)]);
(489, [EatInstr(100,509)]);
(106, [EatInstr(114,155)]);
(490, [EatInstr(110,510)]);
(107, [EatInstr(114,156)]);
(491, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,511)]);
(108, [EatInstr(114,157)]);
(492, [EatInstr(108,512)]);
(109, [EatInstr(114,158)]);
(493, [EatInstr(110,513)]);
(110, [EatInstr(114,159)]);
(494, [EatInstr(97,514)]);
(111, [EatInstr(114,160)]);
(495, [EatInstr(100,515)]);
(112, [EatInstr(114,161)]);
(496, [EatInstr(100,516)]);
(113, [EatInstr(115,162)]);
(497, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,517)]);
(114, [AAction2Instr(__a42,164);AAction2Instr(__a41,163)]);
(498, [AAction2Instr(__a98,518)]);
(115, [EatInstr(111,165)]);
(499, [EatInstr(110,519)]);
(116, [EatInstr(111,166)]);
(500, [EatInstr(103,520)]);
(117, [EatInstr(110,167)]);
(501, [AAction2Instr(__a99,521)]);
(118, [EatInstr(97,168)]);
(502, [EatInstr(105,522)]);
(119, [EatInstr(98,169)]);
(503, [EatInstr(115,523)]);
(120, [EatInstr(99,170)]);
(504, [EatInstr(108,524)]);
(121, [EatInstr(99,171)]);
(505, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,525)]);
(122, [EatInstr(105,172)]);
(506, [EatInstr(101,526)]);
(123, [EatInstr(105,173)]);
(507, [AAction2Instr(__a100,233)]);
(124, [EatInstr(108,174)]);
(508, [EatInstr(108,527)]);
(125, [EatInstr(110,175)]);
(509, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,528)]);
(126, [EatInstr(111,176)]);
(510, [EatInstr(97,529)]);
(127, [EatInstr(118,177)]);
(511, [AAction2Instr(__a101,147)]);
(128, [AAction2Instr(__a43,178)]);
(512, [EatInstr(101,530)]);
(129, [ACallInstr3(__default_call,10);ASimpleCont2Instr(273,__binder3,179)]);
(513, [EatInstr(99,531)]);
(130, [ACallInstr3(__default_call,11);ASimpleCont2Instr(274,__binder0,180)]);
(514, [EatInstr(99,532)]);
(131, [CompleteInstr(269)]);
(515, [EatInstr(121,533)]);
(132, [EatInstr(105,181)]);
(516, [EatInstr(121,534)]);
(133, [EatInstr(101,182)]);
(517, [AAction2Instr(__a102,272)]);
(134, [EatInstr(114,183)]);
(518, [AAction2Instr(__a103,536);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,535)]);
(135, [EatInstr(103,184)]);
(519, [AAction2Instr(__a104,521)]);
(136, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,185)]);
(520, [EatInstr(45,537);AAction2Instr(__a105,521)]);
(137, [EatInstr(110,186)]);
(521, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,538)]);
(138, [EatInstr(114,187)]);
(522, [EatInstr(116,539)]);
(139, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,188)]);
(523, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,540)]);
(140, [EatInstr(115,189)]);
(524, [EatInstr(97,541)]);
(141, [EatInstr(101,190)]);
(525, [AAction2Instr(__a106,272)]);
(142, [EatInstr(101,191)]);
(526, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,542)]);
(143, [EatInstr(108,192)]);
(527, [EatInstr(121,543)]);
(144, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,193)]);
(528, [AAction2Instr(__a107,147)]);
(145, [EatInstr(99,194)]);
(529, [EatInstr(108,544)]);
(146, [AAction2Instr(__a44,195)]);
(530, [EatInstr(45,545)]);
(147, [AAction2Instr(__a45,196)]);
(531, [EatInstr(101,546)]);
(148, [EatInstr(109,197)]);
(532, [EatInstr(116,547)]);
(149, [EatInstr(115,198)]);
(533, [EatInstr(112,548)]);
(150, [EatInstr(116,199)]);
(534, [EatInstr(112,549)]);
(151, [EatInstr(116,200)]);
(535, [AAction2Instr(__a108,518)]);
(152, [EatInstr(115,201)]);
(536, [AWhenInstr3(__p110,__p109,550)]);
(153, [EatInstr(102,202)]);
(537, [EatInstr(115,551)]);
(154, [EatInstr(111,203)]);
(538, [AAction2Instr(__a111,272)]);
(155, [EatInstr(49,204)]);
(539, [EatInstr(105,552)]);
(156, [EatInstr(101,205)]);
(540, [AAction2Instr(__a112,272)]);
(157, [EatInstr(105,206)]);
(541, [EatInstr(114,553)]);
(158, [EatInstr(105,207)]);
(542, [AAction2Instr(__a113,233)]);
(159, [EatInstr(105,208)]);
(543, [EatInstr(115,554)]);
(160, [EatInstr(105,209)]);
(544, [EatInstr(121,555)]);
(161, [EatInstr(105,210)]);
(545, [EatInstr(112,556)]);
(162, [EatInstr(116,211)]);
(546, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,557)]);
(163, [EatInstr(116,212)]);
(547, [EatInstr(105,558)]);
(164, [EatInstr(116,213)]);
(548, [EatInstr(103,559)]);
(165, [EatInstr(117,214)]);
(549, [EatInstr(103,560)]);
(166, [EatInstr(111,215)]);
(550, [AAction2Instr(__a114,561)]);
(167, [EatInstr(114,216)]);
(551, [EatInstr(116,562)]);
(168, [EatInstr(102,217)]);
(552, [EatInstr(118,563)]);
(169, [EatInstr(97,218)]);
(553, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,564)]);
(170, [EatInstr(97,219)]);
(554, [EatInstr(105,565)]);
(171, [EatInstr(104,220)]);
(555, [EatInstr(115,566)]);
(172, [EatInstr(110,221)]);
(556, [EatInstr(114,567)]);
(173, [EatInstr(110,222)]);
(557, [AAction2Instr(__a115,147)]);
(174, [EatInstr(111,223)]);
(558, [EatInstr(111,568)]);
(175, [EatInstr(111,224)]);
(559, [EatInstr(101,569)]);
(176, [EatInstr(110,225)]);
(560, [EatInstr(101,570)]);
(177, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,226)]);
(561, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,571)]);
(178, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,227)]);
(562, [EatInstr(114,572)]);
(179, [AAction2Instr(__a46,81)]);
(563, [EatInstr(101,573)]);
(180, [AAction2Instr(__a47,228)]);
(564, [AAction2Instr(__a116,272)]);
(181, [EatInstr(98,229)]);
(565, [EatInstr(115,574)]);
(182, [EatInstr(45,230)]);
(566, [EatInstr(105,575)]);
(183, [EatInstr(117,231)]);
(567, [EatInstr(101,576)]);
(184, [EatInstr(97,232)]);
(568, [EatInstr(110,577)]);
(185, [AAction2Instr(__a48,233)]);
(569, [EatInstr(110,578)]);
(186, [EatInstr(101,234)]);
(570, [EatInstr(110,579)]);
(187, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,235)]);
(571, [AAction2Instr(__a117,272)]);
(188, [AAction2Instr(__a49,233)]);
(572, [EatInstr(105,580)]);
(189, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,236)]);
(573, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,581)]);
(190, [EatInstr(100,237)]);
(574, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,582)]);
(191, [EatInstr(116,238)]);
(575, [EatInstr(115,583)]);
(192, [EatInstr(108,239)]);
(576, [EatInstr(100,584)]);
(193, [AAction2Instr(__a50,233)]);
(577, [EatInstr(115,585)]);
(194, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,240)]);
(578, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,586)]);
(195, [AAction2Instr(__a51,242);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,241)]);
(579, [EatInstr(45,587)]);
(196, [CompleteInstr(272)]);
(580, [EatInstr(99,588)]);
(197, [EatInstr(112,243)]);
(581, [AAction2Instr(__a118,272)]);
(198, [EatInstr(112,244)]);
(582, [AAction2Instr(__a119,147)]);
(199, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,245)]);
(583, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,589)]);
(200, [EatInstr(114,246)]);
(584, [EatInstr(105,590)]);
(201, [EatInstr(101,247)]);
(585, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,591)]);
(202, [EatInstr(111,248)]);
(586, [AAction2Instr(__a120,147)]);
(203, [EatInstr(107,249)]);
(587, [EatInstr(115,592)]);
(204, [EatInstr(45,250)]);
(588, [EatInstr(116,593)]);
(205, [EatInstr(99,251)]);
(589, [AAction2Instr(__a121,147)]);
(206, [EatInstr(110,252)]);
(590, [EatInstr(99,594)]);
(207, [EatInstr(110,253)]);
(591, [AAction2Instr(__a122,147)]);
(208, [EatInstr(110,254)]);
(592, [EatInstr(99,595)]);
(209, [EatInstr(110,255)]);
(593, [AAction2Instr(__a123,521)]);
(210, [EatInstr(110,256)]);
(594, [EatInstr(97,596)]);
(211, [EatInstr(114,257)]);
(595, [EatInstr(97,597)]);
(212, [EatInstr(114,258)]);
(596, [EatInstr(116,598)]);
(213, [EatInstr(114,259)]);
(597, [EatInstr(110,599)]);
(214, [EatInstr(110,260)]);
(598, [EatInstr(101,600)]);
(215, [EatInstr(116,261)]);
(599, [EatInstr(110,601)]);
(216, [EatInstr(111,262)]);
(600, [EatInstr(115,602)]);
(217, [EatInstr(116,263)]);
(601, [EatInstr(101,603)]);
(218, [EatInstr(99,264)]);
(602, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,604)]);
(219, [EatInstr(115,265)]);
(603, [EatInstr(114,605)]);
(220, [EatInstr(101,266)]);
(604, [AAction2Instr(__a124,147)]);
(221, [EatInstr(108,267)]);
(605, [EatInstr(108,606)]);
(222, [EatInstr(108,268)]);
(606, [EatInstr(101,607)]);
(223, [EatInstr(111,269)]);
(607, [EatInstr(115,608)]);
(224, [EatInstr(45,270)]);
(608, [EatInstr(115,609)]);
(225, [EatInstr(108,271)]);
(609, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,610)]);
(226, [AAction2Instr(__a52,272)]);
(610, [AAction2Instr(__a125,147)]);
(227, [AAction2Instr(__a53,272)]);
(228, [CompleteInstr(267)]);
(229, [EatInstr(117,273)]);
(230, [EatInstr(117,274)]);
(231, [EatInstr(108,275)]);
(232, [EatInstr(114,276)]);
(233, [CompleteInstr(271)]);
(234, [EatInstr(45,277)]);
(235, [AAction2Instr(__a54,233)]);
(236, [AAction2Instr(__a55,233)]);
(237, [EatInstr(101,278)]);
(238, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,279)]);
(239, [EatInstr(45,280)]);
(240, [AAction2Instr(__a56,281)]);
(241, [AAction2Instr(__a57,195)]);
(242, [AWhenInstr3(__p59,__p58,282)]);
(243, [EatInstr(105,283)]);
(244, [EatInstr(97,284)]);
(245, [AAction2Instr(__a60,147)]);
(246, [EatInstr(97,285)]);
(247, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,286)]);
(248, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,287)]);
(249, [EatInstr(97,288)]);
(250, [EatInstr(108,289)]);
(251, [EatInstr(101,290)]);
(252, [EatInstr(116,291)]);
(253, [EatInstr(116,292)]);
(254, [EatInstr(116,293)]);
(255, [EatInstr(116,294)]);
(256, [EatInstr(116,295)]);
(257, [EatInstr(105,296)]);
(258, [EatInstr(97,297)]);
(259, [EatInstr(97,298)]);
(260, [EatInstr(116,299)]);
(261, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,300)]);
(262, [EatInstr(108,301)]);
(263, [EatInstr(101,302)]);
(264, [EatInstr(107,303)]);
(265, [EatInstr(101,304)]);
(266, [EatInstr(99,305)]);
(267, [EatInstr(105,306)]);
(268, [EatInstr(105,307)]);
(269, [EatInstr(107,308)]);
(270, [EatInstr(99,309)]);
(271, [EatInstr(121,310)]);
(272, [AAction2Instr(__a61,311)]);
(273, [EatInstr(116,312)]);
(274, [EatInstr(110,313)]);
(275, [EatInstr(101,314)]);
(276, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,315)]);
(277, [EatInstr(114,316)]);
(278, [EatInstr(110,317)]);
(279, [AAction2Instr(__a62,233)]);
(280, [EatInstr(115,318)]);
(281, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,319)]);
(282, [AAction2Instr(__a63,320)]);
(283, [EatInstr(108,321)]);
(284, [EatInstr(116,322)]);
(285, [EatInstr(99,323)]);
(286, [AAction2Instr(__a64,147)]);
(287, [AAction2Instr(__a65,147)]);
(288, [EatInstr(104,324)]);
(289, [EatInstr(111,325)]);
(290, [EatInstr(100,326)]);
(291, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,327)]);
(292, [EatInstr(45,328)]);
(293, [EatInstr(45,329)]);
(294, [EatInstr(45,330)]);
(295, [EatInstr(45,331)]);
(296, [EatInstr(112,332)]);
(297, [EatInstr(110,333)]);
(298, [EatInstr(110,334)]);
(299, [EatInstr(101,335)]);
(300, [AAction2Instr(__a66,336)]);
(301, [EatInstr(108,337)]);
(302, [EatInstr(114,338)]);
(303, [EatInstr(101,339)]);
(304, [EatInstr(45,340)]);
(305, [EatInstr(107,341)]);
(306, [EatInstr(110,342)]);
(307, [EatInstr(110,343)]);
(308, [EatInstr(97,344)]);
(309, [EatInstr(111,345)]);
(310, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,346)]);
(311, [CompleteInstr(273)]);
(312, [EatInstr(101,347)]);
(313, [EatInstr(100,348)]);
(314, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,349)]);
(315, [AAction2Instr(__a67,233)]);
(316, [EatInstr(101,350)]);
(317, [EatInstr(99,351)]);
(318, [EatInstr(116,352)]);
(319, [AAction2Instr(__a68,353)]);
(320, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,354)]);
(321, [EatInstr(101,355)]);
(322, [EatInstr(99,356)]);
(323, [EatInstr(116,357)]);
(324, [EatInstr(101,358)]);
(325, [EatInstr(111,359)]);
(326, [EatInstr(101,360)]);
(327, [AAction2Instr(__a69,147)]);
(328, [EatInstr(103,361)]);
(329, [EatInstr(110,362)]);
(330, [EatInstr(110,363)]);
(331, [EatInstr(114,364)]);
(332, [EatInstr(45,365)]);
(333, [EatInstr(115,366)]);
(334, [EatInstr(115,367)]);
(335, [EatInstr(114,368)]);
(336, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,369)]);
(337, [EatInstr(45,370)]);
(338, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,371)]);
(339, [EatInstr(110,372)]);
(340, [EatInstr(105,373)]);
(341, [EatInstr(45,374)]);
(342, [EatInstr(101,375)]);
(343, [EatInstr(101,376)]);
(344, [EatInstr(104,377)]);
(345, [EatInstr(97,378)]);
(346, [AAction2Instr(__a70,272)]);
(347, [EatInstr(115,379)]);
(348, [EatInstr(101,380)]);
(349, [AAction2Instr(__a71,233)]);
(350, [EatInstr(103,381)]);
(351, [EatInstr(101,382)]);
(352, [EatInstr(97,383)]);
(353, [AAction2Instr(__a73,385);AAction2Instr(__a72,384)]);
(354, [AAction2Instr(__a74,147)]);
(355, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,386)]);
(356, [EatInstr(104,387)]);
(357, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,388)]);
(358, [EatInstr(97,389)]);
(359, [EatInstr(107,390)]);
(360, [EatInstr(110,391)]);
(361, [EatInstr(105,392)]);
(362, [EatInstr(112,393)]);
(363, [EatInstr(117,394)]);
(364, [EatInstr(101,395)]);
(365, [EatInstr(108,396)]);
(366, [EatInstr(108,397)]);
(367, [EatInstr(108,398)]);
(368, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,399)]);
(369, [AAction2Instr(__a75,400)]);
(370, [EatInstr(115,401)]);
(371, [AAction2Instr(__a76,402)]);
(372, [EatInstr(100,403)]);
(373, [EatInstr(110,404)]);
(374, [EatInstr(108,405)]);
(375, [EatInstr(45,406)]);
(376, [EatInstr(45,407)]);
(377, [EatInstr(101,408)]);
(378, [EatInstr(108,409)]);
(379, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,410)]);
(380, [EatInstr(114,411)]);
(381, [EatInstr(117,412)]);
(382, [ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,413)]);
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
