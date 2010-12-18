
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

let outch = ref stdout
type _yk_t =
| Yk_x1
| Yk_x17 of (int)
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
_r_rfc(_n,ykinput) = (ignore (*1001*) (_n()); 
 (let rec _x31 _x29 = (match _n() with 1002 -> _x29 | _x30 -> _x31(
 (match _x30 with
 | (1003) -> (
 (let _x4 = (ignore (*1004*) (_n()); _n())
  in (ignore (*1007*) (_n()); 
 (let _x3 = (ignore (*1008*) (_n()); _n())
  in (ignore (*1010*) (_n()); 
 (let x = (ignore (*1011*) (_n()); Yak.YkBuf.get_string _x4 _x3 ykinput)
  in (ignore (*1012*) (_n()); 
 (let _x33 = (ignore (*1013*) (_n());  output_string !outch x; output_string !outch "\n";  )
  in ()))
 ))
 ))
 ))
 | _(*1014*) -> (
 (let _x32 = (ignore (*1015*) (_n()); ())
  in ()))
 ))) in _x31(())))
 
 
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

let _x45 =
 (fun _(*pos*) (_,_x34)(*arg of rule*) -> (_t(fun _(*1017*) pos_ -> let _x35 n  = _t(fun _(*1023*) pos_ -> let _x39 _x38  = _t(fun _(*1027*) pos_ -> let _x42 _x41  = _t(function
 | 1031 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1032*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x44) -> Yk_done(ignore(ignore(_x44);_wv0);_wv0) | _ -> failwith "bind-1032"))) in _t(function
 | 1028 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1029*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x43) -> _x42 (_x43)  | _ -> failwith "bind-1029")))) in _t(function
 | 1024 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1025*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x40) -> _x39 (_x40)  | _ -> failwith "bind-1025")))) in _t(fun _(*1018*) pos_ -> let _x36 _x18  = _t(fun _(*1021*) pos_ -> _x35 ((match _x18 with Yk_x17(y) -> y | _ -> failwith "projection")) ) in _t(fun _(*1020*) pos_ -> Yk_bind(function Yk_done(_x37) -> _x36 (_x37)  | _ -> failwith "bind=1020")))),_x34))
let _x50 =
 (fun _(*pos*) (_,_x46)(*arg of rule-indent*) -> (_t(fun _(*1033*) pos_ -> let _x47 _x19  = _t(fun _(*1044*) pos_ -> Yk_done(Yk_x17(_x19))) in _t(fun _(*1035*) pos_ -> let _x48 left  = _t(fun _(*1040*) pos_ -> let _x49 right  = _t(fun _(*1043*) pos_ -> _x47 (right - left) ) in _t(fun _(*1041*) pos_ -> _x49 (pos_) )) in _t(fun _(*1036*) pos_ -> _x48 (pos_) ))),_x46))
let _x58 =
 (fun _(*pos*) -> (function (Yk_done(_x20:_yk_t),_x51) -> (_t(fun _(*1046*) pos_ -> let _x52 _x8  = _t(fun _(*1048*) pos_ -> let _x54 _x53 n = _t(fun _(*1052*) pos_ -> let _x56 left  = _t(fun _(*1057*) pos_ -> let _x57 right  = _t(function
 | 1060 ->
 (fun pos_ -> Yk_when(right - left > n))
 | _(*1061*) ->
 (fun pos_ -> Yk_done(ignore((_wv0));_wv0))) in _t(fun _(*1058*) pos_ -> _x57 (pos_) )) in _t(fun _(*1053*) pos_ -> _x56 (pos_) )) in _t(fun _(*1049*) pos_ -> let _x55 n = _x54 ((_wv0)) n in _t(fun _(*1050*) pos_ -> _x55((match _x8 with (n) -> n))))) in _t(fun _(*1047*) pos_ -> _x52 ((match _x20 with Yk_x17(y) -> y | _ -> failwith "projection")) )),_x51)
| _ -> failwith "indent"))
let _x65 =
 (fun _(*pos*) -> (function (Yk_done(_x21:_yk_t),_x59) -> (_t(fun _(*1063*) pos_ -> let _x60 _x9  = _t(fun _(*1065*) pos_ -> let _x62 _x61 n = _t(function
 | 1071 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | 1072 ->
 (fun pos_ -> Yk_bind(function Yk_done(_x64) -> Yk_done(ignore(ignore(_x64);_wv0);_wv0) | _ -> failwith "bind-1072"))
 | _(*1074*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore(());_wv0);_wv0);_wv0))) in _t(fun _(*1066*) pos_ -> let _x63 n = _x62 ((_wv0)) n in _t(fun _(*1067*) pos_ -> _x63((match _x9 with (n) -> n))))) in _t(fun _(*1064*) pos_ -> _x60 ((match _x21 with Yk_x17(y) -> y | _ -> failwith "projection")) )),_x59)
| _ -> failwith "o"))
let _x72 =
 (fun _(*pos*) -> (function (Yk_done(_x22:_yk_t),_x66) -> (_t(fun _(*1076*) pos_ -> let _x67 _x10  = _t(fun _(*1078*) pos_ -> let _x69 _x68 n = _t(function
 | 1081 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1082*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x71) -> Yk_done(ignore(_x71);_wv0) | _ -> failwith "bind-1082"))) in _t(fun _(*1079*) pos_ -> let _x70 n = _x69 ((_wv0)) n in _t(fun _(*1080*) pos_ -> _x70((match _x10 with (n) -> n))))) in _t(fun _(*1077*) pos_ -> _x67 ((match _x22 with Yk_x17(y) -> y | _ -> failwith "projection")) )),_x66)
| _ -> failwith "elements"))
let _x88 =
 (fun _(*pos*) -> (function (Yk_done(_x23:_yk_t),_x73) -> (_t(fun _(*1084*) pos_ -> let _x74 _x11  = _t(fun _(*1086*) pos_ -> let _x76 _x75 n = _t(fun _(*1089*) pos_ -> let _x79 _x78  = _t(function
 | 1092 ->
 (fun pos_ -> let _x82 _x81  = _t(fun _(*1096*) pos_ -> let _x85 _x84  = _t(function
 | 1099 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1100*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x87) -> Yk_done(ignore(_x87);_wv0) | _ -> failwith "bind-1100"))) in _t(function
 | 1097 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1098*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x86) -> _x85 (_x86)  | _ -> failwith "bind-1098")))) in _t(function
 | 1093 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1094*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x83) -> _x82 (_x83)  | _ -> failwith "bind-1094"))))
 | _(*1102*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(function
 | 1090 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1091*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x80) -> _x79 (_x80)  | _ -> failwith "bind-1091")))) in _t(fun _(*1087*) pos_ -> let _x77 n = _x76 ((_wv0)) n in _t(fun _(*1088*) pos_ -> _x77((match _x11 with (n) -> n))))) in _t(fun _(*1085*) pos_ -> _x74 ((match _x23 with Yk_x17(y) -> y | _ -> failwith "projection")) )),_x73)
| _ -> failwith "alternation"))
let _x103 =
 (fun _(*pos*) -> (function (Yk_done(_x24:_yk_t),_x89) -> (_t(fun _(*1104*) pos_ -> let _x90 _x12  = _t(fun _(*1106*) pos_ -> let _x92 _x91 n = _t(fun _(*1109*) pos_ -> let _x95 _x94  = _t(fun _(*1112*) pos_ -> let rec _x98 _x97  = _t(function
 | 1113 ->
 (fun pos_ -> Yk_done(ignore(_x97);_wv0))
 | _(*1114*) ->
 (fun pos_ -> let _x100 _x99  = _t(function
 | 1117 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1118*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x102) -> _x98 (_x102)  | _ -> failwith "bind-1118"))) in _t(function
 | 1115 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1116*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x101) -> _x100 (_x101)  | _ -> failwith "bind-1116"))))) in _x98 (_wv0) ) in _t(function
 | 1110 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1111*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x96) -> _x95 (_x96)  | _ -> failwith "bind-1111")))) in _t(fun _(*1107*) pos_ -> let _x93 n = _x92 ((_wv0)) n in _t(fun _(*1108*) pos_ -> _x93((match _x12 with (n) -> n))))) in _t(fun _(*1105*) pos_ -> _x90 ((match _x24 with Yk_x17(y) -> y | _ -> failwith "projection")) )),_x89)
| _ -> failwith "concatenation"))
let _x110 =
 (fun _(*pos*) -> (function (Yk_done(_x25:_yk_t),_x104) -> (_t(fun _(*1120*) pos_ -> let _x105 _x13  = _t(fun _(*1122*) pos_ -> let _x107 _x106 n = _t(function
 | 1126 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1127*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x109) -> Yk_done(ignore(_x109);_wv0) | _ -> failwith "bind-1127"))) in _t(fun _(*1123*) pos_ -> let _x108 n = _x107 ((_wv0)) n in _t(fun _(*1124*) pos_ -> _x108((match _x13 with (n) -> n))))) in _t(fun _(*1121*) pos_ -> _x105 ((match _x25 with Yk_x17(y) -> y | _ -> failwith "projection")) )),_x104)
| _ -> failwith "repetition"))
let _x122 =
 (fun _(*pos*) -> (function (Yk_done(_x26:_yk_t),_x111) -> (_t(fun _(*1129*) pos_ -> let _x112 _x14  = _t(fun _(*1131*) pos_ -> let _x114 _x113 n = _t(function
 | 1135 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1136 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | 1137 ->
 (fun pos_ -> Yk_bind(function Yk_done(_x121) -> Yk_done(ignore(_x121);_wv0) | _ -> failwith "bind-1137"))
 | 1138 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | 1139 ->
 (fun pos_ -> Yk_bind(function Yk_done(_x120) -> Yk_done(ignore(_x120);_wv0) | _ -> failwith "bind-1139"))
 | _(*1140*) ->
 (fun pos_ -> let _x117 _x116  = _t(function
 | 1143 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1144*) ->
 (fun pos_ -> let _x119 _x118  = _t(function
 | 1147 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1149*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1145*) pos_ -> _x119 (()) ))) in _t(fun _(*1141*) pos_ -> _x117 (()) ))) in _t(fun _(*1132*) pos_ -> let _x115 n = _x114 ((_wv0)) n in _t(fun _(*1133*) pos_ -> _x115((match _x14 with (n) -> n))))) in _t(fun _(*1130*) pos_ -> _x112 ((match _x26 with Yk_x17(y) -> y | _ -> failwith "projection")) )),_x111)
| _ -> failwith "element"))
let _x135 =
 (fun _(*pos*) -> (function (Yk_done(_x27:_yk_t),_x123) -> (_t(fun _(*1151*) pos_ -> let _x124 _x15  = _t(fun _(*1153*) pos_ -> let _x126 _x125 n = _t(fun _(*1157*) pos_ -> let _x129 _x128  = _t(fun _(*1160*) pos_ -> let _x132 _x131  = _t(function
 | 1164 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1165*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x134) -> Yk_done(ignore(ignore(_x134);_wv0);_wv0) | _ -> failwith "bind-1165"))) in _t(function
 | 1161 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1162*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x133) -> _x132 (_x133)  | _ -> failwith "bind-1162")))) in _t(function
 | 1158 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1159*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x130) -> _x129 (_x130)  | _ -> failwith "bind-1159")))) in _t(fun _(*1154*) pos_ -> let _x127 n = _x126 ((_wv0)) n in _t(fun _(*1155*) pos_ -> _x127((match _x15 with (n) -> n))))) in _t(fun _(*1152*) pos_ -> _x124 ((match _x27 with Yk_x17(y) -> y | _ -> failwith "projection")) )),_x123)
| _ -> failwith "group"))
let _x148 =
 (fun _(*pos*) -> (function (Yk_done(_x28:_yk_t),_x136) -> (_t(fun _(*1167*) pos_ -> let _x137 _x16  = _t(fun _(*1169*) pos_ -> let _x139 _x138 n = _t(fun _(*1173*) pos_ -> let _x142 _x141  = _t(fun _(*1176*) pos_ -> let _x145 _x144  = _t(function
 | 1180 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1181*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x147) -> Yk_done(ignore(ignore(_x147);_wv0);_wv0) | _ -> failwith "bind-1181"))) in _t(function
 | 1177 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1178*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x146) -> _x145 (_x146)  | _ -> failwith "bind-1178")))) in _t(function
 | 1174 ->
 (fun pos_ -> Yk_done(Yk_x17((n))))
 | _(*1175*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x143) -> _x142 (_x143)  | _ -> failwith "bind-1175")))) in _t(fun _(*1170*) pos_ -> let _x140 n = _x139 ((_wv0)) n in _t(fun _(*1171*) pos_ -> _x140((match _x16 with (n) -> n))))) in _t(fun _(*1168*) pos_ -> _x137 ((match _x28 with Yk_x17(y) -> y | _ -> failwith "projection")) )),_x136)
| _ -> failwith "option"))
let _x152 =
 (fun _(*pos*) (_,_x149)(*arg of bitstring*) -> (_t(fun _(*1184*) pos_ -> let _x150 _x5  = _t(function
 | 1190 ->
 (fun pos_ -> Yk_when(_x5>=1))
 | _(*1191*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1185*) pos_ -> let rec _x151 _x5  = _t(function
 | 1186 ->
 (fun pos_ -> _x150 (_x5) )
 | _(*1188*) ->
 (fun pos_ -> _x151 (_x5+1) )) in _x151 (0) )),_x149))
let _x156 =
 (fun _(*pos*) (_,_x153)(*arg of DIGITS*) -> (_t(fun _(*1194*) pos_ -> let _x154 _x6  = _t(function
 | 1200 ->
 (fun pos_ -> Yk_when(_x6>=1))
 | _(*1201*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1195*) pos_ -> let rec _x155 _x6  = _t(function
 | 1196 ->
 (fun pos_ -> _x154 (_x6) )
 | _(*1198*) ->
 (fun pos_ -> _x155 (_x6+1) )) in _x155 (0) )),_x153))
let _x160 =
 (fun _(*pos*) (_,_x157)(*arg of HEXDIGS*) -> (_t(fun _(*1204*) pos_ -> let _x158 _x7  = _t(function
 | 1210 ->
 (fun pos_ -> Yk_when(_x7>=1))
 | _(*1211*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1205*) pos_ -> let rec _x159 _x7  = _t(function
 | 1206 ->
 (fun pos_ -> _x158 (_x7) )
 | _(*1208*) ->
 (fun pos_ -> _x159 (_x7+1) )) in _x159 (0) )),_x157))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __p41 = _dwhen 1200;;
let __a33 = _d 1021;;
let __a42 = _d 1157;;
let __p64 = _dwhen 1060;;
let __a58 = fun p v -> _d 1058 p (_d 1057 p (v));;
let __a56 = _d 1023;;
let __a13 = fun p v -> _d 1149 p (_d 1145 p (_d 1144 p (_d 1141 p (_d 1140 p (_d 1133 p (_d 1132 p (_d 1131 p (_d 1130 p (_d 1129 p (_x122 p (v)))))))))));;
let __a34 = fun p v -> _d 1036 p (_d 1035 p (v));;
let __a20 = _d 1074;;
let __a12 = fun p v -> _d 1147 p (_d 1145 p (_d 1144 p (_d 1141 p (_d 1140 p (_d 1133 p (_d 1132 p (_d 1131 p (_d 1130 p (_d 1129 p (_x122 p (v)))))))))));;
let __a72 = _d 1027;;
let __a55 = fun p v -> _p 1013 p (_p 1012 p (_p 1011 p (_p 1010 p (_p_pos 1008 p (_p 1007 p (v))))));;
let __g54 = _darg 1174;;
let __g24 = _darg 1126;;
let __a6 = fun p v -> _d 1109 p (_d 1108 p (_d 1107 p (_d 1106 p (_d 1105 p (_d 1104 p (_x103 p (v)))))));;
let __g22 = _darg 1090;;
let __a28 = _d 1186;;
let __g68 = _darg 1177;;
let __a7 = fun p v -> _d 1124 p (_d 1123 p (_d 1122 p (_d 1121 p (_d 1120 p (_x110 p (v))))));;
let __a44 = _d 1188;;
let __a60 = _d 1160;;
let __g50 = _darg 1093;;
let __a38 = _d 1112;;
let __a52 = _d 1113;;
let __a16 = fun p v -> _d 1185 p (_d 1184 p (_x152 p (v)));;
let __a51 = _d 1114;;
let __g69 = _darg 1097;;
let __p49 = _dwhen 1210;;
let __g73 = _darg 1099;;
let __a10 = fun p v -> _d 1133 p (_d 1132 p (_d 1131 p (_d 1130 p (_d 1129 p (_x122 p (v))))));;
let __a15 = fun p v -> _d 1171 p (_d 1170 p (_d 1169 p (_d 1168 p (_d 1167 p (_x148 p (v))))));;
let __g19 = _darg 1071;;
let __g53 = _darg 1158;;
let __p45 = _dnext 1191;;
let __g62 = _darg 1024;;
let __a35 = fun p v -> _d 1053 p (_d 1052 p (v));;
let __a1 = fun p v -> _d 1033 p (_x50 p (v));;
let __a5 = fun p v -> _d 1089 p (_d 1088 p (_d 1087 p (_d 1086 p (_d 1085 p (_d 1084 p (_x88 p (v)))))));;
let __g71 = _darg 1180;;
let __p40 = _dnext 1201;;
let __a31 = fun p v -> _p 1015 p (_p 1014 p (v));;
let __a17 = fun p v -> _d 1205 p (_d 1204 p (_x160 p (v)));;
let __a11 = fun p v -> _d 1143 p (_d 1141 p (_d 1140 p (_d 1133 p (_d 1132 p (_d 1131 p (_d 1130 p (_d 1129 p (_x122 p (v)))))))));;
let __a0 = fun p v -> _d 1018 p (_d 1017 p (_x45 p (v)));;
let __g74 = _darg 1028;;
let __p63 = _dnext 1061;;
let __g26 = _darg 1136;;
let __a25 = _d 1196;;
let __a8 = fun p v -> _d 1195 p (_d 1194 p (_x156 p (v)));;
let __g27 = _darg 1138;;
let __a29 = _d 1206;;
let __a39 = _d 1198;;
let __g23 = _darg 1110;;
let __a9 = fun p v -> _d 1135 p (_d 1133 p (_d 1132 p (_d 1131 p (_d 1130 p (_d 1129 p (_x122 p (v)))))));;
let __a18 = _p 1001;;
let __g67 = _darg 1161;;
let __a47 = _d 1208;;
let __a32 = _p 1002;;
let __a3 = fun p v -> _d 1067 p (_d 1066 p (_d 1065 p (_d 1064 p (_d 1063 p (_x65 p (v))))));;
let __a30 = fun p v -> _p_pos 1004 p (_p 1003 p (v));;
let __a43 = _d 1173;;
let __g70 = _darg 1164;;
let __g59 = _darg 1115;;
let __a14 = fun p v -> _d 1155 p (_d 1154 p (_d 1153 p (_d 1152 p (_d 1151 p (_x135 p (v))))));;
let __g75 = _darg 1031;;
let __g66 = _darg 1117;;
let __a61 = _d 1176;;
let __g21 = _darg 1081;;
let __a36 = _d 1092;;
let __a2 = fun p v -> _d 1050 p (_d 1049 p (_d 1048 p (_d 1047 p (_d 1046 p (_x58 p (v))))));;
let __a57 = fun p v -> _d 1044 p (_d 1043 p (_d 1041 p (_d 1040 p (v))));;
let __p48 = _dnext 1211;;
let __a4 = fun p v -> _d 1080 p (_d 1079 p (_d 1078 p (_d 1077 p (_d 1076 p (_x72 p (v))))));;
let __a37 = _d 1102;;
let __a65 = _d 1096;;
let __p46 = _dwhen 1190;;
let __binder0 = __default_ret;;
let __binder1 = _dret 1020;;
let __binder2 = _dret 1072;;
let __binder3 = _dret 1082;;
let __binder4 = _dret 1091;;
let __binder5 = _dret 1111;;
let __binder6 = _dret 1127;;
let __binder7 = _dret 1137;;
let __binder8 = _dret 1139;;
let __binder9 = _dret 1094;;
let __binder10 = _dret 1159;;
let __binder11 = _dret 1175;;
let __binder12 = _dret 1116;;
let __binder13 = _dret 1025;;
let __binder14 = _dret 1118;;
let __binder15 = _dret 1162;;
let __binder16 = _dret 1178;;
let __binder17 = _dret 1098;;
let __binder18 = _dret 1165;;
let __binder19 = _dret 1181;;
let __binder20 = _dret 1100;;
let __binder21 = _dret 1029;;
let __binder22 = _dret 1032;;
let binders : (sv -> sv -> sv) array = [| |]
let num_symbols = 49

let symbol_table = function
  | 292 -> "rulename-body"
  | 295 -> "alternation"
  | 300 -> "group"
  | 294 -> "elements"
  | 291 -> "inside-string"
  | 287 -> "c-nl"
  | 299 -> "element"
  | 279 -> "line-end-line"
  | 311 -> "inside-prose"
  | 308 -> "hex-val"
  | 281 -> "rule"
  | 271 -> "LF"
  | 277 -> "line"
  | 302 -> "bitstring"
  | 265 -> "BIT"
  | 309 -> "num-val"
  | 297 -> "repetition"
  | 273 -> "SP"
  | 264 -> "ALPHA"
  | 282 -> "defined-as"
  | 270 -> "HTAB"
  | 290 -> "string"
  | 286 -> "o"
  | 312 -> "prose-val"
  | 285 -> "indent"
  | 304 -> "DIGITS"
  | 274 -> "VCHAR"
  | 275 -> "WSP"
  | 278 -> "not-line-end"
  | 268 -> "DIGIT"
  | 269 -> "DQUOTE"
  | 283 -> "sp-htab"
  | 301 -> "option"
  | 266 -> "CHAR"
  | 310 -> "char-val"
  | 296 -> "concatenation"
  | 272 -> "OCTET"
  | 307 -> "HEXDIGS"
  | 267 -> "CR"
  | 280 -> "nlf-comment"
  | 305 -> "dec-val"
  | 288 -> "comment"
  | 289 -> "BACKSLASH"
  | 284 -> "rule-indent"
  | 276 -> "rfc"
  | 298 -> "repeat"
  | 306 -> "HEXDIG"
  | 303 -> "bin-val"
  | 293 -> "rulename"
  | x -> if x < 256 then Yak.Pam_internal.default_symbol_table x else "?unknown?"

let get_symb_action = function
  | "rulename-body" -> 292
  | "alternation" -> 295
  | "group" -> 300
  | "elements" -> 294
  | "inside-string" -> 291
  | "c-nl" -> 287
  | "element" -> 299
  | "line-end-line" -> 279
  | "inside-prose" -> 311
  | "hex-val" -> 308
  | "rule" -> 281
  | "LF" -> 271
  | "line" -> 277
  | "bitstring" -> 302
  | "BIT" -> 265
  | "num-val" -> 309
  | "repetition" -> 297
  | "SP" -> 273
  | "ALPHA" -> 264
  | "defined-as" -> 282
  | "HTAB" -> 270
  | "string" -> 290
  | "o" -> 286
  | "prose-val" -> 312
  | "indent" -> 285
  | "DIGITS" -> 304
  | "VCHAR" -> 274
  | "WSP" -> 275
  | "not-line-end" -> 278
  | "DIGIT" -> 268
  | "DQUOTE" -> 269
  | "sp-htab" -> 283
  | "option" -> 301
  | "CHAR" -> 266
  | "char-val" -> 310
  | "concatenation" -> 296
  | "OCTET" -> 272
  | "HEXDIGS" -> 307
  | "CR" -> 267
  | "nlf-comment" -> 280
  | "dec-val" -> 305
  | "comment" -> 288
  | "BACKSLASH" -> 289
  | "rule-indent" -> 284
  | "rfc" -> 276
  | "repeat" -> 298
  | "HEXDIG" -> 306
  | "bin-val" -> 303
  | "rulename" -> 293
  | _ -> raise Not_found

let get_symb_start = function
  | 312 -> 49
  | 311 -> 48
  | 310 -> 47
  | 309 -> 46
  | 308 -> 45
  | 307 -> 44
  | 306 -> 43
  | 305 -> 42
  | 304 -> 41
  | 303 -> 40
  | 302 -> 39
  | 301 -> 38
  | 300 -> 37
  | 299 -> 36
  | 298 -> 35
  | 297 -> 34
  | 296 -> 33
  | 295 -> 32
  | 294 -> 31
  | 293 -> 30
  | 292 -> 29
  | 291 -> 28
  | 290 -> 27
  | 289 -> 26
  | 288 -> 25
  | 287 -> 24
  | 286 -> 23
  | 285 -> 22
  | 284 -> 21
  | 283 -> 20
  | 282 -> 19
  | 281 -> 18
  | 280 -> 17
  | 279 -> 16
  | 278 -> 15
  | 277 -> 14
  | 276 -> 13
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
let rec nullable_rulename_body __lookahead _p0_ _x0_ = None

and nullable_alternation __lookahead _p0_ _x0_ = None

and nullable_group __lookahead _p0_ _x0_ = None

and nullable_elements __lookahead _p0_ _x0_ = None

and nullable_c_nl __lookahead _p0_ _x0_ = None

and nullable_inside_string __lookahead _p0_ _x0_ = None

and nullable_element __lookahead _p0_ _x0_ = None

and nullable_line_end_line __lookahead _p0_ _x0_ = None

and nullable_inside_prose __lookahead _p0_ _x0_ = None

and nullable_hex_val __lookahead _p0_ _x0_ = None

and nullable_rule __lookahead _p0_ _x0_ = None

and nullable_LF __lookahead _p0_ _x0_ = None

and nullable_line = let __tbl = SV_hashtbl.create 11 in
fun __lookahead _p0_ _x0_ -> 
let __p1 = Yak.YkBuf.get_offset _p0_ in
try
let (r, __p2)  = SV_hashtbl.find __tbl _x0_ in
if __p1 = __p2 then r else
let x = ((((Pred.full_lookaheadc false 278 15) __lookahead) _p0_) _x0_) in SV_hashtbl.replace __tbl _x0_ (x, __p1); x
with Not_found ->
  let x = ((((Pred.full_lookaheadc false 278 15) __lookahead) _p0_) _x0_) in SV_hashtbl.add __tbl _x0_ (x, __p1); x

and nullable_bitstring __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1190 and n = _dnext 1191 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (Pred.full_lookaheadc false 265 2)) __lookahead) _p0_) (((_d 1186) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1185 p (_d 1184 p (_x152 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_BIT __lookahead _p0_ _x0_ = None

and nullable_num_val __lookahead _p0_ _x0_ = None

and nullable_repetition __lookahead _p0_ _x0_ = None

and nullable_SP __lookahead _p0_ _x0_ = None

and nullable_ALPHA __lookahead _p0_ _x0_ = None

and nullable_defined_as __lookahead _p0_ _x0_ = None

and nullable_HTAB __lookahead _p0_ _x0_ = None

and nullable_string __lookahead _p0_ _x0_ = None

and nullable_o __lookahead _p0_ _x0_ = ((((Pred.full_lookaheadc false 283 20) __lookahead) _p0_) (((_d 1074) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1067 p (_d 1066 p (_d 1065 p (_d 1064 p (_d 1063 p (_x65 p (v))))))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_prose_val __lookahead _p0_ _x0_ = None

and nullable_indent __lookahead _p0_ _x0_ = None

and nullable_DIGITS = let __tbl = SV_hashtbl.create 11 in
fun __lookahead _p0_ _x0_ -> 
let __p1 = Yak.YkBuf.get_offset _p0_ in
try
let (r, __p2)  = SV_hashtbl.find __tbl _x0_ in
if __p1 = __p2 then r else
let x = ((((Pred.andc (let p = _dwhen 1200 and n = _dnext 1201 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (Pred.full_lookaheadc false 268 5)) __lookahead) _p0_) (((_d 1196) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1195 p (_d 1194 p (_x156 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_))) in SV_hashtbl.replace __tbl _x0_ (x, __p1); x
with Not_found ->
  let x = ((((Pred.andc (let p = _dwhen 1200 and n = _dnext 1201 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (Pred.full_lookaheadc false 268 5)) __lookahead) _p0_) (((_d 1196) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1195 p (_d 1194 p (_x156 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_))) in SV_hashtbl.add __tbl _x0_ (x, __p1); x

and nullable_VCHAR __lookahead _p0_ _x0_ = None

and nullable_WSP __lookahead _p0_ _x0_ = None

and nullable_not_line_end __lookahead _p0_ _x0_ = None

and nullable_DIGIT __lookahead _p0_ _x0_ = None

and nullable_DQUOTE __lookahead _p0_ _x0_ = None

and nullable_sp_htab __lookahead _p0_ _x0_ = None

and nullable_option __lookahead _p0_ _x0_ = None

and nullable_CHAR __lookahead _p0_ _x0_ = None

and nullable_concatenation __lookahead _p0_ _x0_ = None

and nullable_char_val __lookahead _p0_ _x0_ = None

and nullable_OCTET __lookahead _p0_ _x0_ = None

and nullable_HEXDIGS __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1210 and n = _dnext 1211 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (Pred.full_lookaheadc false 306 43)) __lookahead) _p0_) (((_d 1206) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1205 p (_d 1204 p (_x160 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_CR __lookahead _p0_ _x0_ = None

and nullable_nlf_comment __lookahead _p0_ _x0_ = None

and nullable_dec_val __lookahead _p0_ _x0_ = None

and nullable_comment __lookahead _p0_ _x0_ = None

and nullable_BACKSLASH __lookahead _p0_ _x0_ = None

and nullable_rule_indent __lookahead _p0_ _x0_ = None

and nullable_rfc __lookahead _p0_ _x0_ = ((((Pred.andc (let symb_pred = nullable_line
       and f_call = (fun _x1_ _x2_ -> (sv0))
       and f_ret = (fun _x1_ _x2_ _x3_ -> _x2_)
    in
    fun la ykb v ->
     let p = Yak.YkBuf.get_offset ykb in
     match symb_pred la ykb (f_call p v) with
        None -> None
      | Some v2 -> Some (f_ret p v v2)) (fun _x1_ _x2_ _x3_ -> (Some (((_p 1002) ((Yak.YkBuf.get_offset) _x2_)) (((_p 1001) ((Yak.YkBuf.get_offset) _x2_)) _x3_))))) __lookahead) _p0_) _x0_)

and nullable_repeat __lookahead _p0_ _x0_ = ((((Pred.orc (let symb_pred = nullable_DIGITS
       and f_call = (fun _x1_ _x2_ -> (sv0))
       and f_ret = (fun _x1_ _x2_ _x3_ -> _x2_)
    in
    fun la ykb v ->
     let p = Yak.YkBuf.get_offset ykb in
     match symb_pred la ykb (f_call p v) with
        None -> None
      | Some v2 -> Some (f_ret p v v2)) (fun _x1_ _x2_ _x3_ -> None)) __lookahead) _p0_) _x0_)

and nullable_HEXDIG __lookahead _p0_ _x0_ = None

and nullable_bin_val __lookahead _p0_ _x0_ = None

and nullable_rulename __lookahead _p0_ _x0_ = None

let program : (int * sv instruction list) list = [
(191, [AWhenInstr3(__p64,__p63,197)]);
(0, [ASimpleCont2Instr(312,__binder0,49);ASimpleCont2Instr(311,__binder0,48);ASimpleCont2Instr(310,__binder0,47);ASimpleCont2Instr(309,__binder0,46);ASimpleCont2Instr(308,__binder0,45);ASimpleCont2Instr(307,__binder0,44);ASimpleCont2Instr(306,__binder0,43);ASimpleCont2Instr(305,__binder0,42);ASimpleCont2Instr(304,__binder0,41);ASimpleCont2Instr(303,__binder0,40);ASimpleCont2Instr(302,__binder0,39);ASimpleCont2Instr(301,__binder0,38);ASimpleCont2Instr(300,__binder0,37);ASimpleCont2Instr(299,__binder0,36);ASimpleCont2Instr(298,__binder0,35);ASimpleCont2Instr(297,__binder0,34);ASimpleCont2Instr(296,__binder0,33);ASimpleCont2Instr(295,__binder0,32);ASimpleCont2Instr(294,__binder0,31);ASimpleCont2Instr(293,__binder0,30);ASimpleCont2Instr(292,__binder0,29);ASimpleCont2Instr(291,__binder0,28);ASimpleCont2Instr(290,__binder0,27);ASimpleCont2Instr(289,__binder0,26);ASimpleCont2Instr(288,__binder0,25);ASimpleCont2Instr(287,__binder0,24);ASimpleCont2Instr(286,__binder0,23);ASimpleCont2Instr(285,__binder0,22);ASimpleCont2Instr(284,__binder0,21);ASimpleCont2Instr(283,__binder0,20);ASimpleCont2Instr(282,__binder0,19);ASimpleCont2Instr(281,__binder0,18);ASimpleCont2Instr(280,__binder0,17);ASimpleCont2Instr(279,__binder0,16);ASimpleCont2Instr(278,__binder0,15);ASimpleCont2Instr(277,__binder0,14);ASimpleCont2Instr(276,__binder0,13);ASimpleCont2Instr(275,__binder0,12);ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(192, [AAction2Instr(__a65,198)]);
(1, [EatInstr(122,50);EatInstr(121,50);EatInstr(120,50);EatInstr(119,50);EatInstr(118,50);EatInstr(117,50);EatInstr(116,50);EatInstr(115,50);EatInstr(114,50);EatInstr(113,50);EatInstr(112,50);EatInstr(111,50);EatInstr(110,50);EatInstr(109,50);EatInstr(108,50);EatInstr(107,50);EatInstr(106,50);EatInstr(105,50);EatInstr(104,50);EatInstr(103,50);EatInstr(102,50);EatInstr(101,50);EatInstr(100,50);EatInstr(99,50);EatInstr(98,50);EatInstr(97,50);EatInstr(90,50);EatInstr(89,50);EatInstr(88,50);EatInstr(87,50);EatInstr(86,50);EatInstr(85,50);EatInstr(84,50);EatInstr(83,50);EatInstr(82,50);EatInstr(81,50);EatInstr(80,50);EatInstr(79,50);EatInstr(78,50);EatInstr(77,50);EatInstr(76,50);EatInstr(75,50);EatInstr(74,50);EatInstr(73,50);EatInstr(72,50);EatInstr(71,50);EatInstr(70,50);EatInstr(69,50);EatInstr(68,50);EatInstr(67,50);EatInstr(66,50);EatInstr(65,50)]);
(193, [AContInstr3(297,__g66,__binder14,159);ACallInstr3(__g66,34)]);
(2, [EatInstr(49,51);EatInstr(48,51)]);
(194, [AContInstr3(295,__g67,__binder15,199);ACallInstr3(__g67,32)]);
(3, [EatInstr(127,52);EatInstr(126,52);EatInstr(125,52);EatInstr(124,52);EatInstr(123,52);EatInstr(96,52);EatInstr(95,52);EatInstr(94,52);EatInstr(93,52);EatInstr(92,52);EatInstr(91,52);EatInstr(64,52);EatInstr(63,52);EatInstr(62,52);EatInstr(61,52);EatInstr(60,52);EatInstr(59,52);EatInstr(58,52);EatInstr(57,52);EatInstr(56,52);EatInstr(55,52);EatInstr(54,52);EatInstr(53,52);EatInstr(52,52);EatInstr(51,52);EatInstr(50,52);EatInstr(47,52);EatInstr(46,52);EatInstr(45,52);EatInstr(44,52);EatInstr(43,52);EatInstr(42,52);EatInstr(41,52);EatInstr(40,52);EatInstr(39,52);EatInstr(38,52);EatInstr(37,52);EatInstr(36,52);EatInstr(35,52);EatInstr(34,52);EatInstr(33,52);EatInstr(32,52);EatInstr(31,52);EatInstr(30,52);EatInstr(29,52);EatInstr(28,52);EatInstr(27,52);EatInstr(26,52);EatInstr(25,52);EatInstr(24,52);EatInstr(23,52);EatInstr(22,52);EatInstr(21,52);EatInstr(20,52);EatInstr(19,52);EatInstr(18,52);EatInstr(17,52);EatInstr(16,52);EatInstr(15,52);EatInstr(14,52);EatInstr(13,52);EatInstr(12,52);EatInstr(11,52);EatInstr(10,52);EatInstr(9,52);EatInstr(8,52);EatInstr(7,52);EatInstr(6,52);EatInstr(5,52);EatInstr(4,52);EatInstr(3,52);EatInstr(2,52);EatInstr(1,52);EatInstr(49,52);EatInstr(48,52);EatInstr(122,52);EatInstr(121,52);EatInstr(120,52);EatInstr(119,52);EatInstr(118,52);EatInstr(117,52);EatInstr(116,52);EatInstr(115,52);EatInstr(114,52);EatInstr(113,52);EatInstr(112,52);EatInstr(111,52);EatInstr(110,52);EatInstr(109,52);EatInstr(108,52);EatInstr(107,52);EatInstr(106,52);EatInstr(105,52);EatInstr(104,52);EatInstr(103,52);EatInstr(102,52);EatInstr(101,52);EatInstr(100,52);EatInstr(99,52);EatInstr(98,52);EatInstr(97,52);EatInstr(90,52);EatInstr(89,52);EatInstr(88,52);EatInstr(87,52);EatInstr(86,52);EatInstr(85,52);EatInstr(84,52);EatInstr(83,52);EatInstr(82,52);EatInstr(81,52);EatInstr(80,52);EatInstr(79,52);EatInstr(78,52);EatInstr(77,52);EatInstr(76,52);EatInstr(75,52);EatInstr(74,52);EatInstr(73,52);EatInstr(72,52);EatInstr(71,52);EatInstr(70,52);EatInstr(69,52);EatInstr(68,52);EatInstr(67,52);EatInstr(66,52);EatInstr(65,52)]);
(195, [AContInstr3(295,__g68,__binder16,200);ACallInstr3(__g68,32)]);
(4, [EatInstr(13,53)]);
(196, [ACallInstr3(__default_call,19);ASimpleCont2Instr(282,__binder0,201)]);
(5, [EatInstr(57,54);EatInstr(56,54);EatInstr(55,54);EatInstr(54,54);EatInstr(53,54);EatInstr(52,54);EatInstr(51,54);EatInstr(50,54);EatInstr(49,54);EatInstr(48,54)]);
(197, [CompleteInstr(285)]);
(6, [EatInstr(34,55)]);
(198, [AContInstr3(286,__g69,__binder17,202);ACallInstr3(__g69,23)]);
(7, [EatInstr(9,56)]);
(199, [AContInstr3(286,__g70,__binder18,203);ACallInstr3(__g70,23)]);
(8, [EatInstr(10,57)]);
(200, [AContInstr3(286,__g71,__binder19,204);ACallInstr3(__g71,23)]);
(9, [EatInstr(255,58);EatInstr(254,58);EatInstr(253,58);EatInstr(252,58);EatInstr(251,58);EatInstr(250,58);EatInstr(249,58);EatInstr(248,58);EatInstr(247,58);EatInstr(246,58);EatInstr(245,58);EatInstr(244,58);EatInstr(243,58);EatInstr(242,58);EatInstr(241,58);EatInstr(240,58);EatInstr(239,58);EatInstr(238,58);EatInstr(237,58);EatInstr(236,58);EatInstr(235,58);EatInstr(234,58);EatInstr(233,58);EatInstr(232,58);EatInstr(231,58);EatInstr(230,58);EatInstr(229,58);EatInstr(228,58);EatInstr(227,58);EatInstr(226,58);EatInstr(225,58);EatInstr(224,58);EatInstr(223,58);EatInstr(222,58);EatInstr(221,58);EatInstr(220,58);EatInstr(219,58);EatInstr(218,58);EatInstr(217,58);EatInstr(216,58);EatInstr(215,58);EatInstr(214,58);EatInstr(213,58);EatInstr(212,58);EatInstr(211,58);EatInstr(210,58);EatInstr(209,58);EatInstr(208,58);EatInstr(207,58);EatInstr(206,58);EatInstr(205,58);EatInstr(204,58);EatInstr(203,58);EatInstr(202,58);EatInstr(201,58);EatInstr(200,58);EatInstr(199,58);EatInstr(198,58);EatInstr(197,58);EatInstr(196,58);EatInstr(195,58);EatInstr(194,58);EatInstr(193,58);EatInstr(192,58);EatInstr(191,58);EatInstr(190,58);EatInstr(189,58);EatInstr(188,58);EatInstr(187,58);EatInstr(186,58);EatInstr(185,58);EatInstr(184,58);EatInstr(183,58);EatInstr(182,58);EatInstr(181,58);EatInstr(180,58);EatInstr(179,58);EatInstr(178,58);EatInstr(177,58);EatInstr(176,58);EatInstr(175,58);EatInstr(174,58);EatInstr(173,58);EatInstr(172,58);EatInstr(171,58);EatInstr(170,58);EatInstr(169,58);EatInstr(168,58);EatInstr(167,58);EatInstr(166,58);EatInstr(165,58);EatInstr(164,58);EatInstr(163,58);EatInstr(162,58);EatInstr(161,58);EatInstr(160,58);EatInstr(159,58);EatInstr(158,58);EatInstr(157,58);EatInstr(156,58);EatInstr(155,58);EatInstr(154,58);EatInstr(153,58);EatInstr(152,58);EatInstr(151,58);EatInstr(150,58);EatInstr(149,58);EatInstr(148,58);EatInstr(147,58);EatInstr(146,58);EatInstr(145,58);EatInstr(144,58);EatInstr(143,58);EatInstr(142,58);EatInstr(141,58);EatInstr(140,58);EatInstr(139,58);EatInstr(138,58);EatInstr(137,58);EatInstr(136,58);EatInstr(135,58);EatInstr(134,58);EatInstr(133,58);EatInstr(132,58);EatInstr(131,58);EatInstr(130,58);EatInstr(129,58);EatInstr(128,58);EatInstr(0,58);EatInstr(127,58);EatInstr(126,58);EatInstr(125,58);EatInstr(124,58);EatInstr(123,58);EatInstr(96,58);EatInstr(95,58);EatInstr(94,58);EatInstr(93,58);EatInstr(92,58);EatInstr(91,58);EatInstr(64,58);EatInstr(63,58);EatInstr(62,58);EatInstr(61,58);EatInstr(60,58);EatInstr(59,58);EatInstr(58,58);EatInstr(57,58);EatInstr(56,58);EatInstr(55,58);EatInstr(54,58);EatInstr(53,58);EatInstr(52,58);EatInstr(51,58);EatInstr(50,58);EatInstr(47,58);EatInstr(46,58);EatInstr(45,58);EatInstr(44,58);EatInstr(43,58);EatInstr(42,58);EatInstr(41,58);EatInstr(40,58);EatInstr(39,58);EatInstr(38,58);EatInstr(37,58);EatInstr(36,58);EatInstr(35,58);EatInstr(34,58);EatInstr(33,58);EatInstr(32,58);EatInstr(31,58);EatInstr(30,58);EatInstr(29,58);EatInstr(28,58);EatInstr(27,58);EatInstr(26,58);EatInstr(25,58);EatInstr(24,58);EatInstr(23,58);EatInstr(22,58);EatInstr(21,58);EatInstr(20,58);EatInstr(19,58);EatInstr(18,58);EatInstr(17,58);EatInstr(16,58);EatInstr(15,58);EatInstr(14,58);EatInstr(13,58);EatInstr(12,58);EatInstr(11,58);EatInstr(10,58);EatInstr(9,58);EatInstr(8,58);EatInstr(7,58);EatInstr(6,58);EatInstr(5,58);EatInstr(4,58);EatInstr(3,58);EatInstr(2,58);EatInstr(1,58);EatInstr(49,58);EatInstr(48,58);EatInstr(122,58);EatInstr(121,58);EatInstr(120,58);EatInstr(119,58);EatInstr(118,58);EatInstr(117,58);EatInstr(116,58);EatInstr(115,58);EatInstr(114,58);EatInstr(113,58);EatInstr(112,58);EatInstr(111,58);EatInstr(110,58);EatInstr(109,58);EatInstr(108,58);EatInstr(107,58);EatInstr(106,58);EatInstr(105,58);EatInstr(104,58);EatInstr(103,58);EatInstr(102,58);EatInstr(101,58);EatInstr(100,58);EatInstr(99,58);EatInstr(98,58);EatInstr(97,58);EatInstr(90,58);EatInstr(89,58);EatInstr(88,58);EatInstr(87,58);EatInstr(86,58);EatInstr(85,58);EatInstr(84,58);EatInstr(83,58);EatInstr(82,58);EatInstr(81,58);EatInstr(80,58);EatInstr(79,58);EatInstr(78,58);EatInstr(77,58);EatInstr(76,58);EatInstr(75,58);EatInstr(74,58);EatInstr(73,58);EatInstr(72,58);EatInstr(71,58);EatInstr(70,58);EatInstr(69,58);EatInstr(68,58);EatInstr(67,58);EatInstr(66,58);EatInstr(65,58)]);
(201, [AAction2Instr(__a72,205)]);
(10, [EatInstr(32,59)]);
(202, [AContInstr3(295,__g73,__binder20,158);ACallInstr3(__g73,32)]);
(11, [EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(47,60);EatInstr(46,60);EatInstr(45,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(49,60);EatInstr(48,60);EatInstr(122,60);EatInstr(121,60);EatInstr(120,60);EatInstr(119,60);EatInstr(118,60);EatInstr(117,60);EatInstr(116,60);EatInstr(115,60);EatInstr(114,60);EatInstr(113,60);EatInstr(112,60);EatInstr(111,60);EatInstr(110,60);EatInstr(109,60);EatInstr(108,60);EatInstr(107,60);EatInstr(106,60);EatInstr(105,60);EatInstr(104,60);EatInstr(103,60);EatInstr(102,60);EatInstr(101,60);EatInstr(100,60);EatInstr(99,60);EatInstr(98,60);EatInstr(97,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60)]);
(203, [EatInstr(41,206)]);
(12, [EatInstr(32,59);EatInstr(9,56);ASimpleCont2Instr(273,__binder0,61);ASimpleCont2Instr(270,__binder0,61)]);
(204, [EatInstr(93,207)]);
(13, [EatInstr(127,64);EatInstr(126,64);EatInstr(125,64);EatInstr(124,64);EatInstr(123,64);EatInstr(96,64);EatInstr(95,64);EatInstr(94,64);EatInstr(93,64);EatInstr(92,64);EatInstr(91,64);EatInstr(64,64);EatInstr(63,64);EatInstr(62,64);EatInstr(61,64);EatInstr(60,64);EatInstr(59,64);EatInstr(58,64);EatInstr(57,64);EatInstr(56,64);EatInstr(55,64);EatInstr(54,64);EatInstr(53,64);EatInstr(52,64);EatInstr(51,64);EatInstr(50,64);EatInstr(47,64);EatInstr(46,64);EatInstr(45,64);EatInstr(44,64);EatInstr(43,64);EatInstr(42,64);EatInstr(41,64);EatInstr(40,64);EatInstr(39,64);EatInstr(38,64);EatInstr(37,64);EatInstr(36,64);EatInstr(35,64);EatInstr(34,64);EatInstr(33,64);EatInstr(32,64);EatInstr(31,64);EatInstr(30,64);EatInstr(29,64);EatInstr(28,64);EatInstr(27,64);EatInstr(26,64);EatInstr(25,64);EatInstr(24,64);EatInstr(23,64);EatInstr(22,64);EatInstr(21,64);EatInstr(20,64);EatInstr(19,64);EatInstr(18,64);EatInstr(17,64);EatInstr(16,64);EatInstr(15,64);EatInstr(14,64);EatInstr(12,64);EatInstr(11,64);EatInstr(9,64);EatInstr(8,64);EatInstr(7,64);EatInstr(6,64);EatInstr(5,64);EatInstr(4,64);EatInstr(3,64);EatInstr(2,64);EatInstr(1,64);EatInstr(49,64);EatInstr(48,64);EatInstr(122,64);EatInstr(121,64);EatInstr(120,64);EatInstr(119,64);EatInstr(118,64);EatInstr(117,64);EatInstr(116,64);EatInstr(115,64);EatInstr(114,64);EatInstr(113,64);EatInstr(112,64);EatInstr(111,64);EatInstr(110,64);EatInstr(109,64);EatInstr(108,64);EatInstr(107,64);EatInstr(106,64);EatInstr(105,64);EatInstr(104,64);EatInstr(103,64);EatInstr(102,64);EatInstr(101,64);EatInstr(100,64);EatInstr(99,64);EatInstr(98,64);EatInstr(97,64);EatInstr(90,64);EatInstr(89,64);EatInstr(88,64);EatInstr(87,64);EatInstr(86,64);EatInstr(85,64);EatInstr(84,64);EatInstr(83,64);EatInstr(82,64);EatInstr(81,64);EatInstr(80,64);EatInstr(79,64);EatInstr(78,64);EatInstr(77,64);EatInstr(76,64);EatInstr(75,64);EatInstr(74,64);EatInstr(73,64);EatInstr(72,64);EatInstr(71,64);EatInstr(70,64);EatInstr(69,64);EatInstr(68,64);EatInstr(67,64);EatInstr(66,64);EatInstr(65,64);ALookaheadInstr(false,CfgLA (15,278),66);RCompleteInstr2(277,nullable_line);RCompleteInstr2(276,nullable_rfc);ASimpleCont2Instr(278,__binder0,63);ASimpleCont2Instr(277,__binder0,62)]);
(205, [AContInstr3(286,__g74,__binder21,208);ACallInstr3(__g74,23)]);
(14, [EatInstr(127,64);EatInstr(126,64);EatInstr(125,64);EatInstr(124,64);EatInstr(123,64);EatInstr(96,64);EatInstr(95,64);EatInstr(94,64);EatInstr(93,64);EatInstr(92,64);EatInstr(91,64);EatInstr(64,64);EatInstr(63,64);EatInstr(62,64);EatInstr(61,64);EatInstr(60,64);EatInstr(59,64);EatInstr(58,64);EatInstr(57,64);EatInstr(56,64);EatInstr(55,64);EatInstr(54,64);EatInstr(53,64);EatInstr(52,64);EatInstr(51,64);EatInstr(50,64);EatInstr(47,64);EatInstr(46,64);EatInstr(45,64);EatInstr(44,64);EatInstr(43,64);EatInstr(42,64);EatInstr(41,64);EatInstr(40,64);EatInstr(39,64);EatInstr(38,64);EatInstr(37,64);EatInstr(36,64);EatInstr(35,64);EatInstr(34,64);EatInstr(33,64);EatInstr(32,64);EatInstr(31,64);EatInstr(30,64);EatInstr(29,64);EatInstr(28,64);EatInstr(27,64);EatInstr(26,64);EatInstr(25,64);EatInstr(24,64);EatInstr(23,64);EatInstr(22,64);EatInstr(21,64);EatInstr(20,64);EatInstr(19,64);EatInstr(18,64);EatInstr(17,64);EatInstr(16,64);EatInstr(15,64);EatInstr(14,64);EatInstr(12,64);EatInstr(11,64);EatInstr(9,64);EatInstr(8,64);EatInstr(7,64);EatInstr(6,64);EatInstr(5,64);EatInstr(4,64);EatInstr(3,64);EatInstr(2,64);EatInstr(1,64);EatInstr(49,64);EatInstr(48,64);EatInstr(122,64);EatInstr(121,64);EatInstr(120,64);EatInstr(119,64);EatInstr(118,64);EatInstr(117,64);EatInstr(116,64);EatInstr(115,64);EatInstr(114,64);EatInstr(113,64);EatInstr(112,64);EatInstr(111,64);EatInstr(110,64);EatInstr(109,64);EatInstr(108,64);EatInstr(107,64);EatInstr(106,64);EatInstr(105,64);EatInstr(104,64);EatInstr(103,64);EatInstr(102,64);EatInstr(101,64);EatInstr(100,64);EatInstr(99,64);EatInstr(98,64);EatInstr(97,64);EatInstr(90,64);EatInstr(89,64);EatInstr(88,64);EatInstr(87,64);EatInstr(86,64);EatInstr(85,64);EatInstr(84,64);EatInstr(83,64);EatInstr(82,64);EatInstr(81,64);EatInstr(80,64);EatInstr(79,64);EatInstr(78,64);EatInstr(77,64);EatInstr(76,64);EatInstr(75,64);EatInstr(74,64);EatInstr(73,64);EatInstr(72,64);EatInstr(71,64);EatInstr(70,64);EatInstr(69,64);EatInstr(68,64);EatInstr(67,64);EatInstr(66,64);EatInstr(65,64);ALookaheadInstr(false,CfgLA (15,278),66);RCompleteInstr2(277,nullable_line);ASimpleCont2Instr(278,__binder0,63)]);
(206, [CompleteInstr(300)]);
(15, [EatInstr(127,64);EatInstr(126,64);EatInstr(125,64);EatInstr(124,64);EatInstr(123,64);EatInstr(96,64);EatInstr(95,64);EatInstr(94,64);EatInstr(93,64);EatInstr(92,64);EatInstr(91,64);EatInstr(64,64);EatInstr(63,64);EatInstr(62,64);EatInstr(61,64);EatInstr(60,64);EatInstr(59,64);EatInstr(58,64);EatInstr(57,64);EatInstr(56,64);EatInstr(55,64);EatInstr(54,64);EatInstr(53,64);EatInstr(52,64);EatInstr(51,64);EatInstr(50,64);EatInstr(47,64);EatInstr(46,64);EatInstr(45,64);EatInstr(44,64);EatInstr(43,64);EatInstr(42,64);EatInstr(41,64);EatInstr(40,64);EatInstr(39,64);EatInstr(38,64);EatInstr(37,64);EatInstr(36,64);EatInstr(35,64);EatInstr(34,64);EatInstr(33,64);EatInstr(32,64);EatInstr(31,64);EatInstr(30,64);EatInstr(29,64);EatInstr(28,64);EatInstr(27,64);EatInstr(26,64);EatInstr(25,64);EatInstr(24,64);EatInstr(23,64);EatInstr(22,64);EatInstr(21,64);EatInstr(20,64);EatInstr(19,64);EatInstr(18,64);EatInstr(17,64);EatInstr(16,64);EatInstr(15,64);EatInstr(14,64);EatInstr(12,64);EatInstr(11,64);EatInstr(9,64);EatInstr(8,64);EatInstr(7,64);EatInstr(6,64);EatInstr(5,64);EatInstr(4,64);EatInstr(3,64);EatInstr(2,64);EatInstr(1,64);EatInstr(49,64);EatInstr(48,64);EatInstr(122,64);EatInstr(121,64);EatInstr(120,64);EatInstr(119,64);EatInstr(118,64);EatInstr(117,64);EatInstr(116,64);EatInstr(115,64);EatInstr(114,64);EatInstr(113,64);EatInstr(112,64);EatInstr(111,64);EatInstr(110,64);EatInstr(109,64);EatInstr(108,64);EatInstr(107,64);EatInstr(106,64);EatInstr(105,64);EatInstr(104,64);EatInstr(103,64);EatInstr(102,64);EatInstr(101,64);EatInstr(100,64);EatInstr(99,64);EatInstr(98,64);EatInstr(97,64);EatInstr(90,64);EatInstr(89,64);EatInstr(88,64);EatInstr(87,64);EatInstr(86,64);EatInstr(85,64);EatInstr(84,64);EatInstr(83,64);EatInstr(82,64);EatInstr(81,64);EatInstr(80,64);EatInstr(79,64);EatInstr(78,64);EatInstr(77,64);EatInstr(76,64);EatInstr(75,64);EatInstr(74,64);EatInstr(73,64);EatInstr(72,64);EatInstr(71,64);EatInstr(70,64);EatInstr(69,64);EatInstr(68,64);EatInstr(67,64);EatInstr(66,64);EatInstr(65,64)]);
(16, [ALookaheadInstr(false,CfgLA (18,281),67)]);
(207, [CompleteInstr(301)]);
(208, [AContInstr3(294,__g75,__binder22,209);ACallInstr3(__g75,31)]);
(17, [EatInstr(59,68)]);
(209, [CompleteInstr(281);ACallInstr3(__default_call,211);ASimpleCont2Instr(283,__binder0,209);ASimpleCont2Instr(280,__binder0,210)]);
(18, [AAction2Instr(__a0,69)]);
(210, [CompleteInstr(281)]);
(19, [EatInstr(61,70)]);
(211, [EatInstr(59,68);EatInstr(32,59);EatInstr(9,56);ASimpleCont2Instr(273,__binder0,71);ASimpleCont2Instr(270,__binder0,71)]);
(20, [EatInstr(32,59);EatInstr(9,56);ASimpleCont2Instr(273,__binder0,71);ASimpleCont2Instr(270,__binder0,71)]);
(21, [AAction2Instr(__a1,72)]);
(22, [AAction2Instr(__a2,73)]);
(23, [RCompleteInstr2(286,nullable_o);AAction2Instr(__a3,74)]);
(24, [EatInstr(59,76);EatInstr(13,53);EatInstr(10,57);ASimpleCont2Instr(288,__binder0,75);ASimpleCont2Instr(271,__binder0,75);ASimpleCont2Instr(267,__binder0,75)]);
(25, [EatInstr(59,76)]);
(26, [EatInstr(92,77)]);
(27, [EatInstr(34,55);ASimpleCont2Instr(269,__binder0,78)]);
(28, [EatInstr(255,80);EatInstr(254,80);EatInstr(253,80);EatInstr(252,80);EatInstr(251,80);EatInstr(250,80);EatInstr(249,80);EatInstr(248,80);EatInstr(247,80);EatInstr(246,80);EatInstr(245,80);EatInstr(244,80);EatInstr(243,80);EatInstr(242,80);EatInstr(241,80);EatInstr(240,80);EatInstr(239,80);EatInstr(238,80);EatInstr(237,80);EatInstr(236,80);EatInstr(235,80);EatInstr(234,80);EatInstr(233,80);EatInstr(232,80);EatInstr(231,80);EatInstr(230,80);EatInstr(229,80);EatInstr(228,80);EatInstr(227,80);EatInstr(226,80);EatInstr(225,80);EatInstr(224,80);EatInstr(223,80);EatInstr(222,80);EatInstr(221,80);EatInstr(220,80);EatInstr(219,80);EatInstr(218,80);EatInstr(217,80);EatInstr(216,80);EatInstr(215,80);EatInstr(214,80);EatInstr(213,80);EatInstr(212,80);EatInstr(211,80);EatInstr(210,80);EatInstr(209,80);EatInstr(208,80);EatInstr(207,80);EatInstr(206,80);EatInstr(205,80);EatInstr(204,80);EatInstr(203,80);EatInstr(202,80);EatInstr(201,80);EatInstr(200,80);EatInstr(199,80);EatInstr(198,80);EatInstr(197,80);EatInstr(196,80);EatInstr(195,80);EatInstr(194,80);EatInstr(193,80);EatInstr(192,80);EatInstr(191,80);EatInstr(190,80);EatInstr(189,80);EatInstr(188,80);EatInstr(187,80);EatInstr(186,80);EatInstr(185,80);EatInstr(184,80);EatInstr(183,80);EatInstr(182,80);EatInstr(181,80);EatInstr(180,80);EatInstr(179,80);EatInstr(178,80);EatInstr(177,80);EatInstr(176,80);EatInstr(175,80);EatInstr(174,80);EatInstr(173,80);EatInstr(172,80);EatInstr(171,80);EatInstr(170,80);EatInstr(169,80);EatInstr(168,80);EatInstr(167,80);EatInstr(166,80);EatInstr(165,80);EatInstr(164,80);EatInstr(163,80);EatInstr(162,80);EatInstr(161,80);EatInstr(160,80);EatInstr(159,80);EatInstr(158,80);EatInstr(157,80);EatInstr(156,80);EatInstr(155,80);EatInstr(154,80);EatInstr(153,80);EatInstr(152,80);EatInstr(151,80);EatInstr(150,80);EatInstr(149,80);EatInstr(148,80);EatInstr(147,80);EatInstr(146,80);EatInstr(145,80);EatInstr(144,80);EatInstr(143,80);EatInstr(142,80);EatInstr(141,80);EatInstr(140,80);EatInstr(139,80);EatInstr(138,80);EatInstr(137,80);EatInstr(136,80);EatInstr(135,80);EatInstr(134,80);EatInstr(133,80);EatInstr(132,80);EatInstr(131,80);EatInstr(130,80);EatInstr(129,80);EatInstr(128,80);EatInstr(0,80);EatInstr(127,80);EatInstr(126,80);EatInstr(125,80);EatInstr(124,80);EatInstr(123,80);EatInstr(96,80);EatInstr(95,80);EatInstr(94,80);EatInstr(93,80);EatInstr(92,77);EatInstr(91,80);EatInstr(64,80);EatInstr(63,80);EatInstr(62,80);EatInstr(61,80);EatInstr(60,80);EatInstr(59,80);EatInstr(58,80);EatInstr(57,80);EatInstr(56,80);EatInstr(55,80);EatInstr(54,80);EatInstr(53,80);EatInstr(52,80);EatInstr(51,80);EatInstr(50,80);EatInstr(47,80);EatInstr(46,80);EatInstr(45,80);EatInstr(44,80);EatInstr(43,80);EatInstr(42,80);EatInstr(41,80);EatInstr(40,80);EatInstr(39,80);EatInstr(38,80);EatInstr(37,80);EatInstr(36,80);EatInstr(35,80);EatInstr(33,80);EatInstr(32,80);EatInstr(31,80);EatInstr(30,80);EatInstr(29,80);EatInstr(28,80);EatInstr(27,80);EatInstr(26,80);EatInstr(25,80);EatInstr(24,80);EatInstr(23,80);EatInstr(22,80);EatInstr(21,80);EatInstr(20,80);EatInstr(19,80);EatInstr(18,80);EatInstr(17,80);EatInstr(16,80);EatInstr(15,80);EatInstr(14,80);EatInstr(13,80);EatInstr(12,80);EatInstr(11,80);EatInstr(10,80);EatInstr(9,80);EatInstr(8,80);EatInstr(7,80);EatInstr(6,80);EatInstr(5,80);EatInstr(4,80);EatInstr(3,80);EatInstr(2,80);EatInstr(1,80);EatInstr(49,80);EatInstr(48,80);EatInstr(122,80);EatInstr(121,80);EatInstr(120,80);EatInstr(119,80);EatInstr(118,80);EatInstr(117,80);EatInstr(116,80);EatInstr(115,80);EatInstr(114,80);EatInstr(113,80);EatInstr(112,80);EatInstr(111,80);EatInstr(110,80);EatInstr(109,80);EatInstr(108,80);EatInstr(107,80);EatInstr(106,80);EatInstr(105,80);EatInstr(104,80);EatInstr(103,80);EatInstr(102,80);EatInstr(101,80);EatInstr(100,80);EatInstr(99,80);EatInstr(98,80);EatInstr(97,80);EatInstr(90,80);EatInstr(89,80);EatInstr(88,80);EatInstr(87,80);EatInstr(86,80);EatInstr(85,80);EatInstr(84,80);EatInstr(83,80);EatInstr(82,80);EatInstr(81,80);EatInstr(80,80);EatInstr(79,80);EatInstr(78,80);EatInstr(77,80);EatInstr(76,80);EatInstr(75,80);EatInstr(74,80);EatInstr(73,80);EatInstr(72,80);EatInstr(71,80);EatInstr(70,80);EatInstr(69,80);EatInstr(68,80);EatInstr(67,80);EatInstr(66,80);EatInstr(65,80);ASimpleCont2Instr(289,__binder0,79)]);
(29, [EatInstr(58,81);EatInstr(57,54);EatInstr(56,54);EatInstr(55,54);EatInstr(54,54);EatInstr(53,54);EatInstr(52,54);EatInstr(51,54);EatInstr(50,54);EatInstr(45,81);EatInstr(49,54);EatInstr(48,54);EatInstr(122,50);EatInstr(121,50);EatInstr(120,50);EatInstr(119,50);EatInstr(118,50);EatInstr(117,50);EatInstr(116,50);EatInstr(115,50);EatInstr(114,50);EatInstr(113,50);EatInstr(112,50);EatInstr(111,50);EatInstr(110,50);EatInstr(109,50);EatInstr(108,50);EatInstr(107,50);EatInstr(106,50);EatInstr(105,50);EatInstr(104,50);EatInstr(103,50);EatInstr(102,50);EatInstr(101,50);EatInstr(100,50);EatInstr(99,50);EatInstr(98,50);EatInstr(97,50);EatInstr(90,50);EatInstr(89,50);EatInstr(88,50);EatInstr(87,50);EatInstr(86,50);EatInstr(85,50);EatInstr(84,50);EatInstr(83,50);EatInstr(82,50);EatInstr(81,50);EatInstr(80,50);EatInstr(79,50);EatInstr(78,50);EatInstr(77,50);EatInstr(76,50);EatInstr(75,50);EatInstr(74,50);EatInstr(73,50);EatInstr(72,50);EatInstr(71,50);EatInstr(70,50);EatInstr(69,50);EatInstr(68,50);EatInstr(67,50);EatInstr(66,50);EatInstr(65,50);ASimpleCont2Instr(268,__binder0,81);ASimpleCont2Instr(264,__binder0,81)]);
(30, [EatInstr(122,50);EatInstr(121,50);EatInstr(120,50);EatInstr(119,50);EatInstr(118,50);EatInstr(117,50);EatInstr(116,50);EatInstr(115,50);EatInstr(114,50);EatInstr(113,50);EatInstr(112,50);EatInstr(111,50);EatInstr(110,50);EatInstr(109,50);EatInstr(108,50);EatInstr(107,50);EatInstr(106,50);EatInstr(105,50);EatInstr(104,50);EatInstr(103,50);EatInstr(102,50);EatInstr(101,50);EatInstr(100,50);EatInstr(99,50);EatInstr(98,50);EatInstr(97,50);EatInstr(90,50);EatInstr(89,50);EatInstr(88,50);EatInstr(87,50);EatInstr(86,50);EatInstr(85,50);EatInstr(84,50);EatInstr(83,50);EatInstr(82,50);EatInstr(81,50);EatInstr(80,50);EatInstr(79,50);EatInstr(78,50);EatInstr(77,50);EatInstr(76,50);EatInstr(75,50);EatInstr(74,50);EatInstr(73,50);EatInstr(72,50);EatInstr(71,50);EatInstr(70,50);EatInstr(69,50);EatInstr(68,50);EatInstr(67,50);EatInstr(66,50);EatInstr(65,50);ASimpleCont2Instr(264,__binder0,82)]);
(31, [AAction2Instr(__a4,83)]);
(32, [AAction2Instr(__a5,84)]);
(33, [AAction2Instr(__a6,85)]);
(34, [AAction2Instr(__a7,86)]);
(35, [EatInstr(42,88);EatInstr(35,88);RCompleteInstr2(304,nullable_DIGITS);AAction2Instr(__a8,89);RCompleteInstr2(298,nullable_repeat);ASimpleCont2Instr(304,__binder0,87)]);
(36, [AAction2Instr(__a13,94);AAction2Instr(__a12,93);AAction2Instr(__a11,92);AAction2Instr(__a10,91);AAction2Instr(__a9,90)]);
(37, [AAction2Instr(__a14,95)]);
(38, [AAction2Instr(__a15,96)]);
(39, [RCompleteInstr2(302,nullable_bitstring);AAction2Instr(__a16,97)]);
(40, [EatInstr(98,98)]);
(41, [RCompleteInstr2(304,nullable_DIGITS);AAction2Instr(__a8,89)]);
(42, [EatInstr(100,99)]);
(43, [EatInstr(57,54);EatInstr(56,54);EatInstr(55,54);EatInstr(54,54);EatInstr(53,54);EatInstr(52,54);EatInstr(51,54);EatInstr(50,54);EatInstr(49,54);EatInstr(48,54);EatInstr(102,100);EatInstr(101,100);EatInstr(100,100);EatInstr(99,100);EatInstr(98,100);EatInstr(97,100);EatInstr(70,100);EatInstr(69,100);EatInstr(68,100);EatInstr(67,100);EatInstr(66,100);EatInstr(65,100);ASimpleCont2Instr(268,__binder0,100)]);
(44, [RCompleteInstr2(307,nullable_HEXDIGS);AAction2Instr(__a17,101)]);
(45, [EatInstr(120,102)]);
(46, [EatInstr(37,103)]);
(47, [EatInstr(60,105);EatInstr(34,55);ASimpleCont2Instr(269,__binder0,104)]);
(48, [EatInstr(126,106);EatInstr(125,106);EatInstr(124,106);EatInstr(123,106);EatInstr(96,106);EatInstr(95,106);EatInstr(94,106);EatInstr(93,106);EatInstr(92,106);EatInstr(91,106);EatInstr(64,106);EatInstr(63,106);EatInstr(61,106);EatInstr(60,106);EatInstr(59,106);EatInstr(58,106);EatInstr(57,106);EatInstr(56,106);EatInstr(55,106);EatInstr(54,106);EatInstr(53,106);EatInstr(52,106);EatInstr(51,106);EatInstr(50,106);EatInstr(47,106);EatInstr(46,106);EatInstr(45,106);EatInstr(44,106);EatInstr(43,106);EatInstr(42,106);EatInstr(41,106);EatInstr(40,106);EatInstr(39,106);EatInstr(38,106);EatInstr(37,106);EatInstr(36,106);EatInstr(35,106);EatInstr(34,106);EatInstr(33,106);EatInstr(32,106);EatInstr(49,106);EatInstr(48,106);EatInstr(122,106);EatInstr(121,106);EatInstr(120,106);EatInstr(119,106);EatInstr(118,106);EatInstr(117,106);EatInstr(116,106);EatInstr(115,106);EatInstr(114,106);EatInstr(113,106);EatInstr(112,106);EatInstr(111,106);EatInstr(110,106);EatInstr(109,106);EatInstr(108,106);EatInstr(107,106);EatInstr(106,106);EatInstr(105,106);EatInstr(104,106);EatInstr(103,106);EatInstr(102,106);EatInstr(101,106);EatInstr(100,106);EatInstr(99,106);EatInstr(98,106);EatInstr(97,106);EatInstr(90,106);EatInstr(89,106);EatInstr(88,106);EatInstr(87,106);EatInstr(86,106);EatInstr(85,106);EatInstr(84,106);EatInstr(83,106);EatInstr(82,106);EatInstr(81,106);EatInstr(80,106);EatInstr(79,106);EatInstr(78,106);EatInstr(77,106);EatInstr(76,106);EatInstr(75,106);EatInstr(74,106);EatInstr(73,106);EatInstr(72,106);EatInstr(71,106);EatInstr(70,106);EatInstr(69,106);EatInstr(68,106);EatInstr(67,106);EatInstr(66,106);EatInstr(65,106)]);
(49, [EatInstr(60,107)]);
(50, [CompleteInstr(264)]);
(51, [CompleteInstr(265)]);
(52, [CompleteInstr(266)]);
(53, [CompleteInstr(267)]);
(54, [CompleteInstr(268)]);
(55, [CompleteInstr(269)]);
(56, [CompleteInstr(270)]);
(57, [CompleteInstr(271)]);
(58, [CompleteInstr(272)]);
(59, [CompleteInstr(273)]);
(60, [CompleteInstr(274)]);
(61, [CompleteInstr(275)]);
(62, [AAction2Instr(__a18,108)]);
(63, [ALookaheadInstr(false,CfgLA (15,278),66);ACallInstr3(__default_call,15);ASimpleCont2Instr(278,__binder0,63)]);
(64, [CompleteInstr(278)]);
(66, [CompleteInstr(277)]);
(67, [ACallInstr3(__default_call,110);ASimpleCont2Instr(271,__binder0,109);ASimpleCont2Instr(267,__binder0,109)]);
(68, [CompleteInstr(280);ACallInstr3(__default_call,111);ASimpleCont2Instr(275,__binder0,68);ASimpleCont2Instr(274,__binder0,68)]);
(69, [ASimpleCont2Instr(284,__binder1,112);ACallInstr3(__default_call,21)]);
(70, [EatInstr(47,113);CompleteInstr(282)]);
(71, [CompleteInstr(283)]);
(72, [ACallInstr3(__default_call,110);ASimpleCont2Instr(271,__binder0,114);ASimpleCont2Instr(267,__binder0,114)]);
(73, [ACallInstr3(__default_call,24);ASimpleCont2Instr(287,__binder0,115)]);
(74, [AAction2Instr(__a20,118);AContInstr3(285,__g19,__binder2,118);ACallInstr3(__g19,22);ACallInstr3(__default_call,117);ASimpleCont2Instr(287,__binder0,116);ASimpleCont2Instr(283,__binder0,74)]);
(75, [CompleteInstr(287)]);
(76, [ACallInstr3(__default_call,120);ASimpleCont2Instr(275,__binder0,76);ASimpleCont2Instr(274,__binder0,76);ASimpleCont2Instr(271,__binder0,119);ASimpleCont2Instr(267,__binder0,119)]);
(77, [CompleteInstr(289)]);
(78, [ACallInstr3(__default_call,122);ASimpleCont2Instr(291,__binder0,78);ASimpleCont2Instr(269,__binder0,121)]);
(79, [EatInstr(255,80);EatInstr(254,80);EatInstr(253,80);EatInstr(252,80);EatInstr(251,80);EatInstr(250,80);EatInstr(249,80);EatInstr(248,80);EatInstr(247,80);EatInstr(246,80);EatInstr(245,80);EatInstr(244,80);EatInstr(243,80);EatInstr(242,80);EatInstr(241,80);EatInstr(240,80);EatInstr(239,80);EatInstr(238,80);EatInstr(237,80);EatInstr(236,80);EatInstr(235,80);EatInstr(234,80);EatInstr(233,80);EatInstr(232,80);EatInstr(231,80);EatInstr(230,80);EatInstr(229,80);EatInstr(228,80);EatInstr(227,80);EatInstr(226,80);EatInstr(225,80);EatInstr(224,80);EatInstr(223,80);EatInstr(222,80);EatInstr(221,80);EatInstr(220,80);EatInstr(219,80);EatInstr(218,80);EatInstr(217,80);EatInstr(216,80);EatInstr(215,80);EatInstr(214,80);EatInstr(213,80);EatInstr(212,80);EatInstr(211,80);EatInstr(210,80);EatInstr(209,80);EatInstr(208,80);EatInstr(207,80);EatInstr(206,80);EatInstr(205,80);EatInstr(204,80);EatInstr(203,80);EatInstr(202,80);EatInstr(201,80);EatInstr(200,80);EatInstr(199,80);EatInstr(198,80);EatInstr(197,80);EatInstr(196,80);EatInstr(195,80);EatInstr(194,80);EatInstr(193,80);EatInstr(192,80);EatInstr(191,80);EatInstr(190,80);EatInstr(189,80);EatInstr(188,80);EatInstr(187,80);EatInstr(186,80);EatInstr(185,80);EatInstr(184,80);EatInstr(183,80);EatInstr(182,80);EatInstr(181,80);EatInstr(180,80);EatInstr(179,80);EatInstr(178,80);EatInstr(177,80);EatInstr(176,80);EatInstr(175,80);EatInstr(174,80);EatInstr(173,80);EatInstr(172,80);EatInstr(171,80);EatInstr(170,80);EatInstr(169,80);EatInstr(168,80);EatInstr(167,80);EatInstr(166,80);EatInstr(165,80);EatInstr(164,80);EatInstr(163,80);EatInstr(162,80);EatInstr(161,80);EatInstr(160,80);EatInstr(159,80);EatInstr(158,80);EatInstr(157,80);EatInstr(156,80);EatInstr(155,80);EatInstr(154,80);EatInstr(153,80);EatInstr(152,80);EatInstr(151,80);EatInstr(150,80);EatInstr(149,80);EatInstr(148,80);EatInstr(147,80);EatInstr(146,80);EatInstr(145,80);EatInstr(144,80);EatInstr(143,80);EatInstr(142,80);EatInstr(141,80);EatInstr(140,80);EatInstr(139,80);EatInstr(138,80);EatInstr(137,80);EatInstr(136,80);EatInstr(135,80);EatInstr(134,80);EatInstr(133,80);EatInstr(132,80);EatInstr(131,80);EatInstr(130,80);EatInstr(129,80);EatInstr(128,80);EatInstr(0,80);EatInstr(127,80);EatInstr(126,80);EatInstr(125,80);EatInstr(124,80);EatInstr(123,80);EatInstr(96,80);EatInstr(95,80);EatInstr(94,80);EatInstr(93,80);EatInstr(91,80);EatInstr(64,80);EatInstr(63,80);EatInstr(62,80);EatInstr(61,80);EatInstr(60,80);EatInstr(59,80);EatInstr(58,80);EatInstr(57,80);EatInstr(56,80);EatInstr(55,80);EatInstr(54,80);EatInstr(53,80);EatInstr(52,80);EatInstr(51,80);EatInstr(50,80);EatInstr(47,80);EatInstr(46,80);EatInstr(45,80);EatInstr(44,80);EatInstr(43,80);EatInstr(42,80);EatInstr(41,80);EatInstr(40,80);EatInstr(39,80);EatInstr(38,80);EatInstr(37,80);EatInstr(36,80);EatInstr(35,80);EatInstr(33,80);EatInstr(32,80);EatInstr(31,80);EatInstr(30,80);EatInstr(29,80);EatInstr(28,80);EatInstr(27,80);EatInstr(26,80);EatInstr(25,80);EatInstr(24,80);EatInstr(23,80);EatInstr(22,80);EatInstr(21,80);EatInstr(20,80);EatInstr(19,80);EatInstr(18,80);EatInstr(17,80);EatInstr(16,80);EatInstr(15,80);EatInstr(14,80);EatInstr(13,80);EatInstr(12,80);EatInstr(11,80);EatInstr(10,80);EatInstr(9,80);EatInstr(8,80);EatInstr(7,80);EatInstr(6,80);EatInstr(5,80);EatInstr(4,80);EatInstr(3,80);EatInstr(2,80);EatInstr(1,80);EatInstr(49,80);EatInstr(48,80);EatInstr(122,80);EatInstr(121,80);EatInstr(120,80);EatInstr(119,80);EatInstr(118,80);EatInstr(117,80);EatInstr(116,80);EatInstr(115,80);EatInstr(114,80);EatInstr(113,80);EatInstr(112,80);EatInstr(111,80);EatInstr(110,80);EatInstr(109,80);EatInstr(108,80);EatInstr(107,80);EatInstr(106,80);EatInstr(105,80);EatInstr(104,80);EatInstr(103,80);EatInstr(102,80);EatInstr(101,80);EatInstr(100,80);EatInstr(99,80);EatInstr(98,80);EatInstr(97,80);EatInstr(90,80);EatInstr(89,80);EatInstr(88,80);EatInstr(87,80);EatInstr(86,80);EatInstr(85,80);EatInstr(84,80);EatInstr(83,80);EatInstr(82,80);EatInstr(81,80);EatInstr(80,80);EatInstr(79,80);EatInstr(78,80);EatInstr(77,80);EatInstr(76,80);EatInstr(75,80);EatInstr(74,80);EatInstr(73,80);EatInstr(72,80);EatInstr(71,80);EatInstr(70,80);EatInstr(69,80);EatInstr(68,80);EatInstr(67,80);EatInstr(66,80);EatInstr(65,80);ACallInstr3(__default_call,123);ASimpleCont2Instr(289,__binder0,80);ASimpleCont2Instr(269,__binder0,80)]);
(80, [CompleteInstr(291)]);
(81, [CompleteInstr(292)]);
(82, [ALookaheadInstr(false,CfgLA (29,292),124);ACallInstr3(__default_call,29);ASimpleCont2Instr(292,__binder0,82)]);
(83, [AContInstr3(295,__g21,__binder3,125);ACallInstr3(__g21,32)]);
(84, [AContInstr3(296,__g22,__binder4,126);ACallInstr3(__g22,33)]);
(85, [AContInstr3(297,__g23,__binder5,127);ACallInstr3(__g23,34)]);
(86, [AContInstr3(299,__g24,__binder6,129);ACallInstr3(__g24,36);ACallInstr3(__default_call,35);ASimpleCont2Instr(298,__binder0,128)]);
(87, [EatInstr(42,88);EatInstr(35,88);CompleteInstr(298)]);
(88, [CompleteInstr(298);ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,130)]);
(89, [AAction2Instr(__a25,132);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,131)]);
(90, [ACallInstr3(__default_call,30);ASimpleCont2Instr(293,__binder0,133)]);
(91, [AContInstr3(301,__g27,__binder8,133);ACallInstr3(__g27,38);AContInstr3(300,__g26,__binder7,133);ACallInstr3(__g26,37)]);
(92, [ACallInstr3(__default_call,47);ASimpleCont2Instr(310,__binder0,133)]);
(93, [ACallInstr3(__default_call,46);ASimpleCont2Instr(309,__binder0,133)]);
(94, [ACallInstr3(__default_call,49);ASimpleCont2Instr(312,__binder0,133)]);
(95, [EatInstr(40,134)]);
(96, [EatInstr(91,135)]);
(97, [AAction2Instr(__a28,137);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,136)]);
(98, [ACallInstr3(__default_call,39);ASimpleCont2Instr(302,__binder0,138)]);
(99, [ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,139)]);
(100, [CompleteInstr(306)]);
(101, [AAction2Instr(__a29,141);ACallInstr3(__default_call,43);ASimpleCont2Instr(306,__binder0,140)]);
(102, [ACallInstr3(__default_call,44);ASimpleCont2Instr(307,__binder0,142)]);
(103, [ACallInstr3(__default_call,144);ASimpleCont2Instr(308,__binder0,143);ASimpleCont2Instr(305,__binder0,143);ASimpleCont2Instr(303,__binder0,143)]);
(104, [EatInstr(126,104);EatInstr(125,104);EatInstr(124,104);EatInstr(123,104);EatInstr(96,104);EatInstr(95,104);EatInstr(94,104);EatInstr(93,104);EatInstr(92,104);EatInstr(91,104);EatInstr(64,104);EatInstr(63,104);EatInstr(62,104);EatInstr(61,104);EatInstr(60,104);EatInstr(59,104);EatInstr(58,104);EatInstr(57,104);EatInstr(56,104);EatInstr(55,104);EatInstr(54,104);EatInstr(53,104);EatInstr(52,104);EatInstr(51,104);EatInstr(50,104);EatInstr(47,104);EatInstr(46,104);EatInstr(45,104);EatInstr(44,104);EatInstr(43,104);EatInstr(42,104);EatInstr(41,104);EatInstr(40,104);EatInstr(39,104);EatInstr(38,104);EatInstr(37,104);EatInstr(36,104);EatInstr(35,104);EatInstr(33,104);EatInstr(32,104);EatInstr(49,104);EatInstr(48,104);EatInstr(122,104);EatInstr(121,104);EatInstr(120,104);EatInstr(119,104);EatInstr(118,104);EatInstr(117,104);EatInstr(116,104);EatInstr(115,104);EatInstr(114,104);EatInstr(113,104);EatInstr(112,104);EatInstr(111,104);EatInstr(110,104);EatInstr(109,104);EatInstr(108,104);EatInstr(107,104);EatInstr(106,104);EatInstr(105,104);EatInstr(104,104);EatInstr(103,104);EatInstr(102,104);EatInstr(101,104);EatInstr(100,104);EatInstr(99,104);EatInstr(98,104);EatInstr(97,104);EatInstr(90,104);EatInstr(89,104);EatInstr(88,104);EatInstr(87,104);EatInstr(86,104);EatInstr(85,104);EatInstr(84,104);EatInstr(83,104);EatInstr(82,104);EatInstr(81,104);EatInstr(80,104);EatInstr(79,104);EatInstr(78,104);EatInstr(77,104);EatInstr(76,104);EatInstr(75,104);EatInstr(74,104);EatInstr(73,104);EatInstr(72,104);EatInstr(71,104);EatInstr(70,104);EatInstr(69,104);EatInstr(68,104);EatInstr(67,104);EatInstr(66,104);EatInstr(65,104);ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,145)]);
(105, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,146)]);
(106, [CompleteInstr(311)]);
(107, [EatInstr(126,147);EatInstr(125,147);EatInstr(124,147);EatInstr(123,147);EatInstr(96,147);EatInstr(95,147);EatInstr(94,147);EatInstr(93,147);EatInstr(92,147);EatInstr(91,147);EatInstr(64,147);EatInstr(63,147);EatInstr(62,148);EatInstr(61,147);EatInstr(60,147);EatInstr(59,147);EatInstr(58,147);EatInstr(57,147);EatInstr(56,147);EatInstr(55,147);EatInstr(54,147);EatInstr(53,147);EatInstr(52,147);EatInstr(51,147);EatInstr(50,147);EatInstr(47,147);EatInstr(46,147);EatInstr(45,147);EatInstr(44,147);EatInstr(43,147);EatInstr(42,147);EatInstr(41,147);EatInstr(40,147);EatInstr(39,147);EatInstr(38,147);EatInstr(37,147);EatInstr(36,147);EatInstr(35,147);EatInstr(33,147);EatInstr(32,147);EatInstr(49,147);EatInstr(48,147);EatInstr(122,147);EatInstr(121,147);EatInstr(120,147);EatInstr(119,147);EatInstr(118,147);EatInstr(117,147);EatInstr(116,147);EatInstr(115,147);EatInstr(114,147);EatInstr(113,147);EatInstr(112,147);EatInstr(111,147);EatInstr(110,147);EatInstr(109,147);EatInstr(108,147);EatInstr(107,147);EatInstr(106,147);EatInstr(105,147);EatInstr(104,147);EatInstr(103,147);EatInstr(102,147);EatInstr(101,147);EatInstr(100,147);EatInstr(99,147);EatInstr(98,147);EatInstr(97,147);EatInstr(90,147);EatInstr(89,147);EatInstr(88,147);EatInstr(87,147);EatInstr(86,147);EatInstr(85,147);EatInstr(84,147);EatInstr(83,147);EatInstr(82,147);EatInstr(81,147);EatInstr(80,147);EatInstr(79,147);EatInstr(78,147);EatInstr(77,147);EatInstr(76,147);EatInstr(75,147);EatInstr(74,147);EatInstr(73,147);EatInstr(72,147);EatInstr(71,147);EatInstr(70,147);EatInstr(69,147);EatInstr(68,147);EatInstr(67,147);EatInstr(66,147);EatInstr(65,147)]);
(108, [AAction2Instr(__a32,151);AAction2Instr(__a31,150);AAction2Instr(__a30,149)]);
(109, [ACallInstr3(__default_call,14);ASimpleCont2Instr(277,__binder0,152)]);
(110, [EatInstr(13,53);EatInstr(10,57)]);
(111, [EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(47,60);EatInstr(46,60);EatInstr(45,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(32,59);EatInstr(9,56);EatInstr(49,60);EatInstr(48,60);EatInstr(122,60);EatInstr(121,60);EatInstr(120,60);EatInstr(119,60);EatInstr(118,60);EatInstr(117,60);EatInstr(116,60);EatInstr(115,60);EatInstr(114,60);EatInstr(113,60);EatInstr(112,60);EatInstr(111,60);EatInstr(110,60);EatInstr(109,60);EatInstr(108,60);EatInstr(107,60);EatInstr(106,60);EatInstr(105,60);EatInstr(104,60);EatInstr(103,60);EatInstr(102,60);EatInstr(101,60);EatInstr(100,60);EatInstr(99,60);EatInstr(98,60);EatInstr(97,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60);ASimpleCont2Instr(273,__binder0,61);ASimpleCont2Instr(270,__binder0,61)]);
(112, [AAction2Instr(__a33,153)]);
(113, [CompleteInstr(282)]);
(114, [AAction2Instr(__a34,154)]);
(115, [AAction2Instr(__a35,155)]);
(116, [AContInstr3(285,__g19,__binder2,118);ACallInstr3(__g19,22);ACallInstr3(__default_call,117);ASimpleCont2Instr(287,__binder0,116);ASimpleCont2Instr(283,__binder0,116)]);
(117, [EatInstr(59,76);EatInstr(32,59);EatInstr(13,53);EatInstr(10,57);EatInstr(9,56);ASimpleCont2Instr(288,__binder0,75);ASimpleCont2Instr(273,__binder0,71);ASimpleCont2Instr(271,__binder0,75);ASimpleCont2Instr(270,__binder0,71);ASimpleCont2Instr(267,__binder0,75)]);
(118, [ALookaheadInstr(false,CfgLA (20,283),156)]);
(119, [CompleteInstr(288)]);
(120, [EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(47,60);EatInstr(46,60);EatInstr(45,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(32,59);EatInstr(13,53);EatInstr(10,57);EatInstr(9,56);EatInstr(49,60);EatInstr(48,60);EatInstr(122,60);EatInstr(121,60);EatInstr(120,60);EatInstr(119,60);EatInstr(118,60);EatInstr(117,60);EatInstr(116,60);EatInstr(115,60);EatInstr(114,60);EatInstr(113,60);EatInstr(112,60);EatInstr(111,60);EatInstr(110,60);EatInstr(109,60);EatInstr(108,60);EatInstr(107,60);EatInstr(106,60);EatInstr(105,60);EatInstr(104,60);EatInstr(103,60);EatInstr(102,60);EatInstr(101,60);EatInstr(100,60);EatInstr(99,60);EatInstr(98,60);EatInstr(97,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60);ASimpleCont2Instr(273,__binder0,61);ASimpleCont2Instr(270,__binder0,61)]);
(121, [CompleteInstr(290)]);
(122, [EatInstr(255,80);EatInstr(254,80);EatInstr(253,80);EatInstr(252,80);EatInstr(251,80);EatInstr(250,80);EatInstr(249,80);EatInstr(248,80);EatInstr(247,80);EatInstr(246,80);EatInstr(245,80);EatInstr(244,80);EatInstr(243,80);EatInstr(242,80);EatInstr(241,80);EatInstr(240,80);EatInstr(239,80);EatInstr(238,80);EatInstr(237,80);EatInstr(236,80);EatInstr(235,80);EatInstr(234,80);EatInstr(233,80);EatInstr(232,80);EatInstr(231,80);EatInstr(230,80);EatInstr(229,80);EatInstr(228,80);EatInstr(227,80);EatInstr(226,80);EatInstr(225,80);EatInstr(224,80);EatInstr(223,80);EatInstr(222,80);EatInstr(221,80);EatInstr(220,80);EatInstr(219,80);EatInstr(218,80);EatInstr(217,80);EatInstr(216,80);EatInstr(215,80);EatInstr(214,80);EatInstr(213,80);EatInstr(212,80);EatInstr(211,80);EatInstr(210,80);EatInstr(209,80);EatInstr(208,80);EatInstr(207,80);EatInstr(206,80);EatInstr(205,80);EatInstr(204,80);EatInstr(203,80);EatInstr(202,80);EatInstr(201,80);EatInstr(200,80);EatInstr(199,80);EatInstr(198,80);EatInstr(197,80);EatInstr(196,80);EatInstr(195,80);EatInstr(194,80);EatInstr(193,80);EatInstr(192,80);EatInstr(191,80);EatInstr(190,80);EatInstr(189,80);EatInstr(188,80);EatInstr(187,80);EatInstr(186,80);EatInstr(185,80);EatInstr(184,80);EatInstr(183,80);EatInstr(182,80);EatInstr(181,80);EatInstr(180,80);EatInstr(179,80);EatInstr(178,80);EatInstr(177,80);EatInstr(176,80);EatInstr(175,80);EatInstr(174,80);EatInstr(173,80);EatInstr(172,80);EatInstr(171,80);EatInstr(170,80);EatInstr(169,80);EatInstr(168,80);EatInstr(167,80);EatInstr(166,80);EatInstr(165,80);EatInstr(164,80);EatInstr(163,80);EatInstr(162,80);EatInstr(161,80);EatInstr(160,80);EatInstr(159,80);EatInstr(158,80);EatInstr(157,80);EatInstr(156,80);EatInstr(155,80);EatInstr(154,80);EatInstr(153,80);EatInstr(152,80);EatInstr(151,80);EatInstr(150,80);EatInstr(149,80);EatInstr(148,80);EatInstr(147,80);EatInstr(146,80);EatInstr(145,80);EatInstr(144,80);EatInstr(143,80);EatInstr(142,80);EatInstr(141,80);EatInstr(140,80);EatInstr(139,80);EatInstr(138,80);EatInstr(137,80);EatInstr(136,80);EatInstr(135,80);EatInstr(134,80);EatInstr(133,80);EatInstr(132,80);EatInstr(131,80);EatInstr(130,80);EatInstr(129,80);EatInstr(128,80);EatInstr(0,80);EatInstr(127,80);EatInstr(126,80);EatInstr(125,80);EatInstr(124,80);EatInstr(123,80);EatInstr(96,80);EatInstr(95,80);EatInstr(94,80);EatInstr(93,80);EatInstr(92,77);EatInstr(91,80);EatInstr(64,80);EatInstr(63,80);EatInstr(62,80);EatInstr(61,80);EatInstr(60,80);EatInstr(59,80);EatInstr(58,80);EatInstr(57,80);EatInstr(56,80);EatInstr(55,80);EatInstr(54,80);EatInstr(53,80);EatInstr(52,80);EatInstr(51,80);EatInstr(50,80);EatInstr(47,80);EatInstr(46,80);EatInstr(45,80);EatInstr(44,80);EatInstr(43,80);EatInstr(42,80);EatInstr(41,80);EatInstr(40,80);EatInstr(39,80);EatInstr(38,80);EatInstr(37,80);EatInstr(36,80);EatInstr(35,80);EatInstr(34,55);EatInstr(33,80);EatInstr(32,80);EatInstr(31,80);EatInstr(30,80);EatInstr(29,80);EatInstr(28,80);EatInstr(27,80);EatInstr(26,80);EatInstr(25,80);EatInstr(24,80);EatInstr(23,80);EatInstr(22,80);EatInstr(21,80);EatInstr(20,80);EatInstr(19,80);EatInstr(18,80);EatInstr(17,80);EatInstr(16,80);EatInstr(15,80);EatInstr(14,80);EatInstr(13,80);EatInstr(12,80);EatInstr(11,80);EatInstr(10,80);EatInstr(9,80);EatInstr(8,80);EatInstr(7,80);EatInstr(6,80);EatInstr(5,80);EatInstr(4,80);EatInstr(3,80);EatInstr(2,80);EatInstr(1,80);EatInstr(49,80);EatInstr(48,80);EatInstr(122,80);EatInstr(121,80);EatInstr(120,80);EatInstr(119,80);EatInstr(118,80);EatInstr(117,80);EatInstr(116,80);EatInstr(115,80);EatInstr(114,80);EatInstr(113,80);EatInstr(112,80);EatInstr(111,80);EatInstr(110,80);EatInstr(109,80);EatInstr(108,80);EatInstr(107,80);EatInstr(106,80);EatInstr(105,80);EatInstr(104,80);EatInstr(103,80);EatInstr(102,80);EatInstr(101,80);EatInstr(100,80);EatInstr(99,80);EatInstr(98,80);EatInstr(97,80);EatInstr(90,80);EatInstr(89,80);EatInstr(88,80);EatInstr(87,80);EatInstr(86,80);EatInstr(85,80);EatInstr(84,80);EatInstr(83,80);EatInstr(82,80);EatInstr(81,80);EatInstr(80,80);EatInstr(79,80);EatInstr(78,80);EatInstr(77,80);EatInstr(76,80);EatInstr(75,80);EatInstr(74,80);EatInstr(73,80);EatInstr(72,80);EatInstr(71,80);EatInstr(70,80);EatInstr(69,80);EatInstr(68,80);EatInstr(67,80);EatInstr(66,80);EatInstr(65,80);ASimpleCont2Instr(289,__binder0,79)]);
(123, [EatInstr(92,77);EatInstr(34,55)]);
(124, [CompleteInstr(293)]);
(125, [CompleteInstr(294)]);
(126, [AAction2Instr(__a37,158);AAction2Instr(__a36,157)]);
(127, [AAction2Instr(__a38,159)]);
(128, [AContInstr3(299,__g24,__binder6,129);ACallInstr3(__g24,36)]);
(129, [CompleteInstr(297)]);
(130, [CompleteInstr(298)]);
(131, [AAction2Instr(__a39,89)]);
(132, [AWhenInstr3(__p41,__p40,160)]);
(133, [CompleteInstr(299)]);
(134, [AAction2Instr(__a42,161)]);
(135, [AAction2Instr(__a43,162)]);
(136, [AAction2Instr(__a44,97)]);
(137, [AWhenInstr3(__p46,__p45,163)]);
(138, [EatInstr(46,165);EatInstr(45,164);CompleteInstr(303)]);
(139, [EatInstr(46,167);EatInstr(45,166);CompleteInstr(305)]);
(140, [AAction2Instr(__a47,101)]);
(141, [AWhenInstr3(__p49,__p48,168)]);
(142, [EatInstr(46,170);EatInstr(45,169);CompleteInstr(308)]);
(143, [CompleteInstr(309)]);
(144, [EatInstr(120,102);EatInstr(100,99);EatInstr(98,98)]);
(145, [CompleteInstr(310)]);
(146, [EatInstr(62,145)]);
(147, [EatInstr(62,148);ACallInstr3(__default_call,48);ASimpleCont2Instr(311,__binder0,147)]);
(148, [CompleteInstr(312)]);
(149, [ACallInstr3(__default_call,18);ASimpleCont2Instr(281,__binder0,171)]);
(150, [ACallInstr3(__default_call,16);ASimpleCont2Instr(279,__binder0,108)]);
(151, [CompleteInstr(276)]);
(152, [CompleteInstr(279)]);
(153, [ACallInstr3(__default_call,30);ASimpleCont2Instr(293,__binder0,172)]);
(154, [ALookaheadInstr(false,CfgLA (20,283),173);ACallInstr3(__default_call,20);ASimpleCont2Instr(283,__binder0,154)]);
(155, [ALookaheadInstr(false,CfgLA (20,283),174);ACallInstr3(__default_call,20);ASimpleCont2Instr(283,__binder0,155)]);
(156, [CompleteInstr(286)]);
(157, [AContInstr3(286,__g50,__binder9,175);ACallInstr3(__g50,23)]);
(158, [CompleteInstr(295)]);
(159, [AAction2Instr(__a52,177);AAction2Instr(__a51,176)]);
(160, [ALookaheadInstr(false,CfgLA (5,268),178)]);
(161, [AContInstr3(286,__g53,__binder10,179);ACallInstr3(__g53,23)]);
(162, [AContInstr3(286,__g54,__binder11,180);ACallInstr3(__g54,23)]);
(163, [ALookaheadInstr(false,CfgLA (2,265),181)]);
(164, [ACallInstr3(__default_call,39);ASimpleCont2Instr(302,__binder0,182)]);
(165, [ACallInstr3(__default_call,39);ASimpleCont2Instr(302,__binder0,183)]);
(166, [ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,184)]);
(167, [ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,185)]);
(168, [ALookaheadInstr(false,CfgLA (43,306),186)]);
(169, [ACallInstr3(__default_call,44);ASimpleCont2Instr(307,__binder0,187)]);
(170, [ACallInstr3(__default_call,44);ASimpleCont2Instr(307,__binder0,188)]);
(171, [AAction2Instr(__a55,108)]);
(172, [AAction2Instr(__a56,189)]);
(173, [AAction2Instr(__a57,190)]);
(174, [AAction2Instr(__a58,191)]);
(175, [EatInstr(124,192);EatInstr(47,192)]);
(176, [AContInstr3(286,__g59,__binder12,193);ACallInstr3(__g59,23)]);
(177, [CompleteInstr(296)]);
(178, [CompleteInstr(304)]);
(179, [AAction2Instr(__a60,194)]);
(180, [AAction2Instr(__a61,195)]);
(181, [CompleteInstr(302)]);
(182, [CompleteInstr(303)]);
(183, [EatInstr(46,165);CompleteInstr(303)]);
(184, [CompleteInstr(305)]);
(185, [EatInstr(46,167);CompleteInstr(305)]);
(186, [CompleteInstr(307)]);
(187, [CompleteInstr(308)]);
(188, [EatInstr(46,170);CompleteInstr(308)]);
(189, [AContInstr3(286,__g62,__binder13,196);ACallInstr3(__g62,23)]);
(190, [CompleteInstr(284)]);
]

let start_symb = get_symb_action "rfc"

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
      _r_rfc(_n,ykinput)
    )

let visualize = parse
let visualize_file = Yak.Pami.Simple.parse_file visualize
let visualize_string = Yak.Pami.Simple.parse_string visualize

let parse_file = Yak.Pami.Simple.parse_file parse
let parse_string = Yak.Pami.Simple.parse_string parse
;;
Yk_History.memoize := false;;

let extract ch file =
  begin
    outch := ch;
    ignore(parse_file file);
    outch := stdout
  end
