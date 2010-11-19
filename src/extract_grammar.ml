
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
| Yk_x19 of (int)
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
_r_rfc(_n,ykinput) = (ignore (*1000*) (_n()); 
 (let _x31 = (ignore (*1002*) (_n()); 
 (let rec _x34 _x32 = (match _n() with 1003 -> _x32 | _x33 -> _x34(
 (match _x33 with
 | (1011) -> (
 (let _x6 = (ignore (*1012*) (_n()); _n())
  in (ignore (*1013*) (_n()); 
 (let _x5 = (ignore (*1014*) (_n()); _n())
  in (ignore (*1015*) (_n()); 
 (let x = (ignore (*1016*) (_n()); Yak.YkBuf.get_string _x6 _x5 ykinput)
  in (ignore (*1017*) (_n()); 
 (let _x36 = (ignore (*1018*) (_n());  output_string !outch x; output_string !outch "\n";  )
  in ()))
 ))
 ))
 ))
 | _(*1019*) -> (
 (let _x35 = (ignore (*1020*) (_n()); ())
  in ()))
 ))) in _x34(())))
  in ()))
 
 
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

let _x44 =
 (fun _(*pos*) (_,_x37)(*arg of rfc*) -> (_t(fun _(*1002*) pos_ -> let rec _x39 _x38  = _t(function
 | 1003 ->
 (fun pos_ -> Yk_done(ignore(_x38);_wv0))
 | 1004 ->
 (fun pos_ -> let _x40 _x4  = _t(fun _(*1008*) pos_ -> let _x41 _x3  = _t(fun _(*1011*) pos_ -> let _x43 _x42  = _t(fun _(*1014*) pos_ -> Yk_delay(_x39 (ignore((_wv0));_wv0) ,_x3)) in _t(fun _(*1012*) pos_ -> Yk_delay(_x43 ((_wv0)) ,_x4))) in _t(fun _(*1009*) pos_ -> _x41 (pos_) )) in _t(fun _(*1005*) pos_ -> _x40 (pos_) ))
 | _(*1020*) ->
 (fun pos_ -> _x39 (ignore(());_wv0) )) in _x39 (_wv0) ),_x37))
let _x56 =
 (fun _(*pos*) (_,_x45)(*arg of rule*) -> (_t(fun _(*1022*) pos_ -> let _x46 n  = _t(fun _(*1028*) pos_ -> let _x50 _x49  = _t(fun _(*1032*) pos_ -> let _x53 _x52  = _t(function
 | 1036 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1037*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x55) -> Yk_done(ignore(ignore(_x55);_wv0);_wv0) | _ -> failwith "bind-1037"))) in _t(function
 | 1033 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1034*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x54) -> _x53 (_x54)  | _ -> failwith "bind-1034")))) in _t(function
 | 1029 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1030*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x51) -> _x50 (_x51)  | _ -> failwith "bind-1030")))) in _t(fun _(*1023*) pos_ -> let _x47 _x20  = _t(fun _(*1026*) pos_ -> _x46 ((match _x20 with Yk_x19(y) -> y | _ -> failwith "projection")) ) in _t(fun _(*1025*) pos_ -> Yk_bind(function Yk_done(_x48) -> _x47 (_x48)  | _ -> failwith "bind=1025")))),_x45))
let _x61 =
 (fun _(*pos*) (_,_x57)(*arg of rule-indent*) -> (_t(fun _(*1038*) pos_ -> let _x58 _x21  = _t(fun _(*1049*) pos_ -> Yk_done(Yk_x19(_x21))) in _t(fun _(*1040*) pos_ -> let _x59 left  = _t(fun _(*1045*) pos_ -> let _x60 right  = _t(fun _(*1048*) pos_ -> _x58 (right - left) ) in _t(fun _(*1046*) pos_ -> _x60 (pos_) )) in _t(fun _(*1041*) pos_ -> _x59 (pos_) ))),_x57))
let _x69 =
 (fun _(*pos*) -> (function (Yk_done(_x22:_yk_t),_x62) -> (_t(fun _(*1051*) pos_ -> let _x63 _x10  = _t(fun _(*1053*) pos_ -> let _x65 _x64 n = _t(fun _(*1057*) pos_ -> let _x67 left  = _t(fun _(*1062*) pos_ -> let _x68 right  = _t(function
 | 1065 ->
 (fun pos_ -> Yk_when(right - left > n))
 | _(*1066*) ->
 (fun pos_ -> Yk_done(ignore((_wv0));_wv0))) in _t(fun _(*1063*) pos_ -> _x68 (pos_) )) in _t(fun _(*1058*) pos_ -> _x67 (pos_) )) in _t(fun _(*1054*) pos_ -> let _x66 n = _x65 ((_wv0)) n in _t(fun _(*1055*) pos_ -> _x66((match _x10 with (n) -> n))))) in _t(fun _(*1052*) pos_ -> _x63 ((match _x22 with Yk_x19(y) -> y | _ -> failwith "projection")) )),_x62)
| _ -> failwith "indent"))
let _x76 =
 (fun _(*pos*) -> (function (Yk_done(_x23:_yk_t),_x70) -> (_t(fun _(*1068*) pos_ -> let _x71 _x11  = _t(fun _(*1070*) pos_ -> let _x73 _x72 n = _t(function
 | 1076 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | 1077 ->
 (fun pos_ -> Yk_bind(function Yk_done(_x75) -> Yk_done(ignore(ignore(_x75);_wv0);_wv0) | _ -> failwith "bind-1077"))
 | _(*1079*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore(());_wv0);_wv0);_wv0))) in _t(fun _(*1071*) pos_ -> let _x74 n = _x73 ((_wv0)) n in _t(fun _(*1072*) pos_ -> _x74((match _x11 with (n) -> n))))) in _t(fun _(*1069*) pos_ -> _x71 ((match _x23 with Yk_x19(y) -> y | _ -> failwith "projection")) )),_x70)
| _ -> failwith "o"))
let _x83 =
 (fun _(*pos*) -> (function (Yk_done(_x24:_yk_t),_x77) -> (_t(fun _(*1081*) pos_ -> let _x78 _x12  = _t(fun _(*1083*) pos_ -> let _x80 _x79 n = _t(function
 | 1086 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1087*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x82) -> Yk_done(ignore(_x82);_wv0) | _ -> failwith "bind-1087"))) in _t(fun _(*1084*) pos_ -> let _x81 n = _x80 ((_wv0)) n in _t(fun _(*1085*) pos_ -> _x81((match _x12 with (n) -> n))))) in _t(fun _(*1082*) pos_ -> _x78 ((match _x24 with Yk_x19(y) -> y | _ -> failwith "projection")) )),_x77)
| _ -> failwith "elements"))
let _x99 =
 (fun _(*pos*) -> (function (Yk_done(_x25:_yk_t),_x84) -> (_t(fun _(*1089*) pos_ -> let _x85 _x13  = _t(fun _(*1091*) pos_ -> let _x87 _x86 n = _t(fun _(*1094*) pos_ -> let _x90 _x89  = _t(function
 | 1097 ->
 (fun pos_ -> let _x93 _x92  = _t(fun _(*1101*) pos_ -> let _x96 _x95  = _t(function
 | 1104 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1105*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x98) -> Yk_done(ignore(_x98);_wv0) | _ -> failwith "bind-1105"))) in _t(function
 | 1102 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1103*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x97) -> _x96 (_x97)  | _ -> failwith "bind-1103")))) in _t(function
 | 1098 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1099*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x94) -> _x93 (_x94)  | _ -> failwith "bind-1099"))))
 | _(*1107*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(function
 | 1095 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1096*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x91) -> _x90 (_x91)  | _ -> failwith "bind-1096")))) in _t(fun _(*1092*) pos_ -> let _x88 n = _x87 ((_wv0)) n in _t(fun _(*1093*) pos_ -> _x88((match _x13 with (n) -> n))))) in _t(fun _(*1090*) pos_ -> _x85 ((match _x25 with Yk_x19(y) -> y | _ -> failwith "projection")) )),_x84)
| _ -> failwith "alternation"))
let _x114 =
 (fun _(*pos*) -> (function (Yk_done(_x26:_yk_t),_x100) -> (_t(fun _(*1109*) pos_ -> let _x101 _x14  = _t(fun _(*1111*) pos_ -> let _x103 _x102 n = _t(fun _(*1114*) pos_ -> let _x106 _x105  = _t(fun _(*1117*) pos_ -> let rec _x109 _x108  = _t(function
 | 1118 ->
 (fun pos_ -> Yk_done(ignore(_x108);_wv0))
 | _(*1119*) ->
 (fun pos_ -> let _x111 _x110  = _t(function
 | 1122 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1123*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x113) -> _x109 (_x113)  | _ -> failwith "bind-1123"))) in _t(function
 | 1120 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1121*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x112) -> _x111 (_x112)  | _ -> failwith "bind-1121"))))) in _x109 (_wv0) ) in _t(function
 | 1115 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1116*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x107) -> _x106 (_x107)  | _ -> failwith "bind-1116")))) in _t(fun _(*1112*) pos_ -> let _x104 n = _x103 ((_wv0)) n in _t(fun _(*1113*) pos_ -> _x104((match _x14 with (n) -> n))))) in _t(fun _(*1110*) pos_ -> _x101 ((match _x26 with Yk_x19(y) -> y | _ -> failwith "projection")) )),_x100)
| _ -> failwith "concatenation"))
let _x121 =
 (fun _(*pos*) -> (function (Yk_done(_x27:_yk_t),_x115) -> (_t(fun _(*1125*) pos_ -> let _x116 _x15  = _t(fun _(*1127*) pos_ -> let _x118 _x117 n = _t(function
 | 1131 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1132*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x120) -> Yk_done(ignore(_x120);_wv0) | _ -> failwith "bind-1132"))) in _t(fun _(*1128*) pos_ -> let _x119 n = _x118 ((_wv0)) n in _t(fun _(*1129*) pos_ -> _x119((match _x15 with (n) -> n))))) in _t(fun _(*1126*) pos_ -> _x116 ((match _x27 with Yk_x19(y) -> y | _ -> failwith "projection")) )),_x115)
| _ -> failwith "repetition"))
let _x133 =
 (fun _(*pos*) -> (function (Yk_done(_x28:_yk_t),_x122) -> (_t(fun _(*1134*) pos_ -> let _x123 _x16  = _t(fun _(*1136*) pos_ -> let _x125 _x124 n = _t(function
 | 1140 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | 1141 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | 1142 ->
 (fun pos_ -> Yk_bind(function Yk_done(_x132) -> Yk_done(ignore(_x132);_wv0) | _ -> failwith "bind-1142"))
 | 1143 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | 1144 ->
 (fun pos_ -> Yk_bind(function Yk_done(_x131) -> Yk_done(ignore(_x131);_wv0) | _ -> failwith "bind-1144"))
 | _(*1145*) ->
 (fun pos_ -> let _x128 _x127  = _t(function
 | 1148 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1149*) ->
 (fun pos_ -> let _x130 _x129  = _t(function
 | 1152 ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))
 | _(*1154*) ->
 (fun pos_ -> Yk_done(ignore(ignore(());_wv0);_wv0))) in _t(fun _(*1150*) pos_ -> _x130 (()) ))) in _t(fun _(*1146*) pos_ -> _x128 (()) ))) in _t(fun _(*1137*) pos_ -> let _x126 n = _x125 ((_wv0)) n in _t(fun _(*1138*) pos_ -> _x126((match _x16 with (n) -> n))))) in _t(fun _(*1135*) pos_ -> _x123 ((match _x28 with Yk_x19(y) -> y | _ -> failwith "projection")) )),_x122)
| _ -> failwith "element"))
let _x146 =
 (fun _(*pos*) -> (function (Yk_done(_x29:_yk_t),_x134) -> (_t(fun _(*1156*) pos_ -> let _x135 _x17  = _t(fun _(*1158*) pos_ -> let _x137 _x136 n = _t(fun _(*1162*) pos_ -> let _x140 _x139  = _t(fun _(*1165*) pos_ -> let _x143 _x142  = _t(function
 | 1169 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1170*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x145) -> Yk_done(ignore(ignore(_x145);_wv0);_wv0) | _ -> failwith "bind-1170"))) in _t(function
 | 1166 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1167*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x144) -> _x143 (_x144)  | _ -> failwith "bind-1167")))) in _t(function
 | 1163 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1164*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x141) -> _x140 (_x141)  | _ -> failwith "bind-1164")))) in _t(fun _(*1159*) pos_ -> let _x138 n = _x137 ((_wv0)) n in _t(fun _(*1160*) pos_ -> _x138((match _x17 with (n) -> n))))) in _t(fun _(*1157*) pos_ -> _x135 ((match _x29 with Yk_x19(y) -> y | _ -> failwith "projection")) )),_x134)
| _ -> failwith "group"))
let _x159 =
 (fun _(*pos*) -> (function (Yk_done(_x30:_yk_t),_x147) -> (_t(fun _(*1172*) pos_ -> let _x148 _x18  = _t(fun _(*1174*) pos_ -> let _x150 _x149 n = _t(fun _(*1178*) pos_ -> let _x153 _x152  = _t(fun _(*1181*) pos_ -> let _x156 _x155  = _t(function
 | 1185 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1186*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x158) -> Yk_done(ignore(ignore(_x158);_wv0);_wv0) | _ -> failwith "bind-1186"))) in _t(function
 | 1182 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1183*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x157) -> _x156 (_x157)  | _ -> failwith "bind-1183")))) in _t(function
 | 1179 ->
 (fun pos_ -> Yk_done(Yk_x19((n))))
 | _(*1180*) ->
 (fun pos_ -> Yk_bind(function Yk_done(_x154) -> _x153 (_x154)  | _ -> failwith "bind-1180")))) in _t(fun _(*1175*) pos_ -> let _x151 n = _x150 ((_wv0)) n in _t(fun _(*1176*) pos_ -> _x151((match _x18 with (n) -> n))))) in _t(fun _(*1173*) pos_ -> _x148 ((match _x30 with Yk_x19(y) -> y | _ -> failwith "projection")) )),_x147)
| _ -> failwith "option"))
let _x163 =
 (fun _(*pos*) (_,_x160)(*arg of bitstring*) -> (_t(fun _(*1189*) pos_ -> let _x161 _x7  = _t(function
 | 1195 ->
 (fun pos_ -> Yk_when(_x7>=1))
 | _(*1196*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1190*) pos_ -> let rec _x162 _x7  = _t(function
 | 1191 ->
 (fun pos_ -> _x161 (_x7) )
 | _(*1193*) ->
 (fun pos_ -> _x162 (_x7+1) )) in _x162 (0) )),_x160))
let _x167 =
 (fun _(*pos*) (_,_x164)(*arg of DIGITS*) -> (_t(fun _(*1199*) pos_ -> let _x165 _x8  = _t(function
 | 1205 ->
 (fun pos_ -> Yk_when(_x8>=1))
 | _(*1206*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1200*) pos_ -> let rec _x166 _x8  = _t(function
 | 1201 ->
 (fun pos_ -> _x165 (_x8) )
 | _(*1203*) ->
 (fun pos_ -> _x166 (_x8+1) )) in _x166 (0) )),_x164))
let _x171 =
 (fun _(*pos*) (_,_x168)(*arg of HEXDIGS*) -> (_t(fun _(*1209*) pos_ -> let _x169 _x9  = _t(function
 | 1215 ->
 (fun pos_ -> Yk_when(_x9>=1))
 | _(*1216*) ->
 (fun pos_ -> Yk_done(ignore(ignore(ignore((_wv0));_wv0);_wv0);_wv0))) in _t(fun _(*1210*) pos_ -> let rec _x170 _x9  = _t(function
 | 1211 ->
 (fun pos_ -> _x169 (_x9) )
 | _(*1213*) ->
 (fun pos_ -> _x170 (_x9+1) )) in _x170 (0) )),_x168))
let __default_call _ _ = sv0;;
let __default_ret _ v1 _ = v1;;
let __a52 = _d 1211;;
let __a48 = _d 1154;;
let __a29 = _d 1097;;
let __a7 = fun p v -> _d 1114 p (_d 1113 p (_d 1112 p (_d 1111 p (_d 1110 p (_d 1109 p (_x114 p (v)))))));;
let __a42 = _d 1213;;
let __p43 = _dnext 1216;;
let __a30 = _d 1107;;
let __a26 = fun p v -> _d 1058 p (_d 1057 p (v));;
let __p41 = _dwhen 1195;;
let __a68 = fun p v -> _d_and_push 1020 p (_p 1019 p (v));;
let __a59 = fun p v -> _p 1018 p (_p 1017 p (_p 1016 p (_p 1015 p (_ddelay 1014 p (_p 1013 p (_ddelay 1012 p (_d_and_push 1011 p (_d 1009 p (_d 1008 p (v))))))))));;
let __g56 = _darg 1120;;
let __p34 = _dwhen 1205;;
let __g64 = _darg 1122;;
let __a58 = _d 1181;;
let __a24 = _d 1026;;
let __a1 = fun p v -> _d 1023 p (_d 1022 p (_x56 p (v)));;
let __p62 = _dwhen 1065;;
let __a53 = _d 1028;;
let __a11 = fun p v -> _d 1160 p (_d 1159 p (_d 1158 p (_d 1157 p (_d 1156 p (_x146 p (v))))));;
let __a28 = _d 1079;;
let __a67 = fun p v -> _d 1005 p (_d 1004 p (v));;
let __g50 = _darg 1179;;
let __a14 = fun p v -> _d 1210 p (_d 1209 p (_x171 p (v)));;
let __g70 = _darg 1102;;
let __a37 = _d 1162;;
let __g16 = _darg 1095;;
let __g76 = _darg 1104;;
let __a57 = _d 1165;;
let __a13 = fun p v -> _d 1190 p (_d 1189 p (_x163 p (v)));;
let __g45 = _darg 1098;;
let __a31 = _d 1117;;
let __a25 = fun p v -> _d 1041 p (_d 1040 p (v));;
let __a8 = fun p v -> _d 1129 p (_d 1128 p (_d 1127 p (_d 1126 p (_d 1125 p (_x121 p (v))))));;
let __a75 = _d 1032;;
let __a72 = _d 1118;;
let __a71 = _d 1119;;
let __a19 = _d 1140;;
let __a3 = fun p v -> _d 1055 p (_d 1054 p (_d 1053 p (_d 1052 p (_d 1051 p (_x69 p (v))))));;
let __a9 = fun p v -> _d 1200 p (_d 1199 p (_x167 p (v)));;
let __a5 = fun p v -> _d 1085 p (_d 1084 p (_d 1083 p (_d 1082 p (_d 1081 p (_x83 p (v))))));;
let __p44 = _dwhen 1215;;
let __g18 = _darg 1131;;
let __a2 = fun p v -> _d 1038 p (_x61 p (v));;
let __a51 = _d 1191;;
let __g66 = _darg 1182;;
let __g27 = _darg 1076;;
let __a4 = fun p v -> _d 1072 p (_d 1071 p (_d 1070 p (_d 1069 p (_d 1068 p (_x76 p (v))))));;
let __a46 = _d 1201;;
let __a39 = _d 1193;;
let __p40 = _dnext 1196;;
let __a12 = fun p v -> _d 1176 p (_d 1175 p (_d 1174 p (_d 1173 p (_d 1172 p (_x159 p (v))))));;
let __g60 = _darg 1029;;
let __g74 = _darg 1185;;
let __p33 = _dnext 1206;;
let __a32 = _d 1203;;
let __a23 = _d_and_push 1002;;
let __a6 = fun p v -> _d 1094 p (_d 1093 p (_d 1092 p (_d 1091 p (_d 1090 p (_d 1089 p (_x99 p (v)))))));;
let __a69 = _d_and_push 1003;;
let __a35 = _d 1148;;
let __p61 = _dnext 1066;;
let __a55 = fun p v -> _d 1063 p (_d 1062 p (v));;
let __a36 = fun p v -> _d 1150 p (_d 1149 p (v));;
let __a22 = fun p v -> _d 1146 p (_d 1145 p (v));;
let __a0 = fun p v -> _p 1000 p (_x44 p (v));;
let __g49 = _darg 1163;;
let __a10 = fun p v -> _d 1138 p (_d 1137 p (_d 1136 p (_d 1135 p (_d 1134 p (_x133 p (v))))));;
let __g17 = _darg 1115;;
let __g65 = _darg 1166;;
let __g77 = _darg 1033;;
let __a38 = _d 1178;;
let __g73 = _darg 1169;;
let __a63 = _d 1101;;
let __a54 = fun p v -> _d 1049 p (_d 1048 p (_d 1046 p (_d 1045 p (v))));;
let __g20 = _darg 1141;;
let __g78 = _darg 1036;;
let __a47 = _d 1152;;
let __g21 = _darg 1143;;
let __g15 = _darg 1086;;
let __binder0 = __default_ret;;
let __binder1 = _dret 1025;;
let __binder2 = _dret 1087;;
let __binder3 = _dret 1096;;
let __binder4 = _dret 1116;;
let __binder5 = _dret 1132;;
let __binder6 = _dret 1142;;
let __binder7 = _dret 1144;;
let __binder8 = _dret 1077;;
let __binder9 = _dret 1099;;
let __binder10 = _dret 1164;;
let __binder11 = _dret 1180;;
let __binder12 = _dret 1121;;
let __binder13 = _dret 1030;;
let __binder14 = _dret 1123;;
let __binder15 = _dret 1167;;
let __binder16 = _dret 1183;;
let __binder17 = _dret 1103;;
let __binder18 = _dret 1170;;
let __binder19 = _dret 1186;;
let __binder20 = _dret 1105;;
let __binder21 = _dret 1034;;
let __binder22 = _dret 1037;;
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

and nullable_bitstring __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1195 and n = _dnext 1196 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (fun _x1_ _x2_ _x3_ -> ((((Pred.andc (Pred.full_lookaheadc false 265 2) (fun _x4_ _x5_ _x6_ -> (Some _x6_))) _x1_) _x2_) _x3_))) __lookahead) _p0_) (((_d 1191) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1190 p (_d 1189 p (_x163 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

and nullable_BIT __lookahead _p0_ _x0_ = None

and nullable_num_val __lookahead _p0_ _x0_ = None

and nullable_repetition __lookahead _p0_ _x0_ = None

and nullable_SP __lookahead _p0_ _x0_ = None

and nullable_ALPHA __lookahead _p0_ _x0_ = None

and nullable_defined_as __lookahead _p0_ _x0_ = None

and nullable_HTAB __lookahead _p0_ _x0_ = None

and nullable_string __lookahead _p0_ _x0_ = None

and nullable_o __lookahead _p0_ _x0_ = ((((Pred.andc (fun _x1_ _x2_ _x3_ -> (Some (((_d 1079) ((Yak.YkBuf.get_offset) _x2_)) _x3_))) (Pred.andc (Pred.full_lookaheadc false 283 20) (fun _x1_ _x2_ _x3_ -> (Some _x3_)))) __lookahead) _p0_) (((fun p v -> _d 1072 p (_d 1071 p (_d 1070 p (_d 1069 p (_d 1068 p (_x76 p (v))))))) ((Yak.YkBuf.get_offset) _p0_)) _x0_))

and nullable_prose_val __lookahead _p0_ _x0_ = None

and nullable_indent __lookahead _p0_ _x0_ = None

and nullable_DIGITS = let __tbl = SV_hashtbl.create 11 in
fun __lookahead _p0_ _x0_ -> 
let __p1 = Yak.YkBuf.get_offset _p0_ in
try
let (r, __p2)  = SV_hashtbl.find __tbl _x0_ in
if __p1 = __p2 then r else
let x = ((((Pred.andc (let p = _dwhen 1205 and n = _dnext 1206 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (fun _x1_ _x2_ _x3_ -> ((((Pred.andc (Pred.full_lookaheadc false 268 5) (fun _x4_ _x5_ _x6_ -> (Some _x6_))) _x1_) _x2_) _x3_))) __lookahead) _p0_) (((_d 1201) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1200 p (_d 1199 p (_x167 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_))) in SV_hashtbl.replace __tbl _x0_ (x, __p1); x
with Not_found ->
  let x = ((((Pred.andc (let p = _dwhen 1205 and n = _dnext 1206 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (fun _x1_ _x2_ _x3_ -> ((((Pred.andc (Pred.full_lookaheadc false 268 5) (fun _x4_ _x5_ _x6_ -> (Some _x6_))) _x1_) _x2_) _x3_))) __lookahead) _p0_) (((_d 1201) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1200 p (_d 1199 p (_x167 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_))) in SV_hashtbl.add __tbl _x0_ (x, __p1); x

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

and nullable_HEXDIGS __lookahead _p0_ _x0_ = ((((Pred.andc (let p = _dwhen 1215 and n = _dnext 1216 in fun _ ykb v -> let pos = Yak.YkBuf.get_offset ykb in if p pos v then Some(n pos v) else None) (fun _x1_ _x2_ _x3_ -> ((((Pred.andc (Pred.full_lookaheadc false 306 43) (fun _x4_ _x5_ _x6_ -> (Some _x6_))) _x1_) _x2_) _x3_))) __lookahead) _p0_) (((_d 1211) ((Yak.YkBuf.get_offset) _p0_)) (((fun p v -> _d 1210 p (_d 1209 p (_x171 p (v)))) ((Yak.YkBuf.get_offset) _p0_)) _x0_)))

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
      | Some v2 -> Some (f_ret p v v2)) (fun _x1_ _x2_ _x3_ -> (Some (((_d_and_push 1003) ((Yak.YkBuf.get_offset) _x2_)) (((_d_and_push 1002) ((Yak.YkBuf.get_offset) _x2_)) _x3_))))) __lookahead) _p0_) (((fun p v -> _p 1000 p (_x44 p (v))) ((Yak.YkBuf.get_offset) _p0_)) _x0_))

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
(191, [AContInstr3(297,__g64,__binder14,199);ACallInstr3(__g64,34)]);
(0, [ASimpleCont2Instr(312,__binder0,49);ASimpleCont2Instr(311,__binder0,48);ASimpleCont2Instr(310,__binder0,47);ASimpleCont2Instr(309,__binder0,46);ASimpleCont2Instr(308,__binder0,45);ASimpleCont2Instr(307,__binder0,44);ASimpleCont2Instr(306,__binder0,43);ASimpleCont2Instr(305,__binder0,42);ASimpleCont2Instr(304,__binder0,41);ASimpleCont2Instr(303,__binder0,40);ASimpleCont2Instr(302,__binder0,39);ASimpleCont2Instr(301,__binder0,38);ASimpleCont2Instr(300,__binder0,37);ASimpleCont2Instr(299,__binder0,36);ASimpleCont2Instr(298,__binder0,35);ASimpleCont2Instr(297,__binder0,34);ASimpleCont2Instr(296,__binder0,33);ASimpleCont2Instr(295,__binder0,32);ASimpleCont2Instr(294,__binder0,31);ASimpleCont2Instr(293,__binder0,30);ASimpleCont2Instr(292,__binder0,29);ASimpleCont2Instr(291,__binder0,28);ASimpleCont2Instr(290,__binder0,27);ASimpleCont2Instr(289,__binder0,26);ASimpleCont2Instr(288,__binder0,25);ASimpleCont2Instr(287,__binder0,24);ASimpleCont2Instr(286,__binder0,23);ASimpleCont2Instr(285,__binder0,22);ASimpleCont2Instr(284,__binder0,21);ASimpleCont2Instr(283,__binder0,20);ASimpleCont2Instr(282,__binder0,19);ASimpleCont2Instr(281,__binder0,18);ASimpleCont2Instr(280,__binder0,17);ASimpleCont2Instr(279,__binder0,16);ASimpleCont2Instr(278,__binder0,15);ASimpleCont2Instr(277,__binder0,14);ASimpleCont2Instr(276,__binder0,13);ASimpleCont2Instr(275,__binder0,12);ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(192, [CompleteInstr(299)]);
(1, [EatInstr(122,50);EatInstr(121,50);EatInstr(120,50);EatInstr(119,50);EatInstr(118,50);EatInstr(117,50);EatInstr(116,50);EatInstr(115,50);EatInstr(114,50);EatInstr(113,50);EatInstr(112,50);EatInstr(111,50);EatInstr(110,50);EatInstr(109,50);EatInstr(108,50);EatInstr(107,50);EatInstr(106,50);EatInstr(105,50);EatInstr(104,50);EatInstr(103,50);EatInstr(102,50);EatInstr(101,50);EatInstr(100,50);EatInstr(99,50);EatInstr(98,50);EatInstr(97,50);EatInstr(90,50);EatInstr(89,50);EatInstr(88,50);EatInstr(87,50);EatInstr(86,50);EatInstr(85,50);EatInstr(84,50);EatInstr(83,50);EatInstr(82,50);EatInstr(81,50);EatInstr(80,50);EatInstr(79,50);EatInstr(78,50);EatInstr(77,50);EatInstr(76,50);EatInstr(75,50);EatInstr(74,50);EatInstr(73,50);EatInstr(72,50);EatInstr(71,50);EatInstr(70,50);EatInstr(69,50);EatInstr(68,50);EatInstr(67,50);EatInstr(66,50);EatInstr(65,50)]);
(193, [AContInstr3(295,__g65,__binder15,200);ACallInstr3(__g65,32)]);
(2, [EatInstr(49,51);EatInstr(48,51)]);
(194, [AContInstr3(295,__g66,__binder16,201);ACallInstr3(__g66,32)]);
(3, [EatInstr(127,52);EatInstr(126,52);EatInstr(125,52);EatInstr(124,52);EatInstr(123,52);EatInstr(96,52);EatInstr(95,52);EatInstr(94,52);EatInstr(93,52);EatInstr(92,52);EatInstr(91,52);EatInstr(64,52);EatInstr(63,52);EatInstr(62,52);EatInstr(61,52);EatInstr(60,52);EatInstr(59,52);EatInstr(58,52);EatInstr(57,52);EatInstr(56,52);EatInstr(55,52);EatInstr(54,52);EatInstr(53,52);EatInstr(52,52);EatInstr(51,52);EatInstr(50,52);EatInstr(47,52);EatInstr(46,52);EatInstr(45,52);EatInstr(44,52);EatInstr(43,52);EatInstr(42,52);EatInstr(41,52);EatInstr(40,52);EatInstr(39,52);EatInstr(38,52);EatInstr(37,52);EatInstr(36,52);EatInstr(35,52);EatInstr(34,52);EatInstr(33,52);EatInstr(32,52);EatInstr(31,52);EatInstr(30,52);EatInstr(29,52);EatInstr(28,52);EatInstr(27,52);EatInstr(26,52);EatInstr(25,52);EatInstr(24,52);EatInstr(23,52);EatInstr(22,52);EatInstr(21,52);EatInstr(20,52);EatInstr(19,52);EatInstr(18,52);EatInstr(17,52);EatInstr(16,52);EatInstr(15,52);EatInstr(14,52);EatInstr(13,52);EatInstr(12,52);EatInstr(11,52);EatInstr(10,52);EatInstr(9,52);EatInstr(8,52);EatInstr(7,52);EatInstr(6,52);EatInstr(5,52);EatInstr(4,52);EatInstr(3,52);EatInstr(2,52);EatInstr(1,52);EatInstr(49,52);EatInstr(48,52);EatInstr(122,52);EatInstr(121,52);EatInstr(120,52);EatInstr(119,52);EatInstr(118,52);EatInstr(117,52);EatInstr(116,52);EatInstr(115,52);EatInstr(114,52);EatInstr(113,52);EatInstr(112,52);EatInstr(111,52);EatInstr(110,52);EatInstr(109,52);EatInstr(108,52);EatInstr(107,52);EatInstr(106,52);EatInstr(105,52);EatInstr(104,52);EatInstr(103,52);EatInstr(102,52);EatInstr(101,52);EatInstr(100,52);EatInstr(99,52);EatInstr(98,52);EatInstr(97,52);EatInstr(90,52);EatInstr(89,52);EatInstr(88,52);EatInstr(87,52);EatInstr(86,52);EatInstr(85,52);EatInstr(84,52);EatInstr(83,52);EatInstr(82,52);EatInstr(81,52);EatInstr(80,52);EatInstr(79,52);EatInstr(78,52);EatInstr(77,52);EatInstr(76,52);EatInstr(75,52);EatInstr(74,52);EatInstr(73,52);EatInstr(72,52);EatInstr(71,52);EatInstr(70,52);EatInstr(69,52);EatInstr(68,52);EatInstr(67,52);EatInstr(66,52);EatInstr(65,52)]);
(195, [AAction2Instr(__a69,164);AAction2Instr(__a68,163);AAction2Instr(__a67,162)]);
(4, [EatInstr(13,53)]);
(196, [ACallInstr3(__default_call,19);ASimpleCont2Instr(282,__binder0,202)]);
(5, [EatInstr(57,54);EatInstr(56,54);EatInstr(55,54);EatInstr(54,54);EatInstr(53,54);EatInstr(52,54);EatInstr(51,54);EatInstr(50,54);EatInstr(49,54);EatInstr(48,54)]);
(197, [CompleteInstr(285)]);
(6, [EatInstr(34,55)]);
(198, [AContInstr3(286,__g70,__binder17,203);ACallInstr3(__g70,23)]);
(7, [EatInstr(9,56)]);
(199, [AAction2Instr(__a72,172);AAction2Instr(__a71,171)]);
(8, [EatInstr(10,57)]);
(200, [AContInstr3(286,__g73,__binder18,204);ACallInstr3(__g73,23)]);
(9, [EatInstr(255,58);EatInstr(254,58);EatInstr(253,58);EatInstr(252,58);EatInstr(251,58);EatInstr(250,58);EatInstr(249,58);EatInstr(248,58);EatInstr(247,58);EatInstr(246,58);EatInstr(245,58);EatInstr(244,58);EatInstr(243,58);EatInstr(242,58);EatInstr(241,58);EatInstr(240,58);EatInstr(239,58);EatInstr(238,58);EatInstr(237,58);EatInstr(236,58);EatInstr(235,58);EatInstr(234,58);EatInstr(233,58);EatInstr(232,58);EatInstr(231,58);EatInstr(230,58);EatInstr(229,58);EatInstr(228,58);EatInstr(227,58);EatInstr(226,58);EatInstr(225,58);EatInstr(224,58);EatInstr(223,58);EatInstr(222,58);EatInstr(221,58);EatInstr(220,58);EatInstr(219,58);EatInstr(218,58);EatInstr(217,58);EatInstr(216,58);EatInstr(215,58);EatInstr(214,58);EatInstr(213,58);EatInstr(212,58);EatInstr(211,58);EatInstr(210,58);EatInstr(209,58);EatInstr(208,58);EatInstr(207,58);EatInstr(206,58);EatInstr(205,58);EatInstr(204,58);EatInstr(203,58);EatInstr(202,58);EatInstr(201,58);EatInstr(200,58);EatInstr(199,58);EatInstr(198,58);EatInstr(197,58);EatInstr(196,58);EatInstr(195,58);EatInstr(194,58);EatInstr(193,58);EatInstr(192,58);EatInstr(191,58);EatInstr(190,58);EatInstr(189,58);EatInstr(188,58);EatInstr(187,58);EatInstr(186,58);EatInstr(185,58);EatInstr(184,58);EatInstr(183,58);EatInstr(182,58);EatInstr(181,58);EatInstr(180,58);EatInstr(179,58);EatInstr(178,58);EatInstr(177,58);EatInstr(176,58);EatInstr(175,58);EatInstr(174,58);EatInstr(173,58);EatInstr(172,58);EatInstr(171,58);EatInstr(170,58);EatInstr(169,58);EatInstr(168,58);EatInstr(167,58);EatInstr(166,58);EatInstr(165,58);EatInstr(164,58);EatInstr(163,58);EatInstr(162,58);EatInstr(161,58);EatInstr(160,58);EatInstr(159,58);EatInstr(158,58);EatInstr(157,58);EatInstr(156,58);EatInstr(155,58);EatInstr(154,58);EatInstr(153,58);EatInstr(152,58);EatInstr(151,58);EatInstr(150,58);EatInstr(149,58);EatInstr(148,58);EatInstr(147,58);EatInstr(146,58);EatInstr(145,58);EatInstr(144,58);EatInstr(143,58);EatInstr(142,58);EatInstr(141,58);EatInstr(140,58);EatInstr(139,58);EatInstr(138,58);EatInstr(137,58);EatInstr(136,58);EatInstr(135,58);EatInstr(134,58);EatInstr(133,58);EatInstr(132,58);EatInstr(131,58);EatInstr(130,58);EatInstr(129,58);EatInstr(128,58);EatInstr(0,58);EatInstr(127,58);EatInstr(126,58);EatInstr(125,58);EatInstr(124,58);EatInstr(123,58);EatInstr(96,58);EatInstr(95,58);EatInstr(94,58);EatInstr(93,58);EatInstr(92,58);EatInstr(91,58);EatInstr(64,58);EatInstr(63,58);EatInstr(62,58);EatInstr(61,58);EatInstr(60,58);EatInstr(59,58);EatInstr(58,58);EatInstr(57,58);EatInstr(56,58);EatInstr(55,58);EatInstr(54,58);EatInstr(53,58);EatInstr(52,58);EatInstr(51,58);EatInstr(50,58);EatInstr(47,58);EatInstr(46,58);EatInstr(45,58);EatInstr(44,58);EatInstr(43,58);EatInstr(42,58);EatInstr(41,58);EatInstr(40,58);EatInstr(39,58);EatInstr(38,58);EatInstr(37,58);EatInstr(36,58);EatInstr(35,58);EatInstr(34,58);EatInstr(33,58);EatInstr(32,58);EatInstr(31,58);EatInstr(30,58);EatInstr(29,58);EatInstr(28,58);EatInstr(27,58);EatInstr(26,58);EatInstr(25,58);EatInstr(24,58);EatInstr(23,58);EatInstr(22,58);EatInstr(21,58);EatInstr(20,58);EatInstr(19,58);EatInstr(18,58);EatInstr(17,58);EatInstr(16,58);EatInstr(15,58);EatInstr(14,58);EatInstr(13,58);EatInstr(12,58);EatInstr(11,58);EatInstr(10,58);EatInstr(9,58);EatInstr(8,58);EatInstr(7,58);EatInstr(6,58);EatInstr(5,58);EatInstr(4,58);EatInstr(3,58);EatInstr(2,58);EatInstr(1,58);EatInstr(49,58);EatInstr(48,58);EatInstr(122,58);EatInstr(121,58);EatInstr(120,58);EatInstr(119,58);EatInstr(118,58);EatInstr(117,58);EatInstr(116,58);EatInstr(115,58);EatInstr(114,58);EatInstr(113,58);EatInstr(112,58);EatInstr(111,58);EatInstr(110,58);EatInstr(109,58);EatInstr(108,58);EatInstr(107,58);EatInstr(106,58);EatInstr(105,58);EatInstr(104,58);EatInstr(103,58);EatInstr(102,58);EatInstr(101,58);EatInstr(100,58);EatInstr(99,58);EatInstr(98,58);EatInstr(97,58);EatInstr(90,58);EatInstr(89,58);EatInstr(88,58);EatInstr(87,58);EatInstr(86,58);EatInstr(85,58);EatInstr(84,58);EatInstr(83,58);EatInstr(82,58);EatInstr(81,58);EatInstr(80,58);EatInstr(79,58);EatInstr(78,58);EatInstr(77,58);EatInstr(76,58);EatInstr(75,58);EatInstr(74,58);EatInstr(73,58);EatInstr(72,58);EatInstr(71,58);EatInstr(70,58);EatInstr(69,58);EatInstr(68,58);EatInstr(67,58);EatInstr(66,58);EatInstr(65,58)]);
(201, [AContInstr3(286,__g74,__binder19,205);ACallInstr3(__g74,23)]);
(10, [EatInstr(32,59)]);
(202, [AAction2Instr(__a75,206)]);
(11, [EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(47,60);EatInstr(46,60);EatInstr(45,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(49,60);EatInstr(48,60);EatInstr(122,60);EatInstr(121,60);EatInstr(120,60);EatInstr(119,60);EatInstr(118,60);EatInstr(117,60);EatInstr(116,60);EatInstr(115,60);EatInstr(114,60);EatInstr(113,60);EatInstr(112,60);EatInstr(111,60);EatInstr(110,60);EatInstr(109,60);EatInstr(108,60);EatInstr(107,60);EatInstr(106,60);EatInstr(105,60);EatInstr(104,60);EatInstr(103,60);EatInstr(102,60);EatInstr(101,60);EatInstr(100,60);EatInstr(99,60);EatInstr(98,60);EatInstr(97,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60)]);
(203, [AContInstr3(295,__g76,__binder20,207);ACallInstr3(__g76,32)]);
(12, [EatInstr(32,59);EatInstr(9,56);ASimpleCont2Instr(273,__binder0,61);ASimpleCont2Instr(270,__binder0,61)]);
(204, [EatInstr(41,208)]);
(13, [RCompleteInstr2(276,nullable_rfc);AAction2Instr(__a0,62)]);
(205, [EatInstr(93,209)]);
(14, [EatInstr(127,63);EatInstr(126,63);EatInstr(125,63);EatInstr(124,63);EatInstr(123,63);EatInstr(96,63);EatInstr(95,63);EatInstr(94,63);EatInstr(93,63);EatInstr(92,63);EatInstr(91,63);EatInstr(64,63);EatInstr(63,63);EatInstr(62,63);EatInstr(61,63);EatInstr(60,63);EatInstr(59,63);EatInstr(58,63);EatInstr(57,63);EatInstr(56,63);EatInstr(55,63);EatInstr(54,63);EatInstr(53,63);EatInstr(52,63);EatInstr(51,63);EatInstr(50,63);EatInstr(47,63);EatInstr(46,63);EatInstr(45,63);EatInstr(44,63);EatInstr(43,63);EatInstr(42,63);EatInstr(41,63);EatInstr(40,63);EatInstr(39,63);EatInstr(38,63);EatInstr(37,63);EatInstr(36,63);EatInstr(35,63);EatInstr(34,63);EatInstr(33,63);EatInstr(32,63);EatInstr(31,63);EatInstr(30,63);EatInstr(29,63);EatInstr(28,63);EatInstr(27,63);EatInstr(26,63);EatInstr(25,63);EatInstr(24,63);EatInstr(23,63);EatInstr(22,63);EatInstr(21,63);EatInstr(20,63);EatInstr(19,63);EatInstr(18,63);EatInstr(17,63);EatInstr(16,63);EatInstr(15,63);EatInstr(14,63);EatInstr(12,63);EatInstr(11,63);EatInstr(9,63);EatInstr(8,63);EatInstr(7,63);EatInstr(6,63);EatInstr(5,63);EatInstr(4,63);EatInstr(3,63);EatInstr(2,63);EatInstr(1,63);EatInstr(49,63);EatInstr(48,63);EatInstr(122,63);EatInstr(121,63);EatInstr(120,63);EatInstr(119,63);EatInstr(118,63);EatInstr(117,63);EatInstr(116,63);EatInstr(115,63);EatInstr(114,63);EatInstr(113,63);EatInstr(112,63);EatInstr(111,63);EatInstr(110,63);EatInstr(109,63);EatInstr(108,63);EatInstr(107,63);EatInstr(106,63);EatInstr(105,63);EatInstr(104,63);EatInstr(103,63);EatInstr(102,63);EatInstr(101,63);EatInstr(100,63);EatInstr(99,63);EatInstr(98,63);EatInstr(97,63);EatInstr(90,63);EatInstr(89,63);EatInstr(88,63);EatInstr(87,63);EatInstr(86,63);EatInstr(85,63);EatInstr(84,63);EatInstr(83,63);EatInstr(82,63);EatInstr(81,63);EatInstr(80,63);EatInstr(79,63);EatInstr(78,63);EatInstr(77,63);EatInstr(76,63);EatInstr(75,63);EatInstr(74,63);EatInstr(73,63);EatInstr(72,63);EatInstr(71,63);EatInstr(70,63);EatInstr(69,63);EatInstr(68,63);EatInstr(67,63);EatInstr(66,63);EatInstr(65,63);ALookaheadInstr(false,CfgLA (15,278),93);RCompleteInstr2(277,nullable_line);ASimpleCont2Instr(278,__binder0,92)]);
(206, [AContInstr3(286,__g77,__binder21,210);ACallInstr3(__g77,23)]);
(15, [EatInstr(127,63);EatInstr(126,63);EatInstr(125,63);EatInstr(124,63);EatInstr(123,63);EatInstr(96,63);EatInstr(95,63);EatInstr(94,63);EatInstr(93,63);EatInstr(92,63);EatInstr(91,63);EatInstr(64,63);EatInstr(63,63);EatInstr(62,63);EatInstr(61,63);EatInstr(60,63);EatInstr(59,63);EatInstr(58,63);EatInstr(57,63);EatInstr(56,63);EatInstr(55,63);EatInstr(54,63);EatInstr(53,63);EatInstr(52,63);EatInstr(51,63);EatInstr(50,63);EatInstr(47,63);EatInstr(46,63);EatInstr(45,63);EatInstr(44,63);EatInstr(43,63);EatInstr(42,63);EatInstr(41,63);EatInstr(40,63);EatInstr(39,63);EatInstr(38,63);EatInstr(37,63);EatInstr(36,63);EatInstr(35,63);EatInstr(34,63);EatInstr(33,63);EatInstr(32,63);EatInstr(31,63);EatInstr(30,63);EatInstr(29,63);EatInstr(28,63);EatInstr(27,63);EatInstr(26,63);EatInstr(25,63);EatInstr(24,63);EatInstr(23,63);EatInstr(22,63);EatInstr(21,63);EatInstr(20,63);EatInstr(19,63);EatInstr(18,63);EatInstr(17,63);EatInstr(16,63);EatInstr(15,63);EatInstr(14,63);EatInstr(12,63);EatInstr(11,63);EatInstr(9,63);EatInstr(8,63);EatInstr(7,63);EatInstr(6,63);EatInstr(5,63);EatInstr(4,63);EatInstr(3,63);EatInstr(2,63);EatInstr(1,63);EatInstr(49,63);EatInstr(48,63);EatInstr(122,63);EatInstr(121,63);EatInstr(120,63);EatInstr(119,63);EatInstr(118,63);EatInstr(117,63);EatInstr(116,63);EatInstr(115,63);EatInstr(114,63);EatInstr(113,63);EatInstr(112,63);EatInstr(111,63);EatInstr(110,63);EatInstr(109,63);EatInstr(108,63);EatInstr(107,63);EatInstr(106,63);EatInstr(105,63);EatInstr(104,63);EatInstr(103,63);EatInstr(102,63);EatInstr(101,63);EatInstr(100,63);EatInstr(99,63);EatInstr(98,63);EatInstr(97,63);EatInstr(90,63);EatInstr(89,63);EatInstr(88,63);EatInstr(87,63);EatInstr(86,63);EatInstr(85,63);EatInstr(84,63);EatInstr(83,63);EatInstr(82,63);EatInstr(81,63);EatInstr(80,63);EatInstr(79,63);EatInstr(78,63);EatInstr(77,63);EatInstr(76,63);EatInstr(75,63);EatInstr(74,63);EatInstr(73,63);EatInstr(72,63);EatInstr(71,63);EatInstr(70,63);EatInstr(69,63);EatInstr(68,63);EatInstr(67,63);EatInstr(66,63);EatInstr(65,63)]);
(16, [ALookaheadInstr(false,CfgLA (18,281),64)]);
(207, [CompleteInstr(295)]);
(208, [CompleteInstr(300)]);
(17, [EatInstr(59,96)]);
(209, [CompleteInstr(301)]);
(18, [AAction2Instr(__a1,65)]);
(210, [AContInstr3(294,__g78,__binder22,212);ACallInstr3(__g78,31)]);
(19, [EatInstr(61,66)]);
(211, [CompleteInstr(281)]);
(20, [EatInstr(32,59);EatInstr(9,56);ASimpleCont2Instr(273,__binder0,67);ASimpleCont2Instr(270,__binder0,67)]);
(212, [CompleteInstr(281);ACallInstr3(__default_call,213);ASimpleCont2Instr(283,__binder0,212);ASimpleCont2Instr(280,__binder0,211)]);
(21, [AAction2Instr(__a2,68)]);
(213, [EatInstr(59,96);EatInstr(32,59);EatInstr(9,56);ASimpleCont2Instr(273,__binder0,67);ASimpleCont2Instr(270,__binder0,67)]);
(22, [AAction2Instr(__a3,69)]);
(23, [RCompleteInstr2(286,nullable_o);AAction2Instr(__a4,102)]);
(24, [EatInstr(59,106);EatInstr(13,53);EatInstr(10,57);ASimpleCont2Instr(288,__binder0,70);ASimpleCont2Instr(271,__binder0,70);ASimpleCont2Instr(267,__binder0,70)]);
(25, [EatInstr(59,106)]);
(26, [EatInstr(92,71)]);
(27, [EatInstr(34,55);ASimpleCont2Instr(269,__binder0,109)]);
(28, [EatInstr(255,111);EatInstr(254,111);EatInstr(253,111);EatInstr(252,111);EatInstr(251,111);EatInstr(250,111);EatInstr(249,111);EatInstr(248,111);EatInstr(247,111);EatInstr(246,111);EatInstr(245,111);EatInstr(244,111);EatInstr(243,111);EatInstr(242,111);EatInstr(241,111);EatInstr(240,111);EatInstr(239,111);EatInstr(238,111);EatInstr(237,111);EatInstr(236,111);EatInstr(235,111);EatInstr(234,111);EatInstr(233,111);EatInstr(232,111);EatInstr(231,111);EatInstr(230,111);EatInstr(229,111);EatInstr(228,111);EatInstr(227,111);EatInstr(226,111);EatInstr(225,111);EatInstr(224,111);EatInstr(223,111);EatInstr(222,111);EatInstr(221,111);EatInstr(220,111);EatInstr(219,111);EatInstr(218,111);EatInstr(217,111);EatInstr(216,111);EatInstr(215,111);EatInstr(214,111);EatInstr(213,111);EatInstr(212,111);EatInstr(211,111);EatInstr(210,111);EatInstr(209,111);EatInstr(208,111);EatInstr(207,111);EatInstr(206,111);EatInstr(205,111);EatInstr(204,111);EatInstr(203,111);EatInstr(202,111);EatInstr(201,111);EatInstr(200,111);EatInstr(199,111);EatInstr(198,111);EatInstr(197,111);EatInstr(196,111);EatInstr(195,111);EatInstr(194,111);EatInstr(193,111);EatInstr(192,111);EatInstr(191,111);EatInstr(190,111);EatInstr(189,111);EatInstr(188,111);EatInstr(187,111);EatInstr(186,111);EatInstr(185,111);EatInstr(184,111);EatInstr(183,111);EatInstr(182,111);EatInstr(181,111);EatInstr(180,111);EatInstr(179,111);EatInstr(178,111);EatInstr(177,111);EatInstr(176,111);EatInstr(175,111);EatInstr(174,111);EatInstr(173,111);EatInstr(172,111);EatInstr(171,111);EatInstr(170,111);EatInstr(169,111);EatInstr(168,111);EatInstr(167,111);EatInstr(166,111);EatInstr(165,111);EatInstr(164,111);EatInstr(163,111);EatInstr(162,111);EatInstr(161,111);EatInstr(160,111);EatInstr(159,111);EatInstr(158,111);EatInstr(157,111);EatInstr(156,111);EatInstr(155,111);EatInstr(154,111);EatInstr(153,111);EatInstr(152,111);EatInstr(151,111);EatInstr(150,111);EatInstr(149,111);EatInstr(148,111);EatInstr(147,111);EatInstr(146,111);EatInstr(145,111);EatInstr(144,111);EatInstr(143,111);EatInstr(142,111);EatInstr(141,111);EatInstr(140,111);EatInstr(139,111);EatInstr(138,111);EatInstr(137,111);EatInstr(136,111);EatInstr(135,111);EatInstr(134,111);EatInstr(133,111);EatInstr(132,111);EatInstr(131,111);EatInstr(130,111);EatInstr(129,111);EatInstr(128,111);EatInstr(0,111);EatInstr(127,111);EatInstr(126,111);EatInstr(125,111);EatInstr(124,111);EatInstr(123,111);EatInstr(96,111);EatInstr(95,111);EatInstr(94,111);EatInstr(93,111);EatInstr(92,71);EatInstr(91,111);EatInstr(64,111);EatInstr(63,111);EatInstr(62,111);EatInstr(61,111);EatInstr(60,111);EatInstr(59,111);EatInstr(58,111);EatInstr(57,111);EatInstr(56,111);EatInstr(55,111);EatInstr(54,111);EatInstr(53,111);EatInstr(52,111);EatInstr(51,111);EatInstr(50,111);EatInstr(47,111);EatInstr(46,111);EatInstr(45,111);EatInstr(44,111);EatInstr(43,111);EatInstr(42,111);EatInstr(41,111);EatInstr(40,111);EatInstr(39,111);EatInstr(38,111);EatInstr(37,111);EatInstr(36,111);EatInstr(35,111);EatInstr(33,111);EatInstr(32,111);EatInstr(31,111);EatInstr(30,111);EatInstr(29,111);EatInstr(28,111);EatInstr(27,111);EatInstr(26,111);EatInstr(25,111);EatInstr(24,111);EatInstr(23,111);EatInstr(22,111);EatInstr(21,111);EatInstr(20,111);EatInstr(19,111);EatInstr(18,111);EatInstr(17,111);EatInstr(16,111);EatInstr(15,111);EatInstr(14,111);EatInstr(13,111);EatInstr(12,111);EatInstr(11,111);EatInstr(10,111);EatInstr(9,111);EatInstr(8,111);EatInstr(7,111);EatInstr(6,111);EatInstr(5,111);EatInstr(4,111);EatInstr(3,111);EatInstr(2,111);EatInstr(1,111);EatInstr(49,111);EatInstr(48,111);EatInstr(122,111);EatInstr(121,111);EatInstr(120,111);EatInstr(119,111);EatInstr(118,111);EatInstr(117,111);EatInstr(116,111);EatInstr(115,111);EatInstr(114,111);EatInstr(113,111);EatInstr(112,111);EatInstr(111,111);EatInstr(110,111);EatInstr(109,111);EatInstr(108,111);EatInstr(107,111);EatInstr(106,111);EatInstr(105,111);EatInstr(104,111);EatInstr(103,111);EatInstr(102,111);EatInstr(101,111);EatInstr(100,111);EatInstr(99,111);EatInstr(98,111);EatInstr(97,111);EatInstr(90,111);EatInstr(89,111);EatInstr(88,111);EatInstr(87,111);EatInstr(86,111);EatInstr(85,111);EatInstr(84,111);EatInstr(83,111);EatInstr(82,111);EatInstr(81,111);EatInstr(80,111);EatInstr(79,111);EatInstr(78,111);EatInstr(77,111);EatInstr(76,111);EatInstr(75,111);EatInstr(74,111);EatInstr(73,111);EatInstr(72,111);EatInstr(71,111);EatInstr(70,111);EatInstr(69,111);EatInstr(68,111);EatInstr(67,111);EatInstr(66,111);EatInstr(65,111);ASimpleCont2Instr(289,__binder0,72)]);
(29, [EatInstr(58,73);EatInstr(57,54);EatInstr(56,54);EatInstr(55,54);EatInstr(54,54);EatInstr(53,54);EatInstr(52,54);EatInstr(51,54);EatInstr(50,54);EatInstr(45,73);EatInstr(49,54);EatInstr(48,54);EatInstr(122,50);EatInstr(121,50);EatInstr(120,50);EatInstr(119,50);EatInstr(118,50);EatInstr(117,50);EatInstr(116,50);EatInstr(115,50);EatInstr(114,50);EatInstr(113,50);EatInstr(112,50);EatInstr(111,50);EatInstr(110,50);EatInstr(109,50);EatInstr(108,50);EatInstr(107,50);EatInstr(106,50);EatInstr(105,50);EatInstr(104,50);EatInstr(103,50);EatInstr(102,50);EatInstr(101,50);EatInstr(100,50);EatInstr(99,50);EatInstr(98,50);EatInstr(97,50);EatInstr(90,50);EatInstr(89,50);EatInstr(88,50);EatInstr(87,50);EatInstr(86,50);EatInstr(85,50);EatInstr(84,50);EatInstr(83,50);EatInstr(82,50);EatInstr(81,50);EatInstr(80,50);EatInstr(79,50);EatInstr(78,50);EatInstr(77,50);EatInstr(76,50);EatInstr(75,50);EatInstr(74,50);EatInstr(73,50);EatInstr(72,50);EatInstr(71,50);EatInstr(70,50);EatInstr(69,50);EatInstr(68,50);EatInstr(67,50);EatInstr(66,50);EatInstr(65,50);ASimpleCont2Instr(268,__binder0,73);ASimpleCont2Instr(264,__binder0,73)]);
(30, [EatInstr(122,50);EatInstr(121,50);EatInstr(120,50);EatInstr(119,50);EatInstr(118,50);EatInstr(117,50);EatInstr(116,50);EatInstr(115,50);EatInstr(114,50);EatInstr(113,50);EatInstr(112,50);EatInstr(111,50);EatInstr(110,50);EatInstr(109,50);EatInstr(108,50);EatInstr(107,50);EatInstr(106,50);EatInstr(105,50);EatInstr(104,50);EatInstr(103,50);EatInstr(102,50);EatInstr(101,50);EatInstr(100,50);EatInstr(99,50);EatInstr(98,50);EatInstr(97,50);EatInstr(90,50);EatInstr(89,50);EatInstr(88,50);EatInstr(87,50);EatInstr(86,50);EatInstr(85,50);EatInstr(84,50);EatInstr(83,50);EatInstr(82,50);EatInstr(81,50);EatInstr(80,50);EatInstr(79,50);EatInstr(78,50);EatInstr(77,50);EatInstr(76,50);EatInstr(75,50);EatInstr(74,50);EatInstr(73,50);EatInstr(72,50);EatInstr(71,50);EatInstr(70,50);EatInstr(69,50);EatInstr(68,50);EatInstr(67,50);EatInstr(66,50);EatInstr(65,50);ASimpleCont2Instr(264,__binder0,113)]);
(31, [AAction2Instr(__a5,74)]);
(32, [AAction2Instr(__a6,75)]);
(33, [AAction2Instr(__a7,76)]);
(34, [AAction2Instr(__a8,77)]);
(35, [EatInstr(42,79);EatInstr(35,79);RCompleteInstr2(304,nullable_DIGITS);AAction2Instr(__a9,144);RCompleteInstr2(298,nullable_repeat);ASimpleCont2Instr(304,__binder0,78)]);
(36, [AAction2Instr(__a10,80)]);
(37, [AAction2Instr(__a11,81)]);
(38, [AAction2Instr(__a12,82)]);
(39, [RCompleteInstr2(302,nullable_bitstring);AAction2Instr(__a13,150)]);
(40, [EatInstr(98,83)]);
(41, [RCompleteInstr2(304,nullable_DIGITS);AAction2Instr(__a9,144)]);
(42, [EatInstr(100,84)]);
(43, [EatInstr(57,54);EatInstr(56,54);EatInstr(55,54);EatInstr(54,54);EatInstr(53,54);EatInstr(52,54);EatInstr(51,54);EatInstr(50,54);EatInstr(49,54);EatInstr(48,54);EatInstr(102,85);EatInstr(101,85);EatInstr(100,85);EatInstr(99,85);EatInstr(98,85);EatInstr(97,85);EatInstr(70,85);EatInstr(69,85);EatInstr(68,85);EatInstr(67,85);EatInstr(66,85);EatInstr(65,85);ASimpleCont2Instr(268,__binder0,85)]);
(44, [RCompleteInstr2(307,nullable_HEXDIGS);AAction2Instr(__a14,156)]);
(45, [EatInstr(120,86)]);
(46, [EatInstr(37,87)]);
(47, [EatInstr(60,88);EatInstr(34,55);ASimpleCont2Instr(269,__binder0,135)]);
(48, [EatInstr(126,89);EatInstr(125,89);EatInstr(124,89);EatInstr(123,89);EatInstr(96,89);EatInstr(95,89);EatInstr(94,89);EatInstr(93,89);EatInstr(92,89);EatInstr(91,89);EatInstr(64,89);EatInstr(63,89);EatInstr(61,89);EatInstr(60,89);EatInstr(59,89);EatInstr(58,89);EatInstr(57,89);EatInstr(56,89);EatInstr(55,89);EatInstr(54,89);EatInstr(53,89);EatInstr(52,89);EatInstr(51,89);EatInstr(50,89);EatInstr(47,89);EatInstr(46,89);EatInstr(45,89);EatInstr(44,89);EatInstr(43,89);EatInstr(42,89);EatInstr(41,89);EatInstr(40,89);EatInstr(39,89);EatInstr(38,89);EatInstr(37,89);EatInstr(36,89);EatInstr(35,89);EatInstr(34,89);EatInstr(33,89);EatInstr(32,89);EatInstr(49,89);EatInstr(48,89);EatInstr(122,89);EatInstr(121,89);EatInstr(120,89);EatInstr(119,89);EatInstr(118,89);EatInstr(117,89);EatInstr(116,89);EatInstr(115,89);EatInstr(114,89);EatInstr(113,89);EatInstr(112,89);EatInstr(111,89);EatInstr(110,89);EatInstr(109,89);EatInstr(108,89);EatInstr(107,89);EatInstr(106,89);EatInstr(105,89);EatInstr(104,89);EatInstr(103,89);EatInstr(102,89);EatInstr(101,89);EatInstr(100,89);EatInstr(99,89);EatInstr(98,89);EatInstr(97,89);EatInstr(90,89);EatInstr(89,89);EatInstr(88,89);EatInstr(87,89);EatInstr(86,89);EatInstr(85,89);EatInstr(84,89);EatInstr(83,89);EatInstr(82,89);EatInstr(81,89);EatInstr(80,89);EatInstr(79,89);EatInstr(78,89);EatInstr(77,89);EatInstr(76,89);EatInstr(75,89);EatInstr(74,89);EatInstr(73,89);EatInstr(72,89);EatInstr(71,89);EatInstr(70,89);EatInstr(69,89);EatInstr(68,89);EatInstr(67,89);EatInstr(66,89);EatInstr(65,89)]);
(49, [EatInstr(60,90)]);
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
(62, [ACallInstr3(__default_call,14);ASimpleCont2Instr(277,__binder0,91)]);
(63, [CompleteInstr(278)]);
(64, [ACallInstr3(__default_call,95);ASimpleCont2Instr(271,__binder0,94);ASimpleCont2Instr(267,__binder0,94)]);
(65, [ASimpleCont2Instr(284,__binder1,98);ACallInstr3(__default_call,21)]);
(66, [EatInstr(47,99);CompleteInstr(282)]);
(67, [CompleteInstr(283)]);
(68, [ACallInstr3(__default_call,95);ASimpleCont2Instr(271,__binder0,100);ASimpleCont2Instr(267,__binder0,100)]);
(69, [ACallInstr3(__default_call,24);ASimpleCont2Instr(287,__binder0,101)]);
(70, [CompleteInstr(287)]);
(71, [CompleteInstr(289)]);
(72, [EatInstr(255,111);EatInstr(254,111);EatInstr(253,111);EatInstr(252,111);EatInstr(251,111);EatInstr(250,111);EatInstr(249,111);EatInstr(248,111);EatInstr(247,111);EatInstr(246,111);EatInstr(245,111);EatInstr(244,111);EatInstr(243,111);EatInstr(242,111);EatInstr(241,111);EatInstr(240,111);EatInstr(239,111);EatInstr(238,111);EatInstr(237,111);EatInstr(236,111);EatInstr(235,111);EatInstr(234,111);EatInstr(233,111);EatInstr(232,111);EatInstr(231,111);EatInstr(230,111);EatInstr(229,111);EatInstr(228,111);EatInstr(227,111);EatInstr(226,111);EatInstr(225,111);EatInstr(224,111);EatInstr(223,111);EatInstr(222,111);EatInstr(221,111);EatInstr(220,111);EatInstr(219,111);EatInstr(218,111);EatInstr(217,111);EatInstr(216,111);EatInstr(215,111);EatInstr(214,111);EatInstr(213,111);EatInstr(212,111);EatInstr(211,111);EatInstr(210,111);EatInstr(209,111);EatInstr(208,111);EatInstr(207,111);EatInstr(206,111);EatInstr(205,111);EatInstr(204,111);EatInstr(203,111);EatInstr(202,111);EatInstr(201,111);EatInstr(200,111);EatInstr(199,111);EatInstr(198,111);EatInstr(197,111);EatInstr(196,111);EatInstr(195,111);EatInstr(194,111);EatInstr(193,111);EatInstr(192,111);EatInstr(191,111);EatInstr(190,111);EatInstr(189,111);EatInstr(188,111);EatInstr(187,111);EatInstr(186,111);EatInstr(185,111);EatInstr(184,111);EatInstr(183,111);EatInstr(182,111);EatInstr(181,111);EatInstr(180,111);EatInstr(179,111);EatInstr(178,111);EatInstr(177,111);EatInstr(176,111);EatInstr(175,111);EatInstr(174,111);EatInstr(173,111);EatInstr(172,111);EatInstr(171,111);EatInstr(170,111);EatInstr(169,111);EatInstr(168,111);EatInstr(167,111);EatInstr(166,111);EatInstr(165,111);EatInstr(164,111);EatInstr(163,111);EatInstr(162,111);EatInstr(161,111);EatInstr(160,111);EatInstr(159,111);EatInstr(158,111);EatInstr(157,111);EatInstr(156,111);EatInstr(155,111);EatInstr(154,111);EatInstr(153,111);EatInstr(152,111);EatInstr(151,111);EatInstr(150,111);EatInstr(149,111);EatInstr(148,111);EatInstr(147,111);EatInstr(146,111);EatInstr(145,111);EatInstr(144,111);EatInstr(143,111);EatInstr(142,111);EatInstr(141,111);EatInstr(140,111);EatInstr(139,111);EatInstr(138,111);EatInstr(137,111);EatInstr(136,111);EatInstr(135,111);EatInstr(134,111);EatInstr(133,111);EatInstr(132,111);EatInstr(131,111);EatInstr(130,111);EatInstr(129,111);EatInstr(128,111);EatInstr(0,111);EatInstr(127,111);EatInstr(126,111);EatInstr(125,111);EatInstr(124,111);EatInstr(123,111);EatInstr(96,111);EatInstr(95,111);EatInstr(94,111);EatInstr(93,111);EatInstr(91,111);EatInstr(64,111);EatInstr(63,111);EatInstr(62,111);EatInstr(61,111);EatInstr(60,111);EatInstr(59,111);EatInstr(58,111);EatInstr(57,111);EatInstr(56,111);EatInstr(55,111);EatInstr(54,111);EatInstr(53,111);EatInstr(52,111);EatInstr(51,111);EatInstr(50,111);EatInstr(47,111);EatInstr(46,111);EatInstr(45,111);EatInstr(44,111);EatInstr(43,111);EatInstr(42,111);EatInstr(41,111);EatInstr(40,111);EatInstr(39,111);EatInstr(38,111);EatInstr(37,111);EatInstr(36,111);EatInstr(35,111);EatInstr(33,111);EatInstr(32,111);EatInstr(31,111);EatInstr(30,111);EatInstr(29,111);EatInstr(28,111);EatInstr(27,111);EatInstr(26,111);EatInstr(25,111);EatInstr(24,111);EatInstr(23,111);EatInstr(22,111);EatInstr(21,111);EatInstr(20,111);EatInstr(19,111);EatInstr(18,111);EatInstr(17,111);EatInstr(16,111);EatInstr(15,111);EatInstr(14,111);EatInstr(13,111);EatInstr(12,111);EatInstr(11,111);EatInstr(10,111);EatInstr(9,111);EatInstr(8,111);EatInstr(7,111);EatInstr(6,111);EatInstr(5,111);EatInstr(4,111);EatInstr(3,111);EatInstr(2,111);EatInstr(1,111);EatInstr(49,111);EatInstr(48,111);EatInstr(122,111);EatInstr(121,111);EatInstr(120,111);EatInstr(119,111);EatInstr(118,111);EatInstr(117,111);EatInstr(116,111);EatInstr(115,111);EatInstr(114,111);EatInstr(113,111);EatInstr(112,111);EatInstr(111,111);EatInstr(110,111);EatInstr(109,111);EatInstr(108,111);EatInstr(107,111);EatInstr(106,111);EatInstr(105,111);EatInstr(104,111);EatInstr(103,111);EatInstr(102,111);EatInstr(101,111);EatInstr(100,111);EatInstr(99,111);EatInstr(98,111);EatInstr(97,111);EatInstr(90,111);EatInstr(89,111);EatInstr(88,111);EatInstr(87,111);EatInstr(86,111);EatInstr(85,111);EatInstr(84,111);EatInstr(83,111);EatInstr(82,111);EatInstr(81,111);EatInstr(80,111);EatInstr(79,111);EatInstr(78,111);EatInstr(77,111);EatInstr(76,111);EatInstr(75,111);EatInstr(74,111);EatInstr(73,111);EatInstr(72,111);EatInstr(71,111);EatInstr(70,111);EatInstr(69,111);EatInstr(68,111);EatInstr(67,111);EatInstr(66,111);EatInstr(65,111);ACallInstr3(__default_call,112);ASimpleCont2Instr(289,__binder0,111);ASimpleCont2Instr(269,__binder0,111)]);
(73, [CompleteInstr(292)]);
(74, [AContInstr3(295,__g15,__binder2,115);ACallInstr3(__g15,32)]);
(75, [AContInstr3(296,__g16,__binder3,116);ACallInstr3(__g16,33)]);
(76, [AContInstr3(297,__g17,__binder4,117);ACallInstr3(__g17,34)]);
(77, [AContInstr3(299,__g18,__binder5,119);ACallInstr3(__g18,36);ACallInstr3(__default_call,35);ASimpleCont2Instr(298,__binder0,118)]);
(78, [EatInstr(42,79);EatInstr(35,79);CompleteInstr(298)]);
(79, [CompleteInstr(298);ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,143)]);
(80, [AAction2Instr(__a22,123);AContInstr3(301,__g21,__binder7,192);ACallInstr3(__g21,38);AContInstr3(300,__g20,__binder6,192);ACallInstr3(__g20,37);AAction2Instr(__a19,122)]);
(81, [EatInstr(40,124)]);
(82, [EatInstr(91,125)]);
(83, [ACallInstr3(__default_call,39);ASimpleCont2Instr(302,__binder0,128)]);
(84, [ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,129)]);
(85, [CompleteInstr(306)]);
(86, [ACallInstr3(__default_call,44);ASimpleCont2Instr(307,__binder0,132)]);
(87, [ACallInstr3(__default_call,134);ASimpleCont2Instr(308,__binder0,133);ASimpleCont2Instr(305,__binder0,133);ASimpleCont2Instr(303,__binder0,133)]);
(88, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,136)]);
(89, [CompleteInstr(311)]);
(90, [EatInstr(126,161);EatInstr(125,161);EatInstr(124,161);EatInstr(123,161);EatInstr(96,161);EatInstr(95,161);EatInstr(94,161);EatInstr(93,161);EatInstr(92,161);EatInstr(91,161);EatInstr(64,161);EatInstr(63,161);EatInstr(62,137);EatInstr(61,161);EatInstr(60,161);EatInstr(59,161);EatInstr(58,161);EatInstr(57,161);EatInstr(56,161);EatInstr(55,161);EatInstr(54,161);EatInstr(53,161);EatInstr(52,161);EatInstr(51,161);EatInstr(50,161);EatInstr(47,161);EatInstr(46,161);EatInstr(45,161);EatInstr(44,161);EatInstr(43,161);EatInstr(42,161);EatInstr(41,161);EatInstr(40,161);EatInstr(39,161);EatInstr(38,161);EatInstr(37,161);EatInstr(36,161);EatInstr(35,161);EatInstr(33,161);EatInstr(32,161);EatInstr(49,161);EatInstr(48,161);EatInstr(122,161);EatInstr(121,161);EatInstr(120,161);EatInstr(119,161);EatInstr(118,161);EatInstr(117,161);EatInstr(116,161);EatInstr(115,161);EatInstr(114,161);EatInstr(113,161);EatInstr(112,161);EatInstr(111,161);EatInstr(110,161);EatInstr(109,161);EatInstr(108,161);EatInstr(107,161);EatInstr(106,161);EatInstr(105,161);EatInstr(104,161);EatInstr(103,161);EatInstr(102,161);EatInstr(101,161);EatInstr(100,161);EatInstr(99,161);EatInstr(98,161);EatInstr(97,161);EatInstr(90,161);EatInstr(89,161);EatInstr(88,161);EatInstr(87,161);EatInstr(86,161);EatInstr(85,161);EatInstr(84,161);EatInstr(83,161);EatInstr(82,161);EatInstr(81,161);EatInstr(80,161);EatInstr(79,161);EatInstr(78,161);EatInstr(77,161);EatInstr(76,161);EatInstr(75,161);EatInstr(74,161);EatInstr(73,161);EatInstr(72,161);EatInstr(71,161);EatInstr(70,161);EatInstr(69,161);EatInstr(68,161);EatInstr(67,161);EatInstr(66,161);EatInstr(65,161)]);
(91, [AAction2Instr(__a23,195)]);
(92, [ALookaheadInstr(false,CfgLA (15,278),93);ACallInstr3(__default_call,15);ASimpleCont2Instr(278,__binder0,92)]);
(93, [CompleteInstr(277)]);
(94, [ACallInstr3(__default_call,14);ASimpleCont2Instr(277,__binder0,138)]);
(95, [EatInstr(13,53);EatInstr(10,57)]);
(96, [CompleteInstr(280);ACallInstr3(__default_call,97);ASimpleCont2Instr(275,__binder0,96);ASimpleCont2Instr(274,__binder0,96)]);
(97, [EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(47,60);EatInstr(46,60);EatInstr(45,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(32,59);EatInstr(9,56);EatInstr(49,60);EatInstr(48,60);EatInstr(122,60);EatInstr(121,60);EatInstr(120,60);EatInstr(119,60);EatInstr(118,60);EatInstr(117,60);EatInstr(116,60);EatInstr(115,60);EatInstr(114,60);EatInstr(113,60);EatInstr(112,60);EatInstr(111,60);EatInstr(110,60);EatInstr(109,60);EatInstr(108,60);EatInstr(107,60);EatInstr(106,60);EatInstr(105,60);EatInstr(104,60);EatInstr(103,60);EatInstr(102,60);EatInstr(101,60);EatInstr(100,60);EatInstr(99,60);EatInstr(98,60);EatInstr(97,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60);ASimpleCont2Instr(273,__binder0,61);ASimpleCont2Instr(270,__binder0,61)]);
(98, [AAction2Instr(__a24,139)]);
(99, [CompleteInstr(282)]);
(100, [AAction2Instr(__a25,166)]);
(101, [AAction2Instr(__a26,168)]);
(102, [AAction2Instr(__a28,104);AContInstr3(285,__g27,__binder8,104);ACallInstr3(__g27,22);ACallInstr3(__default_call,103);ASimpleCont2Instr(287,__binder0,140);ASimpleCont2Instr(283,__binder0,102)]);
(103, [EatInstr(59,106);EatInstr(32,59);EatInstr(13,53);EatInstr(10,57);EatInstr(9,56);ASimpleCont2Instr(288,__binder0,70);ASimpleCont2Instr(273,__binder0,67);ASimpleCont2Instr(271,__binder0,70);ASimpleCont2Instr(270,__binder0,67);ASimpleCont2Instr(267,__binder0,70)]);
(104, [ALookaheadInstr(false,CfgLA (20,283),141)]);
(105, [CompleteInstr(288)]);
(106, [ACallInstr3(__default_call,107);ASimpleCont2Instr(275,__binder0,106);ASimpleCont2Instr(274,__binder0,106);ASimpleCont2Instr(271,__binder0,105);ASimpleCont2Instr(267,__binder0,105)]);
(107, [EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(47,60);EatInstr(46,60);EatInstr(45,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(32,59);EatInstr(13,53);EatInstr(10,57);EatInstr(9,56);EatInstr(49,60);EatInstr(48,60);EatInstr(122,60);EatInstr(121,60);EatInstr(120,60);EatInstr(119,60);EatInstr(118,60);EatInstr(117,60);EatInstr(116,60);EatInstr(115,60);EatInstr(114,60);EatInstr(113,60);EatInstr(112,60);EatInstr(111,60);EatInstr(110,60);EatInstr(109,60);EatInstr(108,60);EatInstr(107,60);EatInstr(106,60);EatInstr(105,60);EatInstr(104,60);EatInstr(103,60);EatInstr(102,60);EatInstr(101,60);EatInstr(100,60);EatInstr(99,60);EatInstr(98,60);EatInstr(97,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60);ASimpleCont2Instr(273,__binder0,61);ASimpleCont2Instr(270,__binder0,61)]);
(108, [CompleteInstr(290)]);
(109, [ACallInstr3(__default_call,110);ASimpleCont2Instr(291,__binder0,109);ASimpleCont2Instr(269,__binder0,108)]);
(110, [EatInstr(255,111);EatInstr(254,111);EatInstr(253,111);EatInstr(252,111);EatInstr(251,111);EatInstr(250,111);EatInstr(249,111);EatInstr(248,111);EatInstr(247,111);EatInstr(246,111);EatInstr(245,111);EatInstr(244,111);EatInstr(243,111);EatInstr(242,111);EatInstr(241,111);EatInstr(240,111);EatInstr(239,111);EatInstr(238,111);EatInstr(237,111);EatInstr(236,111);EatInstr(235,111);EatInstr(234,111);EatInstr(233,111);EatInstr(232,111);EatInstr(231,111);EatInstr(230,111);EatInstr(229,111);EatInstr(228,111);EatInstr(227,111);EatInstr(226,111);EatInstr(225,111);EatInstr(224,111);EatInstr(223,111);EatInstr(222,111);EatInstr(221,111);EatInstr(220,111);EatInstr(219,111);EatInstr(218,111);EatInstr(217,111);EatInstr(216,111);EatInstr(215,111);EatInstr(214,111);EatInstr(213,111);EatInstr(212,111);EatInstr(211,111);EatInstr(210,111);EatInstr(209,111);EatInstr(208,111);EatInstr(207,111);EatInstr(206,111);EatInstr(205,111);EatInstr(204,111);EatInstr(203,111);EatInstr(202,111);EatInstr(201,111);EatInstr(200,111);EatInstr(199,111);EatInstr(198,111);EatInstr(197,111);EatInstr(196,111);EatInstr(195,111);EatInstr(194,111);EatInstr(193,111);EatInstr(192,111);EatInstr(191,111);EatInstr(190,111);EatInstr(189,111);EatInstr(188,111);EatInstr(187,111);EatInstr(186,111);EatInstr(185,111);EatInstr(184,111);EatInstr(183,111);EatInstr(182,111);EatInstr(181,111);EatInstr(180,111);EatInstr(179,111);EatInstr(178,111);EatInstr(177,111);EatInstr(176,111);EatInstr(175,111);EatInstr(174,111);EatInstr(173,111);EatInstr(172,111);EatInstr(171,111);EatInstr(170,111);EatInstr(169,111);EatInstr(168,111);EatInstr(167,111);EatInstr(166,111);EatInstr(165,111);EatInstr(164,111);EatInstr(163,111);EatInstr(162,111);EatInstr(161,111);EatInstr(160,111);EatInstr(159,111);EatInstr(158,111);EatInstr(157,111);EatInstr(156,111);EatInstr(155,111);EatInstr(154,111);EatInstr(153,111);EatInstr(152,111);EatInstr(151,111);EatInstr(150,111);EatInstr(149,111);EatInstr(148,111);EatInstr(147,111);EatInstr(146,111);EatInstr(145,111);EatInstr(144,111);EatInstr(143,111);EatInstr(142,111);EatInstr(141,111);EatInstr(140,111);EatInstr(139,111);EatInstr(138,111);EatInstr(137,111);EatInstr(136,111);EatInstr(135,111);EatInstr(134,111);EatInstr(133,111);EatInstr(132,111);EatInstr(131,111);EatInstr(130,111);EatInstr(129,111);EatInstr(128,111);EatInstr(0,111);EatInstr(127,111);EatInstr(126,111);EatInstr(125,111);EatInstr(124,111);EatInstr(123,111);EatInstr(96,111);EatInstr(95,111);EatInstr(94,111);EatInstr(93,111);EatInstr(92,71);EatInstr(91,111);EatInstr(64,111);EatInstr(63,111);EatInstr(62,111);EatInstr(61,111);EatInstr(60,111);EatInstr(59,111);EatInstr(58,111);EatInstr(57,111);EatInstr(56,111);EatInstr(55,111);EatInstr(54,111);EatInstr(53,111);EatInstr(52,111);EatInstr(51,111);EatInstr(50,111);EatInstr(47,111);EatInstr(46,111);EatInstr(45,111);EatInstr(44,111);EatInstr(43,111);EatInstr(42,111);EatInstr(41,111);EatInstr(40,111);EatInstr(39,111);EatInstr(38,111);EatInstr(37,111);EatInstr(36,111);EatInstr(35,111);EatInstr(34,55);EatInstr(33,111);EatInstr(32,111);EatInstr(31,111);EatInstr(30,111);EatInstr(29,111);EatInstr(28,111);EatInstr(27,111);EatInstr(26,111);EatInstr(25,111);EatInstr(24,111);EatInstr(23,111);EatInstr(22,111);EatInstr(21,111);EatInstr(20,111);EatInstr(19,111);EatInstr(18,111);EatInstr(17,111);EatInstr(16,111);EatInstr(15,111);EatInstr(14,111);EatInstr(13,111);EatInstr(12,111);EatInstr(11,111);EatInstr(10,111);EatInstr(9,111);EatInstr(8,111);EatInstr(7,111);EatInstr(6,111);EatInstr(5,111);EatInstr(4,111);EatInstr(3,111);EatInstr(2,111);EatInstr(1,111);EatInstr(49,111);EatInstr(48,111);EatInstr(122,111);EatInstr(121,111);EatInstr(120,111);EatInstr(119,111);EatInstr(118,111);EatInstr(117,111);EatInstr(116,111);EatInstr(115,111);EatInstr(114,111);EatInstr(113,111);EatInstr(112,111);EatInstr(111,111);EatInstr(110,111);EatInstr(109,111);EatInstr(108,111);EatInstr(107,111);EatInstr(106,111);EatInstr(105,111);EatInstr(104,111);EatInstr(103,111);EatInstr(102,111);EatInstr(101,111);EatInstr(100,111);EatInstr(99,111);EatInstr(98,111);EatInstr(97,111);EatInstr(90,111);EatInstr(89,111);EatInstr(88,111);EatInstr(87,111);EatInstr(86,111);EatInstr(85,111);EatInstr(84,111);EatInstr(83,111);EatInstr(82,111);EatInstr(81,111);EatInstr(80,111);EatInstr(79,111);EatInstr(78,111);EatInstr(77,111);EatInstr(76,111);EatInstr(75,111);EatInstr(74,111);EatInstr(73,111);EatInstr(72,111);EatInstr(71,111);EatInstr(70,111);EatInstr(69,111);EatInstr(68,111);EatInstr(67,111);EatInstr(66,111);EatInstr(65,111);ASimpleCont2Instr(289,__binder0,72)]);
(111, [CompleteInstr(291)]);
(112, [EatInstr(92,71);EatInstr(34,55)]);
(113, [ALookaheadInstr(false,CfgLA (29,292),114);ACallInstr3(__default_call,29);ASimpleCont2Instr(292,__binder0,113)]);
(114, [CompleteInstr(293)]);
(115, [CompleteInstr(294)]);
(116, [AAction2Instr(__a30,207);AAction2Instr(__a29,142)]);
(117, [AAction2Instr(__a31,199)]);
(118, [AContInstr3(299,__g18,__binder5,119);ACallInstr3(__g18,36)]);
(119, [CompleteInstr(297)]);
(120, [AAction2Instr(__a32,144)]);
(121, [AWhenInstr3(__p34,__p33,145)]);
(122, [ACallInstr3(__default_call,30);ASimpleCont2Instr(293,__binder0,192)]);
(123, [AAction2Instr(__a36,147);AAction2Instr(__a35,146)]);
(124, [AAction2Instr(__a37,148)]);
(125, [AAction2Instr(__a38,149)]);
(126, [AAction2Instr(__a39,150)]);
(127, [AWhenInstr3(__p41,__p40,151)]);
(128, [EatInstr(46,153);EatInstr(45,152);CompleteInstr(303)]);
(129, [EatInstr(46,155);EatInstr(45,154);CompleteInstr(305)]);
(130, [AAction2Instr(__a42,156)]);
(131, [AWhenInstr3(__p44,__p43,157)]);
(132, [EatInstr(46,159);EatInstr(45,158);CompleteInstr(308)]);
(133, [CompleteInstr(309)]);
(134, [EatInstr(120,86);EatInstr(100,84);EatInstr(98,83)]);
(135, [EatInstr(126,135);EatInstr(125,135);EatInstr(124,135);EatInstr(123,135);EatInstr(96,135);EatInstr(95,135);EatInstr(94,135);EatInstr(93,135);EatInstr(92,135);EatInstr(91,135);EatInstr(64,135);EatInstr(63,135);EatInstr(62,135);EatInstr(61,135);EatInstr(60,135);EatInstr(59,135);EatInstr(58,135);EatInstr(57,135);EatInstr(56,135);EatInstr(55,135);EatInstr(54,135);EatInstr(53,135);EatInstr(52,135);EatInstr(51,135);EatInstr(50,135);EatInstr(47,135);EatInstr(46,135);EatInstr(45,135);EatInstr(44,135);EatInstr(43,135);EatInstr(42,135);EatInstr(41,135);EatInstr(40,135);EatInstr(39,135);EatInstr(38,135);EatInstr(37,135);EatInstr(36,135);EatInstr(35,135);EatInstr(33,135);EatInstr(32,135);EatInstr(49,135);EatInstr(48,135);EatInstr(122,135);EatInstr(121,135);EatInstr(120,135);EatInstr(119,135);EatInstr(118,135);EatInstr(117,135);EatInstr(116,135);EatInstr(115,135);EatInstr(114,135);EatInstr(113,135);EatInstr(112,135);EatInstr(111,135);EatInstr(110,135);EatInstr(109,135);EatInstr(108,135);EatInstr(107,135);EatInstr(106,135);EatInstr(105,135);EatInstr(104,135);EatInstr(103,135);EatInstr(102,135);EatInstr(101,135);EatInstr(100,135);EatInstr(99,135);EatInstr(98,135);EatInstr(97,135);EatInstr(90,135);EatInstr(89,135);EatInstr(88,135);EatInstr(87,135);EatInstr(86,135);EatInstr(85,135);EatInstr(84,135);EatInstr(83,135);EatInstr(82,135);EatInstr(81,135);EatInstr(80,135);EatInstr(79,135);EatInstr(78,135);EatInstr(77,135);EatInstr(76,135);EatInstr(75,135);EatInstr(74,135);EatInstr(73,135);EatInstr(72,135);EatInstr(71,135);EatInstr(70,135);EatInstr(69,135);EatInstr(68,135);EatInstr(67,135);EatInstr(66,135);EatInstr(65,135);ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,160)]);
(136, [EatInstr(62,160)]);
(137, [CompleteInstr(312)]);
(138, [CompleteInstr(279)]);
(139, [ACallInstr3(__default_call,30);ASimpleCont2Instr(293,__binder0,165)]);
(140, [AContInstr3(285,__g27,__binder8,104);ACallInstr3(__g27,22);ACallInstr3(__default_call,103);ASimpleCont2Instr(287,__binder0,140);ASimpleCont2Instr(283,__binder0,140)]);
(141, [CompleteInstr(286)]);
(142, [AContInstr3(286,__g45,__binder9,170);ACallInstr3(__g45,23)]);
(143, [CompleteInstr(298)]);
(144, [AAction2Instr(__a46,121);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,120)]);
(145, [ALookaheadInstr(false,CfgLA (5,268),173)]);
(146, [ACallInstr3(__default_call,47);ASimpleCont2Instr(310,__binder0,192)]);
(147, [AAction2Instr(__a48,175);AAction2Instr(__a47,174)]);
(148, [AContInstr3(286,__g49,__binder10,176);ACallInstr3(__g49,23)]);
(149, [AContInstr3(286,__g50,__binder11,177);ACallInstr3(__g50,23)]);
(150, [AAction2Instr(__a51,127);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,126)]);
(151, [ALookaheadInstr(false,CfgLA (2,265),178)]);
(152, [ACallInstr3(__default_call,39);ASimpleCont2Instr(302,__binder0,179)]);
(153, [ACallInstr3(__default_call,39);ASimpleCont2Instr(302,__binder0,180)]);
(154, [ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,181)]);
(155, [ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,182)]);
(156, [AAction2Instr(__a52,131);ACallInstr3(__default_call,43);ASimpleCont2Instr(306,__binder0,130)]);
(157, [ALookaheadInstr(false,CfgLA (43,306),183)]);
(158, [ACallInstr3(__default_call,44);ASimpleCont2Instr(307,__binder0,184)]);
(159, [ACallInstr3(__default_call,44);ASimpleCont2Instr(307,__binder0,185)]);
(160, [CompleteInstr(310)]);
(161, [EatInstr(62,137);ACallInstr3(__default_call,48);ASimpleCont2Instr(311,__binder0,161)]);
(162, [ACallInstr3(__default_call,18);ASimpleCont2Instr(281,__binder0,186)]);
(163, [ACallInstr3(__default_call,16);ASimpleCont2Instr(279,__binder0,195)]);
(164, [CompleteInstr(276)]);
(165, [AAction2Instr(__a53,187)]);
(166, [ALookaheadInstr(false,CfgLA (20,283),167);ACallInstr3(__default_call,20);ASimpleCont2Instr(283,__binder0,166)]);
(167, [AAction2Instr(__a54,188)]);
(168, [ALookaheadInstr(false,CfgLA (20,283),169);ACallInstr3(__default_call,20);ASimpleCont2Instr(283,__binder0,168)]);
(169, [AAction2Instr(__a55,189)]);
(170, [EatInstr(124,190);EatInstr(47,190)]);
(171, [AContInstr3(286,__g56,__binder12,191);ACallInstr3(__g56,23)]);
(172, [CompleteInstr(296)]);
(173, [CompleteInstr(304)]);
(174, [ACallInstr3(__default_call,46);ASimpleCont2Instr(309,__binder0,192)]);
(175, [ACallInstr3(__default_call,49);ASimpleCont2Instr(312,__binder0,192)]);
(176, [AAction2Instr(__a57,193)]);
(177, [AAction2Instr(__a58,194)]);
(178, [CompleteInstr(302)]);
(179, [CompleteInstr(303)]);
(180, [EatInstr(46,153);CompleteInstr(303)]);
(181, [CompleteInstr(305)]);
(182, [EatInstr(46,155);CompleteInstr(305)]);
(183, [CompleteInstr(307)]);
(184, [CompleteInstr(308)]);
(185, [EatInstr(46,159);CompleteInstr(308)]);
(186, [AAction2Instr(__a59,195)]);
(187, [AContInstr3(286,__g60,__binder13,196);ACallInstr3(__g60,23)]);
(188, [CompleteInstr(284)]);
(189, [AWhenInstr3(__p62,__p61,197)]);
(190, [AAction2Instr(__a63,198)]);
]

let start_symb = get_symb_action "rfc"

module P2__ = Yak.Engine.Full_yakker(struct type t = sv let cmp = sv_compare end)

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

let extract ch file =
  begin
    outch := ch;
    ignore(parse_file file);
    outch := stdout
  end
