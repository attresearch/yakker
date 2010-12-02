
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
let __a55 = _d 1154;;
let __a27 = _d 1211;;
let __a32 = _d 1097;;
let __a7 = fun p v -> _d 1114 p (_d 1113 p (_d 1112 p (_d 1111 p (_d 1110 p (_d 1109 p (_x114 p (v)))))));;
let __a45 = _d 1213;;
let __p46 = _dnext 1216;;
let __a33 = _d 1107;;
let __a31 = fun p v -> _d 1058 p (_d 1057 p (v));;
let __p44 = _dwhen 1195;;
let __a64 = fun p v -> _p 1018 p (_p 1017 p (_p 1016 p (_p 1015 p (_ddelay 1014 p (_p 1013 p (_ddelay 1012 p (_d_and_push 1011 p (_d 1009 p (_d 1008 p (v))))))))));;
let __g61 = _darg 1120;;
let __a49 = fun p v -> _d_and_push 1020 p (_p 1019 p (v));;
let __p37 = _dwhen 1205;;
let __g69 = _darg 1122;;
let __a63 = _d 1181;;
let __a29 = _d 1026;;
let __a1 = fun p v -> _d 1023 p (_d 1022 p (_x56 p (v)));;
let __p67 = _dwhen 1065;;
let __a58 = _d 1028;;
let __a11 = fun p v -> _d 1160 p (_d 1159 p (_d 1158 p (_d 1157 p (_d 1156 p (_x146 p (v))))));;
let __a16 = _d 1079;;
let __g57 = _darg 1179;;
let __a48 = fun p v -> _d 1005 p (_d 1004 p (v));;
let __a14 = fun p v -> _d 1210 p (_d 1209 p (_x171 p (v)));;
let __g72 = _darg 1102;;
let __a40 = _d 1162;;
let __g18 = _darg 1095;;
let __g76 = _darg 1104;;
let __a62 = _d 1165;;
let __g51 = _darg 1098;;
let __a13 = fun p v -> _d 1190 p (_d 1189 p (_x163 p (v)));;
let __a34 = _d 1117;;
let __a30 = fun p v -> _d 1041 p (_d 1040 p (v));;
let __a8 = fun p v -> _d 1129 p (_d 1128 p (_d 1127 p (_d 1126 p (_d 1125 p (_x121 p (v))))));;
let __a75 = _d 1032;;
let __a53 = _d 1118;;
let __a52 = _d 1119;;
let __a22 = _d 1140;;
let __a3 = fun p v -> _d 1055 p (_d 1054 p (_d 1053 p (_d 1052 p (_d 1051 p (_x69 p (v))))));;
let __a9 = fun p v -> _d 1200 p (_d 1199 p (_x167 p (v)));;
let __p47 = _dwhen 1215;;
let __a5 = fun p v -> _d 1085 p (_d 1084 p (_d 1083 p (_d 1082 p (_d 1081 p (_x83 p (v))))));;
let __g20 = _darg 1131;;
let __a2 = fun p v -> _d 1038 p (_x61 p (v));;
let __a26 = _d 1191;;
let __g71 = _darg 1182;;
let __g15 = _darg 1076;;
let __a4 = fun p v -> _d 1072 p (_d 1071 p (_d 1070 p (_d 1069 p (_d 1068 p (_x76 p (v))))));;
let __a21 = _d 1201;;
let __a42 = _d 1193;;
let __p43 = _dnext 1196;;
let __a12 = fun p v -> _d 1176 p (_d 1175 p (_d 1174 p (_d 1173 p (_d 1172 p (_x159 p (v))))));;
let __g65 = _darg 1029;;
let __g74 = _darg 1185;;
let __p36 = _dnext 1206;;
let __a35 = _d 1203;;
let __a28 = _d_and_push 1002;;
let __a6 = fun p v -> _d 1094 p (_d 1093 p (_d 1092 p (_d 1091 p (_d 1090 p (_d 1089 p (_x99 p (v)))))));;
let __a50 = _d_and_push 1003;;
let __a38 = _d 1148;;
let __p66 = _dnext 1066;;
let __a60 = fun p v -> _d 1063 p (_d 1062 p (v));;
let __a39 = fun p v -> _d 1150 p (_d 1149 p (v));;
let __a25 = fun p v -> _d 1146 p (_d 1145 p (v));;
let __a0 = fun p v -> _p 1000 p (_x44 p (v));;
let __g56 = _darg 1163;;
let __a10 = fun p v -> _d 1138 p (_d 1137 p (_d 1136 p (_d 1135 p (_d 1134 p (_x133 p (v))))));;
let __g19 = _darg 1115;;
let __g70 = _darg 1166;;
let __g77 = _darg 1033;;
let __a41 = _d 1178;;
let __g73 = _darg 1169;;
let __a68 = _d 1101;;
let __a59 = fun p v -> _d 1049 p (_d 1048 p (_d 1046 p (_d 1045 p (v))));;
let __g23 = _darg 1141;;
let __g78 = _darg 1036;;
let __a54 = _d 1152;;
let __g24 = _darg 1143;;
let __g17 = _darg 1086;;
let __binder0 = __default_ret;;
let __binder1 = _dret 1025;;
let __binder2 = _dret 1077;;
let __binder3 = _dret 1087;;
let __binder4 = _dret 1096;;
let __binder5 = _dret 1116;;
let __binder6 = _dret 1132;;
let __binder7 = _dret 1142;;
let __binder8 = _dret 1144;;
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
(191, [AAction2Instr(__a64,147)]);
(0, [ASimpleCont2Instr(312,__binder0,49);ASimpleCont2Instr(311,__binder0,48);ASimpleCont2Instr(310,__binder0,47);ASimpleCont2Instr(309,__binder0,46);ASimpleCont2Instr(308,__binder0,45);ASimpleCont2Instr(307,__binder0,44);ASimpleCont2Instr(306,__binder0,43);ASimpleCont2Instr(305,__binder0,42);ASimpleCont2Instr(304,__binder0,41);ASimpleCont2Instr(303,__binder0,40);ASimpleCont2Instr(302,__binder0,39);ASimpleCont2Instr(301,__binder0,38);ASimpleCont2Instr(300,__binder0,37);ASimpleCont2Instr(299,__binder0,36);ASimpleCont2Instr(298,__binder0,35);ASimpleCont2Instr(297,__binder0,34);ASimpleCont2Instr(296,__binder0,33);ASimpleCont2Instr(295,__binder0,32);ASimpleCont2Instr(294,__binder0,31);ASimpleCont2Instr(293,__binder0,30);ASimpleCont2Instr(292,__binder0,29);ASimpleCont2Instr(291,__binder0,28);ASimpleCont2Instr(290,__binder0,27);ASimpleCont2Instr(289,__binder0,26);ASimpleCont2Instr(288,__binder0,25);ASimpleCont2Instr(287,__binder0,24);ASimpleCont2Instr(286,__binder0,23);ASimpleCont2Instr(285,__binder0,22);ASimpleCont2Instr(284,__binder0,21);ASimpleCont2Instr(283,__binder0,20);ASimpleCont2Instr(282,__binder0,19);ASimpleCont2Instr(281,__binder0,18);ASimpleCont2Instr(280,__binder0,17);ASimpleCont2Instr(279,__binder0,16);ASimpleCont2Instr(278,__binder0,15);ASimpleCont2Instr(277,__binder0,14);ASimpleCont2Instr(276,__binder0,13);ASimpleCont2Instr(275,__binder0,12);ASimpleCont2Instr(274,__binder0,11);ASimpleCont2Instr(273,__binder0,10);ASimpleCont2Instr(272,__binder0,9);ASimpleCont2Instr(271,__binder0,8);ASimpleCont2Instr(270,__binder0,7);ASimpleCont2Instr(269,__binder0,6);ASimpleCont2Instr(268,__binder0,5);ASimpleCont2Instr(267,__binder0,4);ASimpleCont2Instr(266,__binder0,3);ASimpleCont2Instr(265,__binder0,2);ASimpleCont2Instr(264,__binder0,1)]);
(192, [AContInstr3(286,__g65,__binder13,199);ACallInstr3(__g65,23)]);
(1, [EatInstr(122,50);EatInstr(121,50);EatInstr(120,50);EatInstr(119,50);EatInstr(118,50);EatInstr(117,50);EatInstr(116,50);EatInstr(115,50);EatInstr(114,50);EatInstr(113,50);EatInstr(112,50);EatInstr(111,50);EatInstr(110,50);EatInstr(109,50);EatInstr(108,50);EatInstr(107,50);EatInstr(106,50);EatInstr(105,50);EatInstr(104,50);EatInstr(103,50);EatInstr(102,50);EatInstr(101,50);EatInstr(100,50);EatInstr(99,50);EatInstr(98,50);EatInstr(97,50);EatInstr(90,50);EatInstr(89,50);EatInstr(88,50);EatInstr(87,50);EatInstr(86,50);EatInstr(85,50);EatInstr(84,50);EatInstr(83,50);EatInstr(82,50);EatInstr(81,50);EatInstr(80,50);EatInstr(79,50);EatInstr(78,50);EatInstr(77,50);EatInstr(76,50);EatInstr(75,50);EatInstr(74,50);EatInstr(73,50);EatInstr(72,50);EatInstr(71,50);EatInstr(70,50);EatInstr(69,50);EatInstr(68,50);EatInstr(67,50);EatInstr(66,50);EatInstr(65,50)]);
(193, [CompleteInstr(284)]);
(2, [EatInstr(49,51);EatInstr(48,51)]);
(194, [AWhenInstr3(__p67,__p66,200)]);
(3, [EatInstr(127,52);EatInstr(126,52);EatInstr(125,52);EatInstr(124,52);EatInstr(123,52);EatInstr(96,52);EatInstr(95,52);EatInstr(94,52);EatInstr(93,52);EatInstr(92,52);EatInstr(91,52);EatInstr(64,52);EatInstr(63,52);EatInstr(62,52);EatInstr(61,52);EatInstr(60,52);EatInstr(59,52);EatInstr(58,52);EatInstr(57,52);EatInstr(56,52);EatInstr(55,52);EatInstr(54,52);EatInstr(53,52);EatInstr(52,52);EatInstr(51,52);EatInstr(50,52);EatInstr(47,52);EatInstr(46,52);EatInstr(45,52);EatInstr(44,52);EatInstr(43,52);EatInstr(42,52);EatInstr(41,52);EatInstr(40,52);EatInstr(39,52);EatInstr(38,52);EatInstr(37,52);EatInstr(36,52);EatInstr(35,52);EatInstr(34,52);EatInstr(33,52);EatInstr(32,52);EatInstr(31,52);EatInstr(30,52);EatInstr(29,52);EatInstr(28,52);EatInstr(27,52);EatInstr(26,52);EatInstr(25,52);EatInstr(24,52);EatInstr(23,52);EatInstr(22,52);EatInstr(21,52);EatInstr(20,52);EatInstr(19,52);EatInstr(18,52);EatInstr(17,52);EatInstr(16,52);EatInstr(15,52);EatInstr(14,52);EatInstr(13,52);EatInstr(12,52);EatInstr(11,52);EatInstr(10,52);EatInstr(9,52);EatInstr(8,52);EatInstr(7,52);EatInstr(6,52);EatInstr(5,52);EatInstr(4,52);EatInstr(3,52);EatInstr(2,52);EatInstr(1,52);EatInstr(49,52);EatInstr(48,52);EatInstr(122,52);EatInstr(121,52);EatInstr(120,52);EatInstr(119,52);EatInstr(118,52);EatInstr(117,52);EatInstr(116,52);EatInstr(115,52);EatInstr(114,52);EatInstr(113,52);EatInstr(112,52);EatInstr(111,52);EatInstr(110,52);EatInstr(109,52);EatInstr(108,52);EatInstr(107,52);EatInstr(106,52);EatInstr(105,52);EatInstr(104,52);EatInstr(103,52);EatInstr(102,52);EatInstr(101,52);EatInstr(100,52);EatInstr(99,52);EatInstr(98,52);EatInstr(97,52);EatInstr(90,52);EatInstr(89,52);EatInstr(88,52);EatInstr(87,52);EatInstr(86,52);EatInstr(85,52);EatInstr(84,52);EatInstr(83,52);EatInstr(82,52);EatInstr(81,52);EatInstr(80,52);EatInstr(79,52);EatInstr(78,52);EatInstr(77,52);EatInstr(76,52);EatInstr(75,52);EatInstr(74,52);EatInstr(73,52);EatInstr(72,52);EatInstr(71,52);EatInstr(70,52);EatInstr(69,52);EatInstr(68,52);EatInstr(67,52);EatInstr(66,52);EatInstr(65,52)]);
(195, [AAction2Instr(__a68,201)]);
(4, [EatInstr(13,53)]);
(196, [AContInstr3(297,__g69,__binder14,155);ACallInstr3(__g69,34)]);
(5, [EatInstr(57,54);EatInstr(56,54);EatInstr(55,54);EatInstr(54,54);EatInstr(53,54);EatInstr(52,54);EatInstr(51,54);EatInstr(50,54);EatInstr(49,54);EatInstr(48,54)]);
(197, [AContInstr3(295,__g70,__binder15,202);ACallInstr3(__g70,32)]);
(6, [EatInstr(34,55)]);
(198, [AContInstr3(295,__g71,__binder16,203);ACallInstr3(__g71,32)]);
(7, [EatInstr(9,56)]);
(199, [ACallInstr3(__default_call,19);ASimpleCont2Instr(282,__binder0,204)]);
(8, [EatInstr(10,57)]);
(200, [CompleteInstr(285)]);
(9, [EatInstr(255,58);EatInstr(254,58);EatInstr(253,58);EatInstr(252,58);EatInstr(251,58);EatInstr(250,58);EatInstr(249,58);EatInstr(248,58);EatInstr(247,58);EatInstr(246,58);EatInstr(245,58);EatInstr(244,58);EatInstr(243,58);EatInstr(242,58);EatInstr(241,58);EatInstr(240,58);EatInstr(239,58);EatInstr(238,58);EatInstr(237,58);EatInstr(236,58);EatInstr(235,58);EatInstr(234,58);EatInstr(233,58);EatInstr(232,58);EatInstr(231,58);EatInstr(230,58);EatInstr(229,58);EatInstr(228,58);EatInstr(227,58);EatInstr(226,58);EatInstr(225,58);EatInstr(224,58);EatInstr(223,58);EatInstr(222,58);EatInstr(221,58);EatInstr(220,58);EatInstr(219,58);EatInstr(218,58);EatInstr(217,58);EatInstr(216,58);EatInstr(215,58);EatInstr(214,58);EatInstr(213,58);EatInstr(212,58);EatInstr(211,58);EatInstr(210,58);EatInstr(209,58);EatInstr(208,58);EatInstr(207,58);EatInstr(206,58);EatInstr(205,58);EatInstr(204,58);EatInstr(203,58);EatInstr(202,58);EatInstr(201,58);EatInstr(200,58);EatInstr(199,58);EatInstr(198,58);EatInstr(197,58);EatInstr(196,58);EatInstr(195,58);EatInstr(194,58);EatInstr(193,58);EatInstr(192,58);EatInstr(191,58);EatInstr(190,58);EatInstr(189,58);EatInstr(188,58);EatInstr(187,58);EatInstr(186,58);EatInstr(185,58);EatInstr(184,58);EatInstr(183,58);EatInstr(182,58);EatInstr(181,58);EatInstr(180,58);EatInstr(179,58);EatInstr(178,58);EatInstr(177,58);EatInstr(176,58);EatInstr(175,58);EatInstr(174,58);EatInstr(173,58);EatInstr(172,58);EatInstr(171,58);EatInstr(170,58);EatInstr(169,58);EatInstr(168,58);EatInstr(167,58);EatInstr(166,58);EatInstr(165,58);EatInstr(164,58);EatInstr(163,58);EatInstr(162,58);EatInstr(161,58);EatInstr(160,58);EatInstr(159,58);EatInstr(158,58);EatInstr(157,58);EatInstr(156,58);EatInstr(155,58);EatInstr(154,58);EatInstr(153,58);EatInstr(152,58);EatInstr(151,58);EatInstr(150,58);EatInstr(149,58);EatInstr(148,58);EatInstr(147,58);EatInstr(146,58);EatInstr(145,58);EatInstr(144,58);EatInstr(143,58);EatInstr(142,58);EatInstr(141,58);EatInstr(140,58);EatInstr(139,58);EatInstr(138,58);EatInstr(137,58);EatInstr(136,58);EatInstr(135,58);EatInstr(134,58);EatInstr(133,58);EatInstr(132,58);EatInstr(131,58);EatInstr(130,58);EatInstr(129,58);EatInstr(128,58);EatInstr(0,58);EatInstr(127,58);EatInstr(126,58);EatInstr(125,58);EatInstr(124,58);EatInstr(123,58);EatInstr(96,58);EatInstr(95,58);EatInstr(94,58);EatInstr(93,58);EatInstr(92,58);EatInstr(91,58);EatInstr(64,58);EatInstr(63,58);EatInstr(62,58);EatInstr(61,58);EatInstr(60,58);EatInstr(59,58);EatInstr(58,58);EatInstr(57,58);EatInstr(56,58);EatInstr(55,58);EatInstr(54,58);EatInstr(53,58);EatInstr(52,58);EatInstr(51,58);EatInstr(50,58);EatInstr(47,58);EatInstr(46,58);EatInstr(45,58);EatInstr(44,58);EatInstr(43,58);EatInstr(42,58);EatInstr(41,58);EatInstr(40,58);EatInstr(39,58);EatInstr(38,58);EatInstr(37,58);EatInstr(36,58);EatInstr(35,58);EatInstr(34,58);EatInstr(33,58);EatInstr(32,58);EatInstr(31,58);EatInstr(30,58);EatInstr(29,58);EatInstr(28,58);EatInstr(27,58);EatInstr(26,58);EatInstr(25,58);EatInstr(24,58);EatInstr(23,58);EatInstr(22,58);EatInstr(21,58);EatInstr(20,58);EatInstr(19,58);EatInstr(18,58);EatInstr(17,58);EatInstr(16,58);EatInstr(15,58);EatInstr(14,58);EatInstr(13,58);EatInstr(12,58);EatInstr(11,58);EatInstr(10,58);EatInstr(9,58);EatInstr(8,58);EatInstr(7,58);EatInstr(6,58);EatInstr(5,58);EatInstr(4,58);EatInstr(3,58);EatInstr(2,58);EatInstr(1,58);EatInstr(49,58);EatInstr(48,58);EatInstr(122,58);EatInstr(121,58);EatInstr(120,58);EatInstr(119,58);EatInstr(118,58);EatInstr(117,58);EatInstr(116,58);EatInstr(115,58);EatInstr(114,58);EatInstr(113,58);EatInstr(112,58);EatInstr(111,58);EatInstr(110,58);EatInstr(109,58);EatInstr(108,58);EatInstr(107,58);EatInstr(106,58);EatInstr(105,58);EatInstr(104,58);EatInstr(103,58);EatInstr(102,58);EatInstr(101,58);EatInstr(100,58);EatInstr(99,58);EatInstr(98,58);EatInstr(97,58);EatInstr(90,58);EatInstr(89,58);EatInstr(88,58);EatInstr(87,58);EatInstr(86,58);EatInstr(85,58);EatInstr(84,58);EatInstr(83,58);EatInstr(82,58);EatInstr(81,58);EatInstr(80,58);EatInstr(79,58);EatInstr(78,58);EatInstr(77,58);EatInstr(76,58);EatInstr(75,58);EatInstr(74,58);EatInstr(73,58);EatInstr(72,58);EatInstr(71,58);EatInstr(70,58);EatInstr(69,58);EatInstr(68,58);EatInstr(67,58);EatInstr(66,58);EatInstr(65,58)]);
(201, [AContInstr3(286,__g72,__binder17,205);ACallInstr3(__g72,23)]);
(10, [EatInstr(32,59)]);
(202, [AContInstr3(286,__g73,__binder18,206);ACallInstr3(__g73,23)]);
(11, [EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(47,60);EatInstr(46,60);EatInstr(45,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(49,60);EatInstr(48,60);EatInstr(122,60);EatInstr(121,60);EatInstr(120,60);EatInstr(119,60);EatInstr(118,60);EatInstr(117,60);EatInstr(116,60);EatInstr(115,60);EatInstr(114,60);EatInstr(113,60);EatInstr(112,60);EatInstr(111,60);EatInstr(110,60);EatInstr(109,60);EatInstr(108,60);EatInstr(107,60);EatInstr(106,60);EatInstr(105,60);EatInstr(104,60);EatInstr(103,60);EatInstr(102,60);EatInstr(101,60);EatInstr(100,60);EatInstr(99,60);EatInstr(98,60);EatInstr(97,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60)]);
(203, [AContInstr3(286,__g74,__binder19,207);ACallInstr3(__g74,23)]);
(12, [EatInstr(32,59);EatInstr(9,56);ASimpleCont2Instr(273,__binder0,61);ASimpleCont2Instr(270,__binder0,61)]);
(204, [AAction2Instr(__a75,208)]);
(13, [RCompleteInstr2(276,nullable_rfc);AAction2Instr(__a0,62)]);
(205, [AContInstr3(295,__g76,__binder20,154);ACallInstr3(__g76,32)]);
(14, [EatInstr(127,65);EatInstr(126,65);EatInstr(125,65);EatInstr(124,65);EatInstr(123,65);EatInstr(96,65);EatInstr(95,65);EatInstr(94,65);EatInstr(93,65);EatInstr(92,65);EatInstr(91,65);EatInstr(64,65);EatInstr(63,65);EatInstr(62,65);EatInstr(61,65);EatInstr(60,65);EatInstr(59,65);EatInstr(58,65);EatInstr(57,65);EatInstr(56,65);EatInstr(55,65);EatInstr(54,65);EatInstr(53,65);EatInstr(52,65);EatInstr(51,65);EatInstr(50,65);EatInstr(47,65);EatInstr(46,65);EatInstr(45,65);EatInstr(44,65);EatInstr(43,65);EatInstr(42,65);EatInstr(41,65);EatInstr(40,65);EatInstr(39,65);EatInstr(38,65);EatInstr(37,65);EatInstr(36,65);EatInstr(35,65);EatInstr(34,65);EatInstr(33,65);EatInstr(32,65);EatInstr(31,65);EatInstr(30,65);EatInstr(29,65);EatInstr(28,65);EatInstr(27,65);EatInstr(26,65);EatInstr(25,65);EatInstr(24,65);EatInstr(23,65);EatInstr(22,65);EatInstr(21,65);EatInstr(20,65);EatInstr(19,65);EatInstr(18,65);EatInstr(17,65);EatInstr(16,65);EatInstr(15,65);EatInstr(14,65);EatInstr(12,65);EatInstr(11,65);EatInstr(9,65);EatInstr(8,65);EatInstr(7,65);EatInstr(6,65);EatInstr(5,65);EatInstr(4,65);EatInstr(3,65);EatInstr(2,65);EatInstr(1,65);EatInstr(49,65);EatInstr(48,65);EatInstr(122,65);EatInstr(121,65);EatInstr(120,65);EatInstr(119,65);EatInstr(118,65);EatInstr(117,65);EatInstr(116,65);EatInstr(115,65);EatInstr(114,65);EatInstr(113,65);EatInstr(112,65);EatInstr(111,65);EatInstr(110,65);EatInstr(109,65);EatInstr(108,65);EatInstr(107,65);EatInstr(106,65);EatInstr(105,65);EatInstr(104,65);EatInstr(103,65);EatInstr(102,65);EatInstr(101,65);EatInstr(100,65);EatInstr(99,65);EatInstr(98,65);EatInstr(97,65);EatInstr(90,65);EatInstr(89,65);EatInstr(88,65);EatInstr(87,65);EatInstr(86,65);EatInstr(85,65);EatInstr(84,65);EatInstr(83,65);EatInstr(82,65);EatInstr(81,65);EatInstr(80,65);EatInstr(79,65);EatInstr(78,65);EatInstr(77,65);EatInstr(76,65);EatInstr(75,65);EatInstr(74,65);EatInstr(73,65);EatInstr(72,65);EatInstr(71,65);EatInstr(70,65);EatInstr(69,65);EatInstr(68,65);EatInstr(67,65);EatInstr(66,65);EatInstr(65,65);ALookaheadInstr(false,CfgLA (15,278),66);RCompleteInstr2(277,nullable_line);ASimpleCont2Instr(278,__binder0,64)]);
(206, [EatInstr(41,209)]);
(15, [EatInstr(127,65);EatInstr(126,65);EatInstr(125,65);EatInstr(124,65);EatInstr(123,65);EatInstr(96,65);EatInstr(95,65);EatInstr(94,65);EatInstr(93,65);EatInstr(92,65);EatInstr(91,65);EatInstr(64,65);EatInstr(63,65);EatInstr(62,65);EatInstr(61,65);EatInstr(60,65);EatInstr(59,65);EatInstr(58,65);EatInstr(57,65);EatInstr(56,65);EatInstr(55,65);EatInstr(54,65);EatInstr(53,65);EatInstr(52,65);EatInstr(51,65);EatInstr(50,65);EatInstr(47,65);EatInstr(46,65);EatInstr(45,65);EatInstr(44,65);EatInstr(43,65);EatInstr(42,65);EatInstr(41,65);EatInstr(40,65);EatInstr(39,65);EatInstr(38,65);EatInstr(37,65);EatInstr(36,65);EatInstr(35,65);EatInstr(34,65);EatInstr(33,65);EatInstr(32,65);EatInstr(31,65);EatInstr(30,65);EatInstr(29,65);EatInstr(28,65);EatInstr(27,65);EatInstr(26,65);EatInstr(25,65);EatInstr(24,65);EatInstr(23,65);EatInstr(22,65);EatInstr(21,65);EatInstr(20,65);EatInstr(19,65);EatInstr(18,65);EatInstr(17,65);EatInstr(16,65);EatInstr(15,65);EatInstr(14,65);EatInstr(12,65);EatInstr(11,65);EatInstr(9,65);EatInstr(8,65);EatInstr(7,65);EatInstr(6,65);EatInstr(5,65);EatInstr(4,65);EatInstr(3,65);EatInstr(2,65);EatInstr(1,65);EatInstr(49,65);EatInstr(48,65);EatInstr(122,65);EatInstr(121,65);EatInstr(120,65);EatInstr(119,65);EatInstr(118,65);EatInstr(117,65);EatInstr(116,65);EatInstr(115,65);EatInstr(114,65);EatInstr(113,65);EatInstr(112,65);EatInstr(111,65);EatInstr(110,65);EatInstr(109,65);EatInstr(108,65);EatInstr(107,65);EatInstr(106,65);EatInstr(105,65);EatInstr(104,65);EatInstr(103,65);EatInstr(102,65);EatInstr(101,65);EatInstr(100,65);EatInstr(99,65);EatInstr(98,65);EatInstr(97,65);EatInstr(90,65);EatInstr(89,65);EatInstr(88,65);EatInstr(87,65);EatInstr(86,65);EatInstr(85,65);EatInstr(84,65);EatInstr(83,65);EatInstr(82,65);EatInstr(81,65);EatInstr(80,65);EatInstr(79,65);EatInstr(78,65);EatInstr(77,65);EatInstr(76,65);EatInstr(75,65);EatInstr(74,65);EatInstr(73,65);EatInstr(72,65);EatInstr(71,65);EatInstr(70,65);EatInstr(69,65);EatInstr(68,65);EatInstr(67,65);EatInstr(66,65);EatInstr(65,65)]);
(16, [ALookaheadInstr(false,CfgLA (18,281),67)]);
(207, [EatInstr(93,210)]);
(208, [AContInstr3(286,__g77,__binder21,211);ACallInstr3(__g77,23)]);
(17, [EatInstr(59,68)]);
(209, [CompleteInstr(300)]);
(18, [AAction2Instr(__a1,69)]);
(210, [CompleteInstr(301)]);
(19, [EatInstr(61,70)]);
(211, [AContInstr3(294,__g78,__binder22,212);ACallInstr3(__g78,31)]);
(20, [EatInstr(32,59);EatInstr(9,56);ASimpleCont2Instr(273,__binder0,71);ASimpleCont2Instr(270,__binder0,71)]);
(212, [CompleteInstr(281);ACallInstr3(__default_call,214);ASimpleCont2Instr(283,__binder0,212);ASimpleCont2Instr(280,__binder0,213)]);
(21, [AAction2Instr(__a2,72)]);
(213, [CompleteInstr(281)]);
(22, [AAction2Instr(__a3,73)]);
(214, [EatInstr(59,68);EatInstr(32,59);EatInstr(9,56);ASimpleCont2Instr(273,__binder0,71);ASimpleCont2Instr(270,__binder0,71)]);
(23, [RCompleteInstr2(286,nullable_o);AAction2Instr(__a4,74)]);
(24, [EatInstr(59,76);EatInstr(13,53);EatInstr(10,57);ASimpleCont2Instr(288,__binder0,75);ASimpleCont2Instr(271,__binder0,75);ASimpleCont2Instr(267,__binder0,75)]);
(25, [EatInstr(59,76)]);
(26, [EatInstr(92,77)]);
(27, [EatInstr(34,55);ASimpleCont2Instr(269,__binder0,78)]);
(28, [EatInstr(255,80);EatInstr(254,80);EatInstr(253,80);EatInstr(252,80);EatInstr(251,80);EatInstr(250,80);EatInstr(249,80);EatInstr(248,80);EatInstr(247,80);EatInstr(246,80);EatInstr(245,80);EatInstr(244,80);EatInstr(243,80);EatInstr(242,80);EatInstr(241,80);EatInstr(240,80);EatInstr(239,80);EatInstr(238,80);EatInstr(237,80);EatInstr(236,80);EatInstr(235,80);EatInstr(234,80);EatInstr(233,80);EatInstr(232,80);EatInstr(231,80);EatInstr(230,80);EatInstr(229,80);EatInstr(228,80);EatInstr(227,80);EatInstr(226,80);EatInstr(225,80);EatInstr(224,80);EatInstr(223,80);EatInstr(222,80);EatInstr(221,80);EatInstr(220,80);EatInstr(219,80);EatInstr(218,80);EatInstr(217,80);EatInstr(216,80);EatInstr(215,80);EatInstr(214,80);EatInstr(213,80);EatInstr(212,80);EatInstr(211,80);EatInstr(210,80);EatInstr(209,80);EatInstr(208,80);EatInstr(207,80);EatInstr(206,80);EatInstr(205,80);EatInstr(204,80);EatInstr(203,80);EatInstr(202,80);EatInstr(201,80);EatInstr(200,80);EatInstr(199,80);EatInstr(198,80);EatInstr(197,80);EatInstr(196,80);EatInstr(195,80);EatInstr(194,80);EatInstr(193,80);EatInstr(192,80);EatInstr(191,80);EatInstr(190,80);EatInstr(189,80);EatInstr(188,80);EatInstr(187,80);EatInstr(186,80);EatInstr(185,80);EatInstr(184,80);EatInstr(183,80);EatInstr(182,80);EatInstr(181,80);EatInstr(180,80);EatInstr(179,80);EatInstr(178,80);EatInstr(177,80);EatInstr(176,80);EatInstr(175,80);EatInstr(174,80);EatInstr(173,80);EatInstr(172,80);EatInstr(171,80);EatInstr(170,80);EatInstr(169,80);EatInstr(168,80);EatInstr(167,80);EatInstr(166,80);EatInstr(165,80);EatInstr(164,80);EatInstr(163,80);EatInstr(162,80);EatInstr(161,80);EatInstr(160,80);EatInstr(159,80);EatInstr(158,80);EatInstr(157,80);EatInstr(156,80);EatInstr(155,80);EatInstr(154,80);EatInstr(153,80);EatInstr(152,80);EatInstr(151,80);EatInstr(150,80);EatInstr(149,80);EatInstr(148,80);EatInstr(147,80);EatInstr(146,80);EatInstr(145,80);EatInstr(144,80);EatInstr(143,80);EatInstr(142,80);EatInstr(141,80);EatInstr(140,80);EatInstr(139,80);EatInstr(138,80);EatInstr(137,80);EatInstr(136,80);EatInstr(135,80);EatInstr(134,80);EatInstr(133,80);EatInstr(132,80);EatInstr(131,80);EatInstr(130,80);EatInstr(129,80);EatInstr(128,80);EatInstr(0,80);EatInstr(127,80);EatInstr(126,80);EatInstr(125,80);EatInstr(124,80);EatInstr(123,80);EatInstr(96,80);EatInstr(95,80);EatInstr(94,80);EatInstr(93,80);EatInstr(92,77);EatInstr(91,80);EatInstr(64,80);EatInstr(63,80);EatInstr(62,80);EatInstr(61,80);EatInstr(60,80);EatInstr(59,80);EatInstr(58,80);EatInstr(57,80);EatInstr(56,80);EatInstr(55,80);EatInstr(54,80);EatInstr(53,80);EatInstr(52,80);EatInstr(51,80);EatInstr(50,80);EatInstr(47,80);EatInstr(46,80);EatInstr(45,80);EatInstr(44,80);EatInstr(43,80);EatInstr(42,80);EatInstr(41,80);EatInstr(40,80);EatInstr(39,80);EatInstr(38,80);EatInstr(37,80);EatInstr(36,80);EatInstr(35,80);EatInstr(33,80);EatInstr(32,80);EatInstr(31,80);EatInstr(30,80);EatInstr(29,80);EatInstr(28,80);EatInstr(27,80);EatInstr(26,80);EatInstr(25,80);EatInstr(24,80);EatInstr(23,80);EatInstr(22,80);EatInstr(21,80);EatInstr(20,80);EatInstr(19,80);EatInstr(18,80);EatInstr(17,80);EatInstr(16,80);EatInstr(15,80);EatInstr(14,80);EatInstr(13,80);EatInstr(12,80);EatInstr(11,80);EatInstr(10,80);EatInstr(9,80);EatInstr(8,80);EatInstr(7,80);EatInstr(6,80);EatInstr(5,80);EatInstr(4,80);EatInstr(3,80);EatInstr(2,80);EatInstr(1,80);EatInstr(49,80);EatInstr(48,80);EatInstr(122,80);EatInstr(121,80);EatInstr(120,80);EatInstr(119,80);EatInstr(118,80);EatInstr(117,80);EatInstr(116,80);EatInstr(115,80);EatInstr(114,80);EatInstr(113,80);EatInstr(112,80);EatInstr(111,80);EatInstr(110,80);EatInstr(109,80);EatInstr(108,80);EatInstr(107,80);EatInstr(106,80);EatInstr(105,80);EatInstr(104,80);EatInstr(103,80);EatInstr(102,80);EatInstr(101,80);EatInstr(100,80);EatInstr(99,80);EatInstr(98,80);EatInstr(97,80);EatInstr(90,80);EatInstr(89,80);EatInstr(88,80);EatInstr(87,80);EatInstr(86,80);EatInstr(85,80);EatInstr(84,80);EatInstr(83,80);EatInstr(82,80);EatInstr(81,80);EatInstr(80,80);EatInstr(79,80);EatInstr(78,80);EatInstr(77,80);EatInstr(76,80);EatInstr(75,80);EatInstr(74,80);EatInstr(73,80);EatInstr(72,80);EatInstr(71,80);EatInstr(70,80);EatInstr(69,80);EatInstr(68,80);EatInstr(67,80);EatInstr(66,80);EatInstr(65,80);ASimpleCont2Instr(289,__binder0,79)]);
(29, [EatInstr(58,81);EatInstr(57,54);EatInstr(56,54);EatInstr(55,54);EatInstr(54,54);EatInstr(53,54);EatInstr(52,54);EatInstr(51,54);EatInstr(50,54);EatInstr(45,81);EatInstr(49,54);EatInstr(48,54);EatInstr(122,50);EatInstr(121,50);EatInstr(120,50);EatInstr(119,50);EatInstr(118,50);EatInstr(117,50);EatInstr(116,50);EatInstr(115,50);EatInstr(114,50);EatInstr(113,50);EatInstr(112,50);EatInstr(111,50);EatInstr(110,50);EatInstr(109,50);EatInstr(108,50);EatInstr(107,50);EatInstr(106,50);EatInstr(105,50);EatInstr(104,50);EatInstr(103,50);EatInstr(102,50);EatInstr(101,50);EatInstr(100,50);EatInstr(99,50);EatInstr(98,50);EatInstr(97,50);EatInstr(90,50);EatInstr(89,50);EatInstr(88,50);EatInstr(87,50);EatInstr(86,50);EatInstr(85,50);EatInstr(84,50);EatInstr(83,50);EatInstr(82,50);EatInstr(81,50);EatInstr(80,50);EatInstr(79,50);EatInstr(78,50);EatInstr(77,50);EatInstr(76,50);EatInstr(75,50);EatInstr(74,50);EatInstr(73,50);EatInstr(72,50);EatInstr(71,50);EatInstr(70,50);EatInstr(69,50);EatInstr(68,50);EatInstr(67,50);EatInstr(66,50);EatInstr(65,50);ASimpleCont2Instr(268,__binder0,81);ASimpleCont2Instr(264,__binder0,81)]);
(30, [EatInstr(122,50);EatInstr(121,50);EatInstr(120,50);EatInstr(119,50);EatInstr(118,50);EatInstr(117,50);EatInstr(116,50);EatInstr(115,50);EatInstr(114,50);EatInstr(113,50);EatInstr(112,50);EatInstr(111,50);EatInstr(110,50);EatInstr(109,50);EatInstr(108,50);EatInstr(107,50);EatInstr(106,50);EatInstr(105,50);EatInstr(104,50);EatInstr(103,50);EatInstr(102,50);EatInstr(101,50);EatInstr(100,50);EatInstr(99,50);EatInstr(98,50);EatInstr(97,50);EatInstr(90,50);EatInstr(89,50);EatInstr(88,50);EatInstr(87,50);EatInstr(86,50);EatInstr(85,50);EatInstr(84,50);EatInstr(83,50);EatInstr(82,50);EatInstr(81,50);EatInstr(80,50);EatInstr(79,50);EatInstr(78,50);EatInstr(77,50);EatInstr(76,50);EatInstr(75,50);EatInstr(74,50);EatInstr(73,50);EatInstr(72,50);EatInstr(71,50);EatInstr(70,50);EatInstr(69,50);EatInstr(68,50);EatInstr(67,50);EatInstr(66,50);EatInstr(65,50);ASimpleCont2Instr(264,__binder0,82)]);
(31, [AAction2Instr(__a5,83)]);
(32, [AAction2Instr(__a6,84)]);
(33, [AAction2Instr(__a7,85)]);
(34, [AAction2Instr(__a8,86)]);
(35, [EatInstr(42,88);EatInstr(35,88);RCompleteInstr2(304,nullable_DIGITS);AAction2Instr(__a9,89);RCompleteInstr2(298,nullable_repeat);ASimpleCont2Instr(304,__binder0,87)]);
(36, [AAction2Instr(__a10,90)]);
(37, [AAction2Instr(__a11,91)]);
(38, [AAction2Instr(__a12,92)]);
(39, [RCompleteInstr2(302,nullable_bitstring);AAction2Instr(__a13,93)]);
(40, [EatInstr(98,94)]);
(41, [RCompleteInstr2(304,nullable_DIGITS);AAction2Instr(__a9,89)]);
(42, [EatInstr(100,95)]);
(43, [EatInstr(57,54);EatInstr(56,54);EatInstr(55,54);EatInstr(54,54);EatInstr(53,54);EatInstr(52,54);EatInstr(51,54);EatInstr(50,54);EatInstr(49,54);EatInstr(48,54);EatInstr(102,96);EatInstr(101,96);EatInstr(100,96);EatInstr(99,96);EatInstr(98,96);EatInstr(97,96);EatInstr(70,96);EatInstr(69,96);EatInstr(68,96);EatInstr(67,96);EatInstr(66,96);EatInstr(65,96);ASimpleCont2Instr(268,__binder0,96)]);
(44, [RCompleteInstr2(307,nullable_HEXDIGS);AAction2Instr(__a14,97)]);
(45, [EatInstr(120,98)]);
(46, [EatInstr(37,99)]);
(47, [EatInstr(60,101);EatInstr(34,55);ASimpleCont2Instr(269,__binder0,100)]);
(48, [EatInstr(126,102);EatInstr(125,102);EatInstr(124,102);EatInstr(123,102);EatInstr(96,102);EatInstr(95,102);EatInstr(94,102);EatInstr(93,102);EatInstr(92,102);EatInstr(91,102);EatInstr(64,102);EatInstr(63,102);EatInstr(61,102);EatInstr(60,102);EatInstr(59,102);EatInstr(58,102);EatInstr(57,102);EatInstr(56,102);EatInstr(55,102);EatInstr(54,102);EatInstr(53,102);EatInstr(52,102);EatInstr(51,102);EatInstr(50,102);EatInstr(47,102);EatInstr(46,102);EatInstr(45,102);EatInstr(44,102);EatInstr(43,102);EatInstr(42,102);EatInstr(41,102);EatInstr(40,102);EatInstr(39,102);EatInstr(38,102);EatInstr(37,102);EatInstr(36,102);EatInstr(35,102);EatInstr(34,102);EatInstr(33,102);EatInstr(32,102);EatInstr(49,102);EatInstr(48,102);EatInstr(122,102);EatInstr(121,102);EatInstr(120,102);EatInstr(119,102);EatInstr(118,102);EatInstr(117,102);EatInstr(116,102);EatInstr(115,102);EatInstr(114,102);EatInstr(113,102);EatInstr(112,102);EatInstr(111,102);EatInstr(110,102);EatInstr(109,102);EatInstr(108,102);EatInstr(107,102);EatInstr(106,102);EatInstr(105,102);EatInstr(104,102);EatInstr(103,102);EatInstr(102,102);EatInstr(101,102);EatInstr(100,102);EatInstr(99,102);EatInstr(98,102);EatInstr(97,102);EatInstr(90,102);EatInstr(89,102);EatInstr(88,102);EatInstr(87,102);EatInstr(86,102);EatInstr(85,102);EatInstr(84,102);EatInstr(83,102);EatInstr(82,102);EatInstr(81,102);EatInstr(80,102);EatInstr(79,102);EatInstr(78,102);EatInstr(77,102);EatInstr(76,102);EatInstr(75,102);EatInstr(74,102);EatInstr(73,102);EatInstr(72,102);EatInstr(71,102);EatInstr(70,102);EatInstr(69,102);EatInstr(68,102);EatInstr(67,102);EatInstr(66,102);EatInstr(65,102)]);
(49, [EatInstr(60,103)]);
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
(62, [ACallInstr3(__default_call,14);ASimpleCont2Instr(277,__binder0,104)]);
(64, [ALookaheadInstr(false,CfgLA (15,278),66);ACallInstr3(__default_call,15);ASimpleCont2Instr(278,__binder0,64)]);
(65, [CompleteInstr(278)]);
(66, [CompleteInstr(277)]);
(67, [ACallInstr3(__default_call,106);ASimpleCont2Instr(271,__binder0,105);ASimpleCont2Instr(267,__binder0,105)]);
(68, [CompleteInstr(280);ACallInstr3(__default_call,107);ASimpleCont2Instr(275,__binder0,68);ASimpleCont2Instr(274,__binder0,68)]);
(69, [ASimpleCont2Instr(284,__binder1,108);ACallInstr3(__default_call,21)]);
(70, [EatInstr(47,109);CompleteInstr(282)]);
(71, [CompleteInstr(283)]);
(72, [ACallInstr3(__default_call,106);ASimpleCont2Instr(271,__binder0,110);ASimpleCont2Instr(267,__binder0,110)]);
(73, [ACallInstr3(__default_call,24);ASimpleCont2Instr(287,__binder0,111)]);
(74, [AAction2Instr(__a16,114);AContInstr3(285,__g15,__binder2,114);ACallInstr3(__g15,22);ACallInstr3(__default_call,113);ASimpleCont2Instr(287,__binder0,112);ASimpleCont2Instr(283,__binder0,74)]);
(75, [CompleteInstr(287)]);
(76, [ACallInstr3(__default_call,116);ASimpleCont2Instr(275,__binder0,76);ASimpleCont2Instr(274,__binder0,76);ASimpleCont2Instr(271,__binder0,115);ASimpleCont2Instr(267,__binder0,115)]);
(77, [CompleteInstr(289)]);
(78, [ACallInstr3(__default_call,118);ASimpleCont2Instr(291,__binder0,78);ASimpleCont2Instr(269,__binder0,117)]);
(79, [EatInstr(255,80);EatInstr(254,80);EatInstr(253,80);EatInstr(252,80);EatInstr(251,80);EatInstr(250,80);EatInstr(249,80);EatInstr(248,80);EatInstr(247,80);EatInstr(246,80);EatInstr(245,80);EatInstr(244,80);EatInstr(243,80);EatInstr(242,80);EatInstr(241,80);EatInstr(240,80);EatInstr(239,80);EatInstr(238,80);EatInstr(237,80);EatInstr(236,80);EatInstr(235,80);EatInstr(234,80);EatInstr(233,80);EatInstr(232,80);EatInstr(231,80);EatInstr(230,80);EatInstr(229,80);EatInstr(228,80);EatInstr(227,80);EatInstr(226,80);EatInstr(225,80);EatInstr(224,80);EatInstr(223,80);EatInstr(222,80);EatInstr(221,80);EatInstr(220,80);EatInstr(219,80);EatInstr(218,80);EatInstr(217,80);EatInstr(216,80);EatInstr(215,80);EatInstr(214,80);EatInstr(213,80);EatInstr(212,80);EatInstr(211,80);EatInstr(210,80);EatInstr(209,80);EatInstr(208,80);EatInstr(207,80);EatInstr(206,80);EatInstr(205,80);EatInstr(204,80);EatInstr(203,80);EatInstr(202,80);EatInstr(201,80);EatInstr(200,80);EatInstr(199,80);EatInstr(198,80);EatInstr(197,80);EatInstr(196,80);EatInstr(195,80);EatInstr(194,80);EatInstr(193,80);EatInstr(192,80);EatInstr(191,80);EatInstr(190,80);EatInstr(189,80);EatInstr(188,80);EatInstr(187,80);EatInstr(186,80);EatInstr(185,80);EatInstr(184,80);EatInstr(183,80);EatInstr(182,80);EatInstr(181,80);EatInstr(180,80);EatInstr(179,80);EatInstr(178,80);EatInstr(177,80);EatInstr(176,80);EatInstr(175,80);EatInstr(174,80);EatInstr(173,80);EatInstr(172,80);EatInstr(171,80);EatInstr(170,80);EatInstr(169,80);EatInstr(168,80);EatInstr(167,80);EatInstr(166,80);EatInstr(165,80);EatInstr(164,80);EatInstr(163,80);EatInstr(162,80);EatInstr(161,80);EatInstr(160,80);EatInstr(159,80);EatInstr(158,80);EatInstr(157,80);EatInstr(156,80);EatInstr(155,80);EatInstr(154,80);EatInstr(153,80);EatInstr(152,80);EatInstr(151,80);EatInstr(150,80);EatInstr(149,80);EatInstr(148,80);EatInstr(147,80);EatInstr(146,80);EatInstr(145,80);EatInstr(144,80);EatInstr(143,80);EatInstr(142,80);EatInstr(141,80);EatInstr(140,80);EatInstr(139,80);EatInstr(138,80);EatInstr(137,80);EatInstr(136,80);EatInstr(135,80);EatInstr(134,80);EatInstr(133,80);EatInstr(132,80);EatInstr(131,80);EatInstr(130,80);EatInstr(129,80);EatInstr(128,80);EatInstr(0,80);EatInstr(127,80);EatInstr(126,80);EatInstr(125,80);EatInstr(124,80);EatInstr(123,80);EatInstr(96,80);EatInstr(95,80);EatInstr(94,80);EatInstr(93,80);EatInstr(91,80);EatInstr(64,80);EatInstr(63,80);EatInstr(62,80);EatInstr(61,80);EatInstr(60,80);EatInstr(59,80);EatInstr(58,80);EatInstr(57,80);EatInstr(56,80);EatInstr(55,80);EatInstr(54,80);EatInstr(53,80);EatInstr(52,80);EatInstr(51,80);EatInstr(50,80);EatInstr(47,80);EatInstr(46,80);EatInstr(45,80);EatInstr(44,80);EatInstr(43,80);EatInstr(42,80);EatInstr(41,80);EatInstr(40,80);EatInstr(39,80);EatInstr(38,80);EatInstr(37,80);EatInstr(36,80);EatInstr(35,80);EatInstr(33,80);EatInstr(32,80);EatInstr(31,80);EatInstr(30,80);EatInstr(29,80);EatInstr(28,80);EatInstr(27,80);EatInstr(26,80);EatInstr(25,80);EatInstr(24,80);EatInstr(23,80);EatInstr(22,80);EatInstr(21,80);EatInstr(20,80);EatInstr(19,80);EatInstr(18,80);EatInstr(17,80);EatInstr(16,80);EatInstr(15,80);EatInstr(14,80);EatInstr(13,80);EatInstr(12,80);EatInstr(11,80);EatInstr(10,80);EatInstr(9,80);EatInstr(8,80);EatInstr(7,80);EatInstr(6,80);EatInstr(5,80);EatInstr(4,80);EatInstr(3,80);EatInstr(2,80);EatInstr(1,80);EatInstr(49,80);EatInstr(48,80);EatInstr(122,80);EatInstr(121,80);EatInstr(120,80);EatInstr(119,80);EatInstr(118,80);EatInstr(117,80);EatInstr(116,80);EatInstr(115,80);EatInstr(114,80);EatInstr(113,80);EatInstr(112,80);EatInstr(111,80);EatInstr(110,80);EatInstr(109,80);EatInstr(108,80);EatInstr(107,80);EatInstr(106,80);EatInstr(105,80);EatInstr(104,80);EatInstr(103,80);EatInstr(102,80);EatInstr(101,80);EatInstr(100,80);EatInstr(99,80);EatInstr(98,80);EatInstr(97,80);EatInstr(90,80);EatInstr(89,80);EatInstr(88,80);EatInstr(87,80);EatInstr(86,80);EatInstr(85,80);EatInstr(84,80);EatInstr(83,80);EatInstr(82,80);EatInstr(81,80);EatInstr(80,80);EatInstr(79,80);EatInstr(78,80);EatInstr(77,80);EatInstr(76,80);EatInstr(75,80);EatInstr(74,80);EatInstr(73,80);EatInstr(72,80);EatInstr(71,80);EatInstr(70,80);EatInstr(69,80);EatInstr(68,80);EatInstr(67,80);EatInstr(66,80);EatInstr(65,80);ACallInstr3(__default_call,119);ASimpleCont2Instr(289,__binder0,80);ASimpleCont2Instr(269,__binder0,80)]);
(80, [CompleteInstr(291)]);
(81, [CompleteInstr(292)]);
(82, [ALookaheadInstr(false,CfgLA (29,292),120);ACallInstr3(__default_call,29);ASimpleCont2Instr(292,__binder0,82)]);
(83, [AContInstr3(295,__g17,__binder3,121);ACallInstr3(__g17,32)]);
(84, [AContInstr3(296,__g18,__binder4,122);ACallInstr3(__g18,33)]);
(85, [AContInstr3(297,__g19,__binder5,123);ACallInstr3(__g19,34)]);
(86, [AContInstr3(299,__g20,__binder6,125);ACallInstr3(__g20,36);ACallInstr3(__default_call,35);ASimpleCont2Instr(298,__binder0,124)]);
(87, [EatInstr(42,88);EatInstr(35,88);CompleteInstr(298)]);
(88, [CompleteInstr(298);ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,126)]);
(89, [AAction2Instr(__a21,128);ACallInstr3(__default_call,5);ASimpleCont2Instr(268,__binder0,127)]);
(90, [AAction2Instr(__a25,131);AContInstr3(301,__g24,__binder8,130);ACallInstr3(__g24,38);AContInstr3(300,__g23,__binder7,130);ACallInstr3(__g23,37);AAction2Instr(__a22,129)]);
(91, [EatInstr(40,132)]);
(92, [EatInstr(91,133)]);
(93, [AAction2Instr(__a26,135);ACallInstr3(__default_call,2);ASimpleCont2Instr(265,__binder0,134)]);
(94, [ACallInstr3(__default_call,39);ASimpleCont2Instr(302,__binder0,136)]);
(95, [ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,137)]);
(96, [CompleteInstr(306)]);
(97, [AAction2Instr(__a27,139);ACallInstr3(__default_call,43);ASimpleCont2Instr(306,__binder0,138)]);
(98, [ACallInstr3(__default_call,44);ASimpleCont2Instr(307,__binder0,140)]);
(99, [ACallInstr3(__default_call,142);ASimpleCont2Instr(308,__binder0,141);ASimpleCont2Instr(305,__binder0,141);ASimpleCont2Instr(303,__binder0,141)]);
(100, [EatInstr(126,100);EatInstr(125,100);EatInstr(124,100);EatInstr(123,100);EatInstr(96,100);EatInstr(95,100);EatInstr(94,100);EatInstr(93,100);EatInstr(92,100);EatInstr(91,100);EatInstr(64,100);EatInstr(63,100);EatInstr(62,100);EatInstr(61,100);EatInstr(60,100);EatInstr(59,100);EatInstr(58,100);EatInstr(57,100);EatInstr(56,100);EatInstr(55,100);EatInstr(54,100);EatInstr(53,100);EatInstr(52,100);EatInstr(51,100);EatInstr(50,100);EatInstr(47,100);EatInstr(46,100);EatInstr(45,100);EatInstr(44,100);EatInstr(43,100);EatInstr(42,100);EatInstr(41,100);EatInstr(40,100);EatInstr(39,100);EatInstr(38,100);EatInstr(37,100);EatInstr(36,100);EatInstr(35,100);EatInstr(33,100);EatInstr(32,100);EatInstr(49,100);EatInstr(48,100);EatInstr(122,100);EatInstr(121,100);EatInstr(120,100);EatInstr(119,100);EatInstr(118,100);EatInstr(117,100);EatInstr(116,100);EatInstr(115,100);EatInstr(114,100);EatInstr(113,100);EatInstr(112,100);EatInstr(111,100);EatInstr(110,100);EatInstr(109,100);EatInstr(108,100);EatInstr(107,100);EatInstr(106,100);EatInstr(105,100);EatInstr(104,100);EatInstr(103,100);EatInstr(102,100);EatInstr(101,100);EatInstr(100,100);EatInstr(99,100);EatInstr(98,100);EatInstr(97,100);EatInstr(90,100);EatInstr(89,100);EatInstr(88,100);EatInstr(87,100);EatInstr(86,100);EatInstr(85,100);EatInstr(84,100);EatInstr(83,100);EatInstr(82,100);EatInstr(81,100);EatInstr(80,100);EatInstr(79,100);EatInstr(78,100);EatInstr(77,100);EatInstr(76,100);EatInstr(75,100);EatInstr(74,100);EatInstr(73,100);EatInstr(72,100);EatInstr(71,100);EatInstr(70,100);EatInstr(69,100);EatInstr(68,100);EatInstr(67,100);EatInstr(66,100);EatInstr(65,100);ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,143)]);
(101, [ACallInstr3(__default_call,6);ASimpleCont2Instr(269,__binder0,144)]);
(102, [CompleteInstr(311)]);
(103, [EatInstr(126,145);EatInstr(125,145);EatInstr(124,145);EatInstr(123,145);EatInstr(96,145);EatInstr(95,145);EatInstr(94,145);EatInstr(93,145);EatInstr(92,145);EatInstr(91,145);EatInstr(64,145);EatInstr(63,145);EatInstr(62,146);EatInstr(61,145);EatInstr(60,145);EatInstr(59,145);EatInstr(58,145);EatInstr(57,145);EatInstr(56,145);EatInstr(55,145);EatInstr(54,145);EatInstr(53,145);EatInstr(52,145);EatInstr(51,145);EatInstr(50,145);EatInstr(47,145);EatInstr(46,145);EatInstr(45,145);EatInstr(44,145);EatInstr(43,145);EatInstr(42,145);EatInstr(41,145);EatInstr(40,145);EatInstr(39,145);EatInstr(38,145);EatInstr(37,145);EatInstr(36,145);EatInstr(35,145);EatInstr(33,145);EatInstr(32,145);EatInstr(49,145);EatInstr(48,145);EatInstr(122,145);EatInstr(121,145);EatInstr(120,145);EatInstr(119,145);EatInstr(118,145);EatInstr(117,145);EatInstr(116,145);EatInstr(115,145);EatInstr(114,145);EatInstr(113,145);EatInstr(112,145);EatInstr(111,145);EatInstr(110,145);EatInstr(109,145);EatInstr(108,145);EatInstr(107,145);EatInstr(106,145);EatInstr(105,145);EatInstr(104,145);EatInstr(103,145);EatInstr(102,145);EatInstr(101,145);EatInstr(100,145);EatInstr(99,145);EatInstr(98,145);EatInstr(97,145);EatInstr(90,145);EatInstr(89,145);EatInstr(88,145);EatInstr(87,145);EatInstr(86,145);EatInstr(85,145);EatInstr(84,145);EatInstr(83,145);EatInstr(82,145);EatInstr(81,145);EatInstr(80,145);EatInstr(79,145);EatInstr(78,145);EatInstr(77,145);EatInstr(76,145);EatInstr(75,145);EatInstr(74,145);EatInstr(73,145);EatInstr(72,145);EatInstr(71,145);EatInstr(70,145);EatInstr(69,145);EatInstr(68,145);EatInstr(67,145);EatInstr(66,145);EatInstr(65,145)]);
(104, [AAction2Instr(__a28,147)]);
(105, [ACallInstr3(__default_call,14);ASimpleCont2Instr(277,__binder0,148)]);
(106, [EatInstr(13,53);EatInstr(10,57)]);
(107, [EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(47,60);EatInstr(46,60);EatInstr(45,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(32,59);EatInstr(9,56);EatInstr(49,60);EatInstr(48,60);EatInstr(122,60);EatInstr(121,60);EatInstr(120,60);EatInstr(119,60);EatInstr(118,60);EatInstr(117,60);EatInstr(116,60);EatInstr(115,60);EatInstr(114,60);EatInstr(113,60);EatInstr(112,60);EatInstr(111,60);EatInstr(110,60);EatInstr(109,60);EatInstr(108,60);EatInstr(107,60);EatInstr(106,60);EatInstr(105,60);EatInstr(104,60);EatInstr(103,60);EatInstr(102,60);EatInstr(101,60);EatInstr(100,60);EatInstr(99,60);EatInstr(98,60);EatInstr(97,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60);ASimpleCont2Instr(273,__binder0,61);ASimpleCont2Instr(270,__binder0,61)]);
(108, [AAction2Instr(__a29,149)]);
(109, [CompleteInstr(282)]);
(110, [AAction2Instr(__a30,150)]);
(111, [AAction2Instr(__a31,151)]);
(112, [AContInstr3(285,__g15,__binder2,114);ACallInstr3(__g15,22);ACallInstr3(__default_call,113);ASimpleCont2Instr(287,__binder0,112);ASimpleCont2Instr(283,__binder0,112)]);
(113, [EatInstr(59,76);EatInstr(32,59);EatInstr(13,53);EatInstr(10,57);EatInstr(9,56);ASimpleCont2Instr(288,__binder0,75);ASimpleCont2Instr(273,__binder0,71);ASimpleCont2Instr(271,__binder0,75);ASimpleCont2Instr(270,__binder0,71);ASimpleCont2Instr(267,__binder0,75)]);
(114, [ALookaheadInstr(false,CfgLA (20,283),152)]);
(115, [CompleteInstr(288)]);
(116, [EatInstr(126,60);EatInstr(125,60);EatInstr(124,60);EatInstr(123,60);EatInstr(96,60);EatInstr(95,60);EatInstr(94,60);EatInstr(93,60);EatInstr(92,60);EatInstr(91,60);EatInstr(64,60);EatInstr(63,60);EatInstr(62,60);EatInstr(61,60);EatInstr(60,60);EatInstr(59,60);EatInstr(58,60);EatInstr(57,60);EatInstr(56,60);EatInstr(55,60);EatInstr(54,60);EatInstr(53,60);EatInstr(52,60);EatInstr(51,60);EatInstr(50,60);EatInstr(47,60);EatInstr(46,60);EatInstr(45,60);EatInstr(44,60);EatInstr(43,60);EatInstr(42,60);EatInstr(41,60);EatInstr(40,60);EatInstr(39,60);EatInstr(38,60);EatInstr(37,60);EatInstr(36,60);EatInstr(35,60);EatInstr(34,60);EatInstr(33,60);EatInstr(32,59);EatInstr(13,53);EatInstr(10,57);EatInstr(9,56);EatInstr(49,60);EatInstr(48,60);EatInstr(122,60);EatInstr(121,60);EatInstr(120,60);EatInstr(119,60);EatInstr(118,60);EatInstr(117,60);EatInstr(116,60);EatInstr(115,60);EatInstr(114,60);EatInstr(113,60);EatInstr(112,60);EatInstr(111,60);EatInstr(110,60);EatInstr(109,60);EatInstr(108,60);EatInstr(107,60);EatInstr(106,60);EatInstr(105,60);EatInstr(104,60);EatInstr(103,60);EatInstr(102,60);EatInstr(101,60);EatInstr(100,60);EatInstr(99,60);EatInstr(98,60);EatInstr(97,60);EatInstr(90,60);EatInstr(89,60);EatInstr(88,60);EatInstr(87,60);EatInstr(86,60);EatInstr(85,60);EatInstr(84,60);EatInstr(83,60);EatInstr(82,60);EatInstr(81,60);EatInstr(80,60);EatInstr(79,60);EatInstr(78,60);EatInstr(77,60);EatInstr(76,60);EatInstr(75,60);EatInstr(74,60);EatInstr(73,60);EatInstr(72,60);EatInstr(71,60);EatInstr(70,60);EatInstr(69,60);EatInstr(68,60);EatInstr(67,60);EatInstr(66,60);EatInstr(65,60);ASimpleCont2Instr(273,__binder0,61);ASimpleCont2Instr(270,__binder0,61)]);
(117, [CompleteInstr(290)]);
(118, [EatInstr(255,80);EatInstr(254,80);EatInstr(253,80);EatInstr(252,80);EatInstr(251,80);EatInstr(250,80);EatInstr(249,80);EatInstr(248,80);EatInstr(247,80);EatInstr(246,80);EatInstr(245,80);EatInstr(244,80);EatInstr(243,80);EatInstr(242,80);EatInstr(241,80);EatInstr(240,80);EatInstr(239,80);EatInstr(238,80);EatInstr(237,80);EatInstr(236,80);EatInstr(235,80);EatInstr(234,80);EatInstr(233,80);EatInstr(232,80);EatInstr(231,80);EatInstr(230,80);EatInstr(229,80);EatInstr(228,80);EatInstr(227,80);EatInstr(226,80);EatInstr(225,80);EatInstr(224,80);EatInstr(223,80);EatInstr(222,80);EatInstr(221,80);EatInstr(220,80);EatInstr(219,80);EatInstr(218,80);EatInstr(217,80);EatInstr(216,80);EatInstr(215,80);EatInstr(214,80);EatInstr(213,80);EatInstr(212,80);EatInstr(211,80);EatInstr(210,80);EatInstr(209,80);EatInstr(208,80);EatInstr(207,80);EatInstr(206,80);EatInstr(205,80);EatInstr(204,80);EatInstr(203,80);EatInstr(202,80);EatInstr(201,80);EatInstr(200,80);EatInstr(199,80);EatInstr(198,80);EatInstr(197,80);EatInstr(196,80);EatInstr(195,80);EatInstr(194,80);EatInstr(193,80);EatInstr(192,80);EatInstr(191,80);EatInstr(190,80);EatInstr(189,80);EatInstr(188,80);EatInstr(187,80);EatInstr(186,80);EatInstr(185,80);EatInstr(184,80);EatInstr(183,80);EatInstr(182,80);EatInstr(181,80);EatInstr(180,80);EatInstr(179,80);EatInstr(178,80);EatInstr(177,80);EatInstr(176,80);EatInstr(175,80);EatInstr(174,80);EatInstr(173,80);EatInstr(172,80);EatInstr(171,80);EatInstr(170,80);EatInstr(169,80);EatInstr(168,80);EatInstr(167,80);EatInstr(166,80);EatInstr(165,80);EatInstr(164,80);EatInstr(163,80);EatInstr(162,80);EatInstr(161,80);EatInstr(160,80);EatInstr(159,80);EatInstr(158,80);EatInstr(157,80);EatInstr(156,80);EatInstr(155,80);EatInstr(154,80);EatInstr(153,80);EatInstr(152,80);EatInstr(151,80);EatInstr(150,80);EatInstr(149,80);EatInstr(148,80);EatInstr(147,80);EatInstr(146,80);EatInstr(145,80);EatInstr(144,80);EatInstr(143,80);EatInstr(142,80);EatInstr(141,80);EatInstr(140,80);EatInstr(139,80);EatInstr(138,80);EatInstr(137,80);EatInstr(136,80);EatInstr(135,80);EatInstr(134,80);EatInstr(133,80);EatInstr(132,80);EatInstr(131,80);EatInstr(130,80);EatInstr(129,80);EatInstr(128,80);EatInstr(0,80);EatInstr(127,80);EatInstr(126,80);EatInstr(125,80);EatInstr(124,80);EatInstr(123,80);EatInstr(96,80);EatInstr(95,80);EatInstr(94,80);EatInstr(93,80);EatInstr(92,77);EatInstr(91,80);EatInstr(64,80);EatInstr(63,80);EatInstr(62,80);EatInstr(61,80);EatInstr(60,80);EatInstr(59,80);EatInstr(58,80);EatInstr(57,80);EatInstr(56,80);EatInstr(55,80);EatInstr(54,80);EatInstr(53,80);EatInstr(52,80);EatInstr(51,80);EatInstr(50,80);EatInstr(47,80);EatInstr(46,80);EatInstr(45,80);EatInstr(44,80);EatInstr(43,80);EatInstr(42,80);EatInstr(41,80);EatInstr(40,80);EatInstr(39,80);EatInstr(38,80);EatInstr(37,80);EatInstr(36,80);EatInstr(35,80);EatInstr(34,55);EatInstr(33,80);EatInstr(32,80);EatInstr(31,80);EatInstr(30,80);EatInstr(29,80);EatInstr(28,80);EatInstr(27,80);EatInstr(26,80);EatInstr(25,80);EatInstr(24,80);EatInstr(23,80);EatInstr(22,80);EatInstr(21,80);EatInstr(20,80);EatInstr(19,80);EatInstr(18,80);EatInstr(17,80);EatInstr(16,80);EatInstr(15,80);EatInstr(14,80);EatInstr(13,80);EatInstr(12,80);EatInstr(11,80);EatInstr(10,80);EatInstr(9,80);EatInstr(8,80);EatInstr(7,80);EatInstr(6,80);EatInstr(5,80);EatInstr(4,80);EatInstr(3,80);EatInstr(2,80);EatInstr(1,80);EatInstr(49,80);EatInstr(48,80);EatInstr(122,80);EatInstr(121,80);EatInstr(120,80);EatInstr(119,80);EatInstr(118,80);EatInstr(117,80);EatInstr(116,80);EatInstr(115,80);EatInstr(114,80);EatInstr(113,80);EatInstr(112,80);EatInstr(111,80);EatInstr(110,80);EatInstr(109,80);EatInstr(108,80);EatInstr(107,80);EatInstr(106,80);EatInstr(105,80);EatInstr(104,80);EatInstr(103,80);EatInstr(102,80);EatInstr(101,80);EatInstr(100,80);EatInstr(99,80);EatInstr(98,80);EatInstr(97,80);EatInstr(90,80);EatInstr(89,80);EatInstr(88,80);EatInstr(87,80);EatInstr(86,80);EatInstr(85,80);EatInstr(84,80);EatInstr(83,80);EatInstr(82,80);EatInstr(81,80);EatInstr(80,80);EatInstr(79,80);EatInstr(78,80);EatInstr(77,80);EatInstr(76,80);EatInstr(75,80);EatInstr(74,80);EatInstr(73,80);EatInstr(72,80);EatInstr(71,80);EatInstr(70,80);EatInstr(69,80);EatInstr(68,80);EatInstr(67,80);EatInstr(66,80);EatInstr(65,80);ASimpleCont2Instr(289,__binder0,79)]);
(119, [EatInstr(92,77);EatInstr(34,55)]);
(120, [CompleteInstr(293)]);
(121, [CompleteInstr(294)]);
(122, [AAction2Instr(__a33,154);AAction2Instr(__a32,153)]);
(123, [AAction2Instr(__a34,155)]);
(124, [AContInstr3(299,__g20,__binder6,125);ACallInstr3(__g20,36)]);
(125, [CompleteInstr(297)]);
(126, [CompleteInstr(298)]);
(127, [AAction2Instr(__a35,89)]);
(128, [AWhenInstr3(__p37,__p36,156)]);
(129, [ACallInstr3(__default_call,30);ASimpleCont2Instr(293,__binder0,130)]);
(130, [CompleteInstr(299)]);
(131, [AAction2Instr(__a39,158);AAction2Instr(__a38,157)]);
(132, [AAction2Instr(__a40,159)]);
(133, [AAction2Instr(__a41,160)]);
(134, [AAction2Instr(__a42,93)]);
(135, [AWhenInstr3(__p44,__p43,161)]);
(136, [EatInstr(46,163);EatInstr(45,162);CompleteInstr(303)]);
(137, [EatInstr(46,165);EatInstr(45,164);CompleteInstr(305)]);
(138, [AAction2Instr(__a45,97)]);
(139, [AWhenInstr3(__p47,__p46,166)]);
(140, [EatInstr(46,168);EatInstr(45,167);CompleteInstr(308)]);
(141, [CompleteInstr(309)]);
(142, [EatInstr(120,98);EatInstr(100,95);EatInstr(98,94)]);
(143, [CompleteInstr(310)]);
(144, [EatInstr(62,143)]);
(145, [EatInstr(62,146);ACallInstr3(__default_call,48);ASimpleCont2Instr(311,__binder0,145)]);
(146, [CompleteInstr(312)]);
(147, [AAction2Instr(__a50,171);AAction2Instr(__a49,170);AAction2Instr(__a48,169)]);
(148, [CompleteInstr(279)]);
(149, [ACallInstr3(__default_call,30);ASimpleCont2Instr(293,__binder0,172)]);
(150, [ALookaheadInstr(false,CfgLA (20,283),173);ACallInstr3(__default_call,20);ASimpleCont2Instr(283,__binder0,150)]);
(151, [ALookaheadInstr(false,CfgLA (20,283),174);ACallInstr3(__default_call,20);ASimpleCont2Instr(283,__binder0,151)]);
(152, [CompleteInstr(286)]);
(153, [AContInstr3(286,__g51,__binder9,175);ACallInstr3(__g51,23)]);
(154, [CompleteInstr(295)]);
(155, [AAction2Instr(__a53,177);AAction2Instr(__a52,176)]);
(156, [ALookaheadInstr(false,CfgLA (5,268),178)]);
(157, [ACallInstr3(__default_call,47);ASimpleCont2Instr(310,__binder0,130)]);
(158, [AAction2Instr(__a55,180);AAction2Instr(__a54,179)]);
(159, [AContInstr3(286,__g56,__binder10,181);ACallInstr3(__g56,23)]);
(160, [AContInstr3(286,__g57,__binder11,182);ACallInstr3(__g57,23)]);
(161, [ALookaheadInstr(false,CfgLA (2,265),183)]);
(162, [ACallInstr3(__default_call,39);ASimpleCont2Instr(302,__binder0,184)]);
(163, [ACallInstr3(__default_call,39);ASimpleCont2Instr(302,__binder0,185)]);
(164, [ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,186)]);
(165, [ACallInstr3(__default_call,41);ASimpleCont2Instr(304,__binder0,187)]);
(166, [ALookaheadInstr(false,CfgLA (43,306),188)]);
(167, [ACallInstr3(__default_call,44);ASimpleCont2Instr(307,__binder0,189)]);
(168, [ACallInstr3(__default_call,44);ASimpleCont2Instr(307,__binder0,190)]);
(169, [ACallInstr3(__default_call,18);ASimpleCont2Instr(281,__binder0,191)]);
(170, [ACallInstr3(__default_call,16);ASimpleCont2Instr(279,__binder0,147)]);
(171, [CompleteInstr(276)]);
(172, [AAction2Instr(__a58,192)]);
(173, [AAction2Instr(__a59,193)]);
(174, [AAction2Instr(__a60,194)]);
(175, [EatInstr(124,195);EatInstr(47,195)]);
(176, [AContInstr3(286,__g61,__binder12,196);ACallInstr3(__g61,23)]);
(177, [CompleteInstr(296)]);
(178, [CompleteInstr(304)]);
(179, [ACallInstr3(__default_call,46);ASimpleCont2Instr(309,__binder0,130)]);
(180, [ACallInstr3(__default_call,49);ASimpleCont2Instr(312,__binder0,130)]);
(181, [AAction2Instr(__a62,197)]);
(182, [AAction2Instr(__a63,198)]);
(183, [CompleteInstr(302)]);
(184, [CompleteInstr(303)]);
(185, [EatInstr(46,163);CompleteInstr(303)]);
(186, [CompleteInstr(305)]);
(187, [EatInstr(46,165);CompleteInstr(305)]);
(188, [CompleteInstr(307)]);
(189, [CompleteInstr(308)]);
(190, [EatInstr(46,168);CompleteInstr(308)]);
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
