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

(* Character sets *)

type t = BitSet.t

(* Bugs in BitSet 2009-04-05: diff and differentiate and print and create_full *)
module MyBitSet = struct
  (* copies from original code *)
  type intern
  type t = {data:intern;len:int}
  external to_bitset : t -> BitSet.t = "%identity"
  external from_bitset : BitSet.t -> t = "%identity"

  external fast_get : intern -> int -> int = "%string_unsafe_get"
  external fast_set : intern -> int -> int -> unit = "%string_unsafe_set"
  let fast_length : intern -> int= Obj.magic String.length
  let bcreate : int -> intern = Obj.magic String.create
  let fast_fill : intern -> int -> int -> int (* char *) -> unit = Obj.magic String.fill

  let bfill dst start len c =
    assert (start >= 0 && len >= 0);
    fast_fill dst start len c
  let bget s ndx =
    assert (ndx >= 0 && ndx < fast_length s);
    fast_get s ndx
  let bset s ndx v =
    assert (ndx >= 0 && ndx < fast_length s);
    fast_set s ndx v

  (* bug fixes *)
  let differentiate (a:BitSet.t) (b:BitSet.t) =
    let a = from_bitset a in
    let b = from_bitset b in
    let sl = min a.len b.len in
    let abuf = a.data
    and bbuf = b.data in
    for i = 0 to sl-1 do
      bset abuf i ((bget abuf i) land (lnot (bget bbuf i)))
    done

  let int_size = 7 (* value used to round up index *)
  let log_int_size = 3 (* number of shifts *)

  (* Original code used 0x255 when should have used 0xFF or 255. *)
  let create_full n =
    if n < 0 then raise (BitSet.Negative_index "MyBitSet.create_full");
    let size = (n+int_size) lsr log_int_size in
    let b = bcreate size in
    bfill b 0 size 0xFF;
    to_bitset {
      data = b;
      len = size;
    }
end


let empty = BitSet.empty

(* Create a set containing the half-open interval [lb,ub)

   N.B. Different than Gul module's CharRange,
   which is a closed interval [lb,ub].
*)
let range lb ub =
  let r = empty() in
  for x = lb to (ub-1) do
    BitSet.set r x
  done;
  r

let dup = BitSet.copy
let difference = MyBitSet.differentiate
let union = BitSet.unite
let insert = BitSet.set
let iter f cs = Enum.iter f (BitSet.enum cs)
let compare = BitSet.compare
let complement universe_sz cs =
  let all = MyBitSet.create_full universe_sz in
  difference all cs;
  all

(* insert that half-open interval [lb,ub)

   N.B. Different than Gul module's CharRange,
   which is a closed interval [lb,ub].
*)
let insert_range cs lb ub =
  for x = lb to (ub-1) do
    BitSet.set cs x
  done
let member : t -> int -> bool = BitSet.is_set
let cs2enum = BitSet.enum
let count = BitSet.count
let intersect = BitSet.intersect

let singleton c = (* TODO: rename as just singleton *)
  let res = empty() in
  insert res (Char.code c);
  res

(* Return a list of half-open intervals in sorted order. *)
let cs2ranges a =
  let rec left i =
    if member a i then i else left (i+1) in
  let right i =
    let rec loop i =
      if not(member a (i+1)) then i
      else loop (i+1) in
    loop i in
  let result = ref [] in
  let count = ref (BitSet.count a) in
  let current = ref 0 in
  while !count > 0 do
    let l = left !current in
    let r = right l in
    let howmany = r-l+1 in
    count := !count - howmany;
    current := r+1;
    result := (l,r+1)::!result
  done;
  List.rev !result

(* Useful ranges *)
let _A_Z =
  let x = empty() in
  for i = Char.code 'A' to Char.code 'Z' do insert x i done;
  x

let _a_z =
  let x = empty() in
  for i = Char.code 'a' to Char.code 'z' do insert x i done;
  x

let _0_9 =
  let x = empty() in
  for i = Char.code '0' to Char.code '9' do insert x i done;
  x

(* Print these as singletons *) (* Not used right now *)
let _singletons =
  let x = empty() in
  for i = Char.code '!' to Char.code '+' do insert x i done;
  (* skip over ',' and '-' here *)
  for i = Char.code '.' to Char.code '/' do insert x i done;
  for i = Char.code ':' to Char.code '@' do insert x i done;
  for i = Char.code '[' to Char.code '`' do insert x i done;
  for i = Char.code '{' to Char.code '~' do insert x i done;
  x

let to_dotted_string cs =
  let rs = cs2ranges cs in
  if rs=[] then "" else
  "%d"^
  (String.concat "."
     (List.map
        (fun (lb,ub) ->
          if lb = (ub-1)  then Printf.sprintf "%d" lb
          else Printf.sprintf "%d-%d" lb (ub-1))
        rs))

let to_nice_string cs =
  let rs =
    List.concat
      [(let x = dup _A_Z in intersect x cs; cs2ranges x);
       (let x = dup _a_z in intersect x cs; cs2ranges x);
       (let x = dup _0_9 in intersect x cs; cs2ranges x)] in
  let other =
    (let x = dup cs in difference x _A_Z; difference x _a_z; difference x _0_9; x) in
  let other_s = to_dotted_string other in
  let ranges =
    (List.map
       (fun (lb,ub) ->
         if lb = (ub-1)  then Printf.sprintf "%c" (Char.chr lb)
         else Printf.sprintf "%c-%c"  (Char.chr lb) (Char.chr (ub-1)))
       rs) in
  String.concat ","
    (ranges @ (if other_s = "" then [] else [other_s]))

let to_string cs =
  let rs = cs2ranges cs in
  String.concat ","
    (List.map
       (fun (lb,ub) ->
         if lb = (ub-1)  then Printf.sprintf "%%d%d" lb
         else Printf.sprintf "%%d%d-%d" lb (ub-1))
       rs)

let to_code cs =
  let rs = cs2ranges cs in
  if rs = [] then "Cs.empty()" else
    "let cs = Cs.empty() in " ^
      (String.concat "; "
	 (List.map
	    (fun (lb,ub) ->
               if lb = (ub-1)  then Printf.sprintf "Cs.insert cs %d" lb
               else Printf.sprintf "Cs.insert_range cs %d %d" lb ub)
	    rs)) ^
      "; cs"

let pr cs =
   Printf.printf "%s\n" (to_string cs)
