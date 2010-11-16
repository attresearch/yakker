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

(* The history data structure *)

(* A history is a forest of trees that supports maximal sharing.
   We implement sharing using a weak hash table and memoization.
   Sharing has overhead and can be turned on and off with the
   "memoize" flag.
*)

(** (pseudo) parameterize the root type by the key type. Then, different history mechanisms can choose different keys. *)
type key = int
let key_compare = (-)

type 'a root =
  | Empty of key
  | Root of 'a info
and 'a info = {key : key; v:'a; 
	       mutable branchings:'a branching list; (* NB we aren't compacting 
							(eliminating duplicates) 
							the branchings *)
	      }
and 'a branching =
  | One of 'a root
  | Two of 'a root * 'a root

let get_key p = function Empty _ -> p | Root {key=k} -> k

let impossible() = failwith "History.impossible"

(* The base history class.  uniq should be a memoizing function, use id for no memoization *)
class ['a] history (uniq:'a info -> 'a info) =
  let mk_info k (v:'a) = (* memoized *)
    uniq ({key=k; v=v; branchings=[]}) in
  object (self:'b)
    val root = Empty 0

    method get_root = root

    method empty p =
      {< root = Empty p >}

    method push p v =
      let ({branchings=b;} as inf) = mk_info (get_key p root) v in
      inf.branchings <- (One root)::b;
      {< root = Root inf >} (* copy of self with new root *)

    method merge p v (h2:'b) =
      let ({branchings=b;} as inf) = mk_info (get_key p root) v in
      inf.branchings <- (Two(root,h2#get_root))::b;
      {< root = Root inf >} (* copy of self with new root *)
  end

(* class for lazy postfix traversals of histories *)
type 'a lazyness = Forced of 'a | Delayed of 'a root
class ['a] postfix (h_init:'a history) =
  object (self)
    val mutable current = [Delayed (h_init#get_root)] (* imperative state *)
    method next() =                        (* enumerator *)
      match current with [] -> raise Not_found
      | (Forced v)::tl -> current <- tl; v
      | (Delayed r)::tl ->
          (match r with
          | Empty _ -> current <- tl; self#next()
          | Root{v=v;branchings=(One r2)::_} -> (* silently throw away ambiguities with _ *)
              current <- (Delayed r2)::(Forced v)::tl;
              self#next()
          | Root{v=v;branchings=(Two(r2,r3))::_} -> (* silently throw away ambiguities with _ *)
              current <- (Delayed r2)::(Delayed r3)::tl;
              (* NB: not
                   current <- (Delayed r2)::(Delayed r3)::(Forced v)::tl;
                 because our replay functions expect a merge label to be omitted *)
              self#next()
          | Root{v=v;branchings=[]} -> impossible())
  end

(* General memoization support for histories *)
module type HV = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
end
module Make (Hv : HV) = struct

  let compare h1 h2 =
    (match h1#get_root, h2#get_root with
      Empty k1, Empty k2 -> key_compare k1 k2
    | Empty _, _ -> -1
    | _, Empty _ -> 1
    | Root ({key=k1; v=v1}), Root ({key=k2; v=v2}) -> 
	let c = key_compare k1 k2 in
	if c <> 0 then c else Hv.compare v1 v2)

  module Info = struct
    type t = Hv.t info

    let equal {key=k1; v=v1} {key=k2; v=v2} =
      k1 = k2 && 0 = Hv.compare v1 v2

    let hash {key=k; v=v} = k lxor Hv.hash v
  end

  module WeakInfo = Weak.Make(Info)

  let hash h = match (h#get_root) with Empty k -> k | Root inf -> Info.hash inf
  let memoize = ref false

  let new_history () =
    if !memoize then
      let memo_tbl = WeakInfo.create 11 in
      new history (WeakInfo.merge memo_tbl)
    else
      new history (fun x -> x)

end

module Make_show (Hv : HV) = struct
  module Atom = struct
    type t = Hv.t info

    let equal {key=k1; v=v1} {key=k2; v=v2} =
      k1 = k2 && 0 = Hv.compare v1 v2

    let hash {key=k; v=v} = k lxor Hv.hash v
  end

  let compare_root r1 r2 =
    match r1, r2 with
	Empty _, Empty _ -> 0
      | Empty _, _ -> -1
      | _, Empty _ -> 1
      | Root ({key=k1; v=v1}), Root ({key=k2; v=v2}) -> 
	  let c = key_compare k1 k2 in
	  if c <> 0 then c else Hv.compare v1 v2

  module Edge = struct
    type t = Hv.t branching
    let compare b1 b2 = 
      match b1 , b2 with
	  One r1, One r2 -> compare_root r1 r2
	| One _, Two _ -> -1
	| Two _, One _ -> 1
	| Two (r2, r3), Two (r2', r3') -> 
	    let c = compare_root r2 r2' in
	    if c <> 0 then c else
	      compare r3 r3'
  end
  module Edge_set = struct
    module Edge_set = Set.Make(Edge)
    include Edge_set

    let from_list xs = List.fold_left (fun s e -> add e s) empty xs
  end
  module Hash_atom = Hashtbl.Make(Atom)

  (** returns r and its left siblings. *)
  let get_left_siblings r = 
    let rec loop rs r = match r with
    | Empty _ -> rs
    | Root {branchings = [One r2]}  
    | Root {branchings = [Two(r2,_)]} -> 
	loop (r::rs) r2
    | Root {branchings = (One r2) :: _}
    | Root {branchings = (Two(r2,_)) :: _} -> 
	Printf.eprintf "get_left_siblings: Ambiguity encountered.\n";
	loop (r::rs) r2
    | Root {branchings=[]} -> impossible() in
    loop [] r

  let get_children = function
    | {branchings = [One _]} ->
	[]
    | {branchings = (One _) :: _} ->
	Printf.eprintf "get_children: Ambiguity encountered.\n";
	[]
    | {branchings = [Two (_, r3)]} ->
	get_left_siblings r3
    | {branchings = (Two(_, r3)) :: _} ->
	Printf.eprintf "get_children: Ambiguity encountered.\n";
	get_left_siblings r3	
    | {branchings=[]} -> impossible()

  let dot_show_pretty string_of_atom h =
    let tbl = Hash_atom.create 11 in

    (** returns: last used n *)
    let rec dot_show_child n_parent n_last c =
      let (n_t, n_final) = dot_show_tree n_last c in
      Printf.printf "%i -> %i;\n" n_parent n_t;
      n_final

    (** returns: last used n *)
    and dot_show_tree n_last = function
      | Empty _ -> 0, n_last
      | Root ({v = v} as t) ->
	  let n_opt = try Some (Hash_atom.find tbl t) with Not_found -> None in
	  match n_opt with
            | Some n -> (n,n_last)
            | None ->
		let n = n_last + 1 in
		Hash_atom.add tbl t n;
		Printf.printf "%i [ label = %S shape = box, style = rounded];\n" n
		  (string_of_atom v);
		let children = get_children t in
		let n_final = List.fold_left (dot_show_child n) n children in
		(n,n_final)

    in
    Printf.printf "digraph g {\n";
    Printf.printf "%i [ label = TOP shape = box, style = rounded];\n" 1;
    let t = h#get_root in
    let siblings = get_left_siblings t in
    ignore( List.fold_left (dot_show_child 1) 1 siblings );
    Printf.printf "}\n"

  let dot_show string_of_atom h =
    let tbl = Hash_atom.create 11 in

    (* Printf.print edges in the reverse order in which we encounter them,
       so that dot displays them in left-to-right order wrt the input *)
    let edges = ref [] in
    let cedges = ref [] in
    let edge x y = edges := (x,y)::!edges in
    let child_edge x y = cedges := (x,y)::!cedges in
    let pr_edges() =
      List.iter (fun (x,y) -> if y <> 0 then Printf.printf "%i -> %i;\n" x y) !edges;
      List.iter (fun (x,y) -> if y <> 0 then Printf.printf "%i -> %i [arrowhead = diamond];\n" x y) !cedges in

    (** returns: last used n *)
    let rec dot_show_tree n_last root =

      (** returns: last used n *)
      let dot_show_edge n_v = function
	  One r ->
	    let (n1,n_final) = dot_show_tree n_v r in
	    edge n_v n1;
	    n_final
	| Two (r2, r3) ->
	    let (n3,n_cur) = dot_show_tree n_v r3 in
	    child_edge n_v n3;
	    let (n2,n_final) = dot_show_tree n_cur r2 in
	    edge n_v n2;
	    n_final
      in
      (** returns: last used n *)
      let dot_show_packed n_parent e n_last =
	let n = n_last + 1 in
	Printf.printf "%i [label=\"\" shape = circle width = 0.15];\n" n;
	edge n_parent n;
	dot_show_edge n e
      in
      match root with
      | Empty _ -> 0, n_last
      | Root ({v = v} as t) ->
	  let n_opt = try Some (Hash_atom.find tbl t) with Not_found -> None in
	  match n_opt with
            | Some n -> (n,n_last)
            | None ->
		let n = n_last + 1 in
		Hash_atom.add tbl t n;
		Printf.printf "%i [ label = %S shape = box, style = rounded];\n" n
		  ((string_of_atom v) ^ "(" ^ string_of_int t.key ^ ")");
		let n_final = match t.branchings with
                    [] -> impossible()
		  | [e] -> dot_show_edge n e
		  | edges ->
	              if Logging.activated then
			Logging.log Logging.Features.sppf
                          "Encountered node with non-singular edge set. (%s)." (string_of_atom v);
		      let edgeset = Edge_set.from_list edges in
                      if Edge_set.cardinal edgeset > 1 then
			Edge_set.fold (dot_show_packed n) edgeset n 
		      else dot_show_edge n (Edge_set.choose edgeset) in
		(n,n_final) in
    Printf.printf "digraph g {\nordering=out\n";
    ignore(dot_show_tree 0 h#get_root);
    pr_edges();
    Printf.printf "}\n"
end

(*
(* FOR TESTING *)

module IntIntHashed = struct
  type t = int * int
  let compare i j = compare i j
  let hash i = Hashtbl.hash i
end
module IntIntHistory = Make(IntIntHashed)
let memoize = IntIntHistory.memoize
let new_history = IntIntHistory.new_history
let compare = IntIntHistory.compare
let hash = IntIntHistory.hash

let traverse h =
  let z = new postfix h in
  (try
    while true do
      Printf.printf "%d %!" (z#next())
    done
  with Not_found -> ());
  Printf.printf "\n"

let h0 = new_history()
let x = ((h0#push 3)#push 4)#push 5
let y = ((x#empty)#push 1)#push 2
let z = y#merge (-9999) x
;;
traverse z
(* should print 1 2 3 4 5 *)
;;
*)
