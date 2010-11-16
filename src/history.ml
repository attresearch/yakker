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

type 'a root =
  | Empty
  | Root of 'a info
and 'a info = {v:'a; mutable branchings:'a branching list}
and 'a branching =
  | One of 'a root
  | Two of 'a root * 'a root

let impossible() = failwith "History.impossible"

(* The base history class.  uniq should be a memoizing function, use id for no memoization *)
class ['a] history (uniq:'a root -> 'a root) =
  let mk_root (v:'a) = (* memoized *)
    uniq (Root{v=v;branchings=[]}) in
  object (self:'b)
    val root = Empty

    method get_root = root

    method empty =
      {< root = Empty >}

    method push v =
      let r = mk_root v in
      match r with
      | Empty -> impossible()
      | Root({v=v;branchings=b} as inf)-> (* NB we aren't memoizing the branchings *)
          inf.branchings <- (One root)::b;
          {< root = r >} (* copy of self with new root *)

    method merge v (h2:'b) =
      let r = mk_root v in
      match r with
      | Empty -> impossible()
      | Root({v=v;branchings=b} as inf) -> (* NB we aren't memoizing the branchings *)
          inf.branchings <- (Two(root,h2#get_root))::b;
          {< root = r >} (* copy of self with new root *)
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
          | Empty -> current <- tl; self#next()
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
      Empty,Empty -> 0
    | Empty,_ -> -1
    | _,Empty -> 1
    | Root{v=v1},Root{v=v2} -> Hv.compare v1 v2)
  module Root =
    struct
      type t = Hv.t root
      let equal r1 r2 =
        match r1,r2 with
          Empty,Empty -> true
        | Root{v=v1},Root{v=v2} -> 0 = Hv.compare v1 v2
        | _ -> false
      let hash r =
        match r with
        | Root{v=v} -> Hv.hash v
        | Empty -> 0 (* NB we never expect to enter Empty into the hash set, so this shouldn't matter;
                        in any case, the usual Hashtbl.hash returns a positive integer, so this does not collide *)
    end
  module WeakRoot = Weak.Make(Root)
  let hash h = Root.hash (h#get_root)
  let memoize = ref false
  let new_history () =
    if !memoize then
      let memo_tbl = WeakRoot.create 11 in
      new history (WeakRoot.merge memo_tbl)
    else
      new history (fun x -> x)

end

module Make_show (Hv : HV) = struct
  module Atom = struct
    type t = Hv.t
    let equal v1 v2 = Hv.compare v1 v2 = 0
    let hash = Hv.hash
  end
  module Hash_atom = Hashtbl.Make(Atom)

  (** returns r and its left siblings. *)
  let get_left_siblings r = 
    let rec loop rs r = match r with
    | Empty -> rs
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
    | Empty | Root {branchings = [One _]} ->
	[]
    | Root {branchings = (One _) :: _} ->
	Printf.eprintf "get_children: Ambiguity encountered.\n";
	[]
    | Root {branchings = [Two (_, r3)]} ->
	get_left_siblings r3
    | Root {branchings = (Two(_, r3)) :: _} ->
	Printf.eprintf "get_children: Ambiguity encountered.\n";
	get_left_siblings r3	
    | Root {branchings=[]} -> impossible()

  let dot_show_pretty string_of_atom h =
    let tbl = Hash_atom.create 11 in

    (** returns: last used n *)
    let rec dot_show_child n_parent n_last c =
      let (n_t, n_final) = dot_show_tree n_last c in
      Printf.printf "%i -> %i;\n" n_parent n_t;
      n_final

    (** returns: last used n *)
    and dot_show_tree n_last = function
      | Empty -> 0, n_last
      | (Root {v = v}) as t ->
	  let n_opt = try Some (Hash_atom.find tbl v) with Not_found -> None in
	  match n_opt with
            | Some n -> (n,n_last)
            | None ->
		let n = n_last + 1 in
		Hash_atom.add tbl v n;
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
      List.iter (fun (x,y) -> Printf.printf "%i -> %i;\n" x y) !edges;
      List.iter (fun (x,y) -> Printf.printf "%i -> %i [arrowhead = diamond];\n" x y) !cedges in

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
      let dot_show_packed n_parent n_last e =
	let n = n_last + 1 in
	Printf.printf "%i [label=\"\" shape = circle width = 0.15];\n" n;
	edge n_parent n;
	dot_show_edge n e
      in
      match root with
      | Empty -> 0, n_last
      | Root ({v = v} as t) ->
	  let n_opt = try Some (Hash_atom.find tbl v) with Not_found -> None in
	  match n_opt with
            | Some n -> (n,n_last)
            | None ->
		let n = n_last + 1 in
		Hash_atom.add tbl v n;
		Printf.printf "%i [ label = %S shape = box, style = rounded];\n" n
		  (string_of_atom v);
		let n_final = match t.branchings with
                    [] -> impossible()
		  | [e] -> dot_show_edge n e
		  | edges ->
	              if Logging.activated then
			Logging.log Logging.Features.sppf
                          "Encountered node with non-singular edge set. (%s)." (string_of_atom v);
                      List.fold_left (dot_show_packed n) n edges in
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
