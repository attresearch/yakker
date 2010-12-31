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

(** parameterize the root type by the label type. Then,
    different history mechanisms can choose different keys. *)
type ('a,'lbl) root =
  | Empty
  | Root of ('a,'lbl) info
and ('a,'lbl) info = {label : 'lbl; v:'a;
               mutable branchings:('a,'lbl) branching list; (* NB we aren't compacting
                                                        (eliminating duplicates)
                                                        the branchings *)
              }
and ('a,'lbl) branching =
  | One of ('a,'lbl) root
  | Two of ('a,'lbl) root * ('a,'lbl) root

let get_label p = function Empty -> p | Root {label=p1} -> p1

let impossible() = failwith "History.impossible"

class type ['a] postfix =
      object
        method next : unit -> 'a
      end

class type ['a,'lbl] history =
      object ('h)
        method empty : 'lbl -> 'h
        method merge : 'lbl -> 'a -> 'h -> 'h
        method push : 'lbl -> 'a -> 'h
        method get_root : ('a,'lbl) root

        method traverse_postfix : 'a postfix
      end


type ('a,'lbl) lazyness = Forced of 'a | Delayed of ('a,'lbl) root

(* class for lazy postfix traversals of histories *)
class ['a] postfix_impl (r_init: ('a,'lbl) root) =
  object (self)
    val mutable current = [Delayed (r_init)] (* imperative state *)
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

(* The base history class.  uniq should be a memoizing function, use id for no memoization *)
class ['a, 'lbl] history_impl (uniq: ('a,'lbl) info -> ('a,'lbl) info) =
  let mk_info k (v:'a) = (* memoized *)
    uniq ({label=k; v=v; branchings=[]}) in
  object (self:'b)
    val root = Empty

    method get_root = root

    method empty (p:'lbl) =
      {< root = Empty >}

    method push p v =
      let ({branchings=b;} as inf) = mk_info (get_label p root) v in
      inf.branchings <- (One root)::b;
      {< root = Root inf >} (* copy of self with new root *)

    method merge p v (h2:'b) =
      let ({branchings=b;} as inf) = mk_info (get_label p root) v in
      inf.branchings <- (Two(root,h2#get_root))::b;
      {< root = Root inf >} (* copy of self with new root *)

    method traverse_postfix = new postfix_impl root
  end

module Label = struct
  type t = int

  let empty = 0

  let compare = (-)
  let equal = ((=) : int -> int -> bool)
  let hash a = a

  let to_string a = Printf.sprintf "%d" a
end

type label = Label.t

(* General memoization support for histories *)
module type HV = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (Hv : HV) = struct

  module Info = struct
    type t = (Hv.t,label) info

    let equal {label=k1; v=v1} {label=k2; v=v2} =
      Label.equal k1 k2 && 0 = Hv.compare v1 v2

    let compare {label=k1; v=v1} {label=k2; v=v2} =
      let c = Label.compare k1 k2 in
      if c <> 0 then c else Hv.compare v1 v2

    let hash {label=k; v=v} = (Label.hash k) lxor (Hv.hash v)
  end

  let compare_root r1 r2 =
    match r1, r2 with
        Empty, Empty -> 0
      | Empty, _ -> -1
      | _, Empty -> 1
      | Root inf1, Root inf2 -> Info.compare inf1 inf2

  let compare h1 h2 = compare_root h1#get_root h2#get_root

  module Weak_info = Weak.Make(Info)

  let hash h = match (h#get_root) with Empty -> 0 | Root inf -> Info.hash inf
  let memoize = ref false

  let new_history () =
    if !memoize then
      let memo_tbl = Weak_info.create 11 in
      new history_impl (Weak_info.merge memo_tbl)
    else
      new history_impl (fun x -> x)

  module Root_id_set = Set.Make(Info)

  let add_id_set r ris =
    let rec recur_root ris = function
      | Empty -> ris
      | Root info -> (* Add the current root and then add the children. *)
          let x = Root_id_set.add info ris in
          List.fold_left recur_branching x info.branchings
    and recur_branching ris = function
      | One r -> recur_root ris r
      | Two (r1, r2) ->
          let x = recur_root ris r1 in
          recur_root x r2 in
    recur_root ris r

  let get_id_set r = add_id_set r Root_id_set.empty

(******************************************************************************)

  (**
      Visualization functions.
  *)

  module Edge = struct
    type t = (Hv.t,label) branching

    let compare b1 b2 =
      match b1 , b2 with
          One r1, One r2 -> compare_root r1 r2
        | One _, Two _ -> -1
        | Two _, One _ -> 1
        | Two (r2, r3), Two (r2', r3') ->
            let c = compare_root r2 r2' in
            if c <> 0 then c else
              compare_root r3 r3'
  end
  module Edge_set = struct
    module Edge_set = Set.Make(Edge)
    include Edge_set

    let from_list xs = List.fold_left (fun s e -> add e s) empty xs
  end

  module Hash_info = Hashtbl.Make(Info)

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
    let tbl = Hash_info.create 11 in

    (** returns: last used n *)
    let rec dot_show_child n_parent n_last c =
      let (n_t, n_final) = dot_show_tree n_last c in
      Printf.printf "%i -> %i;\n" n_parent n_t;
      n_final

    (** returns: last used n *)
    and dot_show_tree n_last = function
      | Empty -> 0, n_last
      | Root ({v = v} as t) ->
          let n_opt = try Some (Hash_info.find tbl t) with Not_found -> None in
          match n_opt with
            | Some n -> (n,n_last)
            | None ->
                let n = n_last + 1 in
                Hash_info.add tbl t n;
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
    let tbl = Hash_info.create 11 in

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
      | Empty -> 0, n_last
      | Root ({v = v} as t) ->
          let n_opt = try Some (Hash_info.find tbl t) with Not_found -> None in
          match n_opt with
            | Some n -> (n,n_last)
            | None ->
                let n = n_last + 1 in
                Hash_info.add tbl t n;
                Printf.printf "%i [ label = %S shape = box, style = rounded];\n" n
                  ((string_of_atom v) ^ "(" ^ Label.to_string t.label ^ ")");
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



(*
Alternative: (in progress). would remove need for two labels on nonterminals.

type 'a, node =
  | Empty_node
  | Sibling_node of 'a sibling

and 'a sibling = {left : int; v:'a;
                  mutable links :'a sibling_links ;}

and 'a sibling_links =
  | Trivial
  | Simple of 'a node set
  | Complex of 'a node set * 'a node set  (* sets are very important because two fields are independent. *)

let get_left p = function Empty_node -> p | Sibling {left=p1} -> p1

(* The base history class.  uniq should be a memoizing function, use id for no memoization *)
class ['a, 'lbl] history_impl2 (uniq: ('a,'lbl) sibling -> ('a,'lbl) sibling) =
  let mk_sibling l v = (* memoized *)
    uniq ({label=l; v=v; links=Trivial}) in
  object (self:'b)
    val root = Empty_node

    method get_root = root

    method empty (p:'lbl) =
      {< root = Empty >}

    method push p v =
      let s = mk_sibling (get_start p root) v in
      (match s.links with
         | Trivial -> s.links <- Simple [root]
         | Simple ns -> s.links <- Simple (root :: ns)
         | Complex _ -> impossible ());
      {< root = Root s >} (* copy of self with new root *)

    method merge p v (h2:'b) =
      let s = mk_sibling (get_start p root) v in
      (match s.links with
         | Simple _ -> impossible ()
         | Trivial -> s.links <- Complex ...
         | Complex (ns1, ns2) -> ...
      {< root = Root s >} (* copy of self with new root *)


      let ({branchings=b;} as inf) = mk_info (get_label p root) v in
      inf.branchings <- (Two(root,h2#get_root))::b;
      {< root = Root inf >} (* copy of self with new root *)

    method traverse_postfix = new postfix_impl root
  end

  *)
