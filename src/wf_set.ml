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



module Int_set = struct
  type t = { mutable count : int; dense : int array; sparse : int array}

  let make sz = { count = 0; dense = Array.make sz 0; sparse = Array.make sz 0;}

  let clear s = s.count <- 0

  let insert ({count=n; dense=d; sparse=sp;} as s) i =
    d.(n) <-  i;
    sp.(i) <- n;
    s.count <- n+1

  let mem s i = let k = s.sparse.(i) in k < s.count && i = s.dense.(k)

  let iter f s =
    let d = s.dense in
    for i = 0 to s.count - 1 do
      f (d.(i))
    done

  (** iterate over the set accounting for [f] growing the set during the course of iteration. *)
  let iter_grow f s =
    let d = s.dense in
    let i = ref 0 in
    while !i < s.count do
      f (d.(!i));
      incr i
    done

  let fold f s v =
    let d = s.dense in
    let n = s.count in
    let rec loop i v = if i < n then loop (i+1) (f d.(i) v) else v in
    loop 0 v
end

module Int_map = struct

  type 'a t = { mutable count : int; dense_s : int array; dense_sv : 'a array; sparse : int array}

  let make sz x = { count = 0; dense_s = Array.make sz 0;
			dense_sv = Array.make sz x; sparse = Array.make sz 0; }

  let clear s = s.count <- 0

  let cardinal s = s.count

  (** swap elements at indexes [i] and [j]. *)
  let swap {count=_; dense_s=ds; dense_sv=dsv; sparse=sp;} i j =
    let x = ds.(i)
    and y = ds.(j)
    and v_x = dsv.(i)
    and v_y = dsv.(j) in
    ds.(i) <- y;
    ds.(j) <- x;
    dsv.(i) <- v_y;
    dsv.(j) <- v_x;
    sp.(y) <- i;
    sp.(x) <- j

  let insert ({count=n; dense_s=ds; dense_sv=dsv; sparse=sp;} as s) i x =
    ds.(n) <- i;
    dsv.(n) <- x;
    sp.(i) <- n;
    s.count <- n+1

  (** Imperative-style insert which marks [i] as a member, but, instead of inserting a value keyed to [i],
      returns the current value keyed to [i] for imperative update.
      N.B. Assumes [i] is not a member. *)
  let insert_imp ({count=n; dense_s=ds; dense_sv=dsv; sparse=sp;} as s) i
      null_s mk_empty =
    ds.(n) <- i;
    sp.(i) <- n;
    s.count <- n+1;
    let y = dsv.(n) in
    if y = null_s then
      let x = mk_empty () in
      dsv.(n) <- x;
      x
    else
      y

  (** N.B. Assumes [i] is a member. *)
  let get {dense_sv=dsv; sparse=sp;} i = dsv.(sp.(i))

  (** N.B. Assumes [i] is a member. *)
  let set {dense_sv=dsv; sparse=sp;} i x = dsv.(sp.(i)) <- x

  (** N.B. Assumes [i] is a member. *)
  let update {dense_sv=dsv; sparse=sp;} i f =
    let k = sp.(i) in
    dsv.(k) <- f dsv.(k)

  let mem s i =
    let k = s.sparse.(i) in
    k < s.count && i = s.dense_s.(k)

  let iter f s =
    let ds = s.dense_s in
    let dsv = s.dense_sv in
    for i = 0 to s.count - 1 do
      f ds.(i) dsv.(i)
    done

  (** iterate over the set [s], accounting for [f]
      growing the set during the course of iteration. *)
  let iter_grow f s =
    let ds = s.dense_s in
    let dsv = s.dense_sv in
    let i = ref 0 in
    while !i < s.count do
      f ds.(!i) dsv.(!i)
      incr i
    done

  let fold f s v =
    let ds = s.dense_s in
    let dsv = s.dense_sv in
    let n = s.count in
    let rec loop i v = if i < n then loop (i+1) (f ds.(i) dsv.(i) v) else v in
    loop 0 v

end

type 'a t = { mutable count : int; dense : 'a array; sparse : int array; ord : 'a -> int}

let make sz ord x = { count = 0; dense = Array.make sz x; sparse = Array.make sz 0; ord = ord; }

let clear s = s.count <- 0

let insert ({count=n; dense=d; sparse=sp;} as s) x =
  d.(n) <- x;
  sp.(s.ord x) <- n;
  s.count <- n+1

let mem s x =
  let i = s.ord x in
  let k = s.sparse.(i) in
  k < s.count && i = s.ord (s.dense.(k))

let iter f s =
  let d = s.dense in
  for i = 0 to s.count - 1 do
    f (d.(i))
  done

(** iterate over the set [s], accounting for [f]
    growing the set during the course of iteration. *)
let iter_grow f s =
  let d = s.dense in
  let i = ref 0 in
  while !i < s.count do
    f (d.(!i));
    incr i
  done

let fold f s v =
  let d = s.dense in
  let n = s.count in
  let rec loop i v = if i < n then loop (i+1) (f d.(i) v) else v in
  loop 0 v
