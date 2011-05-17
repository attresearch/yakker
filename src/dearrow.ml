(*******************************************************************************
 * Copyright (c) 2011 AT&T.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Trevor Jim and Yitzhak Mandelbaum
 *******************************************************************************)

(** The arrow-notation transform.
    Prerequisites:
      lifting. provides valuable invariants for Opt, Star. *)

open Yak


open Gul
open Gul.Curried_constructors

(* Note: transformation of do-notation for arrows does a two-fold
   transformation for binding. First, it does the arrow-notation
   translation from figure 3. But, that's not enough because the
   result is now in terms of the bind combinator. The bind combinator
   does its own transformation (so to speak) based on its definition:

      u `bind` f = arr id &&& u >>> f

   Note that &&& is defined in terms of *** which is itself defined in
   terms of first and second. first is a core arrow
   operation. Unfortunately, it has no easy parallel in our
   context. So, we need to implement it as a grammar
   transformation. This grammar transformation, then, is like a second
   pass for the do-notation transformation. (Possibly it even leads to
   exponentially many passes -- have to think about this one.)

   This approach doesn't make much sense. We should handle the binding
   elimination and the issue of threading the environment in one
   pass. perhaps this way, the typing can also be worked out in a semi
   rational way.

   Need to think about binding issue. Do we need CPS style still?
   Might be necessary. I think we just need a flag indicating whether
   the result needs to be saved. Note that this corresponds to the arrow

     arr id &&& u

   from the defintion of `bind`. In essence, if you are going to bind
   u, then you need to duplicate the incoming stream. So, in our case,
   we pass a flag indicating this need.

(side note: can't we use type inference to avoid annotations on nonterminals with early results?)

Specification of arrow transformation

First, we specify some useful abbreviations. I use the tick mark to inject a variable from the abbreviation into the definition.

    let wrap G e = fun ([G]) -> [e]
    -- wrap an expression e so that its free variables from G are properly bound.

    let drop m = fun (n_1, ..., n_m-1, n_m) -> (n_1, ..., n_m-1)
    -- drop the last element of an environment of length m.

    let extend m = fun (n_1, ..., n_m) x -> (n_1, ..., n_m, x)
    -- Extend environment of length m with a new value.

    let dropMerge = fun env _ -> env
    -- drop the newly generated value and just propogate the current environment.

    let contract m = fun (n_1, ..., n_m-2, x, y) -> (n_1, ..., n_m-2, y)
    -- Drop the penultimate element of an environment of length m.
    --
    -- Can be used for purpose is similar to tail-call elimination (e.g. in star)
    -- where, conceptually, the last element is replacing the penultimate one;
    -- or, can be used for unrelated environment manipulation (e.g. in sequence).
    --
    -- Constraint: all variables unique

TODO: Need to define (G) -- in order to be valid pattern, can't have repeated names -- shadowing not supported in patterns.

Next, we specify a transformation from Gul to Gil rules (assuming on untyped language; typed still to come).

    G |-a R ~> r

The transformation is parametrized by the need to save the result being computed.

    a := ^  -- add result to environment
       | #  -- leave environment as is

Note that the first two rules, for terminals, apply for epsilon as well.

    ------------
    G |-# c ~> c

    ---------------------------------
    G |-^ c ~> c {fun env -> [extend |G|] env ()}

    --------------------------------
    G |-# {e} ~> {fun (G) -> e; (G)}

    --------------------------------
    G |-^ {e} ~> {fun (G) -> (G, e)}

    (f = { dropMerge, when a = #
           extend |G|,  when a = ^ } )
    ----------------------------------
    G |-a @box(e) ~> @box(wrap G e, f)

    G |-^ R1 ~> r1   G,x |-a R2 ~> r2
    (f = EITHER
           drop |G,x|,           when a = #
           contract (|G,x| + 1), when a = ^ )
    ----------------------------------
    G |-a R1 $x R2 ~> r1 r2 {f}

    G |-# R1 ~> r1   G |-a R2 ~> r2
    --------------------------------
    G |-a R1 R2 ~> r1 r2

    G |-a R1 ~> r1   G |-a R2 ~> r2
    -------------------------------
    G |-a R1 | R2 ~> r1 | r2

    (f = { dropMerge, when a = #
           extend |G|,  when a = ^ } )
    --------------------------------
    G |-a A(e) ~> A(wrap G e, f)

    G |-^ {e} ~> f   G,x |-^ R ~> r
    (r2 = EITHER
            {drop |G,x|},  when a = #
            epsilon, when a = ^ )
    ---------------------------------------
    G |-a *{x=e}R ~> {f} *(r {[contract (|G,x| + 1)]}) r2



    x |-^ R ~> r
    -------------
    A(x) = R ~> r {[contract 2]}

    |-^ R ~> r
    -------------
    A = R ~> r

And now, for a typed version.

(A QUICK THOUGHT:

It would be illustrative to try to do this without any first step of type inference. Shows why
type inference is useful. I tried in my head and my basic conclusion is that, aside from issues of
compactness (not ending up with a huge number of constructors and type variables), you fundamentally
need some way to know what the constructor to expect is....)

Assume that we have a function c : G -> C, which maps type environments to constructors.

Types T := ...

First, we define an elaboration from implicitly typed right-sides to partially annotated right-sides


We start with a *very* simple definition of type inference for expressions: we simply introduce a fresh variable.

    ---------- (X fresh)
    G |= e : X

Next, we define the aforementioned elaboration.

    ---------------
    G |- c ~> c : unit

    G |= e : T
    ------------------------
    G |- {e} ~> {e : T} : T

    -------------------------------- (X fresh)
    G |- @box(e) ~> @box(e : X) : X

For box, we're not looking for the type of e, but for the type of any (potential) semantic
value which might be generated if the box returns successfully. So, we don't use the inference
for expressions.

    G |- R1 ~> R1' : T1; C1   G,x:T1 |- R2 ~> R2' : T2; C2
    ------------------------------------------------------
    G |- R1 $x R2 ~> R1' $x:T1 R2' : T2; C1, C2

    G |- R1 ~> R1' : T1; C1   G |- R2 ~> R2' : T2; C2
    --------------------------------------------------
    G |- R1 R2 ~>  R1' R2' : T2; C1, C2

    G |- R1 ~> R1' : T1; C1   G |- R2 ~> R2' : T2; C2
    --------------------------------------------------
    G |- R1 | R2 ~>  R1' | R2' : C1, C2, T1=T2

    G |= e : T
    ---------------------------
    G |- A(e) ~> (A(e) : X_A2) : X_A2; X_A1 = T

    G |= e : T0   G,x:T0 |- R ~> R' : T1; C
    ----------------------------------------------------------
    G |- *{x=e}R ~> *{x:T0=e}R' : T1; C, T0 = T1

    G |- R ~> R' : T; C
    ----------------------------------------------------------
    G |- *R ~> *R' : unit; C, T = unit

Next, we define the elaboration for rules:

    x:X_A1 |- R ~> R' : T; C
    -------------------------------------
    A(x) = R ~> A(x:X_A1) = R'; C, X_A2=T

    |- R ~> R' : T; C
    ----------------------
    A = R ~> A = R; C, X_A2=T

Note that the rule for nonterminal application does more work than strictly necessary in relating
the type of the argument e with the (well-known) argument type of A. However, this could come in
handy if, for example, we make the inferrence for expressions more powerful (e.g. adding a rule
for the case of single variables).

Next, we run this elaboration on the grammar (in the obvious manner) to produce a new grammar and a set of constraints.

    G ~> G'; C

Next, we use unification to solve the constraints to the extent possible. I assume we can find a minimal set
of resulting free variables.

Now, we can redefine our arrow transformation in terms of (partially)
annotated right sides and rules. We'll need a global map M from
ordered lists of types to constructor names. I assume functions

    Ty: Env -> [T],

which provides the types of variables G as an ordered list
(including duplicates) and

    Nm : Env -> [x],

which provides the variable names.  Let's abbreviate M . Ty as Mt.

    Mt = M . Ty
    G - k = G' where G = G',x1:T1,...,xk:Tk
    [.] splices a meta-language computation into object-language code.
    /G/ composes a number of the above operations.
    /G/ = [Mt G]([Nm G])

Now, some typed versions of the above combinators:

    let wrap G e = function /G/ -> [e]
    -- wrap an expression e so that its free variables from G are properly bound.

    let boxWrap G e T = function /G/ -> fun input pos ->
                            option_map (fun (n,x) -> n, [M T] x) ([e] input pos)
    -- wrap a box e so that its free variables from G are properly bound.

    let dropK G k = fun pos -> function [Mt G] (n_1, ..., n_m) -> [Mt G'](n_1, ..., n_m-k)
    where G = G',x1:T1,...,xk:Tk
          m = |G|
    -- drop the last k elements of an environment G of length m.

    let drop G = dropK G 1
    -- drop the last element of an environment G of length m.

    let extend G T = function [Mt G] (n_1, ..., n_m) -> fun x -> [Mt (G,_:T)](n_1, ..., n_m, x)
    where m = |G|
    -- Extend environment of length m with a new value of type T.

    let dropKAndExtend G k T =
       function [Mt G] (n_1, ..., n_m) -> function [Mt (x:T)] (x) ->
         [Mt (G',_:T)](n_1, ..., n_m-k, x)
    where G = G', x1:T1, ..., xk:Tk
          m = |G|
    -- Generate a merge function which drops k elements from environment
       of length m and extend with a new value of type T.

    let dropMerge = fun env _ -> env
    -- drop the newly generated value and just propogate the current environment.

    let dropKMerge G k = function /G/ -> fun _ -> /G - k/
    -- drop the newly generated value and k elements from the current environment.

    let propMerge = fun v1 _ -> v1
    -- drop the newly generated value and propogate the current environment.

    let contract G = function [Mt G] (n_1, ..., n_m-2, x, y) -> [Mt (G',_:T)](n_1, ..., n_m-2, y)
    where G = G', _ ,_:T
          m = |G|
    -- Drop the penultimate element of an environment of length m.
    --
    -- Can be used for purpose is similar to tail-call elimination (e.g. in star)
    -- where, conceptually, the last element is replacing the penultimate one;
    -- or, can be used for unrelated environment manipulation (e.g. in sequence).
    --
    -- Constraint: all variables unique

    let replaceK G k e T = ...
    --- replace the last k elements of G with element e of type T.


One catch: we need to properly shrink environments when their variables go out of scope.
Yet, once we involve types, all environment manipulations need to know the types of environment
elements so as to use the proper constructor. So, we have two choices. Either have the translation
also return a type so that in a binding sequence we know the type of the added binding in the
environment, or we can push the burden of deleting from the environment "up" the derivation to the
leaves, where the environment manipulation will be done before the new element is added. We can do this
by sending a "deletion count" up the derivation.

The advantage of the former approach is that it is more modular; the latter,
that it is more efficient. We'll map out both.

First, we specify the approach which returns the type along with the translation.
Notice that in this version the annotation on binders becomes superfluous --
types on leaves are sufficient.

   -------------------
   G |-# c ~> c : unit

   ---------------------------------------------
   G |-^ c ~> c {function [/G/] -> [Mt (G,_:unit)]([Nm G @ ()])} : unit

   -----------------------------------------------
   G |-# {e:T} ~> {function [/G/] -> e; [/G/]} : T

   ----------------------------------------------------------------
   G |-^ {e:T} ~> {function [/G/] -> [Mt (G,_:T)]([Nm G @ e])} : T

   (f = EITHER
          dropMerge,     when a = #
          [extend G T],  when a = ^ )
   ----------------------------------------------
   G |- @box(e : T) ~> @box(boxWrap G e T, f) : T

   G |-^ R1 ~> r1 : T   G,x:T |-a R2 ~> r2 : T2
   (f = EITHER
          [drop G],                when a = #
          [contract (G,x:T,_:T2)], when a = ^ )
   ---------------------------------------------
   G |-a R1 $x:T R2 ~> r1 r2 {f} : T2

   G |-# R1 ~> r1 : T1   G |-a R2 ~> r2 : T2
   -----------------------------------------
   G |-a R1 R2 ~> r1 r2 : T2

   G |-a R1 ~> r1 : T   G |-a R2 ~> r2 : T
   ----------------------------------------
   G |-a R1 | R2 ~> r1 | r2 : T

   (f = { dropMerge,   when a = #
          extend G T,  when a = ^ } )
   ------------------------------------
   G |-a A(e) : T ~> A(wrap G e, f) : T

   G |-^ {e:T} ~> f : T   G,x:T |-^ R ~> r : T
   (r2 = EITHER
           {[drop G,x:T]},  when a = #
           epsilon,         when a = ^ )
   f1 = [contract (G,x:T,_:T)]
   ---------------------------------------------
   G |-a *{x:T=e}R ~> {f} *(r {f1}) r2 : T


    x:T1 |-^ R ~> r : T2
    --------------------
    A(x:T1) = R ~> r {[contract (x:T1, _:T2)]}

    |-^ R ~> r
    ----------
    A = R ~> r

Next, we specify the version which passes "drop counts" up the derivation.
Notice that we don't need to identify "irrelevant" subcomponents.

   ----------------------------
   G;0 |-# c ~> c

   k > 0
   ----------------------------
   G;k |-# c ~> c {[dropK G k]}

   ---------------------------------------------
   G;k |-^ c ~> c {[replaceK G k `() unit]}

   -----------------------------------------------
   G;k |-# {e:T} ~> {function [/G/] -> e; [/G - k/]}

    (G = G', x1:T1, ..., xk:Tk
     G'' = G', _:T)
   ----------------------------------------------------------------
   G;k |-^ {e:T} ~> {function [/G/] -> [Mt G'']([Nm G' @ e])}

   -----------------------------------------------------
   G;0 |-# @when(e) ~> @when(whenWrap G e, fun pos x -> x)

   (f = EITHER
          [dropK G k],       when a = #
          [replaceK G k `true bool_ty],  when a = ^ )
   ----------------------------------------------
   G;k |-a @when(e) ~> @when(whenWrap G e, f)

   -----------------------------------------------------
   G;0 |-# @box(e : T) ~> @box(boxWrap G e T, propMerge)

   (f = EITHER
          [dropKMerge G k],       when a = #
          [replaceKMerge G k T],  when a = ^ )
   ----------------------------------------------
   G;k |-a @box(e : T) ~> @box(boxWrap G e T, f)

   G;0 |-^ R1 ~> r1   G,x:T; k + 1 |-a R2 ~> r2
   --------------------------------------------
   G;k |-a R1 $x:T R2 ~> r1 r2

   G;0 |-# R1 ~> r1   G;k |-a R2 ~> r2
   -----------------------------------
   G;k |-a R1 R2 ~> r1 r2

   G;k |-a R1 ~> r1   G;k |-a R2 ~> r2
   -----------------------------------
   G;k |-a R1 | R2 ~> r1 | r2

   -------------------------------------------
   G;0 |-# A : T ~> A(no_call, propMerge)

   (f = EITHER
          [dropKMerge G k],      when a = #
          [replaceKMerge G k T], when a = ^ )
   -------------------------------------------
   G;k |-a A : T ~> A(no_call, f)

   -------------------------------------------
   G;0 |-# A(e) : T ~> A(wrap G e, propMerge)

   (f = EITHER
          [dropKMerge G k],      when a = #
          [replaceKMerge G k T], when a = ^ )
   -------------------------------------------
   G;k |-a A(e) : T ~> A(wrap G e, f)

   G;0 |-# R ~> r
   G;k |-a epsilon ~> r1
   ---------------------------------------------
   G;k |-a *R ~> *r r1

   G;0 |-^ {e:T} ~> r_e   G,x:T;1 |-^ R ~> r
   G,x:T;k+1 |-a {x:T} ~> r_x
   ---------------------------------------------
   G;k |-a *{x:T=e}R ~> r_e *r r_x



    x:T1;1 |-^ R ~> r
    --------------------
    A(x:T1) = R ~> r

    .;0 |-^ R ~> r
    ---------------
    A = R ~> r

Finally, when all the above is done, we need to create the datatype used above. We
do so by dumping M, which we assume has been built up during the course of tranlsating
the grammar. Our first step is get the union of all types used in call data constructors.
Let those be a list "types". Then, our datatype is:

  type ([types]) contexts = C1 of ... | ... | Cn of ...

However, this datatype is not recursive, so we can choose to use polymorphic variants instead,
if our language supports them.


----------

TODO: add OPtion, DBranch, When, Position, Star with earley accum.



*)

(**

  Missing distinction between contexts (static) and environments
  (dynamic).  leads to abuse of notation. We are using contexts to
  guide us in the correct generation of code for manipulating
  environments. We should be careful not to confuse the two.

  context = ordered list of variable-type pairs
  environment = ordered list of values.

*)
module type CONTEXT = sig
  type pl                               (** "plain" - normal context, might have shadowed variables. *)
  type sf                               (** shadowing-free *)
  type 'a ctxt

  val ctxt_size : 'a ctxt -> int
  val is_empty : 'a ctxt -> bool

  val demote : sf ctxt -> pl ctxt
    (** [demote g] coerces a shadowing-free context to a normal one. *)

  val empty : sf ctxt
  val singleton : var -> ty -> sf ctxt
  val ext : 'a ctxt -> var -> ty -> pl ctxt

  (** If context is already shadow-free, guaranteed not to change. *)
  val deshadow: 'a ctxt -> sf ctxt

  (** [drop_n g n] drops the last [n] members of the environment g.
      If [n] is greater than the size of [g], then the empty environment
      is returned. *)
  val drop_n : 'a ctxt -> int -> 'a ctxt


  val tys_of_ctxt : 'a ctxt -> ty list
    (** [tys_of_ctxt g] provides the types of variables in [g]
        as an ordered list (including duplicates)  *)

  val vars_of_ctxt : 'a ctxt -> var list
    (** [vars_of_ctxt g] provides the variable names in [g]
        as an ordered list (including duplicates)  *)
end

module StringSet = Set.Make(String)
open Util.Operators

module Context : CONTEXT = struct

  (** Contexts are encoded *in reverse*.  *)


  (* We leave the first two types abstract -- they are only used for
     static enforcement of invariants, and have no real meaning. *)
  type pl                               (** "plain" - normal context, might have shadowed variables. *)
  type sf                               (** shadowing-free *)
  type 'a ctxt = (var * ty) list

  let empty = []
  let singleton v t = [(v,t)]
  let ext g v t = (v,t)::g

  let ctxt_size = List.length
  let is_empty = function [] -> true | _ -> false

  let demote g = g

  (* Replaces all shadowed bindings with fresh names.
     If context is already shadow-free, guaranteed not to change. *)
  let deshadow g =
    let freshen = Variables.freshn in
    let maybe_rename (revg, vars) (x,t) =
      if StringSet.mem x vars
      then (freshen x, t)::revg, vars
      else (x,t) :: revg, StringSet.add x vars in
    let revg, _ = List.fold_left maybe_rename ([], StringSet.empty) g in
    List.rev revg

  (** [drop_n g n] drops the last [n] members of the environment g.
      If [n] is greater than the size of [g], then the empty environment
      is returned. *)
  let drop_n g n = Util.list_drop n g


  let vars_of_ctxt g = (List.rev $ fst $ List.split) g

  let tys_of_ctxt g = (List.rev $ snd $ List.split) g

end

open Context

module Meta_prog = struct
  type constructor = string
  type pat = string
  type exp = string

  let unit_ty : ty  = "unit"
  let int_ty : ty  = "int"
  let bool_ty : ty = "bool"

  let unit_val : exp = "()"
  let gen = Printf.sprintf
end

open Meta_prog

let transform gr =

  (** a global map from ordered lists of types to
      constructors. *)
  let con_table : (ty list, constructor) Hashtbl.t = Hashtbl.create 11 in
  Hashtbl.add con_table [] "Ykctxt_empty";
  let con_map  (l : ty list) : constructor =
    match Util.find_option con_table l with
      | None ->
          let c = "Ykctxt" ^ Variables.fresh () in
          Hashtbl.add con_table l c;
          c
      | Some c -> c in


  (** An abbreviation for con_map . tys_of_ctxt *)

  let mt g = (con_map (tys_of_ctxt g)) in


  (*
    WARNING: tricky, because if we use a wildcard in the pattern, then we can run into
    trouble when trying to reconstruct later. We should distinguish between generating
    patterns and expressions.

    Solution: provide "deshadowing" of contexts.
  *)

  let tuple_pat : pat list -> string =
    String.concat "," in

  let tuple_exp : exp list -> string  =
    tuple_pat in

  (** Convert a context to a string encoding the pattern of variables in the context.
      Variables must be deshadowed first so that the pattern remains linear. *)

  let tuple_pat_of_ctxt : sf ctxt -> string =
    tuple_pat $ vars_of_ctxt in

  (** Convert a context to a string encoding an expression for
      constructing an environment based on the variables in the
      context.  Variables must be deshadowed first so that the env. is
      reconstructed correctly. *)

  let tuple_exp_of_ctxt : sf ctxt -> string =
    tuple_pat_of_ctxt in


  (** Generate a list of variables names [x_1] ... [x_n],
      where [n] is the argument. *)

  let vars_of_n : int -> exp list =
    fun n -> Util.list_make n (gen "x%d") in

  (** Generate a tuple of variables names [x_1] ... [x_n],
      where [n] is the argument. *)

  let tuple_of_n : int -> string =
    tuple_exp $ vars_of_n in


  (** /G/ composes a number of the above operations. *)
  let named_pat_of_ctxt : sf ctxt -> pat =
    fun g ->
      if is_empty g then mt g
      else
        mt g ^ "(" ^ tuple_pat_of_ctxt g ^ ")" in

  (** Build a pattern representing an environment from a context, paying
      attention only to the types of the context's bindings, but not the
      names. Use names [x_1] ... [x_m], where [m] is the size of the
      context. Important for propogating values even when there's
      shadowing in the context. *)
  let numbered_pat_of_ctxt : 'a ctxt -> pat =
    fun g ->
      match ctxt_size g with
        | 0 -> mt g
        | n -> mt g ^ "(" ^ tuple_of_n n ^ ")" in

  let numbered_exp_of_ctxt : 'a ctxt -> exp =
    numbered_pat_of_ctxt in

  (* Wrap an expression [e] so that its free variables from [g] are properly bound. *)
  let wrap g e =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    gen "fun _ -> function %s -> (%s) | _ -> failwith \"Expected %s\"" pat_in e pat_in in

  (** the template for box code. *)
  let box_templ env_pat box_exp some_pat some_exp =
    gen "function
  | %s ->
    let f = %s in
    fun input pos ->
      begin match f input pos with
      | None -> None
      | Some (n, %s) -> Some (n, %s)
      end
  | _ -> failwith \"Expected %s\"" env_pat box_exp some_pat some_exp env_pat in

  (** wrap a box e so that its free variables from G are properly bound.
      Does not take care of environment manipulations.
      **Just for illustration**. *)
  let boxWrap g e ty =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let v_c = con_map [ty] in
    box_templ pat_in e "x" (v_c ^ " x") in

  (** wrap a box e so that its free variables from G are properly bound.
      result is dropped and environment is propogated. *)
  let boxWrapProp g e =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let x = Variables.fresh () in
    box_templ (gen "(%s as %s)" pat_in x) e "_" x in

  (** wrap a box e so that its free variables from G are properly bound.
      Drop result and [k] elements from environment. *)
  let boxWrapDropK g e k =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let exp_out = named_pat_of_ctxt (drop_n g k) in
    box_templ pat_in e "_" exp_out in

  (** wrap a box e so that its free variables from G are properly bound.
      Drop result and [k] elements from environment. *)
  let boxWrapReplaceK g e k ty =
    let g = deshadow g in
    let pat_in = named_pat_of_ctxt g in
    let g' = drop_n g k in
    let c = mt (ext g' "z" ty) in       (* [z] is arbitrary; we never make use
                                           of the variable name *)
    let exps_g' = vars_of_ctxt g' in
    let env_out = tuple_exp (exps_g' @ ["x"]) in
    box_templ pat_in e "x" (gen "%s(%s)" c env_out) in

  (**  drop the last k elements of an environment G of length m. *)
  let dropK g k =
    let pat_in = numbered_pat_of_ctxt g in
    let exp_out = numbered_exp_of_ctxt (drop_n g k) in
    gen "fun _ -> function %s -> %s | _ -> failwith \"Expected %s\"" pat_in exp_out pat_in in

  (** [replaceKp ppat g k e ty] replaces the last [k] elements of an
      environment corresponding to [g] with expression [e] of type
      [ty]. [ppat] specifies the pattern to be used for the position variable. *)
  let replaceKp ppat g k e ty =
    let pat_in = numbered_pat_of_ctxt g in
    let g' = drop_n g k in
    let c = mt (ext g' "z" ty) in       (* [z] is arbitrary; we never make use
                                                of the variable name *)
    let vars_g' = vars_of_n (ctxt_size g') in
    let env_out = tuple_exp (vars_g' @ [e]) in
    gen "fun %s -> function %s -> %s(%s) | _ -> failwith \"Expected %s\""
      ppat pat_in c env_out pat_in in

  (** Like [replaceK], but [ppat] is a wildcard. *)
  let replaceK = replaceKp "_" in

  (** generate a merge function that drops the newly generated value
      and k elements from the current environment. *)
  let dropKMerge g k =
    let pat_in = numbered_pat_of_ctxt g in
    let pat_out = numbered_pat_of_ctxt (drop_n g k) in
    Printf.sprintf "fun _ v1 _ -> match v1 with %s -> %s | _ -> failwith \"Expected %s\""
      pat_in pat_out pat_in in

  (** [replaceKMerge g k ty] generates a merge function that drops
      [k] elements from environment [g] of length [m] and extends [g]
      with a new value of type [ty], taken from the function's second argument. *)
  let replaceKMerge g k ty =
    let pat_v1 = numbered_pat_of_ctxt g in
    let v2_var = "x" in
    let pat_v2 = named_pat_of_ctxt (singleton v2_var ty) in
    let g' = drop_n g k in
    let c = mt (ext g' "z" ty) in       (* [z] is arbitrary; we never make use
                                           of the variable name *)
    let vars_g' = vars_of_n (ctxt_size g') in
    let env_out = tuple_exp (vars_g' @ [v2_var]) in
    Printf.sprintf "fun _ v1 v2 -> match (v1,v2) with
  | (%s, %s) -> %s(%s)
  | _ -> failwith \"Expected %s and %s\"" pat_v1 pat_v2 c env_out pat_v1 pat_v2 in


  let rec _tr (g : pl ctxt) k bind_q r =
    let do_base r =
      match k, bind_q with
        | 0, false -> r
        | _, false -> Gil.Seq (r, Gil.Action (dropK g k))
        | _, true  -> Gil.Seq (r, Gil.Action (replaceK g k unit_val unit_ty)) in
    match r.r with
      | Lit (b,s) -> do_base $| Gil.Lit (b, s)
      | DBranch (e, c) -> do_base $| Gil.DBranch (e, c, "")
      | CharRange (m,n) -> do_base $| Gil.CharRange (m,n)
      | Lookahead (b, r1) ->
          let r1 = _tr (demote empty) 0 false r1 in
          do_base $| Gil.Lookahead (b, r1)

      | Action (Some e, None) ->
          let g = deshadow g in
          let pat_in = named_pat_of_ctxt g in
          let e = match k, bind_q with
            | 0, false ->
                let x = Variables.fresh () in
                gen "fun _ -> function (%s as %s) -> %s; %s | _ -> failwith \"Expected %s\""
                  pat_in x e x pat_in
            | _, false ->
                let exp_out = named_pat_of_ctxt (drop_n g k) in
                gen "fun _ -> function %s -> %s; %s | _ -> failwith \"Expected %s\"" pat_in e exp_out pat_in
            | _, true ->
                let g' = drop_n g k in
                let ty = Util.from_some r.a.inf_type in
                let c = mt (ext g' "z" ty) in       (* [z] is arbitrary; we never make use
                                                       of the variable name *)
                let exps_g' = vars_of_ctxt g' in
                let env_out = tuple_exp (exps_g' @ [e]) in
                gen "fun _ -> function %s -> %s(%s) | _ -> failwith \"Expected %s\"" pat_in c env_out pat_in in
          Gil.Action e

      | When e ->
          let f = match k, bind_q with
            | 0, false -> "fun _ x -> x"
            | _, false -> dropK g k
            | _, true  -> replaceK g k "true" bool_ty in
          Gil.When (wrap g e, f)

      | Box (e, Some ty, bn) ->
          let e = match k, bind_q with
            | 0, false -> boxWrapProp g e
            | _, false -> boxWrapDropK g e k
            | _, true  -> boxWrapReplaceK g e k ty in
          Gil.Box(e, bn)

      | Box (_, None, _) ->
          Util.impossible "Dearrow.transform._tr.Box"

      | Symb (nt, e_opt, [], None) ->
          let merge = match k, bind_q with
            | 0, false -> None
            | _, false -> Some (dropKMerge g k)
            | _, true  -> Some (replaceKMerge g k (Util.from_some r.a.inf_type)) in
          Gil.Symb (nt, Util.option_map (wrap g) e_opt, merge)

      | Seq(r1, Some x, late, r2) ->
          let ty = Util.from_some r.a.inf_type in
          let r1 = _tr g 0 true r1 in
          let r2 = _tr (ext g x ty) (k+1) bind_q r2 in
          Gil.Seq (r1,r2)
      | Seq(r1, None, late, r2) ->
          let r1 = _tr g 0 false r1 in
          let r2 = _tr g k bind_q r2 in
          Gil.Seq (r1,r2)

      | Alt(r1, r2) ->
          let r1 = _tr g k bind_q r1 in
          let r2 = _tr g k bind_q r2 in
          Gil.Alt (r1,r2)

      (* Given lifting, Opt and Star(Bounds...) both must carry irrelevant subterms.
         Hence, we treat them like base cases. *)
      | Opt(r1) ->
          let r1 = _tr g 0 false r1 in
          do_base (Gil.Alt(Gil.Lit(false,""), r1))

      | Star(Bounds (0, Infinity), r1) ->
          let r1 = _tr g 0 false r1 in
          do_base (Gil.Star r1)

      | Position true ->
          let f = match k, bind_q with
            | 0, false -> "fun _ x -> x"
            | _, false -> dropK g k
            | _, true  -> let p = Variables.freshn "p" in replaceKp p g k p int_ty in
          Gil.Action f

      (* cases below should have been desugared *)
      | Rcount _    -> Util.impossible "Dearrow.transform._tr.Rcount"
      | Hash _      -> Util.impossible "Dearrow.transform._tr.Hash"
      | Minus _     -> Util.impossible "Dearrow.transform._tr.Minus"
          (* cases below should not be relevant *)
      | Prose _     -> Util.impossible "Dearrow.transform._tr.Prose"
  in
  gr.gildefs <-
    List.concat $|
        List.map
          (function
             | RuleDef(n,r,a) ->
                 let r = match a.Attr.early_params with
                   | None -> _tr (demote empty) 0 true r
                   | Some s ->
                       let x = get_param s in
                       let ty = Util.from_some a.Attr.early_param_type in
                       let g = singleton x ty in
                       _tr (demote g) 1 true r in
                 [(n, r)]
             | _ -> [])
          gr.ds;
  let free_tyvars = Util.remove_dups $| Hashtbl.fold begin fun tys _ ft ->
    List.rev_append (List.filter Ty_infer.is_tyvar tys) ft
  end con_table [] in
  begin match free_tyvars with
    | [] -> add_to_prologue gr $| Printf.sprintf "type _sv = \n"
    | [x] -> add_to_prologue gr $| Printf.sprintf "type %s _sv = \n" x
    | _ -> add_to_prologue gr $| Printf.sprintf "type (%s) _sv = \n" (String.concat " , " free_tyvars)
  end;
  Hashtbl.iter (fun tys c -> match tys with
                  | [] -> add_to_prologue gr $| Printf.sprintf "| %s\n" c
                  | _ -> add_to_prologue gr $| Printf.sprintf "| %s of %s\n" c (String.concat " * " tys)) con_table;
  add_to_prologue gr $| Printf.sprintf "let sv0 = Ykctxt_empty
(* TODO: tag environments, just like we do coroutines, to avoid full-blown comparison. *)
let sv_compare = compare\n";
