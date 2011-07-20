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

(* The coroutine transform *)
open Yak
open Gul

let yk_done e = Printf.sprintf "Yk_done(%s)" e
let yk_box e k =
  let i,j,ykb,v = Variables.fresh(),Variables.fresh(),Variables.fresh(),Variables.fresh() in
  Printf.sprintf "Yk_box(fun %s %s -> match (%s) %s %s with None -> None | Some (%s,%s) -> Some(%s,%s))" i ykb e i ykb j v j (k v)

let yk_when e = Printf.sprintf "Yk_when(%s)" e
let yk_delay e1 e2 = Printf.sprintf "Yk_delay(%s,%s)" e1 e2

let string_of_varset s =
  String.concat " " (PSet.fold (fun x y -> x::y) s [])

let add_no_early_or_late_prologue gr =
  add_to_prologue gr
    "
type sv = unit
let sv_compare = compare
let sv0 = ()
let sv_hash () = 0
"

let add_early_late_prologue gr =
  add_to_prologue gr
    "
(*EARLY-LATE PROLOGUE*)
(*TODO:sv,sv0,sv_compare*)
type _uid = int (* for sharing *)
type _pos = int (* input positions *)
type _lab = int (* dispatch labels *)
(** Early values, aka coroutines.
    ['a] is the type of values eventually computed
    by the coroutines *)
type 'a ev =
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
  | _,_ -> failwith \"TODO sv_compare\"
let _t_count = ref 0
let _fresh_t_id () =
  let count = !_t_count in
  incr _t_count;
  count
let _t f = Yk_more(_fresh_t_id(),f)
type sv = _wv ev * (int * hv * _pos, Yak.History.label) Yak.History.history
let sv0 = (Yk_done _wv0, Yk_History.new_history())
let sv_compare (x1,x2) (y1,y2) =
  (match ev_compare x1 y1 with
  | 0 -> hv_compare x2 y2
  | z -> z)
let _ev_to_string = function
  | Yk_more   _ -> \"Yk_more\"
  | Yk_box    _ -> \"Yk_box\"
  | Yk_when   _ -> \"Yk_when\"
  | Yk_delay  _ -> \"Yk_delay\"
  | Yk_bind   _ -> \"Yk_bind\"
  | Yk_done   _ -> \"Yk_done\"

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
  | (ev,_) -> failwith (Printf.sprintf \"_d(%s)\" (_ev_to_string ev))
let _darg x p = function (* YHM: close to _d *)
    (Yk_more(_,t),h) -> (t x p, _e p h)
  | _ -> failwith \"_darg\"
let _dbox x = function
    (Yk_more(_,t),h) ->
      (fun p ->
        (match t x p with
          Yk_box b ->
            (fun ykb -> (* painful! *)
              (match b p ykb with None -> None
              | Some(p2,a) -> Some(p2,(a,h))))
        | _ -> failwith \"_dbox\"))
  | _ -> failwith \"_dbox\"
let _dwhen x p = function
    (Yk_more(_,t),h) -> (match t x p with Yk_when b -> b | _ -> failwith \"_dwhen\")
  | _ -> failwith \"_dwhen\"
let _ddelay x p =
  (function
    | (Yk_more(_,t),h) -> (match t x p with Yk_delay(v,hv) -> (v,_p x hv p h) | _ -> failwith \"_ddelay1\")
    | _ -> failwith \"_ddelay2\")
let _dret x p v1 v2 =
  match v1 with
    | (Yk_more(_,t), h) ->
        (match t x p with
          | Yk_bind f -> (f (fst v2), h)
          | _ -> failwith \"_dret2\")
    | _ -> failwith \"_dret1\"
let _dmerge x p =
  (function
    | (Yk_more(_,t),h1) ->
        (fun (r,h2) ->
          match t x p with
          | Yk_bind(f) -> (f r, _m x p h1 h2)
          | _ -> failwith \"_dmerge1\")
    | _ -> failwith \"_dmerge3\")
let _dnext x p = function (*TJIM: same as _d without p *)
    (Yk_more(_,t),h) -> (t x p,h)
  | _ -> failwith \"_dnext\"

(* Redefine history constructors *)
let _e p (_,h) = (Yk_done _wv0, _e p h)
let _p lbl hv p (v,h) = (v, _p lbl hv p h)
let _m lbl p (v1,h1) (_,h2) = (v1, _m lbl p h1 h2)
"

let early_prologue = "
(*EARLY PROLOGUE*)
type _uid = int (* for sharing *)
type _pos = int (* input positions *)
type _lab = int (* dispatch labels *)
type 'a ev = (* early values, aka coroutines.  'a is the type of values eventually computed by the coroutines *)
  | Yk_more of _uid * (_lab -> _pos -> 'a ev)
  | Yk_box of (_pos -> Yak.YkBuf.t -> (int * 'a ev) option)
  | Yk_when of bool
  | Yk_bind of ('a ev -> 'a ev)
  | Yk_done of 'a
let ev_compare x y =
  match x,y with
  | Yk_more(c1,_), Yk_more(c2,_) -> compare c1 c2
  | Yk_done c1, Yk_done c2 -> compare c1 c2 (* TODO: user-supplied compare *)
  | Yk_more _, Yk_done _ -> 1
  | Yk_done _, Yk_more _  -> -1
  | _,_ -> failwith \"TODO sv_compare\"
let _t_count = ref 0
let _fresh_t_id () =
  let count = !_t_count in
  incr _t_count;
  count
let _t f = Yk_more(_fresh_t_id(),f)
type sv = _wv ev
let sv0 = Yk_done _wv0
let sv_compare = ev_compare

let sv_hash = Hashtbl.hash

(* Coroutine transformers *)
let _d x p = function
    Yk_more(_,t) -> t x p
  | _ -> failwith \"_d\"
let _darg x p = function
    Yk_more(_,t) -> t x p
  | _ -> failwith \"_darg\"
let _dbox x = function
    Yk_more(_,t) -> (fun p -> (match t x p with Yk_box b -> b p
                               | _ -> failwith \"_dbox\"))
  | _ -> failwith \"_dbox\"
let _dwhen x p = function
    Yk_more(_,t) -> (match t x p with Yk_when b -> b | _ -> failwith \"_dwhen\")
  | _ -> failwith \"_dwhen\"
let _dnext x p = function
    Yk_more(_,t) -> t x p
  | _ -> failwith \"_dnext\"
let _dret x p =
  (function
    | Yk_more(_,t) ->
        (fun r ->
          (match t x p with
          | Yk_bind(f) -> f r
          | _ -> failwith \"_dret1\"))
    | _ -> failwith \"_dret2\")
"

let add_late_prologue gr =
  add_to_prologue gr
"
(*LATE PROLOGUE*)
type _pos = int (* input positions *)
let hv_compare = Yk_History.compare
type sv = (int * hv * _pos, Yak.History.label) Yak.History.history
let sv0 = Yk_History.new_history()
let sv_compare = hv_compare
let sv_hash = Yk_History.hash
"

let all_prologue = "let sv_eq x y = sv_compare x y = 0
let key_eq (i,v1) (j,v2) = i = j &&  sv_eq v1 v2
let key_hash (i,v) = i lxor (sv_hash v)

(** Hashtable for top-down parsing. *)
module TDHashtable = Hashtbl.Make(struct type t = int * sv let equal = key_eq let hash = key_hash end)

"

let transform_grammar gr =

  (match gr.grammar_early_relevant,gr.grammar_late_relevant with
  | true,true ->
      add_early_late_prologue gr
  | true,false ->
      add_to_prologue gr early_prologue
  | false,true ->
      add_late_prologue gr
  | false,false ->
      add_no_early_or_late_prologue gr);
  add_to_prologue gr all_prologue;

  let disp          = Printf.sprintf "_d %d" in
  let disp_when     = Printf.sprintf "_dwhen %d" in
  let disp_box      = Printf.sprintf "_dbox %d" in
  let disp_next     = Printf.sprintf "_dnext %d" in
  let disp_ret      = Printf.sprintf "_dret %d" in
  let disp_arg      = Printf.sprintf "_darg %d" in
  let disp_merge    = Printf.sprintf "_dmerge %d" in
  let merge         = Printf.sprintf "_m %d" in

  let push l e =
    Gil.Action(Printf.sprintf "_p %d (%s)" l e) in
  let disp_delay l    =
    Gil.Action(Printf.sprintf "_ddelay %d" l) in
  let hist_empty = "_e" in

  let current = ref 1000 in
  let fresh() = Util.postincr current in

  (* translate non-early-relevant Gul to Gil.  The Gul rhs might be late-relevant. *)
  let rec gul2gil r =
    match r.r with
    | Symb(n, None, [], _) ->
        if r.a.late_relevant then
          Gil.Symb(n, Some hist_empty, Some (merge (fresh())))
        else
          Gil.Symb(n, None, None)
    | CharRange(x, y) ->
        Gil.CharRange(x, y)
    | Lit(x, y) ->
        Gil.Lit(x, y)
    | Opt(r1) ->
        Gil.Alt(Gil.Lit(false, ""), gul2gil r1)
    | Alt(r1, r2) ->
        Gil.Alt(gul2gil r1, gul2gil r2)
    | Action(None, _) ->
        (* Late actions are possible but will have been taken care of by replay *)
        Gil.Lit(false, "")
    | Seq(r1, None, _, r2) ->
        (* There may be a late binding but this will have been handled by replay *)
        Gil.Seq(gul2gil r1, gul2gil r2)
    | Star(Bounds(0, Infinity), r1)
    | Star(Accumulate(None,_), r1) ->
        Gil.Star(gul2gil r1)
    | Lookahead(b, r1)  ->
        Gil.Lookahead(b, gul2gil r1) (* TODO: cf Dispatch, a little different *)
    | Delay(false, e,_) ->
        push (fresh()) e
          (* The cases below are early relevant, gul2gil should not be called on early relevant rhs *)
    | Seq(_, Some _, _, _) -> Util.impossible "Coroutine.gul2gil.Seq"
    | Star _      ->
        Printf.eprintf "\n%s\n" (Pr.rule2string r);
        Util.impossible "Coroutine.gul2gil.Star"
    | Delay(true, _,_) -> Util.impossible "Coroutine.gul2gil.Delay"
    | Box _       -> Util.impossible "Coroutine.gul2gil.Box"
    | Assign _    -> Util.impossible "Coroutine.gul2gil.Assign"
    | Action _    -> Util.impossible "Coroutine.gul2gil.Action"
    | When _      -> Util.impossible "Coroutine.gul2gil.When"
    | DBranch (e,c)   -> (* TODO-dbranch Util.impossible "Coroutine.gul2gil.DBranch". i.e., allow for relevant dbranches *)
        (* note: presently, this is the only real use of dbranch. *)
        Gil.DBranch(e, c, "")
    | Symb(n,Some _,   _,     _) -> Util.impossible (Printf.sprintf "Coroutine.gul2gil.Symb(%s) with early arguments" n)
    | Symb(n,     _,_::_,     _) -> Util.impossible (Printf.sprintf "Coroutine.gul2gil.Symb(%s) with attributes" n)
          (* The cases below should have been desugared *)
    | Position _  -> Util.impossible "Coroutine.gul2gil.Position"
    | Hash _      -> Util.impossible "Coroutine.gul2gil.Hash"
    | Rcount _    -> Util.impossible "Coroutine.gul2gil.Rcount"
    | Minus _     -> Util.impossible "Coroutine.gul2gil.Minus"
    | Prose _     -> Util.impossible "Coroutine.gul2gil.Prose" in

  let ulam = function
    | [] -> Util.impossible "Coroutine.transform.ulam([])"
    | [(label,body)] ->
        if !Compileopt.check_labels then
          Printf.sprintf "(fun %d pos_ -> %s)" label body
        else
          (* Could just use next case, but this prints a bit more nicely *)
          Printf.sprintf "(fun _(*%d*) pos_ -> %s)" label body (* prevent match warning *)
    | cases ->
        let b = Buffer.create 11 in
        Printf.bprintf b "(function\n ";
        let rec loop = function [] -> Util.impossible "Coroutine.transform.ulam"
          | (label,body)::[] ->
              if !Compileopt.check_labels then
                Printf.bprintf b "| %d ->\n (fun pos_ -> %s)" label body
              else
                Printf.bprintf b "| _(*%d*) ->\n (fun pos_ -> %s)" label body (* prevent match warning *)
          | (label,body)::tl ->
              Printf.bprintf b "| %d ->\n (fun pos_ -> %s)\n " label body;
              loop tl
        in loop cases;
        Printf.bprintf b ")";
        Buffer.contents b in
  let t_ulam l = "_t"^(ulam l) in

  (* invariant: c is only applied to early-relevant r, and returns a non-empty list * Gil.rhs *)
  let rec c r k =
    match r.r with
    | Action (Some e,_) ->
        let label = fresh() in
        [(label, k(e))], Gil.Action(disp(label))

    | Alt(r1,r2) ->
        (* NB both r1 and r2 are early_relevant b/c of force_early_alts *)
        let alts1, gil1 = c r1 k in
        let alts2, gil2 = c r2 k in
        alts1@alts2, Gil.Alt(gil1,gil2)

    | Seq(r1,early,_,r2) ->
        (match r1.a.early_relevant,r2.a.early_relevant with
        | true,true ->
            let x =
              (match early with Some x -> x
              | None -> Variables.fresh()) in (* this case handled by normalization in writeup *)
            let g = Variables.fresh() in
            let assignments = string_of_varset r1.a.early_assignments in

            let alts1, gil1 = c r1 (fun hole -> Printf.sprintf "%s (%s) %s" g hole assignments) in
            let alts2, gil2 = c r2 k in

            let label = fresh() in
            [(label,
              Printf.sprintf
                "let %s %s %s = %s in %s"
                g x assignments (t_ulam alts2)
                (t_ulam alts1))],

            Gil.Seq(Gil.Action(disp(label)),Gil.Seq(gil1,gil2))

        | true,false ->
            let alts1, gil1 = c r1 (fun x -> (k (Printf.sprintf "ignore(%s);_wv0" x))) in
            let gil2 = gul2gil r2 in

            alts1, Gil.Seq(gil1,gil2)

        | false,true ->
            let gil1 = gul2gil r1 in
            let alts2, gil2 = c r2 k in
            let alts =
              match early with
              | None ->
                  alts2

              | Some x ->
                  Printf.eprintf "Warning: binding to _wv0 in %s\n%!" (Pr.rule2string r);
                  let label = fresh() in
                  [(label, Printf.sprintf "let %s=(_wv0) in %s" x (t_ulam(alts2)))] in
            alts, Gil.Seq(gil1,gil2)
        | false,false ->
            (* this case is impossible because our analysis marks this as not early relevant,
               regardless of whether there is a variable binding *)
            Util.impossible "Impossible case in coroutine transformation: not early relevant")

    | Symb(n,Some e,_,_) ->
        let post = fresh() in
        let eta k =
          let x = Variables.fresh() in
          Printf.sprintf "Yk_bind(function Yk_done(%s) -> %s | _ -> failwith \"bind=%d\")" x (k(x)) post in
        let pre = fresh() in
        let alts =
          [(pre, yk_done e);
           (post, eta k)] in
        let gil =
          if r.a.late_relevant then
            Gil.Symb(n, Some(disp_arg pre), Some(disp_merge post))
          else
            Gil.Symb(n, Some(disp_arg pre), Some (disp_ret post)) in
        alts, gil

    | Symb(n,None,_,_) -> (* nonterminal takes no arguments but returns an early result *)
        let post = fresh() in
        let eta k =
          let x = Variables.fresh() in
          Printf.sprintf "Yk_bind(function Yk_done(%s) -> %s | _ -> failwith \"bind=%d\")" x (k(x)) post in
        let alts = [(post, eta k)] in
        let gil =
          if r.a.late_relevant then
            Gil.Symb(n, Some(hist_empty), Some(disp_merge post))
          else
            Gil.Symb(n, None, Some (disp_ret post)) in
        alts, gil

    | Position true ->
        let label = fresh() in
        [(label,k("pos_"))], (* pos_ is bound later by t_ulam; note that k cannot capture variables *)
        Gil.Action(disp(label))

    | Box(e,_,bn) ->
        let label = fresh() in
        [(label, yk_box e k)],
        Gil.Box(disp_box(label), bn)

    | Delay(needs_env,e,_) ->
        let label = fresh() in
        let alts = [(label, yk_delay (k"(_wv0)") e)] in
        let gil =
          if needs_env then
            disp_delay(label)
          else
            push label e in
        alts, gil

    | When e ->
        let pre = fresh() in
        let post = fresh() in
        let alts =
          [(pre, yk_when e);
           (post, k("(_wv0)"))] in
        let gil =
          Gil.When(disp_when pre, disp_next post) in
        alts, gil

    | Assign(_,_,Some _) ->
        Util.impossible "Late attribute assignment in Coroutine should have been eliminated by Replay"
    | Assign(r1,None,_) ->
        c r1 k
    | Assign(r1,Some x,_) ->
        let label = fresh() in
        if r1.a.early_relevant then
          let g = Variables.fresh() in
          let alts1, gil1 = c r1 (fun hole -> Printf.sprintf "%s(%s)" g hole) in
          let alts =
            [(label,
              Printf.sprintf "let %s %s = %s in %s" g x (k("(_wv0)")) (t_ulam alts1))] in
          let gil = Gil.Seq(Gil.Action(disp(label)),gil1) in
          alts, gil
        else
          (Printf.eprintf "Warning: assigning _wv0 in %s\n%!" (Pr.rule2string r);
           [(label,
             Printf.sprintf "let %s=(_wv0) in %s" x (k("(_wv0)")))],
           Gil.Seq(Gil.Action(disp(label)),gul2gil r1))

    | DBranch(e,c) ->

        let pre = fresh() in
        let post = fresh() in
        let alts =
          if !Compileopt.late_only_dbranch then
            Util.impossible "Invariant violated: coroutine transform on late-only dbranch."
          else
            let e = yk_done e in
            let eta k =
              let x = Variables.fresh() in
              Printf.sprintf "Yk_bind(function Yk_done(%s) -> %s | _ -> failwith \"bind-%d\")" x (k(x)) post in
            [(pre, e);
             (post, eta k)] in
        let gil =
          if !Compileopt.late_only_dbranch then
            (* TODO-dbranch: make it possible for late. *)
            Util.impossible "Coroutine.DBranch"
          else Gil.DBranch(disp_arg pre, c, disp_ret post) in
        alts, gil

    | Opt r1 ->
        Util.impossible "Coroutine.coroutine.Opt"
    | Star(loop_condition,r1) ->
        let g = Variables.fresh() in
        let assignments = string_of_varset r1.a.early_assignments in
        let alts1, gil1 = (c r1 (fun hole -> Printf.sprintf "%s (%s) %s" g hole assignments)) in
        let pre = fresh() in
        let post = fresh() in

        let alts =
          let (x,e) =
            (match loop_condition with
            | Accumulate(Some(x,e),_) -> x,e
            | Accumulate(None,_) (* in this case r1 must be early relevant so we need to track pre and post anyway *)
            | Bounds _ -> Variables.fresh(),"_wv0"
                  (* This case of Bounds together with an early-relevant r1 can only occur if lifting was skipped. Otherwise,
                     the star would have been lifted to accumulate a list of values. *)
            ) in
          [(pre,
            Printf.sprintf
              "let rec %s %s %s = %s in %s (%s) %s"
              g x assignments (t_ulam((post,k(x))::alts1))
              g e assignments)] in
        let gil =
          Gil.Seq(Gil.Action(disp(pre)),
                  Gil.Seq(Gil.Star(gil1),
                          Gil.Action(disp(post)))) in

        alts, gil

    | Position false
    | Action(None,_)
    | CharRange(_,_)
    | Prose(_)
    | Lookahead _
    | Lit _  ->
        Util.impossible "Impossible case 2 in coroutine transformation: not early relevant"
          (* should have been desugared *)
    | Rcount(_,_)
    | Hash(_,_)
    | Minus(_,_) ->
        Util.impossible "Impossible case 3 in coroutine transformation: not desugared" in

(*
  let fname n = Printf.sprintf "_co_%s" (Variables.bnf2ocaml n) in
  let first = ref true in
  let pr fmt = Printf.ksprintf (add_to_prologue gr) fmt in
*)

  gr.gildefs <-
    List.concat
      (List.map
         (function
             RuleDef(n,r,a) ->
               if not(gr.grammar_early_relevant) || not(r.a.early_relevant) then [(n,gul2gil r)] else
               begin
                 let alts, gil = c r (fun x->"Yk_done("^x^")") in
                 let c =
                   (match gr.grammar_late_relevant,a.Attr.early_params with
                   | (false,None) ->
                       Printf.sprintf "(fun _(*pos*) _(*arg of %s*) -> %s)" n (t_ulam alts)
                   | (true,None) ->
                       let h = Variables.fresh() in
                       Printf.sprintf "(fun _(*pos*) (_,%s)(*arg of %s*) -> (%s,%s))" h n (t_ulam alts) h
                   | (false,Some x) ->
                       Printf.sprintf "(fun _(*pos*) -> (function Yk_done(%s) -> %s\n| _ -> failwith \"%s\"))"
                         x (t_ulam alts) n
                   | (true,Some x) ->
                       let h = Variables.fresh() in
                       Printf.sprintf "(fun _(*pos*) -> (function (Yk_done(%s),%s) -> (%s,%s)\n| _ -> failwith \"%s\"))"
                         x h (t_ulam alts) h n) in
                 let coroutine = Variables.fresh() in
                 add_to_prologue gr (Printf.sprintf "let %s =\n %s\n" coroutine c);
                 [(n, Gil.Seq(Gil.Action(coroutine), gil))]
               end
(*
               if not(gr.grammar_early_relevant) || not(r.a.early_relevant) then
                 [(n,gul2gil r)]
               else begin
                 pr "%s %s "
                   (if !first then
                     (first := false;
                      "\nlet rec\n ")
                   else
                     "\nand\n ")
                   (fname n);

                 (match a.Attr.early_params with
                   Some x -> pr "(%s) " x
                 | None -> ());

                 pr "= ";

                 let alts, gil = c r (fun x->"Yk_done("^x^")") in

                 if gr.grammar_late_relevant then
                   let h = Variables.fresh() in
                   pr "(fun _(*pos*) (_,%s)(*arg of %s*) -> (%s,%s))" h n (t_ulam alts) h
                 else
                   pr "(fun _(*pos*) _(*arg of %s*) -> %s)" n (t_ulam alts);

                 [(n, gil)]
               end
*)
           | _ -> [])
         gr.ds)
