(*******************************************************************************
 * Copyright (c) 2010, 2011 AT&T.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Trevor Jim and Yitzhak Mandelbaum
 *******************************************************************************)

open Yak
let lookahead_name = "__lookahead"
let nullable_pred_prefix = "nullable_"
let nullable_pred_table_prefix = "npt_"

let mk_var c = Printf.sprintf "_x%d_" c
let mk_pvar c = Printf.sprintf "_p%d_" c
let mk_npname n = Variables.bnf2ocaml (nullable_pred_prefix ^ n)
let mk_nptblname n = Variables.bnf2ocaml (nullable_pred_table_prefix ^ n)


(******************************************************************************)

module DBL = Meta_prog.DB_levels
open Meta_prog.PHOAS


(******************************************************************************)

(******************************************************************************)

(** Convenience constructors *)

let app2 x y z = App (App (x,y), z)
let app3 f x y z = App (App (App (f, x), y), z)
let lam2 f = Lam (fun x -> Lam (f x))
let lam3 f = Lam (fun x -> lam2 (f x))
let none = Con ("None", [])
let some e = Con ("Some", [e])

let gil_callc =
  (* The generated code binds expression to avoid capture. There are
     no (possibly) open expressions in the body of the generated
     code. *)
  Printf.sprintf "(let symb_pred = %s
       and f_call = %s
       and f_ret = %s
    in
    fun la ykb v ->
     let p = Yak.YkBuf.get_offset ykb in
     match symb_pred la ykb (f_call p v) with
        None -> None
      | Some v2 -> Some (f_ret p v v2))"

let callc nt arg_opt merge_opt =
  let name = mk_npname nt in
  match arg_opt, merge_opt with
    | None,   None ->
        Printf.sprintf "(fun la ykb v -> \
                          match %s la ykb sv0 with \
                            | None -> None \
                            | Some _ -> Some v)" name
    | Some e, None ->
        Printf.sprintf "let f_call = %s in \
                         (fun la ykb v -> \
                         let p = Yak.YkBuf.get_offset ykb in
                         match %s la ykb (f_call p v) with
                            None -> None
                          | Some _ -> Some v)" e name
    | None, Some f ->
        Printf.sprintf "let f_ret = %s in \
                         (fun la ykb v -> \
                         let p = Yak.YkBuf.get_offset ykb in
                         match %s la ykb sv0 with
                            None -> None
                          | Some v2 -> Some (f_ret p v v2))" f name
    | Some e, Some f -> gil_callc name e f



(******************************************************************************)

type nt_namespace =
  | Orig_nt        of string
  | Null_nt     of string
  | Substantive_nt of string

(* For reasons of type-var generalization, we use Lam constructors directly,
   rather than using lam3. *)
let ignore_binder_e = Lam (fun p -> Lam (fun x -> Lam (fun y -> Var x)))
let app_iv f ykb v = app2 (InjectE f) (App (InjectE "Yak.YkBuf.get_offset", Var ykb)) (Var v)

let null_nt_name nt = Null_nt nt
let subs_nt_name nt = Substantive_nt nt

(** Inject a Gil s_rhs into a Gil rhs with nt_namespace as the
    nonterminal type. All nonterminals are assumed to inhabit the
    Orig_nt namespace. *)
let rec ns_inject (r : ('a, string) Gil.rhs) : ('a, nt_namespace) Gil.rhs =
  let g = function
    | Gil.Symb (nt, x, y) -> Gil.Symb (Orig_nt nt, x, y)
    | Gil.Lookahead (b, r) -> Gil.Lookahead (b, ns_inject r)
    | r -> Gil.rebuild r in
  Gil.map g r

(** Inject a Gil s_rhs into a Gil rhs with nt_namespace as the
    nonterminal type, assuming all nonterminals inhabit the Null_nt
    namespace, except for those in Lookahead. *)
let rec null_ns_inject (r : ('a, string) Gil.rhs) : ('a, nt_namespace) Gil.rhs =
  let g = function
    | Gil.Symb (nt, x, y) -> Gil.Symb (Null_nt nt, x, y)
    | Gil.Lookahead (b, r) -> Gil.Lookahead (b, ns_inject r)
    | r -> Gil.rebuild r in
  Gil.map g r

(** Project a Gil s_rhs from a Gil rhs with
    nt_namespace as the nonterminal type. *)
let rec ns_project (r : ('a, nt_namespace) Gil.rhs) : ('a, string) Gil.rhs =
  let g = function
  | Gil.Symb (Orig_nt nt, x, y) -> Gil.Symb (nt, x, y)
  | Gil.Symb (Substantive_nt nt, x, y) -> Gil.Symb ("SUBS::" ^ nt, x, y)
  | Gil.Symb (Null_nt nt, x, y) -> Gil.Symb ("NULL::" ^ nt, x, y)
  | Gil.Lookahead (b, r) -> Gil.Lookahead (b, ns_project r)
  | r -> Gil.rebuild r in
  Gil.map g r

let inject_dbranch f1 c f2 =
  if c.Gil.arity = 0 then
    InjectE (Printf.sprintf
               "let f1 = %s and f2 = %s in\n\
                          fun _ ykb v -> match f1 v with\n\
                            | Yk_done %s %s -> Some (f2 v ()) | _ -> None"
               f1 f2 c.Gil.cty c.Gil.cname)
  else
    let vars = Util.list_make c.Gil.arity (Printf.sprintf "v%d") in
    let pattern = String.concat ", " vars in
    InjectE (Printf.sprintf
               "let f1 = %s and f2 = %s in\n\
                          fun _ ykb v -> match f1 v with\n\
                            | Yk_done %s %s (%s) -> Some (f2 v (%s)) | _ -> None"
               f1 f2 c.Gil.cty c.Gil.cname pattern pattern)

(** Dummy value. Invalid arguments in When make sure that we catch
    it if it escapes use in this module.  For cases where we intend
    it to escape, we manually convert it to a valid form with [fix_fail],
    below. *)
let fail = Gil.When ("F", "F")

let fix_fail = function
  | Gil.When("F","F") -> Gil.When("fun _ _ -> false", "fun _ v -> v")
  | r -> r

(** Derive the epsilon subgrammar of a rhs. *)
let derive_eps r =
  let open Gil in
  let nullify =
    Printf.sprintf "let f = %s in\n\
    fun v pos ykb ->\n\
    match f v pos ykb with
    | (Some (0, _)) as x -> x
    | _ -> None" in
  let predify e = "let p = " ^ e ^ " in fun _ _ -> p" in
  let rec trans2 = function
    | Symb (nt, x, y) -> Symb (nt, x, y)
    | ( Lit (_, "")
      | Action _
      | When _
      | Box (_, Always_null)
      | When_special _
      ) as r -> r
    | Lookahead (b,r) -> Lookahead (b, r)
    | Lit _ -> fail
    | CharRange _ -> fail
    | Box (_, Never_null) -> fail
    | Box (e, Runbox_null) -> Box (nullify e, Always_null)
    | Box (_, Runpred_null e) -> When_special (predify e)
    | DBranch _ as r ->
        if !Compileopt.late_only_dbranch then fail
        else r
    | Seq (r1,r2) -> Seq (trans2 r1, trans2 r2)
    | Alt (r1,r2) -> Alt (trans2 r1, trans2 r2)
    | Star r -> Star (trans2 r) in
  trans2 r

(** Derive the non-epsilon (substantive) subgrammar of a rhs. *)
let derive_subs r =
  let open Gil in
  let substantiate =
    Printf.sprintf "let f = %s in\n\
    fun v pos ykb ->
    match f v pos ykb with
    | Some (0, _) -> None
    | x -> x" in
  let rec trans3 = function
    | Symb (nt, x, y) -> Symb (subs_nt_name nt, x, y)
    | ( Lit (_, "")
      | Action _
      | When _
      | Box (_, Always_null)
      | When_special _
      | Lookahead _ ) -> fail
    | ( Lit _
      | CharRange _
      | Box (_, Never_null)
      ) as r -> rebuild r
    | Box (f, Runbox_null) -> Box (substantiate f, Never_null)
    | Box (f, Runpred_null _) -> Box (substantiate f, Never_null)
    | DBranch _ as r ->
        if !Compileopt.late_only_dbranch then rebuild r
        else fail
    | Seq (r1,r2) -> Alt (Seq (trans3 r1, ns_inject r2), Seq (null_ns_inject (derive_eps r1), trans3 r2))
    | Alt (r1,r2) -> Alt (trans3 r1, trans3 r2)
    | Star r -> Seq (trans3 r, Star (ns_inject r)) in
  trans3 r

(** epsilon-equivalence for epsilon right-sides. *)
let rec eps_is_eps_eq =
  let open Gil in
  function
  | Symb (_, _, None) -> true (* grammars are assumed to be pure, so if the
                                 result is not bound, it does not have semantic impact. *)
  | Symb (_, _, Some _) -> false
  | ( Lit (_, "") | Lookahead _) -> true
  | ( Action _ | When _ | Box _ | When_special _) -> false
  | Seq (r1,r2) -> eps_is_eps_eq r1 && eps_is_eps_eq r2
  | Alt (r1,r2) -> eps_is_eps_eq r1 && eps_is_eps_eq r2
  | Star r -> eps_is_eps_eq r
  | DBranch _  ->
      if !Compileopt.late_only_dbranch then
        invalid_arg "terminals should not appear in epsilon right-sides."
      else false
  | ( Lit _ | CharRange _ ) -> invalid_arg "terminals should not appear in epsilon right-sides."

let noneps_is_eps_eq r = false

(** Rewrite a gil rhs based on rules relating to "fail" and epsilon.
    The rewrite rules relating to epsilon depend on a predicate [is_irr]
    to determine whether the rule is equivalent to epsilon.
*)
let rewrite nt_defs is_eps_eq r =
  let open Gil in
  let rec rewrite' = function
    | Symb (nt, _, _) as r_orig ->
        (try
           let r_nt = nt_defs nt in
           match r_nt with
              | (Lit (_, "") | When ("F", _)) -> r_nt
              | _ -> r_orig
         with Not_found -> r_orig)
    | Seq (r1, r2) ->
        let r1 = rewrite' r1 in
        let r2 = rewrite' r2 in
        (match (r1, r2) with
           | (When ("F", _), _) -> r1
           | (_, When ("F", _)) -> r2
           | (_, Lit (_,"")) -> r1
           | (Lit (_,""), _) -> r2
           | _ -> Seq (r1, r2))
    | Alt (r1, r2) ->
        let r1 = rewrite' r1 in
        let r2 = rewrite' r2 in
        (match (r1,r2) with
           | (When ("F", _), _) -> r2
           | (_, When ("F", _)) -> r1
           | (_, Lit (_,"")) when is_eps_eq r1-> r2
           | (Lit (_,""), _) when is_eps_eq r2 -> r1
           | _ -> Alt (r1, r2))
    | Star r1 ->
        (match rewrite' r1 with
           | When ("F", _) -> Lit (false, "")
           | r when is_eps_eq r -> Lit (false, "")
           | r -> Star r)
    | (Lit _ | CharRange _ | Action _ | When _ | When_special _ | DBranch _ | Box _ | Lookahead _) as r -> r
  in
  rewrite' r


(** Conservative determination of when an epsilon right-side is okay
    to inline.

    TODO: make this a bit smarter: inline any single element other than Symb.
*)
let rec can_inline_eps =
  let open Gil in function
  | ( Lit (_,"") | Lookahead _ ) -> true
  | ( Lit _
    | Symb _
    | Action _
    | When _
    | Box _
    | CharRange _
    | When_special _
    | DBranch _ ) -> false
  | ( Seq (r1,r2) | Alt (r1,r2) ) ->
      can_inline_eps r1 && can_inline_eps r2
  | Star r1 -> can_inline_eps r1

(** [prep_for_inlining r] prepares the (epsilon) right-side [r] for inlining.
    It translates the rhs [r] into a new rhs that is either a nullability predicate
    containing a parsing function, or a rhs with no nonterminals. *)
let prep_for_inlining nt arg_opt merge_opt r =
  let open Gil in
  (* check that it is free of calls and sem-val transformations. *)
  if can_inline_eps r then
    match merge_opt with
      | None -> r
      | Some f ->
          let r_b = match arg_opt with
            | None -> Action ("let f = " ^ f ^ " in (fun p v -> f p v sv0)")
            | Some e ->
                Action ("let e = " ^ e
                            ^ " and f = " ^ f
                            ^ " in (fun p v -> f p v (e p v))") in
          Seq (r, r_b)
  else
    When_special (callc nt arg_opt merge_opt)

(* TODO_DOC:...

   The inlining ensures that no regular nonterminal names will remain. All
   such references should have been split into substantive names and
   inlined epsilon definitions. So, we can also remap subs. names to regular
   names in the same transformation. *)
let inline_eps_and_rename eps_defs r =
  let open Gil in
  let rec recur r =
    let rhs_inline_leaf = function
      | Symb (Orig_nt nt, arg_opt, merge_opt) ->
          (* Split the nonterminal and then inline the null part. *)
          let nt_subs = Symb (nt, arg_opt, merge_opt) in
          (match eps_defs nt with
               (* optimize case of non-nullable nonterminal. *)
             | When ("F", "F") -> nt_subs
             | nt_eps ->
                 let nt_eps = prep_for_inlining nt arg_opt merge_opt nt_eps in
                 Alt (nt_subs, nt_eps))
      | Symb (Substantive_nt nt, arg_opt, merge_opt) ->
          Symb (nt, arg_opt, merge_opt)
      | Symb (Null_nt nt, arg_opt, merge_opt) ->
          let nt_eps = eps_defs nt in
          (match nt_eps with
               (* optimize case of non-nullable nonterminal. *)
             | When ("F", "F") -> nt_eps
             | nt_eps -> prep_for_inlining nt arg_opt merge_opt nt_eps)
      | Lookahead (b, r) -> Lookahead (b, recur r)
      | r -> rebuild r in
    map rhs_inline_leaf r in
  recur r

(** [rewrite_recursion tbl] rewrites recursive symbols recorded in
    [tbl] to fail.  Given a grammar in which definitions do not
    contain alternatives, any recursive symbols must be empty. So,
    we can rewrite them accordingly. This rewriting it important if
    we want to translate the grammar to a recursive-descent parser.

    If the input grammar in [tbl] contains alternatives (except in
    negative lookahead), then this rewriting may be incorrect. So,
    in that case, we raise [Invalid_argument].

    Negative lookahead is excluded because recursion through
    negative lookahead does not force a nonterminal to have an empty
    language. Simply put, if the language is really empty, then the
    lookahead will always *succeed*, so it doesn't contribute to the
    language's emptyness. Therefore, we don't even check right-sides
    in negative lookahead positions, so the presence of alts therein
    is irrelevant.
*)
let rewrite_recursion tbl =
  let open Gil in
  let rec assert_no_alts = function
    | Alt _ -> invalid_arg "Right-side contains Alt."
    | Star _ -> invalid_arg "Right-side contains Star."
    | Seq (r1,r2) -> assert_no_alts r1; assert_no_alts r2
    | Lookahead (true, r) -> assert_no_alts r
    | _ -> () in
  let ds = Hashtbl.fold (fun n r ds -> (n,r) :: ds) tbl [] in
  let recgraph = Analyze.Gil.reachable_graph_nnla ds in
  List.iter (fun (n,r) ->
               if Analyze.Gil.is_rec n recgraph then
                 begin
                   assert_no_alts r;
                   Hashtbl.replace tbl n fail
                 end) ds

(* Driver that applies all various above functions:
   neg <- non-eps grammar.
   eg <- eps grammar.
   eg <- rewrite eg.
   g, neg, eg.
   neg <- rename_neps (inline_eps eg neg)
   return start-def + neg

   Important to sort first, then remove the start definition in order to
   get the topo. sort correct.

    TODO: The recursion-related rewriting might result in more
    opportunities to apply the normal rewrite rules. So, ideally,
    we'd have some sort of iteration happen here. At least, we could
    change the treatment of alternatives. Instead of creating the
    recursion graph, and then asserting lack of alternatives, we could a priori
    build the recursion graph such that it excluded cycles through alternatives.
*)
let eliminate_nullables gr =
  let attrs = Gul.attribute_table_of_grammar gr in
  let ds = Gil.sort_definitions gr.Gul.gildefs in
  let start_def, ds = Gil.remove_definition gr.Gul.start_symbol ds in
  let eg_tbl = Hashtbl.create 101 in
  let eps_defs = Hashtbl.find eg_tbl in
  let eps_defs_total nt =
    try Hashtbl.find eg_tbl nt
    with Not_found -> Util.warn_undefined nt; fail in

  let sg = List.map (fun (n,r) ->
                       let r = match (Hashtbl.find attrs n).Gul.Attr.nullability with
                         | Gul.Attr.N.Always_null -> fail
                         | Gul.Attr.N.Never_null -> ns_inject r
                         | Gul.Attr.N.Unknown -> derive_subs r in
                       (n, r)) ds in

  (* Step 1: initialize the table with epsilon subgrammars. *)
  List.iter (fun (n,r) ->
               let r = match (Hashtbl.find attrs n).Gul.Attr.nullability with
                 | Gul.Attr.N.Always_null -> r
                 | Gul.Attr.N.Never_null -> fail
                 | Gul.Attr.N.Unknown -> derive_eps r in
               Hashtbl.add eg_tbl n r) ds;
  (* Step 2: rewrite the entries, according to the order of [ds]. *)
  List.iter (fun (n,_) ->
               let r = rewrite eps_defs eps_is_eps_eq (eps_defs n) in
               Hashtbl.replace eg_tbl n r) ds;
  (* Step 2.5: rewrite the entries again. Second time does the trick for all test grammars. *)
  List.iter (fun (n,_) ->
               let r = rewrite eps_defs eps_is_eps_eq (eps_defs n) in
               Hashtbl.replace eg_tbl n r) ds;

  (* Step 3: eliminate recursion from the epsilon grammar, assuming that
     alternatives are already gone (via rewriting).  TODO: handle
     case where alternatives have not, or cannot, be eliminated.  *)
  rewrite_recursion eg_tbl;

  let ds = List.map (fun (n,r) -> (n, inline_eps_and_rename eps_defs_total r)) sg in

  (* Rewrite subs. entries to percolate [fail]. *)
  let ds_tbl = Hashtbl.create 101 in
  let ds_defs = Hashtbl.find ds_tbl in
  List.iter (fun (n,r) -> Hashtbl.add ds_tbl n r) ds;
  List.iter (fun (n,_) ->
               let r = rewrite ds_defs noneps_is_eps_eq (ds_defs n) in
               Hashtbl.replace ds_tbl n r) ds;
  let ds = List.rev_map (fun (n,_) -> (n, fix_fail (ds_defs n))) ds in
  let start_def = inline_eps_and_rename eps_defs_total (ns_inject start_def) in
  let ds = List.rev ((gr.Gul.start_symbol, start_def) :: ds) in
  ds, eg_tbl

(** [compile r] compiles the (null) rhs [r] into a parsing function with a
    nullability predicate signature. *)
let compile get_action get_start memo_cs r =
  let open Gil in
  let open Util.Operators in
  let predify_box f = InjectE("Pred3.boxc (" ^ f ^ ")") in
  (** [e1] and [e2] are closed expressions. *)
  let rec recur = function
      | Symb (nt, arg, binder) -> InjectE (callc nt arg binder)
      | Lit (_, "") -> lam3 (fun la ykb v -> some (Var v))
      | Action f -> lam3 (fun la ykb v -> some (app_iv f ykb v))
      | When (f_pred, f_next) ->
          lam3 (fun x ykb v ->
                  Let (App (InjectE "Yak.YkBuf.get_offset", Var ykb), begin fun pos ->
                    if_then_else $| app2 (InjectE f_pred) (Var pos) (Var v)
                      $| some (app2 (InjectE f_next) (Var pos) (Var v))
                      $| none
                  end))
      | Box (f, Always_null) -> predify_box f
      | When_special p -> InjectE p
      | Lookahead (b, Gil.Symb(nt, None, None)) ->
          Util.warn Util.Sys_warn
            ("using extended lookahead for nonterminal " ^ nt ^ " in nullability predicate.");
          let n_act = get_action nt in
          InjectE (Printf.sprintf "Pred.full_lookaheadc %B %d %d" b n_act (get_start n_act))
      | Lookahead (b, r1) as r ->
          (match to_cs r1 with
             | Some la_cs ->
                 (* Complement w.r.t. 257 to account for EOF, which we
                    represent as character 256. *)
                 let cs = if b then la_cs else Cs.complement 257 la_cs in
                 let cs_code = memo_cs cs in
                 InjectE (Printf.sprintf "Pred.cs_lookaheadc (%s)" cs_code)
             | None ->
                 Util.error Util.Sys_warn
                   (Printf.sprintf "lookahead limited to character sets or argument-free symbols.\nRule: %s\n"
                      (Pr.Gil.Pretty.rule2string r));
                 InjectE "ERROR")
            (* TODO: expand error messages! *)
      | DBranch (f1, c, f2) ->
          if !Compileopt.late_only_dbranch then invalid_arg "Non-null ..."
          else inject_dbranch f1 c f2
      | Seq (r1,r2) ->
          let e1 = recur r1 in
          let e2 = recur r2 in
          lam3 begin fun la ykb v ->
            Case (app3 e1 (Var la) (Var ykb) (Var v),
                  [("None", 0), (fun [] -> Con ("None",[]));
                   ("Some", 1), (fun [v] -> app3 e2 (Var la) (Var ykb) (Var v))])
          end
      | (Lit _ | CharRange _ | Box (_, Never_null)) -> invalid_arg "Non-null ..."
      | Box _ -> invalid_arg "This box version should have been desugared."
      | (Alt _ | Star _) as r ->
          Util.warn Util.Sys_warn (Printf.sprintf
                                     "Potentially ambiguous right-sides in epsilon grammar:\n%s\n"
                                     (Pr.Gil.Pretty.rule2string r));
          invalid_arg "Potentially ambiguous ..." in
  {e = fun () -> recur r}

(* TODO: maybe get rid of memoization altogether? *)
let print_null_parsers ch get_action get_start is_sv_known eps_defs_tbl =
  (* Record which nonterminals are called from other nonterminals
     (including themselves) in the epsilon grammar. This set is only used
     to determine which functions should be memoized. *)
  let called_set = Hashtbl.fold (fun _ r s -> Gil.add_called_symbs s r) eps_defs_tbl (PSet.create compare) in

  let css = Hashtbl.create 11 in
  let memo_cs cs =
    try fst (Hashtbl.find css cs)
    with Not_found ->
      let x = Variables.freshn "cs" in
      let cd = Cs.to_code cs in
      Hashtbl.add css cs (x,cd); x in

  (* To ensure "let rec" compatibility, we force all generated code to be a syntactic function.
     We therefore eta-expand the expression resulting from compilation. To "prettify" the case
     where the expression is already a syntactic function, we simplify the eta-expanded value.
     Then, we
  *)
  (* If the nonterminal is called from another predicate, then there
     might be recursion and we should memoize the result. Otherwise,
     it doesn't need to be memoized. However, because it could be
     called directly from the grammar, it still needs a function. *)
  let select nt r acc =
    let ntcalled = PSet.mem nt called_set in
    (* if [e_p] is a boolean then it will have been inlined into any
       other calling contexts, so there's no reason to print the
       function. *)
    match r with
      | Gil.When ("F", "F") when not ntcalled -> acc
      | r when can_inline_eps r && not ntcalled -> acc
          (* first condition guarantees not called externally,
             while second guarantees not called internally. *)
      | _ ->
          let e_p = compile get_action get_start memo_cs (fix_fail r) in
          let e_eta = simplify {e = fun () ->
                          lam3 (fun la ykb v -> app3 (e_p.e ()) (Var la) (Var ykb) (Var v))} in
          let ykb = mk_pvar 0 in
          let v = mk_var 0 in
          let body = (open3 e_eta).e_open3 lookahead_name ykb v in
          let body_code = to_string' 1 body in
          let tbls, preds = acc in
          (* TODO: extend comment here to explain memoization process *)
          if ntcalled then begin
            let tbl = mk_nptblname nt in
            let pred = Printf.sprintf "%s %s %s %s =\n  \
              let __p1 = Yak.YkBuf.get_offset %s in\n    \
                try\n      \
                  let (r, __p2)  = SV_hashtbl.find %s %s in\n      \
                  if __p1 = __p2 then r else\n      \
                  let x = %s in SV_hashtbl.replace %s %s (x, __p1); x\n    \
                with Not_found ->\n      \
                  let x = %s in SV_hashtbl.add %s %s (x, __p1); x\n\n"
              (mk_npname nt) lookahead_name ykb v
              ykb
              tbl v
              body_code tbl v
              body_code tbl v in
            nt :: tbls, pred :: preds
          end
          else begin
            let pred = Printf.sprintf "%s %s %s %s = %s\n\n" (mk_npname nt) lookahead_name ykb v body_code in
            tbls, pred :: preds
          end in
  let tyannot = if is_sv_known then ": (sv option * int) SV_hashtbl.t" else "" in
  let print_table nt = Printf.fprintf ch "let %s %s = SV_hashtbl.create 11;;\n" (mk_nptblname nt) tyannot in
  let print_cs _ (varname, code) = Printf.fprintf ch "let %s = %s;;\n" varname code in
  let print_pred = Printf.fprintf ch "and %s" in
  let need_tbls, preds = Hashtbl.fold select eps_defs_tbl ([],[]) in
  match List.rev preds with
    | [] -> ()
    | p::preds ->
        if is_sv_known then
          Printf.fprintf ch "module SV_hashtbl = Hashtbl.Make(struct
                    type t = sv
                    let equal a b = sv_compare a b = 0
                    let hash = Hashtbl.hash end)\n"
        else Printf.fprintf ch "module SV_hashtbl = Hashtbl\n";
        Printf.fprintf ch "module Pred = Pred3\n";
        List.iter print_table need_tbls;
        Hashtbl.iter print_cs css;
        Printf.fprintf ch "let rec %s" p;
        List.iter print_pred preds
