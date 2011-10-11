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

open Yak
open Gul
open Attr
open Printf

let binder_prec = 30

let get_prec r = match r.r with
  (* Open: *)
  | Alt _ | Minus _ -> 5
  | Seq _ -> 10
  | Assign _ -> 11

  (* Open on the right:*)
  | Rcount _ | Star _ | Hash _ | Lookahead _ -> 15

  (* Closed. No possible ambiguity, so precedence is irrelevant.
     They can be considered as highest precedence since their syntactic form
     ensures no confusion. *)
  | Symb _
  | Opt _
  | Lit _
  | Position _
  | CharRange _
  | Prose _
  | Action _
  | Box _
  | Delay _
  | When _
  | DBranch _
    -> 1000

let compare_prec x y = (get_prec x) - (get_prec y)

let right_assoc x y = match (x,y) with
    5,5 -> true
  | _ -> false
let left_assoc x y = false
let non_assoc x y = false

let left x y = not (right_assoc x y or non_assoc x y)
let right x y = not (left_assoc x y or non_assoc x y)

(* [p] is a position predicate. [ok_parse] and [must_group] are inverses of each other.*)
let ok_parse child parent p = (child > parent) or (child = parent && p child parent)
let must_group child parent p = (child < parent) or (child = parent && not (p child parent))

let pr_repeat f = function
  | Num n -> bprintf f "%d" n
  | Infinity -> bprintf f "&"

let pr_boxnull f = function
    Always_null -> bprintf f "*"
  | Never_null -> bprintf f "+"
  | Runbox_null -> ()
  | Runpred_null e -> bprintf f "?{%s}" e

let pr_constr f {cname = n; arity=a; cty=cty;} = bprintf f "%s %d:%s" n a cty

let rec pr_rule_paren f r =
  begin bprintf f "("; pr_rule f r; bprintf f ")"; end

and pr_maybe_group f r r0 pos =
  if must_group (get_prec r) (get_prec r0) pos then pr_rule_paren f r else pr_rule f r

and pr_bound_rule f r =
  if must_group (get_prec r) binder_prec left then pr_rule_paren f r else pr_rule f r

and pr_rule f r =
  (match r.r with
  | Action(None,None) -> pr_rule f (mkLIT "")
  | Action(Some x, Some y) ->
      bprintf f "@${%s}{%s}" x y;
      Util.option () (bprintf f ":%s") r.a.inf_type
  | Action(early,late) ->
      Util.option ()
        (fun x -> bprintf f "@{%s}" x;
           Util.option () (bprintf f ":%s") r.a.inf_type)
        early;
      Util.option () (fun x -> bprintf f "{%s}" x) late   (* Omit the $ for brevity *)
  | When x ->
      bprintf f "@when(%s)" x
  | DBranch (e, c) ->
      bprintf f "@match(%s, %a)" e pr_constr c
  | Box(x,None,boxnull) ->
      bprintf f "@box%a(%s)" pr_boxnull boxnull x
  | Box(x,Some y,boxnull) ->
      bprintf f "@box@>(%s) %a(%s)" y pr_boxnull boxnull x
  | Delay(true,x,None) ->
      bprintf f "@delay(%s)" x
  | Delay(true,x,Some ty) ->
      bprintf f "@delay{%s}(%s)" ty x
  | Delay(false,x,None) ->       (* TODO: there is no user-level syntax for this *)
      bprintf f "@-delay(%s)" x
  | Delay(false,x,Some ty) ->     (* TODO: there is no user-level syntax for this *)
      bprintf f "@-delay{%s}(%s)" ty x
  | Prose x ->
      bprintf f "<%s>" x (* FIX: escape string? *)
  | Position true -> bprintf f "@pos"
  | Position false -> bprintf f "$pos"
  | Symb(x,y,z,w) ->
      (bprintf f "%s" x;
       (match y,z with
       | None,[] ->
           ()
       | None,_ ->
           bprintf f "@(;%s)"
             (String.concat ";" (List.map (fun (var,exp) -> var^"="^exp) z))
       | Some early_args,[] ->
           bprintf f "@(%s)" early_args;
       | Some early_args,_ ->
           bprintf f "@(%s;%s)" early_args
             (String.concat ";" (List.map (fun (var,exp) -> var^"="^exp) z)));
       (match w with None -> ()
       | Some late_args ->
           bprintf f "$(%s)" late_args));
      Util.option () (bprintf f ":%s") r.a.inf_type;
  | CharRange(low,high) ->
      bprintf f "%s" (Cs.to_string (Cs.range low (high+1)))
(*
      if low = high then bprintf f "%%d%d" low
      else bprintf f "%%d%d-%d" low high
*)
  | Lit(case_sensitive,x) -> (* TODO: need syntax to distinguish case sensitive and insensitive literals *)
      if x="\"" then bprintf f "<\">"
      else bprintf f "\"%s\"" x
  | Opt(r2) ->
      (bprintf f "["; pr_rule f r2; bprintf f "]")
  | Lookahead(true,r2) ->
      (bprintf f "&"; pr_maybe_group f r2 r right)
  | Lookahead(false,r2) ->
      (bprintf f "!"; pr_maybe_group f r2 r right)
  | Rcount(c,r2) ->
      (bprintf f "@repeat(%s)" c; pr_rule f r2)
  | Seq(r2,early,late,r3) ->
      if early=None && late=None then
        pr_maybe_group f r2 r left
      else
        pr_bound_rule f r2;
      Util.option ()
        begin fun v ->
          match r.a.inf_type with
            | None -> bprintf f "@%s" v
            | Some ty -> bprintf f "@(%s:%s)" v ty
        end
        early;
      Util.option () (bprintf f "$%s") late;
      bprintf f " ";
      pr_maybe_group f r3 r right
  | Assign(r2,early,late) ->
      bprintf f "(";
      if early=None && late=None then
        pr_maybe_group f r2 r left
      else
        pr_bound_rule f r2;
      Util.option () (bprintf f ">@%s)") early;
      Util.option () (bprintf f ">$%s)") late;
  | Alt(r2,r3) ->
      pr_maybe_group f r2 r left;
      bprintf f "|";
      pr_maybe_group f r3 r right
  | Star(Bounds(m,Num(n)),r2) ->
      if (m = n) then bprintf f "%d" m
      else if (m = 0) then bprintf f "*%d" n
      else bprintf f "%d*%d" m n;
      pr_maybe_group f r2 r right
  | Star(Bounds(m,Infinity),r2) ->
      if (m = 0) then bprintf f "*"
      else bprintf f "%d*" m;
      pr_maybe_group f r2 r right
  | Star(Accumulate(early,late),r2) ->
      bprintf f "*";
      Util.option ()
        (fun (x,e) -> match r.a.inf_type with
           | None -> bprintf f "@[%s=%s]" x e
           | Some ty -> bprintf f "@[%s:%s=%s]" x ty e)
        early;
      Util.option () (fun (x,e) -> bprintf f "$[%s=%s]" x e) late;
      pr_maybe_group f r2 r right
  | Hash(Bounds(m,Num(n)),r2) ->
      if (m = 0) then bprintf f "#%d" n
      else bprintf f "%d#%d" m n;
      pr_maybe_group f r2 r right
  | Hash(Bounds(m,Infinity),r2) ->
      if (m = 0) then bprintf f "#"
      else bprintf f "%d#" m;
      pr_maybe_group f r2 r right
  | Hash(Accumulate(early,late),r2) ->
      bprintf f "#";
      (match early with None -> () | Some(x,e) -> bprintf f "@[%s=%s]" x e);
      (match late with None -> () | Some(x,e) -> bprintf f "$[%s=%s]" x e);
      pr_maybe_group f r2 r right
  | Minus(r2,r3) ->
      pr_maybe_group f r2 r left; bprintf f " - "; pr_maybe_group f r3 r right);
  match r.a.precedence with
    | Default_prec -> ()
    | No_prec -> bprintf f " @no-prec"
    | Some_prec p -> bprintf f " @prec %s" p

let pr_rettype f t = bprintf f "@({%s})" t
let pr_rettype_opt f = function
  | None -> ()
  | Some t -> bprintf f "%a" pr_rettype t

let pr_CHARVAL f = function
  | "\"" -> bprintf f "<\">"
  | s -> bprintf f "\"%s\"" s

(* TODO: analyze text to see whether parens are needed. If only an ID, then not needed. *)
let pr_closed_text f txt = bprintf f "(%s)" txt

let pr_lexer_case f = function
  | TokenSymb(n,t_opt,Some n2) ->
      bprintf f "%s %a = %s" n pr_rettype_opt t_opt n2
  | TokenSymb(n,t_opt,None) ->
      bprintf f "%s %a" n pr_rettype_opt t_opt
  | TokenLit(n,t_opt,s) ->
      bprintf f "%a %a = %s" pr_CHARVAL s pr_rettype_opt t_opt n

let pr_lexer_cases f cs =
  List.iter (bprintf f "| %a\n" pr_lexer_case) cs;
  bprintf f ".\n"

let pr_definition f = function
  | RuleDef(n,r,a) ->
      bprintf f "%s" n;
      (match a.early_params, a.early_param_type, a.input_attributes with
          None,_,[]   -> ()
      |   None,_,y ->
          bprintf f "@(;%s)"
            (String.concat ";" (List.map (fun (var,typ) -> var^":"^typ) y))
      | Some x,None,[]   -> bprintf f "@(%s)" x
      | Some x,Some ty,[]   -> bprintf f "@(%s:%s)" x ty
      | Some x,None,y ->
          bprintf f "@(%s;%s)" x
            (String.concat ";" (List.map (fun (var,typ) -> var^":"^typ) y))
      | Some x,Some ty,y ->
          bprintf f "@(%s:%s;%s)" x ty
            (String.concat ";" (List.map (fun (var,typ) -> var^":"^typ) y)));
      (match a.early_rettype,a.output_attributes with
          None,[]   -> ()
      |   None,y ->
          bprintf f ">@(;%s)"
            (String.concat ";" (List.map (fun (var,typ) -> var^":"^typ) y))
      | Some x,[]   -> bprintf f ">@(%s)" x
      | Some x,y ->
          bprintf f ">@(%s;%s)" x
            (String.concat ";" (List.map (fun (var,typ) -> var^":"^typ) y)));
      (match a.late_params with None -> () | Some x -> bprintf f "$(%s)" x);
      bprintf f " = ";
      pr_rule f r;
      bprintf f ".";
      bprintf f "\n"
  | LexerDecl(n,np,t,l) ->
      bprintf f "@declare-lexer %s %a %s =\n" n pr_rettype t np;
      pr_lexer_cases f l;
      bprintf f "\n"
  | LexerDecl2(g,ty,l) ->
      bprintf f "@declare-lexer2 %a %a =\n" pr_closed_text g pr_closed_text ty;
      pr_lexer_cases f l;
      bprintf f "\n"
  | SingleLexerDecl(g,ty,l) ->
      bprintf f "@set-lexer %a %a =\n" pr_closed_text g pr_closed_text ty;
      pr_lexer_cases f l;
      bprintf f "\n"
  | LexerDef _ -> Util.todo "Pr.pr_definition.LexerDef"

let pr_definitions f ds = List.iter (pr_definition f) ds

let pr_text f = function
  | Ocaml x -> fprintf f "@ocaml {%s}\n" x
  | Ocamllex x -> fprintf f "@ocamllex {%s}\n" x
  | Ocamlyacc x -> fprintf f "@ocamlyacc {%s}\n" x
  | Dypgenlex x -> fprintf f "@dypgenlex {%s}\n" x

let pr_assoc f = function
  | Non_assoc -> fprintf f "@non"
  | Left_assoc -> fprintf f "@left"
  | Right_assoc -> fprintf f "@right"

let pr_grammar f gr =
  List.iter (pr_text f) (List.rev gr.prologue);

  let pls = gr.precs in
  let n_pls = Array.length pls in
  if n_pls = 0 then ()
  else begin
    fprintf f "\n@precedence : ";
    let a, ns = pls.(0) in
    pr_assoc f a; List.iter (fprintf f " %s") ns;
    for i = 1 to n_pls - 1 do
      let a, ns = pls.(i) in
      fprintf f "\n  < "; pr_assoc f a;
      List.iter (fprintf f " %s") ns;
    done;
    fprintf f "\n.\n";
  end;

  if (!Variables.counter > 1) then
    fprintf f "@counter(%d)\n" !Variables.counter;

  let b = Buffer.create 11 in
  List.iter (pr_definition b) gr.ds;
  Buffer.output_buffer f b;

  List.iter (pr_text f) gr.epilogue

let pr_definition_names f ds =
  let names = List.concat (List.map (function RuleDef(n,_,_) -> [n] | _ -> []) ds) in
  fprintf f "%s\n" (String.concat " " names)

let rule2string r =
  let b = Buffer.create 11 in
  pr_rule b r;
  Buffer.contents b

let pr_rule_channel f r =
  let b = Buffer.create 11 in
  pr_rule b r;
  Buffer.output_buffer f b

module Gil = struct

  exception Unsupported_construct

  module Pretty = struct
    let prec_star = 3

    let prec = function
      | Gil.Action _    -> 3
      | Gil.When _      -> 3
      | Gil.When_special _ -> 3
      | Gil.DBranch _   -> 3
      | Gil.Box _       -> 3
      | Gil.Symb _      -> 3
      | Gil.CharRange _ -> 3
      | Gil.Lit _       -> 3
      | Gil.Star _      -> prec_star
      | Gil.Lookahead _ -> prec_star
      | Gil.Seq _       -> 1
      | Gil.Alt _       -> 0

    let pr f r0 =
      let rec loop outer_prec r =
        let recur thunk =
          let my_prec = prec r in
          if my_prec < outer_prec then begin
            bprintf f "(";
            thunk (loop my_prec);
            bprintf f ")" end
          else
            thunk (loop my_prec) in
        match r with
          | Gil.Action(e) ->
              bprintf f "{%s}" e
          | Gil.When(e1,e2) ->
              bprintf f "@when(%s,%s)" e1 e2
          | Gil.When_special _ -> Util.todo "Pr.Gil.Pretty.pr.loop.When_special"
          | Gil.DBranch (f1, c, f2) ->
              bprintf f "@match(%s, {%s, %d, %s}, %s)"
                f1 c.Gil.cname c.Gil.arity c.Gil.cty f2
          | Gil.Box(e,boxnull) ->
              bprintf f "@box(%s%a)" e pr_boxnull boxnull
          | Gil.Symb(x,y,z) ->
              (bprintf f "%s" x;
               (match y,z with
                  | None,None ->
                      bprintf f "()"
                  | Some args,Some binder ->
                      bprintf f "(%s,%s)" args binder
                  | None,Some binder ->
                      bprintf f "(-,%s)"  binder
                  | Some args,None ->
                      bprintf f "(%s,-)" args))
          | Gil.CharRange(low,high) ->
              bprintf f "%s" (Cs.to_string (Cs.range low (high+1)))
          | Gil.Lit(case_sensitive,x) ->
              (* Note the Gil string literals are not the same as
                 ocaml string literals, this causes problems for
                 including the output in an ocaml comment.  The
                 problem is backslash. *)
              (* TODO: need syntax to distinguish case sensitive and insensitive literals *)
              if x="\"" then bprintf f "<\">"
              else bprintf f "\"%s\"" x
          | Gil.Star(r2) ->
              recur (fun go ->
                       bprintf f " *"; (* print a space in case previous char was left paren, so this
                                          does not look like an ocaml comment.  Useful when embedding
                                          the output in an ocaml comment---but not perfect, see Lit
                                          case above. *)
                       go r2)
          | Gil.Lookahead(true,r2) ->
              recur (fun go ->
                       bprintf f "&";
                       go r2)
          | Gil.Lookahead(false,r2) ->
              recur (fun go ->
                       bprintf f "!";
                       go r2)
          | Gil.Seq(r2,r3) ->
              recur (fun go ->
                       go r2;
                       bprintf f " ";
                       go r3)
          | Gil.Alt(r2,r3) ->
              recur (fun go ->
                       go r2;
                       bprintf f " | ";
                       go r3)
      in loop (prec r0) r0


    let pr_definitions b =
      List.iter
        (fun (n,gil) ->
           Printf.bprintf b "%s = " n;
           pr b gil;
           Printf.bprintf b "\n")

    let rule2string r =
      let b = Buffer.create 11 in
      pr b r;
      Buffer.contents b

  end

  module Dypgen = struct

    (** The precedence of dypgen constructs. *)
    (* Happens to be that dypgen's priorities coincide with the standard Gil precedence.  *)
    let prec = Pretty.prec

    (** TODO: an analysis of the grammar which decides whether it is within the class
        supported by this backend. *)
    let is_member gr = true

(* -->  *)
    let getpos_code = "(dyp.Dyp.symbol_start ())"
    let default_reduction = " {()}"

    let prologue = Printf.sprintf "let local_data = sv0
let local_data_equal = sv_eq
let _upd dyp f = [Dyp.Local_data (f dyp.Dyp.local_data)]
let _updp dyp f = [Dyp.Local_data (f %s dyp.Dyp.local_data)]
let _upd_l dyp f = [Dyp.Local_data (f dyp.Dyp.last_local_data)]
let _updp_l dyp f = [Dyp.Local_data (f %s dyp.Dyp.last_local_data)]
let _app dyp f = f dyp.Dyp.last_local_data
let _appp dyp f = f %s dyp.Dyp.last_local_data
"  getpos_code getpos_code getpos_code

(*       Need to set equality functions  *)
    (** create the string to transform local_data based on the supplied function. *)
    let tx_ld f = "_upd dyp (" ^ f ^ ")"

    (** create the string to transform local_data based on the supplied function,
        which expects a position argument. *)
    let tx_ld_p f = "_updp dyp (" ^ f ^ ")"

    (** create the string to transform local_data based on the supplied function. *)
    let tx_last_ld f = "_upd_l dyp (" ^ f ^ ")"

    (** create the string to transform local_data based on the supplied function,
        which expects a position argument. *)
    let tx_last_ld_p f = "_updp_l dyp (" ^ f ^ ")"

    (** create the string to the supplied function to local data. [f] expects a position argument. *)
    let app_ld f = "_app dyp (" ^ f ^ ")"
    let app_ld_p f = "_appp dyp (" ^ f ^ ")"

    let dyp_symb is_scannerless = if is_scannerless then
      Variables.bnf2ocaml_lident "tk" (* assume that any uppercase id
                                         represents a token *)
    else
      Variables.bnf2ocaml  (* If it is not scannerless, than assume uppercase ids are
                              actual token names. *)

    let gensym () = Variables.freshn "dyp"

    let lift lifted e =
      let x = gensym () in
      lifted := (x, sprintf "@{ (), %s }" e)::!lifted;
      x

    let char_tk = Printf.sprintf "Dyp_C%d"

    let pr f is_scannerless lifted is_seq_right r0 =
      let rec loop is_nested outer_prec is_seq_right r =
        let recur thunk =
          let my_prec = prec r in
          if my_prec < outer_prec then
            begin
              bprintf f "[";
              thunk true (loop true my_prec);
              bprintf f "]"
            end
          else
            thunk is_seq_right (loop is_nested my_prec) in
        match r with
          | Gil.DBranch _ -> Util.todo "Pr.Dypgen.pr.loop.DBranch"
          | Gil.Action(e) ->
              if is_seq_right then
                (if is_nested then
                   bprintf f "@{ (), %s }" (tx_last_ld_p e)
                 else
                   bprintf f "{ %s }" (app_ld_p e))
              else
                bprintf f "%s" (lift lifted (tx_ld_p e))

          | Gil.When(e1,e2) ->
              if is_seq_right then
                (if is_nested then
                   bprintf f "@{ (), if %s dyp.Dyp.last_local_data then %s else raise Dyp.Giveup}" e1 (tx_last_ld e2)
                 else
                   bprintf f "{ if %s dyp.Dyp.last_local_data then %s else raise Dyp.Giveup }" e1 (app_ld e2))
              else
                bprintf f "%s" (lift lifted (sprintf "if %s dyp.Dyp.local_data then %s else raise Dyp.Giveup" e1 (tx_ld e2)))

          | Gil.Symb(x, call_opt, Some binder) when not is_seq_right ->
              (* Nest the call so that we can locally change
                 the state before the call without affecting it
                 after the call, and so we can perform the merge
                 without using an early action.
              *)
              bprintf f "[ ";
              (match call_opt with Some call -> bprintf f " ...@{ (), %s} " (tx_ld call) | None -> ());
              bprintf f "%s" (dyp_symb is_scannerless x);
              (* bind the nonterminal and then merge the previous
                 local value and [x]'s result and update the local value with the result
                 of the merge. *)
              let z = Variables.fresh () in
              bprintf f "<%s>" z;
              bprintf f " @{ (), [Dyp.Local_data (%s %s dyp.Dyp.local_data %s)]}" binder getpos_code z;
              bprintf f " ]"
          | Gil.Symb(x, call_opt, binder_opt) ->
              (match call_opt with
                 | None -> bprintf f "%s" (dyp_symb is_scannerless x)
                 | Some call ->
                     (* Nest the call so that we can locally change
                        the state before the call without affecting it
                        after the call. *)
                     bprintf f "[ ";
                     bprintf f "...@{ (), %s} " (tx_ld call);
                     bprintf f "%s" (dyp_symb is_scannerless x);
                     bprintf f " ]");
              (match binder_opt with
                 | None ->
                     (* do nothing to local state, implicitly
                        propogating previous value.  if we're at the
                        end of a sequence, need to ensure that action
                        is present.  *)
                     if is_seq_right then bprintf f "%s" default_reduction
                 | Some binder ->
                     (* bind the nonterminal and then apply the binder to the previous
                        local value and the result and update the local value with the result
                        of the merge. *)
                     let z = Variables.fresh () in
                     bprintf f "<%s>" z;
                     (* Assume is_seq_right because previous match case
                        catches binders when not is_seq_right. *)
                     if is_nested then
                       bprintf f " @{ (), [Dyp.Local_data (%s %s dyp.Dyp.last_local_data %s)]}" binder getpos_code z
                     else
                       bprintf f " { %s %s dyp.Dyp.last_local_data %s }" binder getpos_code z)
          | Gil.CharRange(low, high) ->
              if is_scannerless then
                if low = high then begin
                  bprintf f "%s" (char_tk low);
                  if is_seq_right then bprintf f "%s" default_reduction
                end
                else (* translate to an alt and rerun. *)
                  loop is_nested outer_prec is_seq_right (Gil.charrange2alt low high)
              else begin
                if low = high then
                  bprintf f "%C" (Char.chr low)
                else
                  bprintf f "[%C-%C]" (Char.chr low) (Char.chr high);

                if is_seq_right then bprintf f "%s" default_reduction
              end
          | Gil.Lit(_, "") ->
              (* Don't print the empty string, because it messes up the lexer. *)
              if is_seq_right then bprintf f "%s" default_reduction
          | Gil.Lit(case_sensitive, x) ->
              (* Note the Gil string literals are not the same as
                 ocaml string literals, this causes problems for
                 including the output in an ocaml comment.  The
                 problem is backslash. *)
              (* TODO: need syntax to distinguish case sensitive and insensitive literals *)
              if is_scannerless then
                begin
                  let n = String.length x in
                  (* Note: we are using [r] in the [Seq] as a dummy
                     b/c precedence only looks at the outermost
                     constructor. *)
                  let nest = prec (Gil.Seq(r,r)) < outer_prec && n > 1 in
                  if nest then
                    bprintf f "[";
                  bprintf f "%s" (char_tk (Char.code x.[0]));
                  for i = 1 to n - 1 do
                    bprintf f " %s" (char_tk (Char.code x.[i]))
                  done;
                  if nest then
                    bprintf f "%s]" default_reduction;
                end
              else
                bprintf f "%S" x;

              if is_seq_right then bprintf f "%s" default_reduction
          | Gil.Star(r2) ->
              recur (fun isr go -> go false r2; bprintf f " *");

              if is_seq_right then bprintf f "%s" default_reduction
          | Gil.Seq(r2,Gil.Lit(_, "")) ->   (* Special case empty string at the end of a sequence so
                                               that [is_seq_right] is properly calculated.
                                               Just drop the empty string. *)
              loop is_nested outer_prec is_seq_right r2
          | Gil.Seq(r2,r3) ->
              recur (fun isr go ->
                       go false r2;
                       bprintf f " ";
                       go isr r3)
          | Gil.Alt(r2,Gil.Lit(_, "")) -> (* "resugar" option *)
              let prec_opt = Pretty.prec_star in
              if prec_opt < outer_prec then
                begin
                  bprintf f "[";
                  loop true prec_opt false r2;
                  bprintf f " ?";
                  bprintf f "]"
                end
              else
                begin
                  loop is_nested prec_opt false r2;
                  bprintf f " ?"
                end;
              if is_seq_right then bprintf f "%s" default_reduction
          | Gil.Alt(r2,r3) ->
              recur (fun isr go ->
                       go isr r2;
                       bprintf f "\n | ";
                       go isr r3)
          | Gil.Lookahead _ ->
              Util.warn Util.Sys_warn "Unsupported construct: lookahead"
                (* raise Unsupported_construct*)
          | Gil.When_special _ ->
              Util.warn Util.Sys_warn "Unsupported construct: when_special"
                (* raise Unsupported_construct*)
          | Gil.Box _ ->
              Util.warn Util.Sys_warn "Unsupported construct: box"
                (* raise Unsupported_construct*)
      in loop false (prec r0) is_seq_right r0


      let pr_definitions b is_scannerless ds =
        let lifted = ref [] in
        List.iter
          (fun (n,r_gil) ->
             Printf.bprintf b "%s: " (dyp_symb is_scannerless n);
             pr b is_scannerless lifted true r_gil;
             Printf.bprintf b "\n\n") ds;
        List.iter
          (fun (x, s) -> Printf.bprintf b "%s: %s\n" x s) !lifted

      let pr_grammar b is_scannerless gr =
        let ds = gr.gildefs in
        let gr_prologue = List.rev gr.prologue in

        let is_first = ref true in
        let is_second = ref true in
        List.iter
          (function
             | Ocaml x ->
                 if !is_first then begin
                   Printf.bprintf b "%%mltop{%s}\n\n" x;
                   is_first := false;
                 end else if !is_second then begin
                   Printf.bprintf b "{%s\n\n" x;
                   is_second := false;
                 end else
                   Printf.bprintf b "%s\n\n" x
             | _ -> ())
          gr_prologue;
        if !is_second then
          Printf.bprintf b "{";
        Printf.bprintf b "%s}\n\n" prologue;

        Printf.bprintf b "%%start %s\n\n"
          (dyp_symb is_scannerless gr.start_symbol);

        if is_scannerless then
          begin
            Printf.bprintf b "%%token";
            for i = 0 to 255 do
              Printf.bprintf b " %s" (char_tk i)
            done;
            Printf.bprintf b "\n\n%%lexer\n";
            Printf.bprintf b "main lexer =\n";
            for i = 0 to 255 do
              Printf.bprintf b "  %C -> %s\n" (Char.chr i) (char_tk i)
            done
          end
        else
          begin
            List.iter
              (function
                 | Dypgenlex x -> Printf.bprintf b "%s\n\n" x
                 | _ -> ())
              gr_prologue;
          end;
        Printf.bprintf b "%%parser\n\n";
        pr_definitions b is_scannerless ds

  end

end
