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

open Gul
open Printf

let ocamllex s =
  let (temp_file_name,temp_chan) = Filename.open_temp_file "yakker" ".mll" in
  output_string temp_chan s;
  close_out temp_chan;
  let out_file_name = Filename.temp_file "yakker" "ml" in
  let command = sprintf "ocamllex -q -o %s %s" out_file_name temp_file_name in
  let result =
    (match Unix.system command with
      Unix.WEXITED 0 ->
        let ch = open_in out_file_name in
        let len = in_channel_length ch in
        let s1 = String.create len in
        (try
          really_input ch s1 0 len;
          Ocaml s1
        with _ -> Ocamllex s)
    | Unix.WEXITED _
    | Unix.WSIGNALED _
    | Unix.WSTOPPED _ -> Ocamllex s
    ) in
  Sys.remove temp_file_name;
  Sys.remove out_file_name;
  result

let run_ocamllex gr =
  let replace l =
    let result =
      List.map
        (function
            Ocamllex s -> ocamllex s
          | x -> x)
        l in
    List.iter
      (function
          Ocamllex s -> eprintf "ocamllex failure\n%!"
        | _ -> ())
      result;
    result in
  gr.prologue <- replace gr.prologue;
  gr.epilogue <- replace gr.epilogue

let preprocess_decls use_new_syntax decls =
  List.split
    (List.map
       (function
          | TokenLit(ocaml_constructor, carried_type, lit) ->
              let nonterminal = Variables.fresh() in
              ( [(lit, nonterminal)],
                (nonterminal, carried_type, ocaml_constructor) )
          | TokenSymb (s1, cty, s2_opt) ->
              let s2 = Yak.Util.option_get s1 s2_opt in
              let nonterminal, ocaml_constructor = if use_new_syntax then s1, s2 else s2, s1 in
              ([], (nonterminal, cty, ocaml_constructor)))
       decls)

let mk_lexer use_new_syntax tokenizer token_type decls =
  let tok = Variables.fresh() in
  let tok_def =
    let rettype = Some token_type in
    let a = mkAttr() in
    a.Attr.early_rettype <- rettype;
    RuleDef(tok,mkBOX(tokenizer,rettype, Runbox_null),a) in
  let x = Variables.fresh() in
  let y = Variables.fresh() in
  let lit_envs, decls = preprocess_decls use_new_syntax decls in
  let lit_env = List.flatten lit_envs in
  let other_defs =
    List.map
      (fun (nonterminal, carried_type, ocaml_constructor) ->
         let nb = if (String.uppercase nonterminal) = "EOF" then Attr.N.Unknown else Attr.N.Never_null in
         let a = { (mkAttr ()) with Attr.nullability = nb } in
         (* In case the user has left off the carrier type simply because
            they don't care about the result, but the constructor might
            actually have non-zero arity, we include a wildcard after the
            constructor, a feature supported even for nullary constructors as
            of OCaml 3.11. *)
         let guard = mkWHEN(sprintf "(match %s with %s _ -> true | _ -> false)" x ocaml_constructor) in
         let tail =
           match carried_type with
               Some _ ->
                 let delay = mkDELAY(sprintf "(match %s with %s %s -> %s | _ -> failwith \"impossible\")" x ocaml_constructor y y,
                                     carried_type) in
                 mkSEQ[guard;delay]
             | None -> guard in
         let rhs = mkSEQ2(mkSYMB(tok,None,None),Some x,None, tail) in
         (nonterminal,(ocaml_constructor,carried_type)),RuleDef(nonterminal,rhs,a))
      decls in
  let myenv,other_defs = List.split other_defs in
  lit_env,myenv,tok_def::other_defs

(** Desugar lexer decls to deterministic branches (instead of @when). *)
(* TODO: previous version has the advantage that the Earley state
   implicitly handles memoization of tokenizer's invocations by
   wrapping it in a nonterminals. IN this version, i've inlined the
   boxes so that they can be determinized into one box when tokens
   appear together in the transducer. While that (may) help
   determinization, it loses memoization. Would be best to find a way
   to have both. *)
(** argument [is_simple_dbranch] flags whether dbranch is interpeted as a simple
    lexer.*)
let mk_lexer2 is_simple_dbranch tokenizer token_type decls =
  let tok = Variables.fresh() in (* fresh name for variable used to bind result of tokenizer. *)
  let rettype = Some token_type in
  let tok_box = mkBOX(tokenizer, rettype, Runbox_null) in
  let x = Variables.fresh() in          (* fresh name for variable used to bind result of dbranch. *)
  let lit_envs, decls = preprocess_decls true decls in
  let lit_env = List.flatten lit_envs in
  let other_defs =
    List.map
      (fun (nonterminal, carried_type, ocaml_constructor) ->
         let nb = if nonterminal = "EOF" then Attr.N.Unknown else Attr.N.Never_null in
         let a = { (mkAttr ()) with Attr.nullability = nb} in
         let rhs =
           (* TODO: support arities other than 0, 1. *)
           match carried_type with
               Some _ ->
                 let c = {cname = ocaml_constructor; arity = 1; cty = token_type} in
                 if is_simple_dbranch then
                   mkDBRANCH(tokenizer, c)
                 else
                   mkSEQ2(tok_box, Some tok, None,
                          mkSEQ2(mkDBRANCH(tok, c), Some x, None,
                                 mkDELAY(x, carried_type)))
             | None ->
                 let c = {cname = ocaml_constructor; arity = 0; cty = token_type} in
                 if is_simple_dbranch then
                   mkDBRANCH(tokenizer, c)
                 else
                   mkSEQ2(tok_box, Some tok, None, mkDBRANCH(tok, c)) in
         (nonterminal, (ocaml_constructor,carried_type)), RuleDef(nonterminal,rhs,a))
      decls in
  let myenv, other_defs = List.split other_defs in
    lit_env, myenv, other_defs

let lit_substitution env r0 =
  let rec loop r =
    match r.r with
    | Lit(_,x) ->
        if List.mem_assoc x env then
          r.r <- (mkSYMB(List.assoc x env,None,None)).r
    | Symb _
    | Action _
    | Position _
    | Box _
    | Delay _
    | CharRange _
    | Prose _
    | When _ | DBranch _
    | Lookahead _ ->
        ()
    | Seq(r1,_,_,r2)
    | Minus(r1,r2)
    | Alt(r1,r2) ->
        loop r1; loop r2
    | Opt(r1)
    | Assign(r1,_,_)
    | Rcount(_,r1)
    | Star(_,r1)
    | Hash(_,r1) ->
        loop r1 in
  loop r0

exception Stop_list_search

let transform gr =
  let check_singles seen_single = function
    | SingleLexerDecl _ -> if seen_single then raise Stop_list_search else true
    | LexerDecl _ | LexerDecl2 _ | LexerDef _ -> if seen_single then raise Stop_list_search else seen_single
    | RuleDef _ -> seen_single in
  let validate ds =
    try ignore (List.fold_left check_singles false ds) with
      | Stop_list_search ->
          Yak.Util.error Yak.Util.Sys_warn "Use of multiple lexers with @set-lexer declaration" in
  let rec loop lit_env ds =
    match ds with [] ->
      [],[]
    | (LexerDef _ as hd)::tl ->
        let tokmap,ngr = loop lit_env tl in
          tokmap, [hd]::ngr
    | (RuleDef(n,r,a) as hd)::tl ->
        lit_substitution lit_env r;
        let tokmap,ngr = loop lit_env tl in
          tokmap, [hd]::ngr
    | LexerDecl(tokenizer,tokenizer_peek,token_type,decls)::tl ->
        let lit_env', myenv, defs =
          if !Compileopt.use_dbranch then
            mk_lexer2 false tokenizer token_type decls
          else
            mk_lexer false tokenizer token_type decls in
        let tokmap,ngr = loop (lit_env'@lit_env) tl in
        let ntokmap = (tokenizer_peek,myenv)::tokmap in
          ntokmap, defs::ngr
    | LexerDecl2(tokenizer, token_type, decls)::tl ->
        let lit_env', myenv, defs =
          if !Compileopt.use_dbranch then
            mk_lexer2 false tokenizer token_type decls
          else
            mk_lexer true tokenizer token_type decls in
        let tokmap,ngr = loop (lit_env'@lit_env) tl in
        let ntokmap = ("",myenv)::tokmap in
          ntokmap, defs::ngr
    | SingleLexerDecl (tokenizer, token_type, decls)::tl ->
        (* Add token module to prologue *)
        let tkty = Variables.fresh() in
        let tkmod = Variables.tk_mod in
        let tkdecls = Printf.sprintf
          "type %s = %s\n\
           module %s = Yak.Engine.Make_tokenizer(struct type token = %s let f = %s end)\n"
          tkty token_type tkmod tkty tokenizer in
        gr.prologue <- Ocaml tkdecls :: gr.prologue;
        gr.has_single_lexer <- true;

        let new_tokenizer = tkmod ^ ".lex" in
        let lit_env', myenv, defs =
          mk_lexer2 true new_tokenizer token_type decls in
        let tokmap,ngr = loop (lit_env'@lit_env) tl in
        let ntokmap = ("",myenv)::tokmap in
        ntokmap, defs::ngr in
  validate gr.ds;
  let tokmap, ngr = loop [] gr.ds in
    gr.ds <- List.flatten ngr;
    gr.tokmap <- tokmap
