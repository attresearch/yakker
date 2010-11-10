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

let mk_lexer tokenizer token_type decls =
  let tok = Util.fresh() in
  let tok_def =
    let rettype = Some token_type in
    let a = mkAttr() in
    a.Attr.early_rettype <- rettype;
    RuleDef(tok,mkBOX(tokenizer,rettype,Runbox_null),a) in
  let x = Util.fresh() in
  let y = Util.fresh() in
  let lit_envs,decls =
    List.split
      (List.map
         (function
           | TokenLit(ocaml_constructor,carried_type,lit) ->
               let nonterminal = Util.fresh() in
               ( [(lit,nonterminal)],
                 TokenSymb(ocaml_constructor,carried_type,Some nonterminal) )
           | TokenSymb _ as td -> ([],td))
         decls) in
  let lit_env = List.flatten lit_envs in
  let other_defs =
    List.map
      (function
        | TokenLit _ -> failwith "impossible"
        | TokenSymb(ocaml_constructor,carried_type,nonterminal_opt) ->
            let nonterminal =
              (match nonterminal_opt with
                Some z -> z | None -> ocaml_constructor) in
            let a = mkAttr() in
            let rhs =
              match carried_type with
                Some _ ->
                  let guard = mkWHEN(sprintf "(match %s with %s _ -> true | _ -> false)" x ocaml_constructor) in
                  let delay = mkDELAY(sprintf "(match %s with %s %s -> %s | _ -> failwith \"impossible\")" x ocaml_constructor y y,
                                      carried_type) in
                  mkSEQ2(mkSYMB(tok,None,None),Some x,None,mkSEQ[guard;delay])
              | None ->
                  let guard = mkWHEN(sprintf "(match %s with %s -> true | _ -> false)" x ocaml_constructor) in
                  mkSEQ2(mkSYMB(tok,None,None),Some x,None,guard) in
            (nonterminal,(ocaml_constructor,carried_type)),RuleDef(nonterminal,rhs,a))
      decls in
  let myenv,other_defs = List.split other_defs in
    lit_env,myenv,tok_def::other_defs

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
    | When _
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

let transform gr =
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
        let lit_env',myenv,defs = mk_lexer tokenizer token_type decls in
        let tokmap,ngr = loop (lit_env'@lit_env) tl in
        let ntokmap = (tokenizer_peek,myenv)::tokmap in
          ntokmap, defs::ngr
  in
  let tokmap,ngr = loop [] gr.ds in
    gr.ds <- List.flatten ngr;
    gr.tokmap <- tokmap
