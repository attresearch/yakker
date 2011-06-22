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

(* Gul abstract syntax *)
type nonterminal = Gil.nonterminal
type constr =  Gil.constr = {cname: string; arity:int; cty: string}
type expr = Gil.expr
type ty = string
type var = string

type repeat = Infinity | Num of int
type looper = Bounds of int * repeat | Accumulate of (var * expr) option * (var * expr) option

type prec_annotation = Default_prec | No_prec | Some_prec of string

type annot = { mutable css: Cs.t option;
               mutable early_relevant: bool;
               mutable late_relevant: bool;
               mutable pre:int;
               mutable post:int;
               mutable is_regular:bool;
               mutable early_assignments: var PSet.t;
               mutable late_assignments: var PSet.t;
               mutable precedence : prec_annotation;
               inf_type : ty option
                 (** inferred type. It does *not* necessarily refer to the entire
                     rhs. Rather, its meaning is rhs-dependent. *)
             }

type 'expr boxnull = 'expr Gil.boxnull =
    Always_null          (* box always accepts null, e.g., position *)
  | Never_null           (* box never accepts null *)
  | Runbox_null          (* run box to determine if it accepts null *)
  | Runpred_null of 'expr (* run separate predicate to determine if box accepts null *)

type rhs = { mutable a: annot;
             mutable r: rhs0 }

and rhs0 =
    Symb of nonterminal
      * expr option                (* early args *)
      * ((var * expr) list)        (* attributes *)
      * expr option                (* late args  *)
  | Lit of bool * string                (* true iff case sensitive *)
  | Position of bool                    (* true iff early *)
  | CharRange of int * int
  | Prose of string
  | Action of expr option * expr option
  | Box of expr * ty option * expr boxnull (* argument, return type, box nullability info *)
  | Delay of bool * expr * ty option       (* requires early environments, argument, return type *)
  | When of expr
  | DBranch of expr * constr
  | Opt of rhs
  | Seq of rhs * var option * var option * rhs
  | Assign of rhs * var option * var option
  | Alt of rhs * rhs
  | Minus of rhs * rhs
  | Rcount of expr * rhs
  | Star of looper * rhs
  | Hash of looper * rhs
  | Lookahead of bool * rhs

let dupAnnot a = {a with css=a.css } (* specify one field b/c syntax doesn't let us specify none. however, we just chose that one arbitrarily. All fields are duplicated.*)

let mkAnnot = function
  | None -> {css = None; early_relevant = false; late_relevant = false;
           pre = 0; post = 0; is_regular = false; precedence = Default_prec;
           early_assignments=PSet.empty;late_assignments=PSet.empty;
           inf_type = None;}
  | Some r -> let a = dupAnnot r.a in a.css <- None; a

let dupRhs r = {a=dupAnnot r.a;r=r.r}

(** only need to copy mutable material. (otherwise, there's no
    value to copying that I know of). So, leaves are copied shallowly.
 *)
let rec copy_b = function
  | (Position _
    | Symb _
    | Prose _
    | When _ | Action _ | Box _ | Delay _ | DBranch _
    | CharRange _
    | Lit _) as x -> x
  | Minus(r2,r3) -> Minus(copyRhs r2,copyRhs r3)
  | Seq(r2,v_e,v_l,r3) -> Seq(copyRhs r2,v_e,v_l,copyRhs r3)
  | Assign(r2,v_e,v_l) -> Assign(copyRhs r2,v_e,v_l)
  | Alt(r2,r3) -> Alt(copyRhs r2,copyRhs r3)
  | Opt(r2) -> Opt (copyRhs r2)
  | Rcount(x,r2) -> Rcount(x,copyRhs r2)
  | Star(x,r2) -> Star(x,copyRhs r2)
  | Hash(x,r2) -> Hash(x,copyRhs r2)
  | Lookahead (b,r2) -> Lookahead (b, copyRhs r2)

(** deep copy *)
and copyRhs r = {a=dupAnnot r.a;r= copy_b r.r}

module Attr = struct
  type t = {mutable early_params: string option;
            early_param_type: string option;
            mutable early_rettype: string option;
            mutable late_params: string option;
            mutable input_attributes: (var * string) list;  (* attribute name x type *)
            mutable output_attributes: (var * string) list; (* attribute name x type *)
           }
end

let mkAttr() = {Attr.
                early_params=None; early_rettype=None;
                late_params=None;
                input_attributes=[];
                output_attributes=[];
                early_param_type = None;
               }

type text =
  Ocaml of string
  | Ocamllex of string
  | Ocamlyacc of string
  | Dypgenlex of string

type assoc = Non_assoc | Left_assoc | Right_assoc

type directive =
  | Text_directive of text
  | Disamb_directive of (assoc * nonterminal list) array

type token_decl =
    TokenLit of string * string option * string         (* ocaml constructor, optional carried type, literal *)
  | TokenSymb of string * string option * string option (* ocaml constructor, optional carried type, optional yakker nonterminal *)

type definition =
    RuleDef of nonterminal * rhs * Attr.t
  | LexerDef of rhs list
  | LexerDecl of
      (string *         (* ocaml tokenizer function *)
       string *         (* ocaml tokenizer peek function *)
       string *         (* return type of tokenizer function *)
       token_decl list) (* the tokens *)

  | LexerDecl2 of       (* An alternate representation for lexers; same semantics.
                           Drops peek and interprets the [token_decl list] a bit
                           differently, ala. [SingleLexerDecl]. *)
      (string *         (* ocaml tokenizer function *)
       string *         (* return type of tokenizer function *)
       token_decl list) (* the tokens *)
  | SingleLexerDecl of
      (string *         (* ocaml tokenizer function *)
       string *         (* return type of tokenizer function *)
       token_decl list) (* the tokens *)

type grammar = {
    mutable ds: definition list;
    mutable m: (string,Cs.t option) PMap.t;
    (** A map from nonterminal names to results of character set
        analysis for that nonterminal. *)

    mutable early_producers: nonterminal PSet.t;
    mutable late_producers: nonterminal PSet.t;
    mutable grammar_early_relevant: bool;
    mutable grammar_late_relevant: bool;

    mutable prologue: text list; (* in reverse order *)
    mutable epilogue: text list;
    start_symbol : nonterminal;
    (** Flag indicating whether elements saved in history need to be
        wrapped. *)

    mutable has_single_lexer : bool;
    (** Flag indicating whether the parsing is driven by a single
        lexer, vs. scannerless and/or multilex. *)

    mutable tokmap: (expr * (nonterminal * (nonterminal * expr option)) list) list;
    mutable gildefs: (nonterminal * string Gil.rhs) list;

    mutable precs : (assoc * nonterminal list) array
}

let mkGrammar ds m p e pd =
  {ds=ds; m=m;
   prologue=p;
   epilogue=e;
   early_producers=PSet.empty;
   late_producers=PSet.empty;
   grammar_early_relevant=false;
   grammar_late_relevant=false;
   start_symbol = (* Defaults to first nonterminal *)
   (match (List.fold_left
            (fun x d ->
              match x,d with
                None,RuleDef(n,_,_) -> Some n
              | _,_ -> x)
             None ds) with
     None -> (Printf.eprintf "Warning: grammar contains no rules\n%!"; "")
   | Some y -> y);
   has_single_lexer = false;
   tokmap = [];
   gildefs = [];
   precs = pd;
  }

let add_to_prologue gr s =
  gr.prologue <- (Ocaml s)::gr.prologue

let add_many_to_prologue gr xs =
  gr.prologue <- List.fold_left (fun pr s ->  (Ocaml s) :: pr) gr.prologue xs

let add_to_epilogue gr s =
  gr.epilogue <- (Ocaml s)::gr.epilogue

let add_many_to_epilogue gr xs =
  gr.epilogue <- List.rev_append (List.rev_map (fun s -> Ocaml s) xs) gr.epilogue

(** [s] is a parameter and type declaration, e.g., "x:int", and this extracts the type *)
let get_paramtype s =
  try
    Scanf.sscanf s "%[^:]:%[^\\000]" (fun var typ -> typ)
  with End_of_file -> "int" (* no colon -> no type -> use int as default *)

(** s is a parameter and type declaration, e.g., "x:int", and this extracts the parameter *)
let get_param s =
  try
    Scanf.sscanf s "%[^:]:%[^\\000]" (fun var typ -> var)
  with End_of_file -> s (* no colon -> no type *)

(* rhs constructors *)
let mkRHS r          = {r = r; a = mkAnnot None}

let mkLIT(s)          = mkRHS(Lit(!Compileopt.case_sensitive,s))
let mkPOSITION m      = mkRHS(Position m)
let mkSYMB(s,e,l)     = mkRHS(Symb(s,e,[],l))
let mkSYMB2(s,e,a,l)  = mkRHS(Symb(s,e,a,l))
let mkACTION(e)       = mkRHS(Action(Some e,None))
let mkACTION2(e,l)    = mkRHS(Action(e,l))
let mkASSIGN(r,x,y)   = mkRHS(Assign(r,x,y))
let mkCHARRANGE(m,n)  = mkRHS(CharRange(m,n))
let mkPROSE(s)        = mkRHS(Prose(s))
let mkLOOKAHEAD(b,r)  = mkRHS(Lookahead(b,r))
let mkOPT(r)          = mkRHS(Opt(r))
let mkRCOUNT(c,r)     = mkRHS(Rcount(c,r))
let mkSTAR(m,n,r)     = mkRHS(Star(Bounds(m,n),r)) (* Omits optimization *[r] --> *r for now, see bnf.cyc *)
let mkSTAR2(a,r)      = mkRHS(Star(a,r))
let mkHASH(m,n,r)     = mkRHS(Hash(Bounds(m,n),r))
let mkHASH2(a,r)      = mkRHS(Hash(a,r))
let mkMINUS(r1,r2)    = mkRHS(Minus(r1,r2))
let mkWHEN(e)         = mkRHS(When(e))
let mkDELAY(e,topt)   = mkRHS(Delay(true,e,topt))
let mkDBRANCH(e,c)   = mkRHS(DBranch(e,c))
let mkBOX(e,topt,n)   = mkRHS(Box(e,topt,n))


let rec mkSEQ = function [] -> mkLIT ""
  | [r] -> r
  | r::rs ->
      let r2 = mkSEQ rs in
      mkRHS(Seq(r,None,None,r2))

let mkALT = function [] -> mkLIT ""  (* TODO: mkALT [] should probably match nothing, need when false *)
  | r::rs ->
      List.fold_left
        (fun x y ->
          mkRHS(Alt(x,y)))
        r
        rs

let mkSEQ2(r1,x,y,r2) = mkRHS(Seq(r1,x,y,r2))
let mkALT2 (x,y) = mkALT[x;y]

(** A curried formulation of the constructor functions above. *)
module Curried_constructors = struct
 let mkLIT s           = mkRHS(Lit(!Compileopt.case_sensitive,s))
 let mkPOSITION m      = mkRHS(Position m)
 let mkSYMB s e l      = mkRHS(Symb(s,e,[],l))
 let mkSYMB2 s e a l   = mkRHS(Symb(s,e,a,l))
 let mkACTION e        = mkRHS(Action(Some e,None))
 let mkACTION2 e l     = mkRHS(Action(e,l))
 let mkASSIGN r x y    = mkRHS(Assign(r,x,y))
 let mkCHARRANGE m n   = mkRHS(CharRange(m,n))
 let mkPROSE s         = mkRHS(Prose(s))
 let mkLOOKAHEAD b r   = mkRHS(Lookahead(b,r))
 let mkOPT r           = mkRHS(Opt(r))
 let mkRCOUNT c r      = mkRHS(Rcount(c,r))
 let mkSTAR m n r      = mkRHS(Star(Bounds(m,n),r)) (* Omits optimization *[r] --> *r for now, see bnf.cyc *)
 let mkSTAR2 a r       = mkRHS(Star(a,r))
 let mkHASH m n r      = mkRHS(Hash(Bounds(m,n),r))
 let mkHASH2 a r       = mkRHS(Hash(a,r))
 let mkMINUS r1 r2     = mkRHS(Minus(r1,r2))
 let mkWHEN e          = mkRHS(When(e))
 let mkDELAY e topt    = mkRHS(Delay(true,e,topt))
 let mkDBRANCH e c     = mkRHS(DBranch(e,c))
 let mkBOX e topt n    = mkRHS(Box(e,topt,n))

 (* leave these abbreviations here just for completeness. *)
 let mkSEQ = mkSEQ
 let mkALT = mkALT

 let mkSEQ2 r1 x y r2  = mkRHS(Seq(r1,x,y,r2))
 let mkALT2 x y = mkALT[x;y]
end

(* Convert binary Alt representation to list of alternatives *)
let alts_of_rhs =
  let rec loop l r = match r.r with
  | Alt(r2,r3) -> loop (loop l r3) r2
  | _ -> r::l in
  loop []

let iter_rule_postorder f r =
  let rec loop r =
    (match r.r with
       | Symb _
       | Assign _
       | Action _
       | Position _
       | Box _
       | Prose _
       | When _
       | DBranch _
       | Delay _
       | CharRange _
       | Lit _ -> ()

       | Rcount (_, r1)
       | Star (_, r1)
       | Hash (_, r1)
       | Lookahead  (_, r1)
       | Opt r1 -> loop r1

       | Minus (r1, r2)
       | Alt (r1, r2)
       | Seq(r1,_,_,r2) ->
           loop r1; loop r2);
    f r in
  loop r

let dependency_graph ds =
  let rec get_depend g n r = match r.r with
    (* Add dependencies for n to a graph given definition r *)
  | Symb(x,_,_,_) ->
      Tgraph.add_edge (Tgraph.add_node g x) n x
  | Position _
  | Prose _
  | When _ | Action _ | Box _ | Delay _
  | DBranch _
  | CharRange _
  | Lit _ -> g
  | Minus(r2,r3)
  | Seq(r2,_,_,r3)
  | Alt(r2,r3) ->
      get_depend (get_depend g n r2) n r3
  | Assign(r2,_,_)
  | Opt(r2)
  | Rcount(_,r2)
  | Star(_,r2)
  | Hash(_,r2)
  | Lookahead(_,r2) ->
      get_depend g n r2
  in
  List.fold_left
    (fun result -> function
        RuleDef(n,r, _)->
          get_depend (Tgraph.add_node result n) n r
      | _ -> result)
    Tgraph.empty
    ds

let sort_definitions ds =
  let cmp =
    let (index_map,_) = (* Maps nonterminals to indices 0..n in topological sort order *)
      let g = dependency_graph ds in
      let in_order = List.rev(Tgraph.tsort g) in
      List.fold_left
        (fun (m,i) n -> (PMap.add n i m, i+1))
        (PMap.empty,0)
        in_order in
    fun rd1 rd2 ->
      (match rd1,rd2 with
        RuleDef(n1,_,_), RuleDef(n2,_,_) ->
          compare (PMap.find n1 index_map) (PMap.find n2 index_map)
      | _,_ -> failwith "sort_definitions")
  in
  List.sort cmp ds

let attribute_table_of_definitions ds =
  let n = List.length ds in
  let t = Hashtbl.create n in
  List.iter (function RuleDef (n, _, a) -> Hashtbl.add t n a | _ -> ()) ds;
  t

(* return the subset of the definitions that are reachable from the roots. *)
let get_reachable ds roots =
  if roots = [] then begin
    Printf.eprintf "Warning: empty roots in get_reachable\n%!";
    ds
  end else
  let g = Tgraph.tc(dependency_graph ds) in
  let union x y = PSet.fold PSet.add x y in
  let reachable =
    List.fold_right
      (fun root rset ->
        let root_targets =
          try Tgraph.get_targets g root
          with Not_found -> PSet.empty in
        PSet.add root (union root_targets rset))
      roots
      PSet.empty in
  let is_reachable x = PSet.mem x reachable in
  List.filter (function RuleDef(n,_,_) -> is_reachable n | _ -> false) ds

let free_nonterminals rules =
  let g = Tgraph.tc(dependency_graph rules) in
  let defined =
    List.concat
      (List.map (function RuleDef(n,_,_) -> [n] | _ -> []) rules) in
  let used =
    let union x y = PSet.fold PSet.add x y in
    List.fold_right
      (fun root used ->
        let used_rhs =
          try Tgraph.get_targets g root
          with Not_found -> PSet.empty in
        PSet.add root (union used_rhs used))
      defined
      PSet.empty in
  let free =
    List.fold_right PSet.remove defined used in
  free

(** Transformations *)


(** try to close the original rules using definitions from external rules *)
let close_definitions rules_orig rules_external =
  let needed = free_nonterminals rules_orig in
  let externals =
    List.concat
      (List.map (function RuleDef(n,_,_) -> [n] | _ -> []) rules_external) in
  let externals_needed =
    List.filter (fun x -> PSet.mem x needed) externals in
  if externals_needed = [] then rules_orig (* b/c of how get_reachable handles this case *) else
  let rules_needed = get_reachable rules_external externals_needed in
  let rules_new = List.append rules_needed rules_orig in
  let still_needed = free_nonterminals rules_new in
  if not(PSet.is_empty still_needed) then
    Printf.eprintf "Warning: missing definitions for %s\n%!"
      (String.concat ", " (PSet.fold (fun hd tl -> hd::tl) still_needed []));
  rules_new

(** Create a new copy of the grammar with all late actions
   removed. Intended for printing out a "cleaner" version of a grammar
   for easier reading. *)
let remove_late_actions gr =
  let rec loop r = match r.r with
    | Action (_, None) | Box _ | Symb (_,_,_,None) | Position true
    | Prose _ | When _ | Delay _
    | DBranch _
    | CharRange _ | Lit _ -> r

    | Symb (s,e,a, Some _) -> mkSYMB2(s,e,a,None)

    | Position false -> mkACTION2(None, None)
    | Action (e, Some _) -> mkACTION2 (e, None)
    | Seq(r2, x,  _, r3) ->
        let r3_rla = loop r3 in
        (match r3_rla.r with
           | Action (None, None) -> loop r2
           | _ ->
               let r2_rla = loop r2 in
               (match r2_rla.r, x with
                  | Action (None, None), None -> r3_rla
                  | _ -> mkSEQ2 (r2_rla, x, None, r3_rla)))
    | Assign(r2, x,  _) -> mkASSIGN(loop r2,x,None)
    | Star(Accumulate (Some _ as x, _), r2) -> mkSTAR2(Accumulate (x, None), loop r2)
    | Star(Accumulate (None, _), r2) -> mkSTAR(0, Infinity, loop r2)
    | Hash(Accumulate (Some _ as x, _), r2) -> mkHASH2(Accumulate (x, None), loop r2)
    | Hash(Accumulate (None, _), r2) -> mkHASH(0, Infinity, loop r2)

    | Star(x, r2) -> mkSTAR2(x, loop r2)
    | Hash(x, r2) -> mkHASH2(x, loop r2)
    | Minus(r2,r3) -> mkMINUS (loop r2, loop r3)
    | Alt(r2,r3) -> mkRHS(Alt(loop r2, loop r3))
    | Opt(r2) -> mkOPT (loop r2)
    | Rcount(x,r2) -> mkRCOUNT(x, loop r2)
    | Lookahead(x, r2) -> mkLOOKAHEAD(x, loop r2)
  in

  let rla_def = function
    | RuleDef (n,r,a) ->
        a.Attr.late_params <- None;
        RuleDef (n, loop r, a)
    | d -> d
  in

  {gr with
     ds= List.map rla_def gr.ds;
     early_producers=PSet.empty;
     late_producers=PSet.empty;
     grammar_late_relevant = false;
  }

(** Lookahead is treated as a base case. *)
let fold f_ind2 f_ind1 f_base r =
  let rec loop r = match r.r with
    | Symb _ | Lit _ | Position _
    | CharRange _ | Prose _ | Action _
    | Box _ | Delay _ | When _
    | DBranch _ | Lookahead _
        -> f_base r
    | Opt r1
    | Star (_, r1)
    | Hash (_, r1)
    | Rcount (_, r1)
    | Assign (r1, _, _)
      -> f_ind1 (loop r1)
    | Minus (r1, r2) | Alt (r1,r2) | Seq (r1,_,_,r2)
        -> f_ind2 (loop r1) (loop r2) in
  loop r

type map_funs = {
  m_base : rhs -> rhs;
  m_opt : rhs -> rhs;
  m_seq : rhs -> var option -> var option -> rhs -> rhs;
  m_assign : rhs -> var option -> var option -> rhs;
  m_alt : rhs -> rhs -> rhs;
  m_minus : rhs -> rhs -> rhs;
  m_rcount : expr -> rhs -> rhs;
  m_star : looper -> rhs -> rhs;
  m_hash : looper -> rhs -> rhs;
(*   m_lookahead : bool -> rhs -> rhs; *)
}

let default_map = {
  m_base = (fun x -> x);
  m_opt = Curried_constructors.mkOPT;
  m_seq = Curried_constructors.mkSEQ2;
  m_assign = Curried_constructors.mkASSIGN;
  m_alt = Curried_constructors.mkALT2;
  m_minus = Curried_constructors.mkMINUS;
  m_rcount = Curried_constructors.mkRCOUNT;
  m_star = Curried_constructors.mkSTAR2;
  m_hash = Curried_constructors.mkHASH2;
(*   m_lookahead = Curried_constructors.mkLOOKAHEAD; *)
}

(** generic map -- map with all inductive cases specified separately
    Lookahead is treated as a base case. *)
let map funs r =
  let rec loop r = match r.r with
    | Symb _ | Lit _ | Position _
    | CharRange _ | Prose _ | Action _
    | Box _ | Delay _ | When _
    | DBranch _ | Lookahead _
        -> funs.m_base r
    | Opt r1 -> funs.m_opt (loop r1)
    | Seq (r1,e,l,r2) -> funs.m_seq (loop r1) e l (loop r2)
    | Assign (r1, v1, v2) -> funs.m_assign (loop r1) v1 v2
    | Alt (r1,r2) -> funs.m_alt (loop r1) (loop r2)
    | Minus (r1, r2) -> funs.m_minus (loop r1) (loop r2)
    | Rcount (e, r1) -> funs.m_rcount e (loop r1)
    | Star (l, r1) -> funs.m_star l (loop r1)
    | Hash (l, r1) -> funs.m_hash l (loop r1) in
  loop r

(* fold: f1:(... -> 'a) -> ... fn:(... -> 'a) -> T -> 'a *)
type 'a fold_funs = {
  f_base : rhs -> 'a;
  f_opt : 'a -> 'a;
  f_seq : 'a -> var option -> var option -> 'a -> 'a;
  f_assign : 'a -> var option -> var option -> 'a;
  f_alt : 'a -> 'a -> 'a;
  f_minus : 'a -> 'a -> 'a;
  f_rcount : expr -> 'a -> 'a;
  f_star : looper -> 'a -> 'a;
  f_hash : looper -> 'a -> 'a;
(*   f_lookahead : bool -> 'a -> 'a; *)
}

(* force a match failure with proper.*)
let fail_case _ = failwith "impossible"

let default_fold = {
  f_base = fail_case;
  f_opt = fail_case;
  f_seq = fail_case;
  f_assign = fail_case;
  f_alt = fail_case;
  f_minus = fail_case;
  f_rcount = fail_case;
  f_star = fail_case;
  f_hash = fail_case;
(*   f_lookahead = fail_case; *)
}

(** generic fold -- all inductive cases specified separately
    Lookahead is treated as a base case. *)
let gfold funs r =
  let rec loop r = match r.r with
    | Symb _ | Lit _ | Position _
    | CharRange _ | Prose _ | Action _
    | Box _ | Delay _ | When _
    | DBranch _ | Lookahead _
        -> funs.f_base r
    | Opt r1 -> funs.f_opt (loop r1)
    | Seq (r1,e,l,r2) -> funs.f_seq (loop r1) e l (loop r2)
    | Assign (r1, v1, v2) -> funs.f_assign (loop r1) v1 v2
    | Alt (r1,r2) -> funs.f_alt (loop r1) (loop r2)
    | Minus (r1, r2) -> funs.f_minus (loop r1) (loop r2)
    | Rcount (e, r1) -> funs.f_rcount e (loop r1)
    | Star (l, r1) -> funs.f_star l (loop r1)
    | Hash (l, r1) -> funs.f_hash l (loop r1) in
  loop r


(** Perform a fold with propogation of information from left-to-right,
    in addition to standard bottom-to-top. Output of left sibling is
    provided as first output parameter.

    Lookahead is treated as a base case.
*)
let lrfold f_ind2 f_ind1 f_base r v_init =
  let rec loop v_left r = match r.r with
    | Symb _ | Lit _ | Position _
    | CharRange _ | Prose _ | Action _
    | Box _ | Delay _ | When _
    | DBranch _ | Lookahead _
        -> f_base r v_left
    | Opt r1
    | Star (_, r1)
    | Hash (_, r1)
    | Rcount (_, r1)
    | Assign (r1, _, _)
      -> f_ind1 v_left (loop v_left r1)
    | Minus (r1, r2) | Alt (r1,r2) | Seq (r1,_,_,r2)
        -> let v1 = loop v_left r1 in
        f_ind2 v_left v1 (loop v1 r2) in
  loop v_init r

(** [lrfold] where only the base elements are interesting. The inductive cases
    just propogate changes L-R. *)
let lrfold_b f_b =
  lrfold (fun _ _ v2 -> v2) (fun _ v1 -> v1) f_b
