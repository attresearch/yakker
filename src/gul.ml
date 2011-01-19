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
  | Delay of expr * ty option         (* argument, return type *)
  | When of expr
  | Opt of rhs
  | Seq of rhs * var option * var option * rhs
  | Assign of rhs * var option * var option
  | Alt of rhs * rhs
  | Minus of rhs * rhs
  | Rcount of expr * rhs
  | Star of looper * rhs
  | Hash of looper * rhs
  | Lookahead of bool * rhs

let dupAnnot a =
  { css = a.css;
    early_relevant = a.early_relevant;
    late_relevant = a.late_relevant;
    pre = a.pre;
    post = a.post;
    is_regular = a.is_regular;
    early_assignments=a.early_assignments;
    late_assignments=a.late_assignments;
    precedence = a.precedence;
  }

let rec mkAnnot = function
  | None -> {css = None; early_relevant = false; late_relevant = false;
           pre = 0; post = 0; is_regular = false; precedence = Default_prec;
           early_assignments=PSet.empty;late_assignments=PSet.empty}
  | Some r -> let a = dupAnnot r.a in a.css <- None; a

let dupRule r = {a=dupAnnot r.a;r=r.r}

(** only need to copy mutable material. (otherwise, there's no
    value to copying that I know of). So, leaves are copied shallowly.
 *)
let rec copy_b = function
  | (Position _
    | Symb _
    | Prose _
    | When _ | Action _ | Box _ | Delay _
    | CharRange _
    | Lit _) as x -> x
  | Minus(r2,r3) -> Minus(copyRule r2,copyRule r3)
  | Seq(r2,v_e,v_l,r3) -> Seq(copyRule r2,v_e,v_l,copyRule r3)
  | Assign(r2,v_e,v_l) -> Assign(copyRule r2,v_e,v_l)
  | Alt(r2,r3) -> Alt(copyRule r2,copyRule r3)
  | Opt(r2) -> Opt (copyRule r2)
  | Rcount(x,r2) -> Rcount(x,copyRule r2)
  | Star(x,r2) -> Star(x,copyRule r2)
  | Hash(x,r2) -> Hash(x,copyRule r2)
  | Lookahead (b,r2) -> Lookahead (b, copyRule r2)

(** deep copy *)
and copyRule r = {a=dupAnnot r.a;r= copy_b r.r}

module Attr = struct
  type t = {mutable early_params: string option;
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
    mutable wrapped_history : bool;
    (** Flag indicating whether elements saved in history need to be
        wrapped. *)

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
   wrapped_history = false;
   tokmap = [];
   gildefs = [];
   precs = pd;
  }

let add_to_prologue gr s =
  gr.prologue <- (Ocaml s)::gr.prologue

let add_to_epilogue gr s =
  gr.epilogue <- (Ocaml s)::gr.epilogue

(* rhs constructors *)
let mkRHS r          = {r = r; a = mkAnnot None}

let mkLIT(s)          = mkRHS(Lit(!Compileopt.case_sensitive,s))
let mkPOSITION m      = mkRHS(Position m)
let mkSYMB(s,e,l)     = mkRHS(Symb(s,e,[],l))
let mkSYMB2(s,e,a,l)  = mkRHS(Symb(s,e,a,l))
let mkACTION(e)       = mkRHS(Action(Some e,None))
let mkACTION2(e,l)    = mkRHS(Action(e,l))
let mkSEQ2(r1,x,y,r2) = mkRHS(Seq(r1,x,y,r2))
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
let mkDELAY(e,topt)   = mkRHS(Delay(e,topt))
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

let mkALT2 (x,y) = mkALT[x;y]

(* Convert binary Alt representation to list of alternatives *)
let alt2rules =
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
     wrapped_history = false;
  }
