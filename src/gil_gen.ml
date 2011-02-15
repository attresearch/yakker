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
open Analyze.First_set_gil_lex

let remove_memo = ref true
let use_bucket = ref true
let use_arrays = ref true

let bprintf = Printf.bprintf

let symb_fun_name x = Variables.bnf2ocaml ("s_" ^ x)
let symb_ref_name x = Variables.bnf2ocaml ("s_" ^ x ^ "_ref")

let lookahead_name = Variables.fresh ()

exception Myexcept of string

(*******************
Apply lookahead to LL(1) sub-languages in the following situations:
Assume c is the next character.
1. r -> Star r1
   intersect first(r1) follow(r) = {}
2. r -> Alt r1 r2
   r1, r2 are not nullable, and intersect first(r1) first(r2) = {}
   r1 is nullable, r2 is not nullable, and intersect (union first(r1) follow(r)) first(r2) = {}
   r2 is nullable, r1 is not nullable, and intersect (union first(r2) follow(r)) first(r1) = {}
   r1, r2 are nullable, and intersect first(r1) first(r2) follow(r) = {}
********************)

let pr_ts ts =
  let printone (tokenizer, ntl) =
    Printf.sprintf "%s: %s\n" tokenizer (String.concat "," (List.map (fun nt -> nt) ntl))
  in
    String.concat "\n" (List.map printone ts)

let is_mem_wrap ntl tokendefs =
  if ntl = [] then "raise (Failure \"Empty token list\")" else
    let checkone (ocaml_constructor,carried_type) =
        match carried_type with
            Some _ ->
              Printf.sprintf "| %s _ " ocaml_constructor
          | None ->
              Printf.sprintf "| %s " ocaml_constructor
    in
      Printf.sprintf "match result with %s -> true | _ -> false" (String.concat " " (List.map checkone ntl))

let  alt_w_lookahead_lex_str ntl1 ntl2 tokendefs =
Printf.sprintf "tokenizer r1 r2 k_s ykb v =
  if fill2 ykb 1 then begin
    match tokenizer ykb with
      | None ->
          k_s ykb v
      | Some (n,result) ->
          if (%s) then r1 k_s ykb v
          else if (%s) then r2 k_s ykb v
          else k_s ykb v
  end
  else k_s ykb v
in
" (is_mem_wrap ntl1 tokendefs) (is_mem_wrap ntl2 tokendefs)

let  alt_w_lookahead_lex_str2 ntl tokendefs =
Printf.sprintf "tokenizer r1 r2 k_s ykb v =
  if fill2 ykb 1 then begin
    match tokenizer ykb with
      | None ->
          r2 k_s ykb v
      | Some (n,result) ->
          if (%s) then r1 k_s ykb v
          else r2 k_s ykb v
  end
  else
    r2 k_s ykb v
in
" (is_mem_wrap ntl tokendefs)

let  alt_w_lookahead_lex_str0 ntl1 ntl2 tokendefs =
Printf.sprintf "tokenizer r1 r2 k_s ykb v =
  if fill2 ykb 1 then begin
    match tokenizer ykb with
      | None ->
          ()
      | Some (n,result) ->
          if (%s) then r1 k_s ykb v
          else if (%s) then r2 k_s ykb v
          else ()
  end
  else ()
in
" (is_mem_wrap ntl1 tokendefs) (is_mem_wrap ntl2 tokendefs)

let nalt_w_lookahead_lex_str ntll tokendefs f =
bprintf f "tokenizer rs k_s ykb v =
  if Array.length rs < 2 then raise (Invalid_argument \"nalt_w_lookahead_lex called with less than two branches.\")
  else if fill2 ykb 1 then begin
    match tokenizer ykb with
      | None ->
          ()
      | Some (n,result) ->
          if (%s) then (Array.unsafe_get rs 0) k_s ykb v
" (is_mem_wrap (List.nth ntll 0) tokendefs);
for i = 1 to List.length ntll - 1 do
bprintf f
"         else if (%s) then (Array.unsafe_get rs %d) k_s ykb v
" (is_mem_wrap (List.nth ntll i) tokendefs) i
done;
bprintf f "%s"
"         else ()
  end
  else ()
in
"
let nalt_w_lookahead_lex_bkt_str bktable tokendefs f =
  bprintf f "%s" "tokenizer rs k_s ykb v =
  if Array.length rs < 2 then raise (Invalid_argument \"nalt_w_lookahead_lex_bkt called with less than two branches.\")
  else if fill2 ykb 1 then begin
    match tokenizer ykb with
      | None ->
          ()
      | Some (n,result) ->
          match result with
";
  let matchone ((ocaml_constructor,carried_type),rsil) =
    match carried_type with
      | Some _ ->
          if Array.length rsil < 2 then
            bprintf f
"           | %s _ -> (Array.unsafe_get rs %d) k_s ykb v
" ocaml_constructor (Array.unsafe_get rsil 0)
          else begin
            bprintf f
"           | %s _ -> (nalt [|" ocaml_constructor;
                Array.iter (fun i ->
                                  bprintf f " ";
                                  bprintf f "(Array.unsafe_get rs %d)" i;
                                  bprintf f ";") rsil;
                bprintf f "|] k_s ykb v)
"
          end
      | None ->
          if Array.length rsil < 2 then
            bprintf f
"           | %s -> (Array.unsafe_get rs %d) k_s ykb v
" ocaml_constructor (Array.unsafe_get rsil 0)
          else begin
            bprintf f
"           | %s -> (nalt [|" ocaml_constructor;
                Array.iter (fun i ->
                                  bprintf f " ";
                                  bprintf f "(Array.unsafe_get rs %d)" i;
                                  bprintf f ";") rsil;
                bprintf f "|] k_s ykb v)
"
          end
  in
    List.iter matchone bktable;
    bprintf f "%s"
"           | _ -> ()
  end
  else ()
in
"

let nalt_w_lookahead_bkt_str cstable f =
  bprintf f "rs k_s ykb v =
  if Array.length rs < 2 then raise (Invalid_argument \"nalt_w_lookahead_bkt called with less than two branches.\")
  else if fill2 ykb 1 then begin
    match (Char.code (get_current ykb)) with
      ";
  let matchone (cs,rsil) =
      Cs.iter (fun code -> bprintf f "| %d " code) cs;
      bprintf f "-> ";
      if List.length rsil < 2 then
        bprintf f "(Array.unsafe_get rs %d) k_s ykb v
      " (List.hd rsil)
      else begin
        bprintf f "(nalt [|";
                List.iter (fun i ->
                                  bprintf f " ";
                                  bprintf f "(Array.unsafe_get rs %d)" i;
                                  bprintf f ";") rsil;
                bprintf f "|] k_s ykb v)
      "
      end
  in
    List.iter matchone cstable;
    bprintf f "| _ -> ()
  end
  else ()
in
"

let nalt_w_lookahead_str0 fsl f =
  bprintf f "rs k_s ykb v =
  if Array.length rs < 2 then raise (Invalid_argument \"nalt_w_lookahead called with less than two branches.\")
  else if fill2 ykb 1 then begin
    match (Char.code (get_current ykb)) with
      ";
  let matchone i (cs,ts) =
      Cs.iter (fun code -> bprintf f "| %d " code) cs;
      bprintf f "-> ";
      bprintf f "(Array.unsafe_get rs %d) k_s ykb v
      " i;
      i+1
  in
  let _ = List.fold_left matchone 0 fsl in
    bprintf f "| _ -> ()
  end
  else ()
in
"

let star_w_lookahead_lex_str ntl tokendefs fn =
Printf.sprintf "tokenizer r k_s ykb_init v_init =
  let rec loop ykb v =
    if fill2 ykb 1 then
      match tokenizer ykb with
        | None ->
            k_s ykb v
        | Some (_,result) ->
            if (%s) then r loop ykb v
            else k_s ykb v
    else k_s ykb v
  in
    loop ykb_init v_init
in
" (is_mem_wrap ntl tokendefs)


let pr_gil_lex f use_refs r0 first_map follow_map n tokmap =
  let rec loop r cur_fls =
    match r with
      | Gil.When_special _ -> Util.todo "Gil_gen.pr_gil_lex.loop.When_special"
      | Gil.DBranch _      -> Util.todo "Gil_gen.pr_gil_lex.loop.DBranch"
      | Gil.Action(e) ->
          bprintf f "(action (%s))" e
      | Gil.When(e1,e2) ->
          bprintf f "(pred (%s) (%s))" e1 e2
      | Gil.Box(e,_) ->
          bprintf f "(box (%s))"  e
      | Gil.Symb(x,y,z) ->
          if use_refs then
            bprintf f "(rsymb %s" (symb_ref_name x)
          else
            bprintf f "(symb %s" (symb_fun_name x);
          (match y with
             | None -> bprintf f " _call_fresh"
             | Some args -> bprintf f " (%s)" args);
          (match z with
             | None -> bprintf f " _ignore_ret"
             | Some binder -> bprintf f " (%s)" binder);
          bprintf f ")"
      | Gil.CharRange(low,high) ->
          if low = high then
            bprintf f "(term '%s')" (Char.escaped (Char.chr low))
          else
            bprintf f "(char_range %d %d)" low high
      | Gil.Lit(case_sensitive,x) ->
          if x = "" then bprintf f "eps"
          else if String.length x = 1 then bprintf f "(term '%s')" (Char.escaped x.[0])
          else bprintf f "(terms \"%s\")" (String.escaped x)
      | Gil.Star(r2) ->
          if !Compileopt.lookahead then
            let fs = first_gil_lex r2 first_map in
            let fscs,fsts = fs.nonempty in
            let fls = cur_fls in
            let flscs,flsts = fls in
            let is,flag = fs_fs_dstct fs.nonempty fls in
              if is && fs_notempty fs.nonempty && fs_notempty fls && fs.maybe_nonempty = [] && fs.maybe_empty = [] then begin
                if flag then begin
                  let (tokenizer,ntl) = List.nth fsts 0 in
                  let tokendefs = try List.assoc tokenizer tokmap with Not_found -> (Printf.eprintf "Warning: Unable to find %s" tokenizer;[]) in
                  let fn = Variables.fresh () in
                    bprintf f "\n(let rec star_w_lookahead_lex_%s %s " fn (star_w_lookahead_lex_str ntl tokendefs fn);
                    bprintf f "(star_w_lookahead_lex_%s %s " fn tokenizer;
                        loop r2 cur_fls;
                        bprintf f "))"
                end
                else begin
                  let fsstr,flsstr = Cs.to_code fscs,Cs.to_code flscs in
                    bprintf f "(star_w_lookahead (%s) " fsstr;
                        loop r2 cur_fls;
                        bprintf f ")"
                end
              end
              else begin
                bprintf f "(star ";
                    loop r2 cur_fls;
                    bprintf f ")"
              end
          else begin
            bprintf f "(star ";
                loop r2 cur_fls;
                bprintf f ")"
          end
      | Gil.Lookahead(true,r2) ->
          (match r2 with
             | Gil.CharRange(low,high) ->
                 bprintf f "(pos_cr_lookahead %d %d)" low high;
             | _ ->
                 bprintf f "(pos_lookahead ";
                 loop r2 cur_fls;
                 bprintf f ")")
      | Gil.Lookahead(false,r2) ->
          (match r2 with
             | Gil.CharRange(low,high) ->
                 bprintf f "(neg_cr_lookahead %d %d)" low high;
             | _ ->
                 bprintf f "(neg_lookahead ";
                 loop r2 cur_fls;
                 bprintf f ")")
      | Gil.Seq _ when !use_arrays ->
          bprintf f "(nseq [|";
          let rs = Gil.seq2rules r in
          if not !Compileopt.lookahead then begin
            List.iter (fun r_i ->
                         bprintf f " ";
                         loop r_i cur_fls;
                         bprintf f ";") rs;
          end
          else begin
            let computeone r_i (this_fls,list) =
              let fs = first_gil_lex r_i first_map in
              let new_fls =
                if not(fs_notempty fs.nonempty) || fs.epsilon || fs.maybe_empty <> [] then
                  union_fls fs.nonempty this_fls
                else fs.nonempty
              in
              (new_fls,this_fls::list)
            in
            let (_,flslist) = List.fold_right computeone rs (cur_fls,[]) in
            let do_one i r_i =
              bprintf f " ";
              loop r_i (List.nth flslist i);
              bprintf f ";";
              i+1
            in
            let _ = List.fold_left do_one 0 rs in
            ()
          end;
          bprintf f "|])"
      | Gil.Seq (r2, r3) ->
          if !Compileopt.lookahead then begin
            let fs = first_gil_lex r3 first_map in
            let new_fls =
              if not(fs_notempty fs.nonempty) || fs.epsilon || fs.maybe_empty <> [] then
                union_fls fs.nonempty cur_fls
              else fs.nonempty
            in
            bprintf f "(seq ";
            loop r2 new_fls;
            bprintf f " ";
            loop r3 cur_fls;
            bprintf f ")"
          end
          else begin
            bprintf f "(seq ";
            loop r2 cur_fls;
            bprintf f " ";
            loop r3 cur_fls;
            bprintf f ")"
          end

      | Gil.Alt _ when !use_arrays ->
          handle_alts cur_fls r
      | Gil.Alt (r2, r3) ->
          let regular_alt () =
            bprintf f "(alt ";
                loop r2 cur_fls;
                bprintf f " ";
                loop r3 cur_fls;
            bprintf f ")"
          in
          let alt_wlookahead fsstr2 fsstr3 =
            let fsname2,fsname3 = Variables.fresh(), Variables.fresh() in
              bprintf f "(let %s = (%s) in " fsname2 fsstr2;
              bprintf f "let %s = (%s) in " fsname3 fsstr3;
              bprintf f "(alt_w_lookahead %s %s " fsname2 fsname3;
              loop r2 cur_fls;
              bprintf f " ";
              loop r3 cur_fls;
              bprintf f "))"
          in
          let alt_wlookahead_lex ntl2 ntl3 tokendefs tokenizer =
            let fn = Variables.fresh () in
              bprintf f "\n(let alt_w_lookahead_lex_%s %s" fn (alt_w_lookahead_lex_str ntl2 ntl3 tokendefs);
              bprintf f "(alt_w_lookahead_lex_%s %s " fn tokenizer;
                  loop r2 cur_fls;
              bprintf f " ";
              loop r3 cur_fls;
                  bprintf f "))"
          in
          if !Compileopt.lookahead then begin
            let fs2,fs3 = first_gil_lex r2 first_map, first_gil_lex r3 first_map in
              if fs2.maybe_empty <> [] || fs3.maybe_empty <> [] || fs2.maybe_nonempty <> [] || fs3.maybe_nonempty <> [] then
                regular_alt ()
              else
              match (fs2.epsilon,fs3.epsilon) with
                | (false,false) ->
                    let is,flag = fs_fs_dstct fs2.nonempty fs3.nonempty in
                      if is && fs_notempty fs2.nonempty && fs_notempty fs3.nonempty then begin  (* First sets of the two branches don't intersect and r is not nullable, then play lookahead *)
                        let fs2cs,fs2ts = fs2.nonempty in
                        let fs3cs,fs3ts = fs3.nonempty in
                          if flag then begin
                            let (tokenizer,ntl2) = List.nth fs2ts 0 in
                            let (tokenizer,ntl3) = List.nth fs3ts 0 in
                            let fn = Variables.fresh () in
                            let tokendefs = try List.assoc tokenizer tokmap with Not_found -> (Printf.eprintf "Warning: Unable to find %s" tokenizer; []) in
                              bprintf f "\n(let alt_w_lookahead_lex_%s %s" fn (alt_w_lookahead_lex_str0 ntl2 ntl3 tokendefs);
                              bprintf f "(alt_w_lookahead_lex_%s %s " fn tokenizer;
                                  loop r2 cur_fls;
                              bprintf f " ";
                              loop r3 cur_fls;
                                  bprintf f "))"
                          end
                          else begin
                            let fsstr2,fsstr3 = Cs.to_code fs2cs,Cs.to_code fs3cs in
                            let fsname2,fsname3 = Variables.fresh(), Variables.fresh() in
                              bprintf f "(let %s = (%s) in " fsname2 fsstr2;
                              bprintf f "let %s = (%s) in " fsname3 fsstr3;
                              bprintf f "(alt_w_lookahead0 %s %s " fsname2 fsname3;
                              loop r2 cur_fls;
                              bprintf f " ";
                              loop r3 cur_fls;
                              bprintf f "))"
                          end
                      end
                      else regular_alt ()
                | (true,false) ->
                    let fls = cur_fls in
                    let fs2cs,fs2ts = fs2.nonempty in
                    let flscs,flsts = fls in
                    let fs2u = union_cs fs2cs flscs,union_ts fs2ts flsts in
                      if fs_notempty fls && fs_notempty fs3.nonempty then begin
                        let is,flag = fs_fs_dstct fs2u fs3.nonempty in
                          if is then begin
                            let fs3cs,fs3ts = fs3.nonempty in
                            if flag then begin
                                let (tokenizer,ntl3) = List.nth fs3ts 0 in
                                let tokendefs = try List.assoc tokenizer tokmap with Not_found -> (Printf.eprintf "Warning: Unable to find %s" tokenizer; []) in
                                let fsname3 = Variables.fresh () in
                                  bprintf f "\n(let alt_w_lookahead_lex2_%s %s" fsname3 (alt_w_lookahead_lex_str2 ntl3 tokendefs);
                                  bprintf f "(alt_w_lookahead_lex2_%s %s " fsname3 tokenizer;
                                      loop r3 cur_fls;
                                  bprintf f " ";
                                  loop r2 cur_fls;
                                      bprintf f "))"
                            end
                            else begin
                                let fsstr3 = Cs.to_code fs3cs in
                                let fsname3 = Variables.fresh () in
                                  bprintf f "(let %s = (%s) in " fsname3 fsstr3;
                                  bprintf f "(alt_w_lookahead2 %s " fsname3;
                                  loop r3 cur_fls;
                                  bprintf f " ";
                                  loop r2 cur_fls;
                                  bprintf f "))"
                            end
                          end
                          else regular_alt ()
                      end
                      else  regular_alt ()
                | (false,true) ->
                    let fls = cur_fls in
                    let fs3cs,fs3ts = fs3.nonempty in
                    let flscs,flsts = fls in
                    let fs3u = union_cs fs3cs flscs,union_ts fs3ts flsts in
                      if fs_notempty fls && fs_notempty fs2.nonempty then begin
                        let is,flag = fs_fs_dstct fs3u fs2.nonempty in
                          if is then begin
                            let fs2cs,fs2ts = fs2.nonempty in
                            if flag then begin
                                let (tokenizer,ntl2) = List.nth fs2ts 0 in
                                let tokendefs = try List.assoc tokenizer tokmap with Not_found -> (Printf.eprintf "Warning: Unable to find %s" tokenizer; []) in
                                let fsname2 = Variables.fresh () in
                                  bprintf f "\n(let alt_w_lookahead_lex3_%s %s" fsname2 (alt_w_lookahead_lex_str2 ntl2 tokendefs);
                                  bprintf f "(alt_w_lookahead_lex3_%s %s " fsname2 tokenizer;
                                      loop r2 cur_fls;
                                  bprintf f " ";
                                  loop r3 cur_fls;
                                      bprintf f "))"
                            end
                            else begin
                                let fsstr2 = Cs.to_code fs2cs in
                                let fsname2 = Variables.fresh () in
                                  bprintf f "(let %s = (%s) in " fsname2 fsstr2;
                                  bprintf f "(alt_w_lookahead2 %s " fsname2;
                                  loop r2 cur_fls;
                                  bprintf f " ";
                                  loop r3 cur_fls;
                                  bprintf f "))"
                            end
                          end
                          else regular_alt ()
                      end
                      else regular_alt ()
                | (true,true) ->
                    let fls = cur_fls in
                    let is,flag = fs3dstct fs2.nonempty fs3.nonempty fls in
                      if is && fs_notempty fs2.nonempty && fs_notempty fs3.nonempty && fs_notempty fls then begin
                        let fs2cs,fs2ts = fs2.nonempty in
                        let fs3cs,fs3ts = fs3.nonempty in
                          if flag then begin
                            let (tokenizer,ntl2) = List.nth fs2ts 0 in
                            let (tokenizer,ntl3) = List.nth fs3ts 0 in
                            let tokendefs = try List.assoc tokenizer tokmap with Not_found -> (Printf.eprintf "Warning: Unable to find %s\n" tokenizer; []) in
                              alt_wlookahead_lex ntl2 ntl3 tokendefs tokenizer
                          end
                          else begin
                          let fsstr2,fsstr3 = Cs.to_code fs2cs,Cs.to_code fs3cs in
                            alt_wlookahead fsstr2 fsstr3
                          end
                      end
                      else regular_alt ()
          end
          else regular_alt ()

  and handle_alts cur_fls r =
    let rs = Gil.alt2rules r in
    if not !Compileopt.lookahead then begin
      bprintf f "(nalt [|";
      List.iter (fun r_i ->
                   bprintf f " ";
                   loop r_i cur_fls;
                   bprintf f ";") rs;
      bprintf f "|])"
    end
    else begin
      let fss = List.map (fun r_i -> first_gil_lex r_i first_map) rs in
      let fsl = List.map (fun fs -> fs.nonempty) fss in
      if List.for_all (fun fs -> fs_notempty fs.nonempty && not(fs.epsilon) && fs.maybe_empty = []) fss then
        let is,flag = fssdstct fsl in
        if is then
          if flag then begin
            let fn = Variables.fresh () in
            let (tokenizer, _) = List.nth (let cs,ts = List.nth fsl 0 in ts) 0 in
            let tokendefs = try List.assoc tokenizer tokmap with Not_found -> (Printf.eprintf "Warning: Unable to find %s" tokenizer; []) in
            let ntll = List.map (fun (cs,ntli) -> let (tknz,ntl) = List.nth ntli 0 in ntl) fsl in
            bprintf f "\n(let nalt_w_lookahead_lex_%s " fn;
            nalt_w_lookahead_lex_str ntll tokendefs f;
            bprintf f "(nalt_w_lookahead_lex_%s %s [|" fn tokenizer;
            List.iter (fun r_i ->
                         bprintf f " ";
                         loop r_i cur_fls;
                         bprintf f ";") rs;
            bprintf f "|]))"
          end
          else begin
            (*
              let fn = Variables.fresh () in
              bprintf f "\n(let nalt_w_lookahead_%s " fn;
              nalt_w_lookahead_str0 fsl f;
              bprintf f "(nalt_w_lookahead_%s [|" fn;
              List.iter (fun r_i ->
              bprintf f " ";
              loop r_i cur_fls;
              bprintf f ";") rs;
              bprintf f " |]))";
            *)
            (* The following code uses n if-then-else statements while the above uses a single ocaml match statement *)
            let fs2code (fscs,fsts) =
              let fsstr = Cs.to_code fscs in
              let fsname = Variables.fresh() in
              bprintf f "(let %s = (%s) in " fsname fsstr;
              fsname
            in
            let fslist = List.map fs2code fsl in
            bprintf f "(nalt_w_lookahead [|";
            List.iter (fun fsname -> bprintf f " %s;" fsname) fslist;
            bprintf f "|] [|";
            List.iter (fun r_i ->
                         bprintf f " ";
                         loop r_i cur_fls;
                         bprintf f ";") rs;
            bprintf f "|])";
            List.iter (fun _ ->  bprintf f ")") fsl
          end
        else if !use_bucket && one_tokenizer fsl then begin
          let union_ntl (i,list) (cs,ts) =
            let (tknz,ntl) = List.hd ts in
            let doone mylist nt =
              if List.mem_assoc nt mylist then
                let oldntl = List.assoc nt mylist in
                let oldlist = List.remove_assoc nt mylist in
                oldlist@[(nt, Array.append oldntl [|i|])]
              else mylist@[(nt,[|i|])]
            in
            (i+1), List.fold_left doone list ntl
          in
          let (_,bktable) = List.fold_left union_ntl (0,[]) fsl in
          let fn = Variables.fresh () in
          let (tokenizer, _) = List.nth (let cs,ts = List.nth fsl 0 in ts) 0 in
          let tokendefs = try List.assoc tokenizer tokmap with Not_found -> (Printf.eprintf "Warning: Unable to find %s" tokenizer; []) in
          bprintf f "\n(let nalt_w_lookahead_lex_bkt_%s " fn;
          nalt_w_lookahead_lex_bkt_str bktable tokendefs f;
          bprintf f "(nalt_w_lookahead_lex_bkt_%s %s [|" fn tokenizer;
          List.iter (fun r_i ->
                       bprintf f " ";
                       loop r_i cur_fls;
                       bprintf f ";") rs;
          bprintf f "|]))"
        end
        else if !use_bucket && all_char fsl then begin
          let construct_cslist (i,list,orglist) (topcs,ts) =
            let compare_one (flag,comparecs,mylist) (dsjcs,oldbl) =
              if flag then begin
                let x = Cs.dup dsjcs in
                let _ = Cs.intersect x comparecs in
                if Cs.count x = 0 then
                  flag,comparecs,mylist@[(dsjcs,oldbl)]
                else if Cs.compare x comparecs = 0 then (* case: new cs is the subset of an existing cs *)
                  let _ = Cs.difference dsjcs x in
                  if Cs.count dsjcs = 0 then (* case: new cs is the same as an existing cs *)
                    false,Cs.empty(),mylist@[(x,oldbl@[i])]
                  else
                    false,Cs.empty(),mylist@[(x,oldbl@[i]);(dsjcs,oldbl)]
                else if Cs.compare x dsjcs = 0 then (* case: new cs is the superset of an existing cs *)
                  let _ = Cs.difference comparecs x in
                  flag,comparecs,mylist@[(x,oldbl@[i])]
                else
                  let _ = Cs.difference comparecs x in
                  let _ = Cs.difference dsjcs x in
                  flag,comparecs,mylist@[(dsjcs,oldbl);(x,oldbl@[i])]
              end
              else flag,comparecs,mylist@[(dsjcs,oldbl)]
            in
            let (_,comparecs,newlist) = List.fold_left compare_one (true,(Cs.dup topcs),[]) list in
            let newlist = if Cs.count comparecs = 0 then newlist else newlist@[(comparecs,[i])] in
            (i+1),newlist,orglist
          in
          let (cs0,ts0) = List.hd fsl in
          let _,cstable,_ = List.fold_left construct_cslist (1,[((Cs.dup cs0),[0])],fsl) (List.tl fsl) in
          let fn = Variables.fresh () in
          bprintf f "\n(let nalt_w_lookahead_bkt_%s " fn;
          nalt_w_lookahead_bkt_str cstable f;
          bprintf f "(nalt_w_lookahead_bkt_%s [|" fn;
          List.iter (fun r_i ->
                       bprintf f " ";
                       loop r_i cur_fls;
                       bprintf f ";") rs;
          bprintf f "|]))"
        end
        else begin
          bprintf f "(nalt [|";
          List.iter (fun r_i ->
                       bprintf f " ";
                       loop r_i cur_fls;
                       bprintf f ";") rs;
          bprintf f "|])"
        end
      else begin
        bprintf f "(nalt [|";
        List.iter (fun r_i ->
                     bprintf f " ";
                     loop r_i cur_fls;
                     bprintf f ";") rs;
        bprintf f "|])"
      end
    end in

  let init_fls =
    if !Compileopt.lookahead then
      try
        PMap.find n follow_map
      with Not_found ->
        Printf.eprintf "Warning: Unable to compute %s's FOLLOW set\n" n;
        Cs.empty (), []
    else Cs.empty (), [] in
  loop r0 init_fls

(*
let pr_gil_definitions f start = function
  | [] -> ()
  | (n1,r1)::ds ->
      let b = Buffer.create 11 in
      let first_map = first_gr_gil ds in
      let follow_map = follow_gr_gil ds first_map in
      Printf.bprintf b "(*PARSER-COMBINATOR PROLOGUE*)
module Memo_symb = Allp.Make(struct type semval = sv
                                       module HT = TDHashtable
                        end)
let memo_symb = Memo_symb.memo_symb
let _call_fresh = (fun _ -> sv0)
let _ignore_ret = (fun p x y -> x)
open Allp\n\n";
      Printf.bprintf b "let __parse = \n";
      let name1 = symb_fun_name n1 in
      Printf.bprintf b "let rec %s = \n" name1;
      Printf.bprintf b "  let table = TDHashtable.create 11 in (fun k_s i v ->\n";
      Printf.bprintf b "  memo_symb table \n";
      pr_gil b false false r1 first_map follow_map n1;
      Printf.bprintf b "  k_s i v)\n\n";
      List.iter
        (fun (n,r_gil) ->
           let name = symb_fun_name n in
           Printf.bprintf b "and %s = \n" name;
           Printf.bprintf b "  let table = TDHashtable.create 11 in (fun k_s i v ->\n";
           Printf.bprintf b "  memo_symb table \n";
           pr_gil b false false r_gil first_map follow_map n;
           Printf.bprintf b "  k_s i v)\n\n")
        ds;
      Printf.fprintf f "%s\n" (Buffer.contents b);
      Printf.fprintf f "in Pami.Basic.mk_parse %s sv0\n\n%!" (symb_fun_name start)
*)

let pr_gil_definitions2 f start tokmap = function
  | [] -> ()
  | ds ->
      begin
        let b = Buffer.create 11 in
        let first_map = if !Compileopt.lookahead then first_gr_gil_lex ds tokmap else PMap.empty in
        let follow_map = if !Compileopt.lookahead then follow_gr_gil_lex ds first_map else PMap.empty in
        let tcg = rec_graph ds first_map in
        Printf.bprintf b "(*PARSER-COMBINATOR PROLOGUE*)
module Memo_symb = Allp.Make(struct type semval = sv
                               module HT = TDHashtable
                        end)
let memo_symb = Memo_symb.memo_symb
let _call_fresh = (fun _ -> sv0)
let _ignore_ret = (fun p x y -> x)
open Allp
open YkBuf\n\n";

        Printf.bprintf b "\nlet __parse = \n";

        List.iter begin fun (n,_) ->
          Printf.bprintf b "let %s = ref eps in\n" (symb_ref_name n)
        end ds;

        List.iter begin fun (n, r_gil) ->
          let name = symb_fun_name n in
          Printf.bprintf b "let %s = \n" name;
          if !remove_memo && !Compileopt.lookahead &&
            not(is_rec n tcg)
            (*&& (is_ll1 r_gil first_map follow_map n tokmap) *)
          then ()
          else begin
            Printf.bprintf b "  let table = TDHashtable.create 11 in\n";
            Printf.bprintf b "  memo_symb table %S\n" n;
          end;
          pr_gil_lex b true r_gil first_map follow_map n tokmap;
          Printf.bprintf b " in\n%s := %s;\n\n" (symb_ref_name n) name;
        end ds;

        Printf.fprintf f "%s\n" (Buffer.contents b);
        Printf.fprintf f "Pami.Basic.mk_parse %s sv0\n\n%!" (symb_fun_name start)
      end


module Peg = struct


let pr_gil f liberal use_refs use_arrays r0 =
  let rec loop r =
    match r with
      | Gil.When_special _ -> Util.todo "Gil_gen.Peg.pr_gil.loop.When_special"
      | Gil.DBranch _      -> Util.todo "Gil_gen.Peg.pr_gil.loop.DBranch"
      | Gil.Action(e) ->
          bprintf f "(action (%s))" e
      | Gil.When(e1,e2) ->
          bprintf f "(pred (%s) (%s))" e1 e2
      | Gil.Box(e,_) ->
          bprintf f "(box (%s))"  e
      | Gil.Symb(x,y,z) ->
          if use_refs then
            bprintf f "(rsymb %s" (symb_ref_name x)
          else
            bprintf f "(symb %s" (symb_fun_name x);
          (match y with
             | None -> bprintf f " _call_fresh"
             | Some args -> bprintf f " (%s)" args);
          (match z with
             | None -> bprintf f " _ignore_ret"
             | Some binder -> bprintf f " (%s)" binder);
          bprintf f ")"
      | Gil.CharRange(low,high) ->
          if low = high then
            bprintf f "(term '%s')" (Char.escaped (Char.chr low))
          else
            bprintf f "(char_range %d %d)" low high
      | Gil.Lit(case_sensitive,x) ->
          if x = "" then bprintf f "eps"
          else if String.length x = 1 then bprintf f "(term '%s')" (Char.escaped x.[0])
          else bprintf f "(terms \"%s\")" (String.escaped x)
      | Gil.Star(r2) ->
          if liberal then
            bprintf f "(star "
          else
            bprintf f "(star_strict ";
          loop r2;
          bprintf f ")"
      | Gil.Lookahead(true,r2) ->
          bprintf f "(pos_lookahead ";
          loop r2;
          bprintf f ")"
      | Gil.Lookahead(false,r2) ->
          bprintf f "(neg_lookahead ";
          loop r2;
          bprintf f ")"
      | Gil.Seq _ when use_arrays ->
          bprintf f "(seq [|";
          let rs = Gil.seq2rules r in
          List.iter (fun r_i ->
                       bprintf f " ";
                       loop r_i;
                       bprintf f ";") rs;
          bprintf f "|])"
      | Gil.Alt _ when use_arrays ->
          bprintf f "(alt [|";
          let rs = Gil.alt2rules r in
          List.iter (fun r_i ->
                       bprintf f " ";
                       loop r_i;
                       bprintf f ";") rs;
          bprintf f "|])"

      | Gil.Seq (r2, r3) ->
          bprintf f "(seq ";
          loop r2;
          bprintf f " ";
          loop r3;
          bprintf f ")"
      | Gil.Alt (r2, r3) ->
          bprintf f "(alt ";
          loop r2;
          bprintf f " ";
          loop r3;
          bprintf f ")"
  in loop r0


let pr_definitions f liberal start = function
  | [] -> ()
  | ds ->
      let b = Buffer.create 11 in
      Printf.bprintf b "(*PEG PARSER-COMBINATOR PROLOGUE*)
module Memo_symb = Allp.Peg.Make(struct type semval = sv
                               module HT = TDHashtable
                        end)
let memo_symb = Memo_symb.memo_symb
let _call_fresh = (fun _ -> sv0)
let _ignore_ret = (fun p x y -> x)
open Allp.Peg\n\n";

      Printf.bprintf b "\nlet __parse = \n";

      List.iter
        (fun (n,_) -> Printf.bprintf b "let %s = ref eps in\n" (symb_ref_name n) )
        ds;

      List.iter
        (fun (n,r_gil) ->
           let name = symb_fun_name n in
           Printf.bprintf b "let %s = \n" name;
           Printf.bprintf b "  let table = TDHashtable.create 11 in\n";
           Printf.bprintf b "  memo_symb table \n";
           pr_gil b liberal true false r_gil;
           Printf.bprintf b "\n in %s := %s;\n\n" (symb_ref_name n) name;
        )
        ds;


      Printf.fprintf f "%s\n" (Buffer.contents b);
      Printf.fprintf f "Pami.Peg.mk_parse %s sv0\n\n%!" (symb_fun_name start)
end

module Wadler = struct

  open Util.Operators;;

  let to_char_list s n =
    let rec loop cs k =
      if k < n then loop (s.[k] :: cs) (k + 1)
      else List.rev cs in
    loop [] 0

  let mk_eps = "return []";;
  let mk_tok = Printf.sprintf "tok %C";;

  let mk_seq es ret_e =
    let indent = "   " in
    "\ndo " ^ String.concat ("\n" ^ indent) es ^ "\n"
    ^ indent ^ "return " ^ ret_e

  let mk_lit s =
    match String.length s with
      | 0 -> mk_eps
      | 1 -> mk_tok s.[0]
      | n ->
          let cs = to_char_list s n in
          mk_seq (List.map mk_tok cs) "()"

  let mk_char_range lb ub =
    mk_seq
      ["d <- item";
       Printf.sprintf "guard (%d <= ord d && ord d <= %d)" lb ub]
      "d"


  let pr_gil f r0 =
    let rec loop r =
      match r with
        | Gil.When_special _ -> Util.warn Util.Sys_warn "Gil_gen.Wadler.pr_gil.loop.When_special"
        | Gil.Lit(false, _) -> Util.warn Util.Sys_warn "Gil_gen.Wadler.pr_gil.loop.Gil.Lit(false, _)"
        | Gil.Action _ -> Util.warn Util.Sys_warn "Gil_gen.Wadler.pr_gil.loop.Gil.Action"
        | Gil.When _ -> Util.warn Util.Sys_warn "Gil_gen.Wadler.pr_gil.loop.Gil.When"
        | Gil.Box _ -> Util.warn Util.Sys_warn "Gil_gen.Wadler.pr_gil.loop.Gil.Box"


        | Gil.DBranch (f1, c, _) ->
            Util.warn Util.Sys_warn "Gil_gen.Wadler.pr_gil.loop.DBranch";
            bprintf f "(lextok (%s) %s)" f1 c.Gil.cname
        | Gil.Lookahead (presence, r1) ->
            Util.warn Util.Sys_warn "Gil_gen.Wadler.pr_gil.loop.Gil.Lookahead";
            bprintf f "(lookahead %B " presence;
            loop r1;
            bprintf f ")";


        | Gil.Symb(x, None, None) -> bprintf f "%s" $| symb_fun_name x

        | Gil.Symb _ -> Util.todo "Gil_gen.Wadler.pr_gil.loop.Gil.Symb (with arg or binder)."

        | Gil.CharRange(low,high) ->
            if low = high then
              bprintf f "%s" $ mk_tok $| Char.chr low
            else
              bprintf f "%s" $| mk_char_range low high

        | Gil.Lit(true, x) ->
            bprintf f "%s" $| mk_lit x

        | Gil.Star(r2) ->
            bprintf f "(many ";
            loop r2;
            bprintf f ")";

        | Gil.Seq _  ->
            let rs = Gil.seq2rules r in
            bprintf f "(do {";
            List.iter (fun r_i ->
                         bprintf f " ";
                         loop r_i;
                         bprintf f ";") rs;
            bprintf f " return ()})"

        | Gil.Alt (r2, r3) ->
            loop r2;
            bprintf f " `mplus` ";
            loop r3;
    in loop r0


  let pr_definitions f start = function
    | [] -> ()
    | ds ->
        let b = Buffer.create 11 in

        Printf.bprintf b "do rec {\n";

        List.iter begin fun (n, r_gil) ->
          let name = symb_fun_name n in
          Printf.bprintf b "%s <- loop " name;
          pr_gil b r_gil;
          Printf.bprintf b ";\n"
        end ds;
        Printf.bprintf b "return (final %s)}\n\n" start;

        Printf.fprintf f "%s\n" (Buffer.contents b);
end


