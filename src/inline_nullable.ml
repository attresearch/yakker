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

(* NOTE: to blog:
  If we did this in Gul, we could avoid wrapping/unwrapping costs for these cases.
*)

module N = Nullable_pred

let inline npreds gr =
  let rhs_inline = function
    | (Gil.Symb (nt, arg_opt, merge_opt)) as r ->
          (match N.Gil.get_symbol_nullability npreds nt with
             | N.No_n -> r
             | N.Yes_n ->
                 let r_null = match arg_opt, merge_opt with
                    | None,   None -> Gil.Lit (false, "")
                    | Some e, None ->
                        (* execute e for effect (if any) and discard result *)
                        Gil.Action ("let f = " ^ e ^ " in (fun p v -> f p v; v)")
                    | None,   Some f ->
                        Gil.Action ("let f = " ^ f ^ " in (fun p v -> f p v sv0)")
                    | Some e, Some f ->
                        Gil.Action ("let e = " ^ e
                                    ^ " and f = " ^ f
                                    ^ " in (fun p v -> f p v (e p v))") in
                 Gil.Alt (r, r_null)
             | N.Maybe_n ->
                 let name = Nullable_pred.mk_npname nt in
                 let p = match arg_opt, merge_opt with
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
                    | Some e, Some f -> Nullable_pred.gil_callc name e f in
                 Gil.Alt (r, Gil.When_special p))
    | r -> r in
  let rule_inline (n, r) = (n, Gil.map rhs_inline r) in
  List.map rule_inline gr
