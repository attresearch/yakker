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

(* LOOKAHEAD: are we handling it right? *)

module N = Nullable_pred

let inline npreds ds =
  let rec rhs_inline r =
    let rhs_inline_leaf = function
      | Gil.Lookahead (b, r) -> Gil.Lookahead (b, rhs_inline r)
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
                 let p = Nullable_pred.callc nt arg_opt merge_opt in
                 Gil.Alt (r, Gil.When_special p)
             | N.Rhs_n r2 -> Gil.Alt (r, r2)
          )
      | r -> r in
    Gil.map rhs_inline_leaf r in
  let rule_inline (n, r) = (n, rhs_inline r) in
  List.map rule_inline ds
