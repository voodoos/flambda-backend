(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type descr =
  | Zero_terms
  | Single_term of
      { let_bound : Bound_pattern.t;
        simplified_defining_expr : Simplified_named.t;
        original_defining_expr : Flambda.Named.t
      }
  | Multiple_bindings_to_symbols of
      { bound_vars_to_symbols : (Bound_var.t * Symbol.t) list;
        original_defining_expr : Flambda.Named.t
      }

type t =
  { dacc : Downwards_acc.t;
    descr : descr
  }

let with_dacc ~dacc t = { t with dacc }

let have_simplified_to_zero_terms dacc = { dacc; descr = Zero_terms }

let have_simplified_to_single_term dacc bound_pattern defining_expr
    ~original_defining_expr =
  (* If [original_defining_expr] was simplified to a new term then the benefit
     of doing so is counted in [simplify_named]. *)
  { dacc;
    descr =
      Single_term
        { let_bound = bound_pattern;
          simplified_defining_expr = defining_expr;
          original_defining_expr
        }
  }

let have_lifted_set_of_closures dacc bound_vars_to_symbols
    ~original_defining_expr =
  (* The benefit of lifting the set of closure is added in [simplify_named]. *)
  { dacc;
    descr =
      Multiple_bindings_to_symbols
        { bound_vars_to_symbols; original_defining_expr }
  }

let descr t = t.descr

let dacc t = t.dacc

type binding_to_place =
  { let_bound : Bound_pattern.t;
    simplified_defining_expr : Simplified_named.t;
    original_defining_expr : Flambda.Named.t
  }

let bindings_to_place_in_any_order t =
  match t.descr with
  | Zero_terms -> []
  | Single_term { let_bound; simplified_defining_expr; original_defining_expr }
    ->
    [{ let_bound; simplified_defining_expr; original_defining_expr }]
  | Multiple_bindings_to_symbols
      { bound_vars_to_symbols; original_defining_expr } ->
    ListLabels.fold_left bound_vars_to_symbols ~init:[]
      ~f:(fun bindings (bound_var, symbol) ->
        let let_bound = Bound_pattern.singleton bound_var in
        let simplified_defining_expr =
          Simple.symbol symbol |> Flambda.Named.create_simple
          |> Simplified_named.reachable ~try_reify:false
        in
        { let_bound; simplified_defining_expr; original_defining_expr }
        :: bindings)

let is_invalid t =
  match t.descr with
  | Zero_terms | Multiple_bindings_to_symbols _ -> false
  | Single_term { simplified_defining_expr; _ } ->
    Simplified_named.is_invalid simplified_defining_expr
