(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Simplification of primitives taking variable numbers of arguments. *)

val simplify_variadic_primitive :
  Downwards_acc.t ->
  Flambda_primitive.t ->
  Flambda_primitive.variadic_primitive ->
  args_with_tys:(Simple.t * Flambda2_types.t) list ->
  Debuginfo.t ->
  result_var:Bound_var.t ->
  Simplified_named.t * Downwards_acc.t
