(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

(** Classification of expressions based on their effects and coeffects. *)
type effects_and_coeffects_classification = private
  | Pure
      (** Pure expressions can be commuted with *everything*, including
          effectful expressions such as function calls. *)
  | Effect
      (** Effectful expressions only commute with pure expressions.

          For the purpose of To_cmm, generative effects, i.e. allocations, are
          considered to have effects. This is because the mutable state of the
          GC that allocations affect can be observed by coeffects performed by
          function calls (in particular those coming from the Gc module). *)
  | Coeffect_only
      (** Coeffects without any effect. These expression can commute with other
          coeffectful expressions (and pure expressions), but cannot commute
          with an effectful expression. *)

(** Return the classification of an expression with the given effects and
    coeffects. *)
val classify_by_effects_and_coeffects :
  Effects_and_coeffects.t -> effects_and_coeffects_classification

(** Classification of [Let]-expressions, identifying what may be done with the
    defining expression. *)
type let_expr_classification = private
  | Regular  (** Proceed as normal, do not inline the defining expression. *)
  | Drop_defining_expr  (** The defining expression may be deleted. *)
  | Inline  (** The defining expression may be inlined at the use site. *)

val classify_let_expr :
  Variable.t ->
  effects_and_coeffects_of_defining_expr:Effects_and_coeffects.t ->
  num_normal_occurrences_of_bound_vars:Num_occurrences.t Variable.Map.t ->
  let_expr_classification

(** Classification of continuations, indicating what may be done with the
    handler expression. *)
type continuation_handler_classification = private
  | Regular
  | Inline

val classify_continuation_handler :
  Continuation.t ->
  Continuation_handler.t ->
  num_free_occurrences:Num_occurrences.t Or_unknown.t ->
  is_applied_with_traps:bool ->
  continuation_handler_classification
