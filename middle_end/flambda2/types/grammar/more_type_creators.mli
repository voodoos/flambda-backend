(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Type creation functions that augment the ones in [Type_grammar] but do not
    require direct access to the representation. *)

[@@@ocaml.warning "+a-30-40-41-42"]

val unknown : Flambda_kind.t -> Type_grammar.t

val unknown_like : Type_grammar.t -> Type_grammar.t

val bottom : Flambda_kind.t -> Type_grammar.t

val bottom_like : Type_grammar.t -> Type_grammar.t

val these_naked_immediates : Targetint_31_63.Set.t -> Type_grammar.t

val these_naked_floats : Type_grammar.head_of_kind_naked_float -> Type_grammar.t

val these_naked_int32s : Type_grammar.head_of_kind_naked_int32 -> Type_grammar.t

val these_naked_int64s : Type_grammar.head_of_kind_naked_int64 -> Type_grammar.t

val these_naked_nativeints :
  Type_grammar.head_of_kind_naked_nativeint -> Type_grammar.t

val any_tagged_immediate : Type_grammar.t

val these_tagged_immediates0 :
  no_alias:bool -> Targetint_31_63.Set.t -> Type_grammar.t

val these_tagged_immediates : Targetint_31_63.Set.t -> Type_grammar.t

val any_tagged_bool : Type_grammar.t

val any_naked_bool : Type_grammar.t

val this_boxed_float :
  Numeric_types.Float_by_bit_pattern.t ->
  Alloc_mode.t Or_unknown.t ->
  Type_grammar.t

val this_boxed_int32 : int32 -> Alloc_mode.t Or_unknown.t -> Type_grammar.t

val this_boxed_int64 : int64 -> Alloc_mode.t Or_unknown.t -> Type_grammar.t

val this_boxed_nativeint :
  Targetint_32_64.t -> Alloc_mode.t Or_unknown.t -> Type_grammar.t

val these_boxed_floats :
  Type_grammar.head_of_kind_naked_float ->
  Alloc_mode.t Or_unknown.t ->
  Type_grammar.t

val these_boxed_int32s :
  Type_grammar.head_of_kind_naked_int32 ->
  Alloc_mode.t Or_unknown.t ->
  Type_grammar.t

val these_boxed_int64s :
  Type_grammar.head_of_kind_naked_int64 ->
  Alloc_mode.t Or_unknown.t ->
  Type_grammar.t

val these_boxed_nativeints :
  Type_grammar.head_of_kind_naked_nativeint ->
  Alloc_mode.t Or_unknown.t ->
  Type_grammar.t

val any_boxed_float : Type_grammar.t

val any_boxed_int32 : Type_grammar.t

val any_boxed_int64 : Type_grammar.t

val any_boxed_nativeint : Type_grammar.t

val any_block : Type_grammar.t

val blocks_with_these_tags : Tag.Set.t -> Type_grammar.t Or_unknown.t

val immutable_block :
  is_unique:bool ->
  Tag.t ->
  field_kind:Flambda_kind.t ->
  Alloc_mode.t Or_unknown.t ->
  fields:Type_grammar.t list ->
  Type_grammar.t

val immutable_block_with_size_at_least :
  tag:Tag.t Or_unknown.t ->
  n:Targetint_31_63.Imm.t ->
  field_kind:Flambda_kind.t ->
  field_n_minus_one:Variable.t ->
  Type_grammar.t

val variant :
  const_ctors:Type_grammar.t ->
  non_const_ctors:Type_grammar.t list Tag.Scannable.Map.t ->
  Alloc_mode.t Or_unknown.t ->
  Type_grammar.t

val open_variant_from_const_ctors_type :
  const_ctors:Type_grammar.t -> Type_grammar.t

val open_variant_from_non_const_ctor_with_size_at_least :
  n:Targetint_31_63.Imm.t -> field_n_minus_one:Variable.t -> Type_grammar.t

val exactly_this_closure :
  Function_slot.t ->
  all_function_slots_in_set:
    Type_grammar.function_type Or_unknown_or_bottom.t Function_slot.Map.t ->
  all_closure_types_in_set:Type_grammar.t Function_slot.Map.t ->
  all_value_slots_in_set:Type_grammar.t Value_slot.Map.t ->
  Alloc_mode.t Or_unknown.t ->
  Type_grammar.t

val closure_with_at_least_these_function_slots :
  this_function_slot:Function_slot.t ->
  Simple.t Function_slot.Map.t ->
  Type_grammar.t

val closure_with_at_least_these_value_slots :
  this_function_slot:Function_slot.t ->
  Variable.t Value_slot.Map.t ->
  Type_grammar.t

val closure_with_at_least_this_value_slot :
  this_function_slot:Function_slot.t ->
  Value_slot.t ->
  value_slot_var:Variable.t ->
  Type_grammar.t

val type_for_const : Reg_width_const.t -> Type_grammar.t

val kind_for_const : Reg_width_const.t -> Flambda_kind.t

val is_alias_of_name : Type_grammar.t -> Name.t -> bool

val check_equation : Name.t -> Type_grammar.t -> unit

val arity_of_list : Type_grammar.t list -> Flambda_arity.t

val unknown_with_subkind : Flambda_kind.With_subkind.t -> Type_grammar.t

(** For each of the kinds in an arity, create an "unknown" type. *)
val unknown_types_from_arity : Flambda_arity.t -> Type_grammar.t list

val unknown_types_from_arity_with_subkinds :
  Flambda_arity.With_subkinds.t -> Type_grammar.t list

(** For each of the kinds in an arity, create an "bottom" type. *)
val bottom_types_from_arity : Flambda_arity.t -> Type_grammar.t list
