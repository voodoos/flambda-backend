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

module CUE = Continuation_uses_env
module DE = Downwards_env
module LCS = Lifted_constant_state
module TE = Flambda2_types.Typing_env

type t =
  { denv : DE.t;
    continuation_uses_env : CUE.t;
    shareable_constants : Symbol.t Static_const.Map.t;
    used_value_slots : Name_occurrences.t;
    lifted_constants : LCS.t;
    data_flow : Data_flow.t;
    demoted_exn_handlers : Continuation.Set.t;
    code_ids_to_remember : Code_id.Set.t;
    slot_offsets : Slot_offsets.t Code_id.Map.t
  }

let [@ocamlformat "disable"] print ppf
      { denv; continuation_uses_env; shareable_constants; used_value_slots;
        lifted_constants; data_flow; demoted_exn_handlers; code_ids_to_remember;
        slot_offsets } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(denv@ %a)@]@ \
      @[<hov 1>(continuation_uses_env@ %a)@]@ \
      @[<hov 1>(shareable_constants@ %a)@]@ \
      @[<hov 1>(used_value_slots@ %a)@]@ \
      @[<hov 1>(lifted_constant_state@ %a)@]@ \
      @[<hov 1>(data_flow@ %a)@]@ \
      @[<hov 1>(demoted_exn_handlers@ %a)@]@ \
      @[<hov 1>(code_ids_to_remember@ %a)@]@ \
      @[<hov 1>(slot_offsets@ %a)@]\
      )@]"
    DE.print denv
    CUE.print continuation_uses_env
    (Static_const.Map.print Symbol.print) shareable_constants
    Name_occurrences.print used_value_slots
    LCS.print lifted_constants
    Data_flow.print data_flow
    Continuation.Set.print demoted_exn_handlers
    Code_id.Set.print code_ids_to_remember
    (Code_id.Map.print Slot_offsets.print) slot_offsets

let create denv continuation_uses_env =
  { denv;
    continuation_uses_env;
    slot_offsets = Code_id.Map.empty;
    shareable_constants = Static_const.Map.empty;
    used_value_slots = Name_occurrences.empty;
    lifted_constants = LCS.empty;
    data_flow = Data_flow.empty;
    demoted_exn_handlers = Continuation.Set.empty;
    code_ids_to_remember = Code_id.Set.empty
  }

let denv t = t.denv

let data_flow t = t.data_flow

let[@inline always] map_data_flow t ~f = { t with data_flow = f t.data_flow }

let[@inline always] map_denv t ~f = { t with denv = f t.denv }

let[@inline always] with_denv t denv = { t with denv }

let with_continuation_uses_env t ~cont_uses_env =
  { t with continuation_uses_env = cont_uses_env }

let record_continuation_use t cont use_kind ~env_at_use ~arg_types =
  let cont_uses_env, id =
    CUE.record_continuation_use t.continuation_uses_env cont use_kind
      ~env_at_use ~arg_types
  in
  with_continuation_uses_env t ~cont_uses_env, id

let delete_continuation_uses t cont =
  let cont_uses_env =
    CUE.delete_continuation_uses t.continuation_uses_env cont
  in
  with_continuation_uses_env t ~cont_uses_env

let num_continuation_uses t cont =
  CUE.num_continuation_uses t.continuation_uses_env cont

let continuation_uses_env t = t.continuation_uses_env

let code_age_relation t = TE.code_age_relation (DE.typing_env (denv t))

let with_code_age_relation t code_age_relation =
  let typing_env =
    TE.with_code_age_relation (DE.typing_env (denv t)) code_age_relation
  in
  with_denv t (DE.with_typing_env (denv t) typing_env)

let typing_env t = DE.typing_env (denv t)

let add_variable t var ty = with_denv t (DE.add_variable (denv t) var ty)

let get_typing_env_no_more_than_one_use t k =
  CUE.get_typing_env_no_more_than_one_use t.continuation_uses_env k

let add_to_lifted_constant_accumulator ?also_add_to_env t constants =
  let also_add_to_env =
    match also_add_to_env with None -> false | Some () -> true
  in
  let lifted_constants = LCS.union t.lifted_constants constants in
  let denv =
    if also_add_to_env then LCS.add_to_denv t.denv constants else t.denv
  in
  { t with lifted_constants; denv }

let get_lifted_constants t = t.lifted_constants

let clear_lifted_constants t = { t with lifted_constants = LCS.empty }

let no_lifted_constants t = LCS.is_empty t.lifted_constants

let get_and_clear_lifted_constants t =
  let constants = t.lifted_constants in
  let t = clear_lifted_constants t in
  t, constants

let set_lifted_constants t consts = { t with lifted_constants = consts }

let find_shareable_constant t static_const =
  Static_const.Map.find_opt static_const t.shareable_constants

let consider_constant_for_sharing t symbol static_const =
  if not (Static_const.can_share static_const)
  then t
  else
    { t with
      shareable_constants =
        Static_const.Map.add static_const symbol t.shareable_constants
    }

let with_shareable_constants t ~shareable_constants =
  { t with shareable_constants }

let shareable_constants t = t.shareable_constants

let add_use_of_value_slot t value_slot =
  { t with
    used_value_slots =
      Name_occurrences.add_value_slot_in_projection t.used_value_slots
        value_slot Name_mode.normal
  }

let used_value_slots t = t.used_value_slots

let all_continuations_used t =
  CUE.all_continuations_used t.continuation_uses_env

let with_used_value_slots t ~used_value_slots = { t with used_value_slots }

let add_code_ids_to_remember t code_ids =
  if DE.at_unit_toplevel t.denv
  then t
  else
    { t with
      code_ids_to_remember = Code_id.Set.union code_ids t.code_ids_to_remember
    }

let code_ids_to_remember t = t.code_ids_to_remember

let with_code_ids_to_remember t ~code_ids_to_remember =
  { t with code_ids_to_remember }

let set_do_not_rebuild_terms_and_disable_inlining t =
  { t with denv = DE.set_do_not_rebuild_terms_and_disable_inlining t.denv }

let are_rebuilding_terms t = DE.are_rebuilding_terms t.denv

let do_not_rebuild_terms t =
  Are_rebuilding_terms.do_not_rebuild_terms (are_rebuilding_terms t)

let demote_exn_handler t cont =
  { t with
    demoted_exn_handlers = Continuation.Set.add cont t.demoted_exn_handlers
  }

let demoted_exn_handlers t = t.demoted_exn_handlers

let slot_offsets t = t.slot_offsets

let with_slot_offsets t ~slot_offsets = { t with slot_offsets }
