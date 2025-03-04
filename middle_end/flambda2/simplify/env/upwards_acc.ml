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

module ART = Are_rebuilding_terms
module DA = Downwards_acc
module DE = Downwards_env
module LCS = Lifted_constant_state
module TE = Flambda2_types.Typing_env
module UE = Upwards_env

type t =
  { uenv : UE.t;
    creation_dacc : DA.t;
    code_age_relation : Code_age_relation.t;
    lifted_constants : LCS.t;
    all_code : Exported_code.t;
    name_occurrences : Name_occurrences.t;
    used_value_slots : Name_occurrences.t;
    shareable_constants : Symbol.t Static_const.Map.t;
    cost_metrics : Cost_metrics.t;
    are_rebuilding_terms : ART.t;
    generate_phantom_lets : bool;
    required_names : Name.Set.t;
    reachable_code_ids : Data_flow.Reachable_code_ids.t Or_unknown.t;
    demoted_exn_handlers : Continuation.Set.t;
    slot_offsets : Slot_offsets.t Or_unknown.t
  }

let [@ocamlformat "disable"] print ppf
      { uenv; creation_dacc = _; code_age_relation; lifted_constants;
        name_occurrences; used_value_slots; all_code = _;
        shareable_constants; cost_metrics; are_rebuilding_terms;
        generate_phantom_lets; required_names; reachable_code_ids;
        demoted_exn_handlers; slot_offsets; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(uenv@ %a)@]@ \
      @[<hov 1>(code_age_relation@ %a)@]@ \
      @[<hov 1>(lifted_constants@ %a)@]@ \
      @[<hov 1>(name_occurrences@ %a)@]@ \
      @[<hov 1>(used_value_slots@ %a)@]@ \
      @[<hov 1>(shareable_constants@ %a)@]@ \
      @[<hov 1>(cost_metrics@ %a)@]@ \
      @[<hov 1>(are_rebuilding_terms@ %a)@]@ \
      @[<hov 1>(generate_phantom_lets@ %b)@]@ \
      @[<hov 1>(required_name@ %a)@]@ \
      @[<hov 1>(reachable_code_ids@ %a)@]@ \
      @[<hov 1>(demoted_exn_handlers@ %a)@]@ \
      @[<hov 1>(slot_offsets@ %a@)@]\
      )@]"
    UE.print uenv
    Code_age_relation.print code_age_relation
    LCS.print lifted_constants
    Name_occurrences.print name_occurrences
    Name_occurrences.print used_value_slots
    (Static_const.Map.print Symbol.print) shareable_constants
    Cost_metrics.print cost_metrics
    ART.print are_rebuilding_terms
    generate_phantom_lets
    Name.Set.print required_names
    (Or_unknown.print Data_flow.Reachable_code_ids.print) reachable_code_ids
    Continuation.Set.print demoted_exn_handlers
    (Or_unknown.print Slot_offsets.print) slot_offsets

let create ~required_names ~reachable_code_ids ~compute_slot_offsets uenv dacc =
  let are_rebuilding_terms = DE.are_rebuilding_terms (DA.denv dacc) in
  let generate_phantom_lets = DE.generate_phantom_lets (DA.denv dacc) in
  let slot_offsets : _ Or_unknown.t =
    if compute_slot_offsets then Known (Slot_offsets.create ()) else Unknown
  in
  { uenv;
    creation_dacc = dacc;
    code_age_relation = TE.code_age_relation (DA.typing_env dacc);
    lifted_constants = LCS.empty;
    all_code = Exported_code.empty;
    name_occurrences = Name_occurrences.empty;
    (* [used_value_slots] must be kept separate from the normal free names
       tracking in [name_occurrences], since it is always accumulated, and never
       saved and restored (like free name information is when dealing with a
       [Let_cont]). *)
    (* CR gbury: since [used_value_slots] (and [mshinwell:] various other
       things), are actually never modified in the uacc, and initialised using
       the dacc, why not access them through the dacc ? that would reduce the
       number of words allocated for each uacc (at the cost of an extra
       lookup) *)
    used_value_slots = DA.used_value_slots dacc;
    shareable_constants = DA.shareable_constants dacc;
    cost_metrics = Cost_metrics.zero;
    are_rebuilding_terms;
    generate_phantom_lets;
    required_names;
    reachable_code_ids;
    demoted_exn_handlers = DA.demoted_exn_handlers dacc;
    slot_offsets
  }

let creation_dacc t = t.creation_dacc

let uenv t = t.uenv

let code_age_relation t = t.code_age_relation

let lifted_constants t = t.lifted_constants

let required_names t = t.required_names

let reachable_code_ids t = t.reachable_code_ids

let cost_metrics t = t.cost_metrics

let are_rebuilding_terms t = t.are_rebuilding_terms

let add_outermost_lifted_constant t const =
  { t with lifted_constants = LCS.add_outermost t.lifted_constants const }

let with_lifted_constants t lifted_constants = { t with lifted_constants }

let no_lifted_constants t = LCS.is_empty t.lifted_constants

let map_uenv t ~f = { t with uenv = f t.uenv }

let with_uenv t uenv = { t with uenv }

let remember_code_for_cmx t code =
  if ART.do_not_rebuild_terms t.are_rebuilding_terms
  then t
  else
    let keep_code code_id =
      Code_id.Set.mem code_id (DA.code_ids_to_remember t.creation_dacc)
    in
    let all_code = Exported_code.add_code ~keep_code code t.all_code in
    { t with all_code }

let all_code t = t.all_code

let name_occurrences t = t.name_occurrences

let with_name_occurrences t ~name_occurrences =
  if name_occurrences == t.name_occurrences
  then t
  else { t with name_occurrences }

let clear_name_occurrences t =
  with_name_occurrences t ~name_occurrences:Name_occurrences.empty

let add_free_names t free_names =
  let name_occurrences = Name_occurrences.union t.name_occurrences free_names in
  { t with name_occurrences }

let used_value_slots t = t.used_value_slots

let shareable_constants t = t.shareable_constants

let remove_all_occurrences_of_free_names t to_remove =
  let name_occurrences = Name_occurrences.diff t.name_occurrences to_remove in
  { t with name_occurrences }

let clear_cost_metrics t = { t with cost_metrics = Cost_metrics.zero }

let with_cost_metrics cost_metrics t = { t with cost_metrics }

let notify_added ~code_size t =
  { t with cost_metrics = Cost_metrics.notify_added ~code_size t.cost_metrics }

let notify_removed ~operation t =
  { t with
    cost_metrics = Cost_metrics.notify_removed ~operation t.cost_metrics
  }

let add_cost_metrics cost_metrics t =
  { t with cost_metrics = Cost_metrics.( + ) t.cost_metrics cost_metrics }

let add_cost_metrics_and_with_name_occurrences t cost_metrics name_occurrences =
  { t with
    cost_metrics = Cost_metrics.( + ) t.cost_metrics cost_metrics;
    name_occurrences
  }

let generate_phantom_lets t = t.generate_phantom_lets

let is_demoted_exn_handler t cont =
  Continuation.Set.mem cont t.demoted_exn_handlers

let slot_offsets t = t.slot_offsets

let with_slot_offsets t slot_offsets = { t with slot_offsets }
