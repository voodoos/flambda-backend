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

val make_inlined_body :
  callee:Simple.t ->
  params:'param list ->
  args:Simple.List.t ->
  my_closure:'param ->
  my_depth:Variable.t ->
  rec_info:Rec_info_expr.t ->
  body:'expr_with_acc ->
  exn_continuation:Continuation.t ->
  return_continuation:Continuation.t ->
  apply_exn_continuation:Continuation.t ->
  apply_return_continuation:Flambda.Apply.Result_continuation.t ->
  bind_params:
    (params:'param list ->
    args:Simple.List.t ->
    body:'expr_with_acc ->
    'expr_with_acc) ->
  bind_depth:
    (my_depth:Variable.t ->
    rec_info:Rec_info_expr.t ->
    body:'expr_with_acc ->
    'expr_with_acc) ->
  apply_renaming:('expr_with_acc -> Renaming.t -> 'expr_with_acc) ->
  'expr_with_acc

val wrap_inlined_body_for_exn_support :
  'acc ->
  extra_args:(Simple.t * Flambda_kind.With_subkind.t) list ->
  apply_exn_continuation:Exn_continuation.t ->
  apply_return_continuation:Flambda.Apply.Result_continuation.t ->
  result_arity:Flambda_arity.With_subkinds.t ->
  make_inlined_body:
    ('acc ->
    apply_exn_continuation:Continuation.t ->
    apply_return_continuation:Flambda.Apply.Result_continuation.t ->
    'expr_with_acc) ->
  apply_cont_create:
    ('acc ->
    trap_action:Trap_action.t ->
    Continuation.t ->
    args:Simple.List.t ->
    dbg:Debuginfo.t ->
    'expr_with_acc) ->
  let_cont_create:
    ('acc ->
    Continuation.t ->
    handler_params:Bound_parameters.t ->
    handler:('acc -> 'expr_with_acc) ->
    body:('acc -> 'expr_with_acc) ->
    is_exn_handler:bool ->
    'expr_with_acc) ->
  'expr_with_acc
