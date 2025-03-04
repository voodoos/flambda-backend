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

[@@@ocaml.warning "+a-4-30-40-41-42-66"]

open! Int_replace_polymorphic_compare
open! Flambda
module BP = Bound_parameter
module IR = Closure_conversion_aux.IR
module Acc = Closure_conversion_aux.Acc
module Env = Closure_conversion_aux.Env
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc
module Apply_cont_with_acc = Closure_conversion_aux.Apply_cont_with_acc
module Let_cont_with_acc = Closure_conversion_aux.Let_cont_with_acc
module Let_with_acc = Closure_conversion_aux.Let_with_acc
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module K = Flambda_kind
module P = Flambda_primitive
module VB = Bound_var

type close_functions_result =
  | Lifted of (Symbol.t * Env.value_approximation) Function_slot.Lmap.t
  | Dynamic of Set_of_closures.t * Env.value_approximation Function_slot.Map.t

(* Do not use [Simple.symbol], use this function instead, to ensure that we
   correctly compute the free names of [Code]. *)
let use_of_symbol_as_simple acc symbol = acc, Simple.symbol symbol

let symbol_for_ident acc env id =
  let symbol = Env.symbol_for_global env id in
  use_of_symbol_as_simple acc symbol

let declare_symbol_for_function_slot env ident function_slot : Env.t * Symbol.t
    =
  let symbol =
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.create (Function_slot.to_string function_slot))
  in
  let env = Env.add_simple_to_substitute env ident (Simple.symbol symbol) in
  env, symbol

let register_const0 acc constant name =
  match Static_const.Map.find constant (Acc.shareable_constants acc) with
  | exception Not_found ->
    (* Create a variable to ensure uniqueness of the symbol. *)
    let var = Variable.create name in
    let symbol =
      Symbol.create
        (Compilation_unit.get_current_exn ())
        (Linkage_name.create (Variable.unique_name (Variable.rename var)))
    in
    let acc = Acc.add_declared_symbol ~symbol ~constant acc in
    let acc =
      if Static_const.can_share constant
      then Acc.add_shareable_constant ~symbol ~constant acc
      else acc
    in
    acc, symbol
  | symbol -> acc, symbol

let register_const acc constant name : Acc.t * Field_of_static_block.t * string
    =
  let acc, symbol = register_const0 acc constant name in
  acc, Symbol symbol, name

let register_const_string acc str =
  register_const0 acc (Static_const.Immutable_string str) "string"

let rec declare_const acc (const : Lambda.structured_constant) :
    Acc.t * Field_of_static_block.t * string =
  match const with
  | Const_base (Const_int c) ->
    ( acc,
      Tagged_immediate (Targetint_31_63.int (Targetint_31_63.Imm.of_int c)),
      "int" )
  | Const_base (Const_char c) ->
    acc, Tagged_immediate (Targetint_31_63.char c), "char"
  | Const_base (Const_string (s, _, _)) ->
    let const, name =
      if Flambda_features.safe_string ()
      then Static_const.Immutable_string s, "immstring"
      else Static_const.Mutable_string { initial_value = s }, "string"
    in
    register_const acc const name
  | Const_base (Const_float c) ->
    let c = Numeric_types.Float_by_bit_pattern.create (float_of_string c) in
    register_const acc (Boxed_float (Const c)) "float"
  | Const_base (Const_int32 c) ->
    register_const acc (Boxed_int32 (Const c)) "int32"
  | Const_base (Const_int64 c) ->
    register_const acc (Boxed_int64 (Const c)) "int64"
  | Const_base (Const_nativeint c) ->
    (* CR pchambart: this should be pushed further to lambda *)
    let c = Targetint_32_64.of_int64 (Int64.of_nativeint c) in
    register_const acc (Boxed_nativeint (Const c)) "nativeint"
  | Const_immstring c -> register_const acc (Immutable_string c) "immstring"
  | Const_float_block c ->
    register_const acc
      (Immutable_float_block
         (List.map
            (fun s ->
              let f =
                Numeric_types.Float_by_bit_pattern.create (float_of_string s)
              in
              Or_variable.Const f)
            c))
      "float_block"
  | Const_float_array c ->
    register_const acc
      (Immutable_float_array
         (List.map
            (fun s ->
              let f =
                Numeric_types.Float_by_bit_pattern.create (float_of_string s)
              in
              Or_variable.Const f)
            c))
      "float_array"
  | Const_block (tag, consts) ->
    let acc, field_of_blocks =
      List.fold_left_map
        (fun acc c ->
          let acc, f, _ = declare_const acc c in
          acc, f)
        acc consts
    in
    let const : Static_const.t =
      Block (Tag.Scannable.create_exn tag, Immutable, field_of_blocks)
    in
    register_const acc const "const_block"

let close_const0 acc (const : Lambda.structured_constant) =
  let acc, const, name = declare_const acc const in
  match const with
  | Tagged_immediate c ->
    acc, Simple.const (Reg_width_const.tagged_immediate c), name
  | Symbol s ->
    let acc, simple = use_of_symbol_as_simple acc s in
    acc, simple, name
  | Dynamically_computed _ ->
    Misc.fatal_errorf "Declaring a computed constant %s" name

let close_const acc const =
  let acc, simple, name = close_const0 acc const in
  let named = Named.create_simple simple in
  acc, named, name

let find_simple_from_id env id =
  match Env.find_simple_to_substitute_exn env id with
  | simple -> simple
  | exception Not_found -> (
    match Env.find_var_exn env id with
    | exception Not_found ->
      Misc.fatal_errorf
        "find_simple_from_id: Cannot find [Ident] %a in environment" Ident.print
        id
    | var -> Simple.var var)

(* CR mshinwell: Avoid the double lookup *)
let find_simple acc env (simple : IR.simple) =
  match simple with
  | Const const ->
    let acc, simple, _ = close_const0 acc const in
    acc, simple
  | Var id -> acc, find_simple_from_id env id

let find_simples acc env ids =
  List.fold_left_map (fun acc id -> find_simple acc env id) acc ids

module Inlining = struct
  include Closure_conversion_aux.Inlining

  (* CR keryan: we need to emit warnings *)
  let inlinable env apply callee_approx =
    let tracker = Env.inlining_history_tracker env in
    let are_rebuilding_terms = Are_rebuilding_terms.of_bool true in
    let compilation_unit =
      Env.inlining_history_tracker env
      |> Inlining_history.Tracker.absolute
      |> Inlining_history.Absolute.compilation_unit
    in
    match (callee_approx : Env.value_approximation option) with
    | None | Some Value_unknown ->
      Inlining_report.record_decision_at_call_site_for_unknown_function ~tracker
        ~apply ~pass:After_closure_conversion ();
      Not_inlinable
    | Some (Value_symbol _) | Some (Block_approximation _) -> assert false
    | Some (Closure_approximation (_code_id, _, Metadata_only _)) ->
      Inlining_report.record_decision_at_call_site_for_known_function ~tracker
        ~apply ~pass:After_closure_conversion ~unrolling_depth:None
        ~callee:(Inlining_history.Absolute.empty compilation_unit)
        ~are_rebuilding_terms Definition_says_not_to_inline;
      Not_inlinable
    | Some (Closure_approximation (_code_id, _, Code_present code)) ->
      let fun_params_length =
        Code.params_arity code |> Flambda_arity.With_subkinds.to_arity
        |> Flambda_arity.length
      in
      if fun_params_length > List.length (Apply_expr.args apply)
      then begin
        Inlining_report.record_decision_at_call_site_for_known_function ~tracker
          ~apply ~pass:After_closure_conversion ~unrolling_depth:None
          ~callee:(Code.absolute_history code)
          ~are_rebuilding_terms Definition_says_not_to_inline;
        Not_inlinable
      end
      else
        let inlined_call = Apply_expr.inlined apply in
        let decision, res =
          match inlined_call with
          | Never_inlined ->
            ( Call_site_inlining_decision_type.Never_inlined_attribute,
              Not_inlinable )
          | Always_inlined | Hint_inlined ->
            Call_site_inlining_decision_type.Attribute_always, Inlinable code
          | Default_inlined ->
            ( Call_site_inlining_decision_type.Definition_says_inline,
              Inlinable code )
          | Unroll _ -> assert false
        in
        Inlining_report.record_decision_at_call_site_for_known_function ~tracker
          ~apply ~pass:After_closure_conversion ~unrolling_depth:None
          ~callee:(Code.absolute_history code)
          ~are_rebuilding_terms decision;
        res

  let make_inlined_body acc ~callee ~params ~args ~my_closure ~my_depth ~body
      ~free_names_of_body ~exn_continuation ~return_continuation
      ~apply_exn_continuation ~apply_return_continuation ~apply_depth =
    let rec_info =
      match apply_depth with
      | None -> Rec_info_expr.initial
      | Some depth -> Rec_info_expr.var depth
    in
    let bind_params ~params ~args ~body:(acc, body) =
      let acc = Acc.with_free_names free_names_of_body acc in
      List.fold_left2
        (fun (acc, body) param arg ->
          Let_with_acc.create acc
            (Bound_pattern.singleton (VB.create param Name_mode.normal))
            (Named.create_simple arg) ~body)
        (acc, body) params args
    in
    let bind_depth ~my_depth ~rec_info ~body:(acc, body) =
      Let_with_acc.create acc
        (Bound_pattern.singleton (VB.create my_depth Name_mode.normal))
        (Named.create_rec_info rec_info)
        ~body
    in
    let apply_renaming (acc, body) perm =
      let acc =
        Acc.with_free_names
          (Name_occurrences.apply_renaming (Acc.free_names acc) perm)
          acc
      in
      acc, Expr.apply_renaming body perm
    in
    Inlining_helpers.make_inlined_body ~callee ~params ~args ~my_closure
      ~my_depth ~rec_info ~body:(acc, body) ~exn_continuation
      ~return_continuation ~apply_exn_continuation ~apply_return_continuation
      ~bind_params ~bind_depth ~apply_renaming

  let wrap_inlined_body_for_exn_support acc ~extra_args ~apply_exn_continuation
      ~apply_return_continuation ~result_arity ~make_inlined_body =
    let apply_cont_create acc ~trap_action cont ~args ~dbg =
      let acc, apply_cont =
        Apply_cont_with_acc.create acc ~trap_action cont ~args ~dbg
      in
      Expr_with_acc.create_apply_cont acc apply_cont
    in
    let let_cont_create acc cont ~handler_params ~handler ~body ~is_exn_handler
        =
      Let_cont_with_acc.build_non_recursive acc cont ~handler_params ~handler
        ~body ~is_exn_handler
    in
    Inlining_helpers.wrap_inlined_body_for_exn_support acc ~extra_args
      ~apply_exn_continuation ~apply_return_continuation ~result_arity
      ~make_inlined_body ~apply_cont_create ~let_cont_create

  let inline acc ~apply ~apply_depth ~func_desc:code =
    let callee = Apply.callee apply in
    let args = Apply.args apply in
    let apply_return_continuation = Apply.continuation apply in
    let apply_exn_continuation = Apply.exn_continuation apply in
    let params_and_body = Code.params_and_body code in
    let cost_metrics = Code.cost_metrics code in
    Function_params_and_body.pattern_match params_and_body
      ~f:(fun
           ~return_continuation
           ~exn_continuation
           params
           ~body
           ~my_closure
           ~is_my_closure_used:_
           ~my_depth
           ~free_names_of_body
         ->
        let free_names_of_body =
          match free_names_of_body with
          | Unknown ->
            Misc.fatal_error
              "Params_and_body needs free_names_of_body in [Closure_conversion]"
          | Known free_names -> free_names
        in
        let make_inlined_body =
          make_inlined_body ~callee
            ~params:(Bound_parameters.vars params)
            ~args ~my_closure ~my_depth ~body ~free_names_of_body
            ~exn_continuation ~return_continuation ~apply_depth
        in
        let acc = Acc.with_free_names Name_occurrences.empty acc in
        let acc = Acc.increment_metrics cost_metrics acc in
        match Exn_continuation.extra_args apply_exn_continuation with
        | [] ->
          make_inlined_body acc
            ~apply_exn_continuation:
              (Exn_continuation.exn_handler apply_exn_continuation)
            ~apply_return_continuation
        | extra_args ->
          wrap_inlined_body_for_exn_support acc ~extra_args
            ~apply_exn_continuation ~apply_return_continuation
            ~result_arity:(Code.result_arity code) ~make_inlined_body)
end

let close_c_call acc env ~loc ~let_bound_var
    ({ prim_name;
       prim_arity;
       prim_alloc;
       prim_c_builtin;
       prim_effects = _;
       prim_coeffects = _;
       prim_native_name;
       prim_native_repr_args;
       prim_native_repr_res
     } :
      Primitive.description) ~(args : Simple.t list) exn_continuation dbg
    (k : Acc.t -> Named.t option -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  (* We always replace the original let-binding with an Flambda expression, so
     we call [k] with [None], to get just the closure-converted body of that
     binding. *)
  let cost_metrics_of_body, free_names_of_body, acc, body =
    Acc.measure_cost_metrics acc ~f:(fun acc -> k acc None)
  in
  let box_return_value =
    match prim_native_repr_res with
    | _, Same_as_ocaml_repr -> None
    | _, Unboxed_float -> Some (P.Box_number (Naked_float, Heap))
    | _, Unboxed_integer Pnativeint ->
      Some (P.Box_number (Naked_nativeint, Heap))
    | _, Unboxed_integer Pint32 -> Some (P.Box_number (Naked_int32, Heap))
    | _, Unboxed_integer Pint64 -> Some (P.Box_number (Naked_int64, Heap))
    | _, Untagged_int -> Some P.Tag_immediate
  in
  let return_continuation, needs_wrapper =
    match Expr.descr body with
    | Apply_cont apply_cont
      when Simple.List.equal
             (Apply_cont_expr.args apply_cont)
             [Simple.var let_bound_var]
           && Option.is_none (Apply_cont_expr.trap_action apply_cont)
           && Option.is_none box_return_value ->
      Apply_cont_expr.continuation apply_cont, false
    | _ -> Continuation.create (), true
  in
  let kind_of_primitive_native_repr
      ((_, repr) : Primitive.mode * Primitive.native_repr) =
    match repr with
    | Same_as_ocaml_repr -> K.value
    | Unboxed_float -> K.naked_float
    | Unboxed_integer Pnativeint -> K.naked_nativeint
    | Unboxed_integer Pint32 -> K.naked_int32
    | Unboxed_integer Pint64 -> K.naked_int64
    | Untagged_int -> K.naked_immediate
  in
  let param_arity =
    List.map kind_of_primitive_native_repr prim_native_repr_args
    |> Flambda_arity.create
  in
  let return_kind = kind_of_primitive_native_repr prim_native_repr_res in
  let return_arity = Flambda_arity.create [return_kind] in
  let call_kind =
    Call_kind.c_call ~alloc:prim_alloc ~param_arity ~return_arity
      ~is_c_builtin:prim_c_builtin
  in
  let call_symbol =
    let prim_name =
      if String.equal prim_native_name "" then prim_name else prim_native_name
    in
    (* CR mshinwell: fix "extern" mess (see To_cmm) *)
    Symbol.create
      (Compilation_unit.external_symbols ())
      (Linkage_name.create prim_name)
  in
  let call args acc =
    (* Some C primitives have implementations within Flambda itself. *)
    match prim_native_name with
    | "caml_int64_float_of_bits_unboxed"
    (* There is only one case where this operation is not the identity: on
       32-bit pre-EABI ARM platforms. It is very unlikely anyone would still be
       using one of those, but just in case, we only optimise this primitive on
       64-bit systems. (There is no easy way here of detecting just the specific
       ARM case in question.) *)
      when match Targetint_32_64.num_bits with
           | Thirty_two -> false
           | Sixty_four -> true -> (
      if prim_arity <> 1
      then Misc.fatal_errorf "Expected arity one for %s" prim_native_name
      else
        match prim_native_repr_args, prim_native_repr_res with
        | [(_, Unboxed_integer Pint64)], (_, Unboxed_float) -> begin
          match args with
          | [arg] ->
            let result = Variable.create "reinterpreted_int64" in
            let result' = Bound_var.create result Name_mode.normal in
            let bindable = Bound_pattern.singleton result' in
            let prim = P.Unary (Reinterpret_int64_as_float, arg) in
            let acc, return_result =
              Apply_cont_with_acc.create acc return_continuation
                ~args:[Simple.var result] ~dbg
            in
            let acc, return_result_expr =
              Expr_with_acc.create_apply_cont acc return_result
            in
            Let_with_acc.create acc bindable
              (Named.create_prim prim dbg)
              ~body:return_result_expr
          | [] | _ :: _ ->
            Misc.fatal_errorf "Expected one arg for %s" prim_native_name
        end
        | _, _ ->
          Misc.fatal_errorf "Wrong argument and/or result kind(s) for %s"
            prim_native_name)
    | _ ->
      let acc, callee = use_of_symbol_as_simple acc call_symbol in
      let apply =
        Apply.create ~callee ~continuation:(Return return_continuation)
          exn_continuation ~args ~call_kind dbg ~inlined:Default_inlined
          ~inlining_state:(Inlining_state.default ~round:0)
          ~probe_name:None
          ~relative_history:(Env.relative_history_from_scoped ~loc env)
      in
      Expr_with_acc.create_apply acc apply
  in
  let call : Acc.t -> Acc.t * Expr_with_acc.t =
    List.fold_left2
      (fun (call : Simple.t list -> Acc.t -> Acc.t * Expr_with_acc.t) arg
           (arg_repr : Primitive.mode * Primitive.native_repr) ->
        let unbox_arg : P.unary_primitive option =
          match arg_repr with
          | _, Same_as_ocaml_repr -> None
          | _, Unboxed_float -> Some (P.Unbox_number Naked_float)
          | _, Unboxed_integer Pnativeint ->
            Some (P.Unbox_number Naked_nativeint)
          | _, Unboxed_integer Pint32 -> Some (P.Unbox_number Naked_int32)
          | _, Unboxed_integer Pint64 -> Some (P.Unbox_number Naked_int64)
          | _, Untagged_int -> Some P.Untag_immediate
        in
        match unbox_arg with
        | None -> fun args acc -> call (arg :: args) acc
        | Some named ->
          fun args acc ->
            let unboxed_arg = Variable.create "unboxed" in
            let unboxed_arg' = VB.create unboxed_arg Name_mode.normal in
            let acc, body = call (Simple.var unboxed_arg :: args) acc in
            let named = Named.create_prim (Unary (named, arg)) dbg in
            Let_with_acc.create acc
              (Bound_pattern.singleton unboxed_arg')
              named ~body)
      call args prim_native_repr_args []
  in
  let wrap_c_call acc ~handler_param ~code_after_call c_call =
    let return_kind = Flambda_kind.With_subkind.create return_kind Anything in
    let params =
      [BP.create handler_param return_kind] |> Bound_parameters.create
    in
    Let_cont_with_acc.build_non_recursive acc return_continuation
      ~handler_params:params ~handler:code_after_call ~body:c_call
      ~is_exn_handler:false
  in
  let keep_body acc =
    ( Acc.with_cost_metrics
        (Cost_metrics.( + ) (Acc.cost_metrics acc) cost_metrics_of_body)
        (Acc.with_free_names free_names_of_body acc),
      body )
  in
  let box_unboxed_returns ~let_bound_var ~box_return_value =
    let let_bound_var' = VB.create let_bound_var Name_mode.normal in
    let handler_param = Variable.rename let_bound_var in
    let body acc =
      let acc, body = keep_body acc in
      let named =
        Named.create_prim
          (Unary (box_return_value, Simple.var handler_param))
          dbg
      in
      Let_with_acc.create acc
        (Bound_pattern.singleton let_bound_var')
        named ~body
    in
    body, handler_param
  in
  match box_return_value with
  | None ->
    if needs_wrapper
    then
      wrap_c_call acc ~handler_param:let_bound_var ~code_after_call:keep_body
        call
    else
      (* Here the body is discarded. It might be useful to explicitly remove
         anything that has been added to the acc while converting the body.
         However, as we are hitting this code only when body is a goto
         continuation where the only parameter is [let_bound_var] this operation
         would be a noop and we can skip it. *)
      call acc
  | Some box_return_value ->
    let code_after_call, handler_param =
      box_unboxed_returns ~let_bound_var ~box_return_value
    in
    wrap_c_call acc ~handler_param ~code_after_call call

let close_exn_continuation acc env (exn_continuation : IR.exn_continuation) =
  let acc, extra_args =
    List.fold_left_map
      (fun acc (simple, kind) ->
        let acc, simple = find_simple acc env simple in
        acc, (simple, K.With_subkind.from_lambda kind))
      acc exn_continuation.extra_args
  in
  ( acc,
    Exn_continuation.create ~exn_handler:exn_continuation.exn_handler
      ~extra_args )

let close_primitive acc env ~let_bound_var named (prim : Lambda.primitive) ~args
    loc (exn_continuation : IR.exn_continuation option)
    (k : Acc.t -> Named.t option -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  let acc, exn_continuation =
    match exn_continuation with
    | None -> acc, None
    | Some exn_continuation ->
      let acc, cont = close_exn_continuation acc env exn_continuation in
      acc, Some cont
  in
  let acc, args = find_simples acc env args in
  let dbg = Debuginfo.from_location loc in
  match prim, args with
  | Pccall prim, args ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Pccall is missing exception continuation: %a"
          IR.print_named named
      | Some exn_continuation -> exn_continuation
    in
    close_c_call acc env ~loc ~let_bound_var prim ~args exn_continuation dbg k
  | Pgetglobal id, [] ->
    let is_predef_exn = Ident.is_predef id in
    if not (is_predef_exn || not (Ident.same id (Env.current_unit_id env)))
    then
      Misc.fatal_errorf "Non-predef Pgetglobal %a in the same unit" Ident.print
        id;
    let acc, simple = symbol_for_ident acc env id in
    let named = Named.create_simple simple in
    k acc (Some named)
  | Praise raise_kind, [_] ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Praise is missing exception continuation: %a"
          IR.print_named named
      | Some exn_continuation -> exn_continuation
    in
    let exn_handler = Exn_continuation.exn_handler exn_continuation in
    let args =
      (* CR mshinwell: Share with [Lambda_to_flambda_primitives_helpers] *)
      let extra_args =
        List.map
          (fun (simple, _kind) -> simple)
          (Exn_continuation.extra_args exn_continuation)
      in
      args @ extra_args
    in
    let raise_kind = Some (Trap_action.Raise_kind.from_lambda raise_kind) in
    let trap_action = Trap_action.Pop { exn_handler; raise_kind } in
    let acc, apply_cont =
      Apply_cont_with_acc.create acc ~trap_action exn_handler ~args ~dbg
    in
    (* Since raising of an exception doesn't terminate, we don't call [k]. *)
    Expr_with_acc.create_apply_cont acc apply_cont
  | (Pmakeblock _ | Pmakefloatblock _ | Pmakearray _), [] ->
    (* Special case for liftable empty block or array *)
    let acc, sym =
      match prim with
      | Pmakeblock (tag, _, _, _mode) ->
        if tag <> 0
        then
          (* There should not be any way to reach this from Ocaml code. *)
          Misc.fatal_error
            "Non-zero tag on empty block allocation in [Closure_conversion]"
        else
          register_const0 acc
            (Static_const.Block (Tag.Scannable.zero, Immutable, []))
            "empty_block"
      | Pmakefloatblock _ ->
        Misc.fatal_error "Unexpected empty float block in [Closure_conversion]"
      | Pmakearray (_, _, _mode) ->
        register_const0 acc Static_const.Empty_array "empty_array"
      | Pidentity | Pbytes_to_string | Pbytes_of_string | Pignore | Prevapply _
      | Pdirapply _ | Pgetglobal _ | Psetglobal _ | Pfield _ | Pfield_computed _
      | Psetfield _ | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _
      | Pduprecord _ | Pccall _ | Praise _ | Psequand | Psequor | Pnot | Pnegint
      | Paddint | Psubint | Pmulint | Pdivint _ | Pmodint _ | Pandint | Porint
      | Pxorint | Plslint | Plsrint | Pasrint | Pintcomp _ | Pcompare_ints
      | Pcompare_floats | Pcompare_bints _ | Poffsetint _ | Poffsetref _
      | Pintoffloat | Pfloatofint _ | Pnegfloat _ | Pabsfloat _ | Paddfloat _
      | Psubfloat _ | Pmulfloat _ | Pdivfloat _ | Pfloatcomp _ | Pstringlength
      | Pstringrefu | Pstringrefs | Pbyteslength | Pbytesrefu | Pbytessetu
      | Pbytesrefs | Pbytessets | Pduparray _ | Parraylength _ | Parrayrefu _
      | Parraysetu _ | Parrayrefs _ | Parraysets _ | Pisint | Pisout
      | Pbintofint _ | Pintofbint _ | Pcvtbint _ | Pnegbint _ | Paddbint _
      | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _ | Pandbint _
      | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _
      | Pbintcomp _ | Pbigarrayref _ | Pbigarrayset _ | Pbigarraydim _
      | Pstring_load_16 _ | Pstring_load_32 _ | Pstring_load_64 _
      | Pbytes_load_16 _ | Pbytes_load_32 _ | Pbytes_load_64 _ | Pbytes_set_16 _
      | Pbytes_set_32 _ | Pbytes_set_64 _ | Pbigstring_load_16 _
      | Pbigstring_load_32 _ | Pbigstring_load_64 _ | Pbigstring_set_16 _
      | Pbigstring_set_32 _ | Pbigstring_set_64 _ | Pctconst _ | Pbswap16
      | Pbbswap _ | Pint_as_pointer | Popaque | Pprobe_is_enabled _ ->
        (* Inconsistent with outer match *)
        assert false
    in
    k acc (Some (Named.create_simple (Simple.symbol sym)))
  | prim, args ->
    Lambda_to_flambda_primitives.convert_and_bind acc exn_continuation
      ~big_endian:(Env.big_endian env)
      ~register_const_string:(fun acc -> register_const_string acc)
      prim ~args dbg k

let close_trap_action_opt trap_action =
  Option.map
    (fun (trap_action : IR.trap_action) : Trap_action.t ->
      match trap_action with
      | Push { exn_handler } -> Push { exn_handler }
      | Pop { exn_handler } -> Pop { exn_handler; raise_kind = None })
    trap_action

let close_named acc env ~let_bound_var (named : IR.named)
    (k : Acc.t -> Named.t option -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  match named with
  | Simple (Var id) ->
    let acc, simple =
      if not (Ident.is_predef id)
      then find_simple acc env (Var id)
      else symbol_for_ident acc env id
    in
    let named = Named.create_simple simple in
    k acc (Some named)
  | Simple (Const cst) ->
    let acc, named, _name = close_const acc cst in
    k acc (Some named)
  | Get_tag var ->
    let named = find_simple_from_id env var in
    let prim : Lambda_to_flambda_primitives_helpers.expr_primitive =
      Unary (Tag_immediate, Prim (Unary (Get_tag, Simple named)))
    in
    Lambda_to_flambda_primitives_helpers.bind_rec acc None
      ~register_const_string:(fun acc -> register_const_string acc)
      prim Debuginfo.none
      (fun acc named -> k acc (Some named))
  | Begin_region ->
    let prim : Lambda_to_flambda_primitives_helpers.expr_primitive =
      Nullary Begin_region
    in
    Lambda_to_flambda_primitives_helpers.bind_rec acc None
      ~register_const_string:(fun acc -> register_const_string acc)
      prim Debuginfo.none
      (fun acc named -> k acc (Some named))
  | End_region id ->
    let named = find_simple_from_id env id in
    let prim : Lambda_to_flambda_primitives_helpers.expr_primitive =
      Unary (End_region, Simple named)
    in
    Lambda_to_flambda_primitives_helpers.bind_rec acc None
      ~register_const_string:(fun acc -> register_const_string acc)
      prim Debuginfo.none
      (fun acc named -> k acc (Some named))
  | Prim { prim; args; loc; exn_continuation } ->
    close_primitive acc env ~let_bound_var named prim ~args loc exn_continuation
      k

let close_let acc env id user_visible defining_expr
    ~(body : Acc.t -> Env.t -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  let body_env, var = Env.add_var_like env id user_visible in
  let cont acc (defining_expr : Named.t option) =
    match defining_expr with
    | Some (Simple simple) ->
      let body_env = Env.add_simple_to_substitute env id simple in
      body acc body_env
    | None -> body acc body_env
    | Some defining_expr -> (
      (* CR pchambart: Not tail ! *)
      let body_env =
        match defining_expr with
        | Prim (Variadic (Make_block (_, Immutable, alloc_mode), fields), _) ->
          let approxs =
            List.map
              (fun field ->
                match Simple.must_be_symbol field with
                | None -> Env.find_value_approximation body_env field
                | Some (sym, _) -> Value_approximation.Value_symbol sym)
              fields
            |> Array.of_list
          in
          Some
            (Env.add_block_approximation body_env (Name.var var) approxs
               alloc_mode)
        | Prim (Binary (Block_load _, block, field), _) -> begin
          match Env.find_value_approximation body_env block with
          | Value_unknown -> Some body_env
          | Closure_approximation _ | Value_symbol _ ->
            (* Here we assume [block] has already been substituted as a known
               symbol if it exists, and rely on the invariant that the
               approximation of a symbol is never a symbol. *)
            if Flambda_features.check_invariants ()
            then
              (* CR keryan: This is hidden behind invariants check because it
                 can appear on correct code using Lazy or GADT. It might warrant
                 a proper warning at some point. *)
              Misc.fatal_errorf
                "Unexpected approximation found when block approximation was \
                 expected in [Closure_conversion]: %a"
                Named.print defining_expr
            else None
          | Block_approximation (approx, _alloc_mode) -> (
            let approx =
              Simple.pattern_match field
                ~const:(fun const ->
                  match Reg_width_const.descr const with
                  | Tagged_immediate i ->
                    let i = Targetint_31_63.(Imm.to_int (to_targetint i)) in
                    if i >= Array.length approx
                    then
                      Misc.fatal_errorf
                        "Trying to access the %dth field of a block \
                         approximation of length %d."
                        i (Array.length approx);
                    approx.(i)
                  | _ -> Value_approximation.Value_unknown)
                ~name:(fun _ ~coercion:_ -> Value_approximation.Value_unknown)
            in
            match approx with
            | Value_symbol sym ->
              (* In spirit, this is the same as the simple case but more
                 cumbersome to detect, we have to remove the now useless
                 let-binding later. *)
              Some (Env.add_simple_to_substitute env id (Simple.symbol sym))
            | _ ->
              Some (Env.add_value_approximation body_env (Name.var var) approx))
        end
        | _ -> Some body_env
      in
      let var = VB.create var Name_mode.normal in
      let bound_pattern = Bound_pattern.singleton var in
      match body_env with
      | Some body_env ->
        let acc, body = body acc body_env in
        Let_with_acc.create acc bound_pattern defining_expr ~body
      | None ->
        ( acc,
          Expr.create_invalid
            (Defining_expr_of_let (bound_pattern, defining_expr)) ))
  in
  close_named acc env ~let_bound_var:var defining_expr cont

let close_let_cont acc env ~name ~is_exn_handler ~params
    ~(recursive : Asttypes.rec_flag)
    ~(handler : Acc.t -> Env.t -> Acc.t * Expr_with_acc.t)
    ~(body : Acc.t -> Env.t -> Acc.t * Expr_with_acc.t) :
    Acc.t * Expr_with_acc.t =
  (if is_exn_handler
  then
    match recursive with
    | Nonrecursive -> ()
    | Recursive ->
      Misc.fatal_errorf
        "[Let_cont]s marked as exception handlers must be [Nonrecursive]: %a"
        Continuation.print name);
  let params_with_kinds = params in
  let handler_env, params =
    Env.add_vars_like env
      (List.map
         (fun (param, user_visible, _kind) -> param, user_visible)
         params)
  in
  let handler_params =
    List.map2
      (fun param (_, _, kind) ->
        BP.create param (K.With_subkind.from_lambda kind))
      params params_with_kinds
    |> Bound_parameters.create
  in
  let handler acc =
    let handler_env =
      match Acc.continuation_known_arguments ~cont:name acc with
      | None -> handler_env
      | Some args ->
        List.fold_left2
          (fun env arg_approx param ->
            Env.add_value_approximation env (Name.var param) arg_approx)
          handler_env args params
    in
    handler acc handler_env
  in
  let body acc = body acc env in
  match recursive with
  | Nonrecursive ->
    Let_cont_with_acc.build_non_recursive acc name ~handler_params ~handler
      ~body ~is_exn_handler
  | Recursive ->
    let handlers =
      Continuation.Map.singleton name (handler, handler_params, is_exn_handler)
    in
    Let_cont_with_acc.build_recursive acc ~handlers ~body

let close_exact_or_unknown_apply acc env
    ({ kind;
       func;
       args;
       continuation;
       exn_continuation;
       loc;
       inlined;
       probe;
       mode;
       region_close = _
     } :
      IR.apply) callee_approx : Acc.t * Expr_with_acc.t =
  let callee = find_simple_from_id env func in
  let mode = Alloc_mode.from_lambda mode in
  let acc, call_kind =
    match kind with
    | Function -> (
      ( acc,
        match (callee_approx : Env.value_approximation option) with
        | Some (Closure_approximation (code_id, _function_slot, code_or_meta))
          ->
          let return_arity, is_tupled =
            let meta = Code_or_metadata.code_metadata code_or_meta in
            Code_metadata.(result_arity meta, is_tupled meta)
          in
          if is_tupled
          then
            (* CR keryan : We could do better here since we know the arity, but
               we would have to untuple the arguments and we lack information
               for now *)
            Call_kind.indirect_function_call_unknown_arity mode
          else Call_kind.direct_function_call code_id ~return_arity mode
        | None -> Call_kind.indirect_function_call_unknown_arity mode
        | _ -> assert false
        (* See [close_apply] *) ))
    | Method { kind; obj } ->
      let acc, obj = find_simple acc env obj in
      ( acc,
        Call_kind.method_call (Call_kind.method_kind_from_lambda kind) ~obj mode
      )
  in
  let acc, apply_exn_continuation =
    close_exn_continuation acc env exn_continuation
  in
  let acc, args = find_simples acc env args in
  let inlined_call = Inlined_attribute.from_lambda inlined in
  let probe_name =
    match probe with None -> None | Some { name } -> Some name
  in
  let apply =
    Apply.create ~callee ~continuation:(Return continuation)
      apply_exn_continuation ~args ~call_kind
      (Debuginfo.from_location loc)
      ~inlined:inlined_call
      ~inlining_state:(Inlining_state.default ~round:0)
      ~probe_name
      ~relative_history:(Env.relative_history_from_scoped ~loc env)
  in
  if Flambda_features.classic_mode ()
  then
    match Inlining.inlinable env apply callee_approx with
    | Not_inlinable -> Expr_with_acc.create_apply acc apply
    | Inlinable func_desc ->
      Inlining.inline acc ~apply ~apply_depth:(Env.current_depth env) ~func_desc
  else Expr_with_acc.create_apply acc apply

let close_apply_cont acc env ~dbg cont trap_action args :
    Acc.t * Expr_with_acc.t =
  let acc, args = find_simples acc env args in
  let trap_action = close_trap_action_opt trap_action in
  let args_approx = List.map (Env.find_value_approximation env) args in
  let acc, apply_cont =
    Apply_cont_with_acc.create acc ?trap_action ~args_approx cont ~args ~dbg
  in
  Expr_with_acc.create_apply_cont acc apply_cont

let close_switch acc env ~condition_dbg scrutinee (sw : IR.switch) :
    Acc.t * Expr_with_acc.t =
  let scrutinee = find_simple_from_id env scrutinee in
  let untagged_scrutinee = Variable.create "untagged" in
  let untagged_scrutinee' = VB.create untagged_scrutinee Name_mode.normal in
  let untag =
    Named.create_prim (Unary (Untag_immediate, scrutinee)) condition_dbg
  in
  let acc, arms =
    List.fold_left_map
      (fun acc (case, cont, trap_action, args) ->
        let trap_action = close_trap_action_opt trap_action in
        let acc, args = find_simples acc env args in
        let acc, action =
          Apply_cont_with_acc.create acc ?trap_action cont ~args
            ~dbg:condition_dbg
        in
        acc, (Targetint_31_63.int (Targetint_31_63.Imm.of_int case), action))
      acc sw.consts
  in
  match arms, sw.failaction with
  | [(case, action)], Some (default_action, default_trap_action, default_args)
    when sw.numconsts >= 3 ->
    (* Avoid enormous switches, where every arm goes to the same place except
       one, that arise from single-arm [Lambda] switches with a default case.
       (Seen in code generated by ppx_compare for variants, which exhibited
       quadratic size blowup.) *)
    let compare =
      Named.create_prim
        (Binary
           ( Phys_equal (Flambda_kind.naked_immediate, Eq),
             Simple.var untagged_scrutinee,
             Simple.const (Reg_width_const.naked_immediate case) ))
        condition_dbg
    in
    let comparison_result = Variable.create "eq" in
    let comparison_result' = VB.create comparison_result Name_mode.normal in
    let acc, default_action =
      let acc, args = find_simples acc env default_args in
      let trap_action = close_trap_action_opt default_trap_action in
      Apply_cont_with_acc.create acc ?trap_action default_action ~args
        ~dbg:condition_dbg
    in
    let acc, switch =
      let scrutinee = Simple.var comparison_result in
      Expr_with_acc.create_switch acc
        (Switch.if_then_else ~condition_dbg ~scrutinee ~if_true:action
           ~if_false:default_action)
    in
    let acc, body =
      Let_with_acc.create acc
        (Bound_pattern.singleton comparison_result')
        compare ~body:switch
    in
    Let_with_acc.create acc
      (Bound_pattern.singleton untagged_scrutinee')
      untag ~body
  | _, _ ->
    let acc, arms =
      match sw.failaction with
      | None -> acc, Targetint_31_63.Map.of_list arms
      | Some (default, trap_action, args) ->
        Numeric_types.Int.Set.fold
          (fun case (acc, cases) ->
            let case = Targetint_31_63.int (Targetint_31_63.Imm.of_int case) in
            if Targetint_31_63.Map.mem case cases
            then acc, cases
            else
              let acc, args = find_simples acc env args in
              let trap_action = close_trap_action_opt trap_action in
              let acc, default =
                Apply_cont_with_acc.create acc ?trap_action default ~args
                  ~dbg:condition_dbg
              in
              acc, Targetint_31_63.Map.add case default cases)
          (Numeric_types.Int.zero_to_n (sw.numconsts - 1))
          (acc, Targetint_31_63.Map.of_list arms)
    in
    if Targetint_31_63.Map.is_empty arms
    then Expr_with_acc.create_invalid acc Zero_switch_arms
    else
      let scrutinee = Simple.var untagged_scrutinee in
      let acc, body =
        match Targetint_31_63.Map.get_singleton arms with
        | Some (_discriminant, action) ->
          Expr_with_acc.create_apply_cont acc action
        | None ->
          Expr_with_acc.create_switch acc
            (Switch.create ~condition_dbg ~scrutinee ~arms)
      in
      Let_with_acc.create acc
        (Bound_pattern.singleton untagged_scrutinee')
        untag ~body

let close_one_function acc ~external_env ~by_function_slot decl
    ~has_lifted_closure ~value_slots_from_idents ~function_slots_from_idents
    function_declarations =
  let acc = Acc.with_free_names Name_occurrences.empty acc in
  let body = Function_decl.body decl in
  let loc = Function_decl.loc decl in
  let dbg = Debuginfo.from_location loc in
  let params = Function_decl.params decl in
  let return = Function_decl.return decl in
  let return_continuation = Function_decl.return_continuation decl in
  let recursive = Function_decl.recursive decl in
  let my_closure = Variable.create "my_closure" in
  let function_slot = Function_decl.function_slot decl in
  let my_depth = Variable.create "my_depth" in
  let next_depth = Variable.create "next_depth" in
  let our_let_rec_ident = Function_decl.let_rec_ident decl in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let code_id =
    Code_id.create
      ~name:(Function_slot.to_string function_slot)
      compilation_unit
  in
  let is_curried =
    match Function_decl.kind decl with Curried _ -> true | Tupled -> false
  in
  (* The free variables are: - The parameters: direct substitution by
     [Variable]s - The function being defined: accessible through [my_closure] -
     Other functions in the set being defined: accessible from [my_closure] then
     a [Project_function_slot] - Other free variables: accessible using
     [Project_value_slot] from [my_closure]. Note that free variables
     corresponding to predefined exception identifiers have been filtered out by
     [close_functions], above. *)
  let value_slots_to_bind, value_slots_for_idents =
    Ident.Map.fold
      (fun id value_slots_for_idents (to_bind, var_for_ident) ->
        let var = Variable.create_with_same_name_as_ident id in
        ( Variable.Map.add var value_slots_for_idents to_bind,
          Ident.Map.add id var var_for_ident ))
      value_slots_from_idents
      (Variable.Map.empty, Ident.Map.empty)
  in
  let coerce_to_deeper =
    Coercion.change_depth
      ~from:(Rec_info_expr.var my_depth)
      ~to_:(Rec_info_expr.var next_depth)
  in
  if has_lifted_closure && not (Variable.Map.is_empty value_slots_to_bind)
  then
    Misc.fatal_errorf
      "Variables found in closure when trying to lift %a in \
       [Closure_conversion]."
      Ident.print our_let_rec_ident;
  (* CR mshinwell: Remove "project_closure" names *)
  let project_closure_to_bind, simples_for_project_closure =
    if has_lifted_closure
    then (* No projection needed *)
      Variable.Map.empty, Ident.Map.empty
    else
      List.fold_left
        (fun (to_bind, simples_for_idents) function_decl ->
          let let_rec_ident = Function_decl.let_rec_ident function_decl in
          let to_bind, var =
            if Ident.same our_let_rec_ident let_rec_ident && is_curried
            then
              (* When the function being compiled is tupled, my_closure points
                 to the curried version but let_rec_ident is called with tuple
                 arguments, so the correct closure to bind is the one in the
                 function_slots_from_idents map. *)
              to_bind, my_closure
              (* my_closure is already bound *)
            else
              let variable =
                Variable.create_with_same_name_as_ident let_rec_ident
              in
              let function_slot =
                Ident.Map.find let_rec_ident function_slots_from_idents
              in
              Variable.Map.add variable function_slot to_bind, variable
          in
          let simple = Simple.with_coercion (Simple.var var) coerce_to_deeper in
          to_bind, Ident.Map.add let_rec_ident simple simples_for_idents)
        (Variable.Map.empty, Ident.Map.empty)
        (Function_decls.to_list function_declarations)
  in
  let closure_env_without_parameters =
    let empty_env = Env.clear_local_bindings external_env in
    let env_with_vars =
      Ident.Map.fold
        (fun id var env ->
          Simple.pattern_match
            (find_simple_from_id external_env id)
            ~const:(fun _ -> assert false)
            ~name:(fun name ~coercion:_ ->
              Env.add_approximation_alias (Env.add_var env id var) name
                (Name.var var)))
        value_slots_for_idents empty_env
    in
    Env.add_simple_to_substitute_map env_with_vars simples_for_project_closure
  in
  let closure_env_without_history =
    List.fold_right
      (fun (id, _) env ->
        let env, _var = Env.add_var_like env id User_visible in
        env)
      params closure_env_without_parameters
  in
  let closure_env = Env.with_depth closure_env_without_history my_depth in
  let closure_env, absolute_history, relative_history =
    let tracker = Env.inlining_history_tracker closure_env_without_history in
    let absolute, relative =
      Inlining_history.Tracker.fundecl_of_scoped_location
        ~name:(Function_slot.name function_slot)
        ~path_to_root:(Env.path_to_root closure_env)
        loc tracker
    in
    ( Env.use_inlining_history_tracker closure_env_without_history
        (Inlining_history.Tracker.inside_function absolute),
      absolute,
      relative )
  in
  (* CR-someday pchambart: eta-expansion wrappers for primitives are not marked
     as stubs but certainly should be. *)
  let stub = Function_decl.stub decl in
  let param_vars =
    List.map (fun (id, kind) -> Env.find_var closure_env id, kind) params
  in
  let params =
    List.map
      (fun (var, kind) -> BP.create var (K.With_subkind.from_lambda kind))
      param_vars
    |> Bound_parameters.create
  in
  let acc = Acc.with_seen_a_function acc false in
  let acc, body =
    try body acc closure_env
    with Misc.Fatal_error ->
      let bt = Printexc.get_raw_backtrace () in
      Format.eprintf
        "\n\
         %sContext is:%s closure converting function@ with [our_let_rec_ident] \
         %a (function slot %a)"
        (* @ \ *)
        (* and body:@ %a *)
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Ident.print our_let_rec_ident Function_slot.print function_slot;
      (* print body *)
      Printexc.raise_with_backtrace Misc.Fatal_error bt
  in
  let contains_subfunctions = Acc.seen_a_function acc in
  let my_closure' = Simple.var my_closure in
  let acc, body =
    (* CR mshinwell: These Project_function_slot operations should maybe be
       inserted at the point of use rather than at the top of the function. We
       should also check the behaviour of the backend w.r.t. CSE of projections
       from closures. *)
    Variable.Map.fold
      (fun var move_to (acc, body) ->
        let move : Flambda_primitive.unary_primitive =
          Project_function_slot { move_from = function_slot; move_to }
        in
        let var = VB.create var Name_mode.normal in
        let named =
          Named.create_prim (Unary (move, my_closure')) Debuginfo.none
        in
        Let_with_acc.create acc (Bound_pattern.singleton var) named ~body)
      project_closure_to_bind (acc, body)
  in
  let acc, body =
    Variable.Map.fold
      (fun var value_slot (acc, body) ->
        let var = VB.create var Name_mode.normal in
        let named =
          Named.create_prim
            (Unary
               ( Project_value_slot { project_from = function_slot; value_slot },
                 my_closure' ))
            Debuginfo.none
        in
        Let_with_acc.create acc (Bound_pattern.singleton var) named ~body)
      value_slots_to_bind (acc, body)
  in
  let next_depth_expr = Rec_info_expr.succ (Rec_info_expr.var my_depth) in
  let bound =
    Bound_pattern.singleton (Bound_var.create next_depth Name_mode.normal)
  in
  let acc, body =
    Let_with_acc.create acc bound (Named.create_rec_info next_depth_expr) ~body
  in
  let cost_metrics = Acc.cost_metrics acc in
  let acc, exn_continuation =
    close_exn_continuation acc external_env
      (Function_decl.exn_continuation decl)
  in
  assert (
    match Exn_continuation.extra_args exn_continuation with
    | [] -> true
    | _ :: _ -> false);
  let inline : Inline_attribute.t =
    (* We make a decision based on [fallback_inlining_heuristic] here to try to
       mimic Closure's behaviour as closely as possible, particularly when there
       are functions involving constant closures, which are not lifted during
       Closure (but will prevent inlining) but will likely have been lifted by
       our other check in [Inlining_cost] (thus preventing us seeing they were
       originally there). *)
    if contains_subfunctions
       && Flambda_features.Expert.fallback_inlining_heuristic ()
    then Never_inline
    else Inline_attribute.from_lambda (Function_decl.inline decl)
  in
  let params_and_body =
    Function_params_and_body.create ~return_continuation
      ~exn_continuation:(Exn_continuation.exn_handler exn_continuation)
      params ~body ~my_closure ~my_depth
      ~free_names_of_body:(Known (Acc.free_names acc))
  in
  let acc =
    List.fold_left
      (fun acc param -> Acc.remove_var_from_free_names (BP.var param) acc)
      acc
      (Bound_parameters.to_list params)
    |> Acc.remove_var_from_free_names my_closure
    |> Acc.remove_var_from_free_names my_depth
    |> Acc.remove_continuation_from_free_names return_continuation
    |> Acc.remove_continuation_from_free_names
         (Exn_continuation.exn_handler exn_continuation)
  in
  let params_arity = Bound_parameters.arity_with_subkinds params in
  let is_tupled =
    match Function_decl.kind decl with Curried _ -> false | Tupled -> true
  in
  let inlining_decision =
    if Flambda_features.classic_mode ()
    then Inlining.definition_inlining_decision inline cost_metrics
    else if stub
    then Function_decl_inlining_decision_type.Stub
    else Function_decl_inlining_decision_type.Not_yet_decided
  in
  let code =
    Code.create code_id ~params_and_body
      ~free_names_of_params_and_body:(Acc.free_names acc) ~params_arity
      ~num_trailing_local_params:(Function_decl.num_trailing_local_params decl)
      ~result_arity:
        (Flambda_arity.With_subkinds.create [K.With_subkind.from_lambda return])
      ~result_types:
        (Result_types.create_unknown ~params
           ~result_arity:
             (Flambda_arity.With_subkinds.create
                [K.With_subkind.from_lambda return]))
      ~contains_no_escaping_local_allocs:
        (Function_decl.contains_no_escaping_local_allocs decl)
      ~stub ~inline
      ~is_a_functor:(Function_decl.is_a_functor decl)
      ~recursive ~newer_version_of:None ~cost_metrics
      ~inlining_arguments:(Inlining_arguments.create ~round:0)
      ~dbg ~is_tupled
      ~is_my_closure_used:
        (Function_params_and_body.is_my_closure_used params_and_body)
      ~inlining_decision ~absolute_history ~relative_history
  in
  let approx =
    let code = Code_or_metadata.create code in
    let meta = Code_or_metadata.remember_only_metadata code in
    if Flambda_features.classic_mode ()
    then begin
      Inlining_report.record_decision_at_function_definition ~absolute_history
        ~code_metadata:(Code_or_metadata.code_metadata meta)
        ~pass:After_closure_conversion
        ~are_rebuilding_terms:(Are_rebuilding_terms.of_bool true)
        inlining_decision;
      if Function_decl_inlining_decision_type.must_be_inlined inlining_decision
      then code
      else meta
    end
    else meta
  in
  let acc = Acc.add_code ~code_id ~code acc in
  let acc = Acc.with_seen_a_function acc true in
  acc, Function_slot.Map.add function_slot (code_id, approx) by_function_slot

let close_functions acc external_env function_declarations =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let value_slots_from_idents =
    Ident.Set.fold
      (fun id map ->
        (* Filter out predefined exception identifiers and simple substitutions.
           The former will be turned into symbols, and the latter substituted
           when we closure-convert the body *)
        let has_non_var_subst, subst_var =
          match Env.find_simple_to_substitute_exn external_env id with
          | exception Not_found -> false, None
          | simple ->
            Simple.pattern_match simple
              ~const:(fun _ -> true, None)
              ~name:(fun name ~coercion:_ ->
                Name.pattern_match name
                  ~var:(fun var -> false, Some var)
                  ~symbol:(fun _ -> true, None))
        in
        if has_non_var_subst || Ident.is_predef id
        then map
        else
          let name =
            match subst_var with
            | None -> Ident.name id
            | Some var -> Variable.name var
          in
          Ident.Map.add id (Value_slot.create compilation_unit ~name) map)
      (Function_decls.all_free_idents function_declarations)
      Ident.Map.empty
  in
  let can_be_lifted =
    Ident.Map.is_empty value_slots_from_idents
    && Flambda_features.classic_mode ()
  in
  let func_decl_list = Function_decls.to_list function_declarations in
  let function_slots_from_idents =
    List.fold_left
      (fun map decl ->
        let id = Function_decl.let_rec_ident decl in
        let function_slot = Function_decl.function_slot decl in
        Ident.Map.add id function_slot map)
      Ident.Map.empty func_decl_list
  in
  let external_env, symbol_map =
    if can_be_lifted
    then
      Ident.Map.fold
        (fun ident function_slot (env, symbol_map) ->
          let env, symbol =
            declare_symbol_for_function_slot env ident function_slot
          in
          env, Function_slot.Map.add function_slot symbol symbol_map)
        function_slots_from_idents
        (external_env, Function_slot.Map.empty)
    else external_env, Function_slot.Map.empty
  in
  let acc, approximations =
    List.fold_left
      (fun (acc, by_function_slot) function_decl ->
        let _, _, acc, expr =
          Acc.measure_cost_metrics acc ~f:(fun acc ->
              close_one_function acc ~external_env ~by_function_slot
                function_decl ~has_lifted_closure:can_be_lifted
                ~value_slots_from_idents ~function_slots_from_idents
                function_declarations)
        in
        acc, expr)
      (acc, Function_slot.Map.empty)
      func_decl_list
  in
  let acc = Acc.with_free_names Name_occurrences.empty acc in
  (* CR lmaurer: funs has arbitrary order (ultimately coming from
     function_declarations) *)
  let funs =
    let funs =
      Function_slot.Map.fold
        (fun cid (code_id, _) funs -> (cid, code_id) :: funs)
        approximations []
    in
    Function_slot.Lmap.of_list (List.rev funs)
  in
  let approximations =
    Function_slot.Map.mapi
      (fun function_slot (code_id, code) ->
        Value_approximation.Closure_approximation (code_id, function_slot, code))
      approximations
  in
  let function_decls = Function_declarations.create funs in
  let value_slots =
    Ident.Map.fold
      (fun id value_slot map ->
        let external_simple = find_simple_from_id external_env id in
        (* We're sure [external_simple] is a variable since
           [value_slot_from_idents] has already filtered constants and symbols
           out. *)
        Value_slot.Map.add value_slot external_simple map)
      value_slots_from_idents Value_slot.Map.empty
  in
  let set_of_closures =
    Set_of_closures.create ~value_slots
      (Alloc_mode.from_lambda (Function_decls.alloc_mode function_declarations))
      function_decls
  in
  let acc =
    Acc.add_set_of_closures_offsets ~is_phantom:false acc set_of_closures
  in
  if can_be_lifted
  then
    let symbols =
      Function_slot.Lmap.mapi
        (fun function_slot _ ->
          ( Function_slot.Map.find function_slot symbol_map,
            Function_slot.Map.find function_slot approximations ))
        funs
    in
    let acc = Acc.add_lifted_set_of_closures ~symbols ~set_of_closures acc in
    acc, Lifted symbols
  else acc, Dynamic (set_of_closures, approximations)

let close_let_rec acc env ~function_declarations
    ~(body : Acc.t -> Env.t -> Acc.t * Expr_with_acc.t) =
  let env =
    List.fold_right
      (fun decl env ->
        let id = Function_decl.let_rec_ident decl in
        let env, _var = Env.add_var_like env id User_visible in
        env)
      function_declarations env
  in
  let fun_vars_map, ident_map =
    List.fold_left
      (fun (fun_vars_map, ident_map) decl ->
        let ident = Function_decl.let_rec_ident decl in
        let fun_var = VB.create (Env.find_var env ident) Name_mode.normal in
        let function_slot = Function_decl.function_slot decl in
        ( Function_slot.Map.add function_slot fun_var fun_vars_map,
          Function_slot.Map.add function_slot ident ident_map ))
      (Function_slot.Map.empty, Function_slot.Map.empty)
      function_declarations
  in
  let alloc_mode =
    (* The closure allocation mode must be the same for all closures in the set
       of closures. *)
    List.fold_left
      (fun (alloc_mode : Lambda.alloc_mode option) function_decl ->
        match alloc_mode, Function_decl.closure_alloc_mode function_decl with
        | None, alloc_mode -> Some alloc_mode
        | Some Alloc_heap, Alloc_heap | Some Alloc_local, Alloc_local ->
          alloc_mode
        | Some Alloc_heap, Alloc_local | Some Alloc_local, Alloc_heap ->
          Misc.fatal_errorf
            "let-rec group of [lfunction] declarations have inconsistent alloc \
             modes:@ %a"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space
               Function_slot.print)
            (List.map Function_decl.function_slot function_declarations))
      None function_declarations
  in
  let alloc_mode =
    match alloc_mode with
    | Some alloc_mode -> alloc_mode
    | None ->
      Misc.fatal_error "let-rec group of [lfunction] declarations is empty"
  in
  let acc, closed_functions =
    close_functions acc env
      (Function_decls.create function_declarations alloc_mode)
  in
  match closed_functions with
  | Lifted symbols ->
    let env =
      Function_slot.Lmap.fold
        (fun function_slot (symbol, approx) env ->
          let ident = Function_slot.Map.find function_slot ident_map in
          let env =
            Env.add_simple_to_substitute env ident (Simple.symbol symbol)
          in
          Env.add_value_approximation env (Name.symbol symbol) approx)
        symbols env
    in
    body acc env
  | Dynamic (set_of_closures, approximations) ->
    (* CR mshinwell: We should maybe have something more elegant here *)
    let generated_closures =
      Function_slot.Set.diff
        (Function_slot.Map.keys
           (Function_declarations.funs
              (Set_of_closures.function_decls set_of_closures)))
        (Function_slot.Map.keys fun_vars_map)
    in
    let fun_vars_map =
      Function_slot.Set.fold
        (fun function_slot fun_vars_map ->
          let fun_var =
            VB.create (Variable.create "generated") Name_mode.normal
          in
          Function_slot.Map.add function_slot fun_var fun_vars_map)
        generated_closures fun_vars_map
    in
    let bound_vars =
      List.map
        (fun (function_slot, _) ->
          Function_slot.Map.find function_slot fun_vars_map)
        (Function_declarations.funs_in_order
           (Set_of_closures.function_decls set_of_closures)
        |> Function_slot.Lmap.bindings)
    in
    let env =
      Function_slot.Map.fold
        (fun function_slot fun_var env ->
          let approx = Function_slot.Map.find function_slot approximations in
          Env.add_value_approximation env (Name.var (VB.var fun_var)) approx)
        fun_vars_map env
    in
    let acc, body = body acc env in
    let named = Named.create_set_of_closures set_of_closures in
    Let_with_acc.create acc
      (Bound_pattern.set_of_closures bound_vars)
      named ~body

let wrap_partial_application acc env apply_continuation (apply : IR.apply)
    approx args missing_args ~arity ~num_trailing_local_params
    ~contains_no_escaping_local_allocs =
  (* In case of partial application, creates a wrapping function from scratch to
     allow inlining and lifting *)
  let wrapper_id = Ident.create_local ("partial_" ^ Ident.name apply.func) in
  let function_slot =
    Function_slot.create
      (Compilation_unit.get_current_exn ())
      ~name:(Ident.name wrapper_id)
  in
  let args_arity = List.length args in
  let params =
    (* CR keryan: We should use the arity to produce better kinds *)
    List.mapi
      (fun n _kind_with_subkind ->
        ( Ident.create_local ("param" ^ string_of_int (args_arity + n)),
          Lambda.Pgenval ))
      (Flambda_arity.With_subkinds.to_list missing_args)
  in
  let return_continuation = Continuation.create ~sort:Return () in
  let exn_continuation =
    IR.{ exn_handler = Continuation.create (); extra_args = [] }
  in
  let all_args = args @ List.map (fun (a, _) -> IR.Var a) params in
  let fbody acc env =
    close_exact_or_unknown_apply acc env
      { apply with
        kind = Function;
        args = all_args;
        continuation = return_continuation;
        exn_continuation
      }
      (Some approx)
  in
  let attr =
    Lambda.
      { inline = Default_inline;
        specialise = Default_specialise;
        local = Default_local;
        is_a_functor = false;
        stub = false
      }
  in
  let free_idents_of_body =
    List.fold_left
      (fun ids -> function IR.Var id -> Ident.Set.add id ids | _ -> ids)
      (Ident.Set.singleton apply.func)
      all_args
  in
  let closure_alloc_mode, num_trailing_local_params =
    let num_leading_heap_params =
      Flambda_arity.With_subkinds.cardinal arity - num_trailing_local_params
    in
    if args_arity <= num_leading_heap_params
    then Lambda.Alloc_heap, num_trailing_local_params
    else
      let num_supplied_local_args = args_arity - num_leading_heap_params in
      Lambda.Alloc_local, num_trailing_local_params - num_supplied_local_args
  in
  let function_declarations =
    (* CR keryan: Same as above, better kind for return type *)
    [ Function_decl.create ~let_rec_ident:(Some wrapper_id) ~function_slot
        ~kind:(Lambda.Curried { nlocal = num_trailing_local_params })
        ~params ~return:Lambda.Pgenval ~return_continuation ~exn_continuation
        ~body:fbody ~attr ~loc:apply.loc ~free_idents_of_body ~stub:true
        ~closure_alloc_mode ~num_trailing_local_params
        ~contains_no_escaping_local_allocs Recursive.Non_recursive ]
  in
  let body acc env =
    let arg = find_simple_from_id env wrapper_id in
    let acc, apply_cont =
      Apply_cont_with_acc.create acc
        ~args_approx:[Env.find_value_approximation env arg]
        apply_continuation ~args:[arg] ~dbg:Debuginfo.none
    in
    Expr_with_acc.create_apply_cont acc apply_cont
  in
  close_let_rec acc env ~function_declarations ~body

let wrap_over_application acc env full_call (apply : IR.apply) over_args
    ~contains_no_escaping_local_allocs ~result_arity =
  let wrapper_cont = Continuation.create () in
  let returned_func = Variable.create "func" in
  (* See comments in [Simplify_common.split_direct_over_application] about this
     code for handling local allocations. *)
  let apply_alloc_mode = Alloc_mode.from_lambda apply.mode in
  let apply_return_continuation =
    Apply.Result_continuation.Return apply.continuation
  in
  let acc, args = find_simples acc env over_args in
  let apply_dbg = Debuginfo.from_location apply.loc in
  let needs_region =
    match apply_alloc_mode, contains_no_escaping_local_allocs with
    | Heap, false ->
      Some (Variable.create "over_app_region", Continuation.create ())
    | Heap, true | Local, _ -> None
  in
  let perform_over_application acc =
    let acc, apply_exn_continuation =
      close_exn_continuation acc env apply.exn_continuation
    in
    let inlined = Inlined_attribute.from_lambda apply.inlined in
    (* Keeping the attributes is useless in classic mode but matches the
       behaviour of simplify, and this split is done either way *)
    let probe_name =
      match apply.probe with None -> None | Some { name } -> Some name
    in
    let alloc_mode : Alloc_mode.t =
      if contains_no_escaping_local_allocs then Heap else Local
    in
    let call_kind = Call_kind.indirect_function_call_unknown_arity alloc_mode in
    let continuation =
      match needs_region with
      | None -> apply_return_continuation
      | Some (_, cont) -> Apply.Result_continuation.Return cont
    in
    let over_application =
      Apply.create ~callee:(Simple.var returned_func) ~continuation
        apply_exn_continuation ~args ~call_kind apply_dbg ~inlined
        ~inlining_state:(Inlining_state.default ~round:0)
        ~probe_name
        ~relative_history:(Env.relative_history_from_scoped ~loc:apply.loc env)
    in
    match needs_region with
    | None -> Expr_with_acc.create_apply acc over_application
    | Some (region, after_over_application) ->
      let over_application_results =
        List.mapi
          (fun i kind ->
            BP.create (Variable.create ("result" ^ string_of_int i)) kind)
          (Flambda_arity.With_subkinds.to_list result_arity)
      in
      let handler acc =
        let acc, call_return_continuation =
          let acc, apply_cont_expr =
            Apply_cont_with_acc.create acc apply.continuation
              ~args:(List.map BP.simple over_application_results)
              ~dbg:apply_dbg
          in
          acc, Expr.create_apply_cont apply_cont_expr
        in
        Let_with_acc.create acc
          (Bound_pattern.singleton
             (Bound_var.create (Variable.create "unit") Name_mode.normal))
          (Named.create_prim (Unary (End_region, Simple.var region)) apply_dbg)
          ~body:call_return_continuation
      in
      Let_cont_with_acc.build_non_recursive acc after_over_application
        ~handler_params:(Bound_parameters.create over_application_results)
        ~handler
        ~body:(fun acc -> Expr_with_acc.create_apply acc over_application)
        ~is_exn_handler:false
  in
  let body = full_call wrapper_cont in
  let acc, both_applications =
    Let_cont_with_acc.build_non_recursive acc wrapper_cont
      ~handler_params:
        ([BP.create returned_func K.With_subkind.any_value]
        |> Bound_parameters.create)
      ~handler:perform_over_application ~body ~is_exn_handler:false
  in
  match needs_region with
  | None -> acc, both_applications
  | Some (region, _) ->
    Let_with_acc.create acc
      (Bound_pattern.singleton (Bound_var.create region Name_mode.normal))
      (Named.create_prim (Nullary Begin_region) apply_dbg)
      ~body:both_applications

type call_args_split =
  | Exact of IR.simple list
  | Partial_app of IR.simple list * Flambda_arity.With_subkinds.t
  | Over_app of IR.simple list * IR.simple list

let close_apply acc env (apply : IR.apply) : Acc.t * Expr_with_acc.t =
  let callee = find_simple_from_id env apply.func in
  let approx = Env.find_value_approximation env callee in
  let code_info =
    match approx with
    | Closure_approximation (_, _, Code_present code) ->
      Some
        ( Code.params_arity code,
          Code.is_tupled code,
          Code.num_trailing_local_params code,
          Code.contains_no_escaping_local_allocs code,
          Code.result_arity code )
    | Closure_approximation (_, _, Metadata_only metadata) ->
      Some
        ( Code_metadata.params_arity metadata,
          Code_metadata.is_tupled metadata,
          Code_metadata.num_trailing_local_params metadata,
          Code_metadata.contains_no_escaping_local_allocs metadata,
          Code_metadata.result_arity metadata )
    | Value_unknown -> None
    | _ ->
      if Flambda_features.check_invariants ()
      then
        Misc.fatal_errorf
          "Unexpected approximation for callee %a in [Closure_conversion], \
           expected a closure approximation."
          Simple.print callee
      else None
  in
  match code_info with
  | None -> close_exact_or_unknown_apply acc env apply None
  | Some
      ( arity,
        is_tupled,
        num_trailing_local_params,
        contains_no_escaping_local_allocs,
        result_arity ) -> (
    let split_args =
      let arity = Flambda_arity.With_subkinds.to_list arity in
      let split args arity =
        let rec cut n l =
          if n <= 0
          then [], l
          else
            match l with
            | [] -> [], []
            | h :: t ->
              let before, after = cut (n - 1) t in
              h :: before, after
        in
        let args_l = List.length args in
        let arity_l = List.length arity in
        if args_l = arity_l
        then Exact args
        else if args_l < arity_l
        then
          let _, missing_args = cut args_l arity in
          Partial_app (args, Flambda_arity.With_subkinds.create missing_args)
        else
          let args, remaining_args = cut arity_l args in
          Over_app (args, remaining_args)
      in
      let arity =
        if is_tupled
        then [Flambda_kind.With_subkind.block Tag.zero arity]
        else arity
      in
      split apply.args arity
    in
    match split_args with
    | Exact args ->
      close_exact_or_unknown_apply acc env
        { apply with args; continuation = apply.continuation }
        (Some approx)
    | Partial_app (args, missing_args) ->
      wrap_partial_application acc env apply.continuation apply approx args
        missing_args ~arity ~num_trailing_local_params
        ~contains_no_escaping_local_allocs
    | Over_app (args, remaining_args) ->
      let full_args_call apply_continuation acc =
        close_exact_or_unknown_apply acc env
          { apply with args; continuation = apply_continuation }
          (Some approx)
      in
      wrap_over_application acc env full_args_call apply remaining_args
        ~contains_no_escaping_local_allocs ~result_arity)

module CIS = Code_id_or_symbol
module GroupMap = Numbers.Int.Map
module SCC = Strongly_connected_components.Make (Numbers.Int)

let bind_code_and_sets_of_closures all_code sets_of_closures acc body =
  let fresh_group_id =
    let i = ref 0 in
    fun () ->
      let n = !i in
      incr i;
      n
  in
  let group_to_bound_consts, symbol_to_groups =
    Code_id.Map.fold
      (fun code_id code (g2c, s2g) ->
        let id = fresh_group_id () in
        let bound = Bound_static.Pattern.code code_id in
        let const = Static_const_or_code.create_code code in
        ( GroupMap.add id (bound, const) g2c,
          CIS.Map.add (CIS.create_code_id code_id) id s2g ))
      all_code
      (GroupMap.empty, CIS.Map.empty)
  in
  let group_to_bound_consts, symbol_to_groups =
    List.fold_left
      (fun (g2c, s2g) (symbols, set_of_closures) ->
        let id = fresh_group_id () in
        let symbols = Function_slot.Lmap.map fst symbols in
        let bound = Bound_static.Pattern.set_of_closures symbols in
        let const =
          Static_const_or_code.create_static_const
            (Set_of_closures set_of_closures)
        in
        ( GroupMap.add id (bound, const) g2c,
          Function_slot.Lmap.fold
            (fun _function_slot symbol s2g ->
              CIS.Map.add (CIS.create_symbol symbol) id s2g)
            symbols s2g ))
      (group_to_bound_consts, symbol_to_groups)
      sets_of_closures
  in
  let graph =
    GroupMap.map
      (fun (_bound, const) ->
        let free_names = Static_const_or_code.free_names const in
        let deps =
          Code_id.Set.fold
            (fun code_id deps ->
              match
                CIS.Map.find (CIS.create_code_id code_id) symbol_to_groups
              with
              | exception Not_found -> deps
              | id -> Numbers.Int.Set.add id deps)
            (Name_occurrences.code_ids free_names)
            Numbers.Int.Set.empty
        in
        Symbol.Set.fold
          (fun symbol deps ->
            match CIS.Map.find (CIS.create_symbol symbol) symbol_to_groups with
            | exception Not_found -> deps
            | id -> Numbers.Int.Set.add id deps)
          (Name_occurrences.symbols free_names)
          deps)
      group_to_bound_consts
  in
  let components = SCC.connected_components_sorted_from_roots_to_leaf graph in
  Array.fold_left
    (fun (acc, body) (component : SCC.component) ->
      let group_ids =
        match component with
        | No_loop group_id -> [group_id]
        | Has_loop group_ids -> group_ids
      in
      let bound_static, static_consts =
        List.fold_left
          (fun (bound_static, static_consts) group_id ->
            let bound_symbol, static_const =
              try GroupMap.find group_id group_to_bound_consts
              with Not_found ->
                Misc.fatal_errorf "Unbound static consts group ID %d" group_id
            in
            bound_symbol :: bound_static, static_const :: static_consts)
          ([], []) group_ids
      in
      let defining_expr =
        Static_const_group.create static_consts |> Named.create_static_consts
      in
      Let_with_acc.create acc
        (Bound_pattern.static (Bound_static.create bound_static))
        defining_expr ~body)
    (acc, body) components

let close_program ~symbol_for_global ~big_endian ~cmx_loader ~module_ident
    ~module_block_size_in_words ~program ~prog_return_cont ~exn_continuation =
  let symbol_for_global ident = symbol_for_global ?comp_unit:None ident in
  let env = Env.create ~symbol_for_global ~big_endian ~cmx_loader in
  let module_symbol =
    symbol_for_global (Ident.create_persistent (Ident.name module_ident))
  in
  let module_block_tag = Tag.Scannable.zero in
  let module_block_var = Variable.create "module_block" in
  let return_cont = Continuation.create ~sort:Toplevel_return () in
  let slot_offsets = Slot_offsets.create () in
  let acc = Acc.create ~symbol_for_global ~slot_offsets in
  let load_fields_body acc =
    let field_vars =
      List.init module_block_size_in_words (fun pos ->
          let pos_str = string_of_int pos in
          pos, Variable.create ("field_" ^ pos_str))
    in
    let acc, body =
      let static_const : Static_const.t =
        let field_vars =
          List.map
            (fun (_, var) : Field_of_static_block.t ->
              Dynamically_computed (var, Debuginfo.none))
            field_vars
        in
        Block (module_block_tag, Immutable, field_vars)
      in
      let acc, arg = use_of_symbol_as_simple acc module_symbol in
      let acc, apply_cont =
        (* Module initialisers return unit, but since that is taken care of
           during Cmm generation, we can instead "return" [module_symbol] here
           to ensure that its associated "let symbol" doesn't get deleted. *)
        Apply_cont_with_acc.create acc return_cont ~args:[arg]
          ~dbg:Debuginfo.none
      in
      let acc, return = Expr_with_acc.create_apply_cont acc apply_cont in
      let bound_static =
        Bound_static.singleton (Bound_static.Pattern.block_like module_symbol)
      in
      let named =
        Named.create_static_consts
          (Static_const_group.create
             [Static_const_or_code.create_static_const static_const])
      in
      Let_with_acc.create acc
        (Bound_pattern.static bound_static)
        named ~body:return
    in
    let block_access : P.Block_access_kind.t =
      Values
        { tag = Known Tag.Scannable.zero;
          size = Known (Targetint_31_63.Imm.of_int module_block_size_in_words);
          field_kind = Any_value
        }
    in
    List.fold_left
      (fun (acc, body) (pos, var) ->
        let var = VB.create var Name_mode.normal in
        let pos = Targetint_31_63.int (Targetint_31_63.Imm.of_int pos) in
        let named =
          Named.create_prim
            (Binary
               ( Block_load (block_access, Immutable),
                 Simple.var module_block_var,
                 Simple.const (Reg_width_const.tagged_immediate pos) ))
            Debuginfo.none
        in
        Let_with_acc.create acc (Bound_pattern.singleton var) named ~body)
      (acc, body) (List.rev field_vars)
  in
  let load_fields_handler_param =
    [BP.create module_block_var K.With_subkind.any_value]
    |> Bound_parameters.create
  in
  let acc, body =
    (* This binds the return continuation that is free (or, at least, not bound)
       in the incoming code. The handler for the continuation receives a tuple
       with fields indexed from zero to [module_block_size_in_words]. The
       handler extracts the fields; the variables bound to such fields are then
       used to define the module block symbol. *)
    let body acc = program acc env in
    Let_cont_with_acc.build_non_recursive acc prog_return_cont
      ~handler_params:load_fields_handler_param ~handler:load_fields_body ~body
      ~is_exn_handler:false
  in
  let module_block_approximation =
    match Acc.continuation_known_arguments ~cont:prog_return_cont acc with
    | Some [approx] -> approx
    | _ -> Value_approximation.Value_unknown
  in
  let acc, body =
    bind_code_and_sets_of_closures (Acc.code acc)
      (Acc.lifted_sets_of_closures acc)
      acc body
  in
  (* We must make sure there is always an outer [Let_symbol] binding so that
     lifted constants not in the scope of any other [Let_symbol] binding get put
     into the term and not dropped. Adding this extra binding, which will
     actually be removed by the simplifier, avoids a special case. *)
  let acc =
    match Acc.declared_symbols acc with
    | _ :: _ -> acc
    | [] ->
      let acc, (_sym : Symbol.t) =
        register_const0 acc
          (Static_const.Block (Tag.Scannable.zero, Immutable, []))
          "first_const"
      in
      acc
  in
  let symbols_approximations =
    let symbol_approxs =
      List.fold_left
        (fun sa (symbol, _) ->
          Symbol.Map.add symbol Value_approximation.Value_unknown sa)
        (Symbol.Map.singleton module_symbol module_block_approximation)
        (Acc.declared_symbols acc)
    in
    List.fold_left
      (fun sa (closure_map, _) ->
        Function_slot.Lmap.fold
          (fun _ (symbol, approx) sa -> Symbol.Map.add symbol approx sa)
          closure_map sa)
      symbol_approxs
      (Acc.lifted_sets_of_closures acc)
  in
  let acc, body =
    List.fold_left
      (fun (acc, body) (symbol, static_const) ->
        let bound_static =
          Bound_static.singleton (Bound_static.Pattern.block_like symbol)
        in
        let defining_expr =
          Static_const_group.create
            [Static_const_or_code.create_static_const static_const]
          |> Named.create_static_consts
        in
        Let_with_acc.create acc
          (Bound_pattern.static bound_static)
          defining_expr ~body)
      (acc, body) (Acc.declared_symbols acc)
  in
  let get_code_metadata code_id =
    Code_id.Map.find code_id (Acc.code acc) |> Code.code_metadata
  in
  let all_code =
    Exported_code.add_code (Acc.code acc)
      ~keep_code:(fun _ -> false)
      (Exported_code.mark_as_imported
         (Flambda_cmx.get_imported_code cmx_loader ()))
  in
  let used_value_slots, exported_offsets =
    let used_slots =
      let free_names = Acc.free_names acc in
      Or_unknown.Known
        Slot_offsets.
          { function_slots_in_normal_projections =
              Name_occurrences.function_slots_in_normal_projections free_names;
            all_function_slots = Name_occurrences.all_function_slots free_names;
            value_slots_in_normal_projections =
              Name_occurrences.value_slots_in_normal_projections free_names;
            all_value_slots = Name_occurrences.all_value_slots free_names
          }
    in
    Slot_offsets.finalize_offsets (Acc.slot_offsets acc) ~get_code_metadata
      ~used_slots
  in
  let used_value_slots =
    match used_value_slots with
    | Known used_value_slots -> used_value_slots
    | Unknown ->
      Misc.fatal_error "Closure_conversion needs to know its used value slots."
  in
  let cmx =
    Flambda_cmx.prepare_cmx_from_approx ~approxs:symbols_approximations
      ~module_symbol ~exported_offsets ~used_value_slots all_code
  in
  ( Flambda_unit.create ~return_continuation:return_cont ~exn_continuation ~body
      ~module_symbol ~used_value_slots:(Known used_value_slots),
    all_code,
    cmx,
    exported_offsets )
