(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Typeopt
open Lambda
open Debuginfo.Scoped_location

type error =
    Free_super_var
  | Unreachable_reached
  | Local_allocs_not_enabled

exception Error of Location.t * error

let use_dup_for_constant_arrays_bigger_than = 4

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun ~scopes:_ _cc _rootpath _modl -> assert false) :
      scopes:scopes -> module_coercion -> Path.t option ->
      module_expr -> lambda)

let transl_object =
  ref (fun ~scopes:_ _id _s _cl -> assert false :
       scopes:scopes -> Ident.t -> string list -> class_expr -> lambda)

(* Probe handlers are generated from %probe as closed functions
   during transl_exp and immediately lifted to top level. *)
let probe_handlers = ref []
let clear_probe_handlers () = probe_handlers := []
let declare_probe_handlers lam =
  List.fold_left (fun acc (funcid, func) ->
      Llet(Strict, Pgenval, funcid, func, acc))
    lam
    !probe_handlers

(* Compile an exception/extension definition *)

let prim_fresh_oo_id =
  Pccall (Primitive.simple ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

let transl_extension_constructor ~scopes env path ext =
  let path =
    Printtyp.wrap_printing_env env ~error:true (fun () ->
      Option.map (Printtyp.rewrite_double_underscore_paths env) path)
  in
  let name =
    match path, !Clflags.for_package with
      None, _ -> Ident.name ext.ext_id
    | Some p, None -> Path.name p
    | Some p, Some pack -> Printf.sprintf "%s.%s" pack (Path.name p)
  in
  let loc = of_location ~scopes ext.ext_loc in
  match ext.ext_kind with
    Text_decl _ ->
      (* Extension constructors are currently always Alloc_heap.
         They could be Alloc_local, but that would require changes
         to pattern typing, as patterns can close over them. *)
      Lprim (Pmakeblock (Obj.object_tag, Immutable_unique, None, Alloc_heap),
        [Lconst (Const_base (Const_string (name, ext.ext_loc, None)));
         Lprim (prim_fresh_oo_id, [Lconst (const_int 0)], loc)],
        loc)
  | Text_rebind(path, _lid) ->
      transl_extension_path loc env path

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

let extract_float = function
    Const_base(Const_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"

let transl_alloc_mode loc alloc_mode : Lambda.alloc_mode =
  match Btype.Alloc_mode.constrain_lower alloc_mode with
  | Global -> Alloc_heap
  | Local ->
     if not (Clflags.Extension.is_enabled Local) then
       raise (Error (loc, Local_allocs_not_enabled));
     Alloc_local

let transl_value_mode loc mode =
  let alloc_mode = Btype.Value_mode.regional_to_global_alloc mode in
  transl_alloc_mode loc alloc_mode

let transl_exp_mode e = transl_value_mode e.exp_loc e.exp_mode
let transl_pat_mode p = transl_value_mode p.pat_loc p.pat_mode

let transl_apply_position position =
  match position with
  | Nontail -> Rc_normal
  | Tail -> Rc_close_at_apply

let may_allocate_in_region lam =
  let rec loop = function
    | Lvar _ | Lconst _ -> ()

    | Lfunction {mode=Alloc_heap} -> ()
    | Lfunction {mode=Alloc_local} -> raise Exit

    | Lapply {ap_mode=Alloc_local}
    | Lsend (_,_,_,_,_,Alloc_local,_) -> raise Exit

    | Lprim (prim, args, _) ->
       begin match Lambda.primitive_may_allocate prim with
       | Some Alloc_local -> raise Exit
       | None | Some Alloc_heap ->
          List.iter loop args
       end
    | Lregion _body ->
       (* [_body] might do local allocations, but not in the current region *)
       ()
    | Lwhile (cond, _body) ->
       (* [_body] already has a region *)
       loop cond
    | Lfor (_var, l, u, _dir, _body) ->
       (* [_body] already has a region *)
       loop l; loop u

    | ( Lapply _ | Llet _ | Lletrec _ | Lswitch _ | Lstringswitch _
      | Lstaticraise _ | Lstaticcatch _ | Ltrywith _
      | Lifthenelse _ | Lsequence _ | Lassign _ | Lsend _
      | Levent _ | Lifused _) as lam ->
       Lambda.iter_head_constructor loop lam
  in
  match loop lam with
  | () -> false
  | exception Exit -> true

let maybe_region lam =
  let rec remove_tail_markers = function
    | Lapply ({ap_region_close = Rc_close_at_apply} as ap) ->
       Lapply ({ap with ap_region_close = Rc_normal})
    | Lsend (k, lmet, lobj, largs, Rc_close_at_apply, mode, loc) ->
       Lsend (k, lmet, lobj, largs, Rc_normal, mode, loc)
    | Lregion _ as lam -> lam
    | lam ->
       Lambda.shallow_map ~tail:remove_tail_markers ~non_tail:Fun.id lam
  in
  if may_allocate_in_region lam then Lregion lam
  else remove_tail_markers lam

(* Push the default values under the functional abstractions *)
(* Also push bindings of module patterns, since this sound *)

type binding =
  | Bind_value of value_binding list
  | Bind_module of Ident.t * string option loc * module_presence * module_expr

let rec push_defaults loc bindings cases partial =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc =
               Texp_function { arg_label; param; cases; partial; region }}
        as exp}] ->
      let cases = push_defaults exp.exp_loc bindings cases partial in
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp with exp_desc = Texp_function { arg_label; param; cases;
          partial; region; }}}]
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{Parsetree.attr_name = {txt="#default"};_}];
             exp_desc = Texp_let
               (Nonrecursive, binds, ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (Bind_value binds :: bindings)
                   [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                   partial
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{Parsetree.attr_name = {txt="#modulepat"};_}];
             exp_desc = Texp_letmodule
               (Some id, name, pres, mexpr,
                ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (Bind_module (id, name, pres, mexpr) :: bindings)
                   [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                   partial
  | [case] ->
      let exp =
        List.fold_left
          (fun exp binds ->
            {exp with exp_desc =
             match binds with
             | Bind_value binds -> Texp_let(Nonrecursive, binds, exp)
             | Bind_module (id, name, pres, mexpr) ->
                 Texp_letmodule (Some id, name, pres, mexpr, exp)})
          case.c_rhs bindings
      in
      [{case with c_rhs=exp}]
  | {c_lhs=pat; c_rhs=exp; c_guard=_} :: _ when bindings <> [] ->
      let param = Typecore.name_cases "param" cases in
      let desc =
        {val_type = pat.pat_type; val_kind = Val_reg;
         val_attributes = []; Types.val_loc = Location.none;
         val_uid = Types.Uid.internal_not_actually_unique; }
      in
      let env = Env.add_value param desc exp.exp_env in
      let name = Ident.name param in
      let exp =
        let cases =
          let pure_case ({c_lhs; _} as case) =
            {case with c_lhs = as_computation_pattern c_lhs} in
          List.map pure_case cases in
        { exp with exp_loc = loc; exp_env = env; exp_desc =
          Texp_match
            ({exp with exp_type = pat.pat_type; exp_env = env; exp_desc =
              Texp_ident
                (Path.Pident param, mknoloc (Longident.Lident name),
                 desc, Id_value)},
             cases, partial) }
      in
      push_defaults loc bindings
        [{c_lhs={pat with pat_desc = Tpat_var (param, mknoloc name)};
          c_guard=None; c_rhs=exp}]
        Total
  | _ ->
      cases

(* Insertion of debugging events *)

let event_before ~scopes exp lam =
  Translprim.event_before (of_location ~scopes exp.exp_loc) exp lam

let event_after ~scopes exp lam =
  Translprim.event_after (of_location ~scopes exp.exp_loc) exp lam

let event_function ~scopes exp lam =
  if !Clflags.debug && not !Clflags.native_code then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = of_location ~scopes exp.exp_loc;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = exp.exp_env}))
  else
    lam None

(* Assertions *)

let assert_failed ~scopes exp =
  let slot =
    transl_extension_path Loc_unknown
      Env.initial_safe_string Predef.path_assert_failure
  in
  let loc = exp.exp_loc in
  let (fname, line, char) =
    Location.get_pos_info loc.Location.loc_start
  in
  let loc = of_location ~scopes exp.exp_loc in
  Lprim(Praise Raise_regular, [event_after ~scopes exp
    (Lprim(Pmakeblock(0, Immutable, None, Alloc_heap),
          [slot;
           Lconst(Const_block(0,
              [Const_base(Const_string (fname, exp.exp_loc, None));
               Const_base(Const_int line);
               Const_base(Const_int char)]))], loc))], loc)
;;

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* Translation of expressions *)

let rec iter_exn_names f pat =
  match pat.pat_desc with
  | Tpat_var (id, _) -> f id
  | Tpat_alias (p, id, _) ->
      f id;
      iter_exn_names f p
  | _ -> ()

let transl_ident loc env ty path desc kind =
  match desc.val_kind, kind with
  | Val_prim p, Id_prim pmode ->
      let poly_mode = transl_alloc_mode (to_location loc) pmode in
      Translprim.transl_primitive loc p env ty ~poly_mode (Some path)
  | Val_anc _, Id_value ->
      raise(Error(to_location loc, Free_super_var))
  | (Val_reg | Val_self _), Id_value ->
      transl_value_path loc env path
  |  _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"

let can_apply_primitive loc p pmode pos args =
  let is_omitted = function
    | Arg _ -> false
    | Omitted _ -> true
  in
  if List.exists (fun (_, arg) -> is_omitted arg) args then false
  else begin
    let nargs = List.length args in
    if nargs = p.prim_arity then true
    else if nargs < p.prim_arity then false
    else if pos = Typedtree.Nontail then true
    else begin
      let return_mode = Ctype.prim_mode pmode p.prim_native_repr_res in
      (transl_alloc_mode loc return_mode = Alloc_heap)
    end
  end

let rec transl_exp ~scopes e =
  transl_exp1 ~scopes ~in_new_scope:false e

(* ~in_new_scope tracks whether we just opened a new scope.

   We go to some trouble to avoid introducing many new anonymous function
   scopes, as `let f a b = ...` is desugared to several Pexp_fun.
*)
and transl_exp1 ~scopes ~in_new_scope e =
  List.iter (Translattribute.check_attribute e) e.exp_attributes;
  let eval_once =
    (* Whether classes for immediate objects must be cached *)
    match e.exp_desc with
      Texp_function _ | Texp_for _ | Texp_while _ -> false
    | _ -> true
  in
  if eval_once then transl_exp0 ~scopes ~in_new_scope  e else
  Translobj.oo_wrap e.exp_env true (transl_exp0 ~scopes ~in_new_scope) e

and transl_exp0 ~in_new_scope ~scopes e =
  match e.exp_desc with
  | Texp_ident(path, _, desc, kind) ->
      transl_ident (of_location ~scopes e.exp_loc)
        e.exp_env e.exp_type path desc kind
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      let body_kind = Typeopt.value_kind body.exp_env body.exp_type in
      transl_let ~scopes rec_flag pat_expr_list
        body_kind (event_before ~scopes body (transl_exp ~scopes body))
  | Texp_function { arg_label = _; param; cases; partial; region } ->
      let scopes =
        if in_new_scope then scopes
        else enter_anonymous_function ~scopes
      in
      transl_function ~scopes e param cases partial region
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p},
                                       Id_prim pmode);
                exp_type = prim_type } as funct, oargs, pos)
    when can_apply_primitive e.exp_loc p pmode pos oargs ->
      let argl, extra_args = cut p.prim_arity oargs in
      let arg_exps =
         List.map (function _, Arg x -> x | _ -> assert false) argl
      in
      let args = transl_list ~scopes arg_exps in
      let prim_exp = if extra_args = [] then Some e else None in
      let position =
        if extra_args = [] then transl_apply_position pos
        else Rc_normal
      in
      let prim_mode = transl_alloc_mode e.exp_loc pmode in
      let lam =
        Translprim.transl_primitive_application
          (of_location ~scopes e.exp_loc) p e.exp_env prim_type prim_mode
          path prim_exp args arg_exps position
      in
      if extra_args = [] then lam
      else begin
        let tailcall, funct =
          Translattribute.get_tailcall_attribute funct
        in
        let inlined, funct =
          Translattribute.get_and_remove_inlined_attribute funct
        in
        let specialised, funct =
          Translattribute.get_and_remove_specialised_attribute funct
        in
        let e = { e with exp_desc = Texp_apply(funct, oargs, pos) } in
        let position = transl_apply_position pos in
        let mode = transl_exp_mode e in
        event_after ~scopes e
          (transl_apply ~scopes ~tailcall ~inlined ~specialised ~position ~mode
             lam extra_args (of_location ~scopes e.exp_loc))
      end
  | Texp_apply(funct, oargs, position) ->
      let tailcall, funct =
        Translattribute.get_tailcall_attribute funct
      in
      let inlined, funct =
        Translattribute.get_and_remove_inlined_attribute funct
      in
      let specialised, funct =
        Translattribute.get_and_remove_specialised_attribute funct
      in
      let e = { e with exp_desc = Texp_apply(funct, oargs, position) } in
      let position = transl_apply_position position in
      let mode = transl_exp_mode e in
      event_after ~scopes e
        (transl_apply ~scopes ~tailcall ~inlined ~specialised
           ~position ~mode (transl_exp ~scopes funct)
           oargs (of_location ~scopes e.exp_loc))
  | Texp_match(arg, pat_expr_list, partial) ->
      transl_match ~scopes e arg pat_expr_list partial
  | Texp_try(body, pat_expr_list) ->
      let id = Typecore.name_cases "exn" pat_expr_list in
      let k = Typeopt.value_kind e.exp_env e.exp_type in
      Ltrywith(transl_exp ~scopes body, id,
               Matching.for_trywith ~scopes k e.exp_loc (Lvar id)
                 (transl_cases_try ~scopes pat_expr_list),
               Typeopt.value_kind e.exp_env e.exp_type)
  | Texp_tuple el ->
      let ll, shape = transl_list_with_shape ~scopes el in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable, Some shape,
                         transl_exp_mode e),
              ll,
              (of_location ~scopes e.exp_loc))
      end
  | Texp_construct(_, cstr, args) ->
      let ll, shape = transl_list_with_shape ~scopes args in
      if cstr.cstr_inlined <> None then begin match ll with
        | [x] -> x
        | _ -> assert false
      end else begin match cstr.cstr_tag with
        Cstr_constant n ->
          Lconst(const_int n)
      | Cstr_unboxed ->
          (match ll with [v] -> v | _ -> assert false)
      | Cstr_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(n, Immutable, Some shape,
                             transl_exp_mode e),
                  ll,
                  of_location ~scopes e.exp_loc)
          end
      | Cstr_extension(path, is_const) ->
          let lam = transl_extension_path
                      (of_location ~scopes e.exp_loc) e.exp_env path in
          if is_const then lam
          else
            Lprim(Pmakeblock(0, Immutable, Some (Pgenval :: shape),
                             transl_exp_mode e),
                  lam :: ll, of_location ~scopes e.exp_loc)
      end
  | Texp_extension_constructor (_, path) ->
      transl_extension_path (of_location ~scopes e.exp_loc) e.exp_env path
  | Texp_variant(l, arg) ->
      let tag = Btype.hash_variant l in
      begin match arg with
        None -> Lconst(const_int tag)
      | Some arg ->
          let lam = transl_exp ~scopes arg in
          try
            Lconst(Const_block(0, [const_int tag;
                                   extract_constant lam]))
          with Not_constant ->
            Lprim(Pmakeblock(0, Immutable, None,
                             transl_exp_mode e),
                  [Lconst(const_int tag); lam],
                  of_location ~scopes e.exp_loc)
      end
  | Texp_record {fields; representation; extended_expression} ->
      transl_record ~scopes e.exp_loc e.exp_env
        (transl_exp_mode e)
        fields representation extended_expression
  | Texp_field(arg, _, lbl) ->
      let targ = transl_exp ~scopes arg in
      let sem =
        match lbl.lbl_mut with
        | Immutable -> Reads_agree
        | Mutable -> Reads_vary
      in
      begin match lbl.lbl_repres with
          Record_regular | Record_inlined _ ->
          Lprim (Pfield (lbl.lbl_pos, sem), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_unboxed _ -> targ
        | Record_float ->
          let mode = transl_exp_mode e in
          Lprim (Pfloatfield (lbl.lbl_pos, sem, mode), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_extension _ ->
          Lprim (Pfield (lbl.lbl_pos + 1, sem), [targ],
                 of_location ~scopes e.exp_loc)
      end
  | Texp_setfield(arg, _, lbl, newval) ->
      let mode =
        let arg_mode = Btype.Value_mode.regional_to_local_alloc arg.exp_mode in
        match Btype.Alloc_mode.constrain_lower arg_mode with
        | Global -> Assignment
        | Local -> Local_assignment
      in
      let access =
        match lbl.lbl_repres with
          Record_regular
        | Record_inlined _ ->
          Psetfield(lbl.lbl_pos, maybe_pointer newval, mode)
        | Record_unboxed _ -> assert false
        | Record_float -> Psetfloatfield (lbl.lbl_pos, mode)
        | Record_extension _ ->
          Psetfield (lbl.lbl_pos + 1, maybe_pointer newval, mode)
      in
      Lprim(access, [transl_exp ~scopes arg; transl_exp ~scopes newval],
            of_location ~scopes e.exp_loc)
  | Texp_array expr_list ->
      let kind = array_kind e in
      let ll = transl_list ~scopes expr_list in
      let mode = transl_exp_mode e in
      begin try
        (* For native code the decision as to which compilation strategy to
           use is made later.  This enables the Flambda passes to lift certain
           kinds of array definitions to symbols. *)
        (* Deactivate constant optimization if array is small enough *)
        if List.length ll <= use_dup_for_constant_arrays_bigger_than
        then begin
          raise Not_constant
        end;
        (* Pduparray only works in Alloc_heap mode *)
        if mode <> Alloc_heap then raise Not_constant;
        begin match List.map extract_constant ll with
        | exception Not_constant when kind = Pfloatarray ->
            (* We cannot currently lift [Pintarray] arrays safely in Flambda
               because [caml_modify] might be called upon them (e.g. from
               code operating on polymorphic arrays, or functions such as
               [caml_array_blit].
               To avoid having different Lambda code for
               bytecode/Closure vs.  Flambda, we always generate
               [Pduparray] here, and deal with it in [Bytegen] (or in
               the case of Closure, in [Cmmgen], which already has to
               handle [Pduparray Pmakearray Pfloatarray] in the case
               where the array turned out to be inconstant).
               When not [Pfloatarray], the exception propagates to the handler
               below. *)
            let imm_array =
              Lprim (Pmakearray (kind, Immutable, mode), ll,
                     of_location ~scopes e.exp_loc)
            in
            Lprim (Pduparray (kind, Mutable), [imm_array],
                   of_location ~scopes e.exp_loc)
        | cl ->
            let imm_array =
              match kind with
              | Paddrarray | Pintarray ->
                  Lconst(Const_block(0, cl))
              | Pfloatarray ->
                  Lconst(Const_float_array(List.map extract_float cl))
              | Pgenarray ->
                  raise Not_constant    (* can this really happen? *)
            in
            Lprim (Pduparray (kind, Mutable), [imm_array],
                   of_location ~scopes e.exp_loc)
        end
      with Not_constant ->
        Lprim(Pmakearray (kind, Mutable, mode), ll,
              of_location ~scopes e.exp_loc)
      end
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp ~scopes cond,
                  event_before ~scopes ifso (transl_exp ~scopes ifso),
                  event_before ~scopes ifnot (transl_exp ~scopes ifnot),
                  Typeopt.value_kind e.exp_env e.exp_type)
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp ~scopes cond,
                  event_before ~scopes ifso (transl_exp ~scopes ifso),
                  lambda_unit,
                  Pintval (* unit *))
  | Texp_sequence(expr1, expr2) ->
      Lsequence(transl_exp ~scopes expr1,
                event_before ~scopes expr2 (transl_exp ~scopes expr2))
  | Texp_while(cond, body) ->
      Lwhile(maybe_region (transl_exp ~scopes cond),
             event_before ~scopes body (maybe_region (transl_exp ~scopes body)))
  | Texp_arr_comprehension (body, blocks) ->
    (*One block consists of comprehension statements connected by "and".*)
    let loc = of_location ~scopes e.exp_loc in
    let array_kind = Typeopt.array_kind e in
    Translcomprehension.transl_arr_comprehension
      body blocks ~array_kind ~scopes ~loc ~transl_exp
  | Texp_list_comprehension (body, blocks) ->
    let loc = of_location ~scopes e.exp_loc in
    Translcomprehension.transl_list_comprehension
      body blocks ~scopes ~loc ~transl_exp
  | Texp_for(param, _, low, high, dir, body) ->
      Lfor(param, transl_exp ~scopes low,
           transl_exp ~scopes high, dir,
           event_before ~scopes body (maybe_region (transl_exp ~scopes body)))
  | Texp_send(_, _, Some exp, _) -> transl_exp ~scopes exp
  | Texp_send(expr, met, None, pos) ->
      let obj = transl_exp ~scopes expr in
      let loc = of_location ~scopes e.exp_loc in
      let pos = transl_apply_position pos in
      let mode = transl_exp_mode e in
      let lam =
        match met with
          Tmeth_val id -> Lsend (Self, Lvar id, obj, [], pos, mode, loc)
        | Tmeth_name nm ->
            let (tag, cache) = Translobj.meth obj nm in
            let kind = if cache = [] then Public else Cached in
            Lsend (kind, tag, obj, cache, pos, mode, loc)
      in
      event_after ~scopes e lam
  | Texp_new (cl, {Location.loc=loc}, _, pos) ->
      let loc = of_location ~scopes loc in
      let pos = transl_apply_position pos in
      Lapply{
        ap_loc=loc;
        ap_func=
          Lprim(Pfield (0, Reads_vary),
              [transl_class_path loc e.exp_env cl], loc);
        ap_args=[lambda_unit];
        ap_region_close=pos;
        ap_mode=Alloc_heap;
        ap_tailcall=Default_tailcall;
        ap_inlined=Default_inlined;
        ap_specialised=Default_specialise;
        ap_probe=None;
      }
  | Texp_instvar(path_self, path, _) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let var = transl_value_path loc e.exp_env path in
      Lprim(Pfield_computed Reads_vary, [self; var], loc)
  | Texp_setinstvar(path_self, path, _, expr) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let var = transl_value_path loc e.exp_env path in
      transl_setinstvar ~scopes loc self var expr
  | Texp_override(path_self, modifs) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let cpy = Ident.create_local "copy" in
      Llet(Strict, Pgenval, cpy,
           Lapply{
             ap_loc=Loc_unknown;
             ap_func=Translobj.oo_prim "copy";
             ap_args=[self];
             ap_region_close=Rc_normal;
             ap_mode=Alloc_heap;
             ap_tailcall=Default_tailcall;
             ap_inlined=Default_inlined;
             ap_specialised=Default_specialise;
             ap_probe=None;
           },
           List.fold_right
             (fun (path, _, expr) rem ->
               let var = transl_value_path loc e.exp_env path in
                Lsequence(transl_setinstvar ~scopes Loc_unknown
                            (Lvar cpy) var expr, rem))
             modifs
             (Lvar cpy))
  | Texp_letmodule(None, loc, Mp_present, modl, body) ->
      let lam = !transl_module ~scopes Tcoerce_none None modl in
      Lsequence(Lprim(Pignore, [lam], of_location ~scopes loc.loc),
                transl_exp ~scopes body)
  | Texp_letmodule(Some id, loc, Mp_present, modl, body) ->
      let defining_expr =
        let mod_scopes = enter_module_definition ~scopes id in
        let lam = !transl_module ~scopes:mod_scopes Tcoerce_none None modl in
        Levent (lam, {
          lev_loc = of_location ~scopes loc.loc;
          lev_kind = Lev_module_definition id;
          lev_repr = None;
          lev_env = Env.empty;
        })
      in
      Llet(Strict, Pgenval, id, defining_expr, transl_exp ~scopes body)
  | Texp_letmodule(_, _, Mp_absent, _, body) ->
      transl_exp ~scopes body
  | Texp_letexception(cd, body) ->
      Llet(Strict, Pgenval,
           cd.ext_id, transl_extension_constructor ~scopes e.exp_env None cd,
           transl_exp ~scopes body)
  | Texp_pack modl ->
      !transl_module ~scopes Tcoerce_none None modl
  | Texp_assert {exp_desc=Texp_construct(_, {cstr_name="false"}, _)} ->
      assert_failed ~scopes e
  | Texp_assert (cond) ->
      if !Clflags.noassert
      then lambda_unit
      else begin
        Lifthenelse
          (transl_exp ~scopes cond,
           lambda_unit, 
           assert_failed ~scopes e,
           Pintval (* unit *))
      end
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      assert (transl_exp_mode e = Alloc_heap);
      begin match Typeopt.classify_lazy_argument e with
      | `Constant_or_function ->
        (* A constant expr (of type <> float if [Config.flat_float_array] is
           true) gets compiled as itself. *)
         transl_exp ~scopes e
      | `Float_that_cannot_be_shortcut ->
          (* We don't need to wrap with Popaque: this forward
             block will never be shortcutted since it points to a float
             and Config.flat_float_array is true. *)
         Lprim(Pmakeblock(Obj.forward_tag, Immutable, None,
                          Alloc_heap),
                [transl_exp ~scopes e], of_location ~scopes e.exp_loc)
      | `Identifier `Forward_value ->
         (* CR-someday mshinwell: Consider adding a new primitive
            that expresses the construction of forward_tag blocks.
            We need to use [Popaque] here to prevent unsound
            optimisation in Flambda, but the concept of a mutable
            block doesn't really match what is going on here.  This
            value may subsequently turn into an immediate... *)
         Lprim (Popaque,
                [Lprim(Pmakeblock(Obj.forward_tag, Immutable, None,
                                  Alloc_heap),
                       [transl_exp ~scopes e],
                       of_location ~scopes e.exp_loc)],
                of_location ~scopes e.exp_loc)
      | `Identifier `Other ->
         transl_exp ~scopes e
      | `Other ->
         (* other cases compile to a lazy block holding a function *)
         let scopes = enter_lazy ~scopes in
         let fn = Lfunction {kind = Curried {nlocal=0};
                             params= [Ident.create_local "param", Pgenval];
                             return = Pgenval;
                             attr = default_function_attribute;
                             loc = of_location ~scopes e.exp_loc;
                             mode = Alloc_heap;
                             region = true;
                             body = maybe_region (transl_exp ~scopes e)} in
          Lprim(Pmakeblock(Config.lazy_tag, Mutable, None,
                           Alloc_heap), [fn],
                of_location ~scopes e.exp_loc)
      end
  | Texp_object (cs, meths) ->
      let cty = cs.cstr_type in
      let cl = Ident.create_local "object" in
      !transl_object ~scopes cl meths
        { cl_desc = Tcl_structure cs;
          cl_loc = e.exp_loc;
          cl_type = Cty_signature cty;
          cl_env = e.exp_env;
          cl_attributes = [];
         }
  | Texp_letop{let_; ands; param; body; partial} ->
      event_after ~scopes e
        (transl_letop ~scopes e.exp_loc e.exp_env let_ ands param body partial)
  | Texp_unreachable ->
      raise (Error (e.exp_loc, Unreachable_reached))
  | Texp_open (od, e) ->
      let pure = pure_module od.open_expr in
      (* this optimization shouldn't be needed because Simplif would
          actually remove the [Llet] when it's not used.
          But since [scan_used_globals] runs before Simplif, we need to
          do it. *)
      begin match od.open_bound_items with
      | [] when pure = Alias -> transl_exp ~scopes e
      | _ ->
          let oid = Ident.create_local "open" in
          let body, _ =
            List.fold_left (fun (body, pos) id ->
              Llet(Alias, Pgenval, id,
                   Lprim(mod_field pos, [Lvar oid],
                         of_location ~scopes od.open_loc), body),
              pos + 1
            ) (transl_exp ~scopes e, 0)
              (bound_value_identifiers od.open_bound_items)
          in
          Llet(pure, Pgenval, oid,
               !transl_module ~scopes Tcoerce_none None od.open_expr, body)
      end
  | Texp_probe {name; handler=exp} ->
    if !Clflags.native_code && !Clflags.probes then begin
      let lam = transl_exp ~scopes exp in
      let map =
        Ident.Set.fold (fun v acc -> Ident.Map.add v (Ident.rename v) acc)
          (free_variables lam)
          Ident.Map.empty
      in
      let arg_idents, param_idents = Ident.Map.bindings map |> List.split in
      let body = Lambda.rename map lam in
      let attr =
        { inline = Never_inline;
          specialise = Always_specialise;
          local = Never_local;
          is_a_functor = false;
          stub = false;
        } in
      let funcid = Ident.create_local ("probe_handler_" ^ name) in
      let handler =
        let scopes = enter_value_definition ~scopes funcid in
        { kind = Curried {nlocal=0};
          params = List.map (fun v -> v, Pgenval) param_idents;
          return = Pgenval;
          body;
          loc = of_location ~scopes exp.exp_loc;
          attr;
          mode=Alloc_heap;
          region=true;
        }
      in
      let app =
        { ap_func = Lvar funcid;
          ap_args = List.map (fun id -> Lvar id) arg_idents;
          ap_region_close = Rc_normal;
          ap_mode = Alloc_heap;
          ap_loc = of_location e.exp_loc ~scopes;
          ap_tailcall = Default_tailcall;
          ap_inlined = Never_inlined;
          ap_specialised = Always_specialise;
          ap_probe = Some {name};
        }
      in
      begin match Config.flambda || Config.flambda2 with
      | true ->
          Llet(Strict, Pgenval, funcid, Lfunction handler, Lapply app)
      | false ->
        (* Needs to be lifted to top level manually here,
           because functions that contain other function declarations
           are not inlined by Closure. For example, adding a probe into
           the body of function foo will prevent foo from being inlined
           into another function. *)
        probe_handlers := (funcid, Lfunction handler)::!probe_handlers;
        Lapply app
      end
    end else begin
      lambda_unit
    end
  | Texp_probe_is_enabled {name} ->
    if !Clflags.native_code && !Clflags.probes then
      Lprim(Pprobe_is_enabled {name}, [], of_location ~scopes e.exp_loc)
    else
      lambda_unit

and pure_module m =
  match m.mod_desc with
    Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> pure_module m
  | _ -> Strict

and transl_list ~scopes expr_list =
  List.map (transl_exp ~scopes) expr_list

and transl_list_with_shape ~scopes expr_list =
  let transl_with_shape e =
    let shape = Typeopt.value_kind e.exp_env e.exp_type in
    transl_exp ~scopes e, shape
  in
  List.split (List.map transl_with_shape expr_list)

and transl_guard ~scopes guard rhs =
  let kind = Typeopt.value_kind rhs.exp_env rhs.exp_type in
  let expr = event_before ~scopes rhs (transl_exp ~scopes rhs) in
  match guard with
  | None -> expr
  | Some cond ->
      event_before ~scopes cond
        (Lifthenelse(transl_exp ~scopes cond, expr, staticfail, kind))

and transl_case ~scopes {c_lhs; c_guard; c_rhs} =
  c_lhs, transl_guard ~scopes c_guard c_rhs

and transl_cases ~scopes cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case ~scopes) cases

and transl_case_try ~scopes {c_lhs; c_guard; c_rhs} =
  iter_exn_names Translprim.add_exception_ident c_lhs;
  Misc.try_finally
    (fun () -> c_lhs, transl_guard ~scopes c_guard c_rhs)
    ~always:(fun () ->
        iter_exn_names Translprim.remove_exception_ident c_lhs)

and transl_cases_try ~scopes cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case_try ~scopes) cases

and transl_tupled_cases ~scopes patl_expr_list =
  let patl_expr_list =
    List.filter (fun (_,_,e) -> e.exp_desc <> Texp_unreachable)
      patl_expr_list in
  List.map (fun (patl, guard, expr) -> (patl, transl_guard ~scopes guard expr))
    patl_expr_list

and transl_apply ~scopes
      ?(tailcall=Default_tailcall)
      ?(inlined = Default_inlined)
      ?(specialised = Default_specialise)
      ?(position=Rc_normal)
      ?(mode=Alloc_heap)
      lam sargs loc
  =
  let lapply funct args loc pos mode =
    match funct, pos with
    | Lsend((Self | Public) as k, lmet, lobj, [], _, _, _), _ ->
        Lsend(k, lmet, lobj, args, pos, mode, loc)
    | Lsend(Cached, lmet, lobj, ([_; _] as largs), _, _, _), _ ->
        Lsend(Cached, lmet, lobj, largs @ args, pos, mode, loc)
    | Lsend(k, lmet, lobj, largs, Rc_normal, _, _), Rc_normal ->
        Lsend(k, lmet, lobj, largs @ args, pos, mode, loc)
    | Levent(
      Lsend((Self | Public) as k, lmet, lobj, [], _, _, _), _), _ ->
        Lsend(k, lmet, lobj, args, pos, mode, loc)
    | Levent(
      Lsend(Cached, lmet, lobj, ([_; _] as largs), _, _, _), _), _ ->
        Lsend(Cached, lmet, lobj, largs @ args, pos, mode, loc)
    | Levent(
      Lsend(k, lmet, lobj, largs, Rc_normal, _, _), _), Rc_normal ->
        Lsend(k, lmet, lobj, largs @ args, pos, mode, loc)
    | Lapply ({ ap_region_close = Rc_normal } as ap), Rc_normal ->
        Lapply
          {ap with ap_args = ap.ap_args @ args; ap_loc = loc;
                   ap_region_close = pos; ap_mode = mode}
    | lexp, _ ->
        Lapply {
          ap_loc=loc;
          ap_func=lexp;
          ap_args=args;
          ap_region_close=pos;
          ap_mode=mode;
          ap_tailcall=tailcall;
          ap_inlined=inlined;
          ap_specialised=specialised;
          ap_probe=None;
        }
  in
  let rec build_apply lam args loc pos ap_mode = function
    | Omitted { mode_closure; mode_arg; mode_ret } :: l ->
        assert (pos = Rc_normal);
        let defs = ref [] in
        let protect name lam =
          match lam with
            Lvar _ | Lconst _ -> lam
          | _ ->
              let id = Ident.create_local name in
              defs := (id, lam) :: !defs;
              Lvar id
        in
        let lam =
          if args = [] then lam else lapply lam (List.rev args) loc pos ap_mode
        in
        let handle = protect "func" lam in
        let l =
          List.map
            (fun arg ->
               match arg with
               | Omitted _ -> arg
               | Arg arg -> Arg (protect "arg" arg))
            l
        in
        let id_arg = Ident.create_local "param" in
        let body =
          let loc = map_scopes enter_partial_or_eta_wrapper loc in
          let sloc = to_location loc in
          let mode = transl_alloc_mode sloc mode_closure in
          let arg_mode = transl_alloc_mode sloc mode_arg in
          let ret_mode = transl_alloc_mode sloc mode_ret in
          let body = build_apply handle [Lvar id_arg] loc Rc_normal ret_mode l in
          let nlocal =
            match join_mode mode (join_mode arg_mode ret_mode) with
            | Alloc_local -> 1
            | Alloc_heap -> 0
          in
          let region =
            match ret_mode with
            | Alloc_local -> false
            | Alloc_heap -> true
          in
          Lfunction{kind = Curried {nlocal}; params = [id_arg, Pgenval];
                    return = Pgenval; body; mode; region;
                    attr = default_stub_attribute; loc = loc}
        in
        List.fold_right
          (fun (id, lam) body -> Llet(Strict, Pgenval, id, lam, body))
          !defs body
    | Arg arg :: l -> build_apply lam (arg :: args) loc pos ap_mode l
    | [] -> lapply lam (List.rev args) loc pos ap_mode
  in
  let args =
    List.map
      (fun (_, arg) ->
         match arg with
         | Omitted _ as arg -> arg
         | Arg exp -> Arg (transl_exp ~scopes exp))
      sargs
  in
  build_apply lam [] loc position mode args

and transl_curried_function
      ~scopes loc return
      repr ~mode ~region partial (param:Ident.t) cases =
  let max_arity = Lambda.max_arity () in
  let rec loop ~scopes loc return ~arity ~mode ~region partial
            (param:Ident.t) cases =
    match cases with
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp_desc =
                 Texp_function
                   { arg_label = _; param = param'; cases = cases';
                     partial = partial'; region = region' };
               exp_env; exp_type; exp_loc; exp_mode}}]
      when arity < max_arity ->
      let arg_mode = transl_pat_mode pat in
      let curry_mode = transl_value_mode exp_loc exp_mode in
      (* Lfunctions must have local returns after the first local arg/ret *)
      if not (sub_mode mode curry_mode && sub_mode arg_mode curry_mode) then
        (* Cannot curry here *)
        transl_tupled_function ~scopes ~arity ~mode ~region
          loc return repr partial param cases
      else if Parmatch.inactive ~partial pat
      then
        let kind = value_kind pat.pat_env pat.pat_type in
        let return_kind = function_return_value_kind exp_env exp_type in
        let ((fnkind, params, return, region), body) =
          loop ~scopes exp_loc return_kind
            ~arity:(arity + 1) ~mode:curry_mode ~region:region'
            partial' param' cases'
        in
        let fnkind =
          match curry_mode, fnkind with
          | _, Tupled ->
             (* arity > 1 prevents this *)
             assert false
          | Alloc_heap, (Curried _ as c) -> c
          | Alloc_local, Curried {nlocal} ->
             (* all subsequent curried arrows should be local *)
             assert (nlocal = List.length params);
             Curried {nlocal = nlocal + 1}
        in
        ((fnkind, (param, kind) :: params, return, region),
         Matching.for_function ~scopes return_kind loc None (Lvar param)
           [pat, body] partial)
      else begin
        begin match partial with
        | Total ->
          Location.prerr_warning pat.pat_loc
            Match_on_mutable_state_prevent_uncurry
        | Partial -> ()
        end;
        transl_tupled_function ~scopes ~arity ~mode ~region
          loc return repr partial param cases
      end
    | cases ->
      transl_tupled_function ~scopes ~arity ~mode ~region
        loc return repr partial param cases
  in
  loop ~scopes loc return ~arity:1 ~mode ~region partial param cases

and transl_tupled_function
      ~scopes ~arity ~mode ~region loc return
      repr partial (param:Ident.t) cases =
  match cases with
  | {c_lhs={pat_desc = Tpat_tuple pl; pat_loc; pat_mode }} :: _
    when !Clflags.native_code
      && arity = 1
      && mode = Alloc_heap
      && transl_value_mode pat_loc pat_mode = Alloc_heap
      && List.length pl <= (Lambda.max_arity ()) ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun {c_lhs; c_guard; c_rhs} ->
              (Matching.flatten_pattern size c_lhs, c_guard, c_rhs))
            cases in
        let kinds =
          (* All the patterns might not share the same types. We must take the
             union of the patterns types *)
          match pats_expr_list with
          | [] -> assert false
          | (pats, _, _) :: cases ->
              let first_case_kinds =
                List.map (fun pat -> value_kind pat.pat_env pat.pat_type) pats
              in
              List.fold_left
                (fun kinds (pats, _, _) ->
                   List.map2 (fun kind pat ->
                       value_kind_union kind
                         (value_kind pat.pat_env pat.pat_type))
                     kinds pats)
                first_case_kinds cases
        in
        let tparams =
          List.map (fun kind -> Ident.create_local "param", kind) kinds
        in
        let params = List.map fst tparams in
        ((Tupled, tparams, return, region),
         Matching.for_tupled_function ~scopes loc return params
           (transl_tupled_cases ~scopes pats_expr_list) partial)
    with Matching.Cannot_flatten ->
      transl_function0 ~scopes loc ~mode ~region
        return repr partial param cases
      end
  | _ -> transl_function0 ~scopes loc ~mode ~region
           return repr partial param cases

and transl_function0
      ~scopes loc ~mode ~region return
      repr partial (param:Ident.t) cases =
    let arg_mode, kind =
      match cases with
      | [] ->
        (* With Camlp4, a pattern matching might be empty *)
        Alloc_heap, Pgenval
      | {c_lhs=pat} :: other_cases ->
        (* All the patterns might not share the same types. We must take the
           union of the patterns types *)
        let arg_mode = transl_pat_mode pat in
        arg_mode,
        List.fold_left (fun k {c_lhs=pat} ->
          assert (transl_pat_mode pat = arg_mode);
          Typeopt.value_kind_union k
            (value_kind pat.pat_env pat.pat_type))
          (value_kind pat.pat_env pat.pat_type) other_cases
    in
    let body =
      Matching.for_function ~scopes return loc repr (Lvar param)
        (transl_cases ~scopes cases) partial
    in
    let region = region || not (may_allocate_in_region body) in
    let nlocal =
      if not region then 1
      else match join_mode mode arg_mode with
        | Alloc_local -> 1
        | Alloc_heap -> 0
    in
    ((Curried {nlocal}, [param, kind], return, region), body)

and transl_function ~scopes e param cases partial region =
  let mode = transl_exp_mode e in
  let ((kind, params, return, region), body) =
    event_function ~scopes e
      (function repr ->
         let pl = push_defaults e.exp_loc [] cases partial in
         let return_kind = function_return_value_kind e.exp_env e.exp_type in
         transl_curried_function ~scopes e.exp_loc return_kind
           repr ~mode ~region partial param pl)
  in
  let attr = default_function_attribute in
  let loc = of_location ~scopes e.exp_loc in
  let body = if region then maybe_region body else body in
  let lfunc = {kind; params; return; body; attr; loc; mode; region} in
  Lambda.check_lfunction lfunc;
  let lam = Lfunction lfunc in
  Translattribute.add_function_attributes lam e.exp_loc e.exp_attributes

(* Like transl_exp, but used when a new scope was just introduced. *)
and transl_scoped_exp ~scopes expr =
  transl_exp1 ~scopes ~in_new_scope:true expr

(* Decides whether a pattern binding should introduce a new scope. *)
and transl_bound_exp ~scopes ~in_structure pat expr =
  let should_introduce_scope =
    match expr.exp_desc with
    | Texp_function _ -> true
    | _ when in_structure -> true
    | _ -> false in
  match pat_bound_idents pat with
  | (id :: _) when should_introduce_scope ->
     transl_scoped_exp ~scopes:(enter_value_definition ~scopes id) expr
  | _ -> transl_exp ~scopes expr

(*
  Notice: transl_let consumes (ie compiles) its pat_expr_list argument,
  and returns a function that will take the body of the lambda-let construct.
  This complication allows choosing any compilation order for the
  bindings and body of let constructs.
*)
and transl_let ~scopes ?(add_regions=false) ?(in_structure=false)
               rec_flag pat_expr_list body_kind =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          fun body -> body
      | {vb_pat=pat; vb_expr=expr; vb_attributes=attr; vb_loc} :: rem ->
          let lam = transl_bound_exp ~scopes ~in_structure pat expr in
          let lam = Translattribute.add_function_attributes lam vb_loc attr in
          let lam = if add_regions then maybe_region lam else lam in
          let mk_body = transl rem in
          fun body ->
            Matching.for_let ~scopes pat.pat_loc lam pat body_kind (mk_body body)
      in
      transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun {vb_pat=pat} -> match pat.pat_desc with
              Tpat_var (id,_) -> id
            | Tpat_alias ({pat_desc=Tpat_any}, id,_) -> id
            | _ -> assert false)
        pat_expr_list in
      let transl_case {vb_expr=expr; vb_attributes; vb_loc; vb_pat} id =
        let lam = transl_bound_exp ~scopes ~in_structure vb_pat expr in
        let lam =
          Translattribute.add_function_attributes lam vb_loc vb_attributes
        in
        let lam = if add_regions then maybe_region lam else lam in
        begin match transl_exp_mode expr, lam with
        | Alloc_heap, _ -> ()
        | Alloc_local, Lfunction _ -> ()
        | _ -> Misc.fatal_error "transl_let: local recursive non-function"
        end;
        (id, lam) in
      let lam_bds = List.map2 transl_case pat_expr_list idlist in
      fun body -> Lletrec(lam_bds, body)

and transl_setinstvar ~scopes loc self var expr =
  Lprim(Psetfield_computed (maybe_pointer expr, Assignment),
    [self; var; transl_exp ~scopes expr], loc)

and transl_record ~scopes loc env mode fields repres opt_init_expr =
  let size = Array.length fields in
  (* Determine if there are "enough" fields (only relevant if this is a
     functional-style record update *)
  let no_init = match opt_init_expr with None -> true | _ -> false in
  if no_init || size < Config.max_young_wosize || mode = Lambda.Alloc_local
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let init_id = Ident.create_local "init" in
    let lv =
      Array.mapi
        (fun i (lbl, definition) ->
           match definition with
           | Kept typ ->
               let field_kind = value_kind env typ in
               let sem =
                 match lbl.lbl_mut with
                 | Immutable -> Reads_agree
                 | Mutable -> Reads_vary
               in
               let access =
                 match repres with
                   Record_regular | Record_inlined _ -> Pfield (i, sem)
                 | Record_unboxed _ -> assert false
                 | Record_extension _ -> Pfield (i + 1, sem)
                 | Record_float ->
                    (* This allocation is always deleted,
                       so it's simpler to leave it Alloc_heap *)
                    Pfloatfield (i, sem, Alloc_heap) in
               Lprim(access, [Lvar init_id],
                     of_location ~scopes loc),
               field_kind
           | Overridden (_lid, expr) ->
               let field_kind = value_kind expr.exp_env expr.exp_type in
               transl_exp ~scopes expr, field_kind)
        fields
    in
    let ll, shape = List.split (Array.to_list lv) in
    let mut : Lambda.mutable_flag =
      if Array.exists (fun (lbl, _) -> lbl.lbl_mut = Asttypes.Mutable) fields
      then Mutable
      else Immutable in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
        | Record_regular -> Lconst(Const_block(0, cl))
        | Record_inlined tag -> Lconst(Const_block(tag, cl))
        | Record_unboxed _ -> Lconst(match cl with [v] -> v | _ -> assert false)
        | Record_float ->
            Lconst(Const_float_block(List.map extract_float cl))
        | Record_extension _ ->
            raise Not_constant
      with Not_constant ->
        let loc = of_location ~scopes loc in
        match repres with
          Record_regular ->
            Lprim(Pmakeblock(0, mut, Some shape, mode), ll, loc)
        | Record_inlined tag ->
            Lprim(Pmakeblock(tag, mut, Some shape, mode), ll, loc)
        | Record_unboxed _ -> (match ll with [v] -> v | _ -> assert false)
        | Record_float ->
            Lprim(Pmakefloatblock (mut, mode), ll, loc)
        | Record_extension path ->
            let slot = transl_extension_path loc env path in
            Lprim(Pmakeblock(0, mut, Some (Pgenval :: shape), mode),
                  slot :: ll, loc)
    in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, Pgenval, init_id,
                             transl_exp ~scopes init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    let copy_id = Ident.create_local "newrecord" in
    let update_field cont (lbl, definition) =
      match definition with
      | Kept _type -> cont
      | Overridden (_lid, expr) ->
          let upd =
            match repres with
              Record_regular
            | Record_inlined _ ->
                Psetfield(lbl.lbl_pos, maybe_pointer expr, Assignment)
            | Record_unboxed _ -> assert false
            | Record_float -> Psetfloatfield (lbl.lbl_pos, Assignment)
            | Record_extension _ ->
                Psetfield(lbl.lbl_pos + 1, maybe_pointer expr, Assignment)
          in
          Lsequence(Lprim(upd, [Lvar copy_id; transl_exp ~scopes expr],
                          of_location ~scopes loc),
                    cont)
    in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        assert (mode = Lambda.Alloc_heap); (* Pduprecord must be Alloc_heap *)
        Llet(Strict, Pgenval, copy_id,
             Lprim(Pduprecord (repres, size), [transl_exp ~scopes init_expr],
                   of_location ~scopes loc),
             Array.fold_left update_field (Lvar copy_id) fields)
    end
  end

and transl_match ~scopes e arg pat_expr_list partial =
  let kind = Typeopt.value_kind e.exp_env e.exp_type in
  let rewrite_case (val_cases, exn_cases, static_handlers as acc)
        ({ c_lhs; c_guard; c_rhs } as case) =
    if c_rhs.exp_desc = Texp_unreachable then acc else
    let val_pat, exn_pat = split_pattern c_lhs in
    match val_pat, exn_pat with
    | None, None -> assert false
    | Some pv, None ->
        let val_case =
          transl_case ~scopes { case with c_lhs = pv }
        in
        val_case :: val_cases, exn_cases, static_handlers
    | None, Some pe ->
        let exn_case = transl_case_try ~scopes { case with c_lhs = pe } in
        val_cases, exn_case :: exn_cases, static_handlers
    | Some pv, Some pe ->
        assert (c_guard = None);
        let lbl  = next_raise_count () in
        let static_raise ids =
          Lstaticraise (lbl, List.map (fun id -> Lvar id) ids)
        in
        (* Simplif doesn't like it if binders are not uniq, so we make sure to
           use different names in the value and the exception branches. *)
        let ids_full = Typedtree.pat_bound_idents_full pv in
        let ids = List.map (fun (id, _, _) -> id) ids_full in
        let ids_kinds =
          List.map (fun (id, _, ty) -> id, Typeopt.value_kind pv.pat_env ty)
            ids_full
        in
        let vids = List.map Ident.rename ids in
        let pv = alpha_pat (List.combine ids vids) pv in
        (* Also register the names of the exception so Re-raise happens. *)
        iter_exn_names Translprim.add_exception_ident pe;
        let rhs =
          Misc.try_finally
            (fun () -> event_before ~scopes c_rhs
                         (transl_exp ~scopes c_rhs))
            ~always:(fun () ->
                iter_exn_names Translprim.remove_exception_ident pe)
        in
        (pv, static_raise vids) :: val_cases,
        (pe, static_raise ids) :: exn_cases,
        (lbl, ids_kinds, rhs) :: static_handlers
  in
  let val_cases, exn_cases, static_handlers =
    let x, y, z = List.fold_left rewrite_case ([], [], []) pat_expr_list in
    List.rev x, List.rev y, List.rev z
  in
  let static_catch body val_ids handler =
    let id = Typecore.name_pattern "exn" (List.map fst exn_cases) in
    let static_exception_id = next_raise_count () in
    Lstaticcatch
      (Ltrywith (Lstaticraise (static_exception_id, body), id,
                 Matching.for_trywith ~scopes kind e.exp_loc (Lvar id) exn_cases,
                 kind),
       (static_exception_id, val_ids),
       handler,
      kind)
  in
  let classic =
    match arg, exn_cases with
    | {exp_desc = Texp_tuple argl}, [] ->
      assert (static_handlers = []);
      let mode = transl_exp_mode arg in
      Matching.for_multiple_match ~scopes kind e.exp_loc
        (transl_list ~scopes argl) mode val_cases partial
    | {exp_desc = Texp_tuple argl}, _ :: _ ->
        let val_ids =
          List.map
            (fun arg ->
               Typecore.name_pattern "val" [],
               Typeopt.value_kind arg.exp_env arg.exp_type
            )
            argl
        in
        let lvars = List.map (fun (id, _) -> Lvar id) val_ids in
        let mode = transl_exp_mode arg in
        static_catch (transl_list ~scopes argl) val_ids
          (Matching.for_multiple_match ~scopes kind e.exp_loc
             lvars mode val_cases partial)
    | arg, [] ->
      assert (static_handlers = []);
      Matching.for_function ~scopes kind e.exp_loc
        None (transl_exp ~scopes arg) val_cases partial
    | arg, _ :: _ ->
        let val_id = Typecore.name_pattern "val" (List.map fst val_cases) in
        let k = Typeopt.value_kind arg.exp_env arg.exp_type in
        static_catch [transl_exp ~scopes arg] [val_id, k]
          (Matching.for_function ~scopes kind e.exp_loc
             None (Lvar val_id) val_cases partial)
  in
  List.fold_left (fun body (static_exception_id, val_ids, handler) ->
    Lstaticcatch (body, (static_exception_id, val_ids), handler, kind)
  ) classic static_handlers

and transl_letop ~scopes loc env let_ ands param case partial =
  let rec loop prev_lam = function
    | [] -> prev_lam
    | and_ :: rest ->
        let left_id = Ident.create_local "left" in
        let right_id = Ident.create_local "right" in
        let op =
          transl_ident (of_location ~scopes and_.bop_op_name.loc) env
            and_.bop_op_type and_.bop_op_path and_.bop_op_val Id_value
        in
        let exp = transl_exp ~scopes and_.bop_exp in
        let lam =
          bind Strict right_id exp
            (Lapply{
               ap_loc = of_location ~scopes and_.bop_loc;
               ap_func = op;
               ap_args=[Lvar left_id; Lvar right_id];
               ap_region_close=Rc_normal;
               ap_mode=Alloc_heap;
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inlined;
               ap_specialised = Default_specialise;
               ap_probe=None;
             })
        in
        bind Strict left_id prev_lam (loop lam rest)
  in
  let op =
    transl_ident (of_location ~scopes let_.bop_op_name.loc) env
      let_.bop_op_type let_.bop_op_path let_.bop_op_val Id_value
  in
  let exp = loop (transl_exp ~scopes let_.bop_exp) ands in
  let func =
    let return_kind = value_kind case.c_rhs.exp_env case.c_rhs.exp_type in
    let (kind, params, return, _region), body =
      event_function ~scopes case.c_rhs
        (function repr ->
           transl_curried_function ~scopes case.c_rhs.exp_loc return_kind
             repr ~mode:Alloc_heap ~region:true partial param [case])
    in
    let attr = default_function_attribute in
    let loc = of_location ~scopes case.c_rhs.exp_loc in
    let body = maybe_region body in
    Lfunction{kind; params; return; body; attr; loc;
              mode=Alloc_heap; region=true}
  in
  Lapply{
    ap_loc = of_location ~scopes loc;
    ap_func = op;
    ap_args=[exp; func];
    ap_region_close=Rc_normal;
    ap_mode=Alloc_heap;
    ap_tailcall = Default_tailcall;
    ap_inlined = Default_inlined;
    ap_specialised = Default_specialise;
    ap_probe=None;
  }

(* Wrapper for class/module compilation,
   that can only return global values *)

let transl_exp ~scopes exp =
  maybe_region (transl_exp ~scopes exp)

let transl_let ~scopes ?in_structure rec_flag pat_expr_list =
  transl_let ~scopes ~add_regions:true ?in_structure rec_flag pat_expr_list

let transl_scoped_exp ~scopes exp =
  maybe_region (transl_scoped_exp ~scopes exp)

let transl_apply
      ~scopes ?tailcall ?inlined ?specialised ?position ?mode fn args loc =
  maybe_region (transl_apply
      ~scopes ?tailcall ?inlined ?specialised ?position ?mode fn args loc)

(* Error report *)

open Format

let report_error ppf = function
  | Free_super_var ->
      fprintf ppf
        "Ancestor names can only be used to select inherited methods"
  | Unreachable_reached ->
      fprintf ppf "Unreachable expression was reached"
  | Local_allocs_not_enabled ->
      fprintf ppf "Local allocation required but '-extension local' not enabled"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
