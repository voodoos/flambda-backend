(* TODO SP make it (or check it is made) explicit that all rules are used *)

(*

We call U the set of all paths used directly in a file:

- 1. Any path occurring in the file is in U. For example, List.map occurring in
     the file will add both List.map and List to U.
- 2. All paths for definitions in the current file are in U. So if module M = ...
     occurs in the file then M is in U. [Definitions are items of U, recorded
     with the environment in which they are bound, so that the D rules apply
     to them. Module aliases are excepted as in rule 3.]
- 3. All paths for things "defined" using include or open in the current file
     are in U. It is possible that all of these would end up in D anyway via
     other rules, but it's not entirely obvious so I've included this rule here
     just to make sure they do. [We refined the rule a bit: components brought
     in scope are put in U except module aliases for which we only register a
     substitution, to avoid loading their compilation unit. The components of
     an included structure (or an opened module expression) are handled the
     same way.]
- Note that constructors or fields only used via type-based disambiguation are
  not in U.

We call D the domain of discourse:

- 1. The paths of all the predefined types that are intended for direct use by
     users, like int, are in D.
- 2. If a path is in U then it is also in D.
- 3. If a module path is in U then all the paths of its subcomponents are in D.
- 4. If a value path is in U and its value description was written by a user -
     as opposed to being inferred - then the paths used in that description are
     in D.
- 5. If a module path is in U and its module description was written by a user -
     as opposed to being inferred - then the paths used in that description are
     in D, excluding those paths that only appear inside of a sig...end.
- 6. If a type path is in U then any paths used in its equation or
     representation are in D.
- 7. If a constructor or record field is in U then any paths used in its type
     are in D.
- 8. If a module type path is in U then any paths used in its definition are in
     D, excluding those paths that only appear inside of a sig...end.
- 9. If a class path is in U and its class description was written by a user -
     as opposed to being inferred - then of any paths used in that description
     are in D.
- 10. If a class type path is in U then any paths used in its definition are in
      D.
- 11. If a path is in D and it includes another module path within it, then that
      module path is also in D.
- 12. If a module path m in D - note D not U - is a module alias with target n
      and another path p in D includes n within it, then the path obtained by
      substituting the m for n in p is also in D.
*)

(* Only paths of some kinds can be shortened *)
let map_if_shortenable (kind : Shape.Sig_component_kind.t) v f =
  match kind with
  | Type | Module | Module_type -> f v
  | Value | Extension_constructor | Class | Class_type | Constructor | Label
  | Unboxed_label | Jkind ->
    v

(* This logging section logs a "recap" of the discourse creation: The value of
   [U] before [D.of_U] is called, and the value of [D] at the end.

   It will also display the "intermediate" U that are built during the creation
   of D (see "special rule") *)
let recap_log_section = "discourse-recap"
let { Logger.log = log_recap } = Logger.for_section recap_log_section

let trie_of_paths (d : Discourse_types.t) =
  let open Discourse_types in
  fold
    (fun (kind, path) acc ->
      map_if_shortenable kind acc @@ Path_trie.add path kind)
    d Path_trie.empty

let pp_d fmt d =
  let open Discourse_types in
  let open Format in
  let pp_sep fmt () = fprintf fmt ";@ " in
  let pp_path_set fmt set =
    fprintf fmt "@[<1>[%a]@]"
      (pp_print_list ~pp_sep (Format_doc.compat Path.print))
      (Path.Set.elements set)
  in
  let pp_substs_binding fmt (path, paths) =
    fprintf fmt "@[<2>%a ->@ %a@]"
      (Format_doc.compat Path.print)
      path pp_path_set paths
  in
  let pp_substs fmt map =
    if Path.Map.is_empty map then fprintf fmt "[]"
    else
      fprintf fmt "@[<v>[%a]@]"
        (pp_print_list ~pp_sep pp_substs_binding)
        (Path.Map.bindings map)
  in
  let pp_expandable fmt map =
    fprintf fmt "@[<1>[%a]@]"
      (pp_print_list ~pp_sep (Format_doc.compat Path.print))
      (List.map fst (Path.Map.bindings map))
  in
  fprintf fmt
    "@[<v 2>Discourse {@;\
     size = %i;@;\
     paths =@ %a;@;\
     substs =@ %a;@;\
     expandable =@ %a@;\
     <-2>}@]"
    (Path_trie.size d.paths) Path_trie.pp_seq d.paths pp_substs d.substs
    pp_expandable d.expansions

(* A more verbose section which logs every addition to U or D, along with the
   reason the element is added (U1, U2, U3, D2, ..., D12) *)
let log_section = "discourse-verbose"
let { Logger.log } = Logger.for_section log_section

open Shape.Sig_component_kind
open Discourse_types

module Item_map = Map.Make (Item)

module U = struct
  type ident_mode = Keep | Refresh

  type components_source =
    | Module_path of Path.t
    | Signature of Types.signature

  (* Origin of signature components additions *)
  type origin = Open | Include | Module_type_of | Printing

  (* The rule that recorded a [Used] event in U for printing *)
  type rule = U1 | U2

  (* We build U lazily: during typing we only log [event]s, and the actual work
     (walking signatures, looking up the environment, building the tries) only
     happens in [force], which is called by [D.of_U] the first time something
     needs to be printed. *)
  type event =
    | Used of
        { rule : rule;
          kind : Shape.Sig_component_kind.t;
          lid : Longident.t Location.loc;
          path : Path.t;
          env : Env.t
        }
        (** Rule U1: a path occurring in the file, and U2: a type, module
            type or module defined in the file *)
    | Used_constructor of
        { env : Env.t;
          lid : Longident.t Location.loc;
          discourse : Discourse_types.t
        }  (** Rule U1 for constructors (and rule D7 for their discourse) *)
    | Used_label of
        { env : Env.t;
          lid : Longident.t Location.loc;
          discourse : Discourse_types.t
        }  (** Rule U1 for labels (and rule D7 for their discourse) *)
    | Initial_discourse of Discourse_types.t
        (** Rule D1: the predefined types *)
    | Brought_in_scope of
        { env : Env.t;
          source : components_source;
          ident_mode : ident_mode;
          origin : origin
        }  (** Rule U3 *)

  type u =
    { u_paths : Env.t Item_map.t;
      substs : Discourse_types.substs;
      discourse : Path_trie.t;
      pending : event list
    }

  let pp_u fmt u =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@ " in
    let pp_u_item fmt (kind, path) =
      fprintf fmt "@[<1>{item = (%s,@ %a)}@]"
        (Shape.Sig_component_kind.to_string kind)
        (Format_doc.compat Path.print)
        path
    in
    let pp_u_paths_binding fmt (((_, path) as item), _env) =
      fprintf fmt "@[<2>%a ->@ %a@]"
        (Format_doc.compat Path.print)
        path pp_u_item item
    in
    let pp_u_paths fmt map =
      fprintf fmt "@[<v>[%a]@]"
        (pp_print_list ~pp_sep pp_u_paths_binding)
        (Item_map.bindings map)
    in
    fprintf fmt "@[<v 2>{ u_paths =@ %a;@ substs =@ %a }@]" pp_u_paths u.u_paths
      Discourse_types.pp_substs u.substs

  let add_item item env u =
    if Item_map.mem item u.u_paths then u
    else { u with u_paths = Item_map.add item env u.u_paths }

  let empty_u : u =
    { u_paths = Item_map.empty;
      substs = Path.Map.empty;
      discourse = Path_trie.empty;
      pending = []
    }
  let g = Local_store.s_ref empty_u

  (** We call U the set of all paths used directly in a file:

    1. Any path occurring in the file is in U. For example, List.map occurring in
       the file will add both List.map and List to U.
    2. All paths for definitions in the current file are in U. So if module M = ...
       occurs in the file then M is in U.
    3. All paths for things "defined" using include or open in the current file
       are in U. It is possible that all of these would end up in D anyway via
       other rules, but it's not entirely obvious so I've included this rule here
       just to make sure they do.
    - Note that constructors or fields only used via type-based disambiguation are
      not in U.
*)

  let reset () = g := empty_u

  let record_usages = Config.merlin

  (* We only log events during typing, post-processing is deferred. *)
  let record event =
    if record_usages then g := { !g with pending = event :: !g.pending }

  let merge_discourse paths u =
    { u with discourse = Path_trie.union (trie_of_paths paths) u.discourse }

  let fold_on_path_segments ~init ~kind ~f path =
    let rec aux acc kind (path : Path.t) =
      let acc = f acc kind path in
      match path with
      | Pident _ -> acc
      | Pdot (p, _) -> aux acc Module p
      | Papply (p1, p2) ->
        let acc = aux acc Module p2 in
        aux acc Module p1
      | Pextra_ty (p, Pcstr_ty _) -> aux acc Type p
      | Pextra_ty (p, Pext_ty) -> aux acc Extension_constructor p
      | Pextra_ty (p, Punboxed_ty) -> aux acc Type p
    in
    aux init kind path

  let add_subst substs path ~by:by_path =
    log ~title:"subst" "subst: %a -> %a (open/alias defined in file)" Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) path)
      Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) by_path);
    if Path.same path by_path then substs
    else
      let substs =
        Path.Map.update path
          (function
            | None -> Some (Path.Set.singleton by_path)
            | Some paths -> Some (Path.Set.add by_path paths))
          substs
      in
      substs

  let add_subst_u path ~by:path' u =
    { u with substs = add_subst u.substs path ~by:path' }

  let path_of_ident ?root id =
    match root with
    | Some path -> Path.Pdot (path, Ident.name id)
    | None -> Path.Pident id

  (** {1 Rule U1}

      Any path occurring in the file is in U. For example, List.map occurring in
      the file will add both List.map and List to U.
  *)

  let add_used rule env kind lid path t =
    let loc = lid.Location.loc in
    let log =
      match rule with
      | U1 -> log ~title:"U1" "U1: path %a used in file: %a (%a)"
      | U2 -> log ~title:"U2" "U2: %a defined in current file: %a (%a)"
    in
    let f acc kind path =
      log Logger.fmt
        (fun fmt ->
          Format.pp_print_string fmt (Shape.Sig_component_kind.to_string kind))
        Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path)
        Logger.fmt
        (fun fmt -> Location.print_loc fmt loc);
      add_item (kind, path) env acc
    in
    (* If a path is in D and it includes another module path within it, then that
       module path is also in D.*)
    fold_on_path_segments ~init:t ~kind ~f path

  let use_module env lid path = add_used U1 env Module lid path
  let use_modtype env lid path = add_used U1 env Module_type lid path
  let use_type env lid path = add_used U1 env Type lid path
  let use_value env lid path = add_used U1 env Value lid path

  (** {1 Rules U2 and U3: paths defined in the current file, or brought in scope
      by an include or an open} *)

  (* The apparent names of the components brought in scope by an open or an
     include are added to D. [full_path] is the complete path in the
     environment. *)
  let add_apparent_name_to_d ~from kind ?root ?full_path id u =
    map_if_shortenable kind u @@ fun u ->
    let path = path_of_ident ?root id in
    let origin =
      match from with
      | `Open -> "brought in scope by an open"
      | `Include ->
        "brought in scope by an include (or an open of a module expression)"
    in
    log ~title:"U3" "U3: %s %a%a %s"
      (Shape.Sig_component_kind.to_string kind)
      Logger.fmt
      (fun fmt -> (Format_doc.compat Path.print) fmt path)
      Logger.fmt
      (fun fmt ->
        match full_path with
        | None -> ()
        | Some full_path ->
          Format.fprintf fmt " [%a]" (Format_doc.compat Path.print) full_path)
      origin;
    { u with discourse = Path_trie.add ?full_path path kind u.discourse }

  (** {1 Rule U3}

     All paths for things "defined" using include or open in the current file
     are in U. (TODO CR Ulysse we might want to retry putting opened items in D)
  *)

  (* [open_path] is the path of the module whose components are brought in
     scope. [path_under_open] is the current path relative to [open_path].

     A component [x] is known under two paths: [path_under_open.x], the one the
     user can write now that the module is opened (and the one we want to print)
     and [open_path.path_under_open.x], the one valid in the current
     environment. We remember both, so that a candidate coming out of an [open]
     can still be checked against an environment and filed under the right
     canonical path. *)
  let define_signature_components ~mode ~origin ~env ?open_path ?path_under_open
      (sg : Subst.Lazy.signature) u =
    let from, by =
      match origin with
      | Open -> (`Open, "open")
      | Module_type_of -> (`Open, "module type of")
      | Include -> (`Include, "include")
      | Printing -> (`Include, "for printing")
    in
    (* We add every component's full path to U and its apparent name, if any,
       to D *)
    List.fold_left
      (fun u sig_item ->
        let lid id = Location.mknoloc (Longident.Lident (Ident.name id)) in
        let map_id id =
          (* In some cases we don't want to keep the signature ids because they
             don't make sense in the current environement. This is notable the
             case when opening a module. *)
          match mode with
          | Refresh ->
            (* We keep the scope for sorting *)
            let scope = Ident.scope id in
            if scope = Ident.highest_scope then Ident.rename id
            else Ident.create_scoped ~scope (Ident.name id)
          | Keep -> id
        in
        match (sig_item : Subst.Lazy.signature_item) with
        | Sig_type (id, _, _, _) ->
          let id = map_id id in
          log ~title:"U3" "U3: type %a brought in scope by %s" Logger.fmt
            (Fun.flip Ident.print id) by;
          let full_path = path_of_ident ?root:open_path id in
          let u = use_type env (lid id) full_path u in
          add_apparent_name_to_d ~from Type ?root:path_under_open ~full_path id
            u
        | Sig_value (id, _, _) ->
          let id = map_id id in
          log ~title:"U3" "U3: value %a brought in scope by %s" Logger.fmt
            (Fun.flip Ident.print id) by;
          let full_path = path_of_ident ?root:open_path id in
          let u = use_value env (lid id) full_path u in
          add_apparent_name_to_d ~from Value ?root:path_under_open ~full_path id
            u
        | Sig_typext (_, _, _, _) | Sig_jkind (_, _, _) -> u
        | Sig_module (id, _, { md_type; _ }, _, _) ->
          let id = map_id id in
          log ~title:"U3" "U3: module %a brought in scope by %s" Logger.fmt
            (Fun.flip Ident.print id) by;
          let full_path = path_of_ident ?root:open_path id in
          let u =
            add_apparent_name_to_d ~from Module ?root:path_under_open ~full_path
              id u
          in
          let path_under_open = path_of_ident ?root:path_under_open id in
          let u =
            match md_type with
            | Mty_alias alias_path ->
              let target = Env.normalize_module_path None env alias_path in
              add_subst_u target ~by:path_under_open u
            | _ -> use_module env (lid id) full_path u
          in
          add_subst_u full_path ~by:path_under_open u
        | Sig_modtype (id, _, _) ->
          let id = map_id id in
          log ~title:"U3" "U3: module type %a brought in scope by %s" Logger.fmt
            (Fun.flip Ident.print id) by;
          let full_path = path_of_ident ?root:open_path id in
          let u = use_modtype env (lid id) full_path u in
          add_apparent_name_to_d ~from Module_type ?root:path_under_open
            ~full_path id u
        | Sig_class (_, _, _, _) | Sig_class_type (_, _, _, _) ->
          (* TODO: do *) u)
      u
      (Subst.Lazy.force_signature_once sg)

  let bring_components ~mode ~origin env source u =
    match source with
    | Signature sg ->
      define_signature_components ~mode ~origin ~env
        (Subst.Lazy.of_signature sg)
        u
    | Module_path path -> (
      log ~title:"U3" "U3: components of module %a brought in scope" Logger.fmt
        (fun fmt -> (Format_doc.compat Path.print) fmt path);
      try
        (* We need to traverse the aliases to get the components *)
        let open_path = Env.normalize_module_path None env path in
        let md = Env.find_module_lazy open_path env in
        (* [merlin] We cannot use [Mtype.scrape_lazy] directly: in Merlin
           [Mtype] depends on [Out_type] which depends on this module. *)
        match !Env.scrape_lazy env md.md_type with
        | Subst.Lazy.Mty_signature sg ->
          define_signature_components ~mode ~origin ~env ~open_path:path sg u
        | _ -> u
      with Not_found -> u)

  (* [what] is ["constructor"] or ["label"], [discourse] is the discourse of
     the constructor or label ([cstr_discourse] or [lbl_discourse]). *)
  let use_constructor_or_label ~what env lid discourse u =
    let u =
      (* When using a constructor or a label, the modules appearing in its path
         should be added to U. TODO: we might want to do that even if the
         constructor has been disambiguated *)
      match lid.Location.txt with
      | Longident.Ldot (mod_lid, _) -> (
        (* This find should not load additional CUs, because
           [lookup_structure_components] (resp. [lookup_all_labels]) was called
           anyway by the compiler. *)
        try
          let path, _ = Env.find_module_by_name_lazy mod_lid.txt env in
          use_module env mod_lid path u
        with Not_found -> u)
      | _ -> u
    in
    (* D7: If a constructor or label is in U then any paths used in its type are
       in D. *)
    log ~title:"D7" "D7: %s %a used, merging its discourse" what Logger.fmt
      (Fun.flip Pprintast.longident lid.Location.txt);
    merge_discourse discourse u

  (** {1 Forcing}

      Processes the recorded events *)

  let apply_event u = function
    | Used { rule; kind; lid; path; env } -> add_used rule env kind lid path u
    | Used_constructor { env; lid; discourse } ->
      use_constructor_or_label ~what:"constructor" env lid discourse u
    | Used_label { env; lid; discourse } ->
      use_constructor_or_label ~what:"label" env lid discourse u
    | Initial_discourse paths -> merge_discourse paths u
    | Brought_in_scope { env; source; ident_mode; origin } ->
      bring_components ~mode:ident_mode ~origin env source u

  let force u =
    match u.pending with
    | [] -> u
    | pending ->
      List.fold_left apply_event { u with pending = [] } (List.rev pending)
end

module D = struct
  (**

     {2 The domain of discourse}

     - 1. The paths of all the predefined types that are intended for direct use by
       users, like int, are in D.
     - 2. If a path is in U then it is also in D.
     - 3. If a module path is in U then all the paths of its subcomponents are in D.
     - 4. If a value path is in U and its value description was written by a user -
       as opposed to being inferred - then the paths used in that description are
       in D.
     - 5. If a module path is in U and its module description was written by a user -
       as opposed to being inferred - then the paths used in that description are
       in D, excluding those paths that only appear inside of a sig...end.
     - 6. If a type path is in U then any paths used in its equation or
       representation are in D.
     - 7. If a constructor or record field is in U then any paths used in its type
       are in D.
     - 8. If a module type path is in U then any paths used in its definition are in
       D, excluding those paths that only appear inside of a sig...end.
     - 9. If a class path is in U and its class description was written by a user -
       as opposed to being inferred - then of any paths used in that description
       are in D.
     - 10. If a class type path is in U then any paths used in its definition are in
       D.
     - 11. If a path is in D and it includes another module path within it, then that
       module path is also in D.
     - 12. If a module path m in D - note D not U - is a module alias with target n
       and another path p in D includes n within it, then the path obtained by
       substituting the m for n in p is also in D.
  *)

  (* TODO: this is not really a chain, just increasingly large jumps. This
     reduces the number of combinations when unrolling substitutions but we might
     need the missing internal aliases. *)
  let alias_chain env path =
    let rec loop (path : Path.t) =
      match path with
      | Pident id when Ident.is_global id -> [ path ]
      | _ -> (
        match Env.find_module_lazy path env with
        | { md_type = Subst.Lazy.Mty_alias path1; _ } -> path1 :: loop path1
        | _ -> [ path ])
    in
    loop path

  let follow_aliases_adding_subst log_detail env substs path ~subst_by:path' =
    let add_to_substs substs path =
      log ~title:"D12" "D12: subst %a -> %a (%s)" Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path)
        Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path')
        log_detail;
      U.add_subst substs path ~by:path'
    in
    List.fold_left add_to_substs substs (alias_chain env path)

  (* Same as [Untypeast.lident_of_path]. It is duplicated here so that this
     module, which is used by [Typetexp], does not depend on [Untypeast]. *)
  let rec lident_of_path : Path.t -> Longident.t =
    let noloc_lident_of_path p = Location.mknoloc (lident_of_path p) in
    function
    | Pident id -> Lident (Ident.name id)
    | Papply (p1, p2) ->
      Lapply (noloc_lident_of_path p1, noloc_lident_of_path p2)
    | Pdot (p, s) | Pextra_ty (p, Pcstr_ty s) ->
      Ldot (noloc_lident_of_path p, Location.mknoloc s)
    | Pextra_ty (p, (Pext_ty | Punboxed_ty)) -> lident_of_path p

  let special_rule_for_aliases env d path alias_lid alias_path =
    (* TODO CR Ulysse Working on the longident here does not feel right. *)
    try
      let recorded_path = alias_path in
      let alias_path, _ =
        Env.find_module_by_name_lazy (lident_of_path alias_path) env
      in
      if
        not
          (Path.same alias_path recorded_path
          || Path.same
               (Env.normalize_module_path None env alias_path)
               (Env.normalize_module_path None env recorded_path))
      then raise Not_found;
      let substs =
        log ~title:"D13" "D13: subst %a -> %a[%a] (alias at %a)" Logger.fmt
          (Fun.flip (Format_doc.compat Path.print) path)
          Logger.fmt
          (Fun.flip Pprintast.longident alias_lid.Location.txt)
          Logger.fmt
          (Fun.flip (Format_doc.compat Path.print) alias_path)
          Logger.fmt
          (Fun.flip Location.print_loc alias_lid.Location.loc);
        U.add_subst d.substs alias_path ~by:path
      in
      (* TODO: This is an unwritten rule (yet): If a module in U is an alias
         then this alias is also in U. The alias path and its module prefixes
         are returned to be processed as items of U. *)
      let more =
        U.fold_on_path_segments ~init:[] ~kind:Module
          ~f:(fun acc kind path -> ((kind, path), env) :: acc)
          alias_path
      in
      ({ d with substs }, more)
    with Not_found -> (d, [])

  let d3_rule env path paths substs sig_ =
    let pdot id = Path.Pdot (path, Ident.name id) in
    let items = Subst.Lazy.force_signature_once sig_ in
    (* This function is used to recreate aliases' target paths without looking
       them up from the environment. (which would open unneeded cmis) *)
    let rec prefix (path : Path.t) : Path.t =
      let siblings =
        List.filter_map
          (function
            | Subst.Lazy.Sig_module (id, _, _, _, _) -> Some id
            | _ -> None)
          items
      in
      match path with
      | Pident id ->
        if List.exists (Ident.same id) siblings then pdot id else path
      | Pdot (p, name) -> Pdot (prefix p, name)
      | Papply (p, arg) -> Papply (prefix p, prefix arg)
      | Pextra_ty (p, extra) -> Pextra_ty (prefix p, extra)
    in
    List.fold_left
      (fun (paths, substs) item ->
        let add kind id =
          map_if_shortenable kind paths @@ fun paths ->
          begin
            log ~title:"D3" "D3: signature component %s %a (%a) under module %a"
              (Shape.Sig_component_kind.to_string kind)
              Logger.fmt
              (fun fmt -> Ident.print fmt id)
              Logger.fmt
              (Fun.flip (Format_doc.compat Path.print) (pdot id))
              Logger.fmt
              (Fun.flip (Format_doc.compat Path.print) path);
            Path_trie.add (pdot id) kind paths
          end
        in
        match (item : Subst.Lazy.signature_item) with
        | Subst.Lazy.Sig_value (id, _, _) -> (add Value id, substs)
        | Sig_type (id, _, _, _) -> (add Type id, substs)
        | Sig_typext (id, _, _, _) -> (add Extension_constructor id, substs)
        | Sig_module (id, _, md, _, _) ->
          let paths = add Module id in
          let substs =
            match md.md_type with
            | Mty_alias path' ->
              follow_aliases_adding_subst "sub-component is a module alias" env
                substs (prefix path') ~subst_by:(pdot id)
            | _ -> substs
          in
          (paths, substs)
        | Sig_modtype (id, _, _) -> (add Module_type id, substs)
        | Sig_class (id, _, _, _) -> (add Class id, substs)
        | Sig_class_type (id, _, _, _) -> (add Class_type id, substs)
        | Sig_jkind (id, _, _) -> (add Jkind id, substs))
      (paths, substs) items

  let module_consequences d env path =
    let md = Env.find_module_lazy path env in

    (* D13 If a module in U is an alias then this alias is also in U. *)
    let d, more =
      match md.md_discourse_alias with
      | None -> (d, [])
      | Some (alias_lid, (_Module, alias_path)) ->
        special_rule_for_aliases env d path alias_lid alias_path
    in

    (* D3. If a module path is in U then all the paths of its subcomponents
       are in D. They are only computed on demand when shortening. *)
    let d =
      match md.md_type with
      | Subst.Lazy.Mty_signature sig_ ->
        log ~title:"D3" "D3: components of module %a are in D (on demand)"
          Logger.fmt
          (Fun.flip (Format_doc.compat Path.print) path);
        let components =
          lazy (d3_rule env path Path_trie.empty Path.Map.empty sig_)
        in
        let expansions =
          Path.Map.update path
            (function
              | None -> Some components
              | already -> already)
            d.expansions
        in
        { d with expansions }
      | _ -> d
    in

    (* D5. If a module path is in U and its module description was written then
       the paths used in that description are in D *)
    (* TODO : If a path is in D and it includes another module path within it,
       then that module path is also in D. *)
    log ~title:"D5" "D5: merging discourse of module %a" Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) path);
    let md_discourse = trie_of_paths md.md_discourse in
    ({ d with paths = Path_trie.union d.paths md_discourse }, more)

  (* The consequences of an item are its contribution to D, together with the
     additional items found to be in U while computing it (alias targets). *)

  let consequences d env ((kind, path) : Item.t) =
    match kind with
    | Module_type ->
      let mtd = Env.find_modtype_lazy path env in
      (* D8. If a module type path is in U then any paths used in its definition
         are in *)
      log ~title:"D8" "D8: merging discourse of module type %a" Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path);
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      let mtd_discourse = trie_of_paths mtd.mtd_discourse in
      ({ d with paths = Path_trie.union d.paths mtd_discourse }, [])
    | Module -> module_consequences d env path
    | Value ->
      (* D4. If a value path is in U and its value description was written by a user -
         as opposed to being inferred - then the paths used in that description are
         in D. *)
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      let vd = Env.find_value path env in
      log ~title:"D4" "D4: merging discourse of value %a" Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path);
      let val_discourse = trie_of_paths vd.val_discourse in
      ({ d with paths = Path_trie.union d.paths val_discourse }, [])
    | Type ->
      (* D6. If a type path is in U then any paths used in its equation or
         representation are in D. *)
      (* TODO : If a path is in D and it includes another module path within it,
         then that module path is also in D. *)
      let td = Env.find_type path env in
      (* What does it mean when such a path is just an ident that is local to
         another module ?*)
      log ~title:"D6" "D6: merging discourse of type %a" Logger.fmt
        (Fun.flip (Format_doc.compat Path.print) path);
      let type_discourse = trie_of_paths td.type_discourse in
      ({ d with paths = Path_trie.union d.paths type_discourse }, [])
    | _ -> (d, [])

  let add_from_u_to_d :
      discourse -> Env.t -> Item.t -> discourse * (Item.t * Env.t) list =
   fun d env ((kind, path) as item) ->
    log ~title:"D2" "D2: %a in U so in D (kind: %s)" Logger.fmt
      (Fun.flip (Format_doc.compat Path.print) path)
      (Shape.Sig_component_kind.to_string kind);
    let d =
      (* D2 items in U are in D *)
      map_if_shortenable kind d @@ fun d ->
      { d with paths = Path_trie.add path kind d.paths }
    in
    (* In some cases, [consequences] tries to load its own compilation unit,
       which (inconsistently to our understanding) fails.

       For instance, the two following queries on base's float raise a [Not
       found] exception:

       {[
       /path/to/this/ocamlmerlin single type-enclosing -position '892:7' -index 0  -filename /path/to/base/src/float.ml < /path/to/base/src/float.ml

       {"class":"exception","value":"Not_found
       Raised at Ocaml_typing__Ident.find_same in file \"src/ocaml/typing/ident.ml\", line 305, characters 6-21
       [...]
       Called from Ocaml_typing__Discourse.D.module_consequences in file \"src/ocaml/typing/discourse.ml\", line 574, characters 15-44
       [...]
       ","notifications":[],"timing":{"clock":326,"cpu":234,"query":20,"pp":0,"reader":7,"ppx":41,"typer":166,"error":0},"heap_mbytes":41,"cache":{"reader_phase":"miss","ppx_phase":"miss","typer":"miss","cmt":{"hit":0,"miss":0},"cms":{"hit":0,"miss":0},"cmi":{"hit":0,"miss":53},"document_overrides_phase":"miss","locate_overrides_phase":"miss"},"query_num":0}
       ]}

       and

       {[
       /path/to/this/ocamlmerlin server type-enclosing -position '201:18' -index 0  -filename /path/to/base/src/int32.ml < /path/to/base/src/int32.ml
       ]}

       TODO: Is that still a thing ?
       TODO: find why there is such a self-module added/why it sometimes load
       sometimes raises. *)
    try consequences d env item
    with Not_found | Env.Error (Lookup_error _) -> (d, [])

  let of_U u =
    (* Apply any unprocessed event. *)
    let u = U.force u in
    log_recap ~title:"U" "U at start of D.of_U:\n%a" Logger.fmt
      (Fun.flip U.pp_u u);
    let todo = Queue.create () in
    Item_map.iter (fun item env -> Queue.add (item, env) todo) u.U.u_paths;
    let rec loop seen d =
      match Queue.take_opt todo with
      | None -> d
      | Some (item, env) ->
        (* There is no need to process twice the same path *)
        if Paths.mem item seen then loop seen d
        else begin
          (* Processing one path might bring more work *)
          let d, more = add_from_u_to_d d env item in
          List.iter (fun discovered -> Queue.add discovered todo) more;
          loop (Paths.add item seen) d
        end
    in
    let d =
      loop Paths.empty
        { paths = u.U.discourse;
          substs = u.U.substs;
          expansions = Path.Map.empty
        }
    in
    log_recap ~title:"D" "Final D:\n%a" Logger.fmt (Fun.flip pp_d d);
    d

  let pp = pp_d
end

include U

(* The public recording API *)

let use_module env lid path =
  record (Used { rule = U1; kind = Module; lid; path; env })
let use_modtype env lid path =
  record (Used { rule = U1; kind = Module_type; lid; path; env })
let use_type env lid path =
  record (Used { rule = U1; kind = Type; lid; path; env })
let use_value env lid path =
  record (Used { rule = U1; kind = Value; lid; path; env })
let use_constructor env lid (constr : Data_types.constructor_description) =
  record (Used_constructor { env; lid; discourse = constr.cstr_discourse })
let use_label env lid (label : _ Data_types.gen_label_description) =
  record (Used_label { env; lid; discourse = label.lbl_discourse })

let add_initial_discourse () = record (Initial_discourse (Predef.discourse ()))

(* Rule U2: definitions of the file are recorded as items of U, like used paths,
   so that their D consequences apply. [env] must be the environment in which
   the definition is bound. *)
let ident name id =
  (Location.{ name with txt = Longident.Lident name.txt }, Path.Pident id)

let define_type env name id =
  let lid, path = ident name id in
  record (Used { rule = U2; kind = Type; lid; path; env })

let define_modtype env name id =
  let lid, path = ident name id in
  record (Used { rule = U2; kind = Module_type; lid; path; env })

let define_module env name id =
  let lid, path = ident name id in
  record (Used { rule = U2; kind = Module; lid; path; env })

let bring_in_scope ~ident_mode ~origin env source =
  record (Brought_in_scope { env; source; ident_mode; origin })

let open_module env path =
  bring_in_scope ~ident_mode:Refresh ~origin:Open env (Module_path path)

let include_module env path =
  bring_in_scope ~ident_mode:Keep ~origin:Include env (Module_path path)

let modtype_of_components env path =
  bring_in_scope ~ident_mode:Refresh ~origin:Module_type_of env
    (Module_path path)

let printed_signature env sg =
  bring_in_scope ~ident_mode:Keep ~origin:Printing env (Signature sg)

let define_signature env sg =
  bring_in_scope ~ident_mode:Keep ~origin:Include env (Signature sg)

(* Process all pending U events and save the result so that this is only done
   once even if both [get] and [debug_print] are called. *)
let force_g () =
  let u = force !g in
  g := u;
  u

let get () = D.of_U (force_g ())

let debug_print fmt = D.pp fmt (D.of_U (force_g ()))
