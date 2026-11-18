let log_section = "short-paths"
let { Logger.log } = Logger.for_section log_section
let { Logger.log = log_dbg } = Logger.for_section (log_section ^ "-dbg")

let path_print = Format_doc.compat Path.print

module Out_type = struct
  (* This part is copied from upstream's [Out_type] *)

  (* Normalize paths *)

  type param_subst = Id | Nth of int | Map of int list

  let compose l1 = function
    | Id -> Map l1
    | Map l2 -> Map (List.map (List.nth l1) l2)
    | Nth n -> Nth (List.nth l1 n)

  let rec index l x =
    match l with
    | [] -> raise Not_found
    | a :: l -> if Types.eq_type x a then 0 else 1 + index l x

  let rec uniq = function
    | [] -> true
    | a :: l -> (not (List.memq (a : int) l)) && uniq l

  let rec normalize_type_path ?(cache = false) env p =
    let open Types in
    try
      let params (* type_params *), ty (* type_manifest *), _ =
        (* Find the manifest type associated to a type when appropriate:
           - the type should be public or should have a private row,
           - the type should have an associated manifest type. *)
        Env.find_type_expansion p env
      in
      match get_desc ty with
      | Tconstr (p1, tyl, _) ->
        log ~title:"normalize_type_path" "Found type expansion %a for %a"
          Logger.fmt (Fun.flip path_print p1) Logger.fmt (Fun.flip path_print p);
        if
          List.length params = List.length tyl
          && List.for_all2 eq_type params tyl
          && p1 <> p (* TODO if this the correct way to prevent looping ? *)
        then normalize_type_path ~cache env p1
        else if
          cache
          || List.length params <= List.length tyl
          || not (uniq (List.map get_id tyl))
        then (Env.normalize_type_path None env p, Id)
        else
          let l1 = List.map (index params) tyl in
          let p2, s2 = normalize_type_path ~cache env p1 in
          (p2, compose l1 s2)
      | _ -> (Env.normalize_type_path None env p, Nth (index params ty))
    with Not_found ->
      log ~title:"normalize_type_path"
        "Calling [Env.normalize_type_path] for %a" Logger.fmt
        (Fun.flip path_print p);
      (Env.normalize_type_path None env p, Id)

  let normalize_type_path ?cache env p =
    log ~title:"normalize_type_path" "Normalizing %a " Logger.fmt
      (Fun.flip path_print p);
    let p, subst = normalize_type_path ?cache env p in
    log ~title:"normalize_type_path" "Normalizing %a " Logger.fmt
      (Fun.flip path_print p);
    (p, subst)

  let find_double_underscore s =
    let len = String.length s in
    let rec loop i =
      if i + 1 >= len then None
      else if s.[i] = '_' && s.[i + 1] = '_' then Some i
      else loop (i + 1)
    in
    loop 0

  let penalty_size = 20

  let name_penalty s =
    if s <> "" && s.[0] = '_' then penalty_size
    else
      match find_double_underscore s with
      | None -> 2
      | Some _ -> penalty_size
end

let ident_scope id =
  if Ident.is_predef id then Ident.highest_scope else Ident.scope id

(* Adapted from out_type. Same weight but we favorize predefs.

   TODO CR Ulysse there is room for adjustment here. Scope is not always a very
   good metric, proximity of the last usage might be better. *)
(* Returns a couple with the name weight and the scope *)
let path_size path =
  let name_penalty = Out_type.name_penalty in
  let rec size = function
    | Path.Pident id -> (name_penalty (Ident.name id), -ident_scope id)
    | Pdot (p, id) | Pextra_ty (p, Pcstr_ty id) ->
      let l, b = size p in
      (name_penalty id + l, b)
    | Papply (p1, p2) ->
      let l, b = size p1 in
      (l + fst (size p2), b)
    | Pextra_ty (p, Pext_ty) -> size p
    | Pextra_ty (p, Punboxed_ty) ->
      let l, b = size p in
      (1 + l, b)
  in
  size path

let compare_weight (l1, s1) (l2, s2) =
  let c = l1 - l2 in
  if c <> 0 then c else s1 - s2

let compare_paths_weight p1 p2 = compare_weight (path_size p1) (path_size p2)
type kind = Type | Module | Module_type

let component_kind : kind -> Shape.Sig_component_kind.t = function
  | Type -> Type
  | Module -> Module
  | Module_type -> Module_type

let string_of_kind = function
  | Type -> "Type"
  | Module -> "Module"
  | Module_type -> "Module_type"

module Kind_map = Map.Make (struct
  type t = kind
  let compare = compare
end)

module Path_trie = Discourse_types.Path_trie

(* Shortening a path

   Given a path to print, we want the lightest name in D that denotes the same
   thing in the printing environment, the weight of a name being the sum of the
   penalties of its components (see [path_size]).

   - candidates sit in a queue ordered by weight: levels (all candidates of the
     same weight) are validated in increasing order. Validating a candidate
     means finding its name in the printing environment and checking that it
     denotes the same item as the one we expect. Valid names are added to a
     table indexed by canonical paths, invalid ones are kept aside.
   - after each level the table is consulted for the canonical path of the
     request. The search stops as soon as the best name found is strictly
     shorter than everything left in the queue.

   The search state persists across the calls. *)

(* A name that may denote the path to print. [apparent] is the user-facing name
   while [full] is the actual path in the environment. *)
type candidate = { apparent : Path.t; full : Path.t }

(* Candidates ordered by the weight of their apparent path, lightest first *)
module Candidates = struct
  include Set.Make (struct
    type t = candidate

    let compare c c' =
      let weight = compare_paths_weight c.apparent c'.apparent in
      if weight <> 0 then weight
      else
        let apparent = Path.compare c.apparent c'.apparent in
        if apparent <> 0 then apparent else Path.compare c.full c'.full
  end)

  let pp_elt fmt { apparent; full } =
    Format.fprintf fmt "%a [%a]" path_print apparent path_print full
end

(* An expansion is a module of U (its full path) under one of its apparent
   paths. Expansions are ordered by the weight of the apparent path. *)
module Expansions = Set.Make (struct
  type t = Path.t * Path.t (* apparent path, full path *)

  let compare (apparent, full) (apparent', full') =
    let weight = compare_paths_weight apparent apparent' in
    if weight <> 0 then weight
    else
      let c = Path.compare apparent apparent' in
      if c <> 0 then c else Path.compare full full'
end)

(* The state of the search *)

type queue = Candidates.t Kind_map.t

let empty_queue = Kind_map.empty

let queue_of_kind kind queue =
  Option.value ~default:Candidates.empty (Kind_map.find_opt kind queue)

let map_queue_of_kind kind f queue =
  Kind_map.update kind
    (fun candidates ->
      Some (f (Option.value ~default:Candidates.empty candidates)))
    queue

let pp_queue fmt queue =
  Kind_map.iter
    (fun kind candidates ->
      Format.fprintf fmt "@[<v 2>%s:@ %a@]@;" (string_of_kind kind)
        (Format.pp_print_seq ~pp_sep:Format.pp_print_space Candidates.pp_elt)
        (Candidates.to_seq candidates))
    queue

type search_state =
  { queue : queue;  (** Candidates not tested yet *)
    canon_table : Candidates.t Path.Map.t Kind_map.t;
        (** The validated candidates, by kind and the canonical path they denote *)
    not_in_env : Path_trie.t;
        (** Candidates whose name is not bound in the printing environment, kept
            aside until the environment changes *)
    known : Path_trie.t;
        (** Every path queued so far to prevent queueing the same path twice *)
    substs : Discourse_types.substs;
        (** The substitutions known so far (D12, D13) *)
    available_expansions :
      (Path_trie.t * Discourse_types.substs) Lazy.t Path.Map.t;
        (** The modules of U whose components are in D by rule lazy D3 *)
    pending_expansions : Expansions.t;  (** Scheduled expansions *)
    expanded : Expansions.t
        (** Already expanded paths (to prevent expanding them again) *)
  }

let initial_search_state =
  { queue = empty_queue;
    canon_table = Kind_map.empty;
    not_in_env = Path_trie.empty;
    known = Path_trie.empty;
    substs = Path.Map.empty;
    available_expansions = Path.Map.empty;
    pending_expansions = Expansions.empty;
    expanded = Expansions.empty
  }

let search_state : search_state ref = Local_store.s_ref initial_search_state
(* TODO CR Ulysse having a table keyed by Uids would seem to be a better choice,
   but it does not always work. One example of that is test `sp-13` were a type
   from Deferred0 is redefined in Deferred, causing paths converging to this same
   type to have unmatching uids.*)

type request = { env : Env.t; kind : kind; canon_path : Path.t }

let is_expandable search full = Path.Map.mem full search.available_expansions

let table_of_kind kind table =
  Option.value ~default:Path.Map.empty (Kind_map.find_opt kind table)

let map_table_of_kind kind f table =
  Kind_map.update kind
    (fun names -> Some (f (Option.value ~default:Path.Map.empty names)))
    table

let pp_table fmt table =
  Kind_map.iter
    (fun kind names ->
      Path.Map.iter
        (fun canonical candidates ->
          Format.fprintf fmt "@;%a (%s) -> {%a}" path_print canonical
            (string_of_kind kind)
            (Format.pp_print_seq ~pp_sep:Format.pp_print_space Candidates.pp_elt)
            (Candidates.to_seq candidates))
        names)
    table

let normalize_type_path = Out_type.normalize_type_path ~cache:false

(** {1 Substitutions} *)

let merge_substs (substs : Discourse_types.substs) substs' =
  Path.Map.union (fun _ a b -> Some (Path.Set.union a b)) substs substs'

(** [apply_substitutions_by_target] is cheaper than [apply_substitutions] when
   there are only few substitutions to apply. *)
let apply_substitutions_by_target (substs : Discourse_types.substs) paths =
  Path.Map.fold
    (fun target replacements acc ->
      match Path_trie.reach paths target with
      | None -> acc
      | Some (Trie (kinds, children)) ->
        Path.Set.fold
          (fun path acc ->
            Path_trie.union acc @@ Path_trie.trie_of_path ~children path kinds)
          replacements acc)
    substs Path_trie.empty

(** [apply_substitutions] is cheaper than [apply_substitutions_by_target] when
   the trie is small. *)
let apply_substitutions (substs : Discourse_types.substs) paths =
  Path_trie.fold_nodes
    (fun path (Trie (kinds, children)) acc ->
      match Path.Map.find_opt path substs with
      | None -> acc
      | Some replacements ->
        Path.Set.fold
          (fun path acc ->
            Path_trie.union acc @@ Path_trie.trie_of_path ~children path kinds)
          replacements acc)
    paths Path_trie.empty

(* [paths] and the paths obtained by applying [substs] to them, repeatedly (up
   to a small depth) *)
let apply_substitutions_fixpoint substs paths =
  let rec loop ~fuel acc frontier =
    let rewritten = apply_substitutions substs frontier in
    if Path_trie.is_empty rewritten then acc
    else
      let size = Path_trie.size acc in
      let acc = Path_trie.union acc rewritten in
      if fuel = 0 || size = Path_trie.size acc then acc
      else loop ~fuel:(fuel - 1) acc rewritten
  in
  loop ~fuel:2 paths paths

(** {1 The priority queue handling} *)

(* Queues the [paths] candidates and appropriate pending expansions *)
let enqueue search paths =
  let enqueue_item apparent search (kind, full) =
    let add kind search =
      let candidate = { apparent; full } in
      { search with
        queue = map_queue_of_kind kind (Candidates.add candidate) search.queue
      }
    in
    match (kind : Shape.Sig_component_kind.t) with
    | Type -> add Type search
    | Module_type -> add Module_type search
    | Module ->
      let expansion = (apparent, full) in
      let pending_expansions =
        if
          is_expandable search full
          && not (Expansions.mem expansion search.expanded)
        then Expansions.add expansion search.pending_expansions
        else search.pending_expansions
      in
      add Module { search with pending_expansions }
    | Value
    | Extension_constructor
    | Class
    | Class_type
    | Constructor
    | Label
    | Unboxed_label
    | Jkind -> search
  in
  Path_trie.to_seq paths
  |> Seq.fold_left
       (fun search (apparent, items) ->
         Discourse_types.Paths.fold
           (fun item search -> enqueue_item apparent search item)
           items search)
       search

(* Queues again the candidates kept aside in [not_in_env]: all of them, or only
   those rooted at [ident]. This must be called before shortening in an
   environment different from the one of the previous call, which is done after
   any function wrapped with [Printyp.wrap_printing_env]. *)
let restore_ignored_paths ident =
  let state = !search_state in
  let restored, kept =
    match ident with
    | None -> (Some state.not_in_env, Path_trie.empty)
    | Some ident -> Path_trie.take ident state.not_in_env
  in
  match restored with
  | None -> ()
  | Some paths -> search_state := enqueue { state with not_in_env = kept } paths

(** {1 Canonical paths of candidates} *)

let find_by_name env kind lid =
  match kind with
  | Type -> fst (Env.find_type_by_name lid env)
  | Module -> fst (Env.find_module_by_name_lazy lid env)
  | Module_type -> fst (Env.find_modtype_by_name_lazy lid env)

(* The path a name denotes in [env], if it is bound.

   TODO CR Ulysse if required we might use a shortcut here with "find_"
   functions but these must be treated carefully as they do allow hidden
   module to be used. (breaks l-repro2 for example) *)
let find_path_by_name env kind apparent =
  let lid = Untypeast.lident_of_path apparent in
  match find_by_name env kind lid with
  | found -> Some found
  | exception Not_found -> None

let normalize env kind path =
  match kind with
  | Type -> fst (normalize_type_path env path)
  | Module -> Env.normalize_module_path None env path
  | Module_type -> Env.normalize_modtype_path env path

(* The canonical path of a candidate in [env]. In the presence of [open]
   statements, candidates [apparent] paths they cannot be looked up directly, so
   they are found by name and then we check that the result is coherent with
   their [full] path. *)
let canonical_path_of_candidate { env; kind; _ } { apparent; full } =
  (* TODO it might be worth it to memoïze this function *)
  match find_path_by_name env kind apparent with
  | None ->
    log ~title:"find_path_in_env" "%s name not found: %a" (string_of_kind kind)
      Logger.fmt
      (Fun.flip path_print apparent);
    None
  | Some found -> (
    log ~title:"find_path_in_env" "Lid: %a [%a] Path in env: %a" Logger.fmt
      (Fun.flip path_print apparent)
      Logger.fmt (Fun.flip path_print full) Logger.fmt
      (Fun.flip path_print found);
    try
      let canonical = normalize env kind found in
      if Path.same full found then Some canonical
      else
        let full_canonical = normalize env kind full in
        log ~title:"find_path_in_env" "%a <>? %a" Logger.fmt
          (Fun.flip path_print full_canonical)
          Logger.fmt
          (Fun.flip path_print canonical);
        if Path.same full_canonical canonical then Some canonical else None
    with Not_found -> None)

(* Does [apparent] denote [canon_path] in [env]? The candidates of the table
   are kept across environments, so they are validated again when used. *)
let denotes { env; kind; canon_path } apparent =
  match find_path_by_name env kind apparent with
  | None -> false
  | Some found -> (
    Path.same found canon_path
    ||
    match normalize env kind found with
    | canonical -> Path.same canonical canon_path
    | exception Not_found -> false)

(* Do two paths print the same? *)
let rec same_names (p1 : Path.t) (p2 : Path.t) =
  match (p1, p2) with
  | Pident id1, Pident id2 -> String.equal (Ident.name id1) (Ident.name id2)
  | Pdot (p1, s1), Pdot (p2, s2) -> String.equal s1 s2 && same_names p1 p2
  | Papply (f1, a1), Papply (f2, a2) -> same_names f1 f2 && same_names a1 a2
  | Pextra_ty (p1, e1), Pextra_ty (p2, e2) ->
    Path.compare_extra e1 e2 = 0 && same_names p1 p2
  | (Pident _ | Pdot _ | Papply _ | Pextra_ty _), _ -> false

(* The apparent path of a candidate is not always rooted at an identifier of
   the printing environment: components brought in scope by an [include] keep
   the identifiers of the included signature, and the ones brought by an [open]
   are given fresh identifiers. [Out_type] numbers (as in [t/2]) the
   identifiers that are not the ones bound to their name in the environment, so
   we return the path found in the environment whenever it reads the same. *)
let path_in_env { env; kind; _ } apparent =
  match find_path_by_name env kind apparent with
  | Some found when same_names found apparent -> found
  | Some _ | None -> apparent

(* The lightest valid name denoting [canon_path] known so far *)
let best_known_name request state =
  Option.bind
    (Path.Map.find_opt request.canon_path
       (table_of_kind request.kind state.canon_table))
  @@ fun candidates ->
  Candidates.to_seq candidates
  |> Seq.find_map @@ fun candidate ->
     if denotes request candidate.apparent then Some candidate.apparent
     else None

(** {1 Expansion forcing (rule D3)} *)

let module_components search full =
  match Path.Map.find_opt full search.available_expansions with
  | None -> None
  | Some components -> (
    try Some (Lazy.force components)
    with Not_found | Env.Error (Lookup_error _) ->
      log ~title:"expand" "Components of module %a could not be obtained"
        Logger.fmt (Fun.flip path_print full);
      None)

(* Materialises the components of the module [full], known as [apparent]: they
   are queued, known substitutions are applied to them, and potential new
   substitutions are recorded. *)
let expand search ((apparent, full) as expansion) =
  let search =
    { search with
      pending_expansions = Expansions.remove expansion search.pending_expansions;
      expanded = Expansions.add expansion search.expanded
    }
  in
  match module_components search full with
  | None -> search
  | Some (components, component_substs) ->
    let rerooted =
      if Path.same apparent full then components
      else
        (* Compute apparent paths (for opens) *)
        match Path_trie.reach components full with
        | None -> Path_trie.empty
        | Some (Trie (kinds, children)) ->
          Path_trie.trie_of_path ~children apparent kinds
    in
    (* D12: register only new substitutions *)
    let new_substs =
      Path.Map.filter_map
        (fun target replacements ->
          let known =
            Option.value ~default:Path.Set.empty
              (Path.Map.find_opt target search.substs)
          in
          let fresh = Path.Set.diff replacements known in
          if Path.Set.is_empty fresh then None else Some fresh)
        component_substs
    in
    let substs = merge_substs search.substs new_substs in
    let rewritten =
      (* Apply new subtitutions to known paths and all substitutions to new paths *)
      Path_trie.union rerooted
        (apply_substitutions_by_target new_substs search.known)
      |> apply_substitutions_fixpoint substs
    in
    (* We only queue paths never seen before *)
    let fresh = Path_trie.diff rewritten search.known in
    log ~title:"expand"
      "Expanding %a [%a]: %a components, %d new substs, %a new paths" Logger.fmt
      (Fun.flip path_print apparent)
      Logger.fmt (Fun.flip path_print full) Logger.fmt
      (fun fmt -> Format.pp_print_int fmt (Path_trie.size components))
      (Path.Map.cardinal new_substs)
      Logger.fmt
      (fun fmt -> Format.pp_print_int fmt (Path_trie.size fresh));
    enqueue
      { search with substs; known = Path_trie.union search.known fresh }
      fresh

(** {1 The search} *)

(** Search alternates between two phases:
   1. expand modules whose paths' weights are less than the next level's weight.
   2. treat each item in the next level. *)
type step =
  | Expand of Expansions.elt
      (** A module whose components may belong to the next level *)
  | Level of Path.t * candidate Seq.t
      (** The lightest level not validated yet: its lightest candidate and the
          candidates from it *)

(** The minimum weight of a component of a module known as [path] *)
let min_component_weight path =
  let length, scope = path_size path in
  (length + 1, scope)

(** Returns the next action to perform along with the weight of the shortest path
   that can result from it *)
let next_step request search =
  let candidates =
    Candidates.to_seq (queue_of_kind request.kind search.queue)
  in
  let lightest =
    match candidates () with
    | Seq.Nil -> None
    | Seq.Cons (candidate, _) -> Some candidate.apparent
  in
  let level =
    Option.map
      (fun lightest -> (path_size lightest, Level (lightest, candidates)))
      lightest
  in
  let expansion =
    Option.map
      (fun ((apparent, _) as expansion) ->
        (min_component_weight apparent, Expand expansion))
      (Expansions.min_elt_opt search.pending_expansions)
  in
  match (expansion, level) with
  | None, step | step, None -> step
  | Some ((expansion_weight, _) as expansion), Some ((level_weight, _) as level)
    ->
    if compare_weight expansion_weight level_weight <= 0 then Some expansion
    else Some level

(* Valid (in the current [env]) candidate are moved to the canonical lookup
   table while invalid ones are moved to [not_in_env] for later re-check. *)
let validate request search candidate =
  let kind = request.kind in
  let { apparent; full } = candidate in
  log ~title:"fill_by_level" "Treating %a (%s)" Logger.fmt
    (Fun.flip path_print apparent)
    (string_of_kind kind);
  let queue =
    map_queue_of_kind kind (Candidates.remove candidate) search.queue
  in
  match canonical_path_of_candidate request candidate with
  | Some canonical ->
    log ~title:"fill_by_level" "Updating table: %a -> { %a (%s) }" Logger.fmt
      (Fun.flip path_print canonical)
      Logger.fmt
      (Fun.flip path_print apparent)
      (string_of_kind kind);
    let file names =
      Path.Map.update canonical
        (function
          | None -> Some (Candidates.singleton candidate)
          | Some candidates -> Some (Candidates.add candidate candidates))
        names
    in
    let canon_table = map_table_of_kind kind file search.canon_table in
    { search with queue; canon_table }
  | None ->
    log ~title:"fill_by_level" "Path %a invalid in the current env" Logger.fmt
      (Fun.flip path_print apparent);
    (* Candidates not bound in the current environment are kept aside: this
       prevents testing them again when [shorten] is called repeatedly with the
       same environment, as when printing a large module signature. *)
    let not_in_env =
      Path_trie.add ~full_path:full apparent (component_kind kind)
        search.not_in_env
    in
    { search with queue; not_in_env }

(* Validates the candidates of the level of [lightest] *)
let validate_level request ~lightest candidates search =
  candidates
  |> Seq.take_while (fun c -> compare_paths_weight lightest c.apparent >= 0)
  |> Seq.fold_left (validate request) search

(* Levels are validated in increasing weight, after the expansions whose
   components may belong to them, until the best name found is the lightest. *)
let search_levels request search best =
  let improve search best =
    match best_known_name request search with
    | None -> best
    | found -> found
  in
  let lighter_than path ~weight =
    match path with
    | None -> false
    | Some path -> compare_weight (path_size path) weight < 0
  in
  let rec loop search best =
    log ~title:"fill_by_level" "Current best: %a" Logger.fmt (fun f ->
        Format.pp_print_option path_print f best);
    match next_step request search with
    | None ->
      log ~title:"fill_by_level" "Nothing left";
      (best, search)
    | Some (weight, _) when lighter_than best ~weight ->
      log ~title:"fill_by_level" "Found a name lighter than anything left: %a"
        Logger.fmt (fun f -> Format.pp_print_option path_print f best);
      (best, search)
    | Some (_, Expand expansion) -> loop (expand search expansion) best
    | Some (_, Level (lightest, candidates)) ->
      let search = validate_level request ~lightest candidates search in
      loop search (improve search best)
  in
  loop search best

let rec shorten ~env ~initial ~canon_path kind =
  let discourse = Discourse.get () in
  let state =
    { !search_state with
      substs = merge_substs !search_state.substs discourse.substs;
      available_expansions =
        Path.Map.union
          (fun _ kept _ -> Some kept)
          !search_state.available_expansions discourse.expansions
    }
  in
  log_dbg ~title:"shorten" "Current discourse: %a\n%!" Logger.fmt (fun fmt ->
      Discourse.debug_print fmt);
  log_dbg ~title:"shorten" "Current queue: %a" Logger.fmt (fun fmt ->
      pp_queue fmt state.queue);
  log_dbg ~title:"shorten" "Current notinenv: %a" Logger.fmt (fun fmt ->
      Path_trie.pp_seq fmt state.not_in_env);
  log_dbg ~title:"shorten" "Current table: %a" Logger.fmt (fun fmt ->
      pp_table fmt state.canon_table);

  let state =
    let additional_paths =
      Path_trie.add initial (component_kind kind) discourse.paths
      |> Path_trie.add canon_path (component_kind kind)
      |> apply_substitutions_fixpoint state.substs
    in
    log_dbg ~title:"shorten" "Discourse after substitutions: %a\n%!" Logger.fmt
      (fun fmt -> Path_trie.pp_seq fmt additional_paths);
    (* Paths queued by a previous call were validated already *)
    let fresh = Path_trie.diff additional_paths state.known in
    enqueue { state with known = Path_trie.union state.known fresh } fresh
  in

  log_dbg ~title:"shorten" "Current queue: %a" Logger.fmt (fun fmt ->
      pp_queue fmt state.queue);

  (* A name found by a previous call ? *)
  let request = { env; kind; canon_path } in
  let best = best_known_name request state in
  log ~title:"shorten" "Initial: %a; Canon: %a; Current best: %a" Logger.fmt
    (Fun.flip path_print initial) Logger.fmt (Fun.flip path_print canon_path)
    Logger.fmt (fun f -> Format.pp_print_option path_print f best);

  (* Is there a better one ? *)
  let best, state = search_levels request state best in

  (* Empty U, every relevant information is kept in the search state *)
  Discourse.reset ();
  search_state := state;

  let best =
    match best with
    | None -> initial
    | Some best -> path_in_env request best
  in
  shorten_application_args env best

and shorten_application_args env path =
  match (path : Path.t) with
  | Pident _ -> path
  | Pdot (p, s) ->
    let p' = shorten_application_args env p in
    if p' == p then path else Pdot (p', s)
  | Pextra_ty (p, extra) ->
    let p' = shorten_application_args env p in
    if p' == p then path else Pextra_ty (p', extra)
  | Papply (p1, p2) ->
    let p1' = find_module env p1 in
    let p2' = find_module env p2 in
    if p1' == p1 && p2' == p2 then path else Papply (p1', p2')

and find_module env initial =
  let canon_path = Env.normalize_module_path None env initial in
  shorten ~env ~initial ~canon_path Module

type type_result = Short_paths.type_result =
  | Nth of int
  | Path of int list option * Path.t

type type_resolution = Short_paths.type_resolution =
  | Nth of int
  | Subst of int list
  | Id

let find_type env initial : type_result =
  match normalize_type_path env initial with
  | _, Nth i -> Nth i
  | canon_path, Id -> Path (None, shorten ~env ~initial ~canon_path Type)
  | canon_path, Map l -> Path (Some l, shorten ~env ~initial ~canon_path Type)

let find_type_resolution env path : type_resolution =
  match normalize_type_path env path with
  | _, Nth i -> Nth i
  | _, Id -> Id
  | _, Map l -> Subst l

let find_type_simple env initial =
  let canon_path, _subst = normalize_type_path env initial in
  shorten ~env ~initial ~canon_path Type

let find_module_type env initial =
  let canon_path = Env.normalize_modtype_path env initial in
  shorten ~env ~initial ~canon_path Module_type
