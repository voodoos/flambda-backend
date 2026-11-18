(** Shortening of printed paths. *)

type type_result = Short_paths.type_result =
  | Nth of int
  | Path of int list option * Path.t

type type_resolution = Short_paths.type_resolution =
  | Nth of int
  | Subst of int list
  | Id

val find_type : Env.t -> Path.t -> type_result
val find_type_resolution : Env.t -> Path.t -> type_resolution
val find_type_simple : Env.t -> Path.t -> Path.t
val find_module_type : Env.t -> Path.t -> Path.t
val find_module : Env.t -> Path.t -> Path.t

(** Candidates that were not bound in a previous printing environment are kept
    aside. [restore_ignored_paths None] will re-queue all of them,
    [restore_ignored_paths (Some ident)] only those rooted at the given [ident].
    It must be called before shortening in an environment different from the one
    of the previous call. *)
val restore_ignored_paths : Ident.t option -> unit
