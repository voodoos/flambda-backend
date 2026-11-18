(** The domain of discourse used to shorten printed paths.

    During typing, the typer records the paths used, defined or brought in
    scope in the current file (the set U). These events are only processed when
    something needs to be printed: [get] then computes the domain of discourse
    D from U. See the rules at the top of [discourse.ml]. *)

(** {1 Recording, during typing} *)

(** Rule D1: the predefined types *)
val add_initial_discourse : unit -> unit

(** Rule U1: a path occurring in the file, with the environment in which it
    occurs *)

val use_module : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_modtype : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_type : Env.t -> Longident.t Location.loc -> Path.t -> unit
val use_value : Env.t -> Longident.t Location.loc -> Path.t -> unit

val use_constructor :
  Env.t ->
  Longident.t Location.loc -> Data_types.constructor_description -> unit

val use_label :
  Env.t ->
  Longident.t Location.loc -> _ Data_types.gen_label_description -> unit

(** Rule U2: a definition of the file. *)

val define_type : Env.t -> string Location.loc -> Ident.t -> unit
val define_modtype : Env.t -> string Location.loc -> Ident.t -> unit
val define_module : Env.t -> string Location.loc -> Ident.t -> unit

(** Rule U3: the components of an included structure (or of an open) *)
val define_signature : Env.t -> Types.signature -> unit

(** Rule U3: the components of a module path brought in scope by an [open], an
    [include] or a [module type of] *)

val open_module : Env.t -> Path.t -> unit
val include_module : Env.t -> Path.t -> unit
val modtype_of_components : Env.t -> Path.t -> unit

(** The items of a signature being printed *)
val printed_signature : Env.t -> Types.signature -> unit

(** {1 Querying, when printing} *)

(** Processes the recorded events and returns the domain of discourse *)
val get : unit -> Discourse_types.discourse

val debug_print : Format.formatter -> unit

(** Empty U *)
val reset : unit -> unit
