(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 Translation environment} *)

(** Environment for flambda to cmm translation *)
type t

(** [create offsets k k_exn] creates a local environment for translating a
    flambda expression, with return continuation [k], exception continuation
    [k_exn], and which uses the given closures variables. *)
val create :
  Exported_offsets.t ->
  Exported_code.t ->
  Continuation.t ->
  exn_continuation:Continuation.t ->
  t

(** [enter_function_def env k k_exn] creates a local environment for translating
    a flambda expression, with return continuation [k], exception continuation
    [k_exn], preserving the global info from [env]. *)
val enter_function_def : t -> Continuation.t -> Continuation.t -> t

(** {2 Continuations} *)

(** Returns the return continuation of the environment. *)
val return_cont : t -> Continuation.t

(** Returns the exception continuation of the environment. *)
val exn_cont : t -> Continuation.t

(** {2 Function info *)

(** Retrieve known information on the given function *)
val get_code_metadata : t -> Code_id.t -> Code_metadata.t

type closure_code_pointers =
  | Full_application_only
  | Full_and_partial_application

(** Retrieve the parameter arity of the function declaration (taking into
    account both the function's own arity and the [is_tupled] flag) together
    with the code pointer layout for the closure. *)
val get_func_decl_params_arity :
  t -> Code_id.t -> Clambda.arity * closure_code_pointers * Debuginfo.t

(** {2 Variable bindings} *)

(** Extra information about bound variables. These are not necessary for the
    translation, but useful to enable certain optimization. *)
type extra_info =
  | Untag of Cmm.expression
      (** The variable is bound to the result of untagging the cmm expression.
          This allows to have access to the cmm expression before untagging. *)
  | Box  (** The variable is bound to a number being boxed. *)

(** Create (and bind) a cmm variable for the given flambda variable, and return
    the new environment, and the created variable. Will fail (i.e. assertion
    failure) if the given variable is already bound. *)
val create_variable : t -> Variable.t -> t * Backend_var.With_provenance.t

(** Same as {!create_variable} but for a list of variables. *)
val create_variables :
  t -> Variable.t list -> t * Backend_var.With_provenance.t list

(** Bind a variable to the given cmm expression, to allow for delaying the
    let-binding. *)
val bind_variable :
  t ->
  Variable.t ->
  ?extra:extra_info ->
  Effects_and_coeffects.t ->
  bool ->
  Cmm.expression ->
  t

(** Get the cmm variable bound to a flambda variable.

    Will fail (i.e. assertion failure) if the variable is not bound.

    Be careful: in general you do *NOT* want to use this function but instead
    the {inline_variable} function, as it will correctly perform the inlining of
    used exactly once variables. *)
val get_variable : t -> Variable.t -> Cmm.expression

(** Try and inline an flambda variable using the delayed let-bindings. *)
val inline_variable :
  t -> Variable.t -> Cmm.expression * t * Effects_and_coeffects.t

(** Wrap the given cmm expression with all the delayed let bindings accumulated
    in the environment. *)
val flush_delayed_lets :
  ?entering_loop:bool -> t -> (Cmm.expression -> Cmm.expression) * t

(** Fetch the extra info for a flambda variable (if any). *)
val extra_info : t -> Variable.t -> extra_info option

(** {2 Continuation bindings} *)

(** Translation information for continuations. A continuation may either be
    translated as a static jump, or inlined at its call site. *)
type cont =
  | Jump of
      { types : Cmm.machtype list;
        cont : int
      }
      (** Static jump, with the given cmm continuation.

          The list of machtypes represent the types of arguments expected by the
          catch handler. *)
  | Inline of
      { handler_params : Bound_parameters.t;
        handler_body : Flambda.Expr.t;
        handler_params_occurrences : Num_occurrences.t Variable.Map.t
      }
      (** Inline the continuation. When inlining is not possible, generate a
          jump *)

(** Bind the given continuation to a jump, creating a fresh jump id for it. *)
val add_jump_cont : t -> Cmm.machtype list -> Continuation.t -> int * t

(** Bind the given continuation as an inline continuation, bound over the given
    variables.

    Returns the Cmm continuation id, a reference that will be set to true if a
    catch handler is needed, and the environment. *)
val add_inline_cont :
  t ->
  Continuation.t ->
  Bound_parameters.t ->
  handler_params_occurrences:Num_occurrences.t Variable.Map.t ->
  Flambda.Expr.t ->
  t

(** Register the given continuation as an exception handler and set up the extra
    mutable variables needed if the handler has extra arguments *)
val add_exn_handler :
  t ->
  Continuation.t ->
  Flambda_arity.t ->
  t * (Backend_var.t * Flambda_kind.t) list

(** Return whether the given continuation has been registered as an exception
    handler. *)
val is_exn_handler : t -> Continuation.t -> bool

(** Recover the mutable variables associated with the given continuation *)
val get_exn_extra_args : t -> Continuation.t -> Backend_var.t list

(** Return the binding for a given continuation. Will fail (i.e. assertion
    failure) if given an unbound continuation. *)
val get_k : t -> Continuation.t -> cont

(** Returns the jump id bound to a continuation. Will fail (assertion failure),
    if the continuation is not bound. *)
val get_jump_id : t -> Continuation.t -> int

(** {2 Sets of closures and slot offsets} *)

(** Wrapper around {!Slot_offsets.function_slot_offset}. *)
val function_slot_offset :
  t -> Function_slot.t -> Exported_offsets.function_slot_info

(** Wrapper around {!Slot_offsets.value_slot_offset}. *)
val value_slot_offset : t -> Value_slot.t -> Exported_offsets.value_slot_info

(** Wrapper around {!Slot_offsets.layout}. *)
val layout : t -> Set_of_closures.t -> Slot_offsets.layout

(** Add the given names to the current scope *)
val add_to_scope : t -> Code_id_or_symbol.Set.t -> t

(** Mark the given code id as deleted, so that [check_scope] will report an
    error if it is used. *)
val mark_code_id_as_deleted : t -> Code_id.t -> t

(** Check that the given name is in scope. If [allow_deleted] is [false], check
    that it is not declared as deleted. *)
val check_scope : allow_deleted:bool -> t -> Code_id_or_symbol.t -> t
