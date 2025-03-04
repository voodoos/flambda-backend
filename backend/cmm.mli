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

(* Second intermediate language (machine independent) *)

type machtype_component =
  | Val
  | Addr
  | Int
  | Float

(* - [Val] denotes a valid OCaml value: either a pointer to the beginning
     of a heap block, an infix pointer if it is preceded by the correct
     infix header, or a 2n+1 encoded integer.
   - [Int] is for integers (not necessarily 2n+1 encoded) and for
     pointers outside the heap.
   - [Addr] denotes pointers that are neither [Val] nor [Int], i.e.
     pointers into the heap that point in the middle of a heap block.
     Such derived pointers are produced by e.g. array indexing.
   - [Float] is for unboxed floating-point numbers.

The purpose of these types is twofold.  First, they guide register
allocation: type [Float] goes in FP registers, the other types go
into integer registers.  Second, they determine how local variables are
tracked by the GC:
   - Variables of type [Val] are GC roots.  If they are pointers, the
     GC will not deallocate the addressed heap block, and will update
     the local variable if the heap block moves.
   - Variables of type [Int] and [Float] are ignored by the GC.
     The GC does not change their values.
   - Variables of type [Addr] must never be live across an allocation
     point or function call.  They cannot be given as roots to the GC
     because they don't point after a well-formed block header of the
     kind that the GC needs.  However, the GC may move the block pointed
     into, invalidating the value of the [Addr] variable.
*)

type machtype = machtype_component array

val typ_void: machtype
val typ_val: machtype
val typ_addr: machtype
val typ_int: machtype
val typ_float: machtype

(** Least upper bound of two [machtype_component]s. *)
val lub_component
   : machtype_component
  -> machtype_component
  -> machtype_component

(** Returns [true] iff the first supplied [machtype_component] is greater than
    or equal to the second under the relation used by [lub_component]. *)
val ge_component
   : machtype_component
  -> machtype_component
  -> bool

type exttype =
  | XInt                                (**r OCaml value, word-sized integer *)
  | XInt32                              (**r 32-bit integer *)
  | XInt64                              (**r 64-bit integer  *)
  | XFloat                              (**r double-precision FP number  *)
(** A variant of [machtype] used to describe arguments
    to external C functions *)

val machtype_of_exttype: exttype -> machtype
val machtype_of_exttype_list: exttype list -> machtype

type integer_comparison = Lambda.integer_comparison =
  | Ceq | Cne | Clt | Cgt | Cle | Cge

val negate_integer_comparison: integer_comparison -> integer_comparison
val swap_integer_comparison: integer_comparison -> integer_comparison

type float_comparison = Lambda.float_comparison =
  | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

val negate_float_comparison: float_comparison -> float_comparison
val swap_float_comparison: float_comparison -> float_comparison

type label = int
val new_label: unit -> label
val set_label: label -> unit
val cur_label: unit -> label

type exit_label =
  | Return_lbl
  | Lbl of label

type rec_flag = Nonrecursive | Recursive

type prefetch_temporal_locality_hint = Nonlocal | Low | Moderate | High

type effects = No_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

type phantom_defining_expr =
  (* CR-soon mshinwell: Convert this to [Targetint.OCaml.t] (or whatever the
     representation of "target-width OCaml integers of type [int]"
     becomes when merged). *)
  | Cphantom_const_int of Targetint.t
  (** The phantom-let-bound variable is a constant integer.
      The argument must be the tagged representation of an integer within
      the range of type [int] on the target.  (Analogously to [Cconst_int].) *)
  | Cphantom_const_symbol of string
  (** The phantom-let-bound variable is an alias for a symbol. *)
  | Cphantom_var of Backend_var.t
  (** The phantom-let-bound variable is an alias for another variable.  The
      aliased variable must not be a bound by a phantom let. *)
  | Cphantom_offset_var of { var : Backend_var.t; offset_in_words : int; }
  (** The phantom-let-bound-variable's value is defined by adding the given
      number of words to the pointer contained in the given identifier. *)
  | Cphantom_read_field of { var : Backend_var.t; field : int; }
  (** The phantom-let-bound-variable's value is found by adding the given
      number of words to the pointer contained in the given identifier, then
      dereferencing. *)
  | Cphantom_read_symbol_field of { sym : string; field : int; }
  (** As for [Uphantom_read_var_field], but with the pointer specified by
      a symbol. *)
  | Cphantom_block of { tag : int; fields : Backend_var.t list; }
  (** The phantom-let-bound variable points at a block with the given
      structure. *)

type trywith_shared_label = int (* Same as Ccatch handlers *)

type trap_action =
  | Push of trywith_shared_label
  (** Add the corresponding handler to the trap stack. *)
  | Pop
  (** Remove the last handler from the trap stack. *)

type trywith_kind =
  | Regular
  (** Regular trywith: an uncaught exception from the body will always be
      handled by this handler. *)
  | Delayed of trywith_shared_label
  (** The body starts with the previous exception handler, and only after going
      through an explicit Push-annotated Cexit will this handler become active.
      This allows for sharing a single handler in several places, or having
      multiple entry and exit points to a single trywith block. *)

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word_int                           (* integer or pointer outside heap *)
  | Word_val                           (* pointer inside heap or encoded int *)
  | Single
  | Double                             (* word-aligned 64-bit float
                                          see PR#10433 *)
and operation =
    Capply of machtype * Lambda.region_close
  | Cextcall of
      { func: string;
        ty: machtype;
        ty_args : exttype list;
        alloc: bool;
        builtin: bool;
        returns: bool;
        effects: effects;
        coeffects: coeffects;
      }
      (** The [machtype] is the machine type of the result.
          The [exttype list] describes the unboxing types of the arguments.
          An empty list means "all arguments are machine words [XInt]". *)
  | Cload of memory_chunk * Asttypes.mutable_flag
  | Calloc of Lambda.alloc_mode
  | Cstore of memory_chunk * Lambda.initialization_or_assignment
  | Caddi | Csubi | Cmuli | Cmulhi of { signed: bool }  | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Cbswap of { bitwidth: bswap_bitwidth; }
  | Cclz of { arg_is_non_zero: bool; }
  | Cctz of { arg_is_non_zero: bool; }
  | Cpopcnt
  | Cprefetch of { is_write: bool; locality: prefetch_temporal_locality_hint; }
  | Ccmpi of integer_comparison
  | Caddv (* pointer addition that produces a [Val] (well-formed Caml value) *)
  | Cadda (* pointer addition that produces a [Addr] (derived heap pointer) *)
  | Ccmpa of integer_comparison
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of float_comparison
  | Craise of Lambda.raise_kind
  | Ccheckbound (* Takes two arguments : first the bound to check against,
                   then the index.
                   It results in a bounds error if the index is greater than
                   or equal to the bound. *)
  | Cprobe of { name: string; handler_code_sym: string; }
  | Cprobe_is_enabled of { name: string }
  | Copaque (* Sys.opaque_identity *)
  | Cbeginregion | Cendregion

type value_kind =
  | Vval of Lambda.value_kind (* Valid OCaml values *)
  | Vint (* Untagged integers and off-heap pointers *)
  | Vaddr (* Derived pointers *)
  | Vfloat (* Unboxed floating-point numbers *)

(** Every basic block should have a corresponding [Debuginfo.t] for its
    beginning. *)
type expression =
    Cconst_int of int * Debuginfo.t
  | Cconst_natint of nativeint * Debuginfo.t
  | Cconst_float of float * Debuginfo.t
  | Cconst_symbol of string * Debuginfo.t
  | Cvar of Backend_var.t
  | Clet of Backend_var.With_provenance.t * expression * expression
  | Clet_mut of Backend_var.With_provenance.t * machtype
                * expression * expression
  | Cphantom_let of Backend_var.With_provenance.t
      * phantom_defining_expr option * expression
  (* Cassign must refer to a variable bound by Clet_mut *)
  | Cassign of Backend_var.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list * Debuginfo.t
  | Csequence of expression * expression
  | Cifthenelse of expression * Debuginfo.t * expression
      * Debuginfo.t * expression * Debuginfo.t * value_kind
  | Cswitch of expression * int array * (expression * Debuginfo.t) array
      * Debuginfo.t * value_kind
  | Ccatch of
      rec_flag
        * (label * (Backend_var.With_provenance.t * machtype) list
          * expression * Debuginfo.t) list
        * expression
        * value_kind
  | Cexit of exit_label * expression list * trap_action list
  | Ctrywith of expression * trywith_kind * Backend_var.With_provenance.t
      * expression * Debuginfo.t * value_kind
  | Cregion of expression
  | Ctail of expression

type codegen_option =
  | Reduce_code_size
  | No_CSE
  | Use_linscan_regalloc

type fundecl =
  { fun_name: string;
    fun_args: (Backend_var.With_provenance.t * machtype) list;
    fun_body: expression;
    fun_codegen_options : codegen_option list;
    fun_dbg : Debuginfo.t;
  }

type data_item =
    Cdefine_symbol of string
  | Cglobal_symbol of string
  | Cint8 of int
  | Cint16 of int
  | Cint32 of nativeint
  | Cint of nativeint
  | Csingle of float
  | Cdouble of float
  | Csymbol_address of string
  | Cstring of string
  | Cskip of int
  | Calign of int

type phrase =
    Cfunction of fundecl
  | Cdata of data_item list

val ccatch :
     label * (Backend_var.With_provenance.t * machtype) list
       * expression * expression * Debuginfo.t * value_kind
  -> expression

val reset : unit -> unit

val iter_shallow_tail: (expression -> unit) -> expression -> bool
  (** Either apply the callback to all immediate sub-expressions that
      can produce the final result for the expression and return
      [true], or do nothing and return [false].  Note that the notion
      of "tail" sub-expression used here does not match the one used
      to trigger tail calls; in particular, try...with handlers are
      considered to be in tail position (because their result become
      the final result for the expression).  *)

val map_shallow_tail: ?kind:value_kind -> (expression -> expression) -> expression -> expression
  (** Apply the transformation to those immediate sub-expressions of an
      expression that are in tail position, using the same definition of "tail"
      as [iter_shallow_tail] *)

val map_tail: ?kind:value_kind -> (expression -> expression) -> expression -> expression
  (** Apply the transformation to an expression, trying to push it
      to all inner sub-expressions that can produce the final result,
      by recursively applying map_shallow_tail *)

val iter_shallow: (expression -> unit) -> expression -> unit
  (** Apply the callback to each immediate sub-expression. *)

val map_shallow: (expression -> expression) -> expression -> expression
  (** Apply the transformation to each immediate sub-expression. *)

val equal_machtype_component : machtype_component -> machtype_component -> bool
val equal_exttype : exttype -> exttype -> bool
val equal_float_comparison : float_comparison -> float_comparison -> bool
val equal_memory_chunk : memory_chunk -> memory_chunk -> bool
val equal_integer_comparison : integer_comparison -> integer_comparison -> bool
