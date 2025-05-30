# 2 "printexc.ml"
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

open! Stdlib

[@@@ocaml.flambda_o3]

open Printf

type t = exn = ..

type printers = (exn -> string option) Modes.Portable.t list

let printers : printers Atomic.t = Atomic.make []

let locfmt () = format_of_string "File \"%s\", line %d, characters %d-%d: %s"

let field x i =
  let f = Obj.field x i in
  if not (Obj.is_block f) then
    sprintf "%d" (Obj.magic f : int)           (* can also be a char *)
  else if Obj.tag f = Obj.string_tag then
    sprintf "%S" (Obj.magic f : string)
  else if Obj.tag f = Obj.double_tag then
    string_of_float (Obj.magic f : float)
  else
    "_"

let rec other_fields x i =
  if i >= Obj.size x then ""
  else sprintf ", %s%s" (field x i) (other_fields x (i+1))

let fields x =
  match Obj.size x with
  | 0 -> ""
  | 1 -> ""
  | 2 -> sprintf "(%s)" (field x 1)
  | _ -> sprintf "(%s%s)" (field x 1) (other_fields x 2)

let use_printers x =
  let rec conv = function
    | { Modes.Portable.portable = hd } :: tl ->
        (match hd x with
         | None | exception _ -> conv tl
         | Some s -> Some s)
    | [] -> None in
  conv (Atomic.Contended.get printers)

let destruct_ext_constructor x =
  if Obj.tag x <> 0 then
    ((Obj.magic (Obj.field x 0) : string), None)
  else
    let constructor =
      (Obj.magic (Obj.field (Obj.field x 0) 0) : string) in
    (constructor, Some (fields x))

let string_of_extension_constructor t =
  let constructor, fields_opt = destruct_ext_constructor t in
  match fields_opt with
  | None -> constructor
  | Some f -> constructor ^ f

let to_string_default = function
  | Out_of_memory -> "Out of memory"
  | Stack_overflow -> "Stack overflow"
  | Match_failure(file, line, char) ->
      sprintf (locfmt ()) file line char (char+5) "Pattern matching failed"
  | Assert_failure(file, line, char) ->
      sprintf (locfmt ()) file line char (char+6) "Assertion failed"
  | Undefined_recursive_module(file, line, char) ->
      sprintf (locfmt ()) file line char (char+6) "Undefined recursive module"
  | x ->
      string_of_extension_constructor (Obj.repr x)

let to_string e =
  match use_printers e with
  | Some s -> s
  | None -> to_string_default e

let print fct arg =
  try
    fct arg
  with x ->
    eprintf "Uncaught exception: %s\n" (to_string x);
    flush stderr;
    raise x

let catch fct arg =
  try
    fct arg
  with x ->
    flush stdout;
    eprintf "Uncaught exception: %s\n" (to_string x);
    exit 2

type raw_backtrace_slot : immutable_data
type raw_backtrace_entry = private int
type raw_backtrace = raw_backtrace_entry array

let raw_backtrace_entries bt = bt

external get_raw_backtrace:
  unit -> raw_backtrace @@ portable = "caml_get_exception_raw_backtrace"

external raise_with_backtrace: ('a : value_or_null).
  exn -> raw_backtrace -> 'a @ portable unique @@ portable
  = "%raise_with_backtrace"

(* Disable warning 37: values are constructed in the runtime *)
type[@warning "-37"] backtrace_slot =
  | Known_location of {
      is_raise   : bool;
      filename   : string;
      start_lnum : int;
      start_char : int;
      end_offset : int; (* Relative to beginning of start_lnum *)
      end_lnum   : int;
      end_char   : int; (* Relative to beginning of end_lnum line *)
      is_inline  : bool;
      defname    : string;
    }
  | Unknown_location of {
      is_raise : bool
    }

external convert_raw_backtrace_slot:
  raw_backtrace_slot -> backtrace_slot @@ portable = "caml_convert_raw_backtrace_slot"

external convert_raw_backtrace:
  raw_backtrace -> backtrace_slot array @@ portable = "caml_convert_raw_backtrace"

let convert_raw_backtrace bt =
  try Some (convert_raw_backtrace bt)
  with Failure _ -> None

let format_backtrace_slot pos slot =
  let info is_raise =
    if is_raise then
      if pos = 0 then "Raised at" else "Re-raised at"
    else
      if pos = 0 then "Raised by primitive operation at" else "Called from"
  in
  match slot with
  | Unknown_location l ->
      if l.is_raise then
        (* compiler-inserted re-raise, skipped *) None
      else
        Some (sprintf "%s unknown location" (info false))
  | Known_location l ->
      let lines =
        if l.start_lnum = l.end_lnum then
          Printf.sprintf " %d" l.start_lnum
        else
          Printf.sprintf "s %d-%d" l.start_lnum l.end_lnum
      in
      Some (sprintf "%s %s in file \"%s\"%s, line%s, characters %d-%d"
              (info l.is_raise) l.defname l.filename
              (if l.is_inline then " (inlined)" else "")
              lines l.start_char l.end_offset)

let print_exception_backtrace outchan backtrace =
  match backtrace with
  | None ->
      fprintf outchan
        "(Program not linked with -g, cannot print stack backtrace)\n"
  | Some a ->
      for i = 0 to Array.length a - 1 do
        match format_backtrace_slot i a.(i) with
          | None -> ()
          | Some str -> fprintf outchan "%s\n" str
      done

let print_raw_backtrace outchan raw_backtrace =
  print_exception_backtrace outchan (convert_raw_backtrace raw_backtrace)

(* confusingly named: prints the global current backtrace *)
let print_backtrace outchan =
  print_raw_backtrace outchan (get_raw_backtrace ())

let backtrace_to_string backtrace =
  match backtrace with
  | None ->
     "(Program not linked with -g, cannot print stack backtrace)\n"
  | Some a ->
      let b = Buffer.create 1024 in
      for i = 0 to Array.length a - 1 do
        match format_backtrace_slot i a.(i) with
          | None -> ()
          | Some str -> bprintf b "%s\n" str
      done;
      Buffer.contents b

let raw_backtrace_to_string raw_backtrace =
  backtrace_to_string (convert_raw_backtrace raw_backtrace)

let backtrace_slot_is_raise = function
  | Known_location l -> l.is_raise
  | Unknown_location l -> l.is_raise

let backtrace_slot_is_inline = function
  | Known_location l -> l.is_inline
  | Unknown_location _ -> false

type location = {
  filename : string;
  line_number : int;
  start_char : int;
  end_char : int;
  end_line : int;
  end_col : int;
}

let backtrace_slot_location = function
  | Unknown_location _ -> None
  | Known_location l ->
    Some {
      filename    = l.filename;
      line_number = l.start_lnum;
      start_char  = l.start_char;
      end_char    = l.end_offset;
      end_line    = l.end_lnum;
      end_col     = l.end_char;
    }

let backtrace_slot_defname = function
  | Unknown_location _
  | Known_location { defname = "" } -> None
  | Known_location l -> Some l.defname

let backtrace_slots raw_backtrace =
  (* The documentation of this function guarantees that Some is
     returned only if a part of the trace is usable. This gives us
     a bit more work than just convert_raw_backtrace, but it makes the
     API more user-friendly -- otherwise most users would have to
     reimplement the "Program not linked with -g, sorry" logic
     themselves. *)
  match convert_raw_backtrace raw_backtrace with
    | None -> None
    | Some backtrace ->
      let usable_slot = function
        | Unknown_location _ -> false
        | Known_location _ -> true in
      let rec exists_usable = function
        | (-1) -> false
        | i -> usable_slot backtrace.(i) || exists_usable (i - 1) in
      if exists_usable (Array.length backtrace - 1)
      then Some backtrace
      else None

let backtrace_slots_of_raw_entry entry =
  backtrace_slots [| entry |]

module Slot = struct
  type t = backtrace_slot
  let format = format_backtrace_slot
  let is_raise = backtrace_slot_is_raise
  let is_inline = backtrace_slot_is_inline
  let location = backtrace_slot_location
  let name = backtrace_slot_defname
end

let raw_backtrace_length bt = Array.length bt

external get_raw_backtrace_slot :
  raw_backtrace -> int -> raw_backtrace_slot @@ portable = "caml_raw_backtrace_slot"

external get_raw_backtrace_next_slot :
  raw_backtrace_slot -> raw_backtrace_slot option @@ portable
  = "caml_raw_backtrace_next_slot"

(* confusingly named:
   returns the *string* corresponding to the global current backtrace *)
let get_backtrace () = raw_backtrace_to_string (get_raw_backtrace ())

external record_backtrace: bool -> unit @@ portable = "caml_record_backtrace"
external backtrace_status: unit -> bool @@ portable = "caml_backtrace_status"

let rec register_printer_safe fn =
  let old_printers = Atomic.Contended.get printers in
  let new_printers = { Modes.Portable.portable = fn } :: old_printers in
  let success = Atomic.Contended.compare_and_set printers old_printers new_printers in
  if not success then register_printer_safe fn

let register_printer_unsafe fn = register_printer_safe (Obj.magic_portable fn)

external get_callstack: int -> raw_backtrace @@ portable = "caml_get_current_callstack"

let exn_slot x =
  let x = Obj.repr x in
  if Obj.tag x = 0 then Obj.field x 0 else x

let exn_slot_id x =
  let slot = exn_slot x in
  (Obj.obj (Obj.field slot 1) : int)

let exn_slot_name x =
  let slot = exn_slot x in
  (Obj.obj (Obj.field slot 0) : string)

external get_debug_info_status : unit -> int @@ portable = "caml_ml_debug_info_status"

(* Descriptions for errors in startup.h. See also backtrace.c *)
let errors = [| "";
  (* FILE_NOT_FOUND *)
  "(Cannot print locations:\n \
      bytecode executable program file not found)";
  (* BAD_BYTECODE *)
  "(Cannot print locations:\n \
      bytecode executable program file appears to be corrupt)";
  (* WRONG_MAGIC *)
  "(Cannot print locations:\n \
      bytecode executable program file has wrong magic number)";
  (* NO_FDS *)
  "(Cannot print locations:\n \
      bytecode executable program file cannot be opened;\n \
      -- too many open files. Try running with OCAMLRUNPARAM=b=2)"
|]

let default_uncaught_exception_handler exn raw_backtrace =
  eprintf "Fatal error: exception %s\n" (to_string exn);
  print_raw_backtrace stderr raw_backtrace;
  let status = get_debug_info_status () in
  let errors =
    (* This magic is safe, since [errors] is never mutated. *)
    Obj.magic_uncontended errors
  in
  if status < 0 then
    prerr_endline errors.(abs status);
  flush stderr

let uncaught_exception_handler =
  Atomic.make { Modes.Portable.portable = default_uncaught_exception_handler }

let set_uncaught_exception_handler_safe fn =
  Atomic.Contended.set uncaught_exception_handler { Modes.Portable.portable = fn }

let set_uncaught_exception_handler_unsafe fn =
  set_uncaught_exception_handler_safe (Obj.magic_portable fn)

let empty_backtrace : raw_backtrace = [| |]

let try_get_raw_backtrace () =
  try
    get_raw_backtrace ()
  with _ (* Out_of_memory? *) ->
    empty_backtrace

let handle_uncaught_exception' exn debugger_in_use =
  try
    (* Get the backtrace now, in case one of the [at_exit] function
       destroys it. *)
    let raw_backtrace =
      if debugger_in_use (* Same test as in [runtime/printexc.c] *) then
        empty_backtrace
      else
        try_get_raw_backtrace ()
    in
    (try Stdlib.do_at_exit () with _ -> ());
    try
      (Atomic.Contended.get uncaught_exception_handler).portable exn raw_backtrace
    with exn' ->
      let raw_backtrace' = try_get_raw_backtrace () in
      eprintf "Fatal error: exception %s\n" (to_string exn);
      print_raw_backtrace stderr raw_backtrace;
      eprintf "Fatal error in uncaught exception handler: exception %s\n"
        (to_string exn');
      print_raw_backtrace stderr raw_backtrace';
      flush stderr
  with
    | Out_of_memory ->
        prerr_endline
          "Fatal error: out of memory in uncaught exception handler"

(* This function is called by [caml_fatal_uncaught_exception] in
   [runtime/printexc.c] which expects no exception is raised. *)
let handle_uncaught_exception exn debugger_in_use =
  try
    handle_uncaught_exception' exn debugger_in_use
  with _ ->
    (* There is not much we can do at this point *)
    ()

external register_named_value : string -> 'a -> unit
  = "caml_register_named_value"

let () =
  register_named_value "Printexc.handle_uncaught_exception"
    handle_uncaught_exception

module Safe = struct
  let set_uncaught_exception_handler = set_uncaught_exception_handler_safe
  let register_printer = register_printer_safe
end

let set_uncaught_exception_handler = set_uncaught_exception_handler_unsafe
let register_printer = register_printer_unsafe
