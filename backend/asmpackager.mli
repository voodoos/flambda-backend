(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

val package_files
   : ppf_dump:Format.formatter
  -> Env.t
  -> string list
  -> string
  -> backend:(module Backend_intf.S)
  -> flambda2:(
    ppf_dump:Format.formatter ->
    prefixname:string ->
    filename:string ->
    module_ident:Ident.t ->
    module_block_size_in_words:int ->
    module_initializer:Lambda.lambda ->
    keep_symbol_tables:bool ->
    Cmm.phrase list)
  -> unit

type error =
    Illegal_renaming of string * string * string
  | Forward_reference of string * string
  | Wrong_for_pack of string * string
  | Linking_error
  | Assembler_error of string
  | File_not_found of string

exception Error of error

val report_error: Format.formatter -> error -> unit
