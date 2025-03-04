(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Damien Doligez, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* String operations, based on byte sequence operations *)

external raise : exn -> 'a = "%raise"

external raise_notrace : exn -> 'a = "%raise_notrace"

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

external __LOC__ : string = "%loc_LOC"

external __FILE__ : string = "%loc_FILE"

external __LINE__ : int = "%loc_LINE"

external __MODULE__ : string = "%loc_MODULE"

external __POS__ : string * int * int * int = "%loc_POS"

external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"

external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"

external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"

external ( = ) : 'a -> 'a -> bool = "%equal"

external ( <> ) : 'a -> 'a -> bool = "%notequal"

external ( < ) : 'a -> 'a -> bool = "%lessthan"

external ( > ) : 'a -> 'a -> bool = "%greaterthan"

external ( <= ) : 'a -> 'a -> bool = "%lessequal"

external ( >= ) : 'a -> 'a -> bool = "%greaterequal"

external compare : 'a -> 'a -> int = "%compare"

external ( == ) : 'a -> 'a -> bool = "%eq"

external ( != ) : 'a -> 'a -> bool = "%noteq"

external not : bool -> bool = "%boolnot"

external ( & ) : bool -> bool -> bool = "%sequand"

external ( && ) : bool -> bool -> bool = "%sequand"

external ( or ) : bool -> bool -> bool = "%sequor"

external ( || ) : bool -> bool -> bool = "%sequor"

external ( ~- ) : int -> int = "%negint"

external ( ~+ ) : int -> int = "%identity"

external succ : int -> int = "%succint"

external pred : int -> int = "%predint"

external ( + ) : int -> int -> int = "%addint"

external ( - ) : int -> int -> int = "%subint"

external ( * ) : int -> int -> int = "%mulint"

external ( / ) : int -> int -> int = "%divint"

external ( mod ) : int -> int -> int = "%modint"

external ( land ) : int -> int -> int = "%andint"

external ( lor ) : int -> int -> int = "%orint"

external ( lxor ) : int -> int -> int = "%xorint"

external ( lsl ) : int -> int -> int = "%lslint"

external ( lsr ) : int -> int -> int = "%lsrint"

external ( asr ) : int -> int -> int = "%asrint"

external ( ~-. ) : float -> float = "%negfloat"

external ( ~+. ) : float -> float = "%identity"

external ( +. ) : float -> float -> float = "%addfloat"

external ( -. ) : float -> float -> float = "%subfloat"

external ( *. ) : float -> float -> float = "%mulfloat"

external ( /. ) : float -> float -> float = "%divfloat"

external ( ** ) : float -> float -> float = "caml_power_float" "pow"

external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]

external expm1 : float -> float = "caml_expm1_float" "caml_expm1"

external acos : float -> float = "caml_acos_float" "acos"

external asin : float -> float = "caml_asin_float" "asin"

external atan : float -> float = "caml_atan_float" "atan"

external atan2 : float -> float -> float = "caml_atan2_float" "atan2"

external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]

external cosh : float -> float = "caml_cosh_float" "cosh"

external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]

external log10 : float -> float = "caml_log10_float" "log10"

external log1p : float -> float = "caml_log1p_float" "caml_log1p"

external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]

external sinh : float -> float = "caml_sinh_float" "sinh"

external sqrt : float -> float = "caml_sqrt_float" "sqrt"

external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]

external tanh : float -> float = "caml_tanh_float" "tanh"

external ceil : float -> float = "caml_ceil_float" "ceil"

external floor : float -> float = "caml_floor_float" "floor"

external abs_float : float -> float = "%absfloat"

external mod_float : float -> float -> float = "caml_fmod_float" "fmod"

external frexp : float -> float * int = "caml_frexp_float"

external modf : float -> float * float = "caml_modf_float"

external float : int -> float = "%floatofint"

external float_of_int : int -> float = "%floatofint"

external truncate : float -> int = "%intoffloat"

external int_of_float : float -> int = "%intoffloat"

external string_length : string -> int = "%string_length"

external bytes_length : bytes -> int = "%bytes_length"

external bytes_create : int -> bytes = "caml_create_bytes"

external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"

external int_of_char : char -> int = "%identity"

external unsafe_char_of_int : int -> char = "%identity"

external ignore : 'a -> unit = "%ignore"

external fst : 'a * 'b -> 'a = "%field0"

external snd : 'a * 'b -> 'b = "%field1"

type 'a ref = { mutable contents : 'a }

external ref : 'a -> 'a ref = "%makemutable"

external ( ! ) : 'a ref -> 'a = "%field0"

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

external incr : int ref -> unit = "%incr"

external decr : int ref -> unit = "%decr"

external format_int : string -> int -> string = "caml_format_int"

external format_float : string -> float -> string = "caml_format_float"

external int_of_string : string -> int = "caml_int_of_string"

external string_get : string -> int -> char = "%string_safe_get"

external float_of_string : string -> float = "caml_float_of_string"

external sys_exit : int -> 'a = "caml_sys_exit"

let failwith s = raise (Failure s)

let invalid_arg s = raise (Invalid_argument s)

let rec ( @ ) l1 l2 = match l1 with [] -> l2 | hd :: tl -> hd :: (tl @ l2)
(* WARNING: Some functions in this file are duplicated in bytes.ml for
   efficiency reasons. When you modify the one in this file you need to modify
   its duplicate in bytes.ml. These functions have a "duplicated" comment above
   their definition. *)

external length : string -> int = "%string_length"

external get : string -> int -> char = "%string_safe_get"

external set : bytes -> int -> char -> unit = "%string_safe_set"

external create : int -> bytes = "caml_create_string"

external unsafe_get : string -> int -> char = "%string_unsafe_get"

external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"

external unsafe_blit : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string"
  [@@noalloc]

external unsafe_fill : bytes -> int -> int -> char -> unit = "caml_fill_string"
  [@@noalloc]

module B = Bytes

let bts = B.unsafe_to_string

let bos = B.unsafe_of_string

let make n c = B.make n c |> bts

let init n f = B.init n f |> bts

let copy s = B.copy (bos s) |> bts

let sub s ofs len = B.sub (bos s) ofs len |> bts

let fill = B.fill

let blit = B.blit_string

let ensure_ge (x : int) y = if x >= y then x else invalid_arg "String.concat"

let rec sum_lengths acc seplen = function
  | [] -> acc
  | [hd] -> length hd + acc
  | hd :: tl -> sum_lengths (ensure_ge (length hd + seplen + acc) acc) seplen tl

let rec unsafe_blits dst pos sep seplen = function
  | [] -> dst
  | [hd] ->
    unsafe_blit hd 0 dst pos (length hd);
    dst
  | hd :: tl ->
    unsafe_blit hd 0 dst pos (length hd);
    unsafe_blit sep 0 dst (pos + length hd) seplen;
    unsafe_blits dst (pos + length hd + seplen) sep seplen tl

let concat sep = function
  | [] -> ""
  | l ->
    let seplen = length sep in
    bts @@ unsafe_blits (B.create (sum_lengths 0 seplen l)) 0 sep seplen l

(* duplicated in bytes.ml *)
let iter f s =
  for i = 0 to length s - 1 do
    f (unsafe_get s i)
  done

(* duplicated in bytes.ml *)
let iteri f s =
  for i = 0 to length s - 1 do
    f i (unsafe_get s i)
  done

let map f s = B.map f (bos s) |> bts

let mapi f s = B.mapi f (bos s) |> bts

(* Beware: we cannot use B.trim or B.escape because they always make a copy, but
   String.mli spells out some cases where we are not allowed to make a copy. *)

let is_space = function ' ' | '\012' | '\n' | '\r' | '\t' -> true | _ -> false

let trim s =
  if s = ""
  then s
  else if is_space (unsafe_get s 0) || is_space (unsafe_get s (length s - 1))
  then bts (B.trim (bos s))
  else s

let escaped s =
  let rec escape_if_needed s n i =
    if i >= n
    then s
    else
      match unsafe_get s i with
      | '\"' | '\\' | '\000' .. '\031' | '\127' .. '\255' ->
        bts (B.escaped (bos s))
      | _ -> escape_if_needed s n (i + 1)
  in
  escape_if_needed s (length s) 0

(* duplicated in bytes.ml *)
let rec index_rec s lim i c =
  if i >= lim
  then raise Not_found
  else if unsafe_get s i = c
  then i
  else index_rec s lim (i + 1) c

(* duplicated in bytes.ml *)
let index s c = index_rec s (length s) 0 c

(* duplicated in bytes.ml *)
let rec index_rec_opt s lim i c =
  if i >= lim
  then None
  else if unsafe_get s i = c
  then Some i
  else index_rec_opt s lim (i + 1) c

(* duplicated in bytes.ml *)
let index_opt s c = index_rec_opt s (length s) 0 c

(* duplicated in bytes.ml *)
let index_from s i c =
  let l = length s in
  if i < 0 || i > l
  then invalid_arg "String.index_from / Bytes.index_from"
  else index_rec s l i c

(* duplicated in bytes.ml *)
let index_from_opt s i c =
  let l = length s in
  if i < 0 || i > l
  then invalid_arg "String.index_from_opt / Bytes.index_from_opt"
  else index_rec_opt s l i c

(* duplicated in bytes.ml *)
let rec rindex_rec s i c =
  if i < 0
  then raise Not_found
  else if unsafe_get s i = c
  then i
  else rindex_rec s (i - 1) c

(* duplicated in bytes.ml *)
let rindex s c = rindex_rec s (length s - 1) c

(* duplicated in bytes.ml *)
let rindex_from s i c =
  if i < -1 || i >= length s
  then invalid_arg "String.rindex_from / Bytes.rindex_from"
  else rindex_rec s i c

(* duplicated in bytes.ml *)
let rec rindex_rec_opt s i c =
  if i < 0
  then None
  else if unsafe_get s i = c
  then Some i
  else rindex_rec_opt s (i - 1) c

(* duplicated in bytes.ml *)
let rindex_opt s c = rindex_rec_opt s (length s - 1) c

(* duplicated in bytes.ml *)
let rindex_from_opt s i c =
  if i < -1 || i >= length s
  then invalid_arg "String.rindex_from_opt / Bytes.rindex_from_opt"
  else rindex_rec_opt s i c

(* duplicated in bytes.ml *)
let contains_from s i c =
  let l = length s in
  if i < 0 || i > l
  then invalid_arg "String.contains_from / Bytes.contains_from"
  else
    try
      ignore (index_rec s l i c);
      true
    with Not_found -> false

(* duplicated in bytes.ml *)
let contains s c = contains_from s 0 c

(* duplicated in bytes.ml *)
let rcontains_from s i c =
  if i < 0 || i >= length s
  then invalid_arg "String.rcontains_from / Bytes.rcontains_from"
  else
    try
      ignore (rindex_rec s i c);
      true
    with Not_found -> false

let uppercase_ascii s = B.uppercase_ascii (bos s) |> bts

let lowercase_ascii s = B.lowercase_ascii (bos s) |> bts

let capitalize_ascii s = B.capitalize_ascii (bos s) |> bts

let uncapitalize_ascii s = B.uncapitalize_ascii (bos s) |> bts

type t = string

let compare (x : t) (y : t) = compare x y

external equal : string -> string -> bool = "caml_string_equal" [@@noalloc]

let split_on_char sep s =
  let r = ref [] in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if unsafe_get s i = sep
    then begin
      r := sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  sub s 0 !j :: !r

(* Deprecated functions implemented via other deprecated functions *)
[@@@ocaml.warning "-3"]

let uppercase s = B.uppercase (bos s) |> bts

let lowercase s = B.lowercase (bos s) |> bts

let capitalize s = B.capitalize (bos s) |> bts

let uncapitalize s = B.uncapitalize (bos s) |> bts

(** {1 Iterators} *)

let to_seq s = bos s |> B.to_seq

let to_seqi s = bos s |> B.to_seqi

let of_seq g = B.of_seq g |> bts
