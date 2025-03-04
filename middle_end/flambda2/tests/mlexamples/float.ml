(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Stdlib = struct
  external register_named_value : string -> 'a -> unit
    = "caml_register_named_value"

  let () =
    (* for runtime/fail_nat.c *)
    register_named_value "Pervasives.array_bound_error"
      (Invalid_argument "index out of bounds")

  external raise : exn -> 'a = "%raise"

  external raise_notrace : exn -> 'a = "%raise_notrace"

  let failwith s = raise (Failure s)

  let invalid_arg s = raise (Invalid_argument s)

  exception Exit

  exception Match_failure = Match_failure

  exception Assert_failure = Assert_failure

  exception Invalid_argument = Invalid_argument

  exception Failure = Failure

  exception Not_found = Not_found

  exception Out_of_memory = Out_of_memory

  exception Stack_overflow = Stack_overflow

  exception Sys_error = Sys_error

  exception End_of_file = End_of_file

  exception Division_by_zero = Division_by_zero

  exception Sys_blocked_io = Sys_blocked_io

  exception Undefined_recursive_module = Undefined_recursive_module

  (* Composition operators *)

  external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

  external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

  (* Debugging *)

  external __LOC__ : string = "%loc_LOC"

  external __FILE__ : string = "%loc_FILE"

  external __LINE__ : int = "%loc_LINE"

  external __MODULE__ : string = "%loc_MODULE"

  external __POS__ : string * int * int * int = "%loc_POS"

  external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"

  external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"

  external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"

  (* Comparisons *)

  external ( = ) : 'a -> 'a -> bool = "%equal"

  external ( <> ) : 'a -> 'a -> bool = "%notequal"

  external ( < ) : 'a -> 'a -> bool = "%lessthan"

  external ( > ) : 'a -> 'a -> bool = "%greaterthan"

  external ( <= ) : 'a -> 'a -> bool = "%lessequal"

  external ( >= ) : 'a -> 'a -> bool = "%greaterequal"

  external compare : 'a -> 'a -> int = "%compare"

  let min x y = if x <= y then x else y

  let max x y = if x >= y then x else y

  external ( == ) : 'a -> 'a -> bool = "%eq"

  external ( != ) : 'a -> 'a -> bool = "%noteq"

  (* Boolean operations *)

  external not : bool -> bool = "%boolnot"

  external ( & ) : bool -> bool -> bool = "%sequand"

  external ( && ) : bool -> bool -> bool = "%sequand"

  external ( or ) : bool -> bool -> bool = "%sequor"

  external ( || ) : bool -> bool -> bool = "%sequor"

  (* Integer operations *)

  external ( ~- ) : int -> int = "%negint"

  external ( ~+ ) : int -> int = "%identity"

  external succ : int -> int = "%succint"

  external pred : int -> int = "%predint"

  external ( + ) : int -> int -> int = "%addint"

  external ( - ) : int -> int -> int = "%subint"

  external ( * ) : int -> int -> int = "%mulint"

  external ( / ) : int -> int -> int = "%divint"

  external ( mod ) : int -> int -> int = "%modint"

  let abs x = if x >= 0 then x else -x

  external ( land ) : int -> int -> int = "%andint"

  external ( lor ) : int -> int -> int = "%orint"

  external ( lxor ) : int -> int -> int = "%xorint"

  let lnot x = x lxor -1

  external ( lsl ) : int -> int -> int = "%lslint"

  external ( lsr ) : int -> int -> int = "%lsrint"

  external ( asr ) : int -> int -> int = "%asrint"

  let max_int = -1 lsr 1

  let min_int = max_int + 1

  (* Floating-point operations *)

  external ( ~-. ) : float -> float = "%negfloat"

  external ( ~+. ) : float -> float = "%identity"

  external ( +. ) : float -> float -> float = "%addfloat"

  external ( -. ) : float -> float -> float = "%subfloat"

  external ( *. ) : float -> float -> float = "%mulfloat"

  external ( /. ) : float -> float -> float = "%divfloat"

  external ( ** ) : float -> float -> float = "caml_power_float" "pow"
    [@@unboxed] [@@noalloc]

  external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]

  external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
    [@@unboxed] [@@noalloc]

  external acos : float -> float = "caml_acos_float" "acos"
    [@@unboxed] [@@noalloc]

  external asin : float -> float = "caml_asin_float" "asin"
    [@@unboxed] [@@noalloc]

  external atan : float -> float = "caml_atan_float" "atan"
    [@@unboxed] [@@noalloc]

  external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
    [@@unboxed] [@@noalloc]

  external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
    [@@unboxed] [@@noalloc]

  external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]

  external cosh : float -> float = "caml_cosh_float" "cosh"
    [@@unboxed] [@@noalloc]

  external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]

  external log10 : float -> float = "caml_log10_float" "log10"
    [@@unboxed] [@@noalloc]

  external log1p : float -> float = "caml_log1p_float" "caml_log1p"
    [@@unboxed] [@@noalloc]

  external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]

  external sinh : float -> float = "caml_sinh_float" "sinh"
    [@@unboxed] [@@noalloc]

  external sqrt : float -> float = "caml_sqrt_float" "sqrt"
    [@@unboxed] [@@noalloc]

  external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]

  external tanh : float -> float = "caml_tanh_float" "tanh"
    [@@unboxed] [@@noalloc]

  external ceil : float -> float = "caml_ceil_float" "ceil"
    [@@unboxed] [@@noalloc]

  external floor : float -> float = "caml_floor_float" "floor"
    [@@unboxed] [@@noalloc]

  external abs_float : float -> float = "%absfloat"

  external copysign : float -> float -> float
    = "caml_copysign_float" "caml_copysign"
    [@@unboxed] [@@noalloc]

  external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
    [@@unboxed] [@@noalloc]

  external frexp : float -> float * int = "caml_frexp_float"

  external ldexp : (float[@unboxed]) -> (int[@untagged]) -> (float[@unboxed])
    = "caml_ldexp_float" "caml_ldexp_float_unboxed"
    [@@noalloc]

  external modf : float -> float * float = "caml_modf_float"

  external float : int -> float = "%floatofint"

  external float_of_int : int -> float = "%floatofint"

  external truncate : float -> int = "%intoffloat"

  external int_of_float : float -> int = "%intoffloat"

  external float_of_bits : int64 -> float
    = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
    [@@unboxed] [@@noalloc]

  let infinity = float_of_bits 0x7F_F0_00_00_00_00_00_00L

  let neg_infinity = float_of_bits 0xFF_F0_00_00_00_00_00_00L

  let nan = float_of_bits 0x7F_F8_00_00_00_00_00_01L

  let max_float = float_of_bits 0x7F_EF_FF_FF_FF_FF_FF_FFL

  let min_float = float_of_bits 0x00_10_00_00_00_00_00_00L

  let epsilon_float = float_of_bits 0x3C_B0_00_00_00_00_00_00L

  type fpclass =
    | FP_normal
    | FP_subnormal
    | FP_zero
    | FP_infinite
    | FP_nan

  external classify_float : (float[@unboxed]) -> fpclass
    = "caml_classify_float" "caml_classify_float_unboxed"
    [@@noalloc]

  type 'a ref = { mutable contents : 'a }

  external ref : 'a -> 'a ref = "%makemutable"

  external ( ! ) : 'a ref -> 'a = "%field0"

  external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

  external incr : int ref -> unit = "%incr"

  external decr : int ref -> unit = "%decr"
  (* String and byte sequence operations -- more in modules String and Bytes *)

  external string_length : string -> int = "%string_length"

  external bytes_length : bytes -> int = "%bytes_length"

  external bytes_create : int -> bytes = "caml_create_bytes"

  external string_blit : string -> int -> bytes -> int -> int -> unit
    = "caml_blit_string"
    [@@noalloc]

  external bytes_blit : bytes -> int -> bytes -> int -> int -> unit
    = "caml_blit_bytes"
    [@@noalloc]

  external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"

  let ( ^ ) s1 s2 =
    let l1 = string_length s1 and l2 = string_length s2 in
    let s = bytes_create (l1 + l2) in
    string_blit s1 0 s 0 l1;
    string_blit s2 0 s l1 l2;
    bytes_unsafe_to_string s

  external format_int : string -> int -> string = "caml_format_int"

  external format_float : string -> float -> string = "caml_format_float"

  let string_of_bool b = if b then "true" else "false"

  let bool_of_string = function
    | "true" -> true
    | "false" -> false
    | _ -> invalid_arg "bool_of_string"

  let bool_of_string_opt = function
    | "true" -> Some true
    | "false" -> Some false
    | _ -> None

  let string_of_int n = format_int "%d" n

  external int_of_string : string -> int = "caml_int_of_string"

  let int_of_string_opt s =
    (* TODO: provide this directly as a non-raising primitive. *)
    try Some (int_of_string s) with Failure _ -> None

  external string_get : string -> int -> char = "%string_safe_get"

  let valid_float_lexem s =
    let l = string_length s in
    let rec loop i =
      if i >= l
      then s ^ "."
      else match string_get s i with '0' .. '9' | '-' -> loop (i + 1) | _ -> s
    in
    loop 0

  let string_of_float f = valid_float_lexem (format_float "%.12g" f)

  external float_of_string : string -> float = "caml_float_of_string"

  let float_of_string_opt s =
    (* TODO: provide this directly as a non-raising primitive. *)
    try Some (float_of_string s) with Failure _ -> None
end

open Stdlib

external neg : float -> float = "%negfloat"

external add : float -> float -> float = "%addfloat"

external sub : float -> float -> float = "%subfloat"

external mul : float -> float -> float = "%mulfloat"

external div : float -> float -> float = "%divfloat"

external rem : float -> float -> float = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc]

external fma : float -> float -> float -> float = "caml_fma_float" "caml_fma"
  [@@unboxed] [@@noalloc]

external abs : float -> float = "%absfloat"

let zero = 0.

let one = 1.

let minus_one = -1.

let infinity = Stdlib.infinity

let neg_infinity = Stdlib.neg_infinity

let nan = Stdlib.nan

let is_finite (x : float) = x -. x = 0.

let is_infinite (x : float) = 1. /. x = 0.

let is_nan (x : float) = x <> x

let pi = 0x1.921fb54442d18p+1

let max_float = Stdlib.max_float

let min_float = Stdlib.min_float

let epsilon = Stdlib.epsilon_float

external of_int : int -> float = "%floatofint"

external to_int : float -> int = "%intoffloat"

external of_string : string -> float = "caml_float_of_string"

let of_string_opt = Stdlib.float_of_string_opt

let to_string = Stdlib.string_of_float

type fpclass = Stdlib.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

external classify_float : (float[@unboxed]) -> fpclass
  = "caml_classify_float" "caml_classify_float_unboxed"
  [@@noalloc]

external pow : float -> float -> float = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]

external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]

external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]

external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]

external log10 : float -> float = "caml_log10_float" "log10"
  [@@unboxed] [@@noalloc]

external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
  [@@unboxed] [@@noalloc]

external log1p : float -> float = "caml_log1p_float" "caml_log1p"
  [@@unboxed] [@@noalloc]

external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]

external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]

external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]

external acos : float -> float = "caml_acos_float" "acos"
  [@@unboxed] [@@noalloc]

external asin : float -> float = "caml_asin_float" "asin"
  [@@unboxed] [@@noalloc]

external atan : float -> float = "caml_atan_float" "atan"
  [@@unboxed] [@@noalloc]

external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc]

external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
  [@@unboxed] [@@noalloc]

external cosh : float -> float = "caml_cosh_float" "cosh"
  [@@unboxed] [@@noalloc]

external sinh : float -> float = "caml_sinh_float" "sinh"
  [@@unboxed] [@@noalloc]

external tanh : float -> float = "caml_tanh_float" "tanh"
  [@@unboxed] [@@noalloc]

external trunc : float -> float = "caml_trunc_float" "caml_trunc"
  [@@unboxed] [@@noalloc]

external round : float -> float = "caml_round_float" "caml_round"
  [@@unboxed] [@@noalloc]

external ceil : float -> float = "caml_ceil_float" "ceil"
  [@@unboxed] [@@noalloc]

external floor : float -> float = "caml_floor_float" "floor"
  [@@unboxed] [@@noalloc]

let is_integer x = x = trunc x && is_finite x

external next_after : float -> float -> float
  = "caml_nextafter_float" "caml_nextafter"
  [@@unboxed] [@@noalloc]

let succ x = next_after x infinity

let pred x = next_after x neg_infinity

external copy_sign : float -> float -> float
  = "caml_copysign_float" "caml_copysign"
  [@@unboxed] [@@noalloc]

external sign_bit : (float[@unboxed]) -> bool
  = "caml_signbit_float" "caml_signbit"
  [@@noalloc]

external frexp : float -> float * int = "caml_frexp_float"

external ldexp : (float[@unboxed]) -> (int[@untagged]) -> (float[@unboxed])
  = "caml_ldexp_float" "caml_ldexp_float_unboxed"
  [@@noalloc]

external modf : float -> float * float = "caml_modf_float"

type t = float

external compare : float -> float -> int = "%compare"

let equal x y = compare x y = 0

let[@inline] min (x : float) (y : float) =
  if y > x || ((not (sign_bit y)) && sign_bit x)
  then if is_nan y then y else x
  else if is_nan x
  then x
  else y

let[@inline] max (x : float) (y : float) =
  if y > x || ((not (sign_bit y)) && sign_bit x)
  then if is_nan x then x else y
  else if is_nan y
  then y
  else x

let[@inline] min_max (x : float) (y : float) =
  if is_nan x || is_nan y
  then nan, nan
  else if y > x || ((not (sign_bit y)) && sign_bit x)
  then x, y
  else y, x

let[@inline] min_num (x : float) (y : float) =
  if y > x || ((not (sign_bit y)) && sign_bit x)
  then if is_nan x then y else x
  else if is_nan y
  then x
  else y

let[@inline] max_num (x : float) (y : float) =
  if y > x || ((not (sign_bit y)) && sign_bit x)
  then if is_nan y then x else y
  else if is_nan x
  then y
  else x

let[@inline] min_max_num (x : float) (y : float) =
  if is_nan x
  then y, y
  else if is_nan y
  then x, x
  else if y > x || ((not (sign_bit y)) && sign_bit x)
  then x, y
  else y, x

external seeded_hash_param : int -> int -> int -> float -> int = "caml_hash"
  [@@noalloc]

let hash x = seeded_hash_param 10 100 0 x

module Array = struct
  type t = floatarray

  external length : t -> int = "%floatarray_length"

  external get : t -> int -> float = "%floatarray_safe_get"

  external set : t -> int -> float -> unit = "%floatarray_safe_set"

  external create : int -> t = "caml_floatarray_create"

  external unsafe_get : t -> int -> float = "%floatarray_unsafe_get"

  external unsafe_set : t -> int -> float -> unit = "%floatarray_unsafe_set"

  let unsafe_fill a ofs len v =
    for i = ofs to ofs + len - 1 do
      unsafe_set a i v
    done

  let unsafe_blit src sofs dst dofs len =
    for i = 0 to len - 1 do
      unsafe_set dst (dofs + i) (unsafe_get src (sofs + i))
    done

  let check a ofs len msg =
    if ofs < 0 || len < 0 || ofs + len < 0 || ofs + len > length a
    then invalid_arg msg

  let make n v =
    let result = create n in
    unsafe_fill result 0 n v;
    result

  let init l f =
    if l < 0
    then invalid_arg "Float.Array.init"
    else
      let res = create l in
      for i = 0 to l - 1 do
        unsafe_set res i (f i)
      done;
      res

  let append a1 a2 =
    let l1 = length a1 in
    let l2 = length a2 in
    let result = create (l1 + l2) in
    unsafe_blit a1 0 result 0 l1;
    unsafe_blit a2 0 result l1 l2;
    result

  (* next 3 functions: modified copy of code from string.ml *)
  let ensure_ge (x : int) y =
    if x >= y then x else invalid_arg "Float.Array.concat"

  let rec sum_lengths acc = function
    | [] -> acc
    | hd :: tl -> sum_lengths (ensure_ge (length hd + acc) acc) tl

  let concat l =
    let len = sum_lengths 0 l in
    let result = create len in
    let rec loop l i =
      match l with
      | [] -> assert (i = len)
      | hd :: tl ->
        let hlen = length hd in
        unsafe_blit hd 0 result i hlen;
        loop tl (i + hlen)
    in
    loop l 0;
    result

  let sub a ofs len =
    check a ofs len "Float.Array.sub";
    let result = create len in
    unsafe_blit a ofs result 0 len;
    result

  let copy a =
    let l = length a in
    let result = create l in
    unsafe_blit a 0 result 0 l;
    result

  let fill a ofs len v =
    check a ofs len "Float.Array.fill";
    unsafe_fill a ofs len v

  let blit src sofs dst dofs len =
    check src sofs len "Float.array.blit";
    check dst dofs len "Float.array.blit";
    unsafe_blit src sofs dst dofs len

  let to_list a = List.init (length a) (unsafe_get a)

  let of_list l =
    let result = create (List.length l) in
    let rec fill i l =
      match l with
      | [] -> result
      | h :: t ->
        unsafe_set result i h;
        fill (i + 1) t
    in
    fill 0 l

  (* duplicated from array.ml *)
  let iter f a =
    for i = 0 to length a - 1 do
      f (unsafe_get a i)
    done

  (* duplicated from array.ml *)
  let iter2 f a b =
    if length a <> length b
    then invalid_arg "Float.Array.iter2: arrays must have the same length"
    else
      for i = 0 to length a - 1 do
        f (unsafe_get a i) (unsafe_get b i)
      done

  let map f a =
    let l = length a in
    let r = create l in
    for i = 0 to l - 1 do
      unsafe_set r i (f (unsafe_get a i))
    done;
    r

  let map2 f a b =
    let la = length a in
    let lb = length b in
    if la <> lb
    then invalid_arg "Float.Array.map2: arrays must have the same length"
    else
      let r = create la in
      for i = 0 to la - 1 do
        unsafe_set r i (f (unsafe_get a i) (unsafe_get b i))
      done;
      r

  (* duplicated from array.ml *)
  let iteri f a =
    for i = 0 to length a - 1 do
      f i (unsafe_get a i)
    done

  let mapi f a =
    let l = length a in
    let r = create l in
    for i = 0 to l - 1 do
      unsafe_set r i (f i (unsafe_get a i))
    done;
    r

  (* duplicated from array.ml *)
  let fold_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r (unsafe_get a i)
    done;
    !r

  (* duplicated from array.ml *)
  let fold_right f a x =
    let r = ref x in
    for i = length a - 1 downto 0 do
      r := f (unsafe_get a i) !r
    done;
    !r

  (* duplicated from array.ml *)
  let exists p a =
    let n = length a in
    let rec loop i =
      if i = n then false else if p (unsafe_get a i) then true else loop (i + 1)
    in
    loop 0

  (* duplicated from array.ml *)
  let for_all p a =
    let n = length a in
    let rec loop i =
      if i = n then true else if p (unsafe_get a i) then loop (i + 1) else false
    in
    loop 0

  (* duplicated from array.ml *)
  let mem x a =
    let n = length a in
    let rec loop i =
      if i = n
      then false
      else if compare (unsafe_get a i) x = 0
      then true
      else loop (i + 1)
    in
    loop 0

  (* mostly duplicated from array.ml, but slightly different *)
  let mem_ieee x a =
    let n = length a in
    let rec loop i =
      if i = n then false else if x = unsafe_get a i then true else loop (i + 1)
    in
    loop 0

  (* duplicated from array.ml *)
  exception Bottom of int

  let sort cmp a =
    let maxson l i =
      let i31 = i + i + i + 1 in
      let x = ref i31 in
      if i31 + 2 < l
      then begin
        if cmp (get a i31) (get a (i31 + 1)) < 0 then x := i31 + 1;
        if cmp (get a !x) (get a (i31 + 2)) < 0 then x := i31 + 2;
        !x
      end
      else if i31 + 1 < l && cmp (get a i31) (get a (i31 + 1)) < 0
      then i31 + 1
      else if i31 < l
      then i31
      else raise (Bottom i)
    in
    let rec trickledown l i e =
      let j = maxson l i in
      if cmp (get a j) e > 0
      then begin
        set a i (get a j);
        trickledown l j e
      end
      else set a i e
    in
    let trickle l i e = try trickledown l i e with Bottom i -> set a i e in
    let rec bubbledown l i =
      let j = maxson l i in
      set a i (get a j);
      bubbledown l j
    in
    let bubble l i = try bubbledown l i with Bottom i -> i in
    let rec trickleup i e =
      let father = (i - 1) / 3 in
      assert (i <> father);
      if cmp (get a father) e < 0
      then begin
        set a i (get a father);
        if father > 0 then trickleup father e else set a 0 e
      end
      else set a i e
    in
    let l = length a in
    for i = ((l + 1) / 3) - 1 downto 0 do
      trickle l i (get a i)
    done;
    for i = l - 1 downto 2 do
      let e = get a i in
      set a i (get a 0);
      trickleup (bubble i 0) e
    done;
    if l > 1
    then (
      let e = get a 1 in
      set a 1 (get a 0);
      set a 0 e)

  (* duplicated from array.ml, except for the call to [create] *)
  let cutoff = 5

  let stable_sort cmp a =
    let merge src1ofs src1len src2 src2ofs src2len dst dstofs =
      let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
      let rec loop i1 s1 i2 s2 d =
        if cmp s1 s2 <= 0
        then begin
          set dst d s1;
          let i1 = i1 + 1 in
          if i1 < src1r
          then loop i1 (get a i1) i2 s2 (d + 1)
          else blit src2 i2 dst (d + 1) (src2r - i2)
        end
        else begin
          set dst d s2;
          let i2 = i2 + 1 in
          if i2 < src2r
          then loop i1 s1 i2 (get src2 i2) (d + 1)
          else blit a i1 dst (d + 1) (src1r - i1)
        end
      in
      loop src1ofs (get a src1ofs) src2ofs (get src2 src2ofs) dstofs
    in
    let isortto srcofs dst dstofs len =
      for i = 0 to len - 1 do
        let e = get a (srcofs + i) in
        let j = ref (dstofs + i - 1) in
        while !j >= dstofs && cmp (get dst !j) e > 0 do
          set dst (!j + 1) (get dst !j);
          decr j
        done;
        set dst (!j + 1) e
      done
    in
    let rec sortto srcofs dst dstofs len =
      if len <= cutoff
      then isortto srcofs dst dstofs len
      else
        let l1 = len / 2 in
        let l2 = len - l1 in
        sortto (srcofs + l1) dst (dstofs + l1) l2;
        sortto srcofs a (srcofs + l2) l1;
        merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs
    in
    let l = length a in
    if l <= cutoff
    then isortto 0 a 0 l
    else
      let l1 = l / 2 in
      let l2 = l - l1 in
      let t = create l2 in
      sortto l1 t 0 l2;
      sortto 0 a l2 l1;
      merge l2 l1 t 0 l2 a 0

  let fast_sort = stable_sort

  (* duplicated from array.ml *)
  let to_seq a =
    let rec aux i () =
      if i < length a
      then
        let x = unsafe_get a i in
        Seq.Cons (x, aux (i + 1))
      else Seq.Nil
    in
    aux 0

  (* duplicated from array.ml *)
  let to_seqi a =
    let rec aux i () =
      if i < length a
      then
        let x = unsafe_get a i in
        Seq.Cons ((i, x), aux (i + 1))
      else Seq.Nil
    in
    aux 0

  (* mostly duplicated from array.ml *)
  let of_rev_list l =
    let len = List.length l in
    let a = create len in
    let rec fill i = function
      | [] -> a
      | hd :: tl ->
        unsafe_set a i hd;
        fill (i - 1) tl
    in
    fill (len - 1) l

  (* duplicated from array.ml *)
  let of_seq i =
    let l = Seq.fold_left (fun acc x -> x :: acc) [] i in
    of_rev_list l

  let map_to_array f a =
    let l = length a in
    if l = 0
    then [||]
    else
      let r = Array.make l (f (unsafe_get a 0)) in
      for i = 1 to l - 1 do
        Array.unsafe_set r i (f (unsafe_get a i))
      done;
      r

  let map_from_array f a =
    let l = Array.length a in
    let r = create l in
    for i = 0 to l - 1 do
      unsafe_set r i (f (Array.unsafe_get a i))
    done;
    r
end

module ArrayLabels = Array
