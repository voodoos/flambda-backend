(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = Flambda_kind.t list

type arity = t

let nullary = []

let create t = t

let length t = List.length t

let to_list t = t

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 = List.compare Flambda_kind.compare t1 t2

  let equal t1 t2 = compare t1 t2 = 0

  let hash = Hashtbl.hash

  let print ppf t =
    match t with
    | [] -> Format.pp_print_string ppf "Nullary"
    | _ ->
      Format.fprintf ppf "@[%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf " @<1>\u{2a2f} ")
           Flambda_kind.print)
        t
end)

let is_all_values t = List.for_all Flambda_kind.is_value t

let is_all_naked_floats t = List.for_all Flambda_kind.is_naked_float t

let is_singleton_value t =
  match t with
  | [kind] when Flambda_kind.equal kind Flambda_kind.value -> true
  | _ -> false

module With_subkinds = struct
  type t = Flambda_kind.With_subkind.t list

  let create t = t

  let to_list t = t

  include Container_types.Make (struct
    type nonrec t = t

    let compare t1 t2 = List.compare Flambda_kind.With_subkind.compare t1 t2

    let equal t1 t2 = compare t1 t2 = 0

    let hash = Hashtbl.hash

    let print ppf t =
      match t with
      | [] -> Format.pp_print_string ppf "Nullary"
      | _ ->
        Format.fprintf ppf "@[%a@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf " @<1>\u{2a2f} ")
             Flambda_kind.With_subkind.print)
          t
  end)

  let is_singleton_value t =
    match t with
    | [kind]
      when Flambda_kind.equal
             (Flambda_kind.With_subkind.kind kind)
             Flambda_kind.value ->
      true
    | _ -> false

  let cardinal t = List.length t

  let nullary = []

  let is_nullary t = match t with [] -> true | _ :: _ -> false

  let to_arity t = List.map Flambda_kind.With_subkind.kind t

  let of_arity arity =
    List.map (fun kind -> Flambda_kind.With_subkind.create kind Anything) arity

  let compatible t ~when_used_at =
    if List.compare_lengths t when_used_at <> 0
    then
      Misc.fatal_errorf "Mismatched arities:@ %a@ and@ %a" print t print
        when_used_at;
    List.for_all2
      (fun kind when_used_at ->
        Flambda_kind.With_subkind.compatible kind ~when_used_at)
      t when_used_at
end
