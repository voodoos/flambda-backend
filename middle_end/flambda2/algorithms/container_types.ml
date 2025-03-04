(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "-55"]

module type Thing_no_hash = sig
  type t

  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
end

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t

  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
end

module type Set = sig
  module T : Set.OrderedType

  include Set.S with type elt = T.t

  val print : Format.formatter -> t -> unit

  val to_string : t -> string

  val of_list : elt list -> t

  val map : (elt -> elt) -> t -> t

  val union_list : t list -> t

  val intersection_is_empty : t -> t -> bool

  val get_singleton : t -> elt option
end

module type Map = sig
  module T : Map.OrderedType

  include Map.S with type key = T.t

  module Set : Set with module T := T

  val print_debug :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val of_list : (key * 'a) list -> 'a t

  val disjoint_union :
    ?eq:('a -> 'a -> bool) ->
    ?print:(Format.formatter -> 'a -> unit) ->
    'a t ->
    'a t ->
    'a t

  val rename : key t -> key -> key

  val map_keys : (key -> key) -> 'a t -> 'a t

  val keys : 'a t -> Set.t

  val data : 'a t -> 'a list

  val of_set : (key -> 'a) -> Set.t -> 'a t

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val diff_domains : 'a t -> 'a t -> 'a t

  val inter : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val inter_domain_is_non_empty : 'a t -> 'a t -> bool

  val get_singleton : 'a t -> (key * 'a) option

  val replace : key -> ('a -> 'a) -> 'a t -> 'a t

  val map_sharing : ('a -> 'a) -> 'a t -> 'a t
end

module Pair (A : Thing) (B : Thing) : Thing with type t = A.t * B.t = struct
  type t = A.t * B.t

  let compare (a1, b1) (a2, b2) =
    let c = A.compare a1 a2 in
    if c <> 0 then c else B.compare b1 b2

  let hash (a, b) = Hashtbl.hash (A.hash a, B.hash b)

  let equal (a1, b1) (a2, b2) = A.equal a1 a2 && B.equal b1 b2

  let [@ocamlformat "disable"] print ppf (a, b) = Format.fprintf ppf " (%a, @ %a)" A.print a B.print b
end

module Make_map (T : Thing) (Set : Set with module T := T) = struct
  include Map.Make [@inlined hint] (T)
  module Set = Set

  let of_list l = List.fold_left (fun map (id, v) -> add id v map) empty l

  let disjoint_union ?eq ?print m1 m2 =
    ignore print;
    union
      (fun _id v1 v2 ->
        let ok = match eq with None -> false | Some eq -> eq v1 v2 in
        if not ok
        then
          (* let _err = match print with | None -> Format.asprintf
             "Map.disjoint_union %a" T.print id | Some print -> Format.asprintf
             "Map.disjoint_union %a => %a <> %a" T.print id print v1 print v2
             in *)
          invalid_arg "disjoint_union"
        else Some v1)
      m1 m2

  let union_right m1 m2 =
    merge
      (fun _id x y ->
        match x, y with
        | None, None -> None
        | None, Some v | Some v, None | Some _, Some v -> Some v)
      m1 m2

  let union_left m1 m2 = union_right m2 m1

  let union_merge f m1 m2 =
    let aux _ m1 m2 =
      match m1, m2 with
      | None, m | m, None -> m
      | Some m1, Some m2 -> Some (f m1 m2)
    in
    merge aux m1 m2

  let rename m v = try find v m with Not_found -> v

  let map_keys f m = of_list (List.map (fun (k, v) -> f k, v) (bindings m))

  let [@ocamlformat "disable"] print print_datum ppf t =
    let module Lmap = Lmap.Make (T) in
    Lmap.print print_datum ppf (Lmap.of_list (bindings t))

  let print_debug = print

  let keys map = fold (fun k _ set -> Set.add k set) map Set.empty

  let data t = List.map snd (bindings t)

  let of_set f set = Set.fold (fun e map -> add e (f e) map) set empty

  let transpose_keys_and_data map = fold (fun k v m -> add v k m) map empty

  let transpose_keys_and_data_set map =
    fold
      (fun k v m ->
        let set =
          match find v m with
          | exception Not_found -> Set.singleton k
          | set -> Set.add k set
        in
        add v set m)
      map empty

  let diff_domains t1 t2 =
    merge
      (fun _key datum1 datum2 ->
        match datum1, datum2 with
        | None, None -> None
        | Some datum1, None -> Some datum1
        | None, Some _datum2 -> None
        | Some _datum1, Some _datum2 -> None)
      t1 t2

  let fold2_stop_on_key_mismatch f t1 t2 init =
    (* CR mshinwell: Provide a proper implementation *)
    if cardinal t1 <> cardinal t2
    then None
    else
      let t1 = bindings t1 in
      let t2 = bindings t2 in
      List.fold_left2
        (fun acc (key1, datum1) (key2, datum2) ->
          match acc with
          | None -> None
          | Some acc ->
            if T.compare key1 key2 <> 0
            then None
            else Some (f key1 datum1 datum2 acc))
        (Some init) t1 t2

  let inter f t1 t2 =
    merge
      (fun key datum1_opt datum2_opt ->
        match datum1_opt, datum2_opt with
        | None, None | None, Some _ | Some _, None -> None
        | Some datum1, Some datum2 -> Some (f key datum1 datum2))
      t1 t2

  let inter_domain_is_non_empty _ _ = Misc.fatal_error "Not yet implemented"

  exception More_than_one_binding

  let get_singleton_exn t =
    (* Not as fast as being in the stdlib, but doesn't allocate. *)
    if is_empty t
    then raise Not_found
    else
      try
        let (_ : int) =
          fold
            (fun _key _elt iter_count ->
              if iter_count > 0 then raise More_than_one_binding else 1)
            t 0
        in
        choose t
      with More_than_one_binding -> raise Not_found

  let get_singleton t =
    match get_singleton_exn t with
    | exception Not_found -> None
    | binding -> Some binding

  let replace _ _ _ : _ t = Misc.fatal_error "Not yet implemented"

  let map_sharing = map
end
[@@inline always]

module Make_set (T : Thing_no_hash) = struct
  include Set.Make [@inlined hint] (T)

  let [@ocamlformat "disable"] print ppf s =
    let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" T.print e) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  let to_string s = Format.asprintf "%a" print s

  let of_list l =
    match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q

  let map f s = of_list (List.map f (elements s))

  let rec union_list ts =
    match ts with [] -> empty | t :: ts -> union t (union_list ts)

  let intersection_is_empty t1 t2 = is_empty (inter t1 t2)

  let fixpoint f set =
    let rec aux acc set =
      if is_empty set
      then acc
      else
        let set' = fold (fun x -> union (f x)) set empty in
        let acc = union acc set in
        aux acc (diff set' acc)
    in
    aux empty set

  exception More_than_one_element

  let get_singleton t =
    (* Not as fast as being in the stdlib, but doesn't allocate. *)
    if is_empty t
    then None
    else
      try
        let (_ : int) =
          fold
            (fun _elt iter_count ->
              if iter_count > 0 then raise More_than_one_element else 1)
            t 0
        in
        choose_opt t
      with More_than_one_element -> None
end
[@@inline always]

module type S = sig
  type t

  module T : Thing with type t = t

  include Thing with type t := T.t

  module Set : Set with module T := T

  module Map : Map with module T := T with module Set = Set
end

module Make (T : Thing) = struct
  module T = T
  include T
  module Set = Make_set (T)
  module Map = Make_map (T) (Set)
end
[@@inline always]

module Make_pair (T1 : S) (T2 : S) = struct
  module Pair = Pair (T1.T) (T2.T)
  include Make (Pair)

  let create_from_cross_product t1_set t2_set =
    T1.Set.fold
      (fun t1 result ->
        T2.Set.fold (fun t2 result -> Set.add (t1, t2) result) t2_set result)
      t1_set Set.empty
end
