(* This module exists to prevent a dependency cycle with Types. *)
module Paths = struct
  module T = struct
    type t = Shape.Sig_component_kind.t * Path.t

    let compare (_, p1) (_, p2) =
      Path.compare p1 p2
  end

  include Set.Make (T)
end

let empty = Paths.empty
