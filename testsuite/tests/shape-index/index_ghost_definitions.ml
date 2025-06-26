(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_ghost_definitions.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -uid-deps -decls index_ghost_definitions.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)


module type S (* 1 *) = sig
  val u : unit (* 0 *)
end

module G (X : S) = struct
  let () = X.u (* Reduces to G0 -> 0 *)
  let () = X.u (* Reduces to G0 -> 0 *)
end

module H (X : S) = struct
  let () = X.u (* Reduces to G0 -> 0 *)
  let () = X.u (* Reduces to G0 -> 0 *)
end

(* FIXME: in functor H X.u should reduce to a fresh ghost UID G1 *)
