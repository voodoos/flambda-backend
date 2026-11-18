  $ cat>foo.mli<<'EOF'
  > module Bar : sig module Baz : sig type t end end
  > EOF

  $ cat>a.mli<<'EOF'
  > type t = Foo.Bar.Baz.t
  > val foo : unit -> Foo.Bar.Baz.t
  > EOF



  $ $OCAMLC -c foo.mli a.mli

  $ cat >main.ml <<'EOF'
  > let _ = A.foo ()
  > EOF

(* now print Foo.Bar.Baz.t *)

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > S .
  > EOF

  $ ls
  a.cmi
  a.mli
  foo.cmi
  foo.mli
  main.ml


  $ $MERLIN single type-enclosing -position 1:12 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "unit -> A.t"

  $ cat >main.ml <<'EOF'
  > module _ = A
  > type u = Foo.Bar.Baz.t
  > (* now print Foo.Bar.Baz.t *)
  > EOF



  $ $MERLIN single type-enclosing -position 2:21 \
  > -filename main.ml < main.ml | jq '.value[0].type'
  "type t"
