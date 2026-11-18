Test that short paths will prefer a visible dependency over a hidden one

Start by compiling deps of the test file
  $ $OCAMLC -c hidden/hidden.ml
  $ $OCAMLC -c visible/visible.ml -I hidden

Create a .merlin file
  $ cat > test/.merlin << EOF
  > FLG -short-paths
  > B ../visible
  > S ../visible
  > BH ../hidden
  > SH ../hidden
  > EOF

The type of `Test.foo` either `Hidden.t` or `Visible.Foo.Bar.t`. But `Hidden.t` is not a
valid identifier to write because `Hidden` is a hidden dependency, so Merlin should prefer
`Visible.Foo.Bar.t`. This is what the legacy implementation of short paths does:
  $ $MERLIN single type-enclosing -legacy-short-path -position 1:5 -filename test/test.ml < test/test.ml | jq .value[].type -r
  Visible.Foo.Bar.t

FIXME: the implementation based on the domain of discourse only considers the paths
related to what is written in the current file: `Visible.Foo.Bar.t` is not one of them,
so no valid name is found and the path is left unchanged.
  $ $MERLIN single type-enclosing -position 1:5 -filename test/test.ml < test/test.ml | jq .value[].type -r
  Hidden.t

The type of `Test.bar` is `Hidden.u`. There is no valid path that it can be referred to
by, but Merlin should still be able to give `Hidden.u`.
  $ $MERLIN single type-enclosing -position 2:5 -filename test/test.ml < test/test.ml | jq .value[].type -r
  Hidden.u
