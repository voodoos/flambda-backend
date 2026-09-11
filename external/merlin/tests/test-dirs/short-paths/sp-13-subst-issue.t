This mocks the Async --include--> Async_kernel --exports--> Deferred

  $ mkdir async_kernel
  $ cd async_kernel


  $ cat >async_kernel__.ml <<'EOF'
  > module Deferred = Async_kernel__Deferred
  > module Deferred0 = Async_kernel__Deferred0
  > EOF

  $ $OCAMLC -c async_kernel__.ml -no-alias-deps 2>/dev/null


  $ cat >deferred0.ml <<'EOF'
  > type +'a t = 'a
  > let create : 'a -> 'a t = Fun.id
  > EOF

  $ cat >deferred0.mli <<'EOF'
  > type +'a t 
  > val create : 'a -> 'a t
  > EOF

  $ $OCAMLC -c deferred0.mli -open Async_kernel__ -o Async_kernel__Deferred0
  $ $OCAMLC -c deferred0.ml -open Async_kernel__ -o Async_kernel__Deferred0

  $ cat >deferred.ml <<'EOF'
  > type +'a t = 'a Deferred0.t
  > 
  > module Let_syntax = struct 
  >   module Let_syntax : sig val return : 'a -> 'a t end = struct let return x = Deferred0.create x end
  > end
  > EOF

  $ $OCAMLC -c deferred.ml -open Async_kernel__ -o Async_kernel__Deferred


  $ cat >async_kernel.ml <<'EOF'
  > module Deferred = Deferred
  > module Let_syntax = Deferred.Let_syntax.Let_syntax (* This the use the Deferred that should end in the discourse ? *)
  > EOF

  $ $OCAMLC -c async_kernel.ml -open Async_kernel__


  $ cd ..
  $ mkdir async
  $ cd async

  $ cat >async.ml <<'EOF'
  > module Deferred = Async_kernel.Deferred (* FIXME This should be used to rewrite Async_kernel.Deferred.t *)
  > module Let_syntax = Async_kernel.Let_syntax
  > EOF

  $ $OCAMLC -c async.ml -I ../async_kernel

  $ cd ..

  $ cat >test.ml <<'EOF'
  > open! Async
  > 
  > let foo = Let_syntax.return 5
  > EOF


  $ $OCAMLC -c test.ml  -I async -I async_kernel

  $ cat >.merlin <<'EOF'
  > FLG -short-paths
  > B .
  > B async
  > B async_kernel
  > EOF

  $ $MERLIN single type-enclosing -nostdlib -position 3:5 \
  > -log-file - -log-section discourse-recap \
  > -filename test.ml 2>&1 <test.ml | sed -E 's/^# [0-9]+.[0-9]+/#/'
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [Async! ->
       [{item = (module, Async!); env = with env};
        {item = (module, Async!); env = with env}];
    Async!.Let_syntax -> [{item = (module, Async!.Let_syntax); env = with env}];
    Async!.Let_syntax.return ->
      [{item = (value, Async!.Let_syntax.return); env = with env}]];
    substs =
    Deferred -> [Deferred];
    Let_syntax -> [Let_syntax];
    Async_kernel!.Deferred -> [Deferred];
    Async_kernel!.Let_syntax -> [Let_syntax] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel! -> [{item = (module, Async_kernel!); env = with env}];
    Async_kernel!.Let_syntax ->
      [{item = (module, Async_kernel!.Let_syntax); env = with env}];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}]];
    substs =
     }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel! -> [{item = (module, Async_kernel!); env = with env}];
    Async_kernel!.Deferred ->
      [{item = (module, Async_kernel!.Deferred); env = with env}];
    Async_kernel!.Deferred.Let_syntax ->
      [{item = (module, Async_kernel!.Deferred.Let_syntax); env = with env}];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}];
    Async_kernel!.Deferred.Let_syntax.Let_syntax ->
      [{item = (module, Async_kernel!.Deferred.Let_syntax.Let_syntax);
        env = with env}]];
    substs =
     }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel__! -> [{item = (module, Async_kernel__!); env = with env}];
    Async_kernel__Deferred! ->
      [{item = (module, Async_kernel__Deferred!); env = with env}];
    Async_kernel__!.Deferred ->
      [{item = (module, Async_kernel__!.Deferred); env = with env}];
    Async_kernel__Deferred!.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax); env = with env}];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}]];
    substs =
     }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel__Deferred! ->
       [{item = (module, Async_kernel__Deferred!); env = with env}]];
    substs =
     }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 140;
    paths =
    Let_syntax [module];
    Deferred [module];
    Async_kernel__Deferred! [module];
    Async_kernel__Deferred!.t [type];
    Async_kernel__Deferred!.Let_syntax [module];
    Async_kernel__Deferred!.Let_syntax.Let_syntax [module];
    Async_kernel__Deferred!.Let_syntax.Let_syntax.return [value];
    Async_kernel__! [module];
    Async_kernel__!.Deferred0 [module];
    Async_kernel__!.Deferred [module];
    Async_kernel! [module];
    Async_kernel!.Let_syntax [module];
    Async_kernel!.Deferred [module];
    Async_kernel!.Deferred.Let_syntax [module];
    Async_kernel!.Deferred.Let_syntax.Let_syntax [module];
    Async! [module];
    Async!.Let_syntax [module];
    Async!.Let_syntax.return [value];
    Async!.Deferred [module];
    float64x8! [type];
    float32x16! [type];
    float16x32! [type];
    int64x8! [type];
    int32x16! [type];
    int16x32! [type];
    int8x64! [type];
    float64x4! [type];
    float32x8! [type];
    float16x16! [type];
    int64x4! [type];
    int32x8! [type];
    int16x16! [type];
    int8x32! [type];
    float64x2! [type];
    float32x4! [type];
    float16x8! [type];
    int64x2! [type];
    int32x4! [type];
    int16x8! [type];
    int8x16! [type];
    idx_mut! [type];
    idx_imm! [type];
    or_null! [type];
    eval! [type];
    expr! [type];
    lexing_position! [type];
    atomic_loc! [type];
    iarray! [type];
    floatarray! [type];
    extension_constructor! [type];
    string! [type];
    lazy_t! [type];
    int64! [type];
    int32! [type];
    int16! [type];
    int8! [type];
    nativeint! [type];
    option! [type];
    list! [type];
    array! [type];
    continuation! [type];
    eff! [type];
    exn! [type];
    unit! [type];
    bool! [type];
    float32! [type];
    float! [type];
    bytes! [type];
    char! [type];
    int! [type];
    substs =
    [Async_kernel__Deferred! ->
       [Async!.Deferred; Async_kernel!.Deferred; Async_kernel__!.Deferred];
    Async_kernel__Deferred0! -> [Async_kernel__!.Deferred0];
    Deferred -> [Deferred];
    Let_syntax -> [Let_syntax];
    Async!.Let_syntax -> [Async_kernel!.Let_syntax];
    Async_kernel!.Deferred -> [Deferred; Async_kernel__!.Deferred];
    Async_kernel!.Let_syntax ->
      [Let_syntax; Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel__!.Deferred -> [Async_kernel__Deferred!; Async!.Deferred];
    Async_kernel__Deferred!.Let_syntax -> [Async_kernel!.Deferred.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [Async!.Let_syntax; Async_kernel!.Let_syntax;
       Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel!.Deferred.Let_syntax.Let_syntax -> [Async!.Let_syntax]]
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    Async_kernel__Deferred! ->
      [Async!.Deferred; Async_kernel!.Deferred; Async_kernel__!.Deferred];
    Async_kernel__Deferred0! -> [Async_kernel__!.Deferred0];
    Deferred -> [Deferred];
    Let_syntax -> [Let_syntax];
    Async!.Let_syntax -> [Async_kernel!.Let_syntax];
    Async_kernel!.Deferred -> [Deferred; Async_kernel__!.Deferred];
    Async_kernel!.Let_syntax ->
      [Let_syntax; Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel__!.Deferred -> [Async_kernel__Deferred!; Async!.Deferred];
    Async_kernel__Deferred!.Let_syntax -> [Async_kernel!.Deferred.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [Async!.Let_syntax; Async_kernel!.Let_syntax;
       Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel!.Deferred.Let_syntax.Let_syntax -> [Async!.Let_syntax] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    [Async_kernel__Deferred! ->
       [Async!.Deferred; Async_kernel!.Deferred; Async_kernel__!.Deferred];
    Async_kernel__Deferred0! -> [Async_kernel__!.Deferred0];
    Deferred -> [Deferred];
    Let_syntax -> [Let_syntax];
    Async!.Let_syntax -> [Async_kernel!.Let_syntax];
    Async_kernel!.Deferred -> [Deferred; Async_kernel__!.Deferred];
    Async_kernel!.Let_syntax ->
      [Let_syntax; Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel__!.Deferred -> [Async_kernel__Deferred!; Async!.Deferred];
    Async_kernel__Deferred!.Let_syntax -> [Async_kernel!.Deferred.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [Async!.Let_syntax; Async_kernel!.Let_syntax;
       Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel!.Deferred.Let_syntax.Let_syntax -> [Async!.Let_syntax]]
    }
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 7
        },
        "type": "int Deferred.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }

Dump the discourse so regressions show up as a diff in this test:

  $ $MERLIN single type-enclosing -nostdlib -position 3:5 \
  > -log-file - -log-section discourse-recap \
  > -filename test.ml < test.ml 2>&1| sed -E 's/^# [0-9]+.[0-9]+/#/'
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [Async! ->
       [{item = (module, Async!); env = with env};
        {item = (module, Async!); env = with env}];
    Async!.Let_syntax -> [{item = (module, Async!.Let_syntax); env = with env}];
    Async!.Let_syntax.return ->
      [{item = (value, Async!.Let_syntax.return); env = with env}]];
    substs =
    Deferred -> [Deferred];
    Let_syntax -> [Let_syntax];
    Async_kernel!.Deferred -> [Deferred];
    Async_kernel!.Let_syntax -> [Let_syntax] }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel! -> [{item = (module, Async_kernel!); env = with env}];
    Async_kernel!.Let_syntax ->
      [{item = (module, Async_kernel!.Let_syntax); env = with env}];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}]];
    substs =
     }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel! -> [{item = (module, Async_kernel!); env = with env}];
    Async_kernel!.Deferred ->
      [{item = (module, Async_kernel!.Deferred); env = with env}];
    Async_kernel!.Deferred.Let_syntax ->
      [{item = (module, Async_kernel!.Deferred.Let_syntax); env = with env}];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}];
    Async_kernel!.Deferred.Let_syntax.Let_syntax ->
      [{item = (module, Async_kernel!.Deferred.Let_syntax.Let_syntax);
        env = with env}]];
    substs =
     }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel__! -> [{item = (module, Async_kernel__!); env = with env}];
    Async_kernel__Deferred! ->
      [{item = (module, Async_kernel__Deferred!); env = with env}];
    Async_kernel__!.Deferred ->
      [{item = (module, Async_kernel__!.Deferred); env = with env}];
    Async_kernel__Deferred!.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax); env = with env}];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [{item = (module, Async_kernel__Deferred!.Let_syntax.Let_syntax);
        env = with env}]];
    substs =
     }
  # discourse-recap - next_U
  next_U (non-empty, looping):
  { u_paths =
    [Async_kernel__Deferred! ->
       [{item = (module, Async_kernel__Deferred!); env = with env}]];
    substs =
     }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 140;
    paths =
    Let_syntax [module];
    Deferred [module];
    Async_kernel__Deferred! [module];
    Async_kernel__Deferred!.t [type];
    Async_kernel__Deferred!.Let_syntax [module];
    Async_kernel__Deferred!.Let_syntax.Let_syntax [module];
    Async_kernel__Deferred!.Let_syntax.Let_syntax.return [value];
    Async_kernel__! [module];
    Async_kernel__!.Deferred0 [module];
    Async_kernel__!.Deferred [module];
    Async_kernel! [module];
    Async_kernel!.Let_syntax [module];
    Async_kernel!.Deferred [module];
    Async_kernel!.Deferred.Let_syntax [module];
    Async_kernel!.Deferred.Let_syntax.Let_syntax [module];
    Async! [module];
    Async!.Let_syntax [module];
    Async!.Let_syntax.return [value];
    Async!.Deferred [module];
    float64x8! [type];
    float32x16! [type];
    float16x32! [type];
    int64x8! [type];
    int32x16! [type];
    int16x32! [type];
    int8x64! [type];
    float64x4! [type];
    float32x8! [type];
    float16x16! [type];
    int64x4! [type];
    int32x8! [type];
    int16x16! [type];
    int8x32! [type];
    float64x2! [type];
    float32x4! [type];
    float16x8! [type];
    int64x2! [type];
    int32x4! [type];
    int16x8! [type];
    int8x16! [type];
    idx_mut! [type];
    idx_imm! [type];
    or_null! [type];
    eval! [type];
    expr! [type];
    lexing_position! [type];
    atomic_loc! [type];
    iarray! [type];
    floatarray! [type];
    extension_constructor! [type];
    string! [type];
    lazy_t! [type];
    int64! [type];
    int32! [type];
    int16! [type];
    int8! [type];
    nativeint! [type];
    option! [type];
    list! [type];
    array! [type];
    continuation! [type];
    eff! [type];
    exn! [type];
    unit! [type];
    bool! [type];
    float32! [type];
    float! [type];
    bytes! [type];
    char! [type];
    int! [type];
    substs =
    [Async_kernel__Deferred! ->
       [Async!.Deferred; Async_kernel!.Deferred; Async_kernel__!.Deferred];
    Async_kernel__Deferred0! -> [Async_kernel__!.Deferred0];
    Deferred -> [Deferred];
    Let_syntax -> [Let_syntax];
    Async!.Let_syntax -> [Async_kernel!.Let_syntax];
    Async_kernel!.Deferred -> [Deferred; Async_kernel__!.Deferred];
    Async_kernel!.Let_syntax ->
      [Let_syntax; Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel__!.Deferred -> [Async_kernel__Deferred!; Async!.Deferred];
    Async_kernel__Deferred!.Let_syntax -> [Async_kernel!.Deferred.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [Async!.Let_syntax; Async_kernel!.Let_syntax;
       Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel!.Deferred.Let_syntax.Let_syntax -> [Async!.Let_syntax]]
    }
  # discourse-recap - U
  U at start of D.of_U:
  { u_paths =
    [];
    substs =
    Async_kernel__Deferred! ->
      [Async!.Deferred; Async_kernel!.Deferred; Async_kernel__!.Deferred];
    Async_kernel__Deferred0! -> [Async_kernel__!.Deferred0];
    Deferred -> [Deferred];
    Let_syntax -> [Let_syntax];
    Async!.Let_syntax -> [Async_kernel!.Let_syntax];
    Async_kernel!.Deferred -> [Deferred; Async_kernel__!.Deferred];
    Async_kernel!.Let_syntax ->
      [Let_syntax; Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel__!.Deferred -> [Async_kernel__Deferred!; Async!.Deferred];
    Async_kernel__Deferred!.Let_syntax -> [Async_kernel!.Deferred.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [Async!.Let_syntax; Async_kernel!.Let_syntax;
       Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel!.Deferred.Let_syntax.Let_syntax -> [Async!.Let_syntax] }
  # discourse-recap - D
  Final D:
  Discourse {
    size = 0;
    paths =
    ;
    substs =
    [Async_kernel__Deferred! ->
       [Async!.Deferred; Async_kernel!.Deferred; Async_kernel__!.Deferred];
    Async_kernel__Deferred0! -> [Async_kernel__!.Deferred0];
    Deferred -> [Deferred];
    Let_syntax -> [Let_syntax];
    Async!.Let_syntax -> [Async_kernel!.Let_syntax];
    Async_kernel!.Deferred -> [Deferred; Async_kernel__!.Deferred];
    Async_kernel!.Let_syntax ->
      [Let_syntax; Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel__!.Deferred -> [Async_kernel__Deferred!; Async!.Deferred];
    Async_kernel__Deferred!.Let_syntax -> [Async_kernel!.Deferred.Let_syntax];
    Async_kernel__Deferred!.Let_syntax.Let_syntax ->
      [Async!.Let_syntax; Async_kernel!.Let_syntax;
       Async_kernel!.Deferred.Let_syntax.Let_syntax];
    Async_kernel!.Deferred.Let_syntax.Let_syntax -> [Async!.Let_syntax]]
    }
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 7
        },
        "type": "int Deferred.t",
        "tail": "no"
      }
    ],
    "notifications": []
  }
