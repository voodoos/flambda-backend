(* TEST
 expect;
*)

type ab = [ `A | `B ];;
let f (x : [`A]) = match x with #ab -> 1;;
[%%expect{|
type ab = [ `A | `B ]
Line 2, characters 32-35:
2 | let f (x : [`A]) = match x with #ab -> 1;;
                                    ^^^
Error: This pattern matches values of type "[? `A | `B ]"
       but a pattern was expected which matches values of type "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}];;
let f x = ignore (match x with #ab -> 1); ignore (x : [`A]);;
[%%expect{|
Line 1, characters 31-34:
1 | let f x = ignore (match x with #ab -> 1); ignore (x : [`A]);;
                                   ^^^
Error: This pattern matches values of type "[? `B ]"
       but a pattern was expected which matches values of type "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}];;
let f x = ignore (match x with `A|`B -> 1); ignore (x : [`A]);;
[%%expect{|
Line 1, characters 34-36:
1 | let f x = ignore (match x with `A|`B -> 1); ignore (x : [`A]);;
                                      ^^
Error: This pattern matches values of type "[? `B ]"
       but a pattern was expected which matches values of type "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}];;

let f (x : [< `A | `B]) = match x with `A | `B | `C -> 0;; (* warn *)
[%%expect{|
Line 1, characters 49-51:
1 | let f (x : [< `A | `B]) = match x with `A | `B | `C -> 0;; (* warn *)
                                                     ^^
Warning 12 [redundant-subpat]: this sub-pattern is unused.

val f : [< `A | `B ] -> int = <fun>
|}];;
let f (x : [`A | `B]) = match x with `A | `B | `C -> 0;; (* fail *)
[%%expect{|
Line 1, characters 47-49:
1 | let f (x : [`A | `B]) = match x with `A | `B | `C -> 0;; (* fail *)
                                                   ^^
Error: This pattern matches values of type "[? `C ]"
       but a pattern was expected which matches values of type "[ `A | `B ]"
       The second variant type does not allow tag(s) "`C"
|}];;

(* imported from in poly.ml *)
type t = A | B;;
function `A,_ -> 1 | _,A -> 2 | _,B -> 3;;
function `A,_ -> 1 | _,(A|B) -> 2;;
function Some `A, _ -> 1 | Some _, A -> 2 | None, A -> 3 | _, B -> 4;;
function Some `A, A -> 1 | Some `A, B -> 1
       | Some _, A -> 2  | None, A -> 3 | _, B -> 4;;
function A, `A -> 1 | A, `B -> 2 | B, _ -> 3;;
function `A, A -> 1 | `B, A -> 2 | _, B -> 3;;
function (`A|`B), _ -> 0 | _,(`A|`B) -> 1;;
function `B,1 -> 1 | _,1 -> 2;;
function 1,`B -> 1 | 1,_ -> 2;;
[%%expect {|
type t = A | B
- : [> `A ] * t -> int = <fun>
- : [> `A ] * t -> int = <fun>
- : [> `A ] option * t -> int = <fun>
- : [> `A ] option * t -> int = <fun>
- : t * [< `A | `B ] -> int = <fun>
- : [< `A | `B ] * t -> int = <fun>
Line 9, characters 0-41:
9 | function (`A|`B), _ -> 0 | _,(`A|`B) -> 1;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(`AnyOtherTag, `AnyOtherTag)

- : [> `A | `B ] * [> `A | `B ] -> int = <fun>
Line 10, characters 0-29:
10 | function `B,1 -> 1 | _,1 -> 2;;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(_, 0)

Line 10, characters 21-24:
10 | function `B,1 -> 1 | _,1 -> 2;;
                          ^^^
Warning 11 [redundant-case]: this match case is unused.

- : [< `B ] * int -> int = <fun>
Line 11, characters 0-29:
11 | function 1,`B -> 1 | 1,_ -> 2;;
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(0, _)

Line 11, characters 21-24:
11 | function 1,`B -> 1 | 1,_ -> 2;;
                          ^^^
Warning 11 [redundant-case]: this match case is unused.

- : int * [< `B ] -> int = <fun>
|}];;

(* PR#6787 *)
let revapply x f = f x;;

let f x (g : [< `Foo]) =
  let y = `Bar x, g in
  revapply y (fun ((`Bar i), _) -> i);;
(* f : 'a -> [< `Foo ] -> 'a *)
[%%expect{|
val revapply : 'a -> ('a -> 'b) -> 'b = <fun>
val f : 'a -> [< `Foo ] -> 'a = <fun>
|}];;

(* PR#6124 *)
let f : ([`A | `B ] as 'a) -> [> 'a] -> unit = fun x (y : [> 'a]) -> ();;
let f (x : [`A | `B] as 'a) (y : [> 'a]) = ();;
[%%expect{|
Line 1, characters 61-63:
1 | let f : ([`A | `B ] as 'a) -> [> 'a] -> unit = fun x (y : [> 'a]) -> ();;
                                                                 ^^
Error: The type "'a" does not expand to a polymorphic variant type
Hint: Did you mean "`a"?
|}]

(* CR reisenberg: This test is disabled. It only barely works in `main`
   anyway, as evidenced by https://github.com/ocaml/ocaml/issues/13369
(* PR#5927 *)
type 'a foo = 'a constraint 'a = [< `Tag of & int];;
[%%expect{|
type 'a foo = 'a constraint 'a = [< `Tag of & int ]
|}]
*)

(* PR#7704 *)
type t = private [> `A of string ];;
function (`A x : t) -> x;;
[%%expect{|
type t = private [> `A of string ]
Line 2, characters 0-24:
2 | function (`A x : t) -> x;;
    ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`<some private tag>

- : t -> string = <fun>
|}]

let f = function `AnyOtherTag, _ -> 1 | _, (`AnyOtherTag|`AnyOtherTag') -> 2;;
[%%expect{|
Line 1, characters 8-76:
1 | let f = function `AnyOtherTag, _ -> 1 | _, (`AnyOtherTag|`AnyOtherTag') -> 2;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(`AnyOtherTag', `AnyOtherTag'')

val f : [> `AnyOtherTag ] * [> `AnyOtherTag | `AnyOtherTag' ] -> int = <fun>
|}]

let x:(([`A] as 'a)* ([`B] as 'a)) = [`A]
[%%expect {|
Line 1, characters 31-32:
1 | let x:(([`A] as 'a)* ([`B] as 'a)) = [`A]
                                   ^
Error: This alias is bound to type "[ `B ]" but is used as an instance of type
         "[ `A ]"
       These two variant types have no intersection
|}]

type t = private [< `A]
let f: t -> [ `A ] = fun x -> x
[%%expect {|
type t = private [< `A ]
Line 2, characters 30-31:
2 | let f: t -> [ `A ] = fun x -> x
                                  ^
Error: This expression has type "t" but an expression was expected of type
         "[ `A ]"
       The first variant type is private, it may not allow the tag(s) "`A"
|}]


(** Check that the non-regularity error message is robust to permutation *)

type ('a,'b,'c,'d,'e) a = [ `A of ('d,'a,'e,'c,'b) b ]
and  ('a,'b,'c,'d,'e) b = [ `B of ('c,'d,'e,'a,'b) c ]
and  ('a,'b,'c,'d,'e) c = [ `C of ('a,'b,'c,'d,'e) a ]
[%%expect {|
Line 3, characters 0-54:
3 | type ('a,'b,'c,'d,'e) a = [ `A of ('d,'a,'e,'c,'b) b ]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive type is not regular.
       The type constructor "a" is defined as
         type "('a, 'b, 'c, 'd, 'e) a"
       but it is used as
         "('e, 'c, 'b, 'd, 'a) a"
       after the following expansion(s):
         "[ `A of ('d, 'a, 'e, 'c, 'b) b ]" contains "('d, 'a, 'e, 'c, 'b) b",
         "('d, 'a, 'e, 'c, 'b) b" = "[ `B of ('e, 'c, 'b, 'd, 'a) c ]",
         "[ `B of ('e, 'c, 'b, 'd, 'a) c ]" contains "('e, 'c, 'b, 'd, 'a) c",
         "('e, 'c, 'b, 'd, 'a) c" = "[ `C of ('e, 'c, 'b, 'd, 'a) a ]",
         "[ `C of ('e, 'c, 'b, 'd, 'a) a ]" contains "('e, 'c, 'b, 'd, 'a) a"
       All uses need to match the definition for the recursive type to be regular.
|}]

(* PR 10762 *)
type a = int
type t = [ `A of a ]
let inspect: [< t ] -> unit = function
  | `A 0 -> ()
  | `A _ -> ()
[%%expect {|
type a = int
type t = [ `A of a ]
val inspect : [< `A of a & int ] -> unit = <fun>
|}]

(** Error messages with weakly polymorphic row variables *)
let x = Fun.id (function `X -> () | _ -> ())
[%%expect {|
val x : ([> `X ] as '_weak1) -> unit = <fun>
|}]

let x = let rec x = `X (`Y (fun y -> x = y)) in Fun.id x
[%%expect {|
val x : [> `X of [> `Y of '_weak2 -> bool ] as '_weak3 ] as '_weak2 =
  `X (`Y <fun>)
|}]

(** Code coverage for [unify_row_field] errors *)

(** Arity mismatch *)

let f (x:[`X of int]) = (x:[`X])
[%%expect{|
Line 5, characters 25-26:
5 | let f (x:[`X of int]) = (x:[`X])
                             ^
Error: This expression has type "[ `X of int ]"
       but an expression was expected of type "[ `X ]"
       Types for tag "`X" are incompatible
|}]


let f (x:[`X of int]) = (x:[<`X of & int])
[%%expect{|
Line 1, characters 25-26:
1 | let f (x:[`X of int]) = (x:[<`X of & int])
                             ^
Error: This expression has type "[ `X of int ]"
       but an expression was expected of type "[< `X of & int ]"
       Types for tag "`X" are incompatible
|}]

(** Inconsistent type *)

let f (x:[<`X of & int & float]) = (x:[`X])
[%%expect{|
Line 3, characters 36-37:
3 | let f (x:[<`X of & int & float]) = (x:[`X])
                                        ^
Error: This expression has type "[< `X of & int & float ]"
       but an expression was expected of type "[ `X ]"
       Types for tag "`X" are incompatible
|}]


(** Missing tag correctly attributed *)
type rt = [ `A | `B of string | `R of rt ]
let rec f = function `A -> 0 | `B s -> int_of_string s | `R x -> f x

let g (x:[`A | `R of rt]) = f x
[%%expect{|
type rt = [ `A | `B of string | `R of rt ]
val f : ([< `A | `B of string | `R of 'a ] as 'a) -> int = <fun>
Line 4, characters 30-31:
4 | let g (x:[`A | `R of rt]) = f x
                                  ^
Error: This expression has type "[ `A | `R of rt ]"
       but an expression was expected of type "[< `A | `R of 'a ] as 'a"
       Type "rt" = "[ `A | `B of string | `R of rt ]" is not compatible with type
         "[< `A | `R of 'a ] as 'a"
       The second variant type does not allow tag(s) "`B"
|}]
