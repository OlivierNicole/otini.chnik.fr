---
title: Bringing typed, modular macros to OCaml
---

As part of my [OCaml Labs](https://github.com/ocamllabs) research project, I
implemented a new macro system for OCaml, based on a design by Leo White and
Jeremy Yallop [^1].

It is mainly an effort to make a “compile-time MetaOCaml” [^2] (though you don't
need prior knowledge of MetaOCaml to read what follows). Its compile-time nature
requires additional subtleties to try and make the programming experience as
pleasant as in MetaOCaml. If you want to try it out, the repository is
[available](https://github.com/ocamllabs/ocaml-macros).

What does it look like?
-----------------------

The idea is to write explicitly staged code. There are two phases:
compilation (phase 1) and execution (phase 0). The numbering may seem
counter-intuitive; it is mostly because, theoretically, there may be
several macro expansion phases before compilation (macros inside
macros), the execution being the last phase. In that case, it is more
natural to refer to phases by 0, 1, 2, 3, etc. than to use negative
numbers, or to introduce an abritrary offset.

Compile-time values and functions can be declared using
`static`{.ocaml} instead of `let`{.ocaml} in
toplevel bindings:

``` {.ocaml}
# static x = 42;;
static val x : int = 42
```

Here, `x` is a compile-time value, although of course the difference is
not visible in the toplevel.

It is impossible to mix compile-time and runtime values as it wouldn't
make sense:

``` {.ocaml}
# static x = 42;;
static val x : int = 42
# let y = x + 1;;
Error: Attempt to use value x of phase 1 in an environment of phase 0
```

If we try now to create a static `succ` function:

``` {.ocaml}
# static stat_succ x = x + 1;;
Error: Attempt to use value + of phase 0 in an environment of phase 1
```

This happens because `(+)`{.ocaml} is part of the
`Pervasives`{.ocaml} module, which is considered phase-0. To
use it in phase 1, one must add a tilda (`~`) before its name, like so:

``` {.ocaml}
# static stat_succ x = ~Pervasives.(+) x 1;;
static val stat_succ : int -> int = <fun>
```

Or, alternatively:

``` {.ocaml}
# static stat_succ x = let open ~Pervasives in x + 1;;
static val stat_succ : int -> int = <fun>
```

This operation is called *module lifting*. For phase separation to be enforced,
only external modules (i.e. modules in separate files, separately compiled) can
be lifted.

Now, if we want to write macros, we will need a way to manipulate code
fragments. This is done via *quotations*, *à la* MetaOCaml:

``` {.ocaml}
# macro x = << int_of_string "42" >>;;
macro x : int expr = << Pervasives(*global*).int_of_string "42" >>
```

Note the replacement of the keyword `static` with `macro`. Quoting is only
allowed in `macro` declarations, to avoid capture of local identifiers (see path
closures below).

The result of a quotation is of type `'a expr`{.ocaml}, where `'a`{.ocaml} is
the type of the quoted expression.  Also note that we were able to use directly
`Pervasives.int_of_string`{.ocaml} inside the quotation, without module lifting.
That's because the code inside a quote is of phase 0: it is destined to be
spliced inside phase-0 code during macro expansion.

The splicing operator is the dollar and is used like so:

``` {.ocaml}
# $x;;
- : int = 42
```

What actually happened here is that the quoted expression in `x` was spliced
into the code and evaluated (at runtime) to `42`, just as if we had directly
typed:

``` {.ocaml}
# int_of_string "42";;
- : int = 42
```

Splicing can be done in arbitrary expressions (as long as the types
match):

``` {.ocaml}
# let () = Printf.printf "%d\n" ($x + 1);;
43
```

Expressions can also be spliced inside quotations:

``` {.ocaml}
# macro square = << fun x -> x * x >>;;
macro square : (int -> int) expr =
  << fun x_1  -> Pervasives(*global*).( * ) x_1 x_1 >>
# macro n = << $square (42+1) >>;;
macro n : int expr =
  << (fun x_3  -> Pervasives(*global*).( * ) x_3 x_3)
       (Pervasives(*global*).(+) 42 1) >>
# $n;;
- : int = 1849
```

You can visualize what code was actually spliced in with the `-dsource` or
`-dparsetree` option:

``` {.ocaml}
# $n;;
$n;;
splice #0:
(fun x_3  -> Pervasives(*global*).( * ) x_3 x_3)
             (Pervasives(*global*).(+) 42 1)
- : int = 1849
```

Here is a classic example: a compile-time power function.

``` {.ocaml}
let square x = x * x

open ~Pervasives

macro rec power' n x =
  if n = 0 then
    << 1 >>
  else if n mod 2 = 0 then
    << square $(power' (n/2) x) >>
  else
    << Pervasives.( * ) $x $(power' (pred n) x) >>

macro power n =
  << fun x -> $(power' n <<x>>) >>
```

The `power`{.ocaml} macro, of type
`int -> (int -> int) expr`{.ocaml}, can then be used like
this:

``` {.ocaml}
# let x = $(power 9) 2 ;;
val x : int = 512
```

This is very similar to what would have been done in MetaOCaml. You may
have noticed that there is only one splicing construct (`$`{.sourceCode
.ocaml}), while MetaOCaml has two: *escape* (`.~`{.ocaml})
and *run*. We chose to use the same operator because the semantics are
the same, but under the hood, toplevel splices (that is, splices that
are not inside a quotation) are implemented differently than splices
inside quotations (see below).

For a more "real-world" example, you may consider the `printf` example
from [Yallop and White's
paper](https://www.cl.cam.ac.uk/~jdy22/papers/modular-macros.pdf). A runtime
`printf` function may be defined like this:

``` {.ocaml}
type (_, _) fmt =
    Int : (int → 'a, 'a) fmt
  | Lit : string → ('a, 'a) fmt
  | Cat : ('a, 'b) fmt * ('b, 'c) fmt → ('a, 'c) fmt
let (%) x y = Cat (x, y)

(* analogous to "(%d, %d)" *)
let p = Lit "(" % Int % Lit "," % Int % Lit ")"

let rec printk :
    type 'a 'b. (string → 'b) → ('a, 'b) fmt → 'a =
  fun k → function
    Int → fun s → k (string_of_int s)
  | Lit s → k s
  | Cat (l, r) →
      printk (fun x → printk (fun y → k (x ˆ y)) r) l

let sprintf fmt = printk (fun x → x) fmt
```

We may then write:

``` {.ocaml}
# sprintf p 3 4 ;;
- : string = "(3,4)"
```

The `sprintf` abstraction is more pleasant than writing by hand
`"(" ^ string_of_int 3 ^ "," ^ string_of_int 4 ^ ")"`, but it comes at
an interpretative cost.

To avoid this cost, the `printk` function can be staged into a macro:

``` {.ocaml}
type (_, _) fmt =
    Int : (int -> 'a, 'a) fmt
  | Lit : string -> ('a, 'a) fmt
  | Cat : ('a, 'b) fmt * ('b, 'c) fmt -> ('a, 'c) fmt

static (%) x y = Cat (x, y)

macro rec printk :
    type a b. (string expr -> b expr) -> (a, b) fmt -> a expr =
  fun k -> function
  | Int -> << fun s -> $(k <<string_of_int s>>) >>
  | Lit s -> k (Expr.of_string s)
  | Cat (l, r) ->
      printk (fun x ->
        printk (fun y -> k << $x ^ $y >>) r) l

macro sprintf fmt = printk (fun x -> x) fmt
```

We may then write a function that prints a pair:

``` {.ocaml}
# static p = Lit "(" % Int % Lit "," % Int % Lit ")";;
static val p : (int -> int -> '_a, '_a) fmt =
  Cat (Cat (Cat (Cat (Lit "(", Int), Lit ","), Int), Lit ")")
# let print_pair = $(sprintf p);;
splice #0:
fun s_1  ->
             fun s_2  ->
               Pervasives(*global*).(^)
                 (Pervasives(*global*).(^)
                    (Pervasives(*global*).(^)
                       (Pervasives(*global*).(^) "("
                          (Pervasives(*global*).string_of_int s_1)) ",")
                    (Pervasives(*global*).string_of_int s_2)) ")"
val print_pair : int -> int -> string = <fun>
```

We get the abstraction we wanted at zero interpretative cost.

Integration within modules
--------------------------

We want macros to be seamlessly integrated into OCaml's module system.

For example, modules can export static values as well as regular values.
The above printing functions could be gathered in a `sprintf.ml` file. A
`sprintf.mli` interface file may then be written:

``` {.ocaml}
type (_, _) fmt =
    Int : (int -> 'a, 'a) fmt
  | Lit : string -> ('a, 'a) fmt
  | Cat : ('a, 'b) fmt * ('b, 'c) fmt -> ('a, 'c) fmt
static val ( % ) : ('a, 'b) fmt -> ('b, 'c) fmt -> ('a, 'c) fmt
macro sprintf : ('a, string) fmt -> 'a expr
```

We have hidden `printk` and exported only `sprintf`.

The static components of the `Sprintf`{.ocaml} module will
be saved to a `sprintf.cmm` file (the format is the same as for `.cmo`
files). The `sprintf` macro can now be used in other files.

Path closures
-------------

Consider again the `power'`{.ocaml} macro from above:
``` {.ocaml}
macro rec power' n x =
  if n = 0 then
    <<1>>
  else if n mod 2 = 0 then
    <<square $(power' (n/2) x)>>
  else
    <<Pervasives.( * ) $x $(power' (pred n) x)>>
```
As you can see, applying `power'`{.ocaml} will result in some code, possibly
containing the run-time `square` function. But isn't that dangerous? For
example, what if I put `square`, `power'` and `power` in a module that only
exports `power`? With a naive implementation of macros, the result of splicing
that macro would be an error like `Unbound value square`.

To avoid that, a macro "carries" all the free identifiers that are used in its
body in what we call a *path closure*. Additionnally, every macro takes an
implicit path argument when it's called.

To see the result of macro expansion more easily, you can use `;;;` (triple
semicolon) that lets you evaluate static values in the toplevel:
``` {.ocaml}
# let x = 42;;
val x : int = 42
# macro y () = <<x>>;;
macro y : unit -> int expr = <fun>
# y ();;;
- : int expr = << y.x(*0*) >>
```
The meaning of `y.x(*0*)`{.ocaml} is: field of index 0 in the path closure of
macro `y`. For clarity, along with the field index is printed the name of the
field, i.e. `x`.

To go back to our slightly more complicated `power` example:
``` {.ocaml}
# Power.power 5;;;
- : (int -> int) expr =
<< fun x_2  ->
     Pervasives(*global*).( * ) x_2
       (Power.power.power'(*0*).square(*0*)
          (Power.power.power'(*0*).square(*0*)
             (Pervasives(*global*).( * ) x_2 1))) >>
```
This quote tells us that the runtime function `square` can be found in the
closure of macro `power'`, field 0. Additionnally, `power'` itself is in the
closure of `power` (necessary because `power'` might be out of scope, just like
`square`).

Static modules and functors
---------------------------

If a module or a functor is intended to contain only `static` definitions, you
can make it static:
``` {.ocaml}
static module F(X : sig
  val succ : int -> int
end) = struct
  let x = X.succ 42
end

static module M = F(~Pervasives)
(* M.x = 43 *)
```

Note that static functors can only take static modules as their arguments. Also,
`let`{.ocaml} is used instead of `static`{.ocaml} in a static module.

Passing values between phases
-----------------------------

Imagine you have a compile-time integer (not an `int expr`{.ocaml}, a real
`int`{.ocaml}) and you want to use in your runtime code. This is done via
`Expr.of_int` of type `int -> int expr`{.ocaml}:
``` {.ocaml}
static x = 42

let () = Printf.printf "%d\n" $(Expr.of_int x)
```

`Expr`{.ocaml} is a new module of the standard library, containing functions for
all the standard types: `of_int`, `of_float`, `of_list`… However, if you need to
pass a value of a custom type, you have to write the translation function
yourself:
```{.ocaml}
type t = A | B of int

static x = B 42

macro expr_of_t = function
  | A -> << A >>
  | B x -> << B $(Expr.of_int x) >>

let dyn_x = $(expr_of_t x)
```

Implementation details
----------------------

### Execution of static code

In order to run code at compile-time, all static bindings and splices
are translated to bytecode, then executed using the
`Meta.reify_bytecode` function available in the compiler (this function
is the one used in the toplevel to execute phrases).

One problem is that the optimised versions of the compiler, namely
`ocamlc.opt` and `ocamlopt.opt`, are native programs; but
`Meta.reify_bytecode` can only be called from bytecode programs. This
limitation seems to exist because it would be slightly non-trivial for
native and bytecode programs to share a heap.

One solution would be to compile macros and splices to native code, and
then link them dynamically to the compiler. But there is no portable way
to do dynamic linking in OCaml. The existing solution, developed by
Alain Frisch, resorts to the GNU linking toolchain.

The retained solution for optimised versions of the compiler is
therefore the following: in `ocamlc.opt` and `ocamlopt.opt`, static code
is translated to bytecode and written to a temporary `.cmo` file; this
file is then linked with its dependencies into an executable bytecode
file. The compiler then calls the external program `ocamlrun` on this
file.

This way of doing things is portable, but not very aesthetic. It could
be made more natural by integrating a way to execute native code
on-the-fly inside a native program, as proposed by Meurer and Fischbach [^3].

### Quoting

The quoting part, developed by Leo White, is very similar to what can be
found in MetaOCaml, although we did not reuse MetaOCaml code directly.
It consists of a set of combinators transforming typed syntax trees into
AST-generating lambda code. This lambda code is destined to be executed
as part of the phase-1 code.

    typed AST (Typedtree)
            |
            |
            v
    lambda code (Lambda)
            |
            |
            v
    untyped AST (Parsetree)

The generated AST is untyped, mainly to avoid the hassle of propagating
type information to the generated tree. Instead, the typechecker is
called on the quoted tree during splicing.

### Splicing

Static code is constructed in such a way that it returns, when
evaluated, an array of untyped ASTs
(`Parsetree.expression array`{.ocaml}). These ASTs are then
typed and inserted in place of the splicings into the big syntax tree of
runtime code before it is translated to lambda code.

In the case of `ocamlc.opt` and `ocamlopt.opt`, the splice array needs
to be marshalled to a temporary file (since the compiler cannot share
memory with an external program).

Note that the above only applies to *toplevel splices*, that is, splices
that are not in a quotation. Splices inside quotations do not imply any
communication between phases, and are only here for the typechecker to
keep track of phase. Once the typechecker has done its job, they are
mostly ignored.

Testing and contributing
------------------------

All the examples presented above are working examples. The corresponding
[repository](https://github.com/ocamllabs/ocaml-macros) is available. Of course,
as a prototype it is evolving rapidly and we cannot guarantee that future
changes won't break your code.

You are very welcome to try out macros. It will be very useful if you report any
bugs or inconvenience by opening an issue on this repo.

If you want to set up an Opam switch with our version of the compiler, you must
be aware that there is currently no working version of `camlp4` that
compiles with the macro compiler, but hopefully this will be fixed soon.

Let me know if you write something nice involving macros!

[^1]: White, Leo and Yallop, Jeremy, [Modular
  macros](https://www.cl.cam.ac.uk/~jdy22/papers/modular-macros.pdf), OCaml
  Workshop 2015.

[^2]: Kiselyov, Oleg, [BER MetaOCaml](http://okmij.org/ftp/ML/MetaOCaml.html).

[^3]: Meurer, Benedikt and Fischbach, Marcell, [Towards a native toplevel for
  the OCaml language](http://arxiv.org/pdf/1110.1029v2.pdf).

