Ocalc
=======

Ocalc is a simple lispy calculator written in < 50 lines of OCaml. This was originally intended to be a super small self-hosting lisp/scheme interpreter modelled after lispy by Peter Norvig. However, I think that may be troublesome due to how I am evaluated the tail of the list.

So, I decided to just release the calculator version on its own for now because there will probably need to be major changes before I can get a lisp/scheme working.

One of the biggest challenges is typing in OCaml. It is hard to *succinctly* return multiple types in OCaml. This is necessary in a lisp because the return type of a function could be an atom or a list. If/when I want/ed to implement this in OCaml, I would have to have a `match ... with` statement *every* time I work with the lisp object. Thus, this implementation *only supports ints*.

Of course, I could definitely be missing something. One solution is to store the objects as strings with a type attached so I simply know how to deal with them. However, even in this case, an interface to get that type back is challenging for the same reasons as above.

Anyway, it is impressive enough to me that this fits in under 50 lines (of somewhat butchered width, granted). But still, enjoy!

```ocaml
$ make test
$ ./tests.native
```

Examples
--------

See tests/tests.ml and make tests for example source and build.

```ocaml
open Ocalc

let tests = [
    ("(+ 1 (+ 1 3))", "5");
    ("(- 3 2)", "1");
    ("(/ (- 8 2) 3)", "2");
    ("(* 9 (+ 3 5))", "72");
    ("(+ (if (< 2 3) 1 2) (if (> 4 5) 2 9) 72)", "82");
]

let rec run_tests tests =
    match tests with
    | [] -> ()
    | ((test, answer) :: rem_tests) ->
        print_endline test;
        print_endline ((eval test) ^ " === " ^ answer ^ "\n");
        run_tests rem_tests;;

run_tests tests
```

Features
--------

- No Core/Batteries dependencies

Support
-------

If you are having issues, please let us know.
Email me at me@eatonphil.com

Acknowledgements
----------------

[ivg](http://stackoverflow.com/users/2625442/ivg) on Stackoverflow for helping me debug my sockets. 

License
-------

The project is licensed under the BSD license.
