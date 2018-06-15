# Interpreter of GTLC

This is an interpreter of Gradually Typed Lambda Calculus (GTLC).
This implementation is still experimental.

## Requirements

- menhir
- OUnit2 (only for unit testing)

## Build instructions

```sh
cd src
make depend
make
./gtlc
```

## Features

The syntax is similar to that of ML or OCaml.

Note that type annotations for bound variables are needed.
Also, `let rec` expressions need type annotations:
`let rec x (y : t1) : t2 = e1 in e2`

Notes on available operations:

- variables
- base types are `int` and `bool`
- constants of type `int` and `bool`
- primitive operations: `+`, `-`, `*`, `/`, `<`, `>`, `=`, `<=`, `>=`, `&&`, `||`
- `fun` and application
- `let` and `let rec` expressions
- `if` expressions


## Some examples

```
# (fun (x:?) -> x + 4) true;;
# (fun (x:?) -> x + 4) 3;;
# (fun (x:int) -> x + 4) true;;
```

## References

- Jeremy G. Siek and Walid Taha. Gradual Typing for Functional Languages. Scheme 2006.
- Jeremy G. Siek, Michael M. Vitousek, Matteo Cimini, and John Tang Boyland.
Refined Criteria for Gradual Typing. SNAPL 2015.
- GitHub [aigarashi/STLC](https://github.com/aigarashi/STLC) (especially [this branch](https://github.com/aigarashi/STLC/tree/gtlc))
