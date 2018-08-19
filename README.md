# Interpreter of GTLC

This is an interpreter of Gradually Typed Lambda Calculus (GTLC).

The interpreter consists of three parts:

- Type checking (`src/typing.ml`)
- Cast insertion (`src/translate.ml`)
- Evaluation (`src/eval.ml`)

Be aware that this implementation is incomplete.

## Requirements

- Menhir
- OUnit2 (optional; for unit testing)
- js\_of\_ocaml, js\_of\_ocaml-ppx (optional; for web)

## Build instructions

```sh
cd src
make depend
make
./gtlc
```

## Syntax

- Basically the syntax is similar to ML or OCaml.
- The dynamic type `?` is added.
- Be aware that type annotations are needed.

### Types `T`

- The dynamic type: `?`
  - means a type unknown at compile time.
  - is ignored in static type checking.
- Base types: `int`, `bool`
- Function type: `T1 -> T2`

### Expressions `e`

- Variables: `x`
- Constants: intergers, boolean (`true`, `false`)
- Primitive operations: `+`, `-`, `*`, `/`, `<`, `>`, `=`, `<=`, `>=`, `&&`, `||`
- If: `if e1 then e2 else e3`
- Function abstraction: `fun (x:T) -> e`
- Application: `e1 e2`
- Let: `let x = e1 in e2`
  - `let x1 = e1 and x2 = e2 and ... and xn = en in e` is supported.
- Recursion: `let rec f (x:T1) : T2 = e1 in e2`
  - The return type annotation `T2` is needed.
  - Mutual recursion: `let rec f1 (x1:T11) : T12 = e1 and f2 (x2:T21) : T22 = e2 in e` is supported.

### Top-level input

- Expressions: `e;;`
- Let declaration: `let x = e;;`
- Recursion declaration: `let rec f (x:T1) : T2 = e;;`


## Some examples

- `(fun (x:int) -> x + 2) 3` evaluates to 5.
- `(fun (x:int) -> x + 2) true` fails in static type checking.
- `(fun (x:?) -> x + 2) true`
  - passes static type checking because `x` has the dynamic type,
  - but fails at run time.


## References

- Jeremy G. Siek and Walid Taha. Gradual Typing for Functional Languages. Scheme 2006.
- Jeremy G. Siek, Michael M. Vitousek, Matteo Cimini, and John Tang Boyland.
Refined Criteria for Gradual Typing. SNAPL 2015.
- GitHub [aigarashi/STLC](https://github.com/aigarashi/STLC); especially [this branch](https://github.com/aigarashi/STLC/tree/gtlc)
