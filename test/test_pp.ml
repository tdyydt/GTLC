open OUnit2
open Format

(* test parser and G.string_of_exp *)
(* because I don't have parser for C.exp now *)

let test_pp_exp =
  let test input =
    input >:: fun test_ctxt ->
              (* if you parse input, then stringify it,
               * the string should be same as input. *)
              (* it is also possible to parse again the final string?? *)
              let lexbuf = Lexing.from_string (input ^ ";;") in
              let p = Parser.toplevel Lexer.main lexbuf in
              let s = asprintf "%a" Pp.G.pp_prog p in
              assert_equal input s
  in
  List.map test [
      (* Take care of parentheses and whitespaces and syntax sugar *)
      "1 + 2";
      "3 - 1";                  (* not AppExp(3,-1) *)
      "1 + 2 + 3";              (* left assoc *)
      "1 + (2 + 3)";
      "1 + 2 * 3";
      "1 * 2 + 3";
      "1 < 2 + 3";
      "2 + 3 < 1";
      "true && true && true";   (* right assoc *)
      "(true && true) && true";
      "true && true || true";
      "true && (true || true)";
      "1 = 2 || 2 = 3";

      "fun (x : ?) -> x";
      "fun (x : int) -> x + 1";
      (* Tests for pp_ty *)
      "fun (x : ? -> ?) -> x";
      "fun (x : ? -> ? -> ?) -> x";
      "fun (x : (? -> ?) -> ?) -> x";
      "fun (x : int list list) -> x";
      "fun (x : int list -> int list) -> x";
      "fun (x : (int -> int) list) -> x";

      "fun (x : ?) -> fun (y : ?) -> 1";
      "let x = 5 in x";
      "let x = 5 in let y = 3 in x";
      (* "let f = fun (x : ?) -> x in f";
       * "let x = let y = 3 in y in x"; *)

      "if true then 1 + 3 else f 5";

      (* "x : int => ?";
       * (\* "1 + 2 : int => ?";       (\\* same as: (1 + 2) : int => ? *\\) *\)
       * "(1 + 2) : int => ?";
       * "1 + (x : ? => int)"; *)

      (* f is not bound, but ok in this test *)
      "f 5";
      "f 10 5";
      "f 10 (id 5)";
      "1 + f 5";
      "f 5 + 1";
      "f (-3)";
    ]

let suite = [
    "test_pp_exp" >::: test_pp_exp
  ]
