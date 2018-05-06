open OUnit2
open Syntax.G

(* test stringify *)

(* C.string_of_exp (と parser) の正しさ *)
let test_string_of_exp =
  let test input =
    input >:: fun test_ctxt ->
              let p = Parser.toplevel Lexer.main (Lexing.from_string (input ^ ";;")) in
              let s = string_of_program p in
              assert_equal input s
  in
  List.map test [
      (* 入力のスペースに気を付ける *)
      (* syntax sugar が含まれると，この単純な方法ではダメ *)
      (* Note some of test inputs are semantically incorrect... *)
      "1 + 2";
      "3 - 1";                  (* not AppExp(3,-1) *)
      "1 + 2 + 3";              (* left assoc *)
      "1 + (2 + 3)";
      "1 + 2 * 3";
      "1 * 2 + 3";
      "1 < 2 + 3";
      "2 + 3 < 1";

      "fun (x : ?) -> x";
      "fun (x : int) -> x + 1";
      "fun (x : ? -> ?) -> x";  (* Test string_of_ty *)
      "fun (x : ? -> ? -> ?) -> x";
      "fun (x : (? -> ?) -> ?) -> x";

      "fun (x : ?) -> fun (y : ?) -> 1";
      "let x = 5 in x";
      "let x = 5 in let y = 3 in x";
      "let f = (fun (x : ?) -> x) in f";
      "let x = (let y = 3 in y) in x";

      "if true then 1 + 3 else f 5";


      (* "x : int => ?";
       * (\* "1 + 2 : int => ?";       (\\* same as: (1 + 2) : int => ? *\\) *\)
       * "(1 + 2) : int => ?";
       * "1 + (x : ? => int)"; *)

      "f 5";
      "f 10 5";
      "f 10 (id 5)";
      "1 + f 5";
      "f 5 + 1";
    ]

let suite = [
    "test_string_of_exp" >::: test_string_of_exp
  ]
