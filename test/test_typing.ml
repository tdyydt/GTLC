open OUnit2
open Syntax
open Typing

let test_ty_exp =
  let gamma = Environment.empty in
  let test (input, expected) =  (* name a test *)
    input >:: fun test_ctxt ->
              let e = Parser.toplevel Lexer.main (Lexing.from_string (input ^ ";;")) in
              let t = ty_exp gamma e in
              assert_equal (string_of_ty t) expected in
  List.map test [
      "(fun (x:?) -> x + 3) true", "int";
      "(fun (x:?) -> x) true", "?";
    ]

let suite =
  "test_ty_exp" >::: test_ty_exp

let () =
  run_test_tt_main suite
;;
