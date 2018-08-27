open OUnit2
open Syntax
open Typing.G
open Stringify

(* TODO: Add tests for evaluation *)

let test_ty_exp =
  let gamma = Environment.empty in
  let test (input, expected) =
    (* use input string as test name *)
    (* >:: gives a name for a test *)
    input >:: fun test_ctxt ->
              let p = Parser.toplevel Lexer.main (Lexing.from_string (input ^ ";;")) in
              (match p with
               | Exp e -> let t = ty_exp gamma e in
                          assert_equal (string_of_ty t) expected
               | _ -> assert false)
  in
  List.map test [
      "(fun (x:?) -> x + 3) true", "int";
      "(fun (x:?) -> x) true", "?";
      "let f = (fun (x : ?) -> x) in f", "? -> ?";
      "let f = (fun (x : ?) -> x || true) in f", "? -> bool";
      "let f = (fun (x : ?) -> x || true) in f 3", "bool";
      "let f = (fun (x : ?) -> x + 3) in f", "? -> int";
      "let f = (fun (x : ?) -> x + 3) in f true", "int";
      "let f = (fun (x : ?) -> x + 3) in f 2", "int";
      "let rec fact (n:?) : int = if n < 2 then 1 else n * fact(n-1) in fact 5", "int";
    ]

(* TODO: test cases for program which fails in typing *)
(* e.g. (fun (x:int) -> x + 2) true *)
let test_ty_exp_err = ()

let suite = [
    "test_ty_exp" >::: test_ty_exp
  ]
