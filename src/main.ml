open Printf
open Syntax
open Typing
open Translate
open Eval
open Stringify

let rec read_eval_print gamma env =
  print_string "# ";
  flush stdout;
  try
    (* p has type G.program *)
    let p = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    print_string "[Typing]\n";
    let (gamma', x, t) = ty_prog gamma p in
    printf "val %s : %s" x (string_of_ty t);
    print_newline ();

    (* Cast Insertion Translation *)
    print_string "[Translation]\n";
    (* q has type C.program *)
    let q, t' = translate_prog gamma p in
    print_string (C.string_of_program q);
    print_newline ();
    (* check soundness *)
    print_string (string_of_bool (t = t'));
    print_newline ();

    (* Eval *)
    print_string "[Evaluation]\n";
    let (env', x', v) = eval_prog env q in
    assert (x' = x);
    printf "val %s : %s = %s" x (string_of_ty t) (string_of_value v);
    print_newline ();
    read_eval_print gamma' env'
  with
  | Syntax_error s | Typing_error s | CI_error s | Eval_error s ->
     print_string s;
     print_newline ();
     read_eval_print gamma env
  (* TODO: improve error message *)
  | Parsing.Parse_error ->      (* Menhir *)
     print_string "Parse_error.";
     print_newline ();
     read_eval_print gamma env
  (* Lexer error *)
  (* e.g. Failure("lexing: empty token") *)
  | Failure m -> print_string m;
                 print_newline ();
                 read_eval_print gamma env

let initial_gamma = Environment.empty
let initial_env = Environment.empty

let _ = read_eval_print initial_gamma initial_env
