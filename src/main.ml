open Printf
open Syntax
open Typing
open Translate
open Eval

(* TODO: debug = true の時のみ，途中経過を出力 *)
let read_ty_print () =
  print_string "# ";
  flush stdout;
  try
    let e = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let gamma = Environment.empty in
    let t = ty_exp gamma e in
    print_string "[Typing]\n";
    printf "val - : %s" (string_of_ty t);
    print_newline ();
    (* CI *)
    let f, t' = translate_exp gamma e in
    print_string "[Translation]\n";
    print_string (C.string_of_exp f);
    print_newline ();
    (* check soundness *)
    print_string (string_of_bool (t = t'));
    print_newline ();
    (* Eval *)
    let env = Environment.empty in
    print_string "[Evaluation]\n";
    let v = eval_exp env f in
    printf "val - : %s = %s" (string_of_ty t) (string_of_value v);
    print_newline ()
  with
  | e -> raise e

let _ = read_ty_print ()
