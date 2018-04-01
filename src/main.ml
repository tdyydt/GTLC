open Printf
open Syntax
open Typing
open Translate

(* TODO: debug = true の時のみ，途中経過を出力 *)
let read_ty_print () =
  print_string "# ";
  flush stdout;
  let gamma = Environment.empty in
  try
    let e = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let t = ty_exp gamma e in
    printf "val %s : %s" "-" (string_of_ty t);
    print_newline ();
    let f, t' = translate_exp gamma e in
    print_string @@ C.string_of_exp f;
    print_newline ();
    (* TODO: assert t = t' *)
    print_string @@ string_of_bool (t = t');
  with
  | e -> raise e

let _ = read_ty_print ()
