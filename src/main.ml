open Syntax
open Typing

let read_ty_print () =
  print_string "# ";
  flush stdout;
  try
    let e = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let t = ty_exp Environment.empty e in
    Printf.printf "val %s : %s"
      "-"
      (string_of_ty t);
    print_newline ()
  with
  | e -> raise e

let _ = read_ty_print ()
