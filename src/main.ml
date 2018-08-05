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
    let p : Syntax.G.program =
      Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    print_string "[Typing]\n";
    let (gamma', ty_bindings) = Typing.G.ty_prog gamma p in
    List.iter (fun (x,t) ->
        printf "val %s : %s" x (string_of_ty t);
        print_newline ())
      ty_bindings;

    (* Cast Insertion Translation *)
    print_string "[Translation]\n";
    (* q : Syntax.C.program *)
    let (q, ty_bindings') = translate_prog gamma p in
    (* check soundness *)
    assert (ty_bindings = ty_bindings');
    (* result of translation *)
    print_string (C.string_of_program q);
    print_newline ();

    (* Eval *)
    print_string "[Evaluation]\n";
    let (env', val_bindings) = eval_prog env q in
    List.iter2 (fun (x,t) (x',v) ->
        assert (x' = x);
        printf "val %s : %s = %s" x (string_of_ty t) (string_of_value v);
        print_newline ())
      ty_bindings val_bindings;

    read_eval_print gamma' env'
  with
  | Syntax_error s | Typing_error s | CI_error s | Eval_error s ->
     print_string s;
     print_newline ();
     read_eval_print gamma env
  | Blame (tag1, tag2) ->
     printf "Blame: %s is incompatible with tag %s"
       (string_of_tag tag2) (string_of_tag tag1);
     print_newline ();
     read_eval_print gamma env
  (* TODO: Get error message by Menhir? *)
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
