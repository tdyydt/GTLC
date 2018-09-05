open Printf
open Syntax
open Typing
open Translate
open Eval
open Stringify
open Util.Error

let rec read_eval_print gamma env =
  print_string "# ";
  flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  try
    let open Syntax in
    let p : G.program = Parser.toplevel Lexer.main lexbuf in
    print_string "*** Typing ***\n";
    let open Typing in
    let (gamma', ty_bindings) = G.ty_prog gamma p in
    let open Stringify in
    List.iter (fun (x,t) ->
        printf "val %s : %s\n" x (string_of_ty t))
      ty_bindings;

    (* Cast-Insertion Translation *)
    print_string "*** Cast Insertion ***\n";
    let open Syntax in
    let ((q : C.program), ty_bindings') = translate_prog gamma p in
    (* check soundness *)
    assert (ty_bindings = ty_bindings');
    (* result of translation *)
    let open Stringify in
    print_string (C.string_of_program q);
    print_newline ();

    (* Eval *)
    print_string "*** Evaluation ***\n";
    let (env', val_bindings) = eval_prog env q in
    List.iter2 (fun (x,t) (x',v) ->
        assert (x' = x);
        printf "val %s : %s = %s\n"
          x (string_of_ty t) (string_of_value v))
      ty_bindings val_bindings;

    read_eval_print gamma' env'
  with
  (* e.g. Failure("lexing: empty token") *)
  | Failure m -> printf "Failure: %s\n" m;
                 read_eval_print gamma env
  | Parser.Error ->             (* Menhir *)
     let token = Lexing.lexeme lexbuf in
     printf "Parser.Error: unexpected token: %s\n" token;
     Lexing.flush_input lexbuf;
     read_eval_print gamma env
  | Syntax_error s | Typing_error s | CI_error s | Eval_error s ->
     print_string s;
     print_newline ();
     read_eval_print gamma env
  | Blame (r, plr, tag1, tag2) ->
     begin match plr with
     | Pos -> printf "Blame on the expression side: %s => %s\n"
                (string_of_tag tag1) (string_of_tag tag2)
     | Neg -> printf "Blame on the environment side: %s => %s\n"
                (string_of_tag tag1) (string_of_tag tag2)
     end;
     print_range r;
     print_newline ();
     read_eval_print gamma env

let initial_gamma = Environment.empty
let initial_env = Environment.empty

let _ = read_eval_print initial_gamma initial_env
