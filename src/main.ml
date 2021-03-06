open Util.Error
open Format
open Pp
open Syntax
open Typing
open Translate
open Eval

(* let printf fmt = fprintf std_formatter fmt *)

let rec read_eval_print gamma env =
  printf "# @?";                (* @? to flush the output buffer *)
  flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  try
    let open Syntax in
    let p : G.program = Parser.toplevel Lexer.main lexbuf in
    printf "*** Typing ***\n";
    let open Typing in
    let (gamma', ty_bindings) = G.ty_prog gamma p in
    let open Pp in
    List.iter (fun (x,t) ->
        printf "val %s : %a\n" x pp_ty t)
      ty_bindings;

    (* Cast-insertion Translation *)
    printf "*** Cast Insertion ***\n";
    let open Syntax in
    let ((q : C.program), ty_bindings') = translate_prog gamma p in
    (* Show result of translation *)
    let open Pp in
    printf "%a\n" C.pp_prog q;
    (* check soundness ; equality of lists *)
    assert (ty_bindings = ty_bindings');

    (* Eval *)
    printf "*** Evaluation ***\n";
    let (env', val_bindings) = eval_prog env q in
    List.iter2 (fun (x,t) (x',v) ->
        assert (x' = x);
        printf "val %s : %a = %a\n"
          x pp_ty t pp_value v)
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
  | Syntax_error (r, msg) ->
     printf "%a\n" print_range r;
     printf "%s\n" msg;
     read_eval_print gamma env

  | Type_error (r, msg) ->
     printf "%a\n" print_range r;
     printf "%s\n" msg;
     read_eval_print gamma env

  | Blame (r, plr, tag1, tag2) ->
     printf "%a\n" print_range r;
     begin match plr with
     | Pos -> printf "Blame on the expression side: %s => %s\n"
                (string_of_tag tag1) (string_of_tag tag2)
     | Neg -> printf "Blame on the environment side: %s => %s\n"
                (string_of_tag tag1) (string_of_tag tag2)
     end;
     read_eval_print gamma env
  (* Fatal errors, or implementation bugs *)
  | CI_bug s | Eval_bug s ->
     printf "%s\n" s;
     exit 1

let initial_gamma = Environment.empty
let initial_env = Environment.empty

let _ = read_eval_print initial_gamma initial_env
