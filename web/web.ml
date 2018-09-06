open Util.Error
open Format
open Syntax
open Typing
open Translate
open Eval

(* Status:
 * 0 => ok
 * 1 => lexer/parser error
 * 2 => static type error
 * 3 => runtime type error
 * 4 => (implementation bug) *)

let emptystr = Js.string ""

let _ =
  Js.export "GTLC" @@
    object%js
      method eval input =
        let s = Js.to_string input ^ ";;" in
        let lexbuf = Lexing.from_string s in
        try
          let pgm = Parser.toplevel Lexer.main lexbuf in
          (* expressions only *)
          (match pgm with
           | Exp e ->
              let open Typing in
              let gamma = Environment.empty in
              let t = G.ty_exp gamma e in
              let f, t' = translate_exp gamma e in
              (* Should be t = t' *)
              let env = Environment.empty in
              (try
                 let v = eval_exp env f in
                 let open Pp in
                 let res = asprintf "val - : %a = %a" pp_ty t pp_value v in
                 object%js
                   val status = 0
                   val detail = Js.string res
                   val t = Js.string (asprintf "%a" pp_ty t)
                   val f = Js.string (asprintf "%a" C.pp_exp f)
                   val v = Js.string (asprintf "%a" pp_value v)
                 end
               with
               | Blame (r, plr, tag1, tag2) ->
                  let open Pp in
                  let result =
                    begin match plr with
                    | Pos -> asprintf "Blame on the expression side: %s => %s\n"
                               (string_of_tag tag1) (string_of_tag tag2)
                    | Neg -> asprintf "Blame on the environment side: %s => %s\n"
                               (string_of_tag tag1) (string_of_tag tag2)
                    end in
                  object%js
                    val status = 3
                    val detail = Js.string (asprintf "%a" print_range r)
                    val t = Js.string (asprintf "%a" pp_ty t)
                    val f = Js.string (asprintf "%a" C.pp_exp f)
                    val v = Js.string result
                  end)
           | LetDecl _ | LetRecDecl _ -> (* not implemented yet *)
              object%js
                val status = 4  (* not implemented *)
                val detail = Js.string ("Let declarations are not supported. Use let-in syntax instead.")
                val t = emptystr
                val f = emptystr
                val v = emptystr
              end)
        with
        | Failure m ->        (* e.g. lexing *)
           object%js
             val status = 1
             val detail = Js.string (asprintf "Failure: %s" m)
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end
        | Parser.Error ->       (* Menhir *)
           let token = Lexing.lexeme lexbuf in
           let m = asprintf "Parser.Error: unexpected token: %s" token in
           object%js
             val status = 1
             val detail = Js.string m
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end
        | Syntax_error (r, m) ->
           object%js
             val status = 1
             (* TODO: replace newline with br *)
             val detail = Js.string (asprintf "%a\n%s\n" print_range r m)
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end

        | Type_error (r, msg) ->
           object%js
             val status = 2
             val detail = Js.string (asprintf "%a" print_range r)
             val t = Js.string msg
             val f = emptystr
             val v = emptystr
           end

        | CI_bug m | Eval_bug m ->
           object%js
             val status = 4
             val detail = Js.string m
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end
    end
