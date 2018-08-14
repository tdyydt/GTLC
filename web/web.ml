open Printf
open Syntax
open Syntax.G
open Typing
open Typing.G
open Translate
open Eval
open Stringify

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
        try
          (* expressions only *)
          let s = Js.to_string input ^ ";;" in
          let pgm = Parser.toplevel Lexer.main (Lexing.from_string s) in
          (match pgm with
           | Exp e ->
              let gamma = Environment.empty in
              let t = ty_exp gamma e in
              let f, t' = translate_exp gamma e in
              (* Should be t = t' *)
              let env = Environment.empty in
              (try
                 let v = eval_exp env f in
                 let res = sprintf "val - : %s = %s"
                             (string_of_ty t) (string_of_value v) in
                 object%js
                   val status = 0
                   val detail = Js.string res
                   val t = Js.string (string_of_ty t)
                   val f = Js.string (C.string_of_exp f)
                   val v = Js.string (string_of_value v)
                 end
               with
               | Blame (tag1, tag2) ->
                  let result = sprintf "Blame: %s is incompatible with tag %s"
                                 (string_of_tag tag2) (string_of_tag tag1) in
                  object%js
                    val status = 3
                    val detail = emptystr
                    val t = Js.string (string_of_ty t)
                    val f = Js.string (C.string_of_exp f)
                    val v = Js.string result
                  end)
           | LetDecl _ | LetRecDecl _ -> (* not implemented yet *)
              let m = "Let declarations are not supported. Use let-in syntax instead." in
              object%js
                val status = 4  (* detail error *)
                val detail = Js.string m
                val t = emptystr
                val f = emptystr
                val v = emptystr
              end)
        with
        | Failure m ->          (* lexing *)
           object%js
             val status = 1
             val detail = Js.string m
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end
        | Parsing.Parse_error -> (* Menhir *)
           object%js
             val status = 1
             val detail = emptystr
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end
        | Syntax_error m ->
           object%js
             val status = 1
             val detail = Js.string m
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end
        | Typing_error m ->
           object%js
             val status = 2
             val detail = emptystr
             val t = Js.string m
             val f = emptystr
             val v = emptystr
           end
        | Eval_error m ->
           object%js
             val status = 4
             val detail = Js.string m
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end
        | CI_error m ->
           object%js
             val status = 4
             val detail = Js.string m
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end
        | _ ->
           object%js
             val status = 4
             val detail = Js.string "Unexpected error"
             val t = emptystr
             val f = emptystr
             val v = emptystr
           end
    end
