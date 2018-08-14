open Printf
open Syntax
open Syntax.G
open Typing
open Typing.G
open Translate
open Eval
open Stringify

let _ =
  Js.export "gtlcLib" @@
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
              let v = eval_exp env f in

              let result = sprintf "val - : %s = %s"
                             (string_of_ty t)
                             (string_of_value v) in
              (* or, Js.string result *)
              object%js
                val status = 0  (* ok *)
                val result = Js.string result
              end
           | LetDecl _ | LetRecDecl _ ->
              let m = "Let declarations are not supported. Use let-in syntax instead." in
              object%js
                val status = 3  (* other error *)
                val result = Js.string m
              end)
        with
        | Typing_error m ->
           object%js
             val status = 1     (* static type error *)
             val result = Js.string m
           end
        | Blame (tag1, tag2) ->
           let result = sprintf "Blame: %s is incompatible with tag %s"
                          (string_of_tag tag2) (string_of_tag tag1) in
           object%js
             val status = 2
             val result = Js.string result
           end
        | Eval_error m ->
           object%js
             val status = 3     (* runtime error *)
             val result = Js.string m
           end
        | CI_error m ->
           object%js
             val status = 3
             val result = Js.string ("Error: " ^ m)
           end
        | Parsing.Parse_error -> (* Menhir *)
           object%js
             val status = 3
             val result = Js.string "Parsing error"
           end
        | Failure m ->          (* lexing *)
           object%js
             val status = 3
             val result = Js.string m
           end
        | _ ->
           object%js
             val status = 3
             val result = Js.string "Unexpected error"
           end
    end
