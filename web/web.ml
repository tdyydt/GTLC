(* open Printf *)
open Syntax
open Typing
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
           | G.Exp e ->
              let gamma = Environment.empty in
              let t = ty_exp gamma e in

              let f, t' = translate_exp gamma e in
              (* Should be t = t' *)

              let env = Environment.empty in
              let v = eval_exp env f in

              let result = Printf.sprintf "val - : %s = %s"
                             (string_of_ty t)
                             (string_of_value v) in
              (* or, Js.string result *)
              object%js
                val status = 0  (* ok *)
                val result = Js.string result
              end
           | G.LetDecl _ | G.LetRecDecl _ ->
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
        | Eval_error m ->
           object%js
             val status = 2     (* runtime error *)
             val result = Js.string m
           end
        | Util.Error m | CI_error m ->
           object%js
             val status = 3
             val result = Js.string ("Error: " ^ m)
           end
        | Parsing.Parse_error ->
           object%js
             val status = 3
             val result = Js.string "Parsing error"
           end
        | Failure m ->
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
