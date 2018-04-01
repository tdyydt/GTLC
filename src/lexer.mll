{
module P = Parser
let reservedWords = [
  (* Keywords *)
  ("true", P.TRUE);
  ("false", P.FALSE);
  ("fun", P.FUN);
  ("if", P.IF);                 (* if *)
  ("then", P.THEN);
  ("else", P.ELSE);
  ("let", P.LET);               (* let *)
  ("in", P.IN);
  (* ("and", P.AND);
   * ("rec", P.REC); *)
  (* ("match", P.MATCH);           (\* match *\)
   * ("with", P.WITH); *)
  ("int", P.INT);               (* Types *)
  ("bool", P.BOOL);
]
}

(* '\012' は10進で，Form Feed を表す
 * '\f' と書くが，OCaml ではこの書き方ができない *)
(* '\r' も追加？ *)
let space = [' ' '\t' '\012' '\n']

rule main = parse
| space+ { main lexbuf }
| "-"? ['0'-'9']+
    { P.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(*" { comment 0 lexbuf }     (* Entering comment mode *)
| "(" { P.LPAREN }
| ")" { P.RPAREN }
| ";;" { P.SEMISEMI }
| "+" { P.PLUS }                (* arith *)
| "-" { P.MINUS }
| "*" { P.MULT }
| "/" { P.DIV }
| "<" { P.LT }                  (* relation *)
| ">" { P.GT }
(* | "<=" { P.LE }
 * | ">=" { P.GE } *)
| "=" { P.EQ }
| "->" { P.RARROW }
| "?" { P.QU }                  (* the unknown type *)
| ":" { P.COLON }               (* type annot *)

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }

and comment level = parse
| "*)" {
  if level = 0 then main lexbuf
  else comment (level - 1) lexbuf
  }
| "(*" { comment (level + 1) lexbuf }
| _ { comment level lexbuf }    (* ignore any char *)
| eof { failwith "Comment is not closed." }
