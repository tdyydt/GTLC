{
module P = Parser
open Util.Error

let reservedWords = [
  (* Keywords *)
  ("true", fun r -> P.TRUE r);
  ("false", fun r -> P.FALSE r);
  ("fun", fun r -> P.FUN r);
  ("if", fun r -> P.IF r);      (* if *)
  ("then", fun r -> P.THEN r);
  ("else", fun r -> P.ELSE r);
  ("let", fun r -> P.LET r);    (* let *)
  ("in", fun r -> P.IN r);
  ("rec", fun r -> P.REC r);
  ("and", fun r -> P.AND r);
  (* ("match", P.MATCH);           (\* match *\)
   * ("with", P.WITH); *)
  ("int", fun r -> P.INT r);    (* Types *)
  ("bool", fun r -> P.BOOL r);
]

(* range_of_lexbuf *)
let range_of lexbuf = {
    start_p=Lexing.lexeme_start_p lexbuf;
    end_p=Lexing.lexeme_end_p lexbuf;
  }
}

(* '\012' は10進で，Form Feed (\f) を表す *)
(* '\r' も追加？ *)
let space = [' ' '\t' '\012' '\n']

rule main = parse
| space+ { main lexbuf }
| ['0'-'9']+
    { let r = range_of lexbuf in
      let i = int_of_string (Lexing.lexeme lexbuf) in
      P.INTV { value=i; range=r} }

| "(*" { comment 0 lexbuf }     (* Entering comment mode *)
| "(" { P.LPAREN (range_of lexbuf) }
| ")" { P.RPAREN (range_of lexbuf) }
| ";;" { P.SEMISEMI (range_of lexbuf) }
| "+" { P.PLUS (range_of lexbuf) } (* arith *)
| "-" { P.MINUS (range_of lexbuf) }
| "*" { P.MULT (range_of lexbuf) }
| "/" { P.DIV (range_of lexbuf) }
| "&&" { P.LAND (range_of lexbuf) } (* logical *)
| "||" { P.LOR (range_of lexbuf) }
| "<" { P.LT (range_of lexbuf) } (* relational *)
| ">" { P.GT (range_of lexbuf) }
| "<=" { P.LE (range_of lexbuf) }
| ">=" { P.GE (range_of lexbuf) }
| "=" { P.EQ (range_of lexbuf) } (* let, relational *)
| "->" { P.RARROW (range_of lexbuf) }
| "?" { P.QU (range_of lexbuf) } (* the unknown type *)
| ":" { P.COLON (range_of lexbuf) } (* type annot *)

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      let r = range_of lexbuf in
      try
        (List.assoc id reservedWords) r
      with
      _ -> P.ID { value=id; range=r }
     }
| eof { exit 0 }

and comment level = parse
| "*)" {
  if level = 0 then main lexbuf
  else comment (level - 1) lexbuf
  }
| "(*" { comment (level + 1) lexbuf }
| _ { comment level lexbuf }    (* ignore any char *)
| eof { failwith "Comment is not closed." }
