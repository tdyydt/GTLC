%{
open Syntax
     (* open すると，非終端記号の名前と，
      * AST の variant の名前とが見分けづらくなりがち *)
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT DIV
%token LT GT
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ AND REC
%token RARROW FUN
%token INT BOOL QU              (* Types *)
%token COLON

%token <int> INTV
%token <Syntax.id> ID

(* precedence: lower to higher *)
                   (* fun とか自信がない，確かに RARROW でも良さげ
                    * 構文規則の優先順位の決め方について？？？？？？？？？
                    * letも右でいいのか？*)
%right prec_let prec_fun
%right prec_if
%left EQ LT GT
%left PLUS MINUS
%left MULT DIV
%left prec_app

%start toplevel
%type <Syntax.exp> toplevel     (* program に変更？ *)
%%

toplevel :
  | e=expr SEMISEMI { e }

expr :
  | e=simple_expr { e }
  | e1=expr PLUS e2=expr { BinOp (Plus, e1, e2) } (* arith *)
  | e1=expr MINUS e2=expr { BinOp (Minus, e1, e2) }
  | e1=expr MULT e2=expr { BinOp (Mult, e1, e2) }
  | e1=expr DIV e2=expr { BinOp (Div, e1, e2) }
  | e1=expr LT e2=expr { BinOp (Lt, e1, e2) } (* relational *)
  | e1=expr GT e2=expr { BinOp (Gt, e1, e2) }

  (* | IF *)
  | LET x=ID EQ e1=expr IN e2=expr %prec prec_let
    { LetExp (x, e1, e2) }
  | FUN x=ID COLON t=ty RARROW e=expr %prec prec_fun
    { FunExp (x, t, e) }
  | e1=simple_expr e2=simple_expr (* %prec prec_app *)
    { AppExp (e1, e2) }           (* application *)

simple_expr :             (* 括弧をつけなくても関数の引数になれる式 *)
  | LPAREN e=expr RPAREN { e }
  | n=INTV { ILit n }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | id=ID { Var id }


(* Types: for type annotation *)
ty :
  | t=simple_ty { t }
  | t1=simple_ty RARROW t2=ty { TyFun (t1, t2) }

simple_ty :
  | LPAREN t=ty RPAREN { t }
  | INT { TyInt }
  | BOOL { TyBool }
  | QU { TyDyn }
