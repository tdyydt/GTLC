%{
open Syntax
open Syntax.G
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT DIV
%token LT GT LE GE LAND LOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ (* AND REC *)
%token RARROW FUN
%token INT BOOL QU              (* Types *)
%token COLON

%token <int> INTV
%token <Syntax.id> ID

(* precedence: lower to higher *)
(* via: https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)
%right prec_let prec_fun
%right prec_if
%right LOR
%right LAND
%left LT GT EQ LE GE
%left PLUS MINUS
%left MULT DIV

%start toplevel (* expr *)
%type <Syntax.G.exp> toplevel     (* program に変更？ *)
(* %type <Syntax.G.exp> expr *)
%%

toplevel :
  | e=expr SEMISEMI { e }

expr :
  | e1=expr PLUS e2=expr { BinOp (Plus, e1, e2) } (* arith *)
  | e1=expr MINUS e2=expr { BinOp (Minus, e1, e2) }
  | e1=expr MULT e2=expr { BinOp (Mult, e1, e2) }
  | e1=expr DIV e2=expr { BinOp (Div, e1, e2) }
  | e1=expr LT e2=expr { BinOp (Lt, e1, e2) } (* relational *)
  | e1=expr GT e2=expr { BinOp (Gt, e1, e2) }
  | e1=expr EQ e2=expr { BinOp (Eq, e1, e2) }
  | e1=expr LE e2=expr { BinOp (LE, e1, e2) }
  | e1=expr GE e2=expr { BinOp (GE, e1, e2) }
  | e1=expr LAND e2=expr { BinOp (LAnd, e1, e2) } (* logical *)
  | e1=expr LOR e2=expr { BinOp (LOr, e1, e2) }

  | IF e1=expr THEN e2=expr ELSE e3=expr %prec prec_if
    { IfExp (e1, e2, e3) }
  | LET x=ID EQ e1=expr IN e2=expr %prec prec_let
    { LetExp (x, e1, e2) }
  (* TODO: Is the parenthesis needed?? in (x:t) *)
  | FUN LPAREN x=ID COLON t=ty RPAREN RARROW e=expr %prec prec_fun
    { FunExp (x, t, e) }
  | e=app_expr { e }

(* conflicts を解消できたが，他の部分との一貫性がない *)
app_expr :
  (* application: Left associative *)
  | e1=app_expr e2=simple_expr { AppExp (e1, e2) }
  | e=simple_expr { e }

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
