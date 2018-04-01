%{
open Syntax
open Syntax.G
     (* open すると，非終端記号の名前と，
      * AST の variant の名前とが見分けづらくなりがち *)
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT DIV
%token LT GT
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ (* AND REC *)
%token RARROW FUN
%token INT BOOL QU              (* Types *)
%token COLON

%token <int> INTV
%token <Syntax.id> ID

(* precedence: lower to higher *)
%right prec_let prec_fun
%right prec_if
%left LT GT (* EQ, いまEQは，2項演算ではない *)
%left PLUS MINUS
%left MULT DIV

%start toplevel
%type <Syntax.G.exp> toplevel     (* program に変更？ *)
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

  | IF e1=expr THEN e2=expr ELSE e3=expr %prec prec_if
    { IfExp (e1, e2, e3) }
  | LET x=ID EQ e1=expr IN e2=expr %prec prec_let
    { LetExp (x, e1, e2) }
  (* この規則の優先順位は，default では一番右の RARROW の優先順位と同じだが，
   * RARROW が fun ty にも現れることを考えると prec_fun の導入も妥当？ *)
  (* TODO: Is the parenthesis needed?? in (x:t) *)
  | FUN LPAREN x=ID COLON t=ty RPAREN RARROW e=expr %prec prec_fun
    { FunExp (x, t, e) }
  | e=app_expr { e }

(* prec_app を使って conflicts を解消しようとしたが，上手くいかない？？
 * app に関してのみ，優先度に応じた nonterminal を追加する方針を利用 *)
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
