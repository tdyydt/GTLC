%{
open Syntax
open Syntax.G
open Printf
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT DIV
%token LT GT LE GE LAND LOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ REC
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

%start toplevel
%type <Syntax.G.program> toplevel
%%

toplevel :
  | p=program SEMISEMI { p }

(* parameter *)
para :
  | LPAREN x=ID COLON t=ty RPAREN { (x,t) }
  | x=ID { Util.err (sprintf "Type annotation for %s is mandatory." x) }

(* let x = e *)
let_binding :
  | LET x=ID paras=para* EQ e=expr
    { (* let f (x:t1) (y:t2) = e
       * ==> let f = fun (x:t1) -> fun (x:t2) -> e *)
      let e' = List.fold_right
                 (* acc for accumulator *)
                 (fun (x,t) e_acc -> FunExp (x, t, e_acc))
                 paras e
      in (x, e') }

(* let rec f (x:S) : T = e *)
rec_binding :
  (* 最初の para は特別扱い *)
  (* let rec x (y:t1) [paras] : t2 = ... *)
  | LET REC funid=ID para=para paras=para* COLON retty=ty EQ e0=expr
    { let paraid, paraty = para in
      let e' = List.fold_right
                 (fun (x,t) e_acc -> FunExp (x, t, e_acc))
                 paras e0
      in (funid, paraid, paraty, retty, e') }

  (* Not necessary *)
  | LET REC funid=ID para para* EQ e0=expr
    { Util.err (sprintf "Return type annotation for %s is mandatory." funid) }

program :
  | e=expr { Exp e }
  | p=let_binding { let x, e = p in LetDecl (x,e) }
  (* tup for tuple *)
  | tup=rec_binding { let (x, y, t1, t2, e) = tup in
                      LetRecDecl (x, y, t1, t2, e) }

expr :
  | e1=expr op=binop e2=expr { BinOp (op, e1, e2) }

  | IF e1=expr THEN e2=expr ELSE e3=expr %prec prec_if
    { IfExp (e1, e2, e3) }
  | p=let_binding IN e2=expr %prec prec_let
    { let x, e1 = p in LetExp (x, e1, e2) }

  (* paras must not be empty *)
  | FUN paras=para+ RARROW e0=expr %prec prec_fun
    { List.fold_right
        (fun (x,t) e_acc -> FunExp (x, t, e_acc))
        paras e0 }

  | tup=rec_binding IN e2=expr %prec prec_let
    { let (x, y, t1, t2, e1) = tup in
      LetRecExp (x, y, t1, t2, e1, e2) }
  | e=unary_expr { e }

(* %inline is necessary; see Sec 5.3 of manual *)
%inline binop :
  | PLUS { Plus }               (* arith *)
  | MINUS { Minus }
  | MULT { Mult }
  | DIV { Div }
  | LT { Lt }                   (* relational *)
  | GT { Gt }
  | EQ { Eq }
  | LE { LE }
  | GE { GE }
  | LAND { LAnd }               (* logical *)
  | LOR { LOr }

(* `n-1` should be BinOp(Minus, n, 1), not App(n, -1) *)
unary_expr :
  | MINUS e=unary_expr { BinOp (Minus, ILit 0, e) }
  | e=app_expr { e }

(* To avoid conflicts *)
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
