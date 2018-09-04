%{
open Syntax
open Syntax.G
open Printf
%}

%token <Util.Error.range> LPAREN RPAREN SEMISEMI
%token <Util.Error.range> PLUS MINUS MULT DIV
%token <Util.Error.range> LT GT LE GE LAND LOR
%token <Util.Error.range> IF THEN ELSE TRUE FALSE
%token <Util.Error.range> LET IN EQ REC AND
%token <Util.Error.range> RARROW FUN
%token <Util.Error.range> INT BOOL QU              (* Types *)
%token <Util.Error.range> COLON

%token <int Util.Error.with_range> INTV
%token <Syntax.id Util.Error.with_range> ID

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

program :
  | e=expr { Exp e }
  | LET bindings=separated_nonempty_list(AND, let_binding)
    { LetDecl bindings }
  | LET REC rec_bindings=separated_nonempty_list(AND, rec_binding)
    { LetRecDecl rec_bindings }

(* parameter *)
para :
  | LPAREN x=ID COLON t=ty RPAREN { (x.value,t) }
  | x=ID { err (sprintf "Type annotation for %s is mandatory." x.value) }

(* x = e *)
let_binding :
  | x=ID paras=para* EQ e=expr
    { (* let f (x:t1) (y:t2) = e
       * ==> let f = fun (x:t1) -> fun (x:t2) -> e *)
      let e' = List.fold_right
                 (* acc for accumulator *)
                 (fun (x,t) e_acc -> FunExp (x, t, e_acc))
                 paras e
      in (x.value, e') }

(* f (x:S) : T = e *)
rec_binding :
  (* 最初の para は特別扱い *)
  (* let rec x (y:t1) [paras] : t2 = ... *)
  | funid=ID para=para paras=para* COLON retty=ty EQ e0=expr
    { let paraid, paraty = para in
      let e' = List.fold_right
                 (fun (x,t) e_acc -> FunExp (x, t, e_acc))
                 paras e0
      in (funid.value, paraid, paraty, retty, e') }

  (* Not necessary *)
  | funid=ID para para* EQ e0=expr
    { err (sprintf "Return type annotation for %s is mandatory." funid.value) }

(* Expressions *)
expr :
  | e1=expr op=binop e2=expr { BinOp (op, e1, e2) }

  | IF e1=expr THEN e2=expr ELSE e3=expr %prec prec_if
    { IfExp (e1, e2, e3) }
  | LET bindings=separated_nonempty_list(AND, let_binding)
    IN e=expr %prec prec_let
    { LetExp (bindings, e) }

  (* paras must not be empty *)
  | FUN paras=para+ RARROW e0=expr %prec prec_fun
    { List.fold_right
        (fun (x,t) e_acc -> FunExp (x, t, e_acc))
        paras e0 }

  | LET REC rec_bindings=separated_nonempty_list(AND, rec_binding)
    IN e2=expr %prec prec_let
    { LetRecExp (rec_bindings, e2) }
  | e=minus_expr { e }

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
minus_expr :
  | MINUS e=minus_expr
    { match e with
      | ILit n -> ILit (-n)
      | e -> BinOp (Minus, ILit 0, e) }
  | e=app_expr { e }

(* To avoid conflicts *)
app_expr :
  (* application: Left associative *)
  | e1=app_expr e2=simple_expr { AppExp (e1, e2) }
  | e=simple_expr { e }

simple_expr :             (* 括弧をつけなくても関数の引数になれる式 *)
  | LPAREN e=expr RPAREN { e }
  | n=INTV { ILit n.value }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | id=ID { Var id.value }


(* Types: for type annotation *)
ty :
  | t=simple_ty { t }
  | t1=simple_ty RARROW t2=ty { TyFun (t1, t2) }

simple_ty :
  | LPAREN t=ty RPAREN { t }
  | INT { TyInt }
  | BOOL { TyBool }
  | QU { TyDyn }
