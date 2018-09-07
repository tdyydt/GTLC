%{
open Syntax
open Syntax.G
open Util.Error
%}

%token <Util.Error.range> LPAREN RPAREN SEMISEMI
%token <Util.Error.range> PLUS MINUS MULT DIV
%token <Util.Error.range> LT GT LE GE LAND LOR
%token <Util.Error.range> IF THEN ELSE TRUE FALSE
%token <Util.Error.range> LET IN EQ REC AND
%token <Util.Error.range> RARROW FUN
%token <Util.Error.range> CONS LBRA RBRA AT MATCH WITH VBAR
%token <Util.Error.range> INT BOOL QU              (* Types *)
%token <Util.Error.range> COLON LIST

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
  | LPAREN x=ID COLON t=ty RPAREN { (x.value, t) }
  | x=ID { raise (Syntax_error
                    (x.range,
                     Format.sprintf "Type annotation for %s is mandatory." x.value)) }

(* x = e *)
let_binding :
  | id=ID paras=para* EQ e=expr
    { let r = join_range id.range (range_of_exp e) in (* ok? *)
      (* let f (x:t1) (y:t2) = e
       * ==> let f = fun (x:t1) -> fun (x:t2) -> e *)
      let e' = List.fold_right
                 (* acc for accumulator *)
                 (fun (x,t) e_acc -> FunExp (r, x, t, e_acc))
                 paras e
      in (id.value, e') }

(* f (x:S) : T = e *)
rec_binding :
  (* 最初の para は特別扱い *)
  (* let rec x (y:t1) [paras] : t2 = ... *)
  | funid=ID para=para paras=para* COLON retty=ty EQ e0=expr
    { let r = join_range funid.range (range_of_exp e0) in (* ok? *)
      let paraid, paraty = para in
      let e' = List.fold_right
                 (fun (x,t) e_acc -> FunExp (r, x, t, e_acc))
                 paras e0
      in (funid.value, paraid, paraty, retty, e') }

  (* Not necessary *)
  | funid=ID para para* r1=EQ e0=expr
    { raise (Syntax_error
               (join_range funid.range r1,
                Format.sprintf "Return type annotation for %s is mandatory." funid.value)) }

(* Expressions *)
expr :
  | e1=expr op=binop e2=expr
    { let r = join_range (range_of_exp e1) (range_of_exp e2) in
      BinOp (r, op, e1, e2) }

  | r0=IF e1=expr THEN e2=expr ELSE e3=expr %prec prec_if
    { let r = join_range r0 (range_of_exp e3) in
      IfExp (r, e1, e2, e3) }
  | r0=LET bindings=separated_nonempty_list(AND, let_binding)
    IN e=expr %prec prec_let
    { let r = join_range r0 (range_of_exp e) in
      LetExp (r, bindings, e) }

  (* paras must not be empty *)
  | r0=FUN paras=para+ RARROW e0=expr %prec prec_fun
    { let r = join_range r0 (range_of_exp e0) in
      List.fold_right
        (fun (x,t) e_acc -> FunExp (r, x, t, e_acc))
        paras e0 }

  | r0=LET REC rec_bindings=separated_nonempty_list(AND, rec_binding)
    IN e2=expr %prec prec_let
    { let r = join_range r0 (range_of_exp e2) in
      LetRecExp (r, rec_bindings, e2) }
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
  | r0=MINUS e=minus_expr
    { let r = join_range r0 (range_of_exp e) in
      match e with
      | ILit (_, n) -> ILit (r, -n)
      | e -> let zero = ILit (dummy_range, 0) in
             BinOp (r, Minus, zero , e) }
  | e=app_expr { e }

(* To avoid conflicts *)
app_expr :
  (* application: Left associative *)
  | e1=app_expr e2=simple_expr
    { let r = join_range (range_of_exp e1) (range_of_exp e2) in
      AppExp (r, e1, e2) }
  | e=simple_expr { e }

simple_expr :             (* 括弧をつけなくても関数の引数になれる式 *)
  | LPAREN e=expr RPAREN { e }
  | i=INTV { ILit (i.range, i.value) }
  | r=TRUE { BLit (r, true) }
  | r=FALSE { BLit (r, false) }
  | id=ID { Var (id.range, id.value) }


(* Types: for type annotation *)
ty :
  | t=simple_ty { t }
  | t1=simple_ty RARROW t2=ty { TyFun (t1, t2) }

simple_ty :
  | LPAREN t=ty RPAREN { t }
  | INT { TyInt }
  | BOOL { TyBool }
  | QU { TyDyn }
