open Syntax
open Printf

let with_paren flag s =
  if flag then "(" ^ s ^ ")" else s

(* precedence of type *)
(* larger number means higher precedence *)
let prec_ty = function
  | (TyInt | TyBool | TyDyn) -> 2
  | TyFun _ -> 1

let ge_ty t1 t2 = (prec_ty t1) >= (prec_ty t2)

let rec string_of_ty = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) as t ->
     sprintf "%s -> %s"
       (with_paren (ge_ty t t1) (string_of_ty t1)) (string_of_ty t2)
  | TyDyn -> "?"

(* binOp *)
let string_of_binop = function
  | Plus  -> "+"
  | Minus -> "-"
  | Mult  -> "*"
  | Div   -> "/"
  | Lt    -> "<"
  | Gt    -> ">"
  | Eq    -> "="
  | LE    -> "<="
  | GE    -> ">="
  | LAnd  -> "&&"
  | LOr   -> "||"

(* 1~10 で返す *)
let prec_binop = function
  | LOr -> 1
  | LAnd -> 2
  | Lt | Gt | Eq | LE | GE -> 3
  | Plus | Minus -> 4
  | Mult | Div -> 5


module G = struct
  open Syntax.G
  (* precedence of expression *)
  let prec_exp = function
    | LetExp _ | FunExp _ -> 10
    | IfExp _ -> 20
    | BinOp (op, _, _) -> 30 + prec_binop op
    | AppExp _ -> 40
    | Var _ | ILit _ | BLit _ -> 50
    | FixExp _ -> assert false

  (* e1 > e2 : e1 associates stronger than e2 *)
  let gt_exp e1 e2 = (prec_exp e1) > (prec_exp e2)
  (* e1 >= e2 *)
  let ge_exp e1 e2 = (prec_exp e1) >= (prec_exp e2)

  let rec string_of_exp = function
    | Var x -> x
    | ILit n -> string_of_int n
    | BLit b -> string_of_bool b
    | BinOp (op, e1, e2) as e ->
       (* Left assoc *)
       sprintf "%s %s %s"
         (with_paren (gt_exp e e1) (string_of_exp e1)) (string_of_binop op)
         (with_paren (ge_exp e e2) (string_of_exp e2))
    | IfExp (e1, e2, e3) as e ->
       (* e1,e2 は If 等ならカッコが要る *)
       (* e3 にカッコは不要 *)
       sprintf "if %s then %s else %s"
         (with_paren (ge_exp e e1) (string_of_exp e1))
         (with_paren (ge_exp e e2) (string_of_exp e2))
         (string_of_exp e3)
    (* LetRecExp *)
    | LetExp (x, FixExp (x', y, t1, t2, e1), e2) as e ->
       assert (x = x');         (* function name *)
       sprintf "let rec %s (%s : %s) : %s = %s in %s"
         x y (string_of_ty t1) (string_of_ty t2)
         (with_paren (ge_exp e e1) (string_of_exp e1))
         (string_of_exp e2)
    | LetExp (x, e1, e2) as e ->
       sprintf "let %s = %s in %s"
         x (with_paren (ge_exp e e1) (string_of_exp e1))
         (string_of_exp e2)
    | FunExp (x, t, e1) ->
       sprintf "fun (%s : %s) -> %s"
         x (string_of_ty t) (string_of_exp e1)
    | AppExp (e1, e2) as e ->
       (* Left assoc *)
       sprintf "%s %s"
         (with_paren (gt_exp e e1) (string_of_exp e1))
         (with_paren (ge_exp e e2) (string_of_exp e2))
    | FixExp _ -> assert false

  let rec string_of_program = function
    | Exp e -> string_of_exp e
    (* LetRecDecl *)
    | LetDecl (x, FixExp (x', y, t1, t2, e)) ->
       assert (x = x');
       sprintf "let rec %s (%s : %s) : %s = %s"
         x y (string_of_ty t1) (string_of_ty t2) (string_of_exp e)
    | LetDecl (x, e) ->
       sprintf "let %s = %s" x (string_of_exp e)
end

module C = struct
  open Syntax.C
  (* precedence of expression *)
  let prec_exp = function
    | LetExp _ | FunExp _ -> 10
    | IfExp _ -> 20
    | CastExp _ -> 21           (* ?????? *)
    | BinOp (op, _, _) -> 30 + prec_binop op
    | AppExp _ -> 40
    | Var _ | ILit _ | BLit _ -> 50
    | FixExp _ -> assert false

  (* f1 > f2 : f1 associates stronger than f2 *)
  let gt_exp f1 f2 = (prec_exp f1) > (prec_exp f2)
  (* f1 >= f2 *)
  let ge_exp f1 f2 = (prec_exp f1) >= (prec_exp f2)

  let rec string_of_exp = function
    | Var x -> x
    | ILit n -> string_of_int n
    | BLit b -> string_of_bool b
    | BinOp (op, f1, f2) as f ->
       (* Left assoc *)
       sprintf "%s %s %s"
         (with_paren (gt_exp f f1) (string_of_exp f1)) (string_of_binop op)
         (with_paren (ge_exp f f2) (string_of_exp f2))
    | IfExp (f1, f2, f3) as f ->
       (* f1,f2 は If 等ならカッコが要る *)
       (* f3 にカッコは不要 *)
       sprintf "if %s then %s else %s"
         (with_paren (ge_exp f f1) (string_of_exp f1))
         (with_paren (ge_exp f f2) (string_of_exp f2))
         (string_of_exp f3)
    (* LetRecExp *)
    | LetExp (x, FixExp (x', y, t1, t2, f1), f2) as f ->
       assert (x = x');
       sprintf "let rec %s (%s : %s) : %s = %s in %s"
         x y (string_of_ty t1) (string_of_ty t2)
         (with_paren (ge_exp f f1) (string_of_exp f1))
         (string_of_exp f2)
    | LetExp (x, f1, f2) as f ->
       sprintf "let %s = %s in %s"
         x (with_paren (ge_exp f f1) (string_of_exp f1))
         (string_of_exp f2)
    | FunExp (x, t, f1) ->
       sprintf "fun (%s : %s) -> %s"
         x (string_of_ty t) (string_of_exp f1)
    | AppExp (f1, f2) as f ->
       (* Left assoc *)
       sprintf "%s %s"
         (with_paren (gt_exp f f1) (string_of_exp f1))
         (with_paren (ge_exp f f2) (string_of_exp f2))
    | CastExp (f1, t1, t2) as f ->
       sprintf "%s : %s => %s"
         (with_paren (ge_exp f f1) (string_of_exp f1))
         (string_of_ty t1) (string_of_ty t2)
    | FixExp _ -> assert false

  let rec string_of_program = function
    | Exp f -> string_of_exp f
    (* LetRecDecl *)
    | LetDecl (x, FixExp (x', y, t1, t2, f)) ->
       assert (x = x');
       sprintf "let rec %s (%s : %s) : %s = %s"
         x y (string_of_ty t1) (string_of_ty t2) (string_of_exp f)
    | LetDecl (x, f) ->
       sprintf "let %s = %s" x (string_of_exp f)
end
