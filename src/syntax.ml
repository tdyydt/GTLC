open Util
open Printf

type id = string

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyDyn                       (* the dynamic type ? *)

let rec string_of_ty = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) ->
     (* If t1 is TyFun, put it in parentheses. *)
     let str1 = match t1 with
       | TyFun (_,_) as t1 -> sprintf "(%s)" (string_of_ty t1)
       | _ as t1 -> string_of_ty t1 in
     sprintf "%s -> %s" str1 (string_of_ty t2)
  | TyDyn -> "?"

type binOp = Plus | Minus | Mult | Div | Lt | Gt | Eq | LE | GE | LAnd | LOr

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

(* Gradually typed surface language *)
module G = struct
  type exp =
    | Var of id
    | ILit of int
    | BLit of bool
    | BinOp of binOp * exp * exp
    | IfExp of exp * exp * exp
    | LetExp of id * exp * exp
    (* Type annotation is mandatory at this moment. *)
    (* FunExp (x,t,e) ==> [fun (x:t) -> e] *)
    | FunExp of id * ty * exp
    | AppExp of exp * exp
    (* LetRec (f,x,t1,t2,e1,e2) ==>
     * [let rec f (x:t1) : t2 = e1 in e2]
     * where t2 is return type annotation *)
    | LetRecExp of id * id * ty * ty * exp * exp

  let rec string_of_exp = function
    | Var x -> x
    | ILit n -> string_of_int n
    | BLit b -> string_of_bool b
    | BinOp (op, e1, e2) ->
       sprintf "(%s %s %s)"
         (string_of_exp e1) (string_of_binop op) (string_of_exp e2)
    | IfExp (e1, e2, e3) ->
       sprintf "(if %s then %s else %s)"
         (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
    | LetExp (x, e1, e2) ->
       sprintf "(let %s = %s in %s)"
         x (string_of_exp e1) (string_of_exp e2)
    | FunExp (x, t, f) ->
       sprintf "(fun (%s : %s) -> %s)"
         x (string_of_ty t) (string_of_exp f)
    | AppExp (e1, e2) ->
       sprintf "(%s %s)" (string_of_exp e1) (string_of_exp e2)
    | LetRecExp (x, y, t1, t2, e1, e2) ->
       sprintf "(let rec %s (%s : %s) : %s = %s in %s)"
         x y (string_of_ty t1) (string_of_ty t2)
         (string_of_exp e1) (string_of_exp e2)

  type program =
    | Exp of exp
    (* LetDecl(x,e) ==> [let x = e] *)
    | LetDecl of id * exp
    (* LetRecDecl(x,y,t1,t2,e) ==> [let rec x (y:t1) : t2 = e] *)
    | LetRecDecl of id * id * ty * ty * exp
end

(* Cast Calculus *)
module C = struct
  type exp =
    | Var of id
    | ILit of int
    | BLit of bool
    | BinOp of binOp * exp * exp
    | IfExp of exp * exp * exp
    | LetExp of id * exp * exp
    | FunExp of id * ty * exp
    | AppExp of exp * exp
    | LetRecExp of id * id * ty * ty * exp * exp
    (* CastExp(f,t1,t2) ==> [f: t1 => t2]  *)
    | CastExp of exp * ty * ty

  (* TODO: reduce parentheses, printer *)
  let rec string_of_exp = function
    | Var x -> x
    | ILit n -> string_of_int n
    | BLit b -> string_of_bool b
    | BinOp (op, f1, f2) ->
       sprintf "(%s %s %s)"
         (string_of_exp f1) (string_of_binop op) (string_of_exp f2)
    | IfExp (f1, f2, f3) ->
       sprintf "(if %s then %s else %s)"
         (string_of_exp f1) (string_of_exp f2) (string_of_exp f3)
    | LetExp (x, f1, f2) ->
       sprintf "(let %s = %s in %s)"
         x (string_of_exp f1) (string_of_exp f2)
    | FunExp (x, t, f) ->
       sprintf "(fun (%s : %s) -> %s)"
         x (string_of_ty t) (string_of_exp f)
    | AppExp (f1, f2) ->
       sprintf "(%s %s)" (string_of_exp f1) (string_of_exp f2)
    | LetRecExp (x, y, t1, t2, f1, f2) ->
       sprintf "(let rec %s (%s : %s) : %s = %s in %s)"
         x y (string_of_ty t1) (string_of_ty t2)
         (string_of_exp f1) (string_of_exp f2)
    | CastExp (f, t1, t2) ->
       sprintf "(%s : %s => %s)"
         (string_of_exp f) (string_of_ty t1) (string_of_ty t2)

  type program =
    | Exp of exp
    | LetDecl of id * exp
    | LetRecDecl of id * id * ty * ty * exp

  let rec string_of_program = function
    | Exp f -> string_of_exp f
    | LetDecl (x, f) ->
       sprintf "let %s = %s" x (string_of_exp f)
    | LetRecDecl (x, y, t1, t2, f) ->
       sprintf "let rec %s (%s : %s) : %s = %s"
         x y (string_of_ty t1) (string_of_ty t2) (string_of_exp f)
end
