open Util.Error

(* used in parser.mly *)
exception Syntax_error of range * string

type id = string

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyList of ty
  | TyDyn                       (* the dynamic/unknown type *)

type binOp = Plus | Minus | Mult | Div | Lt | Gt | Eq | LE | GE | LAnd | LOr

(* NOTE: defined here, because Eval uses this *)
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

(* Gradually Typed Lambda Calculus (surface language) *)
module G = struct
  type exp =
    | Var of range * id
    | ILit of range * int
    | BLit of range * bool
    | BinOp of range * binOp * exp * exp
    | IfExp of range * exp * exp * exp
    | LetExp of range * bindings * exp
    (* Type annotation is mandatory at this moment. *)
    (* FunExp (_,x,t,e) ==> [fun (x:t) -> e] *)
    | FunExp of range * id * ty * exp
    | AppExp of range * exp * exp
    (* LetRec (_,[(f,x,t1,t2,e1)],e2) ==>
     * [let rec f (x:t1) : t2 = e1 in e2]
     * where t2 is return type annotation *)
    | LetRecExp of range * rec_bindings * exp
    | NilLit of range * ty
    | ConsExp of range * exp * exp
    (* MatchExp (_, e1, e2, x, y, e3) ==>
     * match e1 with [] -> e2 | x :: y -> e3 *)
    | MatchExp of range * exp * exp * id * id * exp

  and bindings = (id * exp) list
  and rec_bindings = (id * id * ty * ty * exp) list

  type program =
    | Exp of exp
    (* LetDecl(x,e) ==> [let x = e] *)
    | LetDecl of bindings
    (* LetRecDecl([(x,y,t1,t2)],e) ==> [let rec x (y:t1) : t2 = e] *)
    | LetRecDecl of rec_bindings

  let range_of_exp = function
    | Var (r,_) | ILit (r,_) | BLit (r,_)
      | BinOp (r,_,_,_) | IfExp (r,_,_,_) | LetExp (r,_,_)
      | FunExp (r,_,_,_) | AppExp (r,_,_) | LetRecExp (r,_,_) -> r

end

(* Cast Calculus (intermediate language) *)
module C = struct
  type exp =
    | Var of range * id
    | ILit of range * int
    | BLit of range * bool
    | BinOp of range * binOp * exp * exp
    | IfExp of range * exp * exp * exp
    | LetExp of range * bindings * exp
    | FunExp of range * id * ty * exp
    | AppExp of range * exp * exp
    | LetRecExp of range * rec_bindings * exp
    | NilLit of range * ty
    | ConsExp of range * exp * exp
    | MatchExp of range * exp * exp * id * id * exp
    (* CastExp(f,t1,t2) ==> [f: t1 => t2]  *)
    | CastExp of range * exp * ty * ty

  and bindings = (id * exp) list
  and rec_bindings = (id * id * ty * ty * exp) list

  type program =
    | Exp of exp
    | LetDecl of bindings
    | LetRecDecl of rec_bindings

  let range_of_exp = function
    | Var (r,_) | ILit (r,_) | BLit (r,_)
      | BinOp (r,_,_,_) | IfExp (r,_,_,_) | LetExp (r,_,_)
      | FunExp (r,_,_,_) | AppExp (r,_,_) | LetRecExp (r,_,_)
      | CastExp (r,_,_,_) -> r

end
