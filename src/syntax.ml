open Util.Error

(* used in parser.mly *)
exception Syntax_error of string
let err s = raise (Syntax_error s)

type id = string

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyDyn                       (* the dynamic/unknown type *)

type binOp = Plus | Minus | Mult | Div | Lt | Gt | Eq | LE | GE | LAnd | LOr

(* Gradually typed surface language *)
module G = struct
  type exp =
    | Var of range * id
    | ILit of range * int
    | BLit of range * bool
    | BinOp of range * binOp * exp * exp
    | IfExp of range * exp * exp * exp
    | LetExp of range * (id * exp) list * exp
    (* Type annotation is mandatory at this moment. *)
    (* FunExp (x,t,e) ==> [fun (x:t) -> e] *)
    | FunExp of range * id * ty * exp
    | AppExp of range * exp * exp
    (* LetRec ([(f,x,t1,t2,e1)],e2) ==>
     * [let rec f (x:t1) : t2 = e1 in e2]
     * where t2 is return type annotation *)
    | LetRecExp of range * (id * id * ty * ty * exp) list * exp

  type program =
    | Exp of exp
    (* LetDecl(x,e) ==> [let x = e] *)
    | LetDecl of (id * exp) list
    (* LetRecDecl([(x,y,t1,t2)],e) ==> [let rec x (y:t1) : t2 = e] *)
    | LetRecDecl of (id * id * ty * ty * exp) list

  let range_of_exp = function
    | Var (r,_) | ILit (r,_) | BLit (r,_)
      | BinOp (r,_,_,_) | IfExp (r,_,_,_) | LetExp (r,_,_)
      | FunExp (r,_,_,_) | AppExp (r,_,_) | LetRecExp (r,_,_) -> r

end

(* Cast Calculus *)
module C = struct
  type exp =
    | Var of id
    | ILit of int
    | BLit of bool
    | BinOp of binOp * exp * exp
    | IfExp of exp * exp * exp
    | LetExp of (id * exp) list * exp
    | FunExp of id * ty * exp
    | AppExp of exp * exp
    | LetRecExp of (id * id * ty * ty * exp) list * exp
    (* CastExp(f,t1,t2) ==> [f: t1 => t2]  *)
    | CastExp of exp * ty * ty

  type program =
    | Exp of exp
    | LetDecl of (id * exp) list
    | LetRecDecl of (id * id * ty * ty * exp) list
end
