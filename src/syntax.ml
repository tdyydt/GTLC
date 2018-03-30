type id = string

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyDyn                       (* the dynamic type ? *)


type binOp = Plus | Mult | Lt (* | LAnd | LOr *)

(* Gradual: exp *)
(* Gradually typed surface language *)
(* TODO: 別ファイルか，モジュールにする *)
type exp =
  | Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  (* | If  *)
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  (* | LetRecExp *)
