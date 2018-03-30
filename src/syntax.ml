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
     let str1 = match t1 with
       | TyFun (_,_) as t1 -> "(" ^ string_of_ty t1 ^ ")"
       | _ as t1 -> string_of_ty t1 in
     let str2 = string_of_ty t2 in
     str1 ^ " -> " ^ str2
  | TyDyn -> "?"

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
  (* 型注釈が必要 *)
  (* FunExp (x,t,e) => fun (x:t) -> e *)
  | FunExp of id * ty * exp
  | AppExp of exp * exp
  (* | LetRecExp *)
