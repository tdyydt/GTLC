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

(* TODO: Eq は？？ *)
type binOp = Plus | Minus | Mult | Div | Lt | Gt (* | LAnd | LOr *)

(* Gradual: exp *)
(* Gradually typed surface language *)
(* TODO: 別ファイルか，モジュールにする *)
module G = struct
  type exp =
    | Var of id
    | ILit of int
    | BLit of bool
    | BinOp of binOp * exp * exp
    | IfExp of exp * exp * exp
    | LetExp of id * exp * exp
    (* 型注釈が必要 *)
    (* fun (x:t) -> e
     * => FunExp (x,t,e) *)
    | FunExp of id * ty * exp
    | AppExp of exp * exp
    (* | LetRecExp *)
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
    (* [f: t1 => t2] => CastExp(f,t1,t2) *)
    | CastExp of exp * ty * ty
    (* | BlameExp *)
end
