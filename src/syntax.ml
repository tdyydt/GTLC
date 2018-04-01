open Util
open Printf

type id = string
let string_of_id x = x          (* 使うかどうか悩む *)

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
       | TyFun (_,_) as t1 -> sprintf "(%s)" (string_of_ty t1)
       | _ as t1 -> string_of_ty t1 in
     let str2 = string_of_ty t2 in
     str1 ^ " -> " ^ str2
  | TyDyn -> "?"

(* TODO: Eq は？？ *)
type binOp = Plus | Minus | Mult | Div | Lt | Gt (* | LAnd | LOr *)

let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Gt -> ">"

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
    (* [fun (x:t) -> e] => FunExp (x,t,e) *)
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

  (* compoud exp の文字列の際には，必ず (,) を付ける
   * 優先度に応じてカッコを減らすのは，面倒過ぎる？ *)
  let rec string_of_exp = function
    | Var x -> string_of_id x
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
         (string_of_id x) (string_of_exp e1) (string_of_exp e2)
    | FunExp (x, t, e) ->
       sprintf "(fun (%s : %s) -> %s)"
         (string_of_id x) (string_of_ty t) (string_of_exp e)
    | AppExp (e1, e2) ->
       sprintf "(%s %s)" (string_of_exp e1) (string_of_exp e2)
    | CastExp (e, t1, t2) ->
       sprintf "(%s : %s => %s)"
         (string_of_exp e) (string_of_ty t1) (string_of_ty t2)
end
