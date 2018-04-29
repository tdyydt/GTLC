open Util
open Printf

type id = string
let string_of_id x = x          (* Should be used *)

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

(* TODO: Add Eq *)
type binOp = Plus | Minus | Mult | Div | Lt | Gt (* | LAnd | LOr *)

let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Gt -> ">"


(* Gradually typed surface language *)
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

    (* TODO: Add decl *)
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

  (* TODO: reduce parentheses, printer *)
  let rec string_of_exp = function
    | Var x -> string_of_id x
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
         (string_of_id x) (string_of_exp f1) (string_of_exp f2)
    | FunExp (x, t, f) ->
       sprintf "(fun (%s : %s) -> %s)"
         (string_of_id x) (string_of_ty t) (string_of_exp f)
    | AppExp (f1, f2) ->
       sprintf "(%s %s)" (string_of_exp f1) (string_of_exp f2)
    | CastExp (f, t1, t2) ->
       sprintf "(%s : %s => %s)"
         (string_of_exp f) (string_of_ty t1) (string_of_ty t2)
end
