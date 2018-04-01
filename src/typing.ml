open Syntax
open Syntax.G
open Util
open Printf

(* is_consistent としたいところだが，
 * are_consistent の方が正しくないか？？ *)
(* ty -> ty -> bool *)
let rec are_consistent t1 t2 = match (t1,t2) with
  | (TyDyn, _) -> true
  | (_, TyDyn) -> true
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyFun (t11,t12), TyFun (t21,t22)) ->
     are_consistent t11 t21 && are_consistent t12 t22
  | (_, _) -> false

(* option で返しても良い？ *)
let matching_fun = function
  | TyFun (t1, t2) -> (t1, t2)
  | TyDyn -> (TyDyn, TyDyn)
  | _ -> err "matching error"

(* join of typs w.r.t consistency *)
(* もし，join が存在しない場合はエラー？ *)
let rec join t1 t2 = match (t1, t2) with
  | _ -> todo "Join"

(* type of binOp *)
(* binOp -> ty *)
let ty_binop = function
  | (Plus | Minus | Mult | Div) -> (TyInt, TyInt, TyInt)
  | (Lt | Gt) -> (TyInt, TyInt, TyBool)


(* TODO: ty_exp を module C = の中に入れる *)
(* exp -> ty Environment.t -> ty *)
let rec ty_exp gamma = function
  | Var x ->
     (try
        let t = Environment.find x gamma in t
      with
      | Not_found -> err @@ sprintf "%s is not bound" (string_of_id x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, e1, e2) ->
     let t1 = ty_exp gamma e1 in
     let t2 = ty_exp gamma e2 in
     let (u1, u2, u3) = ty_binop op in
     if are_consistent t1 u1 then
       if are_consistent t2 u2 then u3
       else err @@ sprintf "GT-BinOp-R: %s and %s are not consistent"
                     (string_of_ty t2) (string_of_ty u2)
     else err @@ sprintf "GT-BinOp-L: %s and %s are not consistent"
                   (string_of_ty t1) (string_of_ty u1)
  | IfExp (e1, e2, e3) ->
     let t1 = ty_exp gamma e1 in
     if are_consistent t1 TyBool then
       let t2 = ty_exp gamma e2 in
       let t3 = ty_exp gamma e3 in
       join t2 t3
     else err @@ sprintf "GT-If-test: %s is not consistent with bool"
                   (string_of_ty t1)

  | LetExp (x, e1, e2) ->
     let t1 = ty_exp gamma e1 in
     ty_exp (Environment.add x t1 gamma) e2
  | FunExp (x, t, e) ->
     let u = ty_exp (Environment.add x t gamma) e in
     TyFun (t,u)
  | AppExp (e1, e2) ->
     let t1 = ty_exp gamma e1 in
     let t11,t12 = matching_fun t1 in
     let t2 = ty_exp gamma e2 in
     if are_consistent t2 t11 then t12
     else err @@ sprintf "GT-App: %s and %s are not consistent"
                   (string_of_ty t2) (string_of_ty t11)
