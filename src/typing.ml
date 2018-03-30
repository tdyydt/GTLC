open Syntax

(* Error *)
exception Error of string
let err s = raise (Error s)
let todo () = err "Not implemented yet." (* TODO: remove *)

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

let matching_fun = function
  | TyFun (t1, t2) -> (t1, t2)
  | TyDyn -> (TyDyn, TyDyn)
  | _ -> todo () (* matching_error *)

(* exp -> ty Environment.t -> ty *)
let rec ty_exp gamma = function
  | Var x ->
     (try
        let t = Environment.find x gamma in t
      with
      | Not_found -> err "Not bound")
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, e1, e2) ->
     todo ()                    (* ty_prim *)
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
     else err "err"                (* typing_error: not consistent *)
