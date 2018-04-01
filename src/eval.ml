open Syntax
open Syntax.C
open Util
open Printf

(* evaluation *)
(* TODO: small-step operational semantics
 * その場合，関数 step など，sf にある *)

(* subset of C.exp *)
type value =
  | IntV of int
  | BoolV of bool
  (* 関数閉包・環境を持つ？？ *)
  (* function name, body, and environment (= for free variables) *)
  | FunV of id * exp * value Environment.t
  (* tagged value, or injection
   * cast from Ground to TyDyn *)
  | Tagged

(* Big-step evaluation ? *)
let rec eval_exp env = function
  | Var x ->
     (try
        let t = Environment.find x env in t
      with
      | Not_found -> err @@ sprintf "%s is not bound" (string_of_id x))
  | ILit n -> IntV n
  | BLit b -> BoolV b
  | BinOp (op, f1, f2) ->
     let v1 = eval_exp env f1 in
     let v2 = eval_exp env f2 in
     (* TODO: apply_prim op v1 v2 *)
     (match op, v1, v2 with
      | Plus, IntV n1, IntV n2 -> IntV (n1 + n2)
      | Minus, IntV n1, IntV n2 -> IntV (n1 - n2)
      | Mult, IntV n1, IntV n2 -> IntV (n1 * n2)
      | Div, IntV n1, IntV n2 -> IntV (n1 / n2)
      | Lt, IntV n1, IntV n2 -> BoolV (n1 < n2)
      | Gt, IntV n1, IntV n2 -> BoolV (n1 > n2)
      (* op when op is arithmetic *)
      (* | (Plus | Minus | Mult | Div | Lt | Gt), _, _ ->  *)
      | op, _, _ -> err "eval BinOp")
  | IfExp (f1, f2, f3) ->
     let v1 = eval_exp env f1 in
     (match v1 with
      | BoolV true -> eval_exp env f2
      | BoolV false -> eval_exp env f3
      | _ -> err "eval If: Test expression must be boolean")
  | LetExp (x, f1, f2) ->
     let v1 = eval_exp env f1 in
     eval_exp (Environment.add x v1 env) f2
  | FunExp (x, _, f) -> FunV (x, f, env)
  | AppExp (f1, f2) ->
     let v1 = eval_exp env f1 in
     let v2 = eval_exp env f2 in
     (match v1 with
      | FunV (x, body, fun_env) ->
         (* [(fun (x:_) -> body) v2] *)
         eval_exp (Environment.add x v2 fun_env) body
      | _ -> err "eval App: Non-function value is applied")
  | CastExp (f, t1, t2) -> todo "Cast"
