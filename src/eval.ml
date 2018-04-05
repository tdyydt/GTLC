open Syntax
open Syntax.C
open Util
open Printf

(* evaluation *)
(* TODO: small-step operational semantics
 * その場合，関数 step など，sf にある *)

(* TagInt, TagBool, TagFun でいいのでは？ *)
type tag =
  | I                           (* int *)
  | B                           (* bool *)
  | F                           (* Function (Arrow) [?->?] *)

(* exval, dnval *)
type value =
  | IntV of int
  | BoolV of bool
  (* 関数閉包・環境を持つ？？自由変数の情報が必要なため *)
  (* function name, body, and environment (= for free variables) *)
  | FunV of id * exp * value Environment.t
  (* Wrapped function:
   * [v: (t1 -> t2) => (t3 -> t4)] => Wrapped (v,t1,t2,t3,t4) *)
  (* TODO: 他の実装でなぜ無いのか，理解できない *)
  | Wrapped of value * ty * ty * ty * ty
  (* Tagged value, or Injection:
   * [v: G => ?] => Tagged (tag, v)
   * where tag corresponds to G(round) *)
  | Tagged of tag * value

(* convert value to C.exp *)
let rec exp_of_value = function
  | IntV n -> ILit n
  | BoolV b -> BLit b
  | FunV (id, f, _) ->
     let dummy_ty = TyDyn in    (* Fun の型の情報は，eval には不要なので，
                                 * value の定義では消している．しかし，
                                 * exp にするには，必要なため dummy の型を入れる必要がある
                                 * ? に意味はない *)
     FunExp (id, dummy_ty, f)
  | Wrapped (v, t1, t2, t3, t4) ->
     CastExp (exp_of_value v, TyFun (t1, t2), TyFun (t3, t4))
  | Tagged (tag, v) -> let f = exp_of_value v in
                       (match tag with
                        | I -> CastExp (f, TyInt, TyDyn)
                        | B -> CastExp (f, TyBool, TyDyn)
                        | F -> CastExp (f, TyFun (TyDyn, TyDyn), TyDyn))

(* C.string_of_exp に帰着 *)
let string_of_value v = string_of_exp (exp_of_value v)
(* let rec string_of_value = function
 *   | IntV n -> string_of_int n
 *   | BoolV b -> string_of_bool b
 *   | FunV _ -> todo "function"
 *   | Wrapped _ -> todo "wrapped"
 *   | Tagged (tag, v) -> todo "tagged" *)


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
      (* AppCast (Wrap) *)
      | Wrapped (v1', t1, t2, t3, t4) ->
         (* [(v1': t1 -> t2 => t3 -> t4) v2] -->
          * [v1' (v2: t3 => t1): t2 => t4] *)
         eval_exp env
           (CastExp (AppExp (exp_of_value v1',
                             CastExp (exp_of_value v2, t3, t1)),
                     t2, t4))
      | _ -> err "eval App: Non-function value is applied")
  | CastExp (f, t1, t2) ->
     let v = eval_exp env f in
     (match t1, t2 with
      (* IdBase *)
      | TyInt, TyInt -> v
      | TyBool, TyBool -> v
      (* IdStar *)
      | TyDyn, TyDyn -> v
      (* Put tag *)
      (* 動的型へのキャストの際に，タグを付ける(キャスト前の型の情報を表す) *)
      | TyInt, TyDyn -> Tagged (I, v)
      | TyBool, TyDyn -> Tagged (B, v)
      (* Ground = decompose cast *)
      | TyFun (t11, t12), TyDyn ->
         (* [v: (t11 -> t12) => (? -> ?) => ?] *)
         Tagged (F, Wrapped (v, t11, t12, TyDyn, TyDyn))

      (* Succeed (Collapse), Fail (Conflict) *)
      | TyDyn, TyInt ->
         (match v with
          (* [v': int => ? => int]
           * int のタグが付いていたならば ok *)
          | Tagged (I, v') -> v'
          | Tagged (_, _) -> err "Blame: Fail int"
          | _ -> err "Should not happen: Untagged value")
      | TyDyn, TyBool ->
         (match v with
          | Tagged (B, v') -> v'
          | Tagged (_, _) -> err "Blame: Fail bool"
          | _ -> err "Should not happen: Untagged value")
      | TyDyn, TyFun (TyDyn, TyDyn) ->
         (match v with
          | Tagged (F, v') -> v'
          | Tagged (_, _) -> err "Blame: Fail fun" (* v: ? => (? -> ?) の時に，
                                                    * v は必ず Tagged (F, _)だと言っている
                                                    * そうでないと blame だと言っている *)
          | _ -> err "Should not happen: Untagged value")

      (* Expand *)
      (* Expandのケースが謎
       * Tが関数型のものは，あるのか？？ *)
      | TyDyn, TyFun (t21, t22) ->
         (* t21, t22 はともに ? ではない *)
         (* [v: ? => (t21 -> t22)] =>
          * [v: ? => (? -> ?) => (t21 -> t22)]  *)
         todo "Can this happen??"

      | _, _ -> err "Should not happen or Not implemented"
     )
