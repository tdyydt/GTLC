open Syntax
open Util
(* Partial import *)
let ty_binop = Typing.ty_binop
let are_consistent = Typing.are_consistent
let matching_fun = Typing.matching_fun
let join = Typing.join

(* cast-insertion translation *)

(* insert cast if needed *)
(* name candicates: cast dummy, cast if needed *)
(* input: f,t1,t2 & output [f: t1 => t2] or f *)
(* C.exp -> ty -> ty -> C.exp *)
let cast_opt f t1 t2 =
  if t1 = t2 then f
  else C.CastExp (f,t1,t2)

(* [G |- e ~> f : T]
 * input: G, e & output: f, T *)
(* G.exp -> C.exp *)
let rec translate_exp gamma = function
  | G.Var x ->
     (try
        let t = Environment.find x gamma in (C.Var x, t)
      with
      (* G.typing をしてからキャスト挿入するで，
       * キャスト挿入でエラーが起きることは有り得ないべき
       * Implementation error *)
      | Not_found -> err "Not bound")
  | G.ILit n -> (C.ILit n, TyInt)
  | G.BLit b -> (C.BLit b, TyBool)
  | G.BinOp (op, e1, e2) ->
     let f1, t1 = translate_exp gamma e1 in
     let f2, t2 = translate_exp gamma e2 in
     let u1, u2, u3 = ty_binop op in (* depends on op *)
     if are_consistent t1 u1 then
       if are_consistent t2 u2 then
         (C.BinOp (op, cast_opt f1 t1 u1, cast_opt f2 t2 u2), u3)
       else err "CI-BinOp: Should not happen"
     else err "CI-BinOp: Should not happen"
  | G.IfExp (e1, e2, e3) ->
     let f1, t1 = translate_exp gamma e1 in
     if are_consistent t1 TyBool then
       let f2, t2 = translate_exp gamma e2 in
       let f3, t3 = translate_exp gamma e3 in
       let u = join t2 t3 in
       (C.IfExp (cast_opt f1 t1 TyBool,
                 cast_opt f2 t2 u,
                 cast_opt f3 t3 u),
        u)
     else err "CI-If-test: Should not happen"
  | G.LetExp (x, e1, e2) ->
     let f1, t1 = translate_exp gamma e1 in
     let f2, t2 = translate_exp (Environment.add x t1 gamma) e2 in
     (C.LetExp (x, f1, f2), t2)
  | G.FunExp (x, t, e) ->
     let f, u = translate_exp (Environment.add x t gamma) e in
     (C.FunExp (x, t, f), TyFun (t, u))
  | G.AppExp (e1, e2) ->        (* interesting case *)
     let f1, t1 = translate_exp gamma e1 in
     let f2, t2 = translate_exp gamma e2 in
     let t11, t12 = matching_fun t1 in
     if are_consistent t2 t11
     then (C.AppExp (cast_opt f1 t1 (TyFun (t11, t12)),
                     cast_opt f2 t2 t11),
           t12)
     else err "CI-App: Should not happen"
