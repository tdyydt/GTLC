open Syntax
open Util
open Typing

(* cast-insertion translation *)

(* insert cast if needed *)
(* C.exp -> ty -> ty -> C.exp *)
let cast_opt f t1 t2 =
  if t1 = t2 then f
  else C.CastExp (f,t1,t2)

(* cast-insertion should not result in error *)
(* G.ty_exp を通過しているため，キャスト挿入でエラーが起きることは
 * ないはず．もしあれば，実装のバグ *)

(* [G |- e ~> f : T]
 * input: G, e & output: f, T *)
(* tyenv -> G.exp -> C.exp * ty *)
let rec translate_exp gamma = function
  | G.Var x ->
     (try
        let t = Environment.find x gamma in (C.Var x, t)
      with
      | Not_found -> err "CI-Var: Not bound")
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
     (match matching_fun t1 with
      | Some (t11, t12) ->
         let f2, t2 = translate_exp gamma e2 in
         if are_consistent t2 t11
         then (C.AppExp (cast_opt f1 t1 (TyFun (t11, t12)),
                         cast_opt f2 t2 t11),
               t12)
         else err "CI-App: Should not happen"
      | None -> err "CI-App: Not a function")
  (* | G.LetRecExp (x, y, t1, t2, e1, e2) ->
   *    let gamma1 = Environment.add x (TyFun (t1, t2)) gamma in
   *    let gamma2 = Environment.add y t1 gamma1 in
   *    let f1, t2' = translate_exp gamma2 e1 in
   *    if t2' = t2 then
   *      let f2, t = translate_exp gamma1 e2 in
   *      (C.LetRecExp (x, y, t1, t2, f1, f2), t)
   *    else err "CI-LetRec: Should not happen" *)

  (* | G.LetRecExp (x, y, t11, t12, e1, e2) ->
   *    let gamma1 = Environment.add x (TyFun (t11, t12)) gamma in
   *    let gamma2 = Environment.add y t11 gamma1 in
   *    let f1, t1 = translate_exp gamma2 e1 in
   *    if t1 = t12 then
   *      let f2, t2 = translate_exp gamma1 e2 in
   *      (C.LetRecExp (x, y, t11, t12, f1, f2), t2)
   *    else err "CI-LetRec: Should not happen" *)

  (* paraty = parameter type, retty = return type *)
  | G.LetRecExp (x, y, paraty, retty, e1, e2) ->
     let gamma1 = Environment.add x (TyFun (paraty, retty)) gamma in
     let gamma2 = Environment.add y paraty gamma1 in
     let f1, retty' = translate_exp gamma2 e1 in
     if retty' = retty then
       let f2, t2 = translate_exp gamma1 e2 in
       (C.LetRecExp (x, y, paraty, retty, f1, f2), t2)
     else err "CI-LetRec: Should not happen"

(* 型を返す必要性が，あまり感じられない
 * ただ，宣言の型とは，宣言した変数に付いた型だと思える *)
(* tyenv -> G.program -> C.program * ty *)
let translate_prog gamma = function
  | G.Exp e ->
     let f, t = translate_exp gamma e in (C.Exp f, t)
  | G.LetDecl (x, e) ->
     let f, t = translate_exp gamma e in (C.LetDecl (x, f), t)
  (* | G.LetRecDecl (x, y, t1, t2, e) ->
   *    let f, t = translate_exp gamma e in
   *    (C.LetRecDecl (x, y, t1, t2, f), t) *)

  | G.LetRecDecl (x, y, paraty, retty, e) ->
     (* let tyfun = TyFun (paraty, retty) in *)
     let gamma1 = Environment.add x (TyFun (paraty, retty)) gamma in
     let gamma2 = Environment.add y paraty gamma1 in
     let f, retty' = translate_exp gamma2 e in
     if retty' = retty then
       (C.LetRecDecl (x, y, paraty, retty, f), TyFun (paraty, retty))
     else err "CI-LetRecDecl: Should not happen"
