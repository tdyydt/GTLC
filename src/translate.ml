open Syntax
open Util
open Typing

(* cast-insertion translation *)
(* cast-insertion should not result in error *)
exception CI_error of string
let err s = raise (CI_error s)  (* implementation bug *)

(* insert cast if needed *)
(* C.exp -> ty -> ty -> C.exp *)
let cast_opt f t1 t2 =
  if t1 = t2 then f
  else C.CastExp (f,t1,t2)

(* cast insertion [G |- e ~> f : T] *)
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
       (* OR:
        * let u = meet t2 t3 in
        * (C.IfExp (cast_opt f1 t1 TyBool,
        *           cast_opt f2 t2 u,
        *           cast_opt f3 t3 u), u) *)
       if t2 = t3 then
         (C.IfExp (cast_opt f1 t1 TyBool, f2, f3), t2)
       else err "CI-If-branches: Should not happen"
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

  (* paraty = parameter type, retty = return type *)
  | G.FixExp (x, y, paraty, retty, e1) ->
     let gamma1 = Environment.add x (TyFun (paraty, retty)) gamma in
     let gamma2 = Environment.add y paraty gamma1 in
     let f1, retty' = translate_exp gamma2 e1 in
     if retty' = retty then
       (C.FixExp (x, y, paraty, retty, f1), TyFun (paraty, retty))
     else err "CI-LetRec: Should not happen"

(* TODO: remove ty from retval? *)
(* tyenv -> G.program -> C.program * ty *)
let translate_prog gamma = function
  | G.Exp e ->
     let f, t = translate_exp gamma e in (C.Exp f, t)
  | G.LetDecl (x, e) ->
     let f, t = translate_exp gamma e in (C.LetDecl (x, f), t)
