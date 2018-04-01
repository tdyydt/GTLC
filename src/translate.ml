open Syntax

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
let rec translate_exp gamma =
  | G.Var x ->
     (try
        let t = Environment.find x gamma in (C.Var x, t)
      with
      (* G.typing をしてからキャスト挿入するで，
       * キャスト挿入でエラーが起きることは有り得ないべき *)
      | Not_found -> err "Not bound")
  | G.ILit n -> C.ILit n
  | G.BLit b -> C.BLit b
  | G.BinOp (op, e1, e2) ->
     let f1, t1 = translate_exp gamma e1 in
     let f2, t2 = translate_exp gamma e2 in
     let (u1, u2, u3) = ty_binop op in (* depends on op *)
     if are_consistent t1 u1 then
       if are_consistent t2 u2 then
         (C.BinOp (op, cast_opt f1 t1 u1, cast_opt f2 t2 u2), u3)
       else err "Should not happen"
     else err err "Should not happen"
  | G.IfExp (e1, e2, e3) -> todo "translate IfExp"
  | G.LetExp (x, e1, e2) -> todo "translate LetExp"
  | G.FunExp (x, e1, e2) ->
