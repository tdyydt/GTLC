open Syntax
open Syntax.G
open Util
open Printf

(* ty -> ty -> bool *)
let rec are_consistent t1 t2 = match (t1,t2) with
  | (TyDyn, _) -> true
  | (_, TyDyn) -> true
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyFun (t11,t12), TyFun (t21,t22)) ->
     are_consistent t11 t21 && are_consistent t12 t22
  | (_, _) -> false

(* ty -> (ty * ty) option *)
let matching_fun = function
  | TyFun (t1, t2) -> Some (t1, t2)
  | TyDyn -> Some (TyDyn, TyDyn)
  | _ -> None                   (* means matching error *)

(* meet of types w.r.t. precision ?? *)
(* int, bool の precision に関する meet は DYN となるが，
 * これは型エラーとしている等，
 * precision に関する meet とは言えない *)
(* return more dynamic type of two *)
let rec meet t1 t2 = match (t1, t2) with
  | t1, t2 when t1 = t2 -> t1
  | TyDyn, t | t, TyDyn -> t
  | TyFun (t11, t12), TyFun (t21, t22) ->
     TyFun (meet t11 t21, meet t12 t22)
  (* raise error, or return optional value *)
  | _, _ -> err "meet is undefined"

(* type of binOp *)
(* binOp -> ty * ty * ty *)
let ty_binop = function
  | (Plus | Minus | Mult | Div) -> (TyInt, TyInt, TyInt)
  | (Lt | Gt | Eq | LE | GE) -> (TyInt, TyInt, TyBool)
  | (LAnd | LOr) -> (TyBool, TyBool, TyBool)


(* TODO: ty_exp を module G = の中に入れた上で，
 * C.ty_exp を作る *)
(* ty Environment.t -> exp -> ty *)
let rec ty_exp gamma = function
  | Var x ->
     (try
        let t = Environment.find x gamma in t
      with
      | Not_found -> err (sprintf "GT-Var: %s is not bound" x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, e1, e2) ->
     let t1 = ty_exp gamma e1 in
     let t2 = ty_exp gamma e2 in
     let (u1, u2, u3) = ty_binop op in
     if are_consistent t1 u1 then
       if are_consistent t2 u2 then u3
       else err (sprintf "GT-BinOp-R: %s and %s are not consistent"
                   (string_of_ty t2) (string_of_ty u2))
     else err (sprintf "GT-BinOp-L: %s and %s are not consistent"
                 (string_of_ty t1) (string_of_ty u1))
  | IfExp (e1, e2, e3) ->
     let t1 = ty_exp gamma e1 in
     if are_consistent t1 TyBool then
       let t2 = ty_exp gamma e2 in
       let t3 = ty_exp gamma e3 in
       (* OR: meet t2 t3 *)
       if t2 = t3 then t2
       else err (sprintf "GT-If: branches have different types: %s and %s"
                   (string_of_ty t2) (string_of_ty t3))
     else err (sprintf "GT-If-test: %s is not consistent with bool"
                 (string_of_ty t1))

  | LetExp (x, e1, e2) ->
     let t1 = ty_exp gamma e1 in
     ty_exp (Environment.add x t1 gamma) e2
  | FunExp (x, t, e) ->
     let u = ty_exp (Environment.add x t gamma) e in
     TyFun (t,u)
  | AppExp (e1, e2) ->
     let t1 = ty_exp gamma e1 in
     (match matching_fun t1 with
      | Some (t11, t12) ->
         let t2 = ty_exp gamma e2 in
         if are_consistent t2 t11 then t12
         else err @@ sprintf "GT-App: %s and %s are not consistent"
                       (string_of_ty t2) (string_of_ty t11)
      | None -> err @@ sprintf "GT-App: %s is not a function type"
                         (string_of_ty t1))

  | LetRecExp (x, y, t1, t2, e1, e2) ->
    let gamma1 = Environment.add x (TyFun (t1, t2)) gamma in
    let gamma2 = Environment.add y t1 gamma1 in
    let t2' = ty_exp gamma2 e1 in
    (* consistent か，equality か？ *)
    if t2' = t2 then ty_exp gamma1 e2
    (* return type does not match with the given annotation *)
    else err (sprintf ("GT-LetRec: return type %s does not equal "
                       ^^ "to the given annotation %s")
                (string_of_ty t2') (string_of_ty t2))

(* tyenv -> program -> tyenv * id * ty *)
let ty_prog gamma = function
  | Exp e ->
     let t = ty_exp gamma e in (gamma, "-", t)
  | LetDecl (x, e) ->
     (* exp に帰着させている *)
     (* let x = e in x *)
     (* let t = ty_exp gamma (LetExp (x, e, Var x)) in
      * (Environment.add x t gamma, x, t) *)

     let t = ty_exp gamma e in
     (Environment.add x t gamma, x, t)
  | LetRecDecl (x, y, t1, t2, e) ->
     (* let rec x (y:t1) : t2 = e in x *)
     let t = ty_exp gamma (LetRecExp (x, y, t1, t2, e, Var x)) in
     (Environment.add x t gamma, x, t)

     (* let gamma1 = Environment.add x (TyFun (t1, t2)) gamma in
      * let gamma2 = Environment.add y t2 gamma1 in
      * let t2' = ty_exp gamma2 e in
      * if t2' = t2 then (gamma1, x, (TyFun (t1, t2)))
      * else err "GT-LetRecDecl" *)
