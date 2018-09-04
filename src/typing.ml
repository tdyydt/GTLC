open Syntax
open Stringify
open Printf

exception Typing_error of string
let tyerr s = raise (Typing_error s)

type tyenv = ty Environment.t

let rec are_consistent (t1 : ty) (t2 : ty) : bool =
  match (t1,t2) with
  | (TyDyn, _) -> true
  | (_, TyDyn) -> true
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyFun (t11,t12), TyFun (t21,t22)) ->
     are_consistent t11 t21 && are_consistent t12 t22
  | (_, _) -> false

let matching_fun : ty -> ty * ty = function
  | TyFun (t1, t2) -> (t1, t2)
  | TyDyn -> (TyDyn, TyDyn)
  | t -> tyerr (sprintf "%s doesn't match with a function type"
                  (string_of_ty t))

(* meet wrt. precision relation: [T < ?] *)
(* computes greatest common _static_ type of two *)
let rec meet (t1 : ty) (t2 : ty) : ty = match (t1, t2) with
  | t1, t2 when t1 = t2 -> t1
  | TyDyn, t -> t
  | t, TyDyn -> t
  (* covariant in argument type *)
  | TyFun (t11, t12), TyFun (t21, t22) ->
     TyFun (meet t11 t21, meet t12 t22)
  | _ -> tyerr (sprintf "Meet is undefined: %s and %s"
                  (string_of_ty t1) (string_of_ty t2))

(* type of binOp *)
let ty_binop : binOp -> ty * ty * ty = function
  | (Plus | Minus | Mult | Div) -> (TyInt, TyInt, TyInt)
  | (Lt | Gt | Eq | LE | GE) -> (TyInt, TyInt, TyBool)
  | (LAnd | LOr) -> (TyBool, TyBool, TyBool)

(* check if t1 is consistent with given type annotation t2 (if any) *)
let check_consistent (t1 : ty) (tyopt : ty option) : ty =
  match tyopt with
  | Some t2 ->
     (* t2 is type annotation given by programmer *)
     if are_consistent t2 t1 then t2
     else tyerr (sprintf "%s and %s are not consistent"
                   (string_of_ty t2) (string_of_ty t1))
  | None -> t1

module G = struct
  open Syntax.G
  (* NOTE: tyopt means the type expression that e is expected to have.
   * It is currently used in LetRec. tyopt is None if such type
   * is not necessary. *)
  let rec ty_exp (gamma : tyenv) ?(tyopt : ty option = None) (e : exp) : ty =
    match e with
    | Var (_,x) ->
       (try
          let t = Environment.find x gamma in
          check_consistent t tyopt
        with
        | Not_found -> tyerr (sprintf "GT-Var: %s is not bound" x))
    | ILit _ -> check_consistent TyInt tyopt
    | BLit _ -> check_consistent TyBool tyopt
    | BinOp (_, op, e1, e2) ->
       let t1 = ty_exp gamma e1 in
       let t2 = ty_exp gamma e2 in
       let (u1, u2, u3) = ty_binop op in
       if are_consistent t1 u1 then
         if are_consistent t2 u2 then check_consistent u3 tyopt
         else tyerr (sprintf "GT-BinOp-R: %s and %s are not consistent"
                       (string_of_ty t2) (string_of_ty u2))
       else tyerr (sprintf "GT-BinOp-L: %s and %s are not consistent"
                     (string_of_ty t1) (string_of_ty u1))
    | IfExp (_, e1, e2, e3) ->
       let t1 = ty_exp gamma e1 in
       if are_consistent t1 TyBool then
         let t2 = ty_exp gamma e2 in
         let t3 = ty_exp gamma e3 in
         (match tyopt with
          | Some t ->           (* t is respected *)
             if are_consistent t2 t then
               if are_consistent t3 t then t
               else tyerr (sprintf "GT-If-Else: %s and %s are not consistent"
                             (string_of_ty t3) (string_of_ty t))
             else tyerr (sprintf "GT-If-Then: %s and %s are not consistent"
                           (string_of_ty t2) (string_of_ty t))
          | None ->
             (try meet t2 t3
              with
              | Typing_error msg -> tyerr (sprintf "G-If-Br: %s" msg)))
       else tyerr (sprintf "GT-If-Test: %s is not consistent with bool"
                     (string_of_ty t1))

    | LetExp (_, bindings, e2) ->
       let ty_bindings =
         List.map (fun (x,e1) -> (x, ty_exp gamma e1)) bindings in
       let gamma' = Environment.add_all ty_bindings gamma in
       let t2 = ty_exp gamma' e2 in check_consistent t2 tyopt

    | FunExp (_, x, t, e) ->
       let u = ty_exp (Environment.add x t gamma) e in
       check_consistent (TyFun (t,u)) tyopt
    | AppExp (_, e1, e2) ->
       let t1 = ty_exp gamma e1 in
       (try
          let (t11, t12) = matching_fun t1 in
          let t2 = ty_exp gamma e2 in
          if are_consistent t2 t11 then check_consistent t12 tyopt
          else tyerr (sprintf "GT-App: %s and %s are not consistent"
                        (string_of_ty t2) (string_of_ty t11))
        with
        | Typing_error msg -> tyerr ("GT-App: " ^ msg))

    | LetRecExp (_, bindings, e2) ->
       let gamma1, _ = ty_rec_bindings gamma bindings in
       let t2 = ty_exp gamma1 e2 in check_consistent t2 tyopt

  (* auxiliary function for typing LetRec bindings *)
  and ty_rec_bindings : tyenv -> (id * id * ty * ty * exp) list -> tyenv * (id * ty) list =
    fun gamma bindings ->
    let ty_bindings =
      List.map (fun (x,_,paraty,retty,_) -> (x, TyFun (paraty,retty)))
        bindings in
    (* n個の関数を束縛 *)
    let gamma1 = Environment.add_all ty_bindings gamma in
    (* まず，n個のe1を型付け・型注釈が正しいことを確認する *)
    List.iter (fun (x,y,paraty,retty,e1) ->
        let gamma2 = Environment.add y paraty gamma1 in
        (* retty is given by a programmer *)
        let retty' = ty_exp gamma2 e1 ~tyopt:(Some retty) in
        assert (retty = retty'))
      bindings;
    (gamma1, ty_bindings)

  let ty_prog : tyenv -> program -> tyenv * (id * ty) list =
    fun gamma ->
    function
    | Exp e ->
       let t = ty_exp gamma e in (gamma, [("-", t)])
    | LetDecl bindings ->
       let ty_bindings =
         List.map (fun (x,e1) -> (x, ty_exp gamma e1)) bindings in
       let gamma' = Environment.add_all ty_bindings gamma in
       (gamma', ty_bindings)
    | LetRecDecl bindings -> ty_rec_bindings gamma bindings
end
