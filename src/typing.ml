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

let matching_fun : ty -> (ty * ty) option = function
  | TyFun (t1, t2) -> Some (t1, t2)
  | TyDyn -> Some (TyDyn, TyDyn)
  | _ -> None                   (* means matching error *)

(* return more dynamic type of two *)
(* NOTE: Not used for now *)
let rec meet (t1 : ty) (t2 : ty) : ty = match (t1, t2) with
  | t1, t2 when t1 = t2 -> t1
  | TyDyn, t | t, TyDyn -> t
  | TyFun (t11, t12), TyFun (t21, t22) ->
     TyFun (meet t11 t21, meet t12 t22)
  (* raise error, or return optional value *)
  | _, _ -> tyerr "meet is undefined"

(* type of binOp *)
let ty_binop : binOp -> ty * ty * ty = function
  | (Plus | Minus | Mult | Div) -> (TyInt, TyInt, TyInt)
  | (Lt | Gt | Eq | LE | GE) -> (TyInt, TyInt, TyBool)
  | (LAnd | LOr) -> (TyBool, TyBool, TyBool)


(* TODO: Rename to G.ty_exp & Add C.ty_exp *)
module G = struct
  open Syntax.G
  let rec ty_exp : tyenv -> exp -> ty = fun gamma ->
    function
    | Var x ->
       (try
          let t = Environment.find x gamma in t
        with
        | Not_found -> tyerr (sprintf "GT-Var: %s is not bound" x))
    | ILit _ -> TyInt
    | BLit _ -> TyBool
    | BinOp (op, e1, e2) ->
       let t1 = ty_exp gamma e1 in
       let t2 = ty_exp gamma e2 in
       let (u1, u2, u3) = ty_binop op in
       if are_consistent t1 u1 then
         if are_consistent t2 u2 then u3
         else tyerr (sprintf "GT-BinOp-R: %s and %s are not consistent"
                       (string_of_ty t2) (string_of_ty u2))
       else tyerr (sprintf "GT-BinOp-L: %s and %s are not consistent"
                     (string_of_ty t1) (string_of_ty u1))
    | IfExp (e1, e2, e3) ->
       let t1 = ty_exp gamma e1 in
       if are_consistent t1 TyBool then
         let t2 = ty_exp gamma e2 in
         let t3 = ty_exp gamma e3 in
         (* OR: meet t2 t3 *)
         if t2 = t3 then t2
         else tyerr (sprintf "GT-If: branches have different types: %s and %s"
                       (string_of_ty t2) (string_of_ty t3))
       else tyerr (sprintf "GT-If-test: %s is not consistent with bool"
                     (string_of_ty t1))

    | LetExp (bindings, e2) ->
       let ty_bindings =
         List.map (fun (x,e1) -> (x, ty_exp gamma e1)) bindings in
       let gamma' = Environment.add_all ty_bindings gamma in
       ty_exp gamma' e2

    | FunExp (x, t, e) ->
       let u = ty_exp (Environment.add x t gamma) e in
       TyFun (t,u)
    | AppExp (e1, e2) ->
       let t1 = ty_exp gamma e1 in
       (match matching_fun t1 with
        | Some (t11, t12) ->
           let t2 = ty_exp gamma e2 in
           if are_consistent t2 t11 then t12
           else tyerr (sprintf "GT-App: %s and %s are not consistent"
                         (string_of_ty t2) (string_of_ty t11))
        | None -> tyerr (sprintf "GT-App: %s is not a function type"
                           (string_of_ty t1)))

    | LetRecExp (bindings, e2) ->
       let gamma1, _ = ty_rec_bindings gamma bindings in
       ty_exp gamma1 e2

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
        let retty' = ty_exp gamma2 e1 in
        (* consistency rather than equality?? *)
        if retty' = retty then ()
        else tyerr (sprintf ("GT-LetRec: return type %s does not equal"
                             ^^ " to the given annotation %s")
                      (string_of_ty retty') (string_of_ty retty)))
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
