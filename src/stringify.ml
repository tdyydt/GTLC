open Syntax
open Printf

let with_paren flag s =
  if flag then "(" ^ s ^ ")" else s

(* precedence of type *)
(* larger number means higher precedence *)
let prec_ty = function
  | (TyInt | TyBool | TyDyn) -> 2
  | TyFun _ -> 1

let ge_ty t1 t2 = (prec_ty t1) >= (prec_ty t2)

let rec string_of_ty = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) as t ->
     sprintf "%s -> %s"
       (with_paren (ge_ty t t1) (string_of_ty t1)) (string_of_ty t2)
  | TyDyn -> "?"

(* binOp *)
let string_of_binop = function
  | Plus  -> "+"
  | Minus -> "-"
  | Mult  -> "*"
  | Div   -> "/"
  | Lt    -> "<"
  | Gt    -> ">"
  | Eq    -> "="
  | LE    -> "<="
  | GE    -> ">="
  | LAnd  -> "&&"
  | LOr   -> "||"

(* 1~10 で返す *)
let prec_binop = function
  | LOr -> 1
  | LAnd -> 2
  | Lt | Gt | Eq | LE | GE -> 3
  | Plus | Minus -> 4
  | Mult | Div -> 5


module G = struct
  open Syntax.G
  (* precedence of expression *)
  let prec_exp = function
    | LetExp _ | LetRecExp _ | FunExp _ -> 10
    | IfExp _ -> 20
    | BinOp (_, op, _, _) -> 30 + prec_binop op
    | AppExp _ -> 40
    | Var _ | ILit _ | BLit _ -> 50

  (* e1 > e2 : e1 associates stronger than e2 *)
  let gt_exp e1 e2 = (prec_exp e1) > (prec_exp e2)
  (* e1 >= e2 *)
  let ge_exp e1 e2 = (prec_exp e1) >= (prec_exp e2)

  let rec string_of_exp = function
    | Var (_,x) -> x
    | ILit (_,n) -> string_of_int n
    | BLit (_,b) -> string_of_bool b
    | BinOp (_, op, e1, e2) as e ->
       (* Left assoc *)
       sprintf "%s %s %s"
         (with_paren (gt_exp e e1) (string_of_exp e1)) (string_of_binop op)
         (with_paren (ge_exp e e2) (string_of_exp e2))
    | IfExp (_, e1, e2, e3) as e ->
       (* e1,e2 は If 等ならカッコが要る *)
       (* e3 にカッコは不要 *)
       sprintf "if %s then %s else %s"
         (with_paren (ge_exp e e1) (string_of_exp e1))
         (with_paren (ge_exp e e2) (string_of_exp e2))
         (string_of_exp e3)

    | LetExp (_, bindings, e2) as e ->
       (* string representation of bindings *)
       let bindings_str =
         List.map (fun (x,e1) ->
             sprintf "%s = %s"
               x (with_paren (ge_exp e e1) (string_of_exp e1)))
           bindings in
       sprintf "let %s in %s"
         (String.concat " and " bindings_str)
         (string_of_exp e2)

    | LetRecExp (_, bindings, e2) as e ->
       let bindings_str =
         List.map (fun (x,y,t1,t2,e1) ->
             sprintf "%s (%s : %s) : %s = %s"
               x y (string_of_ty t1) (string_of_ty t2)
               (with_paren (ge_exp e e1) (string_of_exp e1)))
           bindings in
       sprintf "let rec %s in %s"
         (String.concat " and " bindings_str)
         (string_of_exp e2)

    | FunExp (_, x, t, e1) ->
       sprintf "fun (%s : %s) -> %s"
         x (string_of_ty t) (string_of_exp e1)
    | AppExp (_, e1, e2) as e ->
       (* Left assoc *)
       sprintf "%s %s"
         (with_paren (gt_exp e e1) (string_of_exp e1))
         (with_paren (ge_exp e e2) (string_of_exp e2))

  let rec string_of_program = function
    | Exp e -> string_of_exp e
    | LetDecl bindings ->
       let bindings_str =
         List.map (fun (x,e1) ->
             sprintf "%s = %s" x (string_of_exp e1))
           bindings in
       sprintf "let %s" (String.concat " and " bindings_str)
    | LetRecDecl bindings ->
       let bindings_str =
         List.map (fun (x,y,t1,t2,e1) ->
             sprintf "%s (%s : %s) : %s = %s"
               x y (string_of_ty t1) (string_of_ty t2) (string_of_exp e1))
           bindings in
       sprintf "let rec %s" (String.concat " and " bindings_str)
end

module C = struct
  open Syntax.C
  (* precedence of expression *)
  let prec_exp = function
    | LetExp _ | LetRecExp _ | FunExp _ -> 10
    | IfExp _ -> 20
    | CastExp _ -> 21           (* ?????? *)
    | BinOp (_, op, _, _) -> 30 + prec_binop op
    | AppExp _ -> 40
    | Var _ | ILit _ | BLit _ -> 50

  (* f1 > f2 : f1 associates stronger than f2 *)
  let gt_exp f1 f2 = (prec_exp f1) > (prec_exp f2)
  (* f1 >= f2 *)
  let ge_exp f1 f2 = (prec_exp f1) >= (prec_exp f2)

  let rec string_of_exp = function
    | Var (_,x) -> x
    | ILit (_,n) -> string_of_int n
    | BLit (_,b) -> string_of_bool b
    | BinOp (_, op, f1, f2) as f ->
       sprintf "%s %s %s"
         (with_paren (gt_exp f f1) (string_of_exp f1)) (string_of_binop op)
         (with_paren (ge_exp f f2) (string_of_exp f2))
    | IfExp (_, f1, f2, f3) as f ->
       sprintf "if %s then %s else %s"
         (with_paren (ge_exp f f1) (string_of_exp f1))
         (with_paren (ge_exp f f2) (string_of_exp f2))
         (string_of_exp f3)
    | LetExp (_, bindings, f2) as f ->
       let bindings_str =
         List.map (fun (x,f1) ->
             sprintf "%s = %s"
               x (with_paren (ge_exp f f1) (string_of_exp f1)))
           bindings in
       sprintf "let %s in %s"
         (String.concat " and " bindings_str)
         (string_of_exp f2)
    | LetRecExp (_, bindings, f2) as f ->
       let bindings_str =
         List.map (fun (x,y,t1,t2,f1) ->
             sprintf "%s (%s : %s) : %s = %s"
               x y (string_of_ty t1) (string_of_ty t2)
               (with_paren (ge_exp f f1) (string_of_exp f1)))
           bindings in
       sprintf "let rec %s in %s"
         (String.concat " and " bindings_str)
         (string_of_exp f2)
    | FunExp (_, x, t, f1) ->
       sprintf "fun (%s : %s) -> %s"
         x (string_of_ty t) (string_of_exp f1)
    | AppExp (_, f1, f2) as f ->
       sprintf "%s %s"
         (with_paren (gt_exp f f1) (string_of_exp f1))
         (with_paren (ge_exp f f2) (string_of_exp f2))
    | CastExp (_, f1, t1, t2) as f ->
       sprintf "%s : %s => %s"
         (with_paren (ge_exp f f1) (string_of_exp f1))
         (string_of_ty t1) (string_of_ty t2)

  let rec string_of_program = function
    | Exp f -> string_of_exp f
    | LetDecl bindings ->
       let bindings_str =
         List.map (fun (x,f1) ->
             sprintf "%s = %s" x (string_of_exp f1))
           bindings in
       sprintf "let %s" (String.concat " and " bindings_str)
    | LetRecDecl bindings ->
       let bindings_str =
         List.map (fun (x,y,t1,t2,f1) ->
             sprintf "%s (%s : %s) : %s = %s"
               x y (string_of_ty t1) (string_of_ty t2) (string_of_exp f1))
           bindings in
       sprintf "let rec %s" (String.concat " and " bindings_str)
end
