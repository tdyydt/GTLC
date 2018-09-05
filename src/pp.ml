open Format
open Syntax

let with_paren flag ppf_e ppf e =
  fprintf ppf (if flag then "(%a)" else "%a") ppf_e e

(* precedence of type *)
(* larger number means higher precedence *)
let prec_ty = function
  | (TyInt | TyBool | TyDyn) -> 2
  | TyFun _ -> 1

let ge_ty t1 t2 = (prec_ty t1) >= (prec_ty t2)

let rec pp_ty ppf t =
  (* put paren around t1 if needed *)
  let with_paren_L ppf t1 = with_paren (ge_ty t t1) pp_ty ppf t1 in
  match t with
  | TyInt -> pp_print_string ppf "int"
  | TyBool -> pp_print_string ppf "bool"
  | TyDyn -> pp_print_string ppf "?"
  | TyFun (t1,t2) ->
     fprintf ppf "%a -> %a"
       (* (with_paren (ge_ty t t1) pp_ty) t1 *)
       with_paren_L t1
       pp_ty t2

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

let pp_binop ppf op =
  pp_print_string ppf (string_of_binop op)

(* 1~10 で返す *)
let prec_binop = function
  | LOr -> 1
  | LAnd -> 2
  | Lt | Gt | Eq | LE | GE -> 3
  | Plus | Minus -> 4
  | Mult | Div -> 5

(* Will be tested *)
(* module G = struct
 *   open Syntax.G
 *   (\* precedence of expression *\)
 *   let prec_exp = function
 *     | LetExp _ | LetRecExp _ | FunExp _ -> 10
 *     | IfExp _ -> 20
 *     | BinOp (_, op, _, _) -> 30 + prec_binop op
 *     | AppExp _ -> 40
 *     | Var _ | ILit _ | BLit _ -> 50
 *
 *   (\* e1 > e2 : e1 associates stronger than e2 *\)
 *   let gt_exp e1 e2 = (prec_exp e1) > (prec_exp e2)
 *   (\* e1 >= e2 *\)
 *   let ge_exp e1 e2 = (prec_exp e1) >= (prec_exp e2)
 *
 *   let rec string_of_exp = function
 *     | Var (_,x) -> x
 *     | ILit (_,n) -> string_of_int n
 *     | BLit (_,b) -> string_of_bool b
 *     | BinOp (_, op, e1, e2) as e ->
 *        (\* Left assoc *\)
 *        sprintf "%s %s %s"
 *          (with_paren (gt_exp e e1) (string_of_exp e1)) (string_of_binop op)
 *          (with_paren (ge_exp e e2) (string_of_exp e2))
 *     | IfExp (_, e1, e2, e3) as e ->
 *        (\* e1,e2 は If 等ならカッコが要る *\)
 *        (\* e3 にカッコは不要 *\)
 *        sprintf "if %s then %s else %s"
 *          (with_paren (ge_exp e e1) (string_of_exp e1))
 *          (with_paren (ge_exp e e2) (string_of_exp e2))
 *          (string_of_exp e3)
 *
 *     | LetExp (_, bindings, e2) as e ->
 *        (\* string representation of bindings *\)
 *        let bindings_str =
 *          List.map (fun (x,e1) ->
 *              sprintf "%s = %s"
 *                x (with_paren (ge_exp e e1) (string_of_exp e1)))
 *            bindings in
 *        sprintf "let %s in %s"
 *          (String.concat " and " bindings_str)
 *          (string_of_exp e2)
 *
 *     | LetRecExp (_, bindings, e2) as e ->
 *        let bindings_str =
 *          List.map (fun (x,y,t1,t2,e1) ->
 *              sprintf "%s (%s : %s) : %s = %s"
 *                x y (string_of_ty t1) (string_of_ty t2)
 *                (with_paren (ge_exp e e1) (string_of_exp e1)))
 *            bindings in
 *        sprintf "let rec %s in %s"
 *          (String.concat " and " bindings_str)
 *          (string_of_exp e2)
 *
 *     | FunExp (_, x, t, e1) ->
 *        sprintf "fun (%s : %s) -> %s"
 *          x (string_of_ty t) (string_of_exp e1)
 *     | AppExp (_, e1, e2) as e ->
 *        (\* Left assoc *\)
 *        sprintf "%s %s"
 *          (with_paren (gt_exp e e1) (string_of_exp e1))
 *          (with_paren (ge_exp e e2) (string_of_exp e2))
 *
 *   let rec string_of_program = function
 *     | Exp e -> string_of_exp e
 *     | LetDecl bindings ->
 *        let bindings_str =
 *          List.map (fun (x,e1) ->
 *              sprintf "%s = %s" x (string_of_exp e1))
 *            bindings in
 *        sprintf "let %s" (String.concat " and " bindings_str)
 *     | LetRecDecl bindings ->
 *        let bindings_str =
 *          List.map (fun (x,y,t1,t2,e1) ->
 *              sprintf "%s (%s : %s) : %s = %s"
 *                x y (string_of_ty t1) (string_of_ty t2) (string_of_exp e1))
 *            bindings in
 *        sprintf "let rec %s" (String.concat " and " bindings_str)
 * end *)

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

  let rec pp_exp ppf f =
    (* pp_exp_paren1 means: paren needed, even if same precedence *)
    (* e.g. For left operand of left-associative operations,
     * no paren needed if same precedence,
     * so you should use pp_exp_paren2 (without eq) *)
    let pp_exp_paren1 ppf f1 = with_paren (ge_exp f f1) pp_exp ppf f1 in
    let pp_exp_paren2 ppf f1 = with_paren (gt_exp f f1) pp_exp ppf f1 in
    match f with
    | Var (_,x) -> pp_print_string ppf x
    | ILit (_,n) -> fprintf ppf "%d" n
    | BLit (_,b) -> pp_print_string ppf (string_of_bool b)
    | BinOp (_, op, f1, f2) ->
       fprintf ppf "%a %a %a"
         pp_exp_paren2 f1
         pp_binop op
         pp_exp_paren1 f2
    | IfExp (_, f1, f2, f3) ->
       fprintf ppf "if %a then %a else %a"
         pp_exp_paren1 f1
         pp_exp_paren1 f2
         pp_exp f3

    | LetExp (_, bindings, f2) ->
       pp_print_string ppf "let ";
       (* print bindings ; print _and_ between them *)
       pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " and ")
         (fun ppf (x,f1) ->
           fprintf ppf "%s = %a" x pp_exp_paren1 f1)
         ppf bindings;
       fprintf ppf " in %a" pp_exp f2

    | LetRecExp (_, bindings, f2) ->
       pp_print_string ppf "let rec ";
       pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " and ")
         (fun ppf (x,y,t1,t2,f1) ->
           fprintf ppf "%s (%s : %a) : %a = %a"
             x y pp_ty t1 pp_ty t2 pp_exp_paren1 f1)
         ppf bindings;
       fprintf ppf " in %a" pp_exp f2

    | FunExp (_, x, t, f1) ->
       fprintf ppf "fun (%s : %a) -> %a"
         x pp_ty t pp_exp f1
    | AppExp (_, f1, f2) ->     (* Left assoc *)
       fprintf ppf "%a %a"
         pp_exp_paren2 f1
         pp_exp_paren1 f2
    | CastExp (_, f1, t1, t2) ->
       fprintf ppf "%a : %a => %a"
         pp_exp_paren1 f1 pp_ty t1 pp_ty t2

  let rec pp_prog ppf = function
    | Exp f -> pp_exp ppf f
    | LetDecl bindings ->
       pp_print_string ppf "let ";
       (* print bindings ; print _and_ between them *)
       pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " and ")
         (fun ppf (x,f1) ->
           fprintf ppf "%s = %a" x pp_exp f1)
         ppf bindings

    | LetRecDecl bindings ->
       pp_print_string ppf "let rec ";
       pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " and ")
         (fun ppf (x,y,t1,t2,f1) ->
           fprintf ppf "%s (%s : %a) : %a = %a"
             x y pp_ty t1 pp_ty t2 pp_exp f1)
         ppf bindings

end
