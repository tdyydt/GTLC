open Format
open Syntax

(* ppf_e is printer for e *)
let with_paren flag ppf_e ppf e =
  fprintf ppf (if flag then "(%a)" else "%a") ppf_e e

(* precedence of type *)
(* larger number means higher precedence *)
let prec_ty = function
  | TyInt | TyBool | TyDyn -> 3
  | TyList _ -> 2
  | TyFun _ -> 1

let ge_ty t1 t2 = (prec_ty t1) >= (prec_ty t2)
let gt_ty t1 t2 = (prec_ty t1) > (prec_ty t2)

let rec pp_ty ppf t =
  (* put paren around t1 if needed;
   * 1: paren needed, even if same precedence
   * 2: no paren needed, if same precedence *)
  let pp_ty_paren1 ppf t1 = with_paren (ge_ty t t1) pp_ty ppf t1 in
  let pp_ty_paren2 ppf t1 = with_paren (gt_ty t t1) pp_ty ppf t1 in
  match t with
  | TyInt -> pp_print_string ppf "int"
  | TyBool -> pp_print_string ppf "bool"
  | TyDyn -> pp_print_string ppf "?"
  | TyFun (t1,t2) ->            (* Right assoc *)
     (* NOTE: gt_ty & pp_ty_paren2 is not necessary;
      * [pp_ty t2] is ok for now *)
     fprintf ppf "%a -> %a" pp_ty_paren1 t1 pp_ty_paren2 t2
  | TyList t1 ->                (* e.g. int list list *)
     fprintf ppf "%a list" pp_ty_paren2 t1

let pp_binop ppf op =
  pp_print_string ppf (string_of_binop op)

(* true if op is right assoc, false if left assoc *)
let right_assoc : binOp -> bool = function
  | LAnd | LOr -> true
  | _ -> false

module C = struct
  open Syntax.C
  (* precedence of expression *)
  let prec_exp = function
    | LetExp _ | LetRecExp _ | FunExp _ | MatchExp _ -> 10
    | IfExp _ -> 20
    | CastExp _ -> 21           (* ?????? *)
    | BinOp (_, LOr, _, _) -> 31
    | BinOp (_, LAnd, _, _) -> 32
    | BinOp (_, (Lt | Gt | Eq | LE | GE), _, _) -> 33
    | ConsExp _ -> 34
    | BinOp (_, (Plus | Minus), _, _) -> 35
    | BinOp (_, (Mult | Div), _, _) -> 36
    | AppExp _ -> 40
    | Var _ | ILit _ | BLit _ | NilLit _ -> 50

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
    | BLit (_,b) -> pp_print_bool ppf b
    | BinOp (_, op, f1, f2) ->
       if right_assoc op then   (* Right assoc *)
         fprintf ppf "%a %a %a"
           pp_exp_paren1 f1
           pp_binop op
           pp_exp_paren2 f2
       else                     (* Left assoc *)
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
       fprintf ppf "let %a in %a"
         pp_bindings bindings pp_exp f2
    | LetRecExp (_, bindings, f2) ->
       fprintf ppf "let rec %a in %a"
         pp_rec_bindings bindings pp_exp f2
    | FunExp (_, x, t, f1) ->
       fprintf ppf "fun (%s : %a) -> %a"
         x pp_ty t pp_exp f1
    | AppExp (_, f1, ILit n) when n < 0 ->
       fprintf ppf "%a (%d)" pp_exp_paren2 f1 n
    | AppExp (_, f1, f2) ->     (* Left assoc *)
       fprintf ppf "%a %a"
         pp_exp_paren2 f1
         pp_exp_paren1 f2
    | CastExp (_, f1, t1, t2) ->
       fprintf ppf "%a : %a => %a"
         pp_exp_paren1 f1 pp_ty t1 pp_ty t2

    | NilLit (_, t) ->          (* use %@ to output @ *)
       fprintf ppf "[%@%a]" pp_ty t
    | ConsExp (_, f1, f2) ->    (* Right assoc*)
       fprintf ppf "%a :: %a"
         pp_exp_paren1 f1 pp_exp_paren2 f2
    | MatchExp (_, f1, f2, x, y, f3) ->
       (* if f2, f3 is Match, we need paren for them *)
       fprintf ppf "match %a with [] -> %a | %s :: %s -> %a"
         pp_exp_paren1 f1 pp_exp_paren1 f2
         x y pp_exp_paren1 f3

  and pp_bindings ppf bindings =
    (* print bindings ; print _and_ between them *)
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " and ")
      (fun ppf (x,f1) ->     (* always without paren; ok? *)
        fprintf ppf "%s = %a" x pp_exp f1)
      ppf bindings

  and pp_rec_bindings ppf bindings =
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " and ")
      (fun ppf (x,y,t1,t2,f1) ->
        fprintf ppf "%s (%s : %a) : %a = %a"
          x y pp_ty t1 pp_ty t2 pp_exp f1)
      ppf bindings


  let rec pp_prog ppf = function
    | Exp f -> pp_exp ppf f
    | LetDecl bindings ->
       fprintf ppf "let %a" pp_bindings bindings
    | LetRecDecl bindings ->
       fprintf ppf "let rec %a" pp_rec_bindings bindings

end

module G = struct
  open Syntax.G
  (* precedence of expression *)
  let prec_exp = function
    | LetExp _ | LetRecExp _ | FunExp _ -> 10
    | IfExp _ -> 20
    | BinOp (_, LOr, _, _) -> 31
    | BinOp (_, LAnd, _, _) -> 32
    | BinOp (_, (Lt | Gt | Eq | LE | GE), _, _) -> 33
    | ConsExp _ -> 34
    | BinOp (_, (Plus | Minus), _, _) -> 35
    | BinOp (_, (Mult | Div), _, _) -> 36
    | AppExp _ -> 40
    | Var _ | ILit _ | BLit _ -> 50

  (* e1 > e2 : e1 associates stronger than e2 *)
  let gt_exp e1 e2 = (prec_exp e1) > (prec_exp e2)
  (* e1 >= e2 *)
  let ge_exp e1 e2 = (prec_exp e1) >= (prec_exp e2)

  let rec pp_exp ppf e =
    let pp_exp_paren1 ppf e1 = with_paren (ge_exp e e1) pp_exp ppf e1 in
    let pp_exp_paren2 ppf e1 = with_paren (gt_exp e e1) pp_exp ppf e1 in
    match e with
    | Var (_,x) -> pp_print_string ppf x
    | ILit (_,n) -> fprintf ppf "%d" n
    | BLit (_,b) -> pp_print_bool ppf b
    | BinOp (_, op, e1, e2) ->
       fprintf ppf "%a %a %a"
         pp_exp_paren2 e1
         pp_binop op
         pp_exp_paren1 e2
    | IfExp (_, e1, e2, e3) ->
       fprintf ppf "if %a then %a else %a"
         pp_exp_paren1 e1
         pp_exp_paren1 e2
         pp_exp e3
    | LetExp (_, bindings, e2) ->
       fprintf ppf "let %a in %a"
         pp_bindings bindings pp_exp e2
    | LetRecExp (_, bindings, e2) ->
       fprintf ppf "let rec %a in %a"
         pp_rec_bindings bindings pp_exp e2
    | FunExp (_, x, t, e1) ->
       fprintf ppf "fun (%s : %a) -> %a"
         x pp_ty t pp_exp e1
    | AppExp (_, e1, e2) ->
       fprintf ppf "%a %a"
         pp_exp_paren2 e1
         pp_exp_paren1 e2

  and pp_bindings ppf bindings =
    (* print bindings ; print _and_ between them *)
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " and ")
      (fun ppf (x,e1) ->
        fprintf ppf "%s = %a" x pp_exp e1)
      ppf bindings

  and pp_rec_bindings ppf bindings =
    pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " and ")
      (fun ppf (x,y,t1,t2,e1) ->
        fprintf ppf "%s (%s : %a) : %a = %a"
          x y pp_ty t1 pp_ty t2 pp_exp e1)
      ppf bindings

  let rec pp_prog ppf = function
    | Exp e -> pp_exp ppf e
    | LetDecl bindings ->
       fprintf ppf "let %a" pp_bindings bindings
    | LetRecDecl bindings ->
       fprintf ppf "let rec %a" pp_rec_bindings bindings
end

open Eval

let string_of_tag = function
  | IntT -> "int"
  | BoolT -> "bool"
  | FunT -> "? -> ?"
  | ListT -> "? list"

let rec pp_value ppf = function
  | IntV n -> fprintf ppf "%d" n
  | BoolV b -> pp_print_bool ppf b
  | FunV _ -> pp_print_string ppf "<fun>"
  | Wrapped (v, t1, t2, t3, t4, _, _) ->
     (* How should wrapped functions be displayed? *)
     (* Wrapped, Tagged can be nested? *)
     fprintf ppf "%a : %a -> %a => %a -> %a"
       pp_value v pp_ty t1 pp_ty t2 pp_ty t3 pp_ty t4
  | Tagged (tag, v) ->
     fprintf ppf "%a : %s => ?" pp_value v (string_of_tag tag)
  | NilV -> pp_print_string ppf "[]"
  (* | ConsV (v1, v2) ->           (\* TODO: Use with_paren *\)
   *    begin match *)
