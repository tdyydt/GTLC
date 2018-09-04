open Syntax
open Syntax.C
open Stringify
open Printf

(* Eval_error is an implementation error
 * because static typing should have rejected them *)
exception Eval_error of string
let everr s = raise (Eval_error s)

type tag =
  | IntT                        (* int *)
  | BoolT                       (* bool *)
  | FunT                        (* Function (Arrow) [?->?] *)

let string_of_tag = function
  | IntT -> "int"
  | BoolT -> "bool"
  | FunT -> "? -> ?"

(* Blame(tag1, tag2):
 * The value has been tagged as tag1, but is being untagged as tag2 *)
exception Blame of tag * tag

(* exval, dnval *)
type value =
  | IntV of int
  | BoolV of bool
  (* Function closure consists of function name, body,
   * and environment, which contains values of free variables *)
  | FunV of id * exp * (env ref)
  (* Wrapped function:
   * Wrapped (v,t1,t2,t3,t4) ==> [v: (t1 -> t2) => (t3 -> t4)] *)
  | Wrapped of value * ty * ty * ty * ty
  (* Tagged value, aka Injection:
   * Tagged (G, v) ==> [v: G => ?] *)
  | Tagged of tag * value
and env = value Environment.t

let rec string_of_value = function
  | IntV n -> string_of_int n
  | BoolV b -> string_of_bool b
  | FunV _ -> "<fun>"
  (* Wrapped, Tagged のネストはあるか？ *)
  | Wrapped (v, t1, t2, t3, t4) ->
     (* How should wrapped functions be displayed? *)
     sprintf "(%s : %s -> %s => %s -> %s)"
       (string_of_value v) (string_of_ty t1) (string_of_ty t2)
       (string_of_ty t3) (string_of_ty t4)
  | Tagged (tag, v) ->
     sprintf "(%s : %s => ?)"
       (string_of_value v) (string_of_tag tag)

(* evaluate binary operation *)
let eval_binop (op : binOp) (v1 : value) (v2 : value) : value =
  match op, v1, v2 with
  | Plus, IntV n1, IntV n2 -> IntV (n1 + n2)
  | Minus, IntV n1, IntV n2 -> IntV (n1 - n2)
  | Mult, IntV n1, IntV n2 -> IntV (n1 * n2)
  | Div, IntV n1, IntV n2 -> IntV (n1 / n2)
  | Lt, IntV n1, IntV n2 -> BoolV (n1 < n2)
  | Gt, IntV n1, IntV n2 -> BoolV (n1 > n2)
  | Eq, IntV n1, IntV n2 -> BoolV (n1 = n2)
  | LE, IntV n1, IntV n2 -> BoolV (n1 <= n2)
  | GE, IntV n1, IntV n2 -> BoolV (n1 >= n2)
  | (Plus | Minus | Mult | Div | Lt | Gt | Eq | LE | GE), _, _ ->
     everr ("Both arguments must be integer: " ^ string_of_binop op)
  | LAnd, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | LOr, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | (LAnd | LOr), _, _ ->
     everr ("Both arguments must be boolean: " ^ string_of_binop op)

(* Big-step evaluation *)
let rec eval_exp : env -> exp -> value = fun env ->
  function
  | Var (_,x) ->
     (try
        let v = Environment.find x env in v
      with
      | Not_found -> everr (sprintf "E-Var: %s is not bound" x))
  | ILit (_,n) -> IntV n
  | BLit (_,b) -> BoolV b
  | BinOp (_, op, f1, f2) ->
     let v1 = eval_exp env f1 in
     let v2 = eval_exp env f2 in
     eval_binop op v1 v2
  | IfExp (_, f1, f2, f3) ->
     let v1 = eval_exp env f1 in
     (match v1 with
      | BoolV true -> eval_exp env f2
      | BoolV false -> eval_exp env f3
      | _ -> everr "E-If: Test expression must be boolean")
  | LetExp (_, bindings, f2) ->
     let val_bindings =
       List.map (fun (x,f1) -> (x, eval_exp env f1))
         bindings in
     let env' = Environment.add_all val_bindings env in
     eval_exp env' f2

  | FunExp (_, x, _, f) -> FunV (x, f, ref env)
  | AppExp (_, f1, f2) ->
     let v1 = eval_exp env f1 in
     let v2 = eval_exp env f2 in
     eval_app v1 v2
  | LetRecExp (_, bindings, f2) ->
     let dummy_env = ref Environment.empty in
     let val_bindings =
       List.map (fun (x,y,_,_,f1) -> (x, FunV (y,f1,dummy_env)))
         bindings in
     (* Overwrite dummy_env *)
     let new_env = Environment.add_all val_bindings env in
     dummy_env := new_env;
     eval_exp new_env f2

  | CastExp (r, f, t1, t2) ->
     let v = eval_exp env f in eval_cast v t1 t2

(* evaluate application [v1 v2] *)
and eval_app (v1 : value) (v2 : value) : value = match v1 with
  | FunV (x, body, fun_env) ->
     (* [(fun (x:_) -> body) v2] *)
     eval_exp (Environment.add x v2 !fun_env) body
  (* Wrap (AppCast) *)
  | Wrapped (v1', t1, t2, t3, t4) ->
     (* [(v1': t1 -> t2 => t3 -> t4) v2] -->
      * [(v1' (v2: t3 => t1)): t2 => t4] *)
     let v2' = eval_cast v2 t3 t1 in
     let v' = eval_app v1' v2' in eval_cast v' t2 t4
  | _ -> everr "EvalApp: Non-function value is applied"

(* evaluate cast [v: t1 => t2] *)
and eval_cast (v : value) (t1 : ty) (t2 : ty) : value =
  match (t1, t2) with
  (* IdBase *)
  | TyInt, TyInt -> v
  | TyBool, TyBool -> v
  (* IdStar *)
  | TyDyn, TyDyn -> v
  (* Cast to DYN *)
  | TyInt, TyDyn -> Tagged (IntT, v)
  | TyBool, TyDyn -> Tagged (BoolT, v)
  (* Ground; decompose cast *)
  | TyFun (t11, t12), TyDyn ->
     (* [(v: (t11 -> t12) => (? -> ?)): (? -> ?) => ?] *)
     let v' = Wrapped (v, t11, t12, TyDyn, TyDyn)
     in Tagged (FunT, v')

  (* either Succeed (Collapse), or Fail (Conflict) *)
  | TyDyn, TyInt ->
     (match v with
      (* [v': int => ? => int] --> v' *)
      | Tagged (IntT, v') -> v'
      | Tagged (tag, _) -> raise (Blame (tag, IntT))
      | _ -> everr "Should not happen: Untagged value")
  | TyDyn, TyBool ->
     (match v with
      | Tagged (BoolT, v') -> v'
      | Tagged (tag, _) -> raise (Blame (tag, BoolT))
      | _ -> everr "Should not happen: Untagged value")
  | TyDyn, TyFun (TyDyn, TyDyn) ->
     (match v with
      | Tagged (FunT, v') -> v'
      | Tagged (tag, _) -> raise (Blame (tag, FunT))
      (* In [v: ? => (? -> ?)], v must be Tagged (F, v') *)
      | _ -> everr "Should not happen: Untagged value")

  (* Expand *)
  | TyDyn, TyFun (t21, t22) ->  (* Not both t21 and t22 is TyDyn *)
     (* [v: ? => (t21 -> t22)] -->
      * [(v: ? => (? -> ?)): (? -> ?) => (t21 -> t22)] *)
     let v' = eval_cast v TyDyn (TyFun (TyDyn, TyDyn))
     in Wrapped (v', TyDyn, TyDyn, t21, t22)

  (* The remaining cases should have been rejected as static type error *)
  | _, _ -> everr "Should not happen"


let eval_prog : env -> program -> env * (id * value) list = fun env ->
  function
  | Exp f -> let v = eval_exp env f in (env, [("-", v)])
  | LetDecl bindings ->
     let val_bindings =
       List.map (fun (x,f1) -> (x, eval_exp env f1))
         bindings in
     let env' = Environment.add_all val_bindings env in
     (env', val_bindings)
  | LetRecDecl bindings ->
     let dummy_env = ref Environment.empty in
     let val_bindings =
       List.map (fun (x,y,_,_,f1) -> (x, FunV (y,f1,dummy_env)))
         bindings in
     let new_env = Environment.add_all val_bindings env in
     dummy_env := new_env;
     (new_env, val_bindings)
