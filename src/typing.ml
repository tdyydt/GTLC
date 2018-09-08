open Util.Error
open Syntax

exception Type_error of range * string

type tyenv = ty Environment.t

let rec are_consistent (t1 : ty) (t2 : ty) : bool =
  match (t1,t2) with
  | TyDyn, _ -> true
  | _, TyDyn -> true
  | TyInt, TyInt -> true
  | TyBool, TyBool -> true
  | TyFun (t11,t12), TyFun (t21,t22) ->
     are_consistent t11 t21 && are_consistent t12 t22
  | TyList t11, TyList t21 -> are_consistent t11 t21
  | (_, _) -> false

let matching_fun : ty -> (ty * ty) option = function
  | TyFun (t1, t2) -> Some (t1, t2)
  | TyDyn -> Some (TyDyn, TyDyn) (* [?->?] *)
  | _ -> None

let matching_list : ty -> ty option = function
  | TyList t -> Some t
  | TyDyn -> Some TyDyn         (* [? list] *)
  | _ -> None

(* meet wrt. precision relation: [T < ?] *)
(* computes greatest common _static_ type of two *)
let rec meet (t1 : ty) (t2 : ty) : ty option = match (t1, t2) with
  | t1, t2 when t1 = t2 -> Some t1
  | TyDyn, t -> Some t
  | t, TyDyn -> Some t
  (* covariant in argument type *)
  | TyFun (t11, t12), TyFun (t21, t22) ->
     begin match meet t11 t21, meet t12 t22 with
     | Some t1', Some t2' -> Some (TyFun (t1', t2'))
     | _ -> None
     end
  | TyList t11, TyList t21 ->
     begin match meet t11 t21 with
     | Some u -> Some (TyList u)
     | None -> None
     end
  | _ -> None

(* type of binOp *)
let ty_binop : binOp -> ty * ty * ty = function
  | (Plus | Minus | Mult | Div) -> (TyInt, TyInt, TyInt)
  | (Lt | Gt | Eq | LE | GE) -> (TyInt, TyInt, TyBool)
  | (LAnd | LOr) -> (TyBool, TyBool, TyBool)

(* auxiliary function, used in ty_exp, to deal with tyopt *)
(* check if t1 is consistent with given type annotation t2 (if any) *)
let check_tyopt (r : range) (t1 : ty) (tyopt : ty option) : ty =
  match tyopt with
  | Some t2 ->
     (* t2 is type annotation given by programmer *)
     if are_consistent t1 t2 then t2
     else raise (Type_error (r, Format.asprintf "%a is not consistent with %a"
                                  Pp.pp_ty t1 Pp.pp_ty t2))
  | None -> t1

module G = struct
  open Syntax.G
  (* NOTE: tyopt means the type expression that e is expected to have.
   * It is currently used in LetRec. tyopt is None if such type
   * is not necessary. *)
  let rec ty_exp (gamma : tyenv) ?(tyopt : ty option = None) (e : exp) : ty =
    match e with
    | Var (r,x) -> begin
        try
          let t = Environment.find x gamma in
          check_tyopt r t tyopt
        with
        | Not_found -> raise (Type_error (r, "Var: Not bound: " ^ x))
      end
    | ILit (r,_) -> check_tyopt r TyInt tyopt
    | BLit (r,_) -> check_tyopt r TyBool tyopt
    | BinOp (r, op, e1, e2) ->
       let t1 = ty_exp gamma e1 in
       let t2 = ty_exp gamma e2 in
       let (u1, u2, u3) = ty_binop op in
       if are_consistent t1 u1 then
         if are_consistent t2 u2 then check_tyopt r u3 tyopt
         else raise (Type_error
                       (range_of_exp e2,
                        Format.asprintf "BinOp (right): %a is not consistent with %a"
                          Pp.pp_ty t2 Pp.pp_ty u2))
       else raise (Type_error (range_of_exp e1,
                               Format.asprintf "BinOp (left): %a is not consistent with %a"
                                 Pp.pp_ty t1 Pp.pp_ty u1))

    | IfExp (r, e1, e2, e3) ->
       let t1 = ty_exp gamma e1 in
       if are_consistent t1 TyBool then
         let t2 = ty_exp gamma e2 in
         let t3 = ty_exp gamma e3 in
         begin match tyopt with
         | Some t ->           (* t is geven annotatation & respected *)
            if are_consistent t2 t then
              if are_consistent t3 t then t
              else raise (Type_error
                            (range_of_exp e3,
                             Format.asprintf "If (else): %a is not consistent with %a"
                               Pp.pp_ty t3 Pp.pp_ty t))
            else raise (Type_error
                          (range_of_exp e2,
                           Format.asprintf "If (then): %a is not consistent with %a"
                             Pp.pp_ty t2 Pp.pp_ty t))
         | None ->
            begin match meet t2 t3 with
            | Some u -> u
            | None -> raise (Type_error
                               (r, Format.asprintf
                                     "If (branches): Meet is undefined on %a and %a"
                                     Pp.pp_ty t2 Pp.pp_ty t3))
            end
         end
       else raise (Type_error
                     (range_of_exp e1,
                      Format.asprintf "If (test): %a is not consistent with bool"
                        Pp.pp_ty t1))

    | LetExp (r, bindings, e2) ->
       let gamma', _ = ty_bindings gamma bindings in
       let t2 = ty_exp gamma' e2 in check_tyopt r t2 tyopt

    | FunExp (r, x, t, e) ->
       let u = ty_exp (Environment.add x t gamma) e in
       check_tyopt r (TyFun (t,u)) tyopt
    | AppExp (r, e1, e2) ->
       let t1 = ty_exp gamma e1 in
       begin match matching_fun t1 with
       | Some (t11, t12) ->
          let t2 = ty_exp gamma e2 in
          if are_consistent t2 t11 then check_tyopt r t12 tyopt
          else raise (Type_error (r, Format.asprintf "App: %a and %a are not consistent"
                                       Pp.pp_ty t2 Pp.pp_ty t11))
       | None -> raise (Type_error
                          (range_of_exp e1,
                           Format.asprintf "App: %a doesn't match a function type"
                             Pp.pp_ty t1))
       end
    | LetRecExp (r, bindings, e2) ->
       let gamma1, _ = ty_rec_bindings gamma bindings in
       let t2 = ty_exp gamma1 e2 in check_tyopt r t2 tyopt

    | NilLit (r, t) -> check_tyopt r (TyList t) tyopt
    | ConsExp (r, e1, e2) ->
       let t1 = ty_exp gamma e1 in
       let t2 = ty_exp gamma e2 in
       begin match matching_list t2 with
       | Some t21 ->
          (* t1 & t21 list *)
          begin match meet t1 t21 with
          | Some u -> check_tyopt r (TyList u) tyopt
          | None -> raise (Type_error
                             (r, Format.asprintf "Cons: Meet is undefined on %a and %a"
                                   Pp.pp_ty t1 Pp.pp_ty t21))
          end
       | None -> raise (Type_error
                          (range_of_exp e2,
                           Format.asprintf "Cons: %a doesn't match a list type"
                             Pp.pp_ty t2))
       end
    | MatchExp (r, e1, e2, x, y, e3) ->
       let t1 = ty_exp gamma e1 in
       begin match matching_list t1 with
       | Some t11 ->            (* e1 : t11 list *)
          let t2 = ty_exp gamma e2 in
          let t3 = ty_exp (Environment.add x t11
                             (Environment.add y (TyList t11) gamma)) e3 in
          begin match meet t2 t3 with
          | Some u -> check_tyopt r u tyopt
          | None -> raise (Type_error
                             (r, Format.asprintf "Match (branches): Meet is undefined on %a and %a"
                                   Pp.pp_ty t2 Pp.pp_ty t3))
          end
       | None -> raise (Type_error
                          (range_of_exp e1,
                           Format.asprintf "Match: %a doesn't match a list type"
                             Pp.pp_ty t1))
       end

  (* auxiliary function for typing LetRec bindings *)
  and ty_rec_bindings : tyenv -> rec_bindings -> tyenv * (id * ty) list =
    fun gamma bindings ->
    let ty_bindings =
      List.map (fun (x,_,paraty,retty,_) -> (x, TyFun (paraty,retty)))
        bindings in
    (* Bind n functions in the environment *)
    let gamma1 = Environment.add_all ty_bindings gamma in
    (* Type each expression (e1) in the bindings,
     * and check the type is consistent with the annotated type (retty) *)
    List.iter (fun (x,y,paraty,retty,e1) ->
        let gamma2 = Environment.add y paraty gamma1 in
        (* retty is given by a programmer *)
        let retty' = ty_exp gamma2 e1 ~tyopt:(Some retty) in
        assert (retty = retty'))
      bindings;
    (gamma1, ty_bindings)

  and ty_bindings : tyenv -> bindings -> tyenv * (id * ty) list =
    fun gamma bindings ->
    let ty_bindings =
      List.map (fun (x,e1) -> (x, ty_exp gamma e1)) bindings in
    let gamma' = Environment.add_all ty_bindings gamma in
    (gamma', ty_bindings)

  let ty_prog : tyenv -> program -> tyenv * (id * ty) list =
    fun gamma ->
    function
    | Exp e ->
       let t = ty_exp gamma e in (gamma, [("-", t)])
    | LetDecl bindings -> ty_bindings gamma bindings
    | LetRecDecl bindings -> ty_rec_bindings gamma bindings
end
