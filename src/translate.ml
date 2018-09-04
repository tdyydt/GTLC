open Typing
open Syntax

(* cast-insertion translation *)

(* CI_error is an implementation error
 * because static typing should have rejected them *)
exception CI_error of string
let err s = raise (CI_error s)  (* implementation bug *)

(* insert cast if needed *)
let cast_opt (f : C.exp) (t1 : ty) (t2 : ty) : C.exp =
  if t1 = t2 then f
  else C.CastExp (f,t1,t2)

(* auxiliary function, used in translate_exp, to deal with tyopt *)
(* cast f to type t2 (if any) *)
let translate_tyopt (f : C.exp) (t1 : ty) (tyopt : ty option) : C.exp * ty =
  match tyopt with
    (* t2 is given by a programmer *)
  | Some t2 -> (cast_opt f t1 t2, t2)
  | None -> (f, t1)

(* cast insertion [G |- e ~> f : T] *)
let rec translate_exp (gamma : tyenv) ?(tyopt : ty option = None) (e : G.exp) : C.exp * ty =
  match e with
  | G.Var (r,x) ->
     (try
        let t = Environment.find x gamma in
        translate_tyopt (C.Var x) t tyopt
      with
      | Not_found -> err "CI-Var: Not bound")
  | G.ILit (r,n) -> translate_tyopt (C.ILit n) TyInt tyopt
  | G.BLit (r,b) -> translate_tyopt (C.BLit b) TyBool tyopt
  | G.BinOp (r, op, e1, e2) ->
     let f1, t1 = translate_exp gamma e1 in
     let f2, t2 = translate_exp gamma e2 in
     let u1, u2, u3 = ty_binop op in (* depends on op *)
     if are_consistent t1 u1 then
       if are_consistent t2 u2 then
         let f = C.BinOp (op, cast_opt f1 t1 u1, cast_opt f2 t2 u2) in
         translate_tyopt f u3 tyopt
       else err "CI-BinOp-L"
     else err "CI-BinOp-R"
  | G.IfExp (r, e1, e2, e3) ->
     let f1, t1 = translate_exp gamma e1 in
     if are_consistent t1 TyBool then
       let f2, t2 = translate_exp gamma e2 in
       let f3, t3 = translate_exp gamma e3 in
       (match tyopt with
        | Some t ->
           if are_consistent t2 t then
             if are_consistent t3 t then
               (C.IfExp (cast_opt f1 t1 TyBool,
                         cast_opt f2 t2 t,
                         cast_opt f3 t3 t), t)
             else err "CI-If-Else"
           else err "CI-If-Then"
        | None ->
           (try
              let u = meet t2 t3 in
              (C.IfExp (cast_opt f1 t1 TyBool,
                        cast_opt f2 t2 u,
                        cast_opt f3 t3 u), u)
            with
            | Typing_error msg -> err ("CI-If-Br: " ^ msg)))
     else err "CI-If-Test"
  | G.LetExp (r, bindings, e2) ->
     let new_bindings =
       List.map (fun (x,e1) ->
           let f1, t1 = translate_exp gamma e1 in (x, f1, t1))
         bindings in
     let gamma' =
       Environment.add_all
         (List.map (fun (x,_,t1) -> (x,t1)) new_bindings)
         gamma in
     let f2, t2 = translate_exp gamma' e2 in
     let f = C.LetExp (List.map (fun (x,f1,_) -> (x,f1)) new_bindings,
                       f2)
     in translate_tyopt f t2 tyopt

  | G.FunExp (r, x, t, e) ->
     let f, u = translate_exp (Environment.add x t gamma) e in
     translate_tyopt (C.FunExp (x, t, f)) (TyFun (t, u)) tyopt
  | G.AppExp (r, e1, e2) ->
     let f1, t1 = translate_exp gamma e1 in
     (try
        let (t11, t12) = matching_fun t1 in
        let f2, t2 = translate_exp gamma e2 in
        if are_consistent t2 t11
        then let f = C.AppExp (cast_opt f1 t1 (TyFun (t11, t12)),
                               cast_opt f2 t2 t11) in
             translate_tyopt f t12 tyopt
        else err "CI-App"
      with
      | Typing_error msg -> tyerr ("CI-App: " ^ msg))

  | G.LetRecExp (r, bindings, e2) ->
     let new_bindings, gamma1, _ =
       translate_rec_bindings gamma bindings in
     let f2, t2 = translate_exp gamma1 e2 in
     translate_tyopt (C.LetRecExp (new_bindings, f2)) t2 tyopt

(* auxiliary function to translate LetRec bindings *)
and translate_rec_bindings : tyenv -> (id * id * ty * ty * G.exp) list -> (id * id * ty * ty * C.exp) list * tyenv * (id * ty) list =
  fun gamma bindings ->
  let ty_bindings =
    List.map (fun (x,_,paraty,retty,_) -> (x, TyFun (paraty,retty)))
      bindings in
  let gamma1 = Environment.add_all ty_bindings gamma in
  let new_bindings =            (* LetRec bindings in CC *)
    List.map (fun (x,y,paraty,retty,e1) ->
        let gamma2 = Environment.add y paraty gamma1 in
        (* retty is given by a programmer *)
        let f1, retty' = translate_exp gamma2 e1 ~tyopt:(Some retty) in
        assert (retty = retty');
        (x,y,paraty,retty,f1))
      bindings
  in (new_bindings, gamma1, ty_bindings)


let translate_prog : tyenv -> G.program -> C.program * (id * ty) list =
  fun gamma ->
  function
  | G.Exp e ->
     let f, t = translate_exp gamma e in (C.Exp f, [("-", t)])
  | G.LetDecl bindings ->
     let new_bindings =
       List.map (fun (x,e1) ->
           let f1, t1 = translate_exp gamma e1 in (x, f1, t1))
         bindings in
     (C.LetDecl (List.map (fun (x,f1,_) -> (x,f1)) new_bindings),
      List.map (fun (x,_,t1) -> (x,t1)) new_bindings)
  | G.LetRecDecl bindings ->
     let new_bindings, _, ty_bindings = translate_rec_bindings gamma bindings
     in (C.LetRecDecl new_bindings, ty_bindings)
