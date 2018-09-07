open Typing
open Syntax

(* cast-insertion translation *)

(* CI_bug is an implementation error
 * because static typing should have rejected them *)
exception CI_bug of string

(* insert cast if needed *)
let cast_opt (f : C.exp) (t1 : ty) (t2 : ty) : C.exp =
  if t1 = t2 then f
  else let r = C.range_of_exp f in C.CastExp (r,f,t1,t2)

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
  | G.Var (r,x) -> begin
     try
       let t = Environment.find x gamma in
       translate_tyopt (C.Var (r,x)) t tyopt
     with
     | Not_found -> raise (CI_bug ("Var: Not bound: " ^ x))
    end
  | G.ILit (r,n) -> translate_tyopt (C.ILit (r,n)) TyInt tyopt
  | G.BLit (r,b) -> translate_tyopt (C.BLit (r,b)) TyBool tyopt
  | G.BinOp (r, op, e1, e2) ->
     let f1, t1 = translate_exp gamma e1 in
     let f2, t2 = translate_exp gamma e2 in
     let u1, u2, u3 = ty_binop op in (* depends on op *)
     if are_consistent t1 u1 then
       if are_consistent t2 u2 then
         let f = C.BinOp (r, op, cast_opt f1 t1 u1, cast_opt f2 t2 u2) in
         translate_tyopt f u3 tyopt
       else raise (CI_bug "BinOp (left)")
     else raise (CI_bug "BinOp (right)")
  | G.IfExp (r, e1, e2, e3) ->
     let f1, t1 = translate_exp gamma e1 in
     if are_consistent t1 TyBool then
       let f2, t2 = translate_exp gamma e2 in
       let f3, t3 = translate_exp gamma e3 in
       begin match tyopt with
       | Some t ->
          if are_consistent t2 t then
            if are_consistent t3 t then
              (C.IfExp (r, cast_opt f1 t1 TyBool,
                        cast_opt f2 t2 t,
                        cast_opt f3 t3 t), t)
            else raise (CI_bug "If (else)")
          else raise (CI_bug "If (then)")
       | None ->
          begin match meet t2 t3 with
          | Some u -> (C.IfExp (r, cast_opt f1 t1 TyBool,
                                cast_opt f2 t2 u,
                                cast_opt f3 t3 u), u)
          | None -> raise (CI_bug "If (branches): Meet undef.")
          end
       end
     else raise (CI_bug "If (test)")
  | G.LetExp (r, bindings, e2) ->
     let new_bindings, gamma', _ = translate_bindings gamma bindings in
     let f2, t2 = translate_exp gamma' e2 in
     let f = C.LetExp (r, new_bindings, f2)
     in translate_tyopt f t2 tyopt

  | G.FunExp (r, x, t, e) ->
     let f, u = translate_exp (Environment.add x t gamma) e in
     translate_tyopt (C.FunExp (r, x, t, f)) (TyFun (t, u)) tyopt
  | G.AppExp (r, e1, e2) ->
     let f1, t1 = translate_exp gamma e1 in
     begin match matching_fun t1 with
     | Some (t11, t12) ->
        let f2, t2 = translate_exp gamma e2 in
        if are_consistent t2 t11
        then let f = C.AppExp (r, cast_opt f1 t1 (TyFun (t11, t12)),
                               cast_opt f2 t2 t11) in
             translate_tyopt f t12 tyopt
        else raise (CI_bug "App")
     | None -> raise (CI_bug "App: matching_fun")
     end

  | G.LetRecExp (r, bindings, e2) ->
     let new_bindings, gamma1, _ =
       translate_rec_bindings gamma bindings in
     let f2, t2 = translate_exp gamma1 e2 in
     translate_tyopt (C.LetRecExp (r, new_bindings, f2)) t2 tyopt

  | G.NilLit (r,t) -> translate_tyopt (C.NilLit (r,t)) (TyList t) tyopt
  | G.ConsExp (r, e1, e2) ->
     let f1, t1 = translate_exp gamma e1 in
     let f2, t2 = translate_exp gamma e2 in
     begin match matching_list t2 with
     | Some t21 ->
        begin match meet t1 t21 with
        | Some u -> let f = C.ConsExp (r, cast_opt f1 t1 u,
                                       cast_opt f2 t2 (TyList u)) in
                    translate_tyopt f u tyopt
        | None -> raise (CI_bug "Cons: Meet undef.")
        end
     | None -> raise (CI_bug "Cons: matching_list")
     end
  | G.MatchExp (r, e1, e2, x, y, e3) ->
     let f1, t1 = translate_exp gamma e1 in
     begin match matching_list t1 with
     | Some t11 ->              (* e1 : t11 list *)
        let f2, t2 = translate_exp gamma e2 in
        let f3, t3 = translate_exp (Environment.add x t11
                                      (Environment.add y (TyList t11) gamma)) e3 in
        begin match meet t2 t3 with
        | Some u -> let f = C.MatchExp (r, cast_opt f1 t1 (TyList t11),
                                        cast_opt f2 t2 u,
                                        x, y, cast_opt f3 t3 u) in
                    translate_tyopt f u tyopt
        | None -> raise (CI_bug "Match: Meet undef.")
        end
     | None -> raise (CI_bug "Match: matching_list")
     end

(* auxiliary function to translate LetRec bindings *)
and translate_rec_bindings : tyenv -> G.rec_bindings -> C.rec_bindings * tyenv * (id * ty) list =
  fun gamma bindings ->
  let ty_bindings =
    List.map (fun (x,_,paraty,retty,_) -> (x, TyFun (paraty,retty)))
      bindings in
  let gamma1 = Environment.add_all ty_bindings gamma in
  let new_bindings =
    List.map (fun (x,y,paraty,retty,e1) ->
        let gamma2 = Environment.add y paraty gamma1 in
        (* retty is given by a programmer *)
        let f1, retty' = translate_exp gamma2 e1 ~tyopt:(Some retty) in
        assert (retty = retty');
        (x,y,paraty,retty,f1))
      bindings in
  (* new_bindings : LetRec bindings in CC
   * gamma1 : bindings are added to the given tyenv (gamma)
   * ty_bindings : Only for soundness check in main, gamma1 is enough for LetRecExp *)
  (new_bindings, gamma1, ty_bindings)

and translate_bindings : tyenv -> G.bindings -> C.bindings * tyenv * (id * ty) list =
  fun gamma bindings ->
     let new_bindings : (id * C.exp * ty) list =
       List.map (fun (x,e1) ->
           let f1, t1 = translate_exp gamma e1 in (x, f1, t1))
         bindings in
     let gamma' =
       Environment.add_all
         (List.map (fun (x,_,t1) -> (x,t1)) new_bindings)
         gamma in
     (List.map (fun (x,f1,_) -> (x,f1)) new_bindings,
      gamma',
      List.map (fun (x,_,t1) -> (x,t1)) new_bindings)

(* ty_bindings (in 2nd retval) is used only for soundness check,
 * and actually is not necessary. *)
let translate_prog : tyenv -> G.program -> C.program * (id * ty) list =
  fun gamma ->
  function
  | G.Exp e ->
     let f, t = translate_exp gamma e in (C.Exp f, [("-", t)])
  | G.LetDecl bindings ->
     let new_bindings, _, ty_bindings = translate_bindings gamma bindings
     in (C.LetDecl new_bindings, ty_bindings)
  | G.LetRecDecl bindings ->
     let new_bindings, _, ty_bindings = translate_rec_bindings gamma bindings
     in (C.LetRecDecl new_bindings, ty_bindings)
