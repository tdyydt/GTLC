open Syntax

val are_consistent : ty -> ty -> bool
val matching_fun : ty -> (ty * ty) option
val join : ty -> ty -> ty       (* change return type to ty option?? *)

val ty_binop : binOp -> ty * ty * ty
val ty_exp : ty Environment.t -> G.exp -> ty
val ty_prog : ty Environment.t -> G.program -> ty Environment.t * id * ty
