(* Error *)
exception Error of string
let err s = raise (Error s)
let todo s = err ("Not implemented yet: " ^ s) (* TODO: remove *)
