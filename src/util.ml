(* Error *)
(* exception Error of string
 * let err s = raise (Error s)
 * let todo s = err ("Not implemented yet: " ^ s) *)

(* open Format *)

module Error = struct
  open Lexing

  type range = {
      start_p : position;
      end_p : position;
    }

  (* token INTV, ID *)
  type 'a with_range = {
      range : range;
      value : 'a;
    }

  let dummy_range : range = {
      start_p=dummy_pos;
      end_p=dummy_pos;
    }

  let join_range (r1 : range) (r2 : range) : range = {
      start_p=r1.start_p;
      end_p=r2.end_p;
    }

end
