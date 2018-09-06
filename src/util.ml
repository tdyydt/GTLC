module Error = struct
  open Format
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

  let print_range ppf { start_p=p1; end_p=p2 } =
    fprintf ppf "line %d, character %d -- line %d, character %d"
      p1.pos_lnum
      (p1.pos_cnum - p1.pos_bol)
      p2.pos_lnum
      (p2.pos_cnum - p2.pos_bol)

end
