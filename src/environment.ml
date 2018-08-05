module M =
  Map.Make (
      struct
        type t = Syntax.id
        let compare (x : Syntax.id) y = compare x y
      end
    )

type 'a t = 'a M.t


let empty = M.empty

let add x v env = M.add x v env

let remove x env = M.remove x env

let find x env = M.find x env

let mem = M.mem

(* extend env with each binding *)
let add_all bindings env =
  List.fold_left (fun env_acc (x,v) ->
      add x v env_acc)
    env bindings
