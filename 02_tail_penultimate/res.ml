(* https://github.com/ocaml/ocaml.org/blob/main/data/exercises/002_tail_penultimate.md *)

let rec last_two = function
  | [] -> None
  | [x;y] -> Some (x, y)
  | _::t -> last t
