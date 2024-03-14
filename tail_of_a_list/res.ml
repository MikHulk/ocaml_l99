(* https://github.com/ocaml/ocaml.org/blob/main/data/exercises/001_tail.md *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::t -> last t
