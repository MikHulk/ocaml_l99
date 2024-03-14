(* https://github.com/ocaml/ocaml.org/blob/main/data/exercises/003_nth_element.md *)
(* boooooooooooooooooring *)

let rec nth l i =
  match l with
    [] -> None
  | h::t -> if i = 1 then Some h else i - 1 |> nth t
