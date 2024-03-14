(* https://github.com/ocaml/ocaml.org/blob/main/data/exercises/007_flatten_list.md *)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten t =
  let rec go cont t l =
    match t with
      One x :: t -> go cont t (x :: l)
    | Many l' :: t -> go (go cont t) l' l
    | [] -> cont l
  in go List.rev t []
