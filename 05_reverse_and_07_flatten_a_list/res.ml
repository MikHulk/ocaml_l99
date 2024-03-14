(* https://github.com/ocaml/ocaml.org/blob/main/data/exercises/005_reverse_list.md *)
(* https://github.com/ocaml/ocaml.org/blob/main/data/exercises/007_flatten_list.md *)

type 'a node =
  | One of 'a
  | Many of 'a node list


let rev =
  let rec go l' = function
    | [] -> l'
    | h::t -> rev (h::l') t
  in go []


let flatten t =
  let rec go cont t l =
    match t with
      One x :: t -> go cont t (x :: l)
    | Many l' :: t -> go (go cont t) l' l
    | [] -> cont l
  in go rev t []
