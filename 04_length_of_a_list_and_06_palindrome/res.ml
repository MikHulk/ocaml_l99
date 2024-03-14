(* https://github.com/ocaml/ocaml.org/blob/main/data/exercises/004_length_of_list.md *)
(* https://github.com/ocaml/ocaml.org/blob/main/data/exercises/006_palindrome.md *)

let length =
  let rec go x = function
    | [] -> x
    | _::t -> go (x+1) t
  in go 0

let palindrome l =
  let len = (length l) in
  let mid = len / 2 in
  let seq = List.to_seq l in
  (Seq.take mid seq |> List.of_seq) =
  ( Seq.drop (mid + len - mid * 2) seq
    |> Seq.fold_left (Fun.flip List.cons) []
  )
