type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode l =
  let process x l' =
    match l' with
    | [] -> [One x]
    | (Many (n, x'))::t ->
      if x = x' then (Many (n + 1, x'))::t else (One x)::l'
    | (One x')::t ->
      if x = x' then (Many (2, x'))::t else (One x)::l'
  in
  List.fold_right process l []

let decode l =
  let process p l' =
    match p with
    | One x -> x::l'
    | Many(n, x) ->
      let s = Seq.repeat x |> Seq.take n in
      Seq.fold_left (Fun.flip List.cons) l' s
  in
  List.fold_right process l []
