let encode l =
  let process x l' =
    match l' with
    | [] -> [(1, x)]
    | (n, x')::t -> if x = x' then (n + 1, x')::t else (1, x)::l'
  in
  List.fold_right process l []
