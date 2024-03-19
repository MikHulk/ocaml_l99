let rm_dup l = 
  let process x l' =
    match l' with
    | [] -> [x]
    | x'::t -> if x <> x' then x::l' else l'
  in
  List.fold_right process l []
