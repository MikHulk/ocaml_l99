let pack_dup l =
  let process x ll =
    match ll with
    | [] -> [[x]]
    | (x'::t')::t -> if x = x' then (x::(x'::t'))::t else [x]::ll
    | []::_ -> raise (Match_failure ("packed element cannot be empty", 6, 4))
  in
  List.fold_right process l []
