(* https://github.com/ocaml/ocaml.org/blob/main/data/exercises/064_btree_layout_1.md *)

type 'a binary_tree =
    Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node
    ( 'n'
    , Node
      ('k'
      , Node
        ( 'c'
        , leaf 'a'
        , Node
          ( 'h'
          , Node
            ( 'g', leaf 'e', Empty )
          , Empty
          )
        )
      , leaf 'm'
      )
    , Node
      ( 'u'
      , Node
        ( 'p'
        , Empty
        , Node
          ( 's', leaf 'q', Empty )
        )
      , Empty
      )
    )

let layout_binary_tree_1 tree =
  let rec go visited_rank depth tree =
    match tree with
      Node (c, left, right) ->
      let visited_rank, visited_left = go visited_rank (depth + 1) left in
      let rank = visited_rank + 1 in
      let visited_rank, visited_right = go rank (depth + 1) right in
      (visited_rank, Node ((c, rank, depth), visited_left, visited_right))
    | _ -> (visited_rank, Empty)
  in go 0 1 tree |> snd
