-- Peel the first row, rotate the rest counter-clockwise, recurse.

-- Core has no List.transpose, so define our own.
partial def transpose : List (List α) → List (List α)
  | []           => []
  | [] :: _      => []
  | rows         =>
      let heads := rows.filterMap (fun r => r.head?)
      let tails := rows.map (fun r => r.tail)
      heads :: transpose tails

partial def spiralOrder : List (List α) → List α
  | []          => []
  | [] :: _     => []
  | row :: rows => row ++ spiralOrder (transpose rows).reverse

#guard spiralOrder [[1,2,3],[4,5,6],[7,8,9]] == [1,2,3,6,9,8,7,4,5]
#guard spiralOrder [[1,2,3,4],[5,6,7,8],[9,10,11,12]] ==
  [1,2,3,4,8,12,11,10,9,5,6,7]
