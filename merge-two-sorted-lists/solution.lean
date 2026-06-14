-- Merge two sorted lists into one sorted list.
-- Repeatedly emit the smaller head; when one list empties, append the other.

def mergeTwo [Ord α] : List α → List α → List α
  | [],      ys      => ys
  | xs,      []      => xs
  | x :: xs, y :: ys =>
    match compare x y with
    | .gt => y :: mergeTwo (x :: xs) ys
    | _   => x :: mergeTwo xs (y :: ys)

#guard mergeTwo [1, 2, 4] [1, 3, 4] == [1, 1, 2, 3, 4, 4]
