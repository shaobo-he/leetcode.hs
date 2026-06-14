def mergeTwo : List Int → List Int → List Int
  | [], ys => ys
  | xs, [] => xs
  | x :: xs, y :: ys =>
      if x <= y then x :: mergeTwo xs (y :: ys)
      else y :: mergeTwo (x :: xs) ys

-- Divide and conquer: pairwise-merge the halves.
partial def mergeK : List (List Int) → List Int
  | []   => []
  | [xs] => xs
  | xss  =>
      let half := xss.length / 2
      let l := xss.take half
      let r := xss.drop half
      mergeTwo (mergeK l) (mergeK r)

#guard mergeK [[1,4,5],[1,3,4],[2,6]] == [1,1,2,3,4,4,5,6]
