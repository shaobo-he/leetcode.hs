def mergeTwo : List Int → List Int → List Int
  | [], ys => ys
  | xs, [] => xs
  | x :: xs, y :: ys =>
      if x <= y then x :: mergeTwo xs (y :: ys)
      else y :: mergeTwo (x :: xs) ys

-- Divide and conquer: pairwise-merge the halves.  Total: for a list of length
-- ≥ 2, both `take half` and `drop half` are strictly shorter.
def mergeK : List (List Int) → List Int
  | []   => []
  | [xs] => xs
  | x :: y :: rest =>
      mergeTwo (mergeK ((x :: y :: rest).take ((x :: y :: rest).length / 2)))
               (mergeK ((x :: y :: rest).drop ((x :: y :: rest).length / 2)))
  termination_by l => l.length
  decreasing_by
    all_goals simp_wf
    all_goals omega

#guard mergeK [[1,4,5],[1,3,4],[2,6]] == [1,1,2,3,4,4,5,6]
