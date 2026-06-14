-- Permutations (Lean 4, core library only)

-- Each element picked as head, paired with the remaining list.
def selections {α : Type} : List α → List (α × List α)
  | [] => []
  | x :: xs => (x, xs) :: (selections xs).map (fun (y, ys) => (y, x :: ys))

partial def permute {α : Type} : List α → List (List α)
  | [] => [[]]
  | xs =>
    (selections xs).flatMap (fun (x, rest) => (permute rest).map (fun p => x :: p))

-- 3 distinct elements -> 3! = 6 permutations, each of length 3.
#guard (permute [1, 2, 3]).length == 6
#guard (permute [1, 2, 3]).all (fun p => p.length == 3)
-- Exact enumeration order matches the .hs/.idr "pick head, recurse" scheme.
#guard permute [1, 2, 3] ==
  [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
