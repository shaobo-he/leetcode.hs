-- Merge Sorted Array (merge two sorted lists, Lean 4, core library only)

def sortedMerge {α : Type} [Ord α] : List α → List α → List α
  | [], ys => ys
  | xs, [] => xs
  | x :: xs, y :: ys =>
    match Ord.compare x y with
    | Ordering.gt => y :: sortedMerge (x :: xs) ys
    | _           => x :: sortedMerge xs (y :: ys)

#guard sortedMerge [1, 2, 3] [2, 5, 6] == [1, 2, 2, 3, 5, 6]
#guard sortedMerge [1, 3, 4] [2, 5, 6] == [1, 2, 3, 4, 5, 6]

-- The dependent-length guarantee that the Idris version puts in its TYPE
-- (`Vect m → Vect n → Vect (m + n)`), here proven as a theorem in Lean.
theorem sortedMerge_length {α : Type} [Ord α] (xs ys : List α) :
    (sortedMerge xs ys).length = xs.length + ys.length := by
  induction xs, ys using sortedMerge.induct with
  | case1 ys => simp [sortedMerge]
  | case2 xs => simp [sortedMerge]
  | case3 x xs y ys h ih =>
      rw [sortedMerge, h]
      simp only [List.length_cons] at ih ⊢
      omega
  | case4 x xs y ys h ih =>
      cases hc : compare x y with
      | gt => exact absurd hc h
      | lt => rw [sortedMerge, hc]; simp only [List.length_cons] at ih ⊢; omega
      | eq => rw [sortedMerge, hc]; simp only [List.length_cons] at ih ⊢; omega
