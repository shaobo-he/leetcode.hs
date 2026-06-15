-- Peel the first row, rotate the rest counter-clockwise, recurse.

-- total element count, opaque to `simp_wf`
def totalLen {α} : List (List α) → Nat
  | []      => 0
  | r :: rs => r.length + totalLen rs

theorem totalLen_append {α} (xs ys : List (List α)) :
    totalLen (xs ++ ys) = totalLen xs + totalLen ys := by
  induction xs with
  | nil => simp [totalLen]
  | cons r rs ih => simp only [List.cons_append, totalLen]; omega

theorem totalLen_reverse {α} (xs : List (List α)) : totalLen xs.reverse = totalLen xs := by
  induction xs with
  | nil => simp
  | cons r rs ih => simp only [List.reverse_cons, totalLen_append, totalLen, ih]; omega

-- one transpose step never grows, and strictly shrinks while a head exists
theorem totalLen_tail_le {α} (xs : List (List α)) :
    totalLen (xs.map List.tail) ≤ totalLen xs := by
  induction xs with
  | nil => simp [totalLen]
  | cons r rs ih =>
    simp only [List.map_cons, totalLen]
    have hr : (List.tail r).length ≤ r.length := by cases r <;> simp
    omega

theorem totalLen_tail_lt {α} (xs : List (List α)) (h : xs.filterMap List.head? ≠ []) :
    totalLen (xs.map List.tail) < totalLen xs := by
  induction xs with
  | nil => simp at h
  | cons r rs ih =>
    cases r with
    | nil =>
      have hrec := ih (by simpa using h)
      simp only [List.map_cons, totalLen, List.tail_nil, List.length_nil]; omega
    | cons a r' =>
      have hle := totalLen_tail_le rs
      simp only [List.map_cons, totalLen, List.tail_cons, List.length_cons]; omega

-- Core has no List.transpose, so define our own.  Total: each step drops the row
-- heads, strictly shrinking the total element count while a non-empty row exists.
def transpose : List (List α) → List (List α)
  | []           => []
  | [] :: _      => []
  | (h :: t) :: rest =>
      ((h :: t) :: rest).filterMap List.head? :: transpose (((h :: t) :: rest).map List.tail)
  termination_by l => totalLen l
  decreasing_by
    simp_wf
    have := totalLen_tail_le rest
    simp only [totalLen, List.length_cons]
    omega

-- the heads and tails of a step partition the elements, so transpose preserves
-- at most the original count
theorem totalLen_heads_tails {α} (xs : List (List α)) :
    totalLen xs = (xs.filterMap List.head?).length + totalLen (xs.map List.tail) := by
  induction xs with
  | nil => simp [totalLen]
  | cons r rs ih =>
    cases r with
    | nil => simp only [List.filterMap_cons, List.head?_nil, List.map_cons, totalLen,
                        List.tail_nil, List.length_nil]; omega
    | cons a r' => simp only [List.filterMap_cons, List.head?_cons, List.map_cons, totalLen,
                              List.tail_cons, List.length_cons]; omega

theorem totalLen_transpose_le {α} (xs : List (List α)) :
    totalLen (transpose xs) ≤ totalLen xs := by
  induction xs using transpose.induct with
  | case1 => simp [transpose, totalLen]
  | case2 _ => simp [transpose, totalLen]
  | case3 h t rest ih =>
    rw [transpose]
    have hsplit := totalLen_heads_tails ((h :: t) :: rest)
    have hcons : totalLen (((h :: t) :: rest).filterMap List.head? ::
                  transpose (((h :: t) :: rest).map List.tail))
                = (((h :: t) :: rest).filterMap List.head?).length
                  + totalLen (transpose (((h :: t) :: rest).map List.tail)) := rfl
    rw [hcons]
    omega

-- Total: peeling a non-empty first row drops ≥ 1 element, and the rotated tail
-- `(transpose rows).reverse` has no more elements than `rows`.
def spiralOrder : List (List α) → List α
  | []               => []
  | [] :: _          => []
  | (h :: t) :: rows => (h :: t) ++ spiralOrder (transpose rows).reverse
  termination_by l => totalLen l
  decreasing_by
    simp_wf
    rw [totalLen_reverse]
    have := totalLen_transpose_le rows
    simp only [totalLen, List.length_cons]
    omega

#guard spiralOrder [[1,2,3],[4,5,6],[7,8,9]] == [1,2,3,6,9,8,7,4,5]
#guard spiralOrder [[1,2,3,4],[5,6,7,8],[9,10,11,12]] ==
  [1,2,3,4,8,12,11,10,9,5,6,7]
