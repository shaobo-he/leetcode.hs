/-
LeetCode 88: Merge Sorted Array — merge two sorted lists (Lean 4, core library only).

The earlier version only proved the *length* is preserved, which is a weak spec:
a function returning `replicate (m+n) 0` would satisfy it.  Here we capture the
real correctness of a merge, against an explicit comparator `le`:

  • `sortedMerge_perm`   — the output is a PERMUTATION of the two inputs
                           (so exactly the right elements, with multiplicity);
  • `sortedMerge_sorted` — merging two SORTED lists yields a SORTED list
                           (when `le` is total and transitive).

Together: the result is a *sorted permutation* of the inputs — i.e. the merge is
fully correct.  Length and membership fall out of the permutation as corollaries.
"Sorted" is core `List.Pairwise`.
-/

variable {α : Type}

def sortedMerge (le : α → α → Bool) : List α → List α → List α
  | [],      ys      => ys
  | x :: xs, []      => x :: xs
  | x :: xs, y :: ys =>
      if le x y then x :: sortedMerge le xs (y :: ys)
                else y :: sortedMerge le (x :: xs) ys

#guard sortedMerge (· ≤ ·) [1, 2, 3] [2, 5, 6] == [1, 2, 2, 3, 5, 6]
#guard sortedMerge (· ≤ ·) [1, 3, 4] [2, 5, 6] == [1, 2, 3, 4, 5, 6]

/- a list is sorted w.r.t. `le` when it is pairwise-`le` (core `List.Pairwise`) -/
def Sorted (le : α → α → Bool) (l : List α) : Prop :=
  l.Pairwise (fun a b => le a b = true)

/- ── "WHAT": the output holds exactly the inputs' elements (a permutation) ── -/
theorem sortedMerge_perm (le : α → α → Bool) :
    ∀ xs ys, List.Perm (sortedMerge le xs ys) (xs ++ ys) := by
  intro xs ys
  induction xs, ys using sortedMerge.induct le with
  | case1 ys     => rw [sortedMerge, List.nil_append]
  | case2 x xs   => rw [sortedMerge, List.append_nil]
  | case3 x xs y ys hle ih =>
      rw [sortedMerge, if_pos hle, List.cons_append]
      exact List.Perm.cons x ih
  | case4 x xs y ys hle ih =>
      rw [sortedMerge, if_neg (by simp [hle])]
      exact (List.Perm.cons y ih).trans List.perm_middle.symm

-- length preservation (the old theorem) now follows from the permutation
theorem sortedMerge_length (le : α → α → Bool) (xs ys : List α) :
    (sortedMerge le xs ys).length = xs.length + ys.length := by
  rw [(sortedMerge_perm le xs ys).length_eq, List.length_append]

-- so does "no element appears that wasn't in an input"
theorem mem_sortedMerge (le : α → α → Bool) (a : α) (xs ys : List α)
    (h : a ∈ sortedMerge le xs ys) : a ∈ xs ∨ a ∈ ys := by
  rw [← List.mem_append]; exact (sortedMerge_perm le xs ys).mem_iff.mp h

/- ── "ORDER": merging two sorted lists yields a sorted list ── -/
theorem sortedMerge_sorted (le : α → α → Bool)
    (tot : ∀ a b, le a b = true ∨ le b a = true)
    (tr  : ∀ a b c, le a b = true → le b c = true → le a c = true) :
    ∀ xs ys, Sorted le xs → Sorted le ys → Sorted le (sortedMerge le xs ys) := by
  intro xs ys
  induction xs, ys using sortedMerge.induct le with
  | case1 ys     => intro _ hy; rw [sortedMerge]; exact hy
  | case2 x xs   => intro hx _;  rw [sortedMerge]; exact hx
  | case3 x xs y ys hle ih =>
      intro hx hy
      rw [sortedMerge, if_pos hle]
      simp only [Sorted, List.pairwise_cons]
      refine ⟨fun b hb => ?_, ih (List.pairwise_cons.mp hx).2 hy⟩
      -- every element of the merged tail is ≥ x: it came from xs (use hx) or
      -- from y :: ys (it is y, or ≥ y, and x ≤ y, so x ≤ it by transitivity)
      rcases mem_sortedMerge le b _ _ hb with hbx | hby
      · exact (List.pairwise_cons.mp hx).1 b hbx
      · rcases List.mem_cons.mp hby with rfl | hb'
        · exact hle
        · exact tr x y b hle ((List.pairwise_cons.mp hy).1 b hb')
  | case4 x xs y ys hle ih =>
      intro hx hy
      have hyx : le y x = true := (tot x y).resolve_left (by simp [hle])
      rw [sortedMerge, if_neg (by simp [hle])]
      simp only [Sorted, List.pairwise_cons]
      refine ⟨fun b hb => ?_, ih hx (List.pairwise_cons.mp hy).2⟩
      rcases mem_sortedMerge le b _ _ hb with hbx | hby
      · rcases List.mem_cons.mp hbx with rfl | hb'
        · exact hyx
        · exact tr y x b hyx ((List.pairwise_cons.mp hx).1 b hb')
      · exact (List.pairwise_cons.mp hy).1 b hby

/- The LeetCode setting is integers: `≤` on `Nat` is total and transitive, so the
   spec instantiates — merging two `≤`-sorted Nat lists gives a `≤`-sorted list. -/
theorem sortedMerge_sorted_nat (xs ys : List Nat)
    (hx : Sorted (· ≤ ·) xs) (hy : Sorted (· ≤ ·) ys) :
    Sorted (· ≤ ·) (sortedMerge (· ≤ ·) xs ys) :=
  sortedMerge_sorted (· ≤ ·)
    (fun a b => by simp only [decide_eq_true_eq]; exact Nat.le_total a b)
    (fun a b c hab hbc => by simp only [decide_eq_true_eq] at *; exact Nat.le_trans hab hbc)
    xs ys hx hy

#print axioms sortedMerge_perm
#print axioms sortedMerge_sorted
