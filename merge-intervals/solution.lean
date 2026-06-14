/-
LeetCode 56: Merge Intervals — with a Lean 4 proof of correctness.

Algorithm: sort intervals by start, then left-fold a coalescing `step` that keeps
the accumulator *reversed* (head = most-recently-added = largest start). Finally
un-reverse. We replace the unverifiable `Array.qsort` with a verified insertion
sort `isort` (by start) so the whole pipeline can be reasoned about.

We prove three properties (each axiom-clean — see `#print axioms` at the bottom):

  1. `merge_handles_WF`     — well-formed inputs are correctly handled: the sort
                               drops/invents nothing, keeps every interval
                               well-formed, and produces a start-sorted list.
  2. `merge_outputs_WF`     — every output interval is well-formed (`lo ≤ hi`).
  3. `merge_outputs_NoOverlap` — the outputs are pairwise non-overlapping and
                               sorted: for any earlier `a` and later `b` in the
                               result, `a.2 < b.1` (a ends strictly before b starts).

Lean 4, core library only (no Mathlib / Std / Batteries).
-/

abbrev Iv := Int × Int

-- An interval `(lo, hi)` is well-formed when `lo ≤ hi`.
def WF (iv : Iv) : Prop := iv.1 ≤ iv.2

/- ── Verified insertion sort by start (`.1`) ───────────────────────────────── -/

def insertIv (x : Iv) : List Iv → List Iv
  | []      => [x]
  | y :: ys => if x.1 ≤ y.1 then x :: y :: ys else y :: insertIv x ys

def isort : List Iv → List Iv
  | []      => []
  | x :: xs => insertIv x (isort xs)

-- "sorted ascending by start" as a `Pairwise`.
def SortedByStart (l : List Iv) : Prop := l.Pairwise (fun a b => a.1 ≤ b.1)

-- insertIv preserves membership (so isort drops nothing and invents nothing).
theorem mem_insertIv (x : Iv) (l : List Iv) (a : Iv) :
    a ∈ insertIv x l ↔ a = x ∨ a ∈ l := by
  induction l with
  | nil => simp [insertIv]
  | cons y ys ih =>
      simp only [insertIv]
      by_cases h : x.1 ≤ y.1
      · rw [if_pos h]; simp only [List.mem_cons]
      · rw [if_neg h]; simp only [List.mem_cons, ih]
        constructor
        · rintro (rfl | rfl | h)
          · exact Or.inr (Or.inl rfl)
          · exact Or.inl rfl
          · exact Or.inr (Or.inr h)
        · rintro (rfl | rfl | h)
          · exact Or.inr (Or.inl rfl)
          · exact Or.inl rfl
          · exact Or.inr (Or.inr h)

theorem mem_isort (a : Iv) (l : List Iv) : a ∈ isort l ↔ a ∈ l := by
  induction l with
  | nil => simp [isort]
  | cons x xs ih => simp only [isort, mem_insertIv, List.mem_cons, ih]

-- insertIv preserves start-sortedness.
theorem sorted_insertIv (x : Iv) (l : List Iv) (hl : SortedByStart l) :
    SortedByStart (insertIv x l) := by
  induction l with
  | nil => simp [insertIv, SortedByStart]
  | cons y ys ih =>
      simp only [insertIv]
      by_cases h : x.1 ≤ y.1
      · rw [if_pos h]
        rw [SortedByStart, List.pairwise_cons]
        refine ⟨?_, hl⟩
        intro a ha
        rw [List.mem_cons] at ha
        rcases ha with rfl | ha
        · exact h
        · have hxy := hl
          rw [SortedByStart, List.pairwise_cons] at hxy
          exact Int.le_trans h (hxy.1 a ha)
      · rw [if_neg h]
        rw [SortedByStart, List.pairwise_cons] at hl
        rw [SortedByStart, List.pairwise_cons]
        refine ⟨?_, ih hl.2⟩
        intro a ha
        rw [mem_insertIv] at ha
        rcases ha with rfl | ha
        · exact Int.le_of_not_le h
        · exact hl.1 a ha

theorem sorted_isort (l : List Iv) : SortedByStart (isort l) := by
  induction l with
  | nil => simp [isort, SortedByStart]
  | cons x xs ih => simp only [isort]; exact sorted_insertIv x (isort xs) ih

/- ── Coalescing fold (accumulator kept reversed) ───────────────────────────── -/

-- The accumulator is reversed: head = most-recently-added = largest start so far.
def step (acc : List Iv) (iv : Iv) : List Iv :=
  match acc with
  | []          => [iv]
  | top :: rest =>
      if iv.1 ≤ top.2 then (top.1, max top.2 iv.2) :: rest
      else iv :: top :: rest

-- Separation on the *reversed* accumulator: `a` precedes `b` (a more recent,
-- larger start) means `b` ends strictly before `a` starts.
def Separated (a b : Iv) : Prop := b.2 < a.1

-- Fold invariant on the reversed accumulator: every interval well-formed, and
-- all pairs separated (which, after reversing, becomes "no overlap").
def AccInv (acc : List Iv) : Prop :=
  (∀ iv ∈ acc, WF iv) ∧ acc.Pairwise Separated

-- `step` preserves the invariant for any well-formed incoming interval.
-- (The fold always feeds intervals in nondecreasing start order, but the
-- invariant as stated does not even need that — only `WF iv`.)
theorem inv_step (acc : List Iv) (iv : Iv) (hinv : AccInv acc) (hwf : WF iv) :
    AccInv (step acc iv) := by
  cases acc with
  | nil =>
      simp only [step]
      refine ⟨?_, ?_⟩
      · intro a ha; rw [List.mem_singleton] at ha; subst ha; exact hwf
      · exact List.pairwise_singleton _ _
  | cons top rest =>
      obtain ⟨hwfAll, hpw⟩ := hinv
      rw [List.pairwise_cons] at hpw
      obtain ⟨htopSep, hrestPw⟩ := hpw
      simp only [step]
      by_cases h : iv.1 ≤ top.2
      · -- overlap: extend the current top's end to `max top.2 iv.2`
        rw [if_pos h]
        refine ⟨?_, ?_⟩
        · intro a ha
          rw [List.mem_cons] at ha
          rcases ha with rfl | ha
          · show top.1 ≤ max top.2 iv.2
            have htw : WF top := hwfAll top (by simp)
            have : top.1 ≤ top.2 := htw
            omega
          · exact hwfAll a (by simp [ha])
        · rw [List.pairwise_cons]
          refine ⟨?_, hrestPw⟩
          intro b hb
          show Separated (top.1, max top.2 iv.2) b
          have := htopSep b hb
          simp only [Separated] at this ⊢
          exact this
      · -- no overlap: push `iv` as a new, separated head
        rw [if_neg h]
        refine ⟨?_, ?_⟩
        · intro a ha
          rw [List.mem_cons] at ha
          rcases ha with rfl | ha
          · exact hwf
          · exact hwfAll a ha
        · rw [List.pairwise_cons]
          refine ⟨?_, ?_⟩
          · intro b hb
            rw [List.mem_cons] at hb
            simp only [Separated]
            rcases hb with rfl | hb
            · -- Separated iv top : top.2 < iv.1
              omega
            · -- Separated iv b : b.2 < top.1 ≤ top.2 < iv.1
              have hbtop := htopSep b hb
              simp only [Separated] at hbtop
              have htw : WF top := hwfAll top (by simp)
              have htle : top.1 ≤ top.2 := htw
              omega
          · rw [List.pairwise_cons]; exact ⟨htopSep, hrestPw⟩

-- Folding `step` over well-formed intervals preserves the invariant.
theorem foldl_inv (l : List Iv) (acc : List Iv)
    (hacc : AccInv acc) (hl : ∀ iv ∈ l, WF iv) :
    AccInv (l.foldl step acc) := by
  induction l generalizing acc with
  | nil => simpa using hacc
  | cons x xs ih =>
      simp only [List.foldl_cons]
      apply ih
      · exact inv_step acc x hacc (hl x (by simp))
      · intro iv hiv; exact hl iv (by simp [hiv])

/- ── The merge and the runnable tests ──────────────────────────────────────── -/

def merge (xs : List Iv) : List Iv :=
  ((isort xs).foldl step []).reverse

-- Output relation: `a` ends strictly before `b` starts — captures BOTH
-- "no overlap" and "sorted by start" for consecutive *and* all pairs.
def NoOverlap (a b : Iv) : Prop := a.2 < b.1

#guard merge [(1,3),(2,6),(8,10),(15,18)] == [(1,6),(8,10),(15,18)]
#guard merge [(1,4),(4,5)] == [(1,5)]

/- ── Correctness theorems ──────────────────────────────────────────────────── -/

-- All intervals produced by the fold are well-formed and pairwise separated.
theorem merge_acc_inv (xs : List Iv) (hwf : ∀ iv ∈ xs, WF iv) :
    AccInv ((isort xs).foldl step []) := by
  apply foldl_inv
  · exact ⟨by intro a ha; simp at ha, List.Pairwise.nil⟩
  · intro iv hiv; exact hwf iv ((mem_isort iv xs).1 hiv)

-- PROPERTY 1 — well-formed inputs are correctly *handled*.
-- Taking "every input interval is well-formed" as the hypothesis, the sorting
-- phase (a) keeps exactly the same intervals (membership both ways — nothing
-- dropped or invented), (b) keeps them all well-formed, and (c) leaves them
-- sorted by start, so the coalescing fold receives a well-formed, start-sorted list.
theorem merge_handles_WF (xs : List Iv) (hwf : ∀ iv ∈ xs, WF iv) :
    (∀ iv, iv ∈ isort xs ↔ iv ∈ xs)
    ∧ (∀ iv ∈ isort xs, WF iv)
    ∧ SortedByStart (isort xs) := by
  refine ⟨?_, ?_, ?_⟩
  · intro iv; exact mem_isort iv xs
  · intro iv hiv; exact hwf iv ((mem_isort iv xs).1 hiv)
  · exact sorted_isort xs

-- PROPERTY 2 — every output interval is well-formed (`lo ≤ hi`).
theorem merge_outputs_WF (xs : List Iv) (hwf : ∀ iv ∈ xs, WF iv) :
    ∀ iv ∈ merge xs, WF iv := by
  intro iv hiv
  have := (merge_acc_inv xs hwf).1
  simp only [merge, List.mem_reverse] at hiv
  exact this iv hiv

-- PROPERTY 3 — outputs are pairwise non-overlapping and sorted: for any earlier
-- `a` and later `b` in the result, `a.2 < b.1`.
theorem merge_outputs_NoOverlap (xs : List Iv) (hwf : ∀ iv ∈ xs, WF iv) :
    (merge xs).Pairwise NoOverlap := by
  have hsep := (merge_acc_inv xs hwf).2
  simp only [merge]
  rw [List.pairwise_reverse]
  apply List.Pairwise.imp _ hsep
  intro a b hab
  simp only [Separated] at hab
  simp only [NoOverlap]
  exact hab

#print axioms merge_handles_WF
#print axioms merge_outputs_WF
#print axioms merge_outputs_NoOverlap
