/-
LeetCode 31: Next Permutation — a purely functional recast over `List Int`,
with Lean 4 proofs of correctness (core library only, no Mathlib / Lake).

Algorithm (the classic in-place one, recast structurally):
  * `findPivot` splits the list as `pre ++ p :: suf`, where `suf` is the maximal
    weakly-decreasing suffix and `p < head suf` (`p` is the rightmost "ascent").
    `none` ⇒ the whole list is weakly decreasing, i.e. it is the maximal
    arrangement, and the answer is the reversal (the minimal arrangement / wrap).
  * `swapLast p suf` replaces the *rightmost* element of `suf` that is `> p` by
    `p`, returning that element `g`.  The modified suffix stays weakly decreasing,
    so reversing it gives the ascending (minimal) tail.
  * `nextPerm xs = pre ++ g :: (modifiedSuffix).reverse`.

We prove two theorems:
  * `nextPerm_perm` — the result is always a permutation of the input;
  * `nextPerm_gt`   — when the input is not already maximal (a pivot exists), the
    result is strictly greater than the input in `List.Lex (· < ·)`.

The betweenness/minimality bound (`nextPerm_least`) is left as future work.
-/

-- Rightmost ascent split: `xs = pre ++ p :: suf` with `suf` weakly decreasing and
-- `p < head suf`.  `none` ⇒ `xs` is weakly decreasing (already maximal).
def findPivot : List Int → Option (List Int × Int × List Int)
  | []           => none
  | [_]          => none
  | x :: y :: rest =>
    match findPivot (y :: rest) with
    | some (pre, p, suf) => some (x :: pre, p, suf)
    | none               => if x < y then some ([], x, y :: rest) else none

-- Replace the rightmost element `> p` with `p`; return that element and the new list.
def swapLast (p : Int) : List Int → Option (Int × List Int)
  | []        => none
  | c :: rest =>
    match swapLast p rest with
    | some (g, rest') => some (g, c :: rest')
    | none            => if p < c then some (c, p :: rest) else none

def nextPerm (xs : List Int) : List Int :=
  match findPivot xs with
  | none               => xs.reverse
  | some (pre, p, suf) =>
    match swapLast p suf with
    | some (g, suf') => pre ++ g :: suf'.reverse
    | none           => xs.reverse        -- unreachable (a pivot guarantees success)

#guard nextPerm [1, 2, 3] == [1, 3, 2]
#guard nextPerm [3, 2, 1] == [1, 2, 3]     -- wrap: maximal → minimal
#guard nextPerm [1, 1, 5] == [1, 5, 1]
#guard nextPerm [1, 3, 2] == [2, 1, 3]
#guard nextPerm [1, 5, 1] == [5, 1, 1]

/- ── `findPivot` recovers the split, and the pivot is below the suffix head ── -/

theorem findPivot_eq : ∀ (xs pre suf : List Int) (p : Int),
    findPivot xs = some (pre, p, suf) → xs = pre ++ p :: suf := by
  intro xs
  induction xs with
  | nil => intro pre suf p h; simp [findPivot] at h
  | cons x xs ih =>
    cases xs with
    | nil => intro pre suf p h; simp [findPivot] at h
    | cons y rest =>
      intro pre suf p h
      simp only [findPivot] at h
      cases hf : findPivot (y :: rest) with
      | some res =>
        obtain ⟨pre', p', suf'⟩ := res
        rw [hf] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨hpre, hp, hsuf⟩ := h
        subst hpre; subst hp; subst hsuf
        have hih := ih pre' suf' p' hf
        rw [hih, List.cons_append]
      | none =>
        rw [hf] at h
        by_cases hxy : x < y
        · rw [if_pos hxy] at h
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨hpre, hp, hsuf⟩ := h
          subst hpre; subst hp; subst hsuf
          rfl
        · rw [if_neg hxy] at h; simp at h

theorem findPivot_lt : ∀ (xs pre suf : List Int) (p : Int),
    findPivot xs = some (pre, p, suf) → ∃ c suf0, suf = c :: suf0 ∧ p < c := by
  intro xs
  induction xs with
  | nil => intro pre suf p h; simp [findPivot] at h
  | cons x xs ih =>
    cases xs with
    | nil => intro pre suf p h; simp [findPivot] at h
    | cons y rest =>
      intro pre suf p h
      simp only [findPivot] at h
      cases hf : findPivot (y :: rest) with
      | some res =>
        obtain ⟨pre', p', suf'⟩ := res
        rw [hf] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨hpre, hp, hsuf⟩ := h
        subst hp; subst hsuf
        exact ih pre' suf' p' hf
      | none =>
        rw [hf] at h
        by_cases hxy : x < y
        · rw [if_pos hxy] at h
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨hpre, hp, hsuf⟩ := h
          subst hp; subst hsuf
          exact ⟨y, rest, rfl, hxy⟩
        · rw [if_neg hxy] at h; simp at h

/- ── `swapLast`: it is a permutation, the swapped element exceeds `p`, and a
   suffix headed by something `> p` makes it succeed. ── -/

theorem swapLast_perm : ∀ (p : Int) (l : List Int) (g : Int) (l' : List Int),
    swapLast p l = some (g, l') → List.Perm (g :: l') (p :: l) := by
  intro p l
  induction l with
  | nil => intro g l' h; simp [swapLast] at h
  | cons c rest ih =>
    intro g l' h
    simp only [swapLast] at h
    cases hs : swapLast p rest with
    | some res =>
      obtain ⟨g0, r0⟩ := res
      rw [hs] at h
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨hg, hl⟩ := h; subst hg; subst hl
      have hih := ih g0 r0 hs
      exact (List.Perm.swap c g0 r0).trans
              ((List.Perm.cons c hih).trans (List.Perm.swap p c rest))
    | none =>
      rw [hs] at h
      by_cases hpc : p < c
      · rw [if_pos hpc] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨hg, hl⟩ := h; subst hg; subst hl
        exact List.Perm.swap p c rest
      · rw [if_neg hpc] at h; simp at h

theorem swapLast_gt : ∀ (p : Int) (l : List Int) (g : Int) (l' : List Int),
    swapLast p l = some (g, l') → p < g := by
  intro p l
  induction l with
  | nil => intro g l' h; simp [swapLast] at h
  | cons c rest ih =>
    intro g l' h
    simp only [swapLast] at h
    cases hs : swapLast p rest with
    | some res =>
      obtain ⟨g0, r0⟩ := res
      rw [hs] at h
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨hg, hl⟩ := h; subst hg
      exact ih g0 r0 hs
    | none =>
      rw [hs] at h
      by_cases hpc : p < c
      · rw [if_pos hpc] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨hg, hl⟩ := h; subst hg; exact hpc
      · rw [if_neg hpc] at h; simp at h

theorem swapLast_succeeds (p c : Int) (rest : List Int) (h : p < c) :
    ∃ g l', swapLast p (c :: rest) = some (g, l') := by
  simp only [swapLast]
  cases hs : swapLast p rest with
  | some res => obtain ⟨g0, r0⟩ := res; exact ⟨g0, c :: r0, rfl⟩
  | none      => rw [if_pos h]; exact ⟨c, p :: rest, rfl⟩

/- ── A common prefix factors out of a lexicographic comparison. ── -/

theorem lex_append_left {α} {r : α → α → Prop} (pre : List α) {a b : List α}
    (h : List.Lex r a b) : List.Lex r (pre ++ a) (pre ++ b) := by
  induction pre with
  | nil => exact h
  | cons x xs ih => exact List.Lex.cons ih

/- ── MAIN THEOREM 1 — the result is a permutation of the input. ── -/

theorem nextPerm_perm (xs : List Int) : List.Perm (nextPerm xs) xs := by
  cases hf : findPivot xs with
  | none =>
    have hval : nextPerm xs = xs.reverse := by simp only [nextPerm, hf]
    rw [hval]; exact List.reverse_perm xs
  | some res =>
    obtain ⟨pre, p, suf⟩ := res
    cases hs : swapLast p suf with
    | none =>
      have hval : nextPerm xs = xs.reverse := by simp only [nextPerm, hf, hs]
      rw [hval]; exact List.reverse_perm xs
    | some res2 =>
      obtain ⟨g, suf'⟩ := res2
      have hval : nextPerm xs = pre ++ g :: suf'.reverse := by
        simp only [nextPerm, hf, hs]
      have heq : xs = pre ++ p :: suf := findPivot_eq xs pre suf p hf
      have hp : List.Perm (g :: suf') (p :: suf) := swapLast_perm p suf g suf' hs
      rw [hval, heq]
      exact List.Perm.append_left pre
              ((List.Perm.cons g (List.reverse_perm suf')).trans hp)

/- ── MAIN THEOREM 2 — when a pivot exists (input not maximal), the result is
   strictly greater than the input in `List.Lex (· < ·)`. ── -/

theorem nextPerm_gt (xs : List Int) {pre suf : List Int} {p : Int}
    (hf : findPivot xs = some (pre, p, suf)) :
    List.Lex (· < ·) xs (nextPerm xs) := by
  obtain ⟨c, suf0, hsuf, hpc⟩ := findPivot_lt xs pre suf p hf
  subst hsuf
  obtain ⟨g, suf', hs⟩ := swapLast_succeeds p c suf0 hpc
  have hgt : p < g := swapLast_gt p (c :: suf0) g suf' hs
  have heq : xs = pre ++ p :: c :: suf0 := findPivot_eq xs pre (c :: suf0) p hf
  have hval : nextPerm xs = pre ++ g :: suf'.reverse := by
    simp only [nextPerm, hf, hs]
  rw [hval, heq]
  exact lex_append_left pre (List.Lex.rel hgt)

#print axioms nextPerm_perm
#print axioms nextPerm_gt
