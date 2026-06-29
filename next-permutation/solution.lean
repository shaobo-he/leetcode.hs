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

We prove three theorems:
  * `nextPerm_perm` — the result is always a permutation of the input;
  * `nextPerm_gt`   — when the input is not already maximal (a pivot exists), the
    result is strictly greater than the input in `List.Lex (· < ·)`.
  * `nextPerm_least` — the betweenness/minimality bound: when a pivot exists, no
    permutation of the input lies strictly between it and `nextPerm` in lex order
    (so `nextPerm` is the *immediate* successor).  The proof rests on three facts:
    the decreasing suffix is the lex-maximal arrangement of its elements, the
    swapped-in `g` is the least suffix element exceeding the pivot `p`, and the
    rebuilt ascending tail is the lex-minimal arrangement of the remainder.
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

/- ── Sorting extremality: an ascending list is the lex-MINIMAL arrangement of
   its elements, a descending list is the lex-MAXIMAL one. ── -/

theorem asc_le : ∀ {l m : List Int},
    List.Pairwise (· ≤ ·) l → List.Perm l m → l ≤ m := by
  intro l
  induction l with
  | nil => intro m _ _; exact List.nil_le m
  | cons a l' ih =>
    intro m hs hperm
    cases m with
    | nil => simp at hperm
    | cons b m' =>
      have hsplit := List.pairwise_cons.mp hs
      have hbmem : b ∈ a :: l' := (hperm.mem_iff).mpr (by simp)
      have hab : a ≤ b := by
        rcases List.mem_cons.mp hbmem with h | h
        · omega
        · have := hsplit.1 b h; omega
      by_cases hb : a = b
      · subst hb
        have hperm' : List.Perm l' m' := (List.perm_cons a).mp hperm
        exact (List.cons_le_cons_iff).mpr (Or.inr ⟨rfl, ih hsplit.2 hperm'⟩)
      · exact (List.cons_le_cons_iff).mpr (Or.inl (by omega))

theorem desc_ge : ∀ {l m : List Int},
    List.Pairwise (· ≥ ·) l → List.Perm l m → m ≤ l := by
  intro l
  induction l with
  | nil =>
    intro m _ hperm
    have : m = [] := by simpa using hperm.symm
    subst this; exact List.le_refl []
  | cons a l' ih =>
    intro m hs hperm
    cases m with
    | nil => simp at hperm
    | cons b m' =>
      have hsplit := List.pairwise_cons.mp hs
      have hbmem : b ∈ a :: l' := (hperm.mem_iff).mpr (by simp)
      have hba : b ≤ a := by
        rcases List.mem_cons.mp hbmem with h | h
        · omega
        · have := hsplit.1 b h; omega
      by_cases hb : a = b
      · subst hb
        have hperm' : List.Perm l' m' := (List.perm_cons a).mp hperm
        exact (List.cons_le_cons_iff).mpr (Or.inr ⟨rfl, ih hsplit.2 hperm'⟩)
      · exact (List.cons_le_cons_iff).mpr (Or.inl (by omega))

/- ── `findPivot none` ⇒ the list is weakly decreasing; hence the returned suffix
   is weakly decreasing. ── -/

theorem findPivot_none_decr : ∀ (xs : List Int),
    findPivot xs = none → List.Pairwise (· ≥ ·) xs := by
  intro xs
  induction xs with
  | nil => intro _; exact List.Pairwise.nil
  | cons x xs ih =>
    cases xs with
    | nil => intro _; simp [List.pairwise_cons]
    | cons y rest =>
      intro h
      simp only [findPivot] at h
      cases hf : findPivot (y :: rest) with
      | some res => obtain ⟨pre', p', suf'⟩ := res; rw [hf] at h; simp at h
      | none =>
        rw [hf] at h
        by_cases hxy : x < y
        · rw [if_pos hxy] at h; simp at h
        · have hrest : List.Pairwise (· ≥ ·) (y :: rest) := ih hf
          rw [List.pairwise_cons]
          refine ⟨?_, hrest⟩
          intro z hz
          rcases List.mem_cons.mp hz with hzy | hzr
          · subst hzy; omega
          · have hyz := (List.pairwise_cons.mp hrest).1 z hzr; omega

theorem findPivot_suf_decr : ∀ (xs pre suf : List Int) (p : Int),
    findPivot xs = some (pre, p, suf) → List.Pairwise (· ≥ ·) suf := by
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
        obtain ⟨_, _, hsuf⟩ := h
        subst hsuf
        exact ih pre' suf' p' hf
      | none =>
        rw [hf] at h
        by_cases hxy : x < y
        · rw [if_pos hxy] at h
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨_, _, hsuf⟩ := h
          subst hsuf
          exact findPivot_none_decr (y :: rest) hf
        · rw [if_neg hxy] at h; simp at h

/- ── `swapLast`: no element exceeds `p` when it fails; in a weakly-decreasing
   list the swapped-in `g` is the LEAST element exceeding `p`; and the rebuilt
   suffix stays weakly decreasing. ── -/

theorem swapLast_none_le : ∀ (p : Int) (l : List Int),
    swapLast p l = none → ∀ z ∈ l, z ≤ p := by
  intro p l
  induction l with
  | nil => intro _ z hz; simp at hz
  | cons c rest ih =>
    intro h z hz
    simp only [swapLast] at h
    cases hs : swapLast p rest with
    | some res => obtain ⟨g0, r0⟩ := res; rw [hs] at h; simp at h
    | none =>
      rw [hs] at h
      by_cases hpc : p < c
      · rw [if_pos hpc] at h; simp at h
      · rcases List.mem_cons.mp hz with hzc | hzr
        · subst hzc; omega
        · exact ih hs z hzr

theorem swapLast_least : ∀ (p : Int) (l : List Int) (g : Int) (l' : List Int),
    List.Pairwise (· ≥ ·) l → swapLast p l = some (g, l') →
    ∀ z ∈ l, p < z → g ≤ z := by
  intro p l
  induction l with
  | nil => intro g l' _ h; simp [swapLast] at h
  | cons c rest ih =>
    intro g l' hdec h
    simp only [swapLast] at h
    cases hs : swapLast p rest with
    | some res =>
      obtain ⟨g0, r0⟩ := res
      rw [hs] at h
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨hg, _⟩ := h
      subst hg
      have hdecrest := (List.pairwise_cons.mp hdec).2
      have hihrest := ih g0 r0 hdecrest hs
      have hgt := swapLast_gt p rest g0 r0 hs
      have hg0mem : g0 ∈ rest := by
        have hp := swapLast_perm p rest g0 r0 hs
        have hm : g0 ∈ p :: rest := (hp.mem_iff).mp (by simp)
        rcases List.mem_cons.mp hm with h0 | h0
        · omega
        · exact h0
      have hcg0 := (List.pairwise_cons.mp hdec).1 g0 hg0mem
      intro z hz hpz
      rcases List.mem_cons.mp hz with hzc | hzr
      · subst hzc; omega
      · exact hihrest z hzr hpz
    | none =>
      rw [hs] at h
      by_cases hpc : p < c
      · rw [if_pos hpc] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨hg, _⟩ := h
        subst hg
        have hnone := swapLast_none_le p rest hs
        intro z hz hpz
        rcases List.mem_cons.mp hz with hzc | hzr
        · subst hzc; omega
        · have := hnone z hzr; omega
      · rw [if_neg hpc] at h; simp at h

theorem swapLast_decr : ∀ (p : Int) (l : List Int) (g : Int) (l' : List Int),
    List.Pairwise (· ≥ ·) l → swapLast p l = some (g, l') →
    List.Pairwise (· ≥ ·) l' := by
  intro p l
  induction l with
  | nil => intro g l' _ h; simp [swapLast] at h
  | cons c rest ih =>
    intro g l' hdec h
    simp only [swapLast] at h
    cases hs : swapLast p rest with
    | some res =>
      obtain ⟨g0, r0⟩ := res
      rw [hs] at h
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨hg, hl⟩ := h
      subst hl
      have hdecrest := (List.pairwise_cons.mp hdec).2
      have hr0 := ih g0 r0 hdecrest hs
      have hp := swapLast_perm p rest g0 r0 hs
      rw [List.pairwise_cons]
      refine ⟨?_, hr0⟩
      intro z hz
      have hzmem : z ∈ p :: rest := (hp.mem_iff).mp (by simp [hz])
      rcases List.mem_cons.mp hzmem with hzp | hzr
      · have hgt := swapLast_gt p rest g0 r0 hs
        have hg0mem : g0 ∈ rest := by
          have hm : g0 ∈ p :: rest := (hp.mem_iff).mp (by simp)
          rcases List.mem_cons.mp hm with h0 | h0
          · omega
          · exact h0
        have hcg0 := (List.pairwise_cons.mp hdec).1 g0 hg0mem
        show c ≥ z
        omega
      · exact (List.pairwise_cons.mp hdec).1 z hzr
    | none =>
      rw [hs] at h
      by_cases hpc : p < c
      · rw [if_pos hpc] at h
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨hg, hl⟩ := h
        subst hl
        have hnone := swapLast_none_le p rest hs
        rw [List.pairwise_cons]
        refine ⟨?_, (List.pairwise_cons.mp hdec).2⟩
        intro z hz
        have := hnone z hz
        show p ≥ z
        omega
      · rw [if_neg hpc] at h; simp at h

/- ── The minimality core: by induction on the common prefix `pre`, no
   permutation of `pre ++ p :: suf` that is lex-above it can be lex-below
   `pre ++ g :: suf'.reverse`. ── -/

theorem nextPerm_least_core
    (p : Int) (suf : List Int) (g : Int) (suf' : List Int)
    (hdec : List.Pairwise (· ≥ ·) suf)
    (hlst : ∀ z ∈ suf, p < z → g ≤ z)
    (hsw : List.Perm (g :: suf') (p :: suf))
    (hasc : List.Pairwise (· ≤ ·) suf'.reverse) :
    ∀ (pre ys : List Int),
      List.Perm ys (pre ++ p :: suf) →
      List.Lex (· < ·) (pre ++ p :: suf) ys →
      pre ++ g :: suf'.reverse ≤ ys := by
  intro pre
  induction pre with
  | nil =>
    intro ys hperm hlt
    cases ys with
    | nil => exact absurd hlt List.not_lex_nil
    | cons b ys_tail =>
      simp only [List.nil_append] at hperm hlt ⊢
      rcases (List.cons_lex_cons_iff).mp hlt with hpb | ⟨hpb, hsuf_lt⟩
      · have hbmem : b ∈ p :: suf := (hperm.mem_iff).mp (by simp)
        have hbsuf : b ∈ suf := by
          rcases List.mem_cons.mp hbmem with h | h
          · omega
          · exact h
        have hgb : g ≤ b := hlst b hbsuf hpb
        by_cases hgbq : g = b
        · subst hgbq
          have h1 : List.Perm (g :: ys_tail) (g :: suf') := hperm.trans hsw.symm
          have h2 : List.Perm ys_tail suf' := (List.perm_cons g).mp h1
          have h3 : List.Perm suf'.reverse ys_tail :=
            (List.reverse_perm suf').trans h2.symm
          exact (List.cons_le_cons_iff).mpr (Or.inr ⟨rfl, asc_le hasc h3⟩)
        · exact (List.cons_le_cons_iff).mpr (Or.inl (by omega))
      · subst hpb
        have h1 : List.Perm ys_tail suf := (List.perm_cons p).mp hperm
        exact absurd hsuf_lt (desc_ge hdec h1.symm)
  | cons x pre' ih =>
    intro ys hperm hlt
    cases ys with
    | nil => exact absurd hlt List.not_lex_nil
    | cons b ys_tail =>
      simp only [List.cons_append] at hperm hlt ⊢
      rcases (List.cons_lex_cons_iff).mp hlt with hxb | ⟨hxb, htail_lt⟩
      · exact (List.cons_le_cons_iff).mpr (Or.inl hxb)
      · subst hxb
        have hpt : List.Perm ys_tail (pre' ++ p :: suf) := (List.perm_cons x).mp hperm
        exact (List.cons_le_cons_iff).mpr (Or.inr ⟨rfl, ih ys_tail hpt htail_lt⟩)

/- ── MAIN THEOREM 3 — minimality / betweenness: when a pivot exists, no
   permutation of the input lies strictly between it and `nextPerm`. ── -/

theorem nextPerm_least (xs : List Int) {pre p suf} (hf : findPivot xs = some (pre, p, suf))
    (ys : List Int) (hperm : ys.Perm xs) (h1 : List.Lex (· < ·) xs ys) :
    ¬ List.Lex (· < ·) ys (nextPerm xs) := by
  obtain ⟨c, suf0, hsuf, hpc⟩ := findPivot_lt xs pre suf p hf
  subst hsuf
  obtain ⟨g, suf', hs⟩ := swapLast_succeeds p c suf0 hpc
  have hdec : List.Pairwise (· ≥ ·) (c :: suf0) := findPivot_suf_decr xs pre (c :: suf0) p hf
  have hlst : ∀ z ∈ (c :: suf0), p < z → g ≤ z := swapLast_least p (c :: suf0) g suf' hdec hs
  have hsw : List.Perm (g :: suf') (p :: c :: suf0) := swapLast_perm p (c :: suf0) g suf' hs
  have hdec' : List.Pairwise (· ≥ ·) suf' := swapLast_decr p (c :: suf0) g suf' hdec hs
  have hasc : List.Pairwise (· ≤ ·) suf'.reverse := by
    rw [List.pairwise_reverse]; exact hdec'
  have heq : xs = pre ++ p :: c :: suf0 := findPivot_eq xs pre (c :: suf0) p hf
  have hval : nextPerm xs = pre ++ g :: suf'.reverse := by simp only [nextPerm, hf, hs]
  rw [List.not_lex_lt, hval]
  apply nextPerm_least_core p (c :: suf0) g suf' hdec hlst hsw hasc pre ys
  · rw [heq] at hperm; exact hperm
  · rw [heq] at h1; exact h1

#print axioms nextPerm_perm
#print axioms nextPerm_gt
#print axioms nextPerm_least
