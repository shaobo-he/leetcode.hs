set_option linter.unusedVariables false
set_option linter.unusedSectionVars false
variable {α : Type} [DecidableEq α]

def step : Option α × Nat → α → Option α × Nat
  | (_,      0),   x => (some x, 1)
  | (some c, k+1), x => if x == c then (some c, k+1+1) else (some c, k)
  | (none,   k+1), x => (some x, 1)

def bm (xs : List α) : Option α × Nat := xs.foldl step (none, 0)

theorem snocRec {motive : List α → Prop} (hnil : motive [])
    (hsnoc : ∀ ys y, motive ys → motive (ys ++ [y])) : ∀ xs, motive xs := by
  have key : ∀ rs : List α, motive rs.reverse := by
    intro rs; induction rs with
    | nil => exact hnil
    | cons r rs ih => rw [List.reverse_cons]; exact hsnoc rs.reverse r ih
  intro xs; have := key xs.reverse; rwa [List.reverse_reverse] at this

def Good (xs : List α) : Option α × Nat → Prop
  | (none,   _) => xs = []
  | (some c, k) => (∀ v, v ≠ c → 2 * xs.count v + k ≤ xs.length)
                 ∧ 2 * xs.count c ≤ xs.length + k

theorem bm_snoc (xs : List α) (x : α) : bm (xs ++ [x]) = step (bm xs) x := by
  unfold bm; rw [List.foldl_append, List.foldl_cons, List.foldl_nil]

theorem len_snoc (xs : List α) (x : α) : (xs ++ [x]).length = xs.length + 1 := by
  simp

theorem count_snoc_self (x : α) (xs : List α) : (xs ++ [x]).count x = xs.count x + 1 := by
  rw [List.count_append, List.count_singleton]; simp

theorem count_snoc_of_ne {a x : α} (h : a ≠ x) (xs : List α) :
    (xs ++ [x]).count a = xs.count a := by
  rw [List.count_append, List.count_singleton]
  have : (x == a) = false := by simp [beq_eq_false_iff_ne, Ne.symm h]
  rw [this]; simp

theorem bm_good (xs : List α) : Good xs (bm xs) := by
  induction xs using snocRec with
  | hnil => simp [bm, Good]
  | hsnoc ys y ih =>
    rw [bm_snoc]
    match hst : bm ys, ih with
    | (none, k), ih =>
      subst ih
      cases k <;>
      · refine ⟨fun v hv => ?_, ?_⟩
        · rw [count_snoc_of_ne hv, len_snoc]; simp only [List.count_nil, List.length_nil]; omega
        · rw [count_snoc_self, len_snoc]; simp only [List.count_nil, List.length_nil]; omega
    | (some c, k), ih =>
      obtain ⟨H1, H2⟩ := ih
      cases k with
      | zero =>            -- state (some c, 0); candidate replaced by y, new (some y, 1)
        refine ⟨fun v hv => ?_, ?_⟩
        · rw [count_snoc_of_ne hv, len_snoc]
          by_cases hvc : v = c
          · subst hvc; have := H2; omega
          · have := H1 v hvc; omega
        · rw [count_snoc_self, len_snoc]
          by_cases hyc : y = c
          · subst hyc; have := H2; omega
          · have := H1 y hyc; omega
      | succ k =>
        simp only [step]
        by_cases hyc : y == c
        · have hyc' : y = c := by simpa using hyc
          subst hyc'; simp only [hyc, if_true]
          refine ⟨fun v hv => ?_, ?_⟩
          · rw [count_snoc_of_ne hv, len_snoc]; have := H1 v hv; omega
          · rw [count_snoc_self, len_snoc]; omega
        · have hyc' : y ≠ c := by simpa using hyc
          simp only [hyc]
          refine ⟨fun v hv => ?_, ?_⟩
          · rw [len_snoc]
            by_cases hvy : v = y
            · subst hvy; rw [count_snoc_self]; have := H1 v hv; omega
            · rw [count_snoc_of_ne hvy]; have := H1 v hv; omega
          · rw [count_snoc_of_ne (Ne.symm hyc'), len_snoc]; omega

-- ── Downstream corollaries ──────────────────────────────────────────────

def majorityElement (xs : List α) : Option α := (bm xs).1

-- (1) If ANY strict majority exists, Boyer–Moore's candidate IS it.
theorem bm_finds_majority (xs : List α) (m : α)
    (hm : 2 * xs.count m > xs.length) :
    majorityElement xs = some m := by
  have hg := bm_good xs
  unfold majorityElement
  -- xs is nonempty, so the candidate is `some c`; show c = m
  match hbm : bm xs, hg with
  | (none, k), hg =>
    -- Good xs (none,k) means xs = [] ; contradicts hm
    rw [hg] at hm; simp at hm
  | (some c, k), hg =>
    obtain ⟨H1, H2⟩ := hg
    by_cases hcm : m = c
    · subst hcm; rfl
    · exfalso; have := H1 m hcm; omega

-- (2) The proposed iff: the candidate is a strict majority  ↔  some value is.
theorem majority_iff (xs : List α) :
    (∃ c, majorityElement xs = some c ∧ 2 * xs.count c > xs.length)
      ↔ (∃ m, 2 * xs.count m > xs.length) := by
  constructor
  · rintro ⟨c, _, hc⟩; exact ⟨c, hc⟩
  · rintro ⟨m, hm⟩
    exact ⟨m, bm_finds_majority xs m hm, hm⟩

-- (3) At most one strict majority exists.
theorem count_two_le (a b : α) (xs : List α) (h : a ≠ b) :
    xs.count a + xs.count b ≤ xs.length := by
  induction xs with
  | nil => simp
  | cons x t ih =>
    rw [List.count_cons, List.count_cons, List.length_cons]
    by_cases hxa : x == a <;> by_cases hxb : x == b <;>
      simp_all <;> omega

theorem majority_unique (xs : List α) (a b : α)
    (ha : 2 * xs.count a > xs.length) (hb : 2 * xs.count b > xs.length) :
    a = b := by
  by_cases hne : a = b
  · exact hne
  · have := count_two_le a b xs hne; omega

#guard majorityElement [3,2,3]                 == some 3
#guard majorityElement [2,2,1,1,1,2,2]         == some 2
#guard majorityElement ([] : List Int)         == none

#print axioms bm_finds_majority
#print axioms majority_iff
#print axioms majority_unique
