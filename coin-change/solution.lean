/-
LeetCode 322: Coin Change — fewest coins (with repetition) summing to `amount`,
or `none` when impossible.  Lean 4 centerpiece: a WELL-FOUNDED top-down
recurrence on `amount` together with a full correctness proof against a
declarative spec.

The shipped Racket/Haskell solutions use the bottom-up DP table (fast); the
PROVEN artifact here is the unmemoized recurrence (same values, exponential):
for each coin `c` with `1 ≤ c ≤ amount` we recurse on the strictly smaller
`amount - c` and take the best (`listMin`) of `1 + (answer for amount - c)`.

Spec.  A *representation* of `amount` is a multiset of chosen coins:
  `Rep coins amount xs := (∀ c ∈ xs, c ∈ coins) ∧ xs.sum = amount`.
We prove the recurrence is exactly the optimum:

  • `coinChange_sound`     — if the answer is `some k`, some representation has
                             length `k` (the answer is *achievable*);
  • `coinChange_lb`        — every representation has length ≥ the answer
                             (the answer is a *lower bound* / optimal);
  • `coinChange_none_iff`  — the answer is `none` ↔ no representation exists.

Core library only (no Mathlib / Lake).  Axiom-clean: see `#print axioms`.
-/

/- ── "min over a list of `Option Nat`", with `none` = unreachable ───────────── -/

-- combine two answers, taking the smaller present value; `none` is identity.
def omin : Option Nat → Option Nat → Option Nat
  | none,   y      => y
  | x,      none   => x
  | some a, some b => some (min a b)

def listMin : List (Option Nat) → Option Nat
  | []      => none
  | x :: xs => omin x (listMin xs)

-- the min value, when present, is achieved by some element (argmin).
theorem listMin_mem {L : List (Option Nat)} {k : Nat}
    (h : listMin L = some k) : some k ∈ L := by
  induction L with
  | nil => simp [listMin] at h
  | cons x xs ih =>
      simp only [listMin] at h
      cases x with
      | none => simp only [omin] at h; exact List.mem_cons_of_mem _ (ih h)
      | some a =>
          cases hxs : listMin xs with
          | none =>
              rw [hxs] at h; simp only [omin] at h
              rw [h]; exact List.mem_cons_self
          | some b =>
              rw [hxs] at h; simp only [omin, Option.some.injEq] at h
              rcases Nat.le_total a b with hab | hab
              · have : min a b = a := Nat.min_eq_left hab
                rw [this] at h; subst h; exact List.mem_cons_self
              · have : min a b = b := Nat.min_eq_right hab
                rw [this] at h; subst h
                exact List.mem_cons_of_mem _ (ih hxs)

-- the min is a lower bound for every present element (monotone).
theorem listMin_le {L : List (Option Nat)} {v : Nat}
    (h : some v ∈ L) : ∃ m, listMin L = some m ∧ m ≤ v := by
  induction L with
  | nil => simp at h
  | cons x xs ih =>
      simp only [listMin]
      rcases List.mem_cons.mp h with hx | hxs
      · subst hx
        cases hxs : listMin xs with
        | none => exact ⟨v, by simp [omin], Nat.le_refl _⟩
        | some b => exact ⟨min v b, by simp [omin], Nat.min_le_left _ _⟩
      · obtain ⟨m, hm, hmv⟩ := ih hxs
        cases x with
        | none => exact ⟨m, by simp [omin, hm], hmv⟩
        | some a =>
            exact ⟨min a m, by simp [omin, hm],
              Nat.le_trans (Nat.min_le_right _ _) hmv⟩

-- `listMin` is `none` exactly when every element is `none`.
theorem listMin_none {L : List (Option Nat)} :
    listMin L = none ↔ ∀ x ∈ L, x = none := by
  induction L with
  | nil => simp [listMin]
  | cons x xs ih =>
      simp only [listMin]
      constructor
      · intro h y hy
        cases x with
        | none =>
            simp only [omin] at h
            rcases List.mem_cons.mp hy with rfl | hys
            · rfl
            · exact (ih.mp h) y hys
        | some a =>
            cases hxs : listMin xs with
            | none => rw [hxs] at h; simp [omin] at h
            | some b => rw [hxs] at h; simp [omin] at h
      · intro h
        have hx : x = none := h x (List.mem_cons_self)
        subst hx
        simp only [omin]
        exact ih.mpr (fun y hy => h y (List.mem_cons_of_mem _ hy))

/- ── the recurrence ─────────────────────────────────────────────────────────── -/

def coinChange (coins : List Nat) : Nat → Option Nat
  | 0       => some 0
  | amt + 1 =>
      listMin (coins.map (fun c =>
        if h : 0 < c ∧ c ≤ amt + 1 then
          (coinChange coins (amt + 1 - c)).map (· + 1)
        else none))
  termination_by amt => amt
  decreasing_by exact Nat.sub_lt (by omega) h.1

#guard coinChange [1,2,5] 11 == some 3
#guard coinChange [2] 3 == none
#guard coinChange [1,2,5] 0 == some 0
#guard coinChange [1] 0 == some 0
#guard coinChange [1] 2 == some 2
#guard coinChange [2,5,10,1] 27 == some 4

-- equational lemmas of the WF def (definitional unfolding is blocked by `fix`).
theorem coinChange_zero (coins : List Nat) : coinChange coins 0 = some 0 := by
  rw [coinChange]

theorem coinChange_succ (coins : List Nat) (amt : Nat) :
    coinChange coins (amt + 1) =
      listMin (coins.map (fun c =>
        if _h : 0 < c ∧ c ≤ amt + 1 then
          (coinChange coins (amt + 1 - c)).map (· + 1)
        else none)) := by
  rw [coinChange]

/- ── the declarative spec: a multiset of coins summing to `amount` ──────────── -/

def Rep (coins : List Nat) (amount : Nat) (xs : List Nat) : Prop :=
  (∀ c ∈ xs, c ∈ coins) ∧ xs.sum = amount

-- head-decomposition: any representation of a POSITIVE amount contains a
-- positive coin `c ∈ coins`; removing it leaves a representation of `amount - c`
-- with one fewer chosen coin.
theorem rep_head {coins : List Nat} : ∀ (xs : List Nat) (amount : Nat),
    0 < amount → (∀ c ∈ xs, c ∈ coins) → xs.sum = amount →
    ∃ c ys, 0 < c ∧ c ≤ amount ∧ c ∈ coins ∧
      Rep coins (amount - c) ys ∧ ys.length + 1 = xs.length := by
  intro xs
  induction xs with
  | nil => intro amount hpos _ hsum; simp only [List.sum_nil] at hsum; omega
  | cons a as ih =>
      intro amount hpos hmem hsum
      simp only [List.sum_cons] at hsum
      by_cases ha : 0 < a
      · exact ⟨a, as, ha, by omega, hmem a (List.mem_cons_self),
          ⟨fun c hc => hmem c (List.mem_cons_of_mem _ hc), by omega⟩, rfl⟩
      · have ha0 : a = 0 := by omega
        subst ha0
        simp only [Nat.zero_add] at hsum
        obtain ⟨c, ys, hcpos, hcle, hcmem, ⟨hys, hyssum⟩, hlen⟩ :=
          ih amount hpos (fun c hc => hmem c (List.mem_cons_of_mem _ hc)) hsum
        refine ⟨c, 0 :: ys, hcpos, hcle, hcmem, ⟨?_, ?_⟩, ?_⟩
        · intro d hd
          rcases List.mem_cons.mp hd with rfl | hd'
          · exact hmem 0 (List.mem_cons_self)
          · exact hys d hd'
        · simp only [List.sum_cons, Nat.zero_add]; exact hyssum
        · simp only [List.length_cons] at hlen ⊢; omega

/- ── SOUNDNESS: a `some k` answer is realised by a length-`k` representation ──── -/

theorem coinChange_sound (coins : List Nat) (amount : Nat) :
    ∀ k, coinChange coins amount = some k → ∃ xs, Rep coins amount xs ∧ xs.length = k := by
  induction amount using coinChange.induct with
  | case1 =>
      intro k hk
      rw [coinChange_zero] at hk
      have : k = 0 := by simp only [Option.some.injEq] at hk; omega
      subst this
      exact ⟨[], ⟨by intro c hc; simp at hc, rfl⟩, rfl⟩
  | case2 amt ih =>
      intro k hk
      rw [coinChange_succ] at hk
      have hmem := listMin_mem hk
      rw [List.mem_map] at hmem
      obtain ⟨c, hcmem, hfc⟩ := hmem
      by_cases hcond : 0 < c ∧ c ≤ amt + 1
      · rw [dif_pos hcond] at hfc
        cases hcc : coinChange coins (amt + 1 - c) with
        | none => rw [hcc] at hfc; simp at hfc
        | some k₀ =>
            rw [hcc] at hfc
            simp only [Option.map_some, Option.some.injEq] at hfc
            obtain ⟨xs', hrep', hlen'⟩ := ih c hcond k₀ hcc
            have hcle := hcond.2
            refine ⟨c :: xs', ⟨?_, ?_⟩, ?_⟩
            · intro d hd
              rcases List.mem_cons.mp hd with rfl | hd'
              · exact hcmem
              · exact hrep'.1 d hd'
            · simp only [List.sum_cons]; rw [hrep'.2]; omega
            · simp only [List.length_cons]; rw [hlen']; omega
      · rw [dif_neg hcond] at hfc; simp at hfc

/- ── LOWER BOUND / OPTIMALITY: no representation beats the answer ────────────── -/

theorem coinChange_lb (coins : List Nat) (amount : Nat) :
    ∀ xs, Rep coins amount xs → ∃ k, coinChange coins amount = some k ∧ k ≤ xs.length := by
  induction amount using coinChange.induct with
  | case1 => intro xs _; exact ⟨0, coinChange_zero coins, Nat.zero_le _⟩
  | case2 amt ih =>
      intro xs hrep
      have hpos : 0 < amt + 1 := Nat.succ_pos _
      obtain ⟨c, ys, hcpos, hcle, hcmem, hrepys, hlen⟩ :=
        rep_head xs (amt + 1) hpos hrep.1 hrep.2
      obtain ⟨k', hk', hk'le⟩ := ih c ⟨hcpos, hcle⟩ ys hrepys
      have hcond : 0 < c ∧ c ≤ amt + 1 := ⟨hcpos, hcle⟩
      have hmemcand : some (k' + 1) ∈ coins.map (fun c =>
          if h : 0 < c ∧ c ≤ amt + 1 then
            (coinChange coins (amt + 1 - c)).map (· + 1) else none) := by
        rw [List.mem_map]
        exact ⟨c, hcmem, by rw [dif_pos hcond, hk']; rfl⟩
      obtain ⟨m, hm, hmle⟩ := listMin_le hmemcand
      refine ⟨m, ?_, ?_⟩
      · rw [coinChange_succ]; exact hm
      · have : k' + 1 ≤ xs.length := by omega
        exact Nat.le_trans hmle this

/- ── EXISTENCE: `none` ⇔ there is no representation at all ────────────────────── -/

theorem coinChange_none_iff (coins : List Nat) (amount : Nat) :
    coinChange coins amount = none ↔ ¬ ∃ xs, Rep coins amount xs := by
  constructor
  · intro h
    rintro ⟨xs, hrep⟩
    obtain ⟨k, hk, _⟩ := coinChange_lb coins amount xs hrep
    rw [h] at hk; simp at hk
  · intro h
    cases hc : coinChange coins amount with
    | none => rfl
    | some k =>
        obtain ⟨xs, hrep, _⟩ := coinChange_sound coins amount k hc
        exact absurd ⟨xs, hrep⟩ h

/- ── corollary: `coinChange` is EXACTLY the optimum over all representations ──── -/

theorem coinChange_correct (coins : List Nat) (amount : Nat) (k : Nat)
    (hk : coinChange coins amount = some k) :
    (∃ xs, Rep coins amount xs ∧ xs.length = k) ∧
    (∀ xs, Rep coins amount xs → k ≤ xs.length) := by
  refine ⟨coinChange_sound coins amount k hk, ?_⟩
  intro xs hrep
  obtain ⟨k', hk', hk'le⟩ := coinChange_lb coins amount xs hrep
  rw [hk] at hk'
  simp only [Option.some.injEq] at hk'
  omega

#print axioms coinChange_sound
#print axioms coinChange_lb
#print axioms coinChange_none_iff
#print axioms coinChange_correct
