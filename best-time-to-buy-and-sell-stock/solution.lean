/-
LeetCode 121: Best Time to Buy and Sell Stock — single pass tracking the lowest
price so far and the best profit, with a Lean 4 proof of OPTIMALITY:
`maxProfit prices` is *exactly* the best profit over all valid trades (buy on
some day, sell on a later-or-equal day).

  • `maxProfit_optimal`    — no trade beats the answer (`s - b ≤ maxProfit`);
  • `maxProfit_achievable` — the answer is realised by an actual trade.

A trade is the inductive `Trade prices b s` (buy `b`, sell `s` no earlier).
Lean 4, core library only.
-/

-- single pass: `lowest` = min price so far, `best` = best profit so far.
def go (lowest best : Int) : List Int → Int
  | []      => best
  | x :: xs => go (min lowest x) (max best (x - lowest)) xs

def maxProfit : List Int → Int
  | []      => 0
  | p :: ps => go p 0 ps

#guard maxProfit [7,1,5,3,6,4] == 5
#guard maxProfit [7,6,4,3,1]   == 0

-- A valid trade in `prices`: buy at `b`, sell at `s` on a later-or-equal day.
--   here  : buy at the head, sell at any element from the head onward;
--   there : the whole trade happens later in the list.
inductive Trade : List Int → Int → Int → Prop where
  | here  {b s rest} : s ∈ (b :: rest) → Trade (b :: rest) b s
  | there {x b s rest} : Trade rest b s → Trade (x :: rest) b s

-- a trade in `pre ++ [x]` either lies inside `pre`, or sells at `x`.
theorem trade_snoc {x b s : Int} : ∀ {pre : List Int},
    Trade (pre ++ [x]) b s → Trade pre b s ∨ (s = x ∧ (b ∈ pre ∨ b = x)) := by
  intro pre ht
  induction pre with
  | nil =>
      simp only [List.nil_append] at ht
      cases ht with
      | here hmem => rcases List.mem_singleton.mp hmem with rfl; exact Or.inr ⟨rfl, Or.inr rfl⟩
      | there ht' => cases ht'
  | cons p pre ih =>
      rw [List.cons_append] at ht
      cases ht with
      | here hmem =>
          rcases List.mem_cons.mp hmem with rfl | hmem'
          · exact Or.inl (.here (List.mem_cons_self ..))
          · rcases List.mem_append.mp hmem' with hpre | hx
            · exact Or.inl (.here (List.mem_cons_of_mem _ hpre))
            · rcases List.mem_singleton.mp hx with rfl
              exact Or.inr ⟨rfl, Or.inl (List.mem_cons_self ..)⟩
      | there ht' =>
          rcases ih ht' with h | ⟨rfl, hb⟩
          · exact Or.inl (.there h)
          · refine Or.inr ⟨rfl, ?_⟩
            rcases hb with hbpre | rfl
            · exact Or.inl (List.mem_cons_of_mem _ hbpre)
            · exact Or.inr rfl

-- `lowest` is the minimum of the (nonempty) consumed prefix.
def IsMin (lowest : Int) (pre : List Int) : Prop := lowest ∈ pre ∧ ∀ p ∈ pre, lowest ≤ p

/- ── OPTIMALITY: no trade beats `go`'s answer ── -/
theorem go_opt (xs : List Int) : ∀ pre lowest best,
    IsMin lowest pre → (∀ b s, Trade pre b s → s - b ≤ best) → 0 ≤ best →
    ∀ b s, Trade (pre ++ xs) b s → s - b ≤ go lowest best xs := by
  induction xs with
  | nil =>
      intro pre lowest best _ hub _ b s ht
      simp only [List.append_nil] at ht
      exact hub b s ht
  | cons x xs ih =>
      intro pre lowest best hmin hub hbest b s ht
      simp only [go]
      apply ih (pre ++ [x]) (min lowest x) (max best (x - lowest))
      · obtain ⟨hmem, hle⟩ := hmin
        refine ⟨?_, ?_⟩
        · rcases Int.le_total lowest x with h | h
          · have hm : min lowest x = lowest := by omega
            rw [hm]; exact List.mem_append_left _ hmem
          · have hm : min lowest x = x := by omega
            rw [hm]; exact List.mem_append_right _ (List.mem_singleton.mpr rfl)
        · intro p hp
          rcases List.mem_append.mp hp with hppre | hpx
          · have := hle p hppre; omega
          · rcases List.mem_singleton.mp hpx with rfl; omega
      · intro b' s' ht'
        rcases trade_snoc ht' with h | ⟨rfl, hb⟩
        · have := hub b' s' h; omega
        · rcases hb with hbpre | rfl
          · have hlb : lowest ≤ b' := hmin.2 b' hbpre; omega
          · omega
      · omega
      · rw [List.append_assoc]; exact ht

theorem maxProfit_optimal (prices : List Int) (b s : Int) (ht : Trade prices b s) :
    s - b ≤ maxProfit prices := by
  cases prices with
  | nil => cases ht
  | cons p ps =>
      simp only [maxProfit]
      have hmin : IsMin p [p] :=
        ⟨List.mem_singleton.mpr rfl, by intro q hq; rcases List.mem_singleton.mp hq with rfl; omega⟩
      have hub : ∀ b' s', Trade [p] b' s' → s' - b' ≤ 0 := by
        intro b' s' ht'
        cases ht' with
        | here hmem => rcases List.mem_singleton.mp hmem with rfl; omega
        | there ht'' => cases ht''
      exact go_opt ps [p] p 0 hmin hub (by omega) b s ht

/- ── ACHIEVABILITY: the answer is realised by an actual trade ── -/
-- a trade survives appending more days on the right
theorem trade_append {pre : List Int} {b s : Int} (ys : List Int) (ht : Trade pre b s) :
    Trade (pre ++ ys) b s := by
  induction ht with
  | here hmem =>
      apply Trade.here
      rcases List.mem_cons.mp hmem with rfl | h
      · exact List.mem_cons_self ..
      · exact List.mem_cons_of_mem _ (List.mem_append_left _ h)
  | there _ ih => exact .there ih

-- buying at any earlier day `b` and selling at the appended day `x` is a trade
theorem trade_mem_snoc {b x : Int} : ∀ {pre : List Int}, b ∈ pre → Trade (pre ++ [x]) b x := by
  intro pre hb
  induction pre with
  | nil => cases hb
  | cons p pre ih =>
      rcases List.mem_cons.mp hb with rfl | hb'
      · exact .here (List.mem_cons_of_mem _ (List.mem_append_right _ (List.mem_singleton.mpr rfl)))
      · exact .there (ih hb')

theorem go_ach (xs : List Int) : ∀ pre lowest best,
    IsMin lowest pre → (∃ b s, Trade pre b s ∧ s - b = best) →
    ∃ b s, Trade (pre ++ xs) b s ∧ s - b = go lowest best xs := by
  induction xs with
  | nil => intro pre lowest best _ hreal; simpa only [go, List.append_nil] using hreal
  | cons x xs ih =>
      intro pre lowest best hmin hreal
      simp only [go]
      have hmin' : IsMin (min lowest x) (pre ++ [x]) := by
        obtain ⟨hmem, hle⟩ := hmin
        refine ⟨?_, ?_⟩
        · rcases Int.le_total lowest x with h | h
          · have hm : min lowest x = lowest := by omega
            rw [hm]; exact List.mem_append_left _ hmem
          · have hm : min lowest x = x := by omega
            rw [hm]; exact List.mem_append_right _ (List.mem_singleton.mpr rfl)
        · intro q hq
          rcases List.mem_append.mp hq with hqpre | hqx
          · have := hle q hqpre; omega
          · rcases List.mem_singleton.mp hqx with rfl; omega
      have hreal' : ∃ b s, Trade (pre ++ [x]) b s ∧ s - b = max best (x - lowest) := by
        rcases Int.le_total (x - lowest) best with h | h
        · obtain ⟨b, s, ht, he⟩ := hreal
          exact ⟨b, s, trade_append [x] ht, by omega⟩
        · exact ⟨lowest, x, trade_mem_snoc hmin.1, by omega⟩
      have key := ih (pre ++ [x]) (min lowest x) (max best (x - lowest)) hmin' hreal'
      rw [List.append_assoc] at key
      exact key

theorem maxProfit_achievable (p : Int) (ps : List Int) :
    ∃ b s, Trade (p :: ps) b s ∧ s - b = maxProfit (p :: ps) := by
  simp only [maxProfit]
  have hmin : IsMin p [p] :=
    ⟨List.mem_singleton.mpr rfl, by intro q hq; rcases List.mem_singleton.mp hq with rfl; omega⟩
  have hreal : ∃ b s, Trade [p] b s ∧ s - b = 0 :=
    ⟨p, p, .here (List.mem_singleton.mpr rfl), by omega⟩
  exact go_ach ps [p] p 0 hmin hreal

/- ── `maxProfit` is EXACTLY the best achievable trade profit ── -/
theorem maxProfit_correct (p : Int) (ps : List Int) :
    (∃ b s, Trade (p :: ps) b s ∧ s - b = maxProfit (p :: ps)) ∧
    (∀ b s, Trade (p :: ps) b s → s - b ≤ maxProfit (p :: ps)) :=
  ⟨maxProfit_achievable p ps, fun b s => maxProfit_optimal (p :: ps) b s⟩
