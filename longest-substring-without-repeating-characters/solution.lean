-- Longest substring without repeating characters.
-- Sliding window: track the last-seen index per character in a hash map
-- (`Std.HashMap`, O(1) expected lookup/insert — the genuine O(n) algorithm,
-- matching the Haskell version's `Data.Map`, not the O(n·σ) assoc list);
-- when a repeat is seen, jump the window start past its previous position.
import Std.Data.HashMap
open Std

-- Last-seen index per char, in a real hash map.  `setIdx`/`lookupIdx` are thin
-- wrappers so the entire optimality proof below — which reaches the store only
-- through these two names plus the `lookupIdx_setIdx` functional-update law — is
-- reused verbatim; only that one law is re-proven, from the HashMap library.
abbrev Store := HashMap Char Nat

def setIdx (c : Char) (i : Nat) (m : Store) : Store := m.insert c i

def lookupIdx (c : Char) (m : Store) : Option Nat := m[c]?

def goFast (i start : Nat) (seen : Store) (best : Nat) : List Char → Nat
  | []      => best
  | c :: cs =>
    let start' := match lookupIdx c seen with
                  | some j => max start (j + 1)
                  | none   => start
    goFast (i + 1) start' (setIdx c i seen) (max best ((i + 1) - start')) cs

def lengthOfLongest (s : String) : Nat := goFast 0 0 ∅ 0 s.toList

#guard lengthOfLongest "abcabcbb" == 3
#guard lengthOfLongest "bbbbb" == 1
#guard lengthOfLongest "pwwkew" == 3

/- ════════════════════════════════════════════════════════════════════════
   FULLY VERIFIED VARIANT.

   The window above (last-seen-index, O(n)) is the fast runnable solution. Below
   is a structurally-shrinking variant `lengthOfLongestV`, for which we prove
   FULL OPTIMALITY: its answer is *exactly* the maximum length over all
   substrings whose characters are all distinct (no repeats).

   Same machinery as the k-distinct proof, with validity = "all characters
   distinct" (`Distinct cs := nd cs = cs.length`, i.e. the distinct count
   equals the length). `#print axioms` confirms no `sorry`.
   ════════════════════════════════════════════════════════════════════════ -/

/- distinct-character count, via decidable Char equality -/
def decElem (x : Char) : List Char → Bool
  | []      => false
  | y :: ys => (x == y) || decElem x ys

def nd : List Char → Nat
  | []      => 0
  | x :: xs => if decElem x xs then nd xs else nd xs + 1

theorem decElem_snoc (x c : Char) (xs : List Char) :
    decElem x (xs ++ [c]) = (decElem x xs || (x == c)) := by
  induction xs with
  | nil => simp [decElem]
  | cons y ys ih => simp [decElem, ih, Bool.or_assoc]

theorem beq_symm_char (a b : Char) : (a == b) = (b == a) := by
  by_cases h : a = b
  · subst h; rfl
  · rw [beq_eq_false_iff_ne.mpr h, beq_eq_false_iff_ne.mpr (Ne.symm h)]

-- appending one char changes the distinct count by exactly 0 or 1
theorem nd_snoc (c : Char) (xs : List Char) :
    nd (xs ++ [c]) = if decElem c xs then nd xs else nd xs + 1 := by
  induction xs with
  | nil => simp [nd, decElem]
  | cons x xs ih =>
      simp only [List.cons_append, nd, decElem_snoc, decElem, ih]
      rw [beq_symm_char c x]
      rcases Bool.eq_false_or_eq_true (decElem x xs) with h1 | h1 <;>
      rcases Bool.eq_false_or_eq_true (x == c) with h2 | h2 <;>
      rcases Bool.eq_false_or_eq_true (decElem c xs) with h3 | h3 <;>
        simp [h1, h2, h3] <;> (obtain rfl := eq_of_beq h2; simp_all)

theorem nd_mono (xs : List Char) (c : Char) : nd xs ≤ nd (xs ++ [c]) := by
  rw [nd_snoc]; by_cases h : decElem c xs = true <;> simp [h]

-- the distinct count never exceeds the length
theorem nd_le_length (cs : List Char) : nd cs ≤ cs.length := by
  induction cs with
  | nil => simp [nd]
  | cons x xs ih => simp only [nd, List.length_cons]; split <;> omega

-- "all characters distinct": the distinct count equals the length
def Distinct (cs : List Char) : Prop := nd cs = cs.length

theorem distinct_nil : Distinct [] := rfl

-- a prefix of an all-distinct list is all-distinct
theorem distinct_init (xs : List Char) (c : Char) (h : Distinct (xs ++ [c])) : Distinct xs := by
  simp only [Distinct, nd_snoc, List.length_append, List.length_cons, List.length_nil] at h
  have hle := nd_le_length xs
  simp only [Distinct]
  by_cases hc : decElem c xs = true <;> simp [hc] at h <;> omega

/- shrinkV: drop chars from the FRONT until the window is all-distinct.
   = the longest all-distinct suffix of the window. -/
def shrinkV : List Char → List Char
  | []      => []
  | x :: xs => if nd (x :: xs) = (x :: xs).length then x :: xs else shrinkV xs

theorem shrinkV_suffix (w : List Char) : shrinkV w <:+ w := by
  induction w with
  | nil => simp [shrinkV]
  | cons x xs ih =>
      simp only [shrinkV]; split
      · exact List.suffix_rfl
      · exact ih.trans (List.suffix_cons x xs)

theorem shrinkV_valid (w : List Char) : Distinct (shrinkV w) := by
  induction w with
  | nil => simp [shrinkV, Distinct, nd]
  | cons x xs ih =>
      simp only [shrinkV]; split
      · assumption
      · exact ih

-- every all-distinct suffix of w is a suffix of shrinkV w
theorem shrinkV_longest (sf : List Char) (hv : Distinct sf) :
    ∀ w, sf <:+ w → sf <:+ shrinkV w := by
  intro w hsf
  induction w with
  | nil => rw [List.suffix_nil.mp hsf]; simp [shrinkV]
  | cons x xs ih =>
      simp only [shrinkV]; split
      · exact hsf
      · rename_i hgt
        have hne : sf ≠ x :: xs := fun h => hgt (h ▸ hv)
        exact ih ((List.suffix_cons_iff.mp hsf).resolve_left hne)

/- ── list helpers ── -/
theorem suffix_snoc_mono {sf w : List Char} (c : Char) (h : sf <:+ w) :
    sf ++ [c] <:+ w ++ [c] := by
  obtain ⟨t, ht⟩ := h
  exact ⟨t, by rw [← List.append_assoc, ht]⟩

theorem suffix_snoc_decomp {sf xs : List Char} {c : Char} (h : sf <:+ xs ++ [c]) :
    sf = [] ∨ ∃ sf0, sf = sf0 ++ [c] ∧ sf0 <:+ xs := by
  rcases List.eq_nil_or_concat sf with rfl | ⟨sf0, a, rfl⟩
  · exact Or.inl rfl
  · right
    rw [List.concat_eq_append] at h ⊢
    obtain ⟨t, ht⟩ := h
    rw [← List.append_assoc] at ht
    obtain ⟨h1, h2⟩ := List.append_inj' ht rfl
    obtain rfl : a = c := by injection h2
    exact ⟨sf0, rfl, ⟨t, h1⟩⟩

/- ── the structural sliding window ── -/
def goV : Nat → List Char → List Char → Nat
  | best, _win, []      => best
  | best, win,  c :: cs =>
      let w := shrinkV (win ++ [c])
      goV (max best w.length) w cs

def lengthOfLongestV (s : String) : Nat := goV 0 [] s.toList

-- the verified variant agrees with the fast one on the examples
#guard lengthOfLongestV "abcabcbb" == 3
#guard lengthOfLongestV "bbbbb"    == 1
#guard lengthOfLongestV "pwwkew"   == 3

theorem goV_mono : ∀ rem best win, best ≤ goV best win rem := by
  intro rem
  induction rem with
  | nil => intro best win; simp [goV]
  | cons c cs ih =>
      intro best win; simp only [goV]
      exact Nat.le_trans (Nat.le_max_left _ _) (ih _ _)

/- ── the window invariant: `win` is the longest all-distinct suffix of consumed `P` ── -/
def IsLVS (win P : List Char) : Prop :=
  win <:+ P ∧ Distinct win ∧ ∀ sf, sf <:+ P → Distinct sf → sf <:+ win

theorem lvs_step (win P : List Char) (c : Char) (h : IsLVS win P) :
    IsLVS (shrinkV (win ++ [c])) (P ++ [c]) := by
  obtain ⟨hwP, hwv, hall⟩ := h
  refine ⟨(shrinkV_suffix (win ++ [c])).trans (suffix_snoc_mono c hwP),
          shrinkV_valid (win ++ [c]), ?_⟩
  intro sf hsf hv
  rcases suffix_snoc_decomp hsf with rfl | ⟨sf0, rfl, hsf0⟩
  · exact List.nil_suffix
  · have h0v : Distinct sf0 := distinct_init sf0 c hv
    exact shrinkV_longest (sf0 ++ [c]) hv (win ++ [c]) (suffix_snoc_mono c (hall sf0 hsf0 h0v))

/- ── LOWER BOUND: the window's answer ≥ the length of every all-distinct substring ── -/
theorem goVOpt : ∀ rem win best P,
    IsLVS win P → win.length ≤ best →
    ∀ sub pre suf, rem = pre ++ suf → sub <:+ (P ++ pre) → Distinct sub →
      sub.length ≤ goV best win rem := by
  intro rem
  induction rem with
  | nil =>
      intro win best P hinv hwb sub pre suf hrem hsub hsv
      have hpre : pre = [] := (List.append_eq_nil_iff.mp hrem.symm).1
      subst hpre
      simp only [List.append_nil] at hsub
      obtain ⟨_, _, hall⟩ := hinv
      simp only [goV]
      exact Nat.le_trans (List.IsSuffix.length_le (hall sub hsub hsv)) hwb
  | cons c cs ih =>
      intro win best P hinv hwb sub pre suf hrem hsub hsv
      simp only [goV]
      have hinv' : IsLVS (shrinkV (win ++ [c])) (P ++ [c]) := lvs_step win P c hinv
      have hwb' : (shrinkV (win ++ [c])).length ≤ max best (shrinkV (win ++ [c])).length :=
        Nat.le_max_right _ _
      cases pre with
      | nil =>
          simp only [List.append_nil] at hsub
          obtain ⟨_, _, hall⟩ := hinv
          have hle : sub.length ≤ best :=
            Nat.le_trans (List.IsSuffix.length_le (hall sub hsub hsv)) hwb
          exact Nat.le_trans hle (Nat.le_trans (Nat.le_max_left _ _) (goV_mono cs _ _))
      | cons a pre' =>
          rw [List.cons_append] at hrem
          injection hrem with hac hcs
          subst hac
          rw [List.append_cons] at hsub
          exact ih (shrinkV (win ++ [c])) (max best (shrinkV (win ++ [c])).length) (P ++ [c])
                   hinv' hwb' sub pre' suf hcs hsub hsv

-- OPTIMALITY: every all-distinct substring is no longer than the window's answer.
theorem window_optimal (s : String) (sub : List Char)
    (hsub : sub <:+: s.toList) (hv : Distinct sub) :
    sub.length ≤ lengthOfLongestV s := by
  obtain ⟨before, after, hb⟩ := hsub
  have hinv : IsLVS [] [] :=
    ⟨List.nil_suffix, distinct_nil,
     fun sf hsf _ => by rw [List.suffix_nil.mp hsf]; exact List.nil_suffix⟩
  exact goVOpt s.toList [] 0 [] hinv (Nat.le_refl 0) sub (before ++ sub) after hb.symm
    (List.suffix_append before sub) hv

/- ── UPPER BOUND: the window's answer is realized by an actual all-distinct substring ── -/
theorem suffix_prefix_infix {a b full : List Char} (hs : a <:+ b) (hp : ∃ t, b ++ t = full) :
    a <:+: full := by
  obtain ⟨p, hp1⟩ := hs
  obtain ⟨t, hp2⟩ := hp
  exact ⟨p, t, by rw [← hp2, ← hp1]⟩

theorem goVReal (full : List Char) : ∀ rem win best P,
    P ++ rem = full → win <:+ P → Distinct win →
    (∃ sub, sub <:+: full ∧ Distinct sub ∧ sub.length = best) →
    ∃ sub, sub <:+: full ∧ Distinct sub ∧ sub.length = goV best win rem := by
  intro rem
  induction rem with
  | nil => intro win best P _ _ _ hreal; simpa [goV] using hreal
  | cons c cs ih =>
      intro win best P hPrem hwP _ hreal
      simp only [goV]
      have hP'full : (P ++ [c]) ++ cs = full := by rw [List.append_assoc]; exact hPrem
      have hwP' : shrinkV (win ++ [c]) <:+ P ++ [c] :=
        (shrinkV_suffix (win ++ [c])).trans (suffix_snoc_mono c hwP)
      have hwinf : shrinkV (win ++ [c]) <:+: full := suffix_prefix_infix hwP' ⟨cs, hP'full⟩
      have hreal' : ∃ sub, sub <:+: full ∧ Distinct sub ∧
          sub.length = max best (shrinkV (win ++ [c])).length := by
        rcases Nat.le_total (shrinkV (win ++ [c])).length best with hle | hge
        · have hm : max best (shrinkV (win ++ [c])).length = best := by omega
          rw [hm]; exact hreal
        · have hm : max best (shrinkV (win ++ [c])).length
              = (shrinkV (win ++ [c])).length := by omega
          rw [hm]; exact ⟨_, hwinf, shrinkV_valid (win ++ [c]), rfl⟩
      exact ih (shrinkV (win ++ [c])) (max best (shrinkV (win ++ [c])).length) (P ++ [c])
               hP'full hwP' (shrinkV_valid (win ++ [c])) hreal'

theorem window_realizable (s : String) :
    ∃ sub, sub <:+: s.toList ∧ Distinct sub ∧ sub.length = lengthOfLongestV s :=
  goVReal s.toList s.toList [] 0 [] (by simp) List.nil_suffix distinct_nil
    ⟨[], ⟨[], s.toList, by simp⟩, distinct_nil, rfl⟩

/- ── `lengthOfLongestV s` is EXACTLY the length of the longest all-distinct substring ── -/
theorem lengthOfLongestV_optimal (s : String) :
    (∃ sub, sub <:+: s.toList ∧ Distinct sub ∧ sub.length = lengthOfLongestV s) ∧
    (∀ sub, sub <:+: s.toList → Distinct sub → sub.length ≤ lengthOfLongestV s) :=
  ⟨window_realizable s, fun sub h hv => window_optimal s sub h hv⟩

#print axioms lengthOfLongestV_optimal

/- ════════════════════════════════════════════════════════════════════════
   EQUIVALENCE: the fast last-seen-index solution `lengthOfLongest` computes the
   same value as the verified shrink-window `lengthOfLongestV`.  So the
   optimality proof above transfers to the shipped algorithm.
   ════════════════════════════════════════════════════════════════════════ -/

-- index of the LAST occurrence of c in xs (positions from 0), as an Option
def lastIdx (c : Char) : List Char → Option Nat
  | []      => none
  | x :: xs => match lastIdx c xs with
               | some j => some (j + 1)
               | none   => if c == x then some 0 else none

-- setIdx then lookupIdx behaves like a functional update — now discharged from
-- the HashMap library's own `getElem?_insert` instead of by list induction.
theorem lookupIdx_setIdx (c d : Char) (i : Nat) (seen : Store) :
    lookupIdx d (setIdx c i seen) = if d == c then some i else lookupIdx d seen := by
  unfold lookupIdx setIdx
  rw [Std.HashMap.getElem?_insert]
  by_cases h : d = c
  · subst h; simp
  · have h' : ¬ c = d := fun e => h e.symm
    simp [h, h']

#check @lookupIdx_setIdx

-- lastIdx after appending: the new char becomes the last index; others unchanged
theorem lastIdx_snoc_self (c : Char) (P : List Char) :
    lastIdx c (P ++ [c]) = some P.length := by
  induction P with
  | nil => simp [lastIdx]
  | cons x xs ih => simp only [List.cons_append, lastIdx, ih, List.length_cons]

theorem lastIdx_snoc_other (c d : Char) (P : List Char) (h : (d == c) = false) :
    lastIdx d (P ++ [c]) = lastIdx d P := by
  induction P with
  | nil => simp [lastIdx, h]
  | cons x xs ih => simp only [List.cons_append, lastIdx, ih]

-- the longest distinct suffix is idempotent under "re-append and re-shrink"
theorem shrinkV_idem (P : List Char) (c : Char) :
    shrinkV (shrinkV P ++ [c]) = shrinkV (P ++ [c]) := by
  have h1 : shrinkV (shrinkV P ++ [c]) <:+ shrinkV (P ++ [c]) := by
    apply shrinkV_longest _ (shrinkV_valid _)
    exact (shrinkV_suffix _).trans (suffix_snoc_mono c (shrinkV_suffix P))
  have h2 : shrinkV (P ++ [c]) <:+ shrinkV (shrinkV P ++ [c]) := by
    apply shrinkV_longest _ (shrinkV_valid _)
    rcases suffix_snoc_decomp (shrinkV_suffix (P ++ [c])) with h | ⟨sf0, heq, hsf0⟩
    · rw [h]; exact List.nil_suffix
    · have hd : Distinct (shrinkV (P ++ [c])) := shrinkV_valid (P ++ [c])
      rw [heq] at hd ⊢
      exact suffix_snoc_mono c (shrinkV_longest sf0 (distinct_init sf0 c hd) P hsf0)
  exact h1.sublist.eq_of_length (Nat.le_antisymm h1.length_le h2.length_le)

#check @shrinkV_idem



/- ── crux lemmas connecting `lastIdx`/`seen` to the shrink window ── -/

theorem distinct_cons_iff (x : Char) (xs : List Char) :
    Distinct (x :: xs) ↔ (decElem x xs = false ∧ Distinct xs) := by
  have hle := nd_le_length xs
  simp only [Distinct, nd, List.length_cons]
  cases hd : decElem x xs <;> simp [hd] <;> omega

theorem distinct_snoc (c : Char) (xs : List Char) (hc : decElem c xs = false) (hd : Distinct xs) :
    Distinct (xs ++ [c]) := by
  have hn : nd (xs ++ [c]) = nd xs + 1 := by rw [nd_snoc]; simp [hc]
  simp only [Distinct] at hd ⊢
  rw [hn, List.length_append, List.length_cons, List.length_nil]; omega

theorem lastIdx_none_iff (c : Char) (xs : List Char) :
    lastIdx c xs = none ↔ decElem c xs = false := by
  induction xs with
  | nil => simp [lastIdx, decElem]
  | cons x xs ih =>
      cases hde : decElem c xs with
      | false =>
          have hl : lastIdx c xs = none := ih.mpr hde
          simp only [lastIdx, decElem, hl, hde, Bool.or_false]
          cases hcx : c == x <;> simp [hcx]
      | true =>
          have hl : lastIdx c xs ≠ none := fun h => by rw [ih.mp h] at hde; simp at hde
          cases hh : lastIdx c xs with
          | none   => exact absurd hh hl
          | some k => simp [lastIdx, decElem, hh, hde]

theorem lastIdx_cons (c x : Char) (xs : List Char) :
    lastIdx c (x :: xs) =
      (lastIdx c xs).elim (if c == x then some 0 else none) (fun j => some (j + 1)) := by
  simp only [lastIdx]; cases lastIdx c xs <;> simp [Option.elim]

theorem lastIdx_lt (c : Char) : ∀ (xs : List Char) (j : Nat), lastIdx c xs = some j → j < xs.length := by
  intro xs
  induction xs with
  | nil => intro j h; simp [lastIdx] at h
  | cons x xs ih =>
      intro j h
      rw [lastIdx_cons] at h
      cases hl : lastIdx c xs with
      | some k =>
          rw [hl] at h; simp only [Option.elim, Option.some.injEq] at h
          have := ih k hl; simp only [List.length_cons]; omega
      | none =>
          rw [hl] at h; simp only [Option.elim] at h
          cases hcx : c == x <;> simp [hcx] at h <;> simp only [List.length_cons] <;> omega

def dropAmt (win : List Char) (c : Char) : Nat :=
  (lastIdx c win).elim 0 (fun j => j + 1)


-- appending c to a distinct list keeps it distinct only if c is new
theorem distinct_snoc_decElem (c : Char) (xs : List Char) (h : Distinct (xs ++ [c])) :
    decElem c xs = false := by
  have hle := nd_le_length xs
  simp only [Distinct, nd_snoc, List.length_append, List.length_cons, List.length_nil] at h
  cases hc : decElem c xs with
  | false => rfl
  | true => simp [hc] at h; omega

-- THE CRUX: for a distinct window, appending c and re-shrinking = dropping past
-- c's last occurrence (or nothing, if c is new).
theorem shrinkV_snoc_drop (c : Char) : ∀ win, Distinct win →
    shrinkV (win ++ [c]) = (win ++ [c]).drop (dropAmt win c) := by
  intro win hwin
  induction win with
  | nil => simp [shrinkV, dropAmt, lastIdx, nd, decElem]
  | cons x w ih =>
      obtain ⟨hxw, hw⟩ := (distinct_cons_iff x w).mp hwin
      rw [List.cons_append, shrinkV]
      by_cases hcond : nd (x :: (w ++ [c])) = (x :: (w ++ [c])).length
      · rw [if_pos hcond]
        obtain ⟨hx2, hwc⟩ := (distinct_cons_iff x (w ++ [c])).mp hcond
        rw [decElem_snoc] at hx2
        have hcw : decElem c w = false := distinct_snoc_decElem c w hwc
        have hcwn : lastIdx c w = none := (lastIdx_none_iff c w).mpr hcw
        have hcx : (c == x) = false := by
          cases h : c == x with
          | false => rfl
          | true =>
              have hxc : (x == c) = true := by rw [beq_symm_char]; exact h
              simp [hxc] at hx2
        simp [dropAmt, lastIdx_cons, hcwn, hcx, Option.elim]
      · rw [if_neg hcond, ih hw]
        have hstep : dropAmt (x :: w) c = dropAmt w c + 1 := by
          simp only [dropAmt, lastIdx_cons]
          cases hl : lastIdx c w with
          | some k => simp [hl, Option.elim]
          | none =>
              have hcw : decElem c w = false := (lastIdx_none_iff c w).mp hl
              have hcx : (c == x) = true := by
                rcases Bool.eq_false_or_eq_true (c == x) with h | h
                · exact h
                · exfalso
                  apply hcond
                  have hd : Distinct (x :: (w ++ [c])) := by
                    rw [distinct_cons_iff]
                    refine ⟨?_, distinct_snoc c w hcw hw⟩
                    rw [decElem_snoc, hxw, Bool.false_or]
                    exact (beq_symm_char x c).trans h
                  exact hd
              simp [hl, hcx, Option.elim]
        rw [hstep]; rfl

#check @shrinkV_snoc_drop

/- ── the simulation: goFast tracks the same window as goV ── -/

-- `seen` correctly records each char's last index in the consumed prefix
def SeenOk (seen : Store) (P : List Char) : Prop :=
  ∀ d, lookupIdx d seen = lastIdx d P

theorem seenOk_step (seen : Store) (P : List Char) (c : Char) (i : Nat)
    (hi : i = P.length) (h : SeenOk seen P) : SeenOk (setIdx c i seen) (P ++ [c]) := by
  intro d
  rw [lookupIdx_setIdx]
  cases hdc : d == c with
  | true =>
      obtain rfl := beq_iff_eq.mp hdc
      simp only [hdc, if_true]
      rw [lastIdx_snoc_self, hi]
  | false =>
      simp only [hdc, Bool.false_eq_true, if_false]
      rw [h d, lastIdx_snoc_other c d P hdc]

theorem drop_append_le {α} {l₁ l₂ : List α} {n : Nat} (h : n ≤ l₁.length) :
    (l₁ ++ l₂).drop n = l₁.drop n ++ l₂ := by
  induction l₁ generalizing n with
  | nil => simp only [List.length_nil, Nat.le_zero_eq] at h; subst h; simp
  | cons a l₁ ih =>
      cases n with
      | zero => simp
      | succ m =>
          simp only [List.cons_append, List.drop_succ_cons]
          exact ih (by simp only [List.length_cons] at h; omega)

-- last occurrence index in an append
theorem lastIdx_append (c : Char) (xs ys : List Char) :
    lastIdx c (xs ++ ys) = (lastIdx c ys).elim (lastIdx c xs) (fun j => some (xs.length + j)) := by
  induction xs with
  | nil => cases h : lastIdx c ys <;> simp [lastIdx, h, Option.elim]
  | cons x xs ih =>
      rw [List.cons_append, lastIdx_cons, ih, lastIdx_cons, List.length_cons]
      cases h : lastIdx c ys with
      | none => simp [Option.elim]
      | some k => simp only [Option.elim]; congr 1; omega

-- the fast `start'` equals `start` plus how far the shrink drops
theorem start_conn (P : List Char) (start : Nat) (c : Char) (hstart : start ≤ P.length) :
    (match lastIdx c P with | some j => max start (j + 1) | none => start)
      = start + dropAmt (P.drop start) c := by
  have htake : (P.take start).length = start := by rw [List.length_take]; omega
  have key := lastIdx_append c (P.take start) (P.drop start)
  rw [List.take_append_drop, htake] at key
  rw [key, dropAmt]
  cases hd : lastIdx c (P.drop start) with
  | some k => simp only [hd, Option.elim]; rw [Nat.max_eq_right (by omega)]; omega
  | none =>
      simp only [hd, Option.elim]
      cases ht : lastIdx c (P.take start) with
      | some j =>
          have hj := htake ▸ lastIdx_lt c (P.take start) j ht
          simp only [ht, Option.elim]; rw [Nat.max_eq_left (by omega)]; omega
      | none => simp only [ht, Option.elim]; omega

-- the new window stays the longest distinct suffix, at index `start + dropAmt`
theorem window_step (P : List Char) (start : Nat) (c : Char) (hstart : start ≤ P.length)
    (hwin : shrinkV P = P.drop start) :
    shrinkV (P ++ [c]) = (P ++ [c]).drop (start + dropAmt (P.drop start) c) := by
  rw [← shrinkV_idem, hwin, shrinkV_snoc_drop c (P.drop start) (hwin ▸ shrinkV_valid P),
      ← List.drop_drop, drop_append_le hstart]

theorem goFast_eq_goV (rem : List Char) : ∀ P i start seen best,
    i = P.length → start ≤ P.length → shrinkV P = P.drop start → SeenOk seen P →
    goFast i start seen best rem = goV best (shrinkV P) rem := by
  induction rem with
  | nil => intro P i start seen best _ _ _ _; simp only [goFast, goV]
  | cons c cs ih =>
      intro P i start seen best hi hstart hwin hseen
      simp only [goFast, goV]
      rw [hseen c, shrinkV_idem, start_conn P start c hstart]
      have hwstep := window_step P start c hstart hwin
      have hstart' : start + dropAmt (P.drop start) c ≤ P.length := by
        rw [dropAmt]
        cases hd : lastIdx c (P.drop start) with
        | some k => have := lastIdx_lt c (P.drop start) k hd; rw [List.length_drop] at this
                    simp [hd, Option.elim]; omega
        | none => simp only [hd, Option.elim]; omega
      have hlen : (shrinkV (P ++ [c])).length = (i + 1) - (start + dropAmt (P.drop start) c) := by
        rw [hwstep, List.length_drop, List.length_append, List.length_cons, List.length_nil, hi]
      rw [hlen]
      exact ih (P ++ [c]) (i + 1) (start + dropAmt (P.drop start) c) (setIdx c i seen) _
        (by rw [List.length_append, List.length_cons, List.length_nil, hi])
        (by rw [List.length_append, List.length_cons, List.length_nil]; omega)
        hwstep (seenOk_step seen P c i hi hseen)

-- EQUIVALENCE: the fast last-seen-index solution equals the verified window
theorem lengthOfLongest_eq (s : String) : lengthOfLongest s = lengthOfLongestV s := by
  have := goFast_eq_goV s.toList [] 0 0 ∅ 0 rfl (Nat.zero_le _)
    (by simp [shrinkV]) (by intro d; simp [lookupIdx, lastIdx])
  simpa [lengthOfLongest, lengthOfLongestV, shrinkV] using this

#print axioms lengthOfLongest_eq

-- PAYOFF: the SHIPPED `lengthOfLongest` is exactly the longest all-distinct
-- substring length (optimality transferred via the equivalence).
theorem lengthOfLongest_optimal (s : String) :
    (∃ sub, sub <:+: s.toList ∧ Distinct sub ∧ sub.length = lengthOfLongest s) ∧
    (∀ sub, sub <:+: s.toList → Distinct sub → sub.length ≤ lengthOfLongest s) := by
  rw [lengthOfLongest_eq]; exact lengthOfLongestV_optimal s

#print axioms lengthOfLongest_optimal

/- ════════════════════════════════════════════════════════════════════════
   COSMETIC: the same shipped algorithm, but in State-monad style — mirroring
   the Haskell version's `State (Int, Map Char Int)`.  This is pure plumbing
   (`get`/`set` instead of explicit accumulators); it changes nothing about the
   algorithm or its complexity.  We prove it computes *exactly* `lengthOfLongest`
   (`lengthOfLongestS_eq`), so it inherits optimality for free and stays
   axiom-clean — the State monad buys no power, only syntax.
-/

-- threaded state: (window start, last-seen map, best length so far)
abbrev LState := Nat × Store × Nat

def stepS (c : Char) (i : Nat) : StateM LState Unit := do
  let (start, seen, best) ← get
  let start' := match lookupIdx c seen with
                | some j => max start (j + 1)
                | none   => start
  set (start', setIdx c i seen, max best ((i + 1) - start'))

def goFastS : List Char → Nat → StateM LState Unit
  | [],      _ => pure ()
  | c :: cs, i => do stepS c i; goFastS cs (i + 1)

def lengthOfLongestS (s : String) : Nat :=
  ((goFastS s.toList 0).run (0, ∅, 0)).2.2.2

#guard lengthOfLongestS "abcabcbb" == 3
#guard lengthOfLongestS "bbbbb" == 1
#guard lengthOfLongestS "pwwkew" == 3

-- the State plumbing threads exactly the explicit accumulators of `goFast`
theorem goFastS_eq (rem : List Char) : ∀ i start seen best,
    ((goFastS rem i).run (start, seen, best)).2.2.2 = goFast i start seen best rem := by
  induction rem with
  | nil => intro i start seen best; rfl
  | cons c cs ih =>
      intro i start seen best
      simp only [goFastS, stepS, goFast, bind, get, getThe, MonadStateOf.get,
                 MonadState.get, StateT.get, set, MonadStateOf.set, StateT.set,
                 pure, StateT.pure, StateT.bind, StateT.run, Id.run]
      exact ih (i + 1) _ _ _

-- the State-monad solution equals the shipped solution (hence is optimal)
theorem lengthOfLongestS_eq (s : String) : lengthOfLongestS s = lengthOfLongest s :=
  goFastS_eq s.toList 0 0 ∅ 0

#print axioms lengthOfLongestS_eq
