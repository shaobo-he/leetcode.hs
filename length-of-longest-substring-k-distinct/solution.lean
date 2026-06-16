/-
LeetCode 340: Longest Substring with At Most K Distinct Characters
Sliding window holding at most k distinct chars, with per-char counts.
Lean 4, core library only.
-/

def maxN (a b : Nat) : Nat := if a < b then b else a

def incr (x : Char) : List (Char × Nat) → List (Char × Nat)
  | [] => [(x, 1)]
  | (y, n) :: rest =>
      if x == y then (y, n + 1) :: rest else (y, n) :: incr x rest

def decr (x : Char) : List (Char × Nat) → List (Char × Nat)
  | [] => []
  | (y, n) :: rest =>
      if x == y then
        (match n with
         | k + 2 => (y, k + 1) :: rest
         | _     => rest)
      else (y, n) :: decr x rest

-- structural on `win` (recurses on its tail)
def shrinkW (k : Nat) (win : List Char) (counts : List (Char × Nat)) :
    List Char × List (Char × Nat) :=
  if counts.length > k then
    match win with
    | d :: ds => shrinkW k ds (decr d counts)
    | []      => (win, counts)
  else (win, counts)

-- structural on the input list (recurses on its tail)
def goW (k : Nat) (win : List Char) (counts : List (Char × Nat))
    (best : Nat) : List Char → Nat
  | []      => best
  | c :: cs =>
      let (win2, counts2) := shrinkW k (win ++ [c]) (incr c counts)
      goW k win2 counts2 (maxN best win2.length) cs

def lenKDistinct (s : String) (k : Nat) : Nat :=
  goW k [] [] 0 s.toList

#guard lenKDistinct "eceba" 2 == 3
#guard lenKDistinct "aa" 1    == 2
#guard lenKDistinct "abee" 1  == 2

/- ════════════════════════════════════════════════════════════════════════
   FULLY VERIFIED VARIANT.

   The window above (counts-based, O(n)) is the fast runnable solution.  Below
   is a structurally-shrinking variant `lenKDistinctV`, for which we prove FULL
   OPTIMALITY: its answer is *exactly* the maximum length over all substrings
   with ≤ k distinct characters.

   This is the proof the companion Idris file (`solution.idr`) left open: Idris
   proved soundness (`windowSound : window ≤ solve`) and the core
   `shrinkVLongest`, but not the lower bound `solve ≤ window`.  The blocker was
   `numDistinct` monotonicity under append, which needs lawful decidable
   equality; in Lean it falls out of `nd_snoc`/`nd_mono` below, and the whole
   argument goes through with `induction`/`simp`/`omega`.  We additionally use
   core `List.IsInfix`/`IsSuffix`, which replaces ~100 lines of Idris
   substring-enumeration lemmas.

   `#print axioms` at the bottom confirms it relies only on Lean's three
   standard foundational axioms — no `sorry`.
   ════════════════════════════════════════════════════════════════════════ -/

/- distinct-character count, via decidable Char equality (so we can reason
   about it; the `==`/`nub` "distinct" doesn't reflect propositional equality) -/
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

-- appending one char changes the distinct count by exactly 0 or 1 …
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

-- … hence it is monotone under append (the lemma that blocked the Idris proof)
theorem nd_mono (xs : List Char) (c : Char) : nd xs ≤ nd (xs ++ [c]) := by
  rw [nd_snoc]; by_cases h : decElem c xs = true <;> simp [h]

/- shrinkV: drop chars from the FRONT until the distinct count is ≤ k.
   = the longest valid (≤ k-distinct) suffix of the window. -/
def shrinkV (k : Nat) : List Char → List Char
  | []      => []
  | x :: xs => if nd (x :: xs) ≤ k then x :: xs else shrinkV k xs

theorem shrinkV_suffix (k : Nat) (w : List Char) : shrinkV k w <:+ w := by
  induction w with
  | nil => simp [shrinkV]
  | cons x xs ih =>
      simp only [shrinkV]; split
      · exact List.suffix_rfl
      · exact ih.trans (List.suffix_cons x xs)

theorem shrinkV_valid (k : Nat) (w : List Char) : nd (shrinkV k w) ≤ k := by
  induction w with
  | nil => simp [shrinkV, nd]
  | cons x xs ih =>
      simp only [shrinkV]; split
      · assumption
      · exact ih

-- the core local lemma: every valid suffix of w is a suffix of shrinkV k w
theorem shrinkV_longest (k : Nat) (sf : List Char) (hv : nd sf ≤ k) :
    ∀ w, sf <:+ w → sf <:+ shrinkV k w := by
  intro w hsf
  induction w with
  | nil => rw [List.suffix_nil.mp hsf]; simp [shrinkV]
  | cons x xs ih =>
      simp only [shrinkV]; split
      · exact hsf
      · rename_i hgt
        have hne : sf ≠ x :: xs := fun h => hgt (h ▸ hv)
        exact ih ((List.suffix_cons_iff.mp hsf).resolve_left hne)

/- ── list helpers for the window invariant ── -/
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
def goV (k : Nat) : Nat → List Char → List Char → Nat
  | best, _win, []      => best
  | best, win,  c :: cs =>
      let w := shrinkV k (win ++ [c])
      goV k (max best w.length) w cs

def lenKDistinctV (s : String) (k : Nat) : Nat := goV k 0 [] s.toList

-- the verified variant agrees with the fast one on the examples
#guard lenKDistinctV "eceba" 2 == 3
#guard lenKDistinctV "aa" 1    == 2
#guard lenKDistinctV "abee" 1  == 2

theorem goV_mono (k : Nat) : ∀ rem best win, best ≤ goV k best win rem := by
  intro rem
  induction rem with
  | nil => intro best win; simp [goV]
  | cons c cs ih =>
      intro best win; simp only [goV]
      exact Nat.le_trans (Nat.le_max_left _ _) (ih _ _)

/- ── the window invariant: `win` is the longest valid suffix of consumed `P` ── -/
def IsLVS (k : Nat) (win P : List Char) : Prop :=
  win <:+ P ∧ nd win ≤ k ∧ ∀ sf, sf <:+ P → nd sf ≤ k → sf <:+ win

theorem lvs_step (k : Nat) (win P : List Char) (c : Char) (h : IsLVS k win P) :
    IsLVS k (shrinkV k (win ++ [c])) (P ++ [c]) := by
  obtain ⟨hwP, hwv, hall⟩ := h
  refine ⟨(shrinkV_suffix k (win ++ [c])).trans (suffix_snoc_mono c hwP),
          shrinkV_valid k (win ++ [c]), ?_⟩
  intro sf hsf hv
  rcases suffix_snoc_decomp hsf with rfl | ⟨sf0, rfl, hsf0⟩
  · exact List.nil_suffix
  · have h0v : nd sf0 ≤ k := Nat.le_trans (nd_mono sf0 c) hv
    exact shrinkV_longest k (sf0 ++ [c]) hv (win ++ [c]) (suffix_snoc_mono c (hall sf0 hsf0 h0v))

def validK (k : Nat) (cs : List Char) : Prop := nd cs ≤ k

/- ── LOWER BOUND (the part Idris left open): the window's answer is ≥ the
      length of every valid substring ── -/
theorem goVOpt (k : Nat) : ∀ rem win best P,
    IsLVS k win P → win.length ≤ best →
    ∀ sub pre suf, rem = pre ++ suf → sub <:+ (P ++ pre) → nd sub ≤ k →
      sub.length ≤ goV k best win rem := by
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
      have hinv' : IsLVS k (shrinkV k (win ++ [c])) (P ++ [c]) := lvs_step k win P c hinv
      have hwb' : (shrinkV k (win ++ [c])).length ≤ max best (shrinkV k (win ++ [c])).length :=
        Nat.le_max_right _ _
      cases pre with
      | nil =>
          simp only [List.append_nil] at hsub
          obtain ⟨_, _, hall⟩ := hinv
          have hle : sub.length ≤ best :=
            Nat.le_trans (List.IsSuffix.length_le (hall sub hsub hsv)) hwb
          exact Nat.le_trans hle (Nat.le_trans (Nat.le_max_left _ _) (goV_mono k cs _ _))
      | cons a pre' =>
          rw [List.cons_append] at hrem
          injection hrem with hac hcs
          subst hac
          rw [List.append_cons] at hsub
          exact ih (shrinkV k (win ++ [c])) (max best (shrinkV k (win ++ [c])).length) (P ++ [c])
                   hinv' hwb' sub pre' suf hcs hsub hsv

-- OPTIMALITY: every valid substring is no longer than the window's answer.
theorem window_optimal (s : String) (k : Nat) (sub : List Char)
    (hsub : sub <:+: s.toList) (hv : validK k sub) :
    sub.length ≤ lenKDistinctV s k := by
  obtain ⟨before, after, hb⟩ := hsub
  have hinv : IsLVS k [] [] :=
    ⟨List.nil_suffix, Nat.zero_le k,
     fun sf hsf _ => by rw [List.suffix_nil.mp hsf]; exact List.nil_suffix⟩
  exact goVOpt k s.toList [] 0 [] hinv (Nat.le_refl 0) sub (before ++ sub) after hb.symm
    (List.suffix_append before sub) hv

/- ── UPPER BOUND: the window's answer is realized by an actual valid substring ── -/
theorem suffix_prefix_infix {a b full : List Char} (hs : a <:+ b) (hp : ∃ t, b ++ t = full) :
    a <:+: full := by
  obtain ⟨p, hp1⟩ := hs
  obtain ⟨t, hp2⟩ := hp
  exact ⟨p, t, by rw [← hp2, ← hp1]⟩

theorem goVReal (k : Nat) (full : List Char) : ∀ rem win best P,
    P ++ rem = full → win <:+ P → nd win ≤ k →
    (∃ sub, sub <:+: full ∧ nd sub ≤ k ∧ sub.length = best) →
    ∃ sub, sub <:+: full ∧ nd sub ≤ k ∧ sub.length = goV k best win rem := by
  intro rem
  induction rem with
  | nil => intro win best P _ _ _ hreal; simpa [goV] using hreal
  | cons c cs ih =>
      intro win best P hPrem hwP _ hreal
      simp only [goV]
      have hP'full : (P ++ [c]) ++ cs = full := by rw [List.append_assoc]; exact hPrem
      have hwP' : shrinkV k (win ++ [c]) <:+ P ++ [c] :=
        (shrinkV_suffix k (win ++ [c])).trans (suffix_snoc_mono c hwP)
      have hwinf : shrinkV k (win ++ [c]) <:+: full := suffix_prefix_infix hwP' ⟨cs, hP'full⟩
      have hreal' : ∃ sub, sub <:+: full ∧ nd sub ≤ k ∧
          sub.length = max best (shrinkV k (win ++ [c])).length := by
        rcases Nat.le_total (shrinkV k (win ++ [c])).length best with hle | hge
        · have hm : max best (shrinkV k (win ++ [c])).length = best := by omega
          rw [hm]; exact hreal
        · have hm : max best (shrinkV k (win ++ [c])).length
              = (shrinkV k (win ++ [c])).length := by omega
          rw [hm]; exact ⟨_, hwinf, shrinkV_valid k (win ++ [c]), rfl⟩
      exact ih (shrinkV k (win ++ [c])) (max best (shrinkV k (win ++ [c])).length) (P ++ [c])
               hP'full hwP' (shrinkV_valid k (win ++ [c])) hreal'

theorem window_realizable (s : String) (k : Nat) :
    ∃ sub, sub <:+: s.toList ∧ validK k sub ∧ sub.length = lenKDistinctV s k :=
  goVReal k s.toList s.toList [] 0 [] (by simp) List.nil_suffix (Nat.zero_le k)
    ⟨[], ⟨[], s.toList, by simp⟩, Nat.zero_le k, rfl⟩

/- ── FULL OPTIMALITY: `lenKDistinctV s k` is EXACTLY the maximum length over
      substrings with ≤ k distinct chars — the lower bound (left open in Idris)
      *and* the upper bound. ── -/
theorem lenKDistinctV_optimal (s : String) (k : Nat) :
    (∃ sub, sub <:+: s.toList ∧ validK k sub ∧ sub.length = lenKDistinctV s k) ∧
    (∀ sub, sub <:+: s.toList → validK k sub → sub.length ≤ lenKDistinctV s k) :=
  ⟨window_realizable s k, fun sub h hv => window_optimal s k sub h hv⟩

#print axioms lenKDistinctV_optimal
