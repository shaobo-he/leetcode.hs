/-
LeetCode 162: Find Peak Element — binary search for a peak, justified by a search
*invariant* rather than sortedness (a first for this repo: "binary search whose
correctness rests on an invariant, not on the array being sorted").

We model the array as a total function `a : Nat → Int` together with a length `n`.
`search a lo hi` binary-searches the window `[lo, hi]`: compare `a mid` with
`a (mid+1)` and recurse RIGHT (`[mid+1, hi]`) when ascending, else LEFT (`[lo, mid]`,
keeping `mid`).  It is TOTAL by well-founded recursion on the window size `hi - lo`
(`termination_by hi - lo`, `decreasing_by omega`).

A *peak* uses DISJUNCTIONS at the boundaries (no -∞ sentinel):

  PeakAt a n i := (i = 0 ∨ a (i-1) < a i) ∧ (i + 1 = n ∨ a (i+1) < a i).

THEOREM `search_peak`: on any `a` whose adjacent entries differ
(`∀ k, a k ≠ a (k+1)`), running `search` on the full window `[0, n-1]` of a
nonempty array returns an index that is a genuine peak.  Adjacent-distinctness is
used in EXACTLY one spot — the `≤ ∧ ≠ ⇒ <` step in the left branch (discharged by
`omega`).  `[1,1]` is the counterexample showing the hypothesis is necessary.
Existence of a peak on any nonempty adjacent-distinct array is then a free
corollary (`exists_peak`); `findPeak` packages the index with its peak proof as a
subtype `{ i // i < n ∧ PeakAt a n i }`.

Lean 4, core library only (no Mathlib / no Lake).
-/

def PeakAt (a : Nat → Int) (n i : Nat) : Prop :=
  (i = 0 ∨ a (i - 1) < a i) ∧ (i + 1 = n ∨ a (i + 1) < a i)

-- Binary search of the window [lo, hi].  Total via well-founded recursion on the
-- window size `hi - lo`; the only branches recurse on strictly smaller windows.
def search (a : Nat → Int) (lo hi : Nat) : Nat :=
  if h : lo < hi then
    if a ((lo + hi) / 2) < a ((lo + hi) / 2 + 1) then
      search a ((lo + hi) / 2 + 1) hi
    else
      search a lo ((lo + hi) / 2)
  else
    lo
termination_by hi - lo
decreasing_by all_goals omega

-- runnable smoke tests (the function returns a sensible index for ANY input).
private def t1 : Nat → Int := fun i => [1, 2, 3, 1].getD i 0
private def t2 : Nat → Int := fun i => [1, 2, 1, 3, 5, 6, 4].getD i 0
#guard search t1 0 3 == 2
#guard search t2 0 6 == 5
#guard decide (t1 1 < t1 2 ∧ t1 3 < t1 2)   -- index 2 is a peak in t1
#guard decide (t2 4 < t2 5 ∧ t2 6 < t2 5)   -- index 5 is a peak in t2

/- ── Correctness: `search` on `[0, n-1]` returns a peak ───────────────────────
The window invariant carried through the recursion is:
  • `lo ≤ hi` and `hi < n`                              (window valid, in range);
  • `lo = 0 ∨ a (lo-1) < a lo`                          (LEFT boundary ascends in);
  • `hi + 1 = n ∨ a (hi+1) < a hi`                      (RIGHT boundary ascends in).
When the window collapses (`¬ lo < hi`, so `lo = hi`) those two boundary facts are
exactly `PeakAt a n lo`.  Each recursion re-establishes the moved boundary from the
comparison `a mid` vs `a (mid+1)`. -/
theorem search_peak (a : Nat → Int) (n : Nat) (hadj : ∀ k, a k ≠ a (k + 1)) :
    ∀ lo hi, lo ≤ hi → hi < n →
      (lo = 0 ∨ a (lo - 1) < a lo) →
      (hi + 1 = n ∨ a (hi + 1) < a hi) →
      search a lo hi < n ∧ PeakAt a n (search a lo hi) := by
  intro lo hi
  induction lo, hi using search.induct (a := a) with
  | case1 lo hi h hc ih =>         -- lo < hi and a mid < a (mid+1): recurse RIGHT
      intro _ hn _ bR
      rw [search.eq_def, dif_pos h, if_pos hc]
      refine ih (by omega) hn (Or.inr ?_) bR
      have he : (lo + hi) / 2 + 1 - 1 = (lo + hi) / 2 := by omega
      rw [he]; exact hc
  | case2 lo hi h hc ih =>         -- lo < hi and ¬ a mid < a (mid+1): recurse LEFT
      intro _ hn bL _
      rw [search.eq_def, dif_pos h, if_neg hc]
      refine ih (by omega) (by omega) bL (Or.inr ?_)
      have := hadj ((lo + hi) / 2)        -- adjacent-distinctness used HERE only
      omega
  | case3 lo hi h =>               -- ¬ lo < hi: the window is a single index `lo`
      intro hle hn bL bR
      rw [search.eq_def, dif_neg h]
      have e : lo = hi := by omega
      exact ⟨by omega, bL, by rw [e]; exact bR⟩

-- packaging: an index together with a proof that it is a peak.
def findPeak (a : Nat → Int) (n : Nat) (hn : 0 < n) (hadj : ∀ k, a k ≠ a (k + 1)) :
    { i // i < n ∧ PeakAt a n i } :=
  ⟨search a 0 (n - 1),
   search_peak a n hadj 0 (n - 1) (by omega) (by omega) (Or.inl rfl) (Or.inl (by omega))⟩

-- free corollary: every nonempty adjacent-distinct array HAS a peak.
theorem exists_peak (a : Nat → Int) (n : Nat) (hn : 0 < n) (hadj : ∀ k, a k ≠ a (k + 1)) :
    ∃ i, i < n ∧ PeakAt a n i :=
  ⟨_, (findPeak a n hn hadj).2⟩

#print axioms search_peak
#print axioms findPeak
#print axioms exists_peak
