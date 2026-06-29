module solution where

-- LeetCode 162: Find Peak Element.
--
-- Binary search for a peak: model the array as a function `a : ℕ → ℕ`, search the
-- window [lo, hi]; compare `a mid` with `a (suc mid)` and recurse RIGHT
-- ([suc mid, hi]) when ascending, else LEFT ([lo, mid], keeping mid).
--
-- TOTAL by WELL-FOUNDED recursion on the window size `hi ∸ lo` (`<-wellFounded`),
-- NO {-# TERMINATING #-}.  The two decreasing facts (`leftDecr`, `rightDecr`) are
-- proved by induction; choosing `mid = lo + half (hi ∸ lo)` keeps the proofs to
-- `half (suc d) ≤ d` and `m ∸ n ≤ m`, with no division lemma.
--
-- CORRECTNESS (ported from Lean `search_peak`): on a nonempty, adjacent-distinct
-- input the returned index is a genuine `PeakAt`.  `search-correct` carries the same
-- window invariant as the Lean proof and is driven by the SAME Acc-induction the
-- function uses (it recurses on the `acc rec` witness, structurally smaller each
-- step — no {-# TERMINATING #-}, no postulate).  Adjacent-distinctness is used in
-- exactly one spot (the LEFT branch, via `≤∧≢⇒<`), mirroring the Lean `omega` step.

open import Data.Nat using (ℕ; zero; suc; _+_; _∸_; _<_; _≤_; s≤s; z≤n; _<ᵇ_; _≡ᵇ_)
open import Data.Nat.Properties using
  ( m∸n≤m; m≤n⇒m≤1+n; <-trans; ≤-<-trans; ≤-antisym; ≮⇒≥; ≤∧≢⇒<
  ; m≤m+n; <ᵇ⇒<; <⇒<ᵇ; n<1+n )
open import Data.Nat.Induction using (<-wellFounded)
open import Relation.Binary using (Decidable)
open import Data.Nat using (_<?_)
open import Induction.WellFounded using (Acc; acc)
open import Data.Bool using (Bool; true; false; if_then_else_; _∧_; _∨_; T)
open import Data.List using (List; []; _∷_; length)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Product using (_×_; _,_; ∃)
open import Relation.Nullary using (yes; no)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; sym; subst)

-- floor(n/2), structural.
half : ℕ → ℕ
half zero          = zero
half (suc zero)    = zero
half (suc (suc n)) = suc (half n)

half-≤ : (d : ℕ) → half (suc d) ≤ d
half-≤ zero          = z≤n
half-≤ (suc zero)    = s≤s z≤n
half-≤ (suc (suc k)) = s≤s (m≤n⇒m≤1+n (half-≤ k))

half-< : (d : ℕ) → half (suc d) < suc d
half-< d = s≤s (half-≤ d)

-- LEFT branch: [lo, mid] is a strictly smaller window than [lo, hi].
leftDecr : (lo hi : ℕ) → suc lo ≤ hi → ((lo + half (hi ∸ lo)) ∸ lo) < (hi ∸ lo)
leftDecr zero    (suc h) _       = half-< h
leftDecr (suc l) (suc h) (s≤s p) = leftDecr l h p
leftDecr lo      zero    ()

-- RIGHT branch: [suc mid, hi] is a strictly smaller window than [lo, hi].
rightDecr : (lo hi : ℕ) → suc lo ≤ hi → (hi ∸ suc (lo + half (hi ∸ lo))) < (hi ∸ lo)
rightDecr zero    (suc h) _       = s≤s (m∸n≤m h (half (suc h)))
rightDecr (suc l) (suc h) (s≤s p) = rightDecr l h p
rightDecr lo      zero    ()

-- The chosen midpoint lies strictly below `hi` (so `[suc mid, hi]` is valid).
mid<hi : (lo hi : ℕ) → suc lo ≤ hi → lo + half (hi ∸ lo) < hi
mid<hi zero    (suc h) _       = half-< h
mid<hi (suc l) (suc h) (s≤s p) = s≤s (mid<hi l h p)
mid<hi lo      zero    ()

-- The verified-total binary search.
search : (a : ℕ → ℕ) → (lo hi : ℕ) → Acc _<_ (hi ∸ lo) → ℕ
search a lo hi (acc rec) with lo <? hi
... | no  _      = lo
... | yes lo<hi =
        if a (lo + half (hi ∸ lo)) <ᵇ a (suc (lo + half (hi ∸ lo)))
          then search a (suc (lo + half (hi ∸ lo))) hi (rec (rightDecr lo hi lo<hi))
          else search a lo (lo + half (hi ∸ lo)) (rec (leftDecr lo hi lo<hi))

-- runnable wrappers over List ℕ.
nth : List ℕ → ℕ → ℕ
nth []        _       = 0
nth (x ∷ _)   zero    = x
nth (_ ∷ xs)  (suc k) = nth xs k

findPeak : List ℕ → ℕ
findPeak xs = search (nth xs) 0 (length xs ∸ 1) (<-wellFounded ((length xs ∸ 1) ∸ 0))

-- PeakAt as a Boolean check: boundaries handled as disjunctions (no -∞ sentinel).
peakAtᵇ : (ℕ → ℕ) → ℕ → ℕ → Bool
peakAtᵇ a n i = ((i ≡ᵇ 0) ∨ (a (i ∸ 1) <ᵇ a i)) ∧ ((suc i ≡ᵇ n) ∨ (a (suc i) <ᵇ a i))

-- PeakAt as a *proposition* (mirror of the Lean `PeakAt`): boundary disjunctions.
PeakAt : (ℕ → ℕ) → ℕ → ℕ → Set
PeakAt a n i = ((i ≡ 0) ⊎ (a (i ∸ 1) < a i)) × ((suc i ≡ n) ⊎ (a (suc i) < a i))

-- ── Correctness ──────────────────────────────────────────────────────────────
-- Window invariant (cf. Lean `search_peak`): `lo ≤ hi`, `hi < n`, and the two
-- boundaries ascend INTO the window.  When the window collapses (`¬ lo < hi`, so
-- `lo ≡ hi`) those boundary facts are exactly `PeakAt a n lo`.  Each step
-- re-establishes the moved boundary from the comparison `a mid` vs `a (suc mid)`.
search-correct :
  (a : ℕ → ℕ) (n : ℕ) → (∀ k → a k ≢ a (suc k)) →
  (lo hi : ℕ) (ac : Acc _<_ (hi ∸ lo)) →
  lo ≤ hi → hi < n →
  ((lo ≡ 0) ⊎ (a (lo ∸ 1) < a lo)) →
  ((suc hi ≡ n) ⊎ (a (suc hi) < a hi)) →
  (search a lo hi ac < n) × PeakAt a n (search a lo hi ac)
search-correct a n hadj lo hi (acc rec) lo≤hi hi<n bL bR with lo <? hi
... | no ¬lo<hi =
      ≤-<-trans lo≤hi hi<n
    , bL
    , subst (λ x → (suc x ≡ n) ⊎ (a (suc x) < a x))
            (sym (≤-antisym lo≤hi (≮⇒≥ ¬lo<hi))) bR
... | yes lo<hi
      with a (lo + half (hi ∸ lo)) <ᵇ a (suc (lo + half (hi ∸ lo))) in eq
...   | true  =                         -- ascending: recurse RIGHT, [suc mid, hi]
        search-correct a n hadj (suc (lo + half (hi ∸ lo))) hi
          (rec (rightDecr lo hi lo<hi))
          (mid<hi lo hi lo<hi) hi<n
          (inj₂ (<ᵇ⇒< _ _ (subst T (sym eq) _)))
          bR
...   | false =                         -- not ascending: recurse LEFT, [lo, mid]
        search-correct a n hadj lo (lo + half (hi ∸ lo))
          (rec (leftDecr lo hi lo<hi))
          (m≤m+n lo (half (hi ∸ lo)))
          (<-trans (mid<hi lo hi lo<hi) hi<n)
          bL
          (inj₂ (≤∧≢⇒< (≮⇒≥ (λ lt → subst T eq (<⇒<ᵇ lt)))
                       (λ e → hadj (lo + half (hi ∸ lo)) (sym e))))

-- Top-level corollary (mirror of Lean `findPeak`): the full search on a nonempty
-- array `[0, n-1]` lands on a genuine peak.
findPeak-peak :
  (a : ℕ → ℕ) (m : ℕ) → (∀ k → a k ≢ a (suc k)) →
  let i = search a 0 (suc m ∸ 1) (<-wellFounded ((suc m ∸ 1) ∸ 0)) in
  (i < suc m) × PeakAt a (suc m) i
findPeak-peak a m hadj =
  search-correct a (suc m) hadj 0 (suc m ∸ 1)
    (<-wellFounded ((suc m ∸ 1) ∸ 0))
    z≤n (n<1+n m) (inj₁ refl) (inj₁ refl)

-- Free corollary (mirror of Lean `exists_peak`): every nonempty adjacent-distinct
-- array HAS a peak.
exists-peak :
  (a : ℕ → ℕ) (m : ℕ) → (∀ k → a k ≢ a (suc k)) →
  ∃ (λ i → (i < suc m) × PeakAt a (suc m) i)
exists-peak a m hadj = _ , findPeak-peak a m hadj

-- compile-time tests (the `#guard` analogue): the search lands on a peak.
_ : findPeak (1 ∷ 2 ∷ 3 ∷ 1 ∷ []) ≡ 2
_ = refl

_ : peakAtᵇ (nth (1 ∷ 2 ∷ 3 ∷ 1 ∷ [])) 4 (findPeak (1 ∷ 2 ∷ 3 ∷ 1 ∷ [])) ≡ true
_ = refl

-- [1,2,1,3,5,6,4] has two peaks (1 and 5); the search lands on index 5.
_ : findPeak (1 ∷ 2 ∷ 1 ∷ 3 ∷ 5 ∷ 6 ∷ 4 ∷ []) ≡ 5
_ = refl

_ : peakAtᵇ (nth (1 ∷ 2 ∷ 1 ∷ 3 ∷ 5 ∷ 6 ∷ 4 ∷ [])) 7
      (findPeak (1 ∷ 2 ∷ 1 ∷ 3 ∷ 5 ∷ 6 ∷ 4 ∷ [])) ≡ true
_ = refl

_ : findPeak (1 ∷ []) ≡ 0
_ = refl
