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
-- future work: port the Lean `search_peak` PeakAt *correctness* proof (the binary
-- search returns a genuine peak on adjacent-distinct input).  Here we ship the
-- total search, a Boolean `peakAtᵇ` checker mirroring the PeakAt disjunctions, and
-- compile-time `refl` tests.  See solution.lean for the fully verified version.

open import Data.Nat using (ℕ; zero; suc; _+_; _∸_; _<_; _≤_; s≤s; z≤n; _<ᵇ_; _≡ᵇ_)
open import Data.Nat.Properties using (m∸n≤m; m≤n⇒m≤1+n)
open import Data.Nat.Induction using (<-wellFounded)
open import Relation.Binary using (Decidable)
open import Data.Nat using (_<?_)
open import Induction.WellFounded using (Acc; acc)
open import Data.Bool using (Bool; true; false; if_then_else_; _∧_; _∨_)
open import Data.List using (List; []; _∷_; length)
open import Relation.Nullary using (yes; no)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

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
