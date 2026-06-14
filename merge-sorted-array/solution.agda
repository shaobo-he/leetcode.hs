module solution where

-- LeetCode 88: Merge Sorted Array — merge two sorted lists.
-- Mirrors the Haskell/Lean/Idris `sortedMerge`: walk both lists, always
-- emitting the smaller head, until one side is exhausted.

open import Data.Bool using (if_then_else_)
open import Data.Nat using (ℕ; _≤ᵇ_)
open import Data.List using (List; []; _∷_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

sortedMerge : List ℕ → List ℕ → List ℕ
sortedMerge []       ys       = ys
sortedMerge (x ∷ xs) []       = x ∷ xs
sortedMerge (x ∷ xs) (y ∷ ys) =
  if x ≤ᵇ y
    then x ∷ sortedMerge xs (y ∷ ys)
    else y ∷ sortedMerge (x ∷ xs) ys

-- compile-time tests (same examples the other languages test)
_ : sortedMerge (1 ∷ 2 ∷ 3 ∷ []) (2 ∷ 5 ∷ 6 ∷ []) ≡ (1 ∷ 2 ∷ 2 ∷ 3 ∷ 5 ∷ 6 ∷ [])
_ = refl

_ : sortedMerge (1 ∷ 3 ∷ 4 ∷ []) (2 ∷ 5 ∷ 6 ∷ []) ≡ (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ 6 ∷ [])
_ = refl
