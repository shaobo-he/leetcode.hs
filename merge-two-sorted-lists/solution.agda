module solution where

-- Merge two sorted lists into one sorted list.
-- Repeatedly emit the smaller head; when one list empties, append the other.
-- Specialised to ℕ so the Bool comparison _≤ᵇ_ reduces under refl.

open import Data.Nat using (ℕ; _≤ᵇ_)
open import Data.List using (List; []; _∷_)
open import Data.Bool using (true; false)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- Not structurally recursive on a single argument (decreases one list per call,
-- chosen by the comparison), so flag it TERMINATING (the accepted escape hatch).
{-# TERMINATING #-}
mergeTwo : List ℕ → List ℕ → List ℕ
mergeTwo []       ys       = ys
mergeTwo (x ∷ xs) []       = x ∷ xs
mergeTwo (x ∷ xs) (y ∷ ys) with x ≤ᵇ y
... | true  = x ∷ mergeTwo xs (y ∷ ys)
... | false = y ∷ mergeTwo (x ∷ xs) ys

-- compile-time test
_ : mergeTwo (1 ∷ 2 ∷ 4 ∷ []) (1 ∷ 3 ∷ 4 ∷ []) ≡ (1 ∷ 1 ∷ 2 ∷ 3 ∷ 4 ∷ 4 ∷ [])
_ = refl
