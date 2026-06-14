module solution where

open import Data.Nat using (ℕ; zero; suc; _/_; _≤ᵇ_)
open import Data.List using (List; []; _∷_; length; splitAt)
open import Data.Product using (_,_)
open import Data.Bool using (if_then_else_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- merge two sorted lists.  Structural on the combined "fuel" via the pair
-- pattern: we recurse on a strictly smaller argument in each clause.
mergeTwo : List ℕ → List ℕ → List ℕ
mergeTwo []       ys       = ys
mergeTwo (x ∷ xs) []       = x ∷ xs
mergeTwo (x ∷ xs) (y ∷ ys) =
  if x ≤ᵇ y then x ∷ mergeTwo xs (y ∷ ys)
            else y ∷ mergeTwo (x ∷ xs) ys

-- Divide and conquer: pairwise-merge the halves.  Not structurally recursive
-- (we split the list in half), so we mark it TERMINATING.
{-# TERMINATING #-}
mergeK : List (List ℕ) → List ℕ
mergeK []            = []
mergeK (xs ∷ [])     = xs
mergeK xss@(_ ∷ _ ∷ _) with splitAt (length xss / 2) xss
... | (l , r) = mergeTwo (mergeK l) (mergeK r)

-- compile-time test: same example the other languages assert.
_ : mergeK ((1 ∷ 4 ∷ 5 ∷ []) ∷ (1 ∷ 3 ∷ 4 ∷ []) ∷ (2 ∷ 6 ∷ []) ∷ [])
    ≡ (1 ∷ 1 ∷ 2 ∷ 3 ∷ 4 ∷ 4 ∷ 5 ∷ 6 ∷ [])
_ = refl
