module solution where

-- Merge two sorted lists into one sorted list.
-- Repeatedly emit the smaller head; when one list empties, append the other.
-- Specialised to ℕ so the Bool comparison _≤ᵇ_ reduces under refl.

open import Data.Nat using (ℕ; _≤ᵇ_)
open import Data.List using (List; []; _∷_)
open import Data.Bool using (true; false; if_then_else_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- Total without a TERMINATING pragma: writing the comparison with
-- `if_then_else_` (rather than `with`) keeps both recursive calls in the clause
-- body, so Agda sees the lexicographic descent — `xs` shrinks in the `then`
-- branch, `ys` shrinks (with the first list fixed) in the `else` branch.
mergeTwo : List ℕ → List ℕ → List ℕ
mergeTwo []       ys       = ys
mergeTwo (x ∷ xs) []       = x ∷ xs
mergeTwo (x ∷ xs) (y ∷ ys) = if x ≤ᵇ y then x ∷ mergeTwo xs (y ∷ ys)
                                       else y ∷ mergeTwo (x ∷ xs) ys

-- compile-time test
_ : mergeTwo (1 ∷ 2 ∷ 4 ∷ []) (1 ∷ 3 ∷ 4 ∷ []) ≡ (1 ∷ 1 ∷ 2 ∷ 3 ∷ 4 ∷ 4 ∷ [])
_ = refl
