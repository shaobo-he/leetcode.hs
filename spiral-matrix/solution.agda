module solution where

open import Data.Nat using (ℕ)
open import Data.List using (List; []; _∷_; _++_; map; reverse; mapMaybe)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

private
  variable
    A : Set

headᵐ : List A → Maybe A
headᵐ []      = nothing
headᵐ (x ∷ _) = just x

tailᴸ : List A → List A
tailᴸ []       = []
tailᴸ (_ ∷ xs) = xs

-- stdlib has no List transpose, so define our own (matches the Lean version).
{-# TERMINATING #-}
transpose : List (List A) → List (List A)
transpose []            = []
transpose ([] ∷ _)      = []
transpose rows          = mapMaybe headᵐ rows ∷ transpose (map tailᴸ rows)

-- Peel the first row, rotate the rest counter-clockwise (reverse ∘ transpose),
-- recurse.  Not structurally recursive on the matrix.
{-# TERMINATING #-}
spiralOrder : List (List A) → List A
spiralOrder []           = []
spiralOrder ([] ∷ _)     = []
spiralOrder (row ∷ rows) = row ++ spiralOrder (reverse (transpose rows))

-- compile-time tests: same exact outputs the other languages assert.
_ : spiralOrder ((1 ∷ 2 ∷ 3 ∷ []) ∷ (4 ∷ 5 ∷ 6 ∷ []) ∷ (7 ∷ 8 ∷ 9 ∷ []) ∷ [])
    ≡ (1 ∷ 2 ∷ 3 ∷ 6 ∷ 9 ∷ 8 ∷ 7 ∷ 4 ∷ 5 ∷ [])
_ = refl

_ : spiralOrder ((1 ∷ 2 ∷ 3 ∷ 4 ∷ []) ∷ (5 ∷ 6 ∷ 7 ∷ 8 ∷ []) ∷ (9 ∷ 10 ∷ 11 ∷ 12 ∷ []) ∷ [])
    ≡ (1 ∷ 2 ∷ 3 ∷ 4 ∷ 8 ∷ 12 ∷ 11 ∷ 10 ∷ 9 ∷ 5 ∷ 6 ∷ 7 ∷ [])
_ = refl
