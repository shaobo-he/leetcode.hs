module solution where

open import Data.Nat using (ℕ; suc; _<_)
open import Data.Nat.Properties using (≤-refl)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.List using (List; []; _∷_; _++_; map; reverse; mapMaybe; length)
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
-- Total by well-founded recursion on the first row's length: each step drops a
-- column (`map tailᴸ`), shrinking that length until a row empties.
firstRow : List (List A) → List A
firstRow []      = []
firstRow (r ∷ _) = r

transpose : List (List A) → List (List A)
transpose rows = go rows (<-wellFounded (length (firstRow rows)))
  where
    go : (rs : List (List A)) → Acc _<_ (length (firstRow rs)) → List (List A)
    go []             _         = []
    go ([] ∷ _)       _         = []
    go ((x ∷ r) ∷ rs) (acc rec) =
      mapMaybe headᵐ ((x ∷ r) ∷ rs) ∷ go (map tailᴸ ((x ∷ r) ∷ rs)) (rec ≤-refl)

-- Peel the first row, rotate the rest counter-clockwise (reverse ∘ transpose),
-- recurse.  Terminates on the total element count (each step removes the peeled
-- row and `reverse`/`transpose` only rearrange), but that measure needs
-- sumLen-preservation lemmas for `reverse`/`transpose`; left TERMINATING for now.
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
