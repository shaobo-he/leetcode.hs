module solution where

-- N-Queens: count distinct solutions (port of the Lean/Idris solution).
-- A placement assigns one column per successive row; we enumerate permutations
-- of [1..n] (columns are then automatically distinct) and keep the diagonal-safe
-- ones, counting how many survive.

open import Data.Nat using (ℕ; zero; suc; _∸_; _+_; _≡ᵇ_)
open import Data.List using (List; []; _∷_; map; concatMap; length; zip; all; filterᵇ; upTo)
open import Data.Bool using (Bool; true; false; not; _∧_)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- |a - b| for natural numbers
absDiff : ℕ → ℕ → ℕ
absDiff a b = (a ∸ b) + (b ∸ a)

-- All ways to pick one element out of a list, paired with the remaining list.
selections : ∀ {A : Set} → List A → List (A × List A)
selections []       = []
selections (x ∷ xs) = (x , xs) ∷ map (λ p → proj₁ p , x ∷ proj₂ p) (selections xs)

-- All permutations of a list. Not structurally recursive (recurses on the
-- `rest` produced by `selections`), so mark TERMINATING — mirrors the Lean
-- `partial def permute`.
{-# TERMINATING #-}
permute : ∀ {A : Set} → List A → List (List A)
permute []         = [] ∷ []
permute (x ∷ xs)   =
  concatMap (λ p → map (λ ps → proj₁ p ∷ ps) (permute (proj₂ p)))
            (selections (x ∷ xs))

-- Two queens at (r1,c1) and (r2,c2) don't attack diagonally.
ok : (ℕ × ℕ) → (ℕ × ℕ) → Bool
ok (r1 , c1) (r2 , c2) = not (absDiff r1 r2 ≡ᵇ absDiff c1 c2)

-- cols is the column chosen per row; pair with row indices 1..len and check
-- every pair of placements is diagonal-safe.
diagSafe : List ℕ → Bool
diagSafe cols = check (zip rows cols)
  where
    rows : List ℕ
    rows = map suc (upTo (length cols))
    check : List (ℕ × ℕ) → Bool
    check []       = true
    check (p ∷ ps) = all (ok p) ps ∧ check ps

queens : ℕ → List (List ℕ)
queens n = filterᵇ diagSafe (permute (map suc (upTo n)))

-- compile-time tests (refl fails to typecheck if the computation is wrong)
_ : length (queens 4) ≡ 2
_ = refl

_ : length (queens 5) ≡ 10
_ = refl

_ : length (queens 6) ≡ 4
_ = refl
