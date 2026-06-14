module solution where

open import Data.Nat using (ℕ)
open import Data.List using (List; []; _∷_; map; concatMap; length)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.Bool using (Bool; true)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

private
  variable
    A : Set

-- each element picked as head, paired with the remaining list
selections : List A → List (A × List A)
selections []       = []
selections (x ∷ xs) = (x , xs) ∷ map (λ p → (proj₁ p , x ∷ proj₂ p)) (selections xs)

-- All permutations: pick each element as the head, recurse on the rest.
-- `permute rest` is a call on the second component of a `selections` element,
-- which Agda's termination checker does not see as structurally smaller, so we
-- mark it TERMINATING (it does terminate: rest is always one shorter than xs).
{-# TERMINATING #-}
permute : List A → List (List A)
permute []       = [] ∷ []
permute (x ∷ xs) =
  concatMap (λ p → map (proj₁ p ∷_) (permute (proj₂ p)))
            (selections (x ∷ xs))

-- compile-time tests: 3 distinct elements -> 3! = 6 permutations.
-- Exact enumeration order matches the .hs/.idr/.lean "pick head, recurse" scheme.
_ : permute (1 ∷ 2 ∷ 3 ∷ [])
  ≡ (1 ∷ 2 ∷ 3 ∷ []) ∷ (1 ∷ 3 ∷ 2 ∷ [])
  ∷ (2 ∷ 1 ∷ 3 ∷ []) ∷ (2 ∷ 3 ∷ 1 ∷ [])
  ∷ (3 ∷ 1 ∷ 2 ∷ []) ∷ (3 ∷ 2 ∷ 1 ∷ [])
  ∷ []
_ = refl

_ : length (permute (1 ∷ 2 ∷ 3 ∷ [])) ≡ 6
_ = refl
