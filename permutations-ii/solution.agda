module solution where

open import Data.Nat using (ℕ; zero; suc; _∸_; _≡ᵇ_)
open import Data.List using (List; []; _∷_; map; concatMap; foldl; all; length)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.Bool using (Bool; true; false; if_then_else_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- multiset over ℕ as an assoc list of (value, count)
bump : ℕ → List (ℕ × ℕ) → List (ℕ × ℕ)
bump x []             = (x , 1) ∷ []
bump x ((y , n) ∷ rest) =
  if x ≡ᵇ y then (y , suc n) ∷ rest
            else (y , n) ∷ bump x rest

countsOf : List ℕ → List (ℕ × ℕ)
countsOf = foldl (λ acc x → bump x acc) []

decrCount : ℕ → List (ℕ × ℕ) → List (ℕ × ℕ)
decrCount x = map (λ p → if proj₁ p ≡ᵇ x then (proj₁ p , proj₂ p ∸ 1) else p)

-- pick each distinct value with count > 0 as the next element, recurse.
-- The recursive call is on `decrCount …`, not a structural sub-term, so we mark
-- it TERMINATING (it terminates: the total remaining count strictly decreases).
{-# TERMINATING #-}
permsFromCounts : List (ℕ × ℕ) → List (List ℕ)
permsFromCounts cs =
  if all (λ p → proj₂ p ≡ᵇ 0) cs
    then [] ∷ []
    else concatMap
           (λ p → if proj₂ p ≡ᵇ 0
                    then []
                    else map (proj₁ p ∷_) (permsFromCounts (decrCount (proj₁ p) cs)))
           cs

permuteUnique : List ℕ → List (List ℕ)
permuteUnique xs = permsFromCounts (countsOf xs)

-- compile-time tests: [1,1,2] has exactly 3 unique permutations, in this order.
_ : permuteUnique (1 ∷ 1 ∷ 2 ∷ [])
  ≡ (1 ∷ 1 ∷ 2 ∷ []) ∷ (1 ∷ 2 ∷ 1 ∷ []) ∷ (2 ∷ 1 ∷ 1 ∷ []) ∷ []
_ = refl

_ : length (permuteUnique (1 ∷ 1 ∷ 2 ∷ [])) ≡ 3
_ = refl
