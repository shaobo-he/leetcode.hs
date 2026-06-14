module solution where

open import Data.Nat using (ℕ; _⊔_; _≤ᵇ_)
open import Data.List using (List; []; _∷_; foldl; reverse)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.Bool using (if_then_else_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- An interval is a (lo , hi) pair of naturals.
Iv : Set
Iv = ℕ × ℕ

-- verified-style insertion sort by start (proj₁); structural, so total.
insertIv : Iv → List Iv → List Iv
insertIv x []       = x ∷ []
insertIv x (y ∷ ys) =
  if proj₁ x ≤ᵇ proj₁ y then x ∷ y ∷ ys
                        else y ∷ insertIv x ys

isort : List Iv → List Iv
isort []       = []
isort (x ∷ xs) = insertIv x (isort xs)

-- coalescing fold; accumulator kept reversed (head = most recent / largest start).
step : List Iv → Iv → List Iv
step []           iv = iv ∷ []
step (top ∷ rest) iv =
  if proj₁ iv ≤ᵇ proj₂ top
    then (proj₁ top , proj₂ top ⊔ proj₂ iv) ∷ rest
    else iv ∷ top ∷ rest

merge : List Iv → List Iv
merge xs = reverse (foldl step [] (isort xs))

-- compile-time tests: the standard example and the touching-endpoints case.
_ : merge ((1 , 3) ∷ (2 , 6) ∷ (8 , 10) ∷ (15 , 18) ∷ [])
  ≡ (1 , 6) ∷ (8 , 10) ∷ (15 , 18) ∷ []
_ = refl

_ : merge ((1 , 4) ∷ (4 , 5) ∷ []) ≡ (1 , 5) ∷ []
_ = refl

_ : merge [] ≡ []
_ = refl
