module solution where

-- Kadane's algorithm. Inputs may be negative, so use ℤ.

open import Data.Integer using (ℤ; +_; -[1+_]; _+_; _⊔_)
open import Data.List using (List; []; _∷_; foldl)
open import Data.Product using (_×_; _,_; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- max on ℤ is _⊔_.
step : (ℤ × ℤ) → ℤ → (ℤ × ℤ)
step (cur , best) n = let cur' = n ⊔ (cur + n) in (cur' , (best ⊔ cur'))

maxSubArray : List ℤ → ℤ
maxSubArray []       = + 0
maxSubArray (x ∷ xs) = proj₂ (foldl step (x , x) xs)

-- compile-time tests
_ : maxSubArray (-[1+ 1 ] ∷ + 1 ∷ -[1+ 2 ] ∷ + 4 ∷ -[1+ 0 ] ∷ + 2 ∷ + 1 ∷ -[1+ 4 ] ∷ + 4 ∷ []) ≡ + 6
_ = refl

_ : maxSubArray (+ 5 ∷ + 4 ∷ -[1+ 0 ] ∷ + 7 ∷ + 8 ∷ []) ≡ + 23
_ = refl
