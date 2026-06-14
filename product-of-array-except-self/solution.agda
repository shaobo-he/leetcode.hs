module solution where

-- Product of array except self.
-- prefix products times suffix products gives, at each position, the product of
-- everything to the left times everything to the right (i.e. all but self).

open import Data.Integer using (ℤ; +_; _*_)
open import Data.List using (List; []; _∷_; reverse; zipWith)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- running products: prefixProds acc xs = [acc, acc*x0, acc*x0*x1, ...]
prefixProds : ℤ → List ℤ → List ℤ
prefixProds acc []       = []
prefixProds acc (y ∷ ys) = acc ∷ prefixProds (acc * y) ys

productExceptSelf : List ℤ → List ℤ
productExceptSelf xs =
  let pre = prefixProds (+ 1) xs
      suf = reverse (prefixProds (+ 1) (reverse xs))
  in zipWith _*_ pre suf

-- compile-time tests
_ : productExceptSelf (+ 1 ∷ + 2 ∷ + 3 ∷ + 4 ∷ []) ≡ (+ 24 ∷ + 12 ∷ + 8 ∷ + 6 ∷ [])
_ = refl

_ : productExceptSelf (+ 5 ∷ + 2 ∷ + 3 ∷ []) ≡ (+ 6 ∷ + 15 ∷ + 10 ∷ [])
_ = refl
