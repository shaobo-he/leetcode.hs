module solution where

-- LeetCode 328: Odd Even Linked List
-- Group elements at odd positions ahead of those at even positions.

open import Data.Nat using (ℕ)
open import Data.List using (List; []; _∷_; _++_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

module _ {a} {A : Set a} where

  odds : List A → List A
  odds (x ∷ _ ∷ rest) = x ∷ odds rest
  odds (x ∷ [])       = x ∷ []
  odds []             = []

  evens : List A → List A
  evens (_ ∷ y ∷ rest) = y ∷ evens rest
  evens _              = []

  oddEven : List A → List A
  oddEven xs = odds xs ++ evens xs

-- compile-time tests
_ : oddEven (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ []) ≡ (1 ∷ 3 ∷ 5 ∷ 2 ∷ 4 ∷ [])
_ = refl

_ : oddEven (2 ∷ 1 ∷ 3 ∷ 5 ∷ 6 ∷ 4 ∷ 7 ∷ []) ≡ (2 ∷ 3 ∷ 6 ∷ 7 ∷ 1 ∷ 5 ∷ 4 ∷ [])
_ = refl
