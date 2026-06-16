module solution where

open import Data.Nat using (ℕ; suc; _≤_; _<_; _≤ᵇ_; z≤n; s≤s)
open import Data.Nat.Properties using (m≤n⇒m≤1+n)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.List using (List; []; _∷_; length)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.Bool using (if_then_else_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

private
  variable
    A : Set

-- merge two sorted lists.  Structural on the combined "fuel" via the pair
-- pattern: we recurse on a strictly smaller argument in each clause.
mergeTwo : List ℕ → List ℕ → List ℕ
mergeTwo []       ys       = ys
mergeTwo (x ∷ xs) []       = x ∷ xs
mergeTwo (x ∷ xs) (y ∷ ys) =
  if x ≤ᵇ y then x ∷ mergeTwo xs (y ∷ ys)
            else y ∷ mergeTwo (x ∷ xs) ys

-- Split a list into two by alternating elements.  Structural, and (unlike
-- splitAt ⌊n/2⌋) gives clean length bounds without any division arithmetic.
-- The grouping is irrelevant: mergeK still merges every sublist pairwise, so the
-- fully sorted result is the same as a halving split.
splitAlt : List A → List A × List A
splitAlt []           = [] , []
splitAlt (x ∷ [])     = (x ∷ []) , []
splitAlt (x ∷ y ∷ xs) = (x ∷ proj₁ (splitAlt xs)) , (y ∷ proj₂ (splitAlt xs))

splitAlt-len₁ : (xs : List A) → length (proj₁ (splitAlt xs)) ≤ length xs
splitAlt-len₁ []           = z≤n
splitAlt-len₁ (x ∷ [])     = s≤s z≤n
splitAlt-len₁ (x ∷ y ∷ xs) = s≤s (m≤n⇒m≤1+n (splitAlt-len₁ xs))

splitAlt-len₂ : (xs : List A) → length (proj₂ (splitAlt xs)) ≤ length xs
splitAlt-len₂ []           = z≤n
splitAlt-len₂ (x ∷ [])     = z≤n
splitAlt-len₂ (x ∷ y ∷ xs) = s≤s (m≤n⇒m≤1+n (splitAlt-len₂ xs))

-- Divide and conquer: pairwise-merge the two halves.  Total by well-founded
-- recursion on the number of lists — once there are ≥ 2, each half is strictly
-- shorter (the head two elements are split apart, the rest distributed).
mergeK : List (List ℕ) → List ℕ
mergeK xss = go xss (<-wellFounded (length xss))
  where
    go : (xss : List (List ℕ)) → Acc _<_ (length xss) → List ℕ
    go []             _         = []
    go (xs ∷ [])      _         = xs
    go (a ∷ b ∷ rest) (acc rec) =
      mergeTwo (go (proj₁ (splitAlt (a ∷ b ∷ rest))) (rec (s≤s (s≤s (splitAlt-len₁ rest)))))
               (go (proj₂ (splitAlt (a ∷ b ∷ rest))) (rec (s≤s (s≤s (splitAlt-len₂ rest)))))

-- compile-time test: same example the other languages assert.
_ : mergeK ((1 ∷ 4 ∷ 5 ∷ []) ∷ (1 ∷ 3 ∷ 4 ∷ []) ∷ (2 ∷ 6 ∷ []) ∷ [])
    ≡ (1 ∷ 1 ∷ 2 ∷ 3 ∷ 4 ∷ 4 ∷ 5 ∷ 6 ∷ [])
_ = refl
