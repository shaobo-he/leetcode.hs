module solution where

-- Hash complement lookup via an assoc list of value -> index seen so far.
-- Inputs are ℤ since `target - x` may be negative.

open import Data.Integer using (ℤ; _-_; _≤ᵇ_)
open import Data.Nat using (ℕ; zero; suc)
open import Data.List using (List; []; _∷_)
open import Data.Product using (_×_; _,_)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Bool using (Bool; true; false; _∧_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- Bool equality on ℤ via antisymmetry of _≤ᵇ_.
_≡ᵇℤ_ : ℤ → ℤ → Bool
x ≡ᵇℤ y = (x ≤ᵇ y) ∧ (y ≤ᵇ x)

-- find the index of the first key in `seen` equal to `k`.
lookupℤ : ℤ → List (ℤ × ℕ) → Maybe ℕ
lookupℤ k [] = nothing
lookupℤ k ((v , j) ∷ rest) with k ≡ᵇℤ v
... | true  = just j
... | false = lookupℤ k rest

twoSum : List ℤ → ℤ → Maybe (ℕ × ℕ)
twoSum nums target = go 0 [] nums
  where
    go : ℕ → List (ℤ × ℕ) → List ℤ → Maybe (ℕ × ℕ)
    go i seen [] = nothing
    go i seen (x ∷ xs) with lookupℤ (target - x) seen
    ... | just j  = just (j , i)
    ... | nothing = go (suc i) ((x , i) ∷ seen) xs

-- compile-time tests (refl fails to typecheck if the computation is wrong)
open import Data.Integer using (+_)

_ : twoSum (+ 2 ∷ + 7 ∷ + 11 ∷ + 15 ∷ []) (+ 9) ≡ just (0 , 1)
_ = refl

_ : twoSum (+ 3 ∷ + 2 ∷ + 4 ∷ []) (+ 6) ≡ just (1 , 2)
_ = refl
