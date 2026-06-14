module solution where

-- Longest substring without repeating characters.
-- Sliding window: track the last-seen index per character in an assoc list;
-- when a repeat is seen, jump the window start past its previous position.

open import Data.Bool using (if_then_else_)
open import Data.Nat using (ℕ; zero; suc; _∸_; _⊔_)
open import Data.Char using (Char; _==_)
open import Data.List using (List; []; _∷_)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Product using (_×_; _,_)
open import Data.String using (String; toList)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- last-seen index per char as an assoc list
setIdx : Char → ℕ → List (Char × ℕ) → List (Char × ℕ)
setIdx c i []              = (c , i) ∷ []
setIdx c i ((d , j) ∷ rest) =
  if c == d then (c , i) ∷ rest else (d , j) ∷ setIdx c i rest

lookupIdx : Char → List (Char × ℕ) → Maybe ℕ
lookupIdx c []              = nothing
lookupIdx c ((d , j) ∷ rest) = if c == d then just j else lookupIdx c rest

-- Sliding window: jump start past any repeat of the current char.
go : ℕ → ℕ → List (Char × ℕ) → ℕ → List Char → ℕ
go _ _     _    best []       = best
go i start seen best (c ∷ cs) =
  let start' = jump (lookupIdx c seen)
  in go (suc i) start' (setIdx c i seen) (best ⊔ (suc i ∸ start')) cs
  where
    jump : Maybe ℕ → ℕ
    jump (just j) = start ⊔ suc j
    jump nothing  = start

lengthOfLongest : String → ℕ
lengthOfLongest s = go 0 0 [] 0 (toList s)

-- compile-time tests (same examples the other languages test)
_ : lengthOfLongest "abcabcbb" ≡ 3
_ = refl

_ : lengthOfLongest "bbbbb" ≡ 1
_ = refl

_ : lengthOfLongest "pwwkew" ≡ 3
_ = refl
