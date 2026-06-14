module solution where

-- Count Unique Characters of All Substrings (LeetCode 828).
-- Each occurrence of a letter at index i contributes (i - prev) * (next - i),
-- where prev/next are the indices of the same letter on either side (or -1 / n).
-- Summing these over every occurrence counts, for each substring, how many of
-- its letters are unique.

open import Data.Bool using (if_then_else_)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Integer using (ℤ; +_; -[1+_]; _-_; _*_; _+_) renaming (0ℤ to zeroℤ)
open import Data.Char using (Char; _==_)
open import Data.List using (List; []; _∷_; length; _++_; map; foldr; drop)
open import Data.Product using (_×_; _,_; proj₂)
open import Data.String using (String; toList)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- collect the indices (ascending) at which each char occurs
appendIdx : Char → ℕ → List (Char × List ℕ) → List (Char × List ℕ)
appendIdx c i []              = (c , i ∷ []) ∷ []
appendIdx c i ((d , is) ∷ rest) =
  if c == d then (d , is ++ (i ∷ [])) ∷ rest else (d , is) ∷ appendIdx c i rest

positionsOf : List Char → List (Char × List ℕ)
positionsOf cs = go 0 [] cs
  where
    go : ℕ → List (Char × List ℕ) → List Char → List (Char × List ℕ)
    go _ acc []        = acc
    go i acc (c ∷ rest) = go (suc i) (appendIdx c i acc) rest

-- a 3-way zip-and-sum of (i - l) * (r - i) over aligned (ls , is , rs)
zipContrib : List ℤ → List ℤ → List ℤ → ℤ
zipContrib (l ∷ ls) (i ∷ is) (r ∷ rs) = ((i - l) * (r - i)) + zipContrib ls is rs
zipContrib _        _        _        = zeroℤ

-- each occurrence contributes (i - prev) * (next - i), prev/next = -1 / n
contrib : ℕ → List ℕ → ℤ
contrib n idxs =
  let is = map +_ idxs
  in zipContrib (-[1+ 0 ] ∷ is) is (drop 1 is ++ (+ n ∷ []))

uniqueLetterString : String → ℤ
uniqueLetterString s =
  let cs = toList s
      n  = length cs
  in foldr (λ p acc → contrib n (proj₂ p) + acc) zeroℤ (positionsOf cs)

-- compile-time tests (same examples the other languages test)
_ : uniqueLetterString "ABC" ≡ + 10
_ = refl

_ : uniqueLetterString "ABA" ≡ + 8
_ = refl

_ : uniqueLetterString "LEETCODE" ≡ + 92
_ = refl
