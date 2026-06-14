module solution where

open import Data.List using (List; []; _∷_; map; foldr; concatMap; length)
open import Data.Char using (Char)
open import Data.String using (String; toList; fromList)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- digit -> the letters it maps to on a phone keypad
letters : Char → List Char
letters '2' = 'a' ∷ 'b' ∷ 'c' ∷ []
letters '3' = 'd' ∷ 'e' ∷ 'f' ∷ []
letters '4' = 'g' ∷ 'h' ∷ 'i' ∷ []
letters '5' = 'j' ∷ 'k' ∷ 'l' ∷ []
letters '6' = 'm' ∷ 'n' ∷ 'o' ∷ []
letters '7' = 'p' ∷ 'q' ∷ 'r' ∷ 's' ∷ []
letters '8' = 't' ∷ 'u' ∷ 'v' ∷ []
letters '9' = 'w' ∷ 'x' ∷ 'y' ∷ []
letters _   = []

step : Char → List (List Char) → List (List Char)
step d acc = concatMap (λ l → map (l ∷_) acc) (letters d)

letterCombinations : String → List String
letterCombinations s with toList s
... | []      = []
... | (c ∷ cs) = map fromList (foldr step ([] ∷ []) (c ∷ cs))

-- compile-time tests: exact char-list output for "23" (matches foldr enumeration)
_ : foldr step ([] ∷ []) (toList "23")
  ≡ ('a' ∷ 'd' ∷ []) ∷ ('a' ∷ 'e' ∷ []) ∷ ('a' ∷ 'f' ∷ [])
  ∷ ('b' ∷ 'd' ∷ []) ∷ ('b' ∷ 'e' ∷ []) ∷ ('b' ∷ 'f' ∷ [])
  ∷ ('c' ∷ 'd' ∷ []) ∷ ('c' ∷ 'e' ∷ []) ∷ ('c' ∷ 'f' ∷ [])
  ∷ []
_ = refl

_ : length (letterCombinations "23") ≡ 9
_ = refl

_ : letterCombinations "" ≡ []
_ = refl
