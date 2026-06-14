module solution where

open import Data.Nat using (ℕ; zero; suc)
open import Data.List using (List; []; _∷_; map; _++_; length)
open import Data.Char using (Char)
open import Data.String using (String; fromList)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- Grammar of valid parentheses, generated structurally.
-- State: `o` = opens still to place, `d` = current nesting depth.
-- Recursion is structural (on o, then on d), so Agda accepts it as total.
genB : ℕ → ℕ → List (List Char)
genB zero    zero     = [] ∷ []
genB zero    (suc d') = map (')' ∷_) (genB zero d')
genB (suc o') zero    = map ('(' ∷_) (genB o' 1)
genB (suc o') (suc d') = map ('(' ∷_) (genB o' (suc (suc d')))
                      ++ map (')' ∷_) (genB (suc o') d')

generateParenthesis : ℕ → List String
generateParenthesis n = map fromList (genB n zero)

-- compile-time tests: same counts the other languages assert
-- generateParenthesis 3 has 5 results, generateParenthesis 4 has 14 (Catalan).
_ : length (generateParenthesis 3) ≡ 5
_ = refl

_ : length (generateParenthesis 4) ≡ 14
_ = refl

-- exact char-list output for n = 2 (matches the genB enumeration order)
_ : genB 2 0 ≡ ('(' ∷ '(' ∷ ')' ∷ ')' ∷ [])
              ∷ ('(' ∷ ')' ∷ '(' ∷ ')' ∷ [])
              ∷ []
_ = refl
