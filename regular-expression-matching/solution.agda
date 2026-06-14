module solution where

-- Brzozowski derivatives (port of the Lean/Idris/Haskell solution).
-- We match one character at a time, taking the derivative of the regex.

open import Data.Bool using (Bool; true; false; _∧_; _∨_; if_then_else_)
open import Data.Char using (Char; _==_)
open import Data.List using (List; []; _∷_; foldl)
open import Data.String using (String; toList)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

data RE : Set where
  empty : RE
  eps   : RE
  dot   : RE
  chr   : Char → RE
  seq   : RE → RE → RE
  alt   : RE → RE → RE
  star  : RE → RE

-- Does the regex match the empty string?
nullable : RE → Bool
nullable empty     = false
nullable eps       = true
nullable dot       = false
nullable (chr _)   = false
nullable (seq a b) = nullable a ∧ nullable b
nullable (alt a b) = nullable a ∨ nullable b
nullable (star _)  = true

-- Brzozowski derivative of a regex with respect to a character.
deriv : Char → RE → RE
deriv _ empty     = empty
deriv _ eps       = empty
deriv _ dot       = eps
deriv c (chr c')  = if c == c' then eps else empty
deriv c (alt a b) = alt (deriv c a) (deriv c b)
deriv c (seq a b) = if nullable a
                      then alt (seq (deriv c a) b) (deriv c b)
                      else seq (deriv c a) b
deriv c (star a)  = seq (deriv c a) (star a)

matchRE : List Char → RE → Bool
matchRE cs r = nullable (foldl (λ acc c → deriv c acc) r cs)

atom : Char → RE
atom c = if c == '.' then dot else chr c

-- The parser is not structurally recursive on its argument (the two-character
-- case 'c ∷ * ∷ rest' recurses on rest, the one-character case recurses on the
-- tail), so we mark it TERMINATING (it does terminate: every clause shrinks the
-- list). This mirrors the `partial def parse` in Lean / total-but-covering Idris.
{-# TERMINATING #-}
parse : List Char → RE
parse []                = eps
parse (c ∷ '*' ∷ rest)  = seq (star (atom c)) (parse rest)
parse (c ∷ rest)        = seq (atom c) (parse rest)

isMatch : String → String → Bool
isMatch s p = matchRE (toList s) (parse (toList p))

-- compile-time tests (refl fails to typecheck if the computation is wrong)
_ : isMatch "aa" "a" ≡ false
_ = refl

_ : isMatch "aa" "a*" ≡ true
_ = refl

_ : isMatch "ab" ".*" ≡ true
_ = refl

_ : isMatch "aab" "c*a*b" ≡ true
_ = refl

_ : isMatch "mississippi" "mis*is*p*." ≡ false
_ = refl
