module solution where

-- Recursive-descent calculator (port of the Lean/Idris solution).
-- expr = term (('+'|'-') term)* ; term = factor (('*'|'/') factor)*
-- factor = '(' expr ')' | number
-- Values are integers (ℤ) because subexpressions can be negative, e.g. (0-3)/2.

open import Data.Bool using (Bool; true; false; _xor_; if_then_else_)
open import Data.Nat using (ℕ; zero; suc; _∸_) renaming (_*_ to _*ℕ_; _+_ to _+ℕ_; _/_ to _/ℕ_)
open import Data.Integer using (ℤ; +_; -[1+_]; _+_; _-_; _*_; ∣_∣; -_)
open import Data.Char using (Char; isDigit; toℕ)
open import Data.List using (List; []; _∷_)
open import Data.String using (String; toList)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- skip leading spaces
{-# TERMINATING #-}
dropSpaces : List Char → List Char
dropSpaces (' ' ∷ cs) = dropSpaces cs
dropSpaces cs         = cs

-- natural division returning 0 on a zero divisor (avoids a NonZero obligation;
-- the divisor is never zero in well-formed input).
divN : ℕ → ℕ → ℕ
divN a zero    = zero
divN a (suc m) = a /ℕ (suc m)

isNeg : ℤ → Bool
isNeg (+_ _)   = false
isNeg -[1+ _ ] = true

-- integer division truncated toward zero (LeetCode semantics)
tdiv : ℤ → ℤ → ℤ
tdiv a b = let q = + (divN ∣ a ∣ ∣ b ∣) in
           if (isNeg a) xor (isNeg b) then - q else q

charDigit : Char → ℕ
charDigit c = toℕ c ∸ toℕ '0'

-- parse a non-negative integer literal
parseDigits : ℕ → List Char → ℕ × List Char
parseDigits acc []       = acc , []
parseDigits acc (c ∷ cs) with isDigit c
... | true  = parseDigits (acc *ℕ 10 +ℕ charDigit c) cs
... | false = acc , (c ∷ cs)

parseNumber : List Char → ℤ × List Char
parseNumber cs = let r = parseDigits 0 cs in (+ proj₁ r) , proj₂ r

-- The recursive-descent grammar is left-recursive-free but consumes the input
-- across mutually recursive calls, so it is not structurally recursive; mark
-- TERMINATING (mirrors `partial def` in Lean and covering Idris definitions).
{-# TERMINATING #-}
mutual
  parseExpr : List Char → ℤ × List Char
  parseExpr cs = let r = parseTerm cs in exprTail (proj₁ r) (dropSpaces (proj₂ r))

  exprTail : ℤ → List Char → ℤ × List Char
  exprTail acc ('+' ∷ cs) = let r = parseTerm cs in exprTail (acc + proj₁ r) (dropSpaces (proj₂ r))
  exprTail acc ('-' ∷ cs) = let r = parseTerm cs in exprTail (acc - proj₁ r) (dropSpaces (proj₂ r))
  exprTail acc cs         = acc , cs

  parseTerm : List Char → ℤ × List Char
  parseTerm cs = let r = parseFactor cs in termTail (proj₁ r) (dropSpaces (proj₂ r))

  termTail : ℤ → List Char → ℤ × List Char
  termTail acc ('*' ∷ cs) = let r = parseFactor cs in termTail (acc * proj₁ r) (dropSpaces (proj₂ r))
  termTail acc ('/' ∷ cs) = let r = parseFactor cs in termTail (tdiv acc (proj₁ r)) (dropSpaces (proj₂ r))
  termTail acc cs         = acc , cs

  parseFactor : List Char → ℤ × List Char
  parseFactor cs0 with dropSpaces cs0
  ... | ('(' ∷ rest) =
          let r = parseExpr rest in
          dropParen (proj₁ r) (dropSpaces (proj₂ r))
  ... | cs = parseNumber cs

  -- consume a matching ')' if present
  dropParen : ℤ → List Char → ℤ × List Char
  dropParen v (')' ∷ rest) = v , rest
  dropParen v other        = v , other

calculate : String → ℤ
calculate s = proj₁ (parseExpr (toList s))

-- compile-time tests (refl fails to typecheck if the computation is wrong)
_ : calculate "1+1" ≡ + 2
_ = refl

_ : calculate "6-4/2" ≡ + 4
_ = refl

_ : calculate "2*(5+5*2)+3*5" ≡ + 45
_ = refl

_ : calculate "14-3/2" ≡ + 13
_ = refl

_ : calculate "(0-3)/2" ≡ (+ 0 - + 1)
_ = refl
