module solution where

-- Serialize and Deserialize N-ary Tree (port of the Lean/Idris/Haskell solution).
-- Pre-order parenthesised encoding: (val child1 child2 ...).

open import Data.Bool using (Bool; true; false; _∧_)
open import Data.Nat using (ℕ; zero; suc; _∸_; _+_; _*_)
open import Data.Nat.Show renaming (show to showNat)
open import Data.Char using (Char; isDigit; toℕ)
open import Data.List using (List; []; _∷_)
open import Data.String using (String; toList; fromList; _++_)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

data Tree : Set where
  node : ℕ → List Tree → Tree

-- serialize is structurally recursive on the tree (children are smaller),
-- but Agda's termination checker cannot see through the `map` /`foldr`, so we
-- inline a helper `concatChildren` that recurses structurally on the list.
mutual
  serialize : Tree → String
  serialize (node v cs) = "(" ++ showNat v ++ concatChildren cs ++ ")"

  concatChildren : List Tree → String
  concatChildren []       = ""
  concatChildren (c ∷ cs) = serialize c ++ concatChildren cs

-- Parse a non-negative integer (the trees in the examples use non-negative values).
charDigit : Char → ℕ
charDigit c = toℕ c ∸ toℕ '0'

parseDigits : ℕ → List Char → ℕ × List Char
parseDigits acc []       = acc , []
parseDigits acc (c ∷ cs) with isDigit c
... | true  = parseDigits (acc * 10 + charDigit c) cs
... | false = acc , (c ∷ cs)

parseInt : List Char → ℕ × List Char
parseInt cs = parseDigits 0 cs

-- The mutual parser recurses on suffixes of the list, not structurally, so we
-- mark it TERMINATING (mirrors `partial def parseTree`/`parseChildren` in Lean).
{-# TERMINATING #-}
mutual
  parseTree : List Char → Tree × List Char
  parseTree ('(' ∷ rest) =
    let vr = parseInt rest
        cr = parseChildren (proj₂ vr)
    in node (proj₁ vr) (proj₁ cr) , proj₂ cr
  parseTree cs = node 0 [] , cs

  parseChildren : List Char → List Tree × List Char
  parseChildren (')' ∷ rest) = [] , rest
  parseChildren ('(' ∷ rest) =
    let tr = parseTree ('(' ∷ rest)
        cr = parseChildren (proj₂ tr)
    in (proj₁ tr ∷ proj₁ cr) , proj₂ cr
  parseChildren cs = [] , cs

deserialize : String → Tree
deserialize s = proj₁ (parseTree (toList s))

t : Tree
t = node 1 (node 3 (node 5 [] ∷ node 6 [] ∷ []) ∷ node 2 [] ∷ node 4 [] ∷ [])

-- compile-time tests (refl fails to typecheck if the computation is wrong)
_ : serialize t ≡ "(1(3(5)(6))(2)(4))"
_ = refl

_ : serialize (deserialize (serialize t)) ≡ serialize t
_ = refl
