module solution where

-- LeetCode 200: Number of Islands
-- Flood-fill DFS over a grid of '1' (land) / '0' (water).
-- Grid is a List (List Char) (rows).  Coordinates are ℕ; we guard every
-- neighbour move with explicit bounds so we never read out of range and never
-- underflow (no spurious wrap of r ∸ 1 / c ∸ 1 at the 0 edge).

open import Data.Nat using (ℕ; zero; suc; _≡ᵇ_; _<ᵇ_; pred)
open import Data.Bool using (Bool; true; false; if_then_else_; _∧_; _∨_; not)
open import Data.Char using (Char; _==_)
open import Data.String using (String; toList)
open import Data.List using (List; []; _∷_; map; foldl; length; upTo; concatMap)
open import Data.Product using (_×_; _,_; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

Grid : Set
Grid = List (List Char)

Pos : Set
Pos = ℕ × ℕ

-- index into a list, nothing-style default '0' (water) when out of range.
nth0 : ℕ → List Char → Char
nth0 _       []       = '0'
nth0 zero    (x ∷ _)  = x
nth0 (suc n) (_ ∷ xs) = nth0 n xs

nthRow : ℕ → Grid → List Char
nthRow _       []       = []
nthRow zero    (x ∷ _)  = x
nthRow (suc n) (_ ∷ xs) = nthRow n xs

cellAt : Grid → ℕ → ℕ → Char
cellAt g r c = nth0 c (nthRow r g)

posEq : Pos → Pos → Bool
posEq (a , b) (x , y) = (a ≡ᵇ x) ∧ (b ≡ᵇ y)

memPos : Pos → List Pos → Bool
memPos _ []       = false
memPos p (q ∷ qs) = posEq p q ∨ memPos p qs

-- flood fill: add every connected land cell to the visited set.
-- Not structurally recursive (recurses on neighbours), so TERMINATING.
{-# TERMINATING #-}
flood : Grid → ℕ → ℕ → ℕ → ℕ → List Pos → List Pos
flood g rows cols r c visited =
  if not (cellAt g r c == '1') ∨ memPos (r , c) visited
  then visited
  else
    let v0 = (r , c) ∷ visited
        -- down: r+1, guarded by r+1 < rows
        v1 = if (suc r) <ᵇ rows then flood g rows cols (suc r) c v0 else v0
        -- up: r-1, guarded by r > 0 (i.e. 0 < r)
        v2 = if zero <ᵇ r       then flood g rows cols (pred r) c v1 else v1
        -- right: c+1, guarded by c+1 < cols
        v3 = if (suc c) <ᵇ cols then flood g rows cols r (suc c) v2 else v2
        -- left: c-1, guarded by c > 0
        v4 = if zero <ᵇ c       then flood g rows cols r (pred c) v3 else v3
    in v4

numCols : Grid → ℕ
numCols []      = 0
numCols (r ∷ _) = length r

step : Grid → ℕ → ℕ → (List Pos × ℕ) → Pos → (List Pos × ℕ)
step g rows cols (visited , count) (r , c) =
  if (cellAt g r c == '1') ∧ not (memPos (r , c) visited)
  then (flood g rows cols r c visited , suc count)
  else (visited , count)

numIslands : Grid → ℕ
numIslands g =
  let rows   = length g
      cols   = numCols g
      coords = concatMap (λ r → map (λ c → (r , c)) (upTo cols)) (upTo rows)
  in proj₂ (foldl (step g rows cols) ([] , 0) coords)

mkGrid : List String → Grid
mkGrid = map toList

-- compile-time tests: same grids / counts the other languages assert.
_ : numIslands (mkGrid ("11110" ∷ "11010" ∷ "11000" ∷ "00000" ∷ [])) ≡ 1
_ = refl

_ : numIslands (mkGrid ("11000" ∷ "11000" ∷ "00100" ∷ "00011" ∷ [])) ≡ 3
_ = refl
