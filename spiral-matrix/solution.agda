module solution where

open import Data.Nat using (ℕ; suc; _+_; _≤_; _<_; z≤n; s≤s)
open import Data.Nat.Properties using (≤-refl; ≤-trans; ≤-reflexive; m≤n+m;
                                       +-assoc; +-comm; +-identityʳ; +-monoʳ-≤)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.List using (List; []; _∷_; _++_; _∷ʳ_; map; reverse; mapMaybe; length)
open import Data.List.Properties using (unfold-reverse)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; sym; trans)

private
  variable
    A : Set

headᵐ : List A → Maybe A
headᵐ []      = nothing
headᵐ (x ∷ _) = just x

tailᴸ : List A → List A
tailᴸ []       = []
tailᴸ (_ ∷ xs) = xs

-- stdlib has no List transpose, so define our own (matches the Lean version).
-- Total by well-founded recursion on the first row's length: each step drops a
-- column (`map tailᴸ`), shrinking that length until a row empties.
firstRow : List (List A) → List A
firstRow []      = []
firstRow (r ∷ _) = r

transposeGo : (rs : List (List A)) → Acc _<_ (length (firstRow rs)) → List (List A)
transposeGo []             _         = []
transposeGo ([] ∷ _)       _         = []
transposeGo ((x ∷ r) ∷ rs) (acc rec) =
  mapMaybe headᵐ ((x ∷ r) ∷ rs) ∷ transposeGo (map tailᴸ ((x ∷ r) ∷ rs)) (rec ≤-refl)

transpose : List (List A) → List (List A)
transpose rows = transposeGo rows (<-wellFounded (length (firstRow rows)))

-- total number of elements (sum of row lengths)
sumLen : List (List A) → ℕ
sumLen []       = 0
sumLen (r ∷ rs) = length r + sumLen rs

-- peeling one column (heads) and shortening the rest (tails) preserves the count
heads-tails : (M : List (List A)) →
    length (mapMaybe headᵐ M) + sumLen (map tailᴸ M) ≡ sumLen M
heads-tails []             = refl
heads-tails ([] ∷ rs)      = heads-tails rs
heads-tails ((x ∷ r) ∷ rs) =
  cong suc
    (trans (sym (+-assoc a b c))
      (trans (cong (_+ c) (+-comm a b))
        (trans (+-assoc b a c) (cong (b +_) (heads-tails rs)))))
  where
    a = length (mapMaybe headᵐ rs)
    b = length r
    c = sumLen (map tailᴸ rs)

-- transpose never manufactures elements
transposeGo-sumLen : (rs : List (List A)) (ac : Acc _<_ (length (firstRow rs))) →
    sumLen (transposeGo rs ac) ≤ sumLen rs
transposeGo-sumLen []             _         = z≤n
transposeGo-sumLen ([] ∷ rs)      _         = z≤n
transposeGo-sumLen ((x ∷ r) ∷ rs) (acc rec) =
  ≤-trans (+-monoʳ-≤ (length (mapMaybe headᵐ ((x ∷ r) ∷ rs)))
                     (transposeGo-sumLen (map tailᴸ ((x ∷ r) ∷ rs)) (rec ≤-refl)))
          (≤-reflexive (heads-tails ((x ∷ r) ∷ rs)))

transpose-sumLen : (rows : List (List A)) → sumLen (transpose rows) ≤ sumLen rows
transpose-sumLen rows = transposeGo-sumLen rows (<-wellFounded (length (firstRow rows)))

-- reverse permutes rows, so the element count is unchanged
sumLen-snoc : (M : List (List A)) (x : List A) → sumLen (M ∷ʳ x) ≡ sumLen M + length x
sumLen-snoc []      x = +-identityʳ (length x)
sumLen-snoc (m ∷ M) x =
  trans (cong (length m +_) (sumLen-snoc M x))
        (sym (+-assoc (length m) (sumLen M) (length x)))

sumLen-reverse : (M : List (List A)) → sumLen (reverse M) ≡ sumLen M
sumLen-reverse []       = refl
sumLen-reverse (x ∷ xs) =
  trans (cong sumLen (unfold-reverse x xs))
    (trans (sumLen-snoc (reverse xs) x)
      (trans (cong (_+ length x) (sumLen-reverse xs))
             (+-comm (sumLen xs) (length x))))

-- Peel the first row, rotate the rest counter-clockwise (reverse ∘ transpose),
-- recurse.  Total by well-founded recursion on the total element count: peeling
-- a non-empty row drops it, and reverse/transpose only rearrange (≤) the rest.
spiralOrder : List (List A) → List A
spiralOrder M = go M (<-wellFounded (sumLen M))
  where
    go : (M : List (List A)) → Acc _<_ (sumLen M) → List A
    go []               _         = []
    go ([] ∷ _)         _         = []
    go ((x ∷ r) ∷ rows) (acc rec) =
      (x ∷ r) ++ go (reverse (transpose rows))
        (rec (s≤s (≤-trans (≤-trans (≤-reflexive (sumLen-reverse (transpose rows)))
                                    (transpose-sumLen rows))
                           (m≤n+m (sumLen rows) (length r)))))

-- compile-time tests: same exact outputs the other languages assert.
_ : spiralOrder ((1 ∷ 2 ∷ 3 ∷ []) ∷ (4 ∷ 5 ∷ 6 ∷ []) ∷ (7 ∷ 8 ∷ 9 ∷ []) ∷ [])
    ≡ (1 ∷ 2 ∷ 3 ∷ 6 ∷ 9 ∷ 8 ∷ 7 ∷ 4 ∷ 5 ∷ [])
_ = refl

_ : spiralOrder ((1 ∷ 2 ∷ 3 ∷ 4 ∷ []) ∷ (5 ∷ 6 ∷ 7 ∷ 8 ∷ []) ∷ (9 ∷ 10 ∷ 11 ∷ 12 ∷ []) ∷ [])
    ≡ (1 ∷ 2 ∷ 3 ∷ 4 ∷ 8 ∷ 12 ∷ 11 ∷ 10 ∷ 9 ∷ 5 ∷ 6 ∷ 7 ∷ [])
_ = refl
