module solution where

-- N-Queens: count distinct solutions (port of the Lean/Idris solution).
-- A placement assigns one column per successive row; we enumerate permutations
-- of [1..n] (columns are then automatically distinct) and keep the diagonal-safe
-- ones, counting how many survive.

open import Data.Nat using (ℕ; suc; _∸_; _+_; _<_; _≡ᵇ_; s≤s)
open import Data.Nat.Properties using (≤-refl)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.List using (List; []; _∷_; map; concatMap; length; zip; all; filterᵇ; upTo)
open import Data.Bool using (Bool; true; false; not; _∧_)
open import Data.Product using (_×_; _,_; ∃)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

private
  variable
    A : Set

-- |a - b| for natural numbers
absDiff : ℕ → ℕ → ℕ
absDiff a b = (a ∸ b) + (b ∸ a)

-- Each pick of one element, paired with the remaining list AND a proof that the
-- remainder is shorter — so the recursion below is well-founded.
selections : (xs : List A) → List (A × ∃ λ (r : List A) → length r < length xs)
selections []       = []
selections (x ∷ xs) =
  (x , xs , ≤-refl) ∷ map (λ (a , r , pf) → (a , x ∷ r , s≤s pf)) (selections xs)

-- All permutations of a list, well-founded on its length; the carried proof
-- from `selections` discharges each step (the recursive call sits inside a
-- concatMap lambda, so it needs the decrease witness right there).
permute : List A → List (List A)
permute xs = go xs (<-wellFounded (length xs))
  where
    go : (xs : List A) → Acc _<_ (length xs) → List (List A)
    go []       _         = [] ∷ []
    go (x ∷ xs) (acc rec) =
      concatMap (λ (a , r , pf) → map (a ∷_) (go r (rec pf)))
                (selections (x ∷ xs))

-- Two queens at (r1,c1) and (r2,c2) don't attack diagonally.
ok : (ℕ × ℕ) → (ℕ × ℕ) → Bool
ok (r1 , c1) (r2 , c2) = not (absDiff r1 r2 ≡ᵇ absDiff c1 c2)

-- cols is the column chosen per row; pair with row indices 1..len and check
-- every pair of placements is diagonal-safe.
diagSafe : List ℕ → Bool
diagSafe cols = check (zip rows cols)
  where
    rows : List ℕ
    rows = map suc (upTo (length cols))
    check : List (ℕ × ℕ) → Bool
    check []       = true
    check (p ∷ ps) = all (ok p) ps ∧ check ps

queens : ℕ → List (List ℕ)
queens n = filterᵇ diagSafe (permute (map suc (upTo n)))

-- compile-time tests (refl fails to typecheck if the computation is wrong)
_ : length (queens 4) ≡ 2
_ = refl

_ : length (queens 5) ≡ 10
_ = refl

_ : length (queens 6) ≡ 4
_ = refl
