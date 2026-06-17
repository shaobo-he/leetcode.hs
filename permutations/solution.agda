module solution where

open import Data.Nat using (ℕ; _<_; s≤s)
open import Data.Nat.Properties using (≤-refl)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.List using (List; []; _∷_; map; concatMap; length)
open import Data.Product using (_×_; _,_; ∃)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

private
  variable
    A : Set

-- Each element picked as head, paired with the remaining list AND a proof that
-- the remainder is strictly shorter.  Carrying the proof through `selections`
-- is what lets the recursion below be well-founded (no pragma): the
-- recursive call happens inside a `concatMap` lambda, so it needs the decrease
-- witness right there.
selections : (xs : List A) → List (A × ∃ λ (r : List A) → length r < length xs)
selections []       = []
selections (x ∷ xs) =
  (x , xs , ≤-refl) ∷ map (λ (a , r , pf) → (a , x ∷ r , s≤s pf)) (selections xs)

-- All permutations: pick each element as the head, recurse on the rest.
-- Well-founded on the list length; the carried proof discharges each step.
permute : List A → List (List A)
permute xs = go xs (<-wellFounded (length xs))
  where
    go : (xs : List A) → Acc _<_ (length xs) → List (List A)
    go []       _         = [] ∷ []
    go (x ∷ xs) (acc rec) =
      concatMap (λ (a , r , pf) → map (a ∷_) (go r (rec pf)))
                (selections (x ∷ xs))

-- compile-time tests: 3 distinct elements -> 3! = 6 permutations.
-- Exact enumeration order matches the .hs/.idr/.lean "pick head, recurse" scheme.
_ : permute (1 ∷ 2 ∷ 3 ∷ [])
  ≡ (1 ∷ 2 ∷ 3 ∷ []) ∷ (1 ∷ 3 ∷ 2 ∷ [])
  ∷ (2 ∷ 1 ∷ 3 ∷ []) ∷ (2 ∷ 3 ∷ 1 ∷ [])
  ∷ (3 ∷ 1 ∷ 2 ∷ []) ∷ (3 ∷ 2 ∷ 1 ∷ [])
  ∷ []
_ = refl

_ : length (permute (1 ∷ 2 ∷ 3 ∷ [])) ≡ 6
_ = refl
