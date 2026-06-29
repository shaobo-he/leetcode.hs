module solution where

------------------------------------------------------------------------
-- LeetCode 169: Majority Element — Boyer–Moore voting.
--
-- A total left fold (structural over the list, no pragma) carrying
-- (candidate, count).  `majorityElement` returns the candidate; on inputs
-- that have a strict majority (the LeetCode guarantee) it IS that element.
-- The full count-invariant proof of the correctness IFF lives in
-- solution.lean.
--
-- future work: port the Lean `bm_good` count invariant (snocRec induction)
-- to Agda; the runnable, refl-tested fold below is the must-have.
------------------------------------------------------------------------

open import Data.Nat using (ℕ; zero; suc; _≡ᵇ_)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Product using (_×_; _,_; proj₁)
open import Data.List using (List; []; _∷_; foldl)
open import Data.Bool using (if_then_else_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

step : Maybe ℕ × ℕ → ℕ → Maybe ℕ × ℕ
step (_       , zero)  x = just x , 1
step (just c  , suc k) x = if x ≡ᵇ c then (just c , suc (suc k)) else (just c , suc k)
step (nothing , suc k) x = just x , 1

bm : List ℕ → Maybe ℕ × ℕ
bm xs = foldl step (nothing , 0) xs

majorityElement : List ℕ → Maybe ℕ
majorityElement xs = proj₁ (bm xs)

-- compile-time tests
_ : majorityElement (3 ∷ 2 ∷ 3 ∷ []) ≡ just 3
_ = refl

_ : majorityElement (2 ∷ 2 ∷ 1 ∷ 1 ∷ 1 ∷ 2 ∷ 2 ∷ []) ≡ just 2
_ = refl

_ : majorityElement [] ≡ nothing
_ = refl
