module solution where

-- LeetCode 322: Coin Change — fewest coins summing to `amount`, or `nothing`
-- when impossible.  TOTAL by well-founded recursion on `amount` (no TERMINATING):
-- for each coin `c` with 1 ≤ c ≤ amount we recurse on `amount ∸ c`, which is
-- strictly smaller (`∸-<-lemma`), and take the best with `listMin`.  This mirrors
-- the proven Lean recurrence; the Acc pattern follows permutations-ii / course-schedule.

open import Data.Nat using (ℕ; zero; suc; _∸_; _<_; _≤_; _⊓_; s≤s; z≤n; _≤?_)
open import Data.Nat.Properties using (m∸n≤m)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.List using (List; []; _∷_; map; foldr)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Nullary using (yes; no)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- min over a list of `Maybe ℕ`, with `nothing` = "unreachable" (identity).
omin : Maybe ℕ → Maybe ℕ → Maybe ℕ
omin nothing  y        = y
omin (just a) nothing  = just a
omin (just a) (just b) = just (a ⊓ b)

listMin : List (Maybe ℕ) → Maybe ℕ
listMin = foldr omin nothing

mmap : (ℕ → ℕ) → Maybe ℕ → Maybe ℕ
mmap f nothing  = nothing
mmap f (just x) = just (f x)

-- the well-founded measure decreases: subtracting a positive coin ≤ n shrinks n.
∸-<-lemma : (n c' : ℕ) → suc c' ≤ n → n ∸ suc c' < n
∸-<-lemma (suc m) c' (s≤s _) = s≤s (m∸n≤m m c')

-- the recurrence, total by structural Acc on the amount.
coinChangeGo : List ℕ → (n : ℕ) → Acc _<_ n → Maybe ℕ
coinChangeGo coins zero    _         = just 0
coinChangeGo coins (suc n) (acc rec) = listMin (map cand coins)
  where
    cand : ℕ → Maybe ℕ
    cand zero      = nothing                       -- value-0 coins excluded
    cand (suc c') with suc c' ≤? suc n
    ... | no  _   = nothing                        -- c > amount: not usable
    ... | yes c≤n =
            mmap suc (coinChangeGo coins (suc n ∸ suc c')
                       (rec (∸-<-lemma (suc n) c' c≤n)))

coinChange : List ℕ → ℕ → Maybe ℕ
coinChange coins n = coinChangeGo coins n (<-wellFounded n)

-- compile-time tests (the Acc recursion reduces on closed inputs).
_ : coinChange (1 ∷ 2 ∷ 5 ∷ []) 0 ≡ just 0
_ = refl

_ : coinChange (1 ∷ []) 0 ≡ just 0
_ = refl

_ : coinChange (2 ∷ []) 3 ≡ nothing
_ = refl

_ : coinChange (1 ∷ 2 ∷ 5 ∷ []) 6 ≡ just 2
_ = refl

_ : coinChange (1 ∷ []) 2 ≡ just 2
_ = refl
