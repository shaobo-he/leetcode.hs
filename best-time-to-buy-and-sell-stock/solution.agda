module solution where

open import Data.Nat using (ℕ; zero; suc; _⊔_; _⊓_; _∸_)
open import Data.List using (List; []; _∷_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- single pass: `lowest` = min price so far, `best` = best profit so far (monus).
go : ℕ → ℕ → List ℕ → ℕ
go lowest best []       = best
go lowest best (x ∷ xs) = go (lowest ⊓ x) (best ⊔ (x ∸ lowest)) xs

maxProfit : List ℕ → ℕ
maxProfit []       = 0
maxProfit (p ∷ ps) = go p 0 ps

-- compile-time tests (refl fails to typecheck if the computation is wrong)
_ : maxProfit (7 ∷ 1 ∷ 5 ∷ 3 ∷ 6 ∷ 4 ∷ []) ≡ 5
_ = refl

_ : maxProfit (7 ∷ 6 ∷ 4 ∷ 3 ∷ 1 ∷ []) ≡ 0
_ = refl
