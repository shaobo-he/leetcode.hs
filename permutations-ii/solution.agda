module solution where

open import Data.Nat using (ℕ; zero; suc; _∸_; _+_; _<_; _≤_; _≡ᵇ_; s≤s; z≤n)
open import Data.Nat.Properties using
  (≤-refl; ≤-<-trans; +-monoʳ-≤; +-mono-≤; +-mono-≤-<; +-monoʳ-<; m∸n≤m)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.List using (List; []; _∷_; map; concatMap; foldl; all; length)
open import Data.List.Membership.Propositional using (_∈_)
open import Data.List.Relation.Unary.Any using (here; there)
open import Data.Product using (_×_; _,_; proj₁; proj₂; Σ; Σ-syntax)
open import Data.Bool using (Bool; true; false; if_then_else_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- multiset over ℕ as an assoc list of (value, count)
bump : ℕ → List (ℕ × ℕ) → List (ℕ × ℕ)
bump x []             = (x , 1) ∷ []
bump x ((y , n) ∷ rest) =
  if x ≡ᵇ y then (y , suc n) ∷ rest
            else (y , n) ∷ bump x rest

countsOf : List ℕ → List (ℕ × ℕ)
countsOf = foldl (λ cnts x → bump x cnts) []

decrCount : ℕ → List (ℕ × ℕ) → List (ℕ × ℕ)
decrCount x = map (λ p → if proj₁ p ≡ᵇ x then (proj₁ p , proj₂ p ∸ 1) else p)

-- total remaining count: the well-founded measure
totalCount : List (ℕ × ℕ) → ℕ
totalCount []             = 0
totalCount ((_ , n) ∷ cs) = n + totalCount cs

≡ᵇ-refl : (n : ℕ) → (n ≡ᵇ n) ≡ true
≡ᵇ-refl zero    = refl
≡ᵇ-refl (suc n) = ≡ᵇ-refl n

-- decrementing a key never grows the total …
totalCount-le : (x : ℕ) (cs : List (ℕ × ℕ)) → totalCount (decrCount x cs) ≤ totalCount cs
totalCount-le x []             = ≤-refl
totalCount-le x ((y , m) ∷ cs) with y ≡ᵇ x
... | true  = +-mono-≤ (m∸n≤m m 1) (totalCount-le x cs)
... | false = +-mono-≤ ≤-refl      (totalCount-le x cs)

-- … and strictly shrinks it when a present key has a positive count
totalCount-lt : (x k : ℕ) (cs : List (ℕ × ℕ)) → (x , suc k) ∈ cs
              → totalCount (decrCount x cs) < totalCount cs
totalCount-lt x k (_ ∷ cs) (here refl) rewrite ≡ᵇ-refl x =
  ≤-<-trans (+-monoʳ-≤ k (totalCount-le x cs)) ≤-refl
totalCount-lt x k ((y , m) ∷ cs) (there mem) with y ≡ᵇ x
... | true  = +-mono-≤-< (m∸n≤m m 1) (totalCount-lt x k cs mem)
... | false = +-monoʳ-< m            (totalCount-lt x k cs mem)

-- pair every element with its membership proof (the Agda analogue of Lean `.attach`)
attach : ∀ {A : Set} (xs : List A) → List (Σ[ a ∈ A ] (a ∈ xs))
attach []       = []
attach (x ∷ xs) = (x , here refl) ∷ map (λ p → proj₁ p , there (proj₂ p)) (attach xs)

-- Pick each distinct value with a positive count as the next element, recurse.
-- TOTAL by well-founded recursion on `totalCount`: decrementing a present,
-- positive count strictly drops the total (totalCount-lt).  `attach` carries the
-- membership proof that lemma needs, and matching the count as `suc k` supplies
-- the positivity — so the count-0 entries take the empty branch with no proof.
permsFromCounts : List (ℕ × ℕ) → List (List ℕ)
permsFromCounts cs0 = go cs0 (<-wellFounded (totalCount cs0))
  where
    go : (cs : List (ℕ × ℕ)) → Acc _<_ (totalCount cs) → List (List ℕ)
    go cs (acc rec) =
      if all (λ p → proj₂ p ≡ᵇ 0) cs
        then [] ∷ []
        else concatMap
               (λ where
                  ((x , zero)  , _)   → []
                  ((x , suc k) , mem) →
                    map (x ∷_) (go (decrCount x cs) (rec (totalCount-lt x k cs mem))))
               (attach cs)

permuteUnique : List ℕ → List (List ℕ)
permuteUnique xs = permsFromCounts (countsOf xs)

-- compile-time tests: [1,1,2] has exactly 3 unique permutations, in this order.
_ : permuteUnique (1 ∷ 1 ∷ 2 ∷ [])
  ≡ (1 ∷ 1 ∷ 2 ∷ []) ∷ (1 ∷ 2 ∷ 1 ∷ []) ∷ (2 ∷ 1 ∷ 1 ∷ []) ∷ []
_ = refl

_ : length (permuteUnique (1 ∷ 1 ∷ 2 ∷ [])) ≡ 3
_ = refl
