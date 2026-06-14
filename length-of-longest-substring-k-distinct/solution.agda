module solution where

-- LeetCode 340: Longest Substring with At Most K Distinct Characters.
-- Sliding window holding at most k distinct chars, with per-char counts;
-- shrink from the left whenever the window has more than k distinct chars.

open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Nat using (ℕ; zero; suc; _⊔_)
open import Data.Nat.Base using (_<ᵇ_)
open import Data.Char using (Char; _==_)
open import Data.List using (List; []; _∷_; length; _++_)
open import Data.Product using (_×_; _,_)
open import Data.String using (String; toList)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- per-char occurrence counts as an assoc list
incr : Char → List (Char × ℕ) → List (Char × ℕ)
incr x []              = (x , 1) ∷ []
incr x ((y , n) ∷ rest) =
  if x == y then (y , suc n) ∷ rest else (y , n) ∷ incr x rest

decr : Char → List (Char × ℕ) → List (Char × ℕ)
decr x []              = []
decr x ((y , n) ∷ rest) =
  if x == y
    then dropOne n
    else (y , n) ∷ decr x rest
  where
    dropOne : ℕ → List (Char × ℕ)
    dropOne (suc (suc k)) = (y , suc k) ∷ rest
    dropOne _             = rest

-- shrink from the left while the window holds more than k distinct chars.
-- structural on `win`: the recursive call drops the head `d`.
shrinkW : ℕ → List Char → List (Char × ℕ) → (List Char × List (Char × ℕ))
shrinkW k []       counts = ([] , counts)
shrinkW k (d ∷ ds) counts =
  if k <ᵇ length counts
    then shrinkW k ds (decr d counts)
    else (d ∷ ds , counts)

goW : ℕ → List Char → List (Char × ℕ) → ℕ → List Char → ℕ
goW _ _   _      best []       = best
goW k win counts best (c ∷ cs) =
  let r = shrinkW k (win ++ (c ∷ [])) (incr c counts)
  in step r
  where
    step : (List Char × List (Char × ℕ)) → ℕ
    step (win2 , counts2) = goW k win2 counts2 (best ⊔ length win2) cs

lenKDistinct : String → ℕ → ℕ
lenKDistinct s k = goW k [] [] 0 (toList s)

-- compile-time tests (same examples the other languages test)
_ : lenKDistinct "eceba" 2 ≡ 3
_ = refl

_ : lenKDistinct "aa" 1 ≡ 2
_ = refl

_ : lenKDistinct "abee" 1 ≡ 2
_ = refl
