module solution where

-- LeetCode 31: Next Permutation — a purely functional recast over `List ℕ`
-- (non-negative integers; all the standard test cases are non-negative).
--
-- Every function is TOTAL by structure (no TERMINATING pragma): `findPivot`
-- recurses on the tail `y ∷ rest ⊏ x ∷ y ∷ rest`, `swapLast` on `rest ⊏ c ∷ rest`.
-- Tests are compile-time `refl` assertions.  The Perm / Lex correctness proofs
-- are carried by the Lean centerpiece (solution.lean).

open import Data.Nat using (ℕ; zero; suc; _<ᵇ_)
open import Data.List using (List; []; _∷_; _++_; reverse)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Product using (_×_; _,_)
open import Data.Bool using (Bool; true; false; if_then_else_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

-- Rightmost-ascent split: xs = pre ++ p ∷ suf, suf maximal weakly-decreasing,
-- p < head suf.  nothing ⇒ xs is weakly decreasing (already maximal).
findPivot : List ℕ → Maybe (List ℕ × ℕ × List ℕ)
findPivot []           = nothing
findPivot (_ ∷ [])     = nothing
findPivot (x ∷ y ∷ rest) with findPivot (y ∷ rest)
... | just (pre , p , suf) = just (x ∷ pre , p , suf)
... | nothing = if x <ᵇ y then just ([] , x , y ∷ rest) else nothing

-- replace the rightmost element > p with p; return (that element , new suffix)
swapLast : ℕ → List ℕ → Maybe (ℕ × List ℕ)
swapLast p []         = nothing
swapLast p (c ∷ rest) with swapLast p rest
... | just (g , rest') = just (g , c ∷ rest')
... | nothing = if p <ᵇ c then just (c , p ∷ rest) else nothing

nextPermutation : List ℕ → List ℕ
nextPermutation xs with findPivot xs
... | nothing            = reverse xs
... | just (pre , p , suf) with swapLast p suf
...   | just (g , suf') = pre ++ g ∷ reverse suf'
...   | nothing         = reverse xs

-- Proof 1: findPivot recovers the structural split  xs ≡ pre ++ p ∷ suf.
findPivotEq : (xs pre suf : List ℕ) (p : ℕ) →
              findPivot xs ≡ just (pre , p , suf) → xs ≡ pre ++ p ∷ suf
findPivotEq [] pre suf p ()
findPivotEq (x ∷ []) pre suf p ()
findPivotEq (x ∷ y ∷ rest) pre suf p eq with findPivot (y ∷ rest) in eqf
findPivotEq (x ∷ y ∷ rest) .(x ∷ pre') .suf' .p' refl | just (pre' , p' , suf') =
  cong (x ∷_) (findPivotEq (y ∷ rest) pre' suf' p' eqf)
findPivotEq (x ∷ y ∷ rest) pre suf p eq | nothing with x <ᵇ y
findPivotEq (x ∷ y ∷ rest) .[] .(y ∷ rest) .x refl | nothing | true = refl
findPivotEq (x ∷ y ∷ rest) pre suf p () | nothing | false

-- Proof 2: the swapped-in element strictly exceeds the pivot p.
swapLastGt : (p : ℕ) (l : List ℕ) (g : ℕ) (l' : List ℕ) →
             swapLast p l ≡ just (g , l') → (p <ᵇ g) ≡ true
swapLastGt p [] g l' ()
swapLastGt p (c ∷ rest) g l' eq with swapLast p rest in eqs
swapLastGt p (c ∷ rest) .g0 .(c ∷ r0) refl | just (g0 , r0) =
  swapLastGt p rest g0 r0 eqs
swapLastGt p (c ∷ rest) g l' eq | nothing with p <ᵇ c in eqc
swapLastGt p (c ∷ rest) .c .(p ∷ rest) refl | nothing | true = eqc
swapLastGt p (c ∷ rest) g l' () | nothing | false

-- compile-time tests (the #guard / rackunit analogues)
_ : nextPermutation (1 ∷ 2 ∷ 3 ∷ []) ≡ (1 ∷ 3 ∷ 2 ∷ [])
_ = refl

_ : nextPermutation (3 ∷ 2 ∷ 1 ∷ []) ≡ (1 ∷ 2 ∷ 3 ∷ [])   -- wrap
_ = refl

_ : nextPermutation (1 ∷ 1 ∷ 5 ∷ []) ≡ (1 ∷ 5 ∷ 1 ∷ [])
_ = refl

_ : nextPermutation (1 ∷ 3 ∷ 2 ∷ []) ≡ (2 ∷ 1 ∷ 3 ∷ [])
_ = refl

_ : nextPermutation (1 ∷ 5 ∷ 1 ∷ []) ≡ (5 ∷ 1 ∷ 1 ∷ [])
_ = refl
