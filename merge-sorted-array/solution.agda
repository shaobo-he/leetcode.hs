module solution where

-- LeetCode 88: Merge Sorted Array — merge two sorted lists.
-- Mirrors the Haskell/Lean/Idris `sortedMerge`: walk both lists, always
-- emitting the smaller head, until one side is exhausted.
--
-- This file also VERIFIES the merge, porting the Lean `sortedMerge_sorted`
-- and Idris `mergeSorted`: for any total + transitive comparator `le`,
-- merging two SORTED lists yields a SORTED list.  "Sorted" is an inductive
-- pairwise relation (the Idris `AllLE`/`Sorted` form).  The spec is then
-- instantiated for ℕ with `≤` (analog of Lean `sortedMerge_sorted_nat`).

open import Data.Bool using (Bool; true; false; T; if_then_else_)
open import Data.Nat using (ℕ; _≤_; _≤ᵇ_; _<_; _+_; suc; s≤s)
open import Data.Nat.Properties using (≤ᵇ⇒≤; ≤⇒≤ᵇ; ≤-trans; ≤-total; n<1+n; +-suc)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.List using (List; []; _∷_; _++_; length)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Unit using (tt)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl)
open import Data.List.Properties using (++-identityʳ)
open import Data.List.Relation.Binary.Permutation.Propositional
  using (_↭_; refl; prep; trans; ↭-sym; ↭-reflexive)
open import Data.List.Relation.Binary.Permutation.Propositional.Properties
  using (shift)

private
  variable
    A : Set

------------------------------------------------------------------------
-- The merge (the LeetCode solution), generic over a comparator `le`.
-- `sortedMerge` below is just `mergeL _≤ᵇ_` (definitionally equal).
------------------------------------------------------------------------

mergeL : (le : A → A → Bool) → List A → List A → List A
mergeL le []       ys       = ys
mergeL le (x ∷ xs) []       = x ∷ xs
mergeL le (x ∷ xs) (y ∷ ys) =
  if le x y
    then x ∷ mergeL le xs (y ∷ ys)
    else y ∷ mergeL le (x ∷ xs) ys

sortedMerge : List ℕ → List ℕ → List ℕ
sortedMerge = mergeL _≤ᵇ_

-- compile-time tests (same examples the other languages test)
_ : sortedMerge (1 ∷ 2 ∷ 3 ∷ []) (2 ∷ 5 ∷ 6 ∷ []) ≡ (1 ∷ 2 ∷ 2 ∷ 3 ∷ 5 ∷ 6 ∷ [])
_ = refl

_ : sortedMerge (1 ∷ 3 ∷ 4 ∷ []) (2 ∷ 5 ∷ 6 ∷ []) ≡ (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ 6 ∷ [])
_ = refl

------------------------------------------------------------------------
-- Sortedness as an inductive pairwise relation (Idris `AllLE`/`Sorted`).
-- Pattern matching recovers the indices, so no explicit element field is
-- needed (unlike the Idris workaround).
------------------------------------------------------------------------

-- `AllLE le z l`: z ≤ every element of l.
data AllLE (le : A → A → Bool) (z : A) : List A → Set where
  ALNil  : AllLE le z []
  ALCons : ∀ {w ws} → T (le z w) → AllLE le z ws → AllLE le z (w ∷ ws)

-- `Sorted le l`: head ≤ all of tail, recursively.
data Sorted (le : A → A → Bool) : List A → Set where
  SNil  : Sorted le []
  SCons : ∀ {x xs} → AllLE le x xs → Sorted le xs → Sorted le (x ∷ xs)

------------------------------------------------------------------------
-- Order lemmas, ported from the Idris proof.
------------------------------------------------------------------------

-- a lower bound for both inputs is a lower bound for their merge.
-- The boolean `le x y` is taken as an explicit argument of the worker so the
-- two-way (lexicographic) recursion stays structurally visible to Agda — a
-- top-level `with le x y` would defeat the termination checker here.
allMergeLE-aux :
  (le : A → A → Bool) (z x y : A) {xs ys : List A} (b : Bool) →
  AllLE le z (x ∷ xs) → AllLE le z (y ∷ ys) →
  AllLE le z (if b then x ∷ mergeL le xs (y ∷ ys)
                   else y ∷ mergeL le (x ∷ xs) ys)

allMergeLE : (le : A → A → Bool) (z : A) {xs ys : List A} →
             AllLE le z xs → AllLE le z ys → AllLE le z (mergeL le xs ys)
allMergeLE le z ALNil           pys           = pys
allMergeLE le z (ALCons px pxs) ALNil         = ALCons px pxs
allMergeLE le z {x ∷ xs} {y ∷ ys} (ALCons px pxs) (ALCons py pys) =
  allMergeLE-aux le z x y (le x y) (ALCons px pxs) (ALCons py pys)

allMergeLE-aux le z x y true  (ALCons px pxs) pyAll =
  ALCons px (allMergeLE le z pxs pyAll)
allMergeLE-aux le z x y false pxAll (ALCons py pys) =
  ALCons py (allMergeLE le z pxAll pys)

-- transitivity lowers a bound: x ≤ y and y ≤ all ys  ⇒  x ≤ all ys
allTransLE : (le : A → A → Bool) (x y : A) →
             (tr : ∀ u v w → T (le u v) → T (le v w) → T (le u w)) →
             T (le x y) → ∀ {ys} → AllLE le y ys → AllLE le x ys
allTransLE le x y tr lexy ALNil              = ALNil
allTransLE le x y tr lexy (ALCons pyw ps) =
  ALCons (tr x y _ lexy pyw) (allTransLE le x y tr lexy ps)

------------------------------------------------------------------------
-- THE SPEC (port of Lean `sortedMerge_sorted`, Idris `mergeSorted`):
-- merging two sorted lists yields a sorted list, for a total + transitive
-- comparator.
------------------------------------------------------------------------

-- Worker dispatching on `b = le x y` (kept explicit, with its defining
-- equation `b ≡ le x y`, so the lexicographic recursion stays structural).
-- The full `Sorted (x ∷ xs)` / `Sorted (y ∷ ys)` proofs are passed whole so
-- that the round-trip with `mergeSorted` exhibits a strict structural descent.
mergeSorted-aux :
  (le : A → A → Bool) →
  (tot : ∀ u v → T (le u v) ⊎ T (le v u)) →
  (tr  : ∀ u v w → T (le u v) → T (le v w) → T (le u w)) →
  ∀ {x y xs ys} (b : Bool) → b ≡ le x y →
  AllLE le x xs → Sorted le (x ∷ xs) → AllLE le y ys → Sorted le (y ∷ ys) →
  Sorted le (if b then x ∷ mergeL le xs (y ∷ ys)
                  else y ∷ mergeL le (x ∷ xs) ys)

mergeSorted : (le : A → A → Bool) →
              (tot : ∀ u v → T (le u v) ⊎ T (le v u)) →
              (tr  : ∀ u v w → T (le u v) → T (le v w) → T (le u w)) →
              ∀ {xs ys} → Sorted le xs → Sorted le ys →
              Sorted le (mergeL le xs ys)
mergeSorted le tot tr SNil            sy            = sy
mergeSorted le tot tr (SCons axs sxs) SNil          = SCons axs sxs
mergeSorted le tot tr {x ∷ xs} {y ∷ ys}
            sx@(SCons axs sxs) sy@(SCons ays sys) =
  mergeSorted-aux le tot tr (le x y) refl axs sx ays sy

mergeSorted-aux le tot tr {x} {y} true eq axs (SCons _ sxs) ays sy =
      SCons (allMergeLE le x axs (ALCons lexy (allTransLE le x y tr lexy ays)))
            (mergeSorted le tot tr sxs sy)
  where
    lexy : T (le x y)
    lexy rewrite (Eq.sym eq) = tt
mergeSorted-aux le tot tr {x} {y} false eq axs sx ays (SCons _ sys) =
      SCons (allMergeLE le y (ALCons leyx (allTransLE le y x tr leyx axs)) ays)
            (mergeSorted le tot tr sx sys)
  where
    -- `le x y` is false, so totality forces `le y x`
    leyx : T (le y x)
    leyx with tot x y
    ... | inj₁ lxy rewrite (Eq.sym eq) = ⊥-elim lxy
    ... | inj₂ lyx = lyx

------------------------------------------------------------------------
-- Instantiation for ℕ with `≤` (analog of Lean `sortedMerge_sorted_nat`).
-- The solution's comparator is `_≤ᵇ_`; it is total and transitive via the
-- stdlib `_≤ᵇ_`↔`_≤_` bridge.
------------------------------------------------------------------------

≤ᵇ-total : ∀ u v → T (u ≤ᵇ v) ⊎ T (v ≤ᵇ u)
≤ᵇ-total u v with ≤-total u v
... | inj₁ u≤v = inj₁ (≤⇒≤ᵇ u≤v)
... | inj₂ v≤u = inj₂ (≤⇒≤ᵇ v≤u)

≤ᵇ-trans : ∀ u v w → T (u ≤ᵇ v) → T (v ≤ᵇ w) → T (u ≤ᵇ w)
≤ᵇ-trans u v w u≤v v≤w =
  ≤⇒≤ᵇ (≤-trans (≤ᵇ⇒≤ u v u≤v) (≤ᵇ⇒≤ v w v≤w))

-- merging two `≤`-sorted ℕ lists yields a `≤`-sorted ℕ list
sortedMerge-sorted-ℕ : ∀ {xs ys} →
                       Sorted _≤ᵇ_ xs → Sorted _≤ᵇ_ ys →
                       Sorted _≤ᵇ_ (sortedMerge xs ys)
sortedMerge-sorted-ℕ sx sy = mergeSorted _≤ᵇ_ ≤ᵇ-total ≤ᵇ-trans sx sy

------------------------------------------------------------------------
-- BONUS (port of Lean `sortedMerge_perm`): the merge is a permutation of
-- the concatenation of its inputs — so it holds exactly the right
-- elements, with multiplicity.
------------------------------------------------------------------------

-- Dispatcher on `b = le x y`, given both recursive results.  This isolates
-- the `if` so the `mergeL (x ∷ xs) (y ∷ ys)` clause reduces.
permGo : (le : A → A → Bool) (b : Bool) (x y : A) (xs ys : List A) →
         mergeL le xs (y ∷ ys) ↭ (xs ++ (y ∷ ys)) →
         mergeL le (x ∷ xs) ys ↭ ((x ∷ xs) ++ ys) →
         (if b then x ∷ mergeL le xs (y ∷ ys)
               else y ∷ mergeL le (x ∷ xs) ys)
         ↭ ((x ∷ xs) ++ (y ∷ ys))
permGo le true  x y xs ys ih1 ih2 = prep x ih1
permGo le false x y xs ys ih1 ih2 =
  -- y ∷ ((x ∷ xs) ++ ys)  ↭  (x ∷ xs) ++ (y ∷ ys)
  trans (prep y ih2) (↭-sym (shift y (x ∷ xs) ys))

-- The two-list merge's lexicographic recursion defeats Agda's `with`-based
-- termination checker, so we recurse on a well-founded `<` measure (the
-- combined length).  This is genuine structural recursion on the `Acc`
-- witness — no `TERMINATING`/`postulate`.
mergeL-perm-wf : (le : A → A → Bool) (xs ys : List A) →
                 Acc _<_ (length xs + length ys) →
                 mergeL le xs ys ↭ (xs ++ ys)
mergeL-perm-wf le []       ys       _       = refl
mergeL-perm-wf le (x ∷ xs) []       _       =
  ↭-reflexive (Eq.cong (x ∷_) (Eq.sym (++-identityʳ xs)))
mergeL-perm-wf le (x ∷ xs) (y ∷ ys) (acc rs) =
  permGo le (le x y) x y xs ys
    -- length xs + suc (length ys) < suc (length xs + suc (length ys))
    (mergeL-perm-wf le xs (y ∷ ys)
       (rs (n<1+n (length xs + suc (length ys)))))
    -- suc (length xs) + length ys < suc (length xs) + suc (length ys)
    (mergeL-perm-wf le (x ∷ xs) ys
       (rs (s≤s (Eq.subst (λ z → length xs + length ys < z)
                          (Eq.sym (+-suc (length xs) (length ys)))
                          (n<1+n (length xs + length ys))))))

mergeL-perm : (le : A → A → Bool) (xs ys : List A) →
              mergeL le xs ys ↭ (xs ++ ys)
mergeL-perm le xs ys = mergeL-perm-wf le xs ys (<-wellFounded _)

sortedMerge-perm-ℕ : (xs ys : List ℕ) → sortedMerge xs ys ↭ (xs ++ ys)
sortedMerge-perm-ℕ = mergeL-perm _≤ᵇ_
