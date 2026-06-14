module solution where

open import Data.Nat using (ℕ; _⊔_; _≤ᵇ_; _≤_; _<_)
open import Data.Nat.Properties
  using (≤-trans; <-trans; ≤-<-trans; m≤m⊔n; ≤⇒≤ᵇ; ≰⇒>)
open import Data.List using (List; []; _∷_; foldl; reverse)
open import Data.List.Base using (reverseAcc)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.Bool using (Bool; true; false; if_then_else_; T)
open import Relation.Nullary using (¬_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; subst)

-- An interval is a (lo , hi) pair of naturals.
Iv : Set
Iv = ℕ × ℕ

-- verified-style insertion sort by start (proj₁); structural, so total.
insertIv : Iv → List Iv → List Iv
insertIv x []       = x ∷ []
insertIv x (y ∷ ys) =
  if proj₁ x ≤ᵇ proj₁ y then x ∷ y ∷ ys
                        else y ∷ insertIv x ys

isort : List Iv → List Iv
isort []       = []
isort (x ∷ xs) = insertIv x (isort xs)

-- coalescing fold; accumulator kept reversed (head = most recent / largest start).
step : List Iv → Iv → List Iv
step []           iv = iv ∷ []
step (top ∷ rest) iv =
  if proj₁ iv ≤ᵇ proj₂ top
    then (proj₁ top , proj₂ top ⊔ proj₂ iv) ∷ rest
    else iv ∷ top ∷ rest

merge : List Iv → List Iv
merge xs = reverse (foldl step [] (isort xs))

-- compile-time tests: the standard example and the touching-endpoints case.
_ : merge ((1 , 3) ∷ (2 , 6) ∷ (8 , 10) ∷ (15 , 18) ∷ [])
  ≡ (1 , 6) ∷ (8 , 10) ∷ (15 , 18) ∷ []
_ = refl

_ : merge ((1 , 4) ∷ (4 , 5) ∷ []) ≡ (1 , 5) ∷ []
_ = refl

_ : merge [] ≡ []
_ = refl

------------------------------------------------------------------------
-- PROOF OF CORRECTNESS (port of the Lean `merge_outputs_WF` +
-- `merge_outputs_NoOverlap`, following the Idris `solution.idr`).
--
-- Intervals are ℕ × ℕ.  `WF (lo , hi) = lo ≤ hi`.  `NoOverlap` is an inductive
-- Pairwise relation over ALL pairs: an earlier interval `a` ends strictly
-- before a later interval `b` starts (`proj₂ a < proj₁ b`).
--
-- We prove, via the fold invariant (all-WF ∧ pairwise-separated) preserved by
-- `step` for any well-formed incoming interval (no sortedness needed — exactly
-- as in the Lean/Idris blueprints), that:
--   * mergeOutputsWF        : every interval in `merge xs` is WF, and
--   * mergeOutputsNoOverlap : `merge xs` is pairwise NoOverlap.
--
-- The sort (`isort`) stays an UNVERIFIED runnable detail: sortedness is not
-- part of the correctness argument.  Coverage/union (that the output covers
-- exactly the union of the inputs) is NOT proven — same caveat as Lean/Idris.
------------------------------------------------------------------------

-- An interval (lo , hi) is well-formed when lo ≤ hi.
WF : Iv → Set
WF iv = proj₁ iv ≤ proj₂ iv

------------------------------------------------------------------------
-- Bridge: the `step` overlap test branches on the Bool `proj₁ iv ≤ᵇ proj₂ top`.
-- When that Bool is `false`, `proj₂ top < proj₁ iv`.
------------------------------------------------------------------------

≤ᵇfalse⇒> : (a b : ℕ) → (a ≤ᵇ b) ≡ false → b < a
≤ᵇfalse⇒> a b eq = ≰⇒> a≰b
  where
    a≰b : ¬ (a ≤ b)
    a≰b a≤b = subst T eq (≤⇒≤ᵇ a≤b)

-- the coalesced interval (top.lo , max top.hi iv.hi) is WF, given top is WF.
wfMax : (lo hi h : ℕ) → lo ≤ hi → lo ≤ (hi ⊔ h)
wfMax lo hi h p = ≤-trans p (m≤m⊔n hi h)

------------------------------------------------------------------------
-- Invariant data types (mirror Lean's AccInv = all-WF AND pairwise-separated).
------------------------------------------------------------------------

-- AllWF xs : every interval in xs is well-formed.
data AllWF : List Iv → Set where
  AWNil  : AllWF []
  AWCons : ∀ {x xs} → WF x → AllWF xs → AllWF (x ∷ xs)

-- SepBelow z ws : every interval in ws ends strictly before z starts.
data SepBelow : Iv → List Iv → Set where
  SBNil  : ∀ {z} → SepBelow z []
  SBCons : ∀ {z w ws} → proj₂ w < proj₁ z → SepBelow z ws → SepBelow z (w ∷ ws)

-- SepSorted xs : pairwise separated -- on the reversed accumulator, each
-- earlier-pushed interval ends strictly before every more-recently-pushed one.
data SepSorted : List Iv → Set where
  SSNil  : SepSorted []
  SSCons : ∀ {top rest} → SepBelow top rest → SepSorted rest → SepSorted (top ∷ rest)

------------------------------------------------------------------------
-- `step` preserves (AllWF AND SepSorted) for any WELL-FORMED incoming iv.
-- This does NOT depend on sorted input -- the key insight of the Lean proof.
------------------------------------------------------------------------

-- When we coalesce, the new head is (top.lo , max top.hi iv.hi).  Every element
-- separated below `top` is still separated below the coalesced head, because
-- the coalesced head keeps top.lo as its start.
sepBelowCoalesce : ∀ {rest} (top iv : Iv) → SepBelow top rest →
                   SepBelow (proj₁ top , proj₂ top ⊔ proj₂ iv) rest
sepBelowCoalesce top iv SBNil            = SBNil
sepBelowCoalesce top iv (SBCons prf sb)  =
  SBCons prf (sepBelowCoalesce top iv sb)

-- AllWF preserved by step.
stepAllWF : (acc : List Iv) (iv : Iv) → AllWF acc → WF iv → AllWF (step acc iv)
stepAllWF []           iv AWNil                wfIv = AWCons wfIv AWNil
stepAllWF (top ∷ rest) iv (AWCons wfTop wfRest) wfIv
    with proj₁ iv ≤ᵇ proj₂ top
... | true  =
      -- coalesce: new head (top.lo , max top.hi iv.hi) is WF since top is.
      AWCons (wfMax (proj₁ top) (proj₂ top) (proj₂ iv) wfTop) wfRest
... | false = AWCons wfIv (AWCons wfTop wfRest)

-- SepSorted preserved by step.
stepSepSorted : (acc : List Iv) (iv : Iv) → AllWF acc → SepSorted acc → WF iv →
                SepSorted (step acc iv)
stepSepSorted []           iv AWNil                SSNil               wfIv =
  SSCons SBNil SSNil
stepSepSorted (top ∷ rest) iv (AWCons wfTop wfRest) (SSCons sbTop ssRest) wfIv
    with proj₁ iv ≤ᵇ proj₂ top in eq
... | true  =
      -- coalesce: head becomes (top.lo , max top.hi iv.hi); rest unchanged.
      SSCons (sepBelowCoalesce top iv sbTop) ssRest
... | false =
      -- no overlap: push iv as a new head, separated above top (and thus above
      -- everything in rest, since each is separated below top).
      SSCons (SBCons topHiLtIvLo (chain rest sbTop)) (SSCons sbTop ssRest)
  where
    topHiLtIvLo : proj₂ top < proj₁ iv
    topHiLtIvLo = ≤ᵇfalse⇒> (proj₁ iv) (proj₂ top) eq
    -- every w in rest has w.hi < top.lo ≤ top.hi < iv.lo, so w.hi < iv.lo.
    chain : (ws : List Iv) → SepBelow top ws → SepBelow iv ws
    chain []       SBNil           = SBNil
    chain (w ∷ ws) (SBCons wLt sb) =
      SBCons (<-trans wLt (≤-<-trans wfTop topHiLtIvLo)) (chain ws sb)

------------------------------------------------------------------------
-- Lift the invariant over foldl.
------------------------------------------------------------------------

foldlWF : (l acc : List Iv) → AllWF acc → SepSorted acc → AllWF l →
          AllWF (foldl step acc l)
foldlWF []       acc awAcc ssAcc AWNil            = awAcc
foldlWF (x ∷ xs) acc awAcc ssAcc (AWCons wfX awXs) =
  foldlWF xs (step acc x)
          (stepAllWF acc x awAcc wfX)
          (stepSepSorted acc x awAcc ssAcc wfX)
          awXs

foldlSS : (l acc : List Iv) → AllWF acc → SepSorted acc → AllWF l →
          SepSorted (foldl step acc l)
foldlSS []       acc awAcc ssAcc AWNil            = ssAcc
foldlSS (x ∷ xs) acc awAcc ssAcc (AWCons wfX awXs) =
  foldlSS xs (step acc x)
          (stepAllWF acc x awAcc wfX)
          (stepSepSorted acc x awAcc ssAcc wfX)
          awXs

------------------------------------------------------------------------
-- From the (reversed-accumulator) invariant to the final no-overlap relation.
-- The OUTPUT relation is an inductive Pairwise covering ALL pairs.
------------------------------------------------------------------------

-- NoOvBelow b xs : `b` ends strictly before every interval in xs starts.
data NoOvBelow : Iv → List Iv → Set where
  NOBNil  : ∀ {b} → NoOvBelow b []
  NOBCons : ∀ {b c cs} → proj₂ b < proj₁ c → NoOvBelow b cs → NoOvBelow b (c ∷ cs)

-- Pairwise NoOverlap: for every earlier `a` and later `b`, a.hi < b.lo.
data NoOverlap : List Iv → Set where
  NONil  : NoOverlap []
  NOCons : ∀ {a rest} → NoOvBelow a rest → NoOverlap rest → NoOverlap (a ∷ rest)

-- For every w in todo and every c in done: w.hi < c.lo.
data BeforeAll : List Iv → List Iv → Set where
  BANil  : ∀ {done} → BeforeAll [] done
  BACons : ∀ {w ws done} → NoOvBelow w done → BeforeAll ws done →
           BeforeAll (w ∷ ws) done

-- combine: w before top (from SepBelow top) and w before done (from BeforeAll)
mkBA : (rest : List Iv) (t : Iv) (ds : List Iv) →
       SepBelow t rest → BeforeAll rest ds → BeforeAll rest (t ∷ ds)
mkBA []       t ds SBNil           BANil              = BANil
mkBA (w ∷ ws) t ds (SBCons wLt sb) (BACons nobWDone baWs) =
  BACons (NOBCons wLt nobWDone) (mkBA ws t ds sb baWs)

-- If todo is SepSorted and BeforeAll todo done and done is NoOverlap, then
-- reversing todo onto done (`reverseAcc done todo`) yields NoOverlap.
revOnto : (todo done : List Iv) →
          SepSorted todo → NoOverlap done → BeforeAll todo done →
          NoOverlap (reverseAcc done todo)
revOnto []           done SSNil               noDone baDone = noDone
revOnto (top ∷ rest) done (SSCons sbTop ssRest) noDone (BACons nobTopDone baRest) =
  revOnto rest (top ∷ done) ssRest
          (NOCons nobTopDone noDone)
          (mkBA rest top done sbTop baRest)

------------------------------------------------------------------------
-- isort preserves AllWF (to feed the WELL-FORMED hypothesis through the sort
-- into the fold).  Proven structurally so the WF theorems remain honest.
------------------------------------------------------------------------

insertWF : (x : Iv) (l : List Iv) → WF x → AllWF l → AllWF (insertIv x l)
insertWF x []       wfX AWNil            = AWCons wfX AWNil
insertWF x (y ∷ ys) wfX (AWCons wfY awYs)
    with proj₁ x ≤ᵇ proj₁ y
... | true  = AWCons wfX (AWCons wfY awYs)
... | false = AWCons wfY (insertWF x ys wfX awYs)

isortWF : (l : List Iv) → AllWF l → AllWF (isort l)
isortWF []       AWNil            = AWNil
isortWF (x ∷ xs) (AWCons wfX awXs) = insertWF x (isort xs) wfX (isortWF xs awXs)

------------------------------------------------------------------------
-- Correctness theorems.
------------------------------------------------------------------------

-- The fold's accumulator is well-formed and pairwise-separated.
mergeAccWF : (xs : List Iv) → AllWF xs → AllWF (foldl step [] (isort xs))
mergeAccWF xs awXs = foldlWF (isort xs) [] AWNil SSNil (isortWF xs awXs)

mergeAccSS : (xs : List Iv) → AllWF xs → SepSorted (foldl step [] (isort xs))
mergeAccSS xs awXs = foldlSS (isort xs) [] AWNil SSNil (isortWF xs awXs)

-- helper: AllWF on a reversed list (reverseAcc with a WF accumulator).
revOntoWF : (todo done : List Iv) → AllWF todo → AllWF done →
            AllWF (reverseAcc done todo)
revOntoWF []       done AWNil            awDone = awDone
revOntoWF (x ∷ xs) done (AWCons wfX awXs) awDone =
  revOntoWF xs (x ∷ done) awXs (AWCons wfX awDone)

-- every list is BeforeAll the empty `done` list (NoOvBelow w [] is trivial).
beforeAllNil : (todo : List Iv) → BeforeAll todo []
beforeAllNil []       = BANil
beforeAllNil (w ∷ ws) = BACons NOBNil (beforeAllNil ws)

-- PROPERTY 1 -- every output interval is well-formed (lo ≤ hi).
mergeOutputsWF : (xs : List Iv) → AllWF xs → AllWF (merge xs)
mergeOutputsWF xs awXs =
  revOntoWF (foldl step [] (isort xs)) [] (mergeAccWF xs awXs) AWNil

-- PROPERTY 2 -- outputs are pairwise non-overlapping: for any earlier `a` and
-- later `b` in the result, a.hi < b.lo.
mergeOutputsNoOverlap : (xs : List Iv) → AllWF xs → NoOverlap (merge xs)
mergeOutputsNoOverlap xs awXs =
  revOnto (foldl step [] (isort xs)) [] (mergeAccSS xs awXs) NONil
          (beforeAllNil (foldl step [] (isort xs)))
