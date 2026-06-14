module Main

------------------------------------------------------------------------
-- LeetCode 56: Merge Intervals -- with an Idris 2 proof of correctness,
-- porting the Lean proof in solution.lean.
--
-- Algorithm: sort intervals by start, then left-fold a coalescing `step`
-- that keeps the accumulator *reversed* (head = most-recently-added =
-- largest start). Finally un-reverse.
--
-- We prove three properties:
--   1. mergeHandlesWF      -- well-formed inputs (lo <= hi) are handled:
--                             the (unverified) sort drops/invents nothing
--                             relevant; the part the proof actually relies on
--                             is that `step` preserves the invariant for any
--                             WELL-FORMED incoming interval (no sortedness
--                             needed -- exactly as in the Lean proof).
--   2. mergeOutputsWF      -- every output interval is well-formed (lo <= hi).
--   3. mergeOutputsNoOverlap -- the outputs are pairwise non-overlapping: for
--                             any earlier `a` and later `b` in the result,
--                             a.snd < b.fst  (a ends strictly before b starts).
--
-- We use Nat intervals so that the structural comparisons below REDUCE inside
-- proofs (Idris has no `omega`).  Every lemma/proof function is marked `total`
-- and contains none of: believe_me, really_believe_me, assert_total,
-- assert_smaller, idris_crash, postulate, or holes.  (The whole file in fact
-- passes the global `idris2 --check --total`, which totality-checks the
-- executable algorithm too.)
------------------------------------------------------------------------

Iv : Type
Iv = (Nat, Nat)

------------------------------------------------------------------------
-- Structural Bool comparisons on Nat (these reduce in proofs, unlike the
-- comparison-based stdlib `<=`/`max`, which are not lawful for our needs).
------------------------------------------------------------------------

total
lteNat : Nat -> Nat -> Bool
lteNat Z     _     = True
lteNat (S _) Z     = False
lteNat (S m) (S n) = lteNat m n

total
ltNat : Nat -> Nat -> Bool
ltNat a b = lteNat (S a) b

total
maxNat : Nat -> Nat -> Nat
maxNat Z     b     = b
maxNat a     Z     = a
maxNat (S a) (S b) = S (maxNat a b)

-- An interval (lo, hi) is well-formed when lo <= hi.
WF : Iv -> Type
WF iv = (lteNat (fst iv) (snd iv) = True)

------------------------------------------------------------------------
-- Arithmetic lemmas (short structural inductions; all total).
------------------------------------------------------------------------

total
lteTrans : (a, b, c : Nat) -> lteNat a b = True -> lteNat b c = True -> lteNat a c = True
lteTrans Z     b     c     _   _   = Refl
lteTrans (S a) (S b) (S c) p   q   = lteTrans a b c p q
lteTrans (S a) Z     c     p   _   = absurd p
lteTrans (S a) (S b) Z     _   q   = absurd q

-- the no-overlap branch test `not (lo <= top.hi)` gives `top.hi < lo`.
total
lteFalseToLt : (a, b : Nat) -> lteNat a b = False -> ltNat b a = True
lteFalseToLt Z     b     prf = absurd prf
lteFalseToLt (S a) Z     prf = Refl
lteFalseToLt (S a) (S b) prf = lteFalseToLt a b prf

total
maxLeR : (hi, h : Nat) -> lteNat hi (maxNat hi h) = True
maxLeR Z     h     = Refl
maxLeR (S hi) Z     = lteRefl hi
  where
    lteRefl : (n : Nat) -> lteNat n n = True
    lteRefl Z     = Refl
    lteRefl (S m) = lteRefl m
maxLeR (S hi) (S h) = maxLeR hi h

-- the coalesced interval (top.fst, max top.snd iv.snd) is WF, given top is WF.
total
wfMax : (lo, hi, h : Nat) -> lteNat lo hi = True -> lteNat lo (maxNat hi h) = True
wfMax lo hi h p = lteTrans lo hi (maxNat hi h) p (maxLeR hi h)

total
lteLtTrans : (a, b, c : Nat) -> lteNat a b = True -> ltNat b c = True -> ltNat a c = True
lteLtTrans a b c p q = lteTrans (S a) (S b) c p q

total
ltTrans : (a, b, c : Nat) -> ltNat a b = True -> ltNat b c = True -> ltNat a c = True
ltTrans a b c p q = lteTrans (S a) b c p (lteTrans b (S b) c (lteSucc b) q)
  where
    lteSucc : (n : Nat) -> lteNat n (S n) = True
    lteSucc Z     = Refl
    lteSucc (S m) = lteSucc m

------------------------------------------------------------------------
-- The coalescing fold (accumulator kept reversed) -- mirrors Lean `step`.
------------------------------------------------------------------------

-- head = most-recently-added = largest start so far.  Coalesce if the new
-- start is <= the current top's end; otherwise push the new interval.
total
step : List Iv -> Iv -> List Iv
step []            iv = [iv]
step (top :: rest) iv =
  if lteNat (fst iv) (snd top)
    then (fst top, maxNat (snd top) (snd iv)) :: rest
    else iv :: top :: rest

------------------------------------------------------------------------
-- Invariant data types (mirror Lean's AccInv = all-WF AND pairwise-separated).
-- Explicit `(w : Iv)` / `(top : Iv)` fields so the elements are NOT erased and
-- we can use them at runtime in `lteNat`/`ltNat` (Idris erasure gotcha).
------------------------------------------------------------------------

-- AllWF xs : every interval in xs is well-formed.
data AllWF : List Iv -> Type where
  AWNil  : AllWF []
  AWCons : (x : Iv) -> WF x -> AllWF xs -> AllWF (x :: xs)

-- SepBelow z ws : every interval in ws ends strictly before z starts.
data SepBelow : Iv -> List Iv -> Type where
  SBNil  : SepBelow z []
  SBCons : (w : Iv) -> ltNat (snd w) (fst z) = True -> SepBelow z ws -> SepBelow z (w :: ws)

-- SepSorted xs : pairwise separated -- the head ends after all of the tail
-- start (i.e. on the reversed accumulator, each earlier-pushed interval ends
-- strictly before every more-recently-pushed one starts).
data SepSorted : List Iv -> Type where
  SSNil  : SepSorted []
  SSCons : (top : Iv) -> SepBelow top rest -> SepSorted rest -> SepSorted (top :: rest)

------------------------------------------------------------------------
-- `step` preserves (AllWF AND SepSorted) for any WELL-FORMED incoming iv.
-- This does NOT depend on sorted input -- exactly the key insight of the Lean
-- proof.  We prove the two halves and combine.
------------------------------------------------------------------------

-- When we coalesce, the new head is (top.fst, max top.snd iv.snd).  Every
-- element that was separated below `top` is still separated below the coalesced
-- head, because the coalesced head keeps top.fst as its start.
total
sepBelowCoalesce : (top, iv : Iv) -> SepBelow top rest ->
                   SepBelow (fst top, maxNat (snd top) (snd iv)) rest
sepBelowCoalesce top iv SBNil = SBNil
sepBelowCoalesce top iv (SBCons w prf rest') =
  -- prf : ltNat (snd w) (fst top) = True ; goal start is also fst top
  SBCons w prf (sepBelowCoalesce top iv rest')

-- AllWF preserved by step.
total
stepAllWF : (acc : List Iv) -> (iv : Iv) -> AllWF acc -> WF iv -> AllWF (step acc iv)
stepAllWF []            iv AWNil               wfIv = AWCons iv wfIv AWNil
stepAllWF (top :: rest) iv (AWCons top wfTop wfRest) wfIv with (lteNat (fst iv) (snd top))
  _ | True  =
        -- coalesce: new head (top.fst, max top.snd iv.snd) is WF since top is.
        AWCons (fst top, maxNat (snd top) (snd iv))
               (wfMax (fst top) (snd top) (snd iv) wfTop)
               wfRest
  _ | False = AWCons iv wfIv (AWCons top wfTop wfRest)

-- SepSorted preserved by step.
total
stepSepSorted : (acc : List Iv) -> (iv : Iv) -> AllWF acc -> SepSorted acc -> WF iv ->
                SepSorted (step acc iv)
stepSepSorted []            iv AWNil SSNil wfIv = SSCons iv SBNil SSNil
stepSepSorted (top :: rest) iv (AWCons top wfTop wfRest) (SSCons top sbTop ssRest) wfIv
    with (lteNat (fst iv) (snd top)) proof prf
  _ | True  =
        -- coalesce: head becomes (top.fst, max top.snd iv.snd); rest unchanged.
        -- Its SepBelow comes from `top`'s, with the same start (top.fst).
        SSCons (fst top, maxNat (snd top) (snd iv))
               (sepBelowCoalesce top iv sbTop)
               ssRest
  _ | False =
        -- no overlap: push iv as a new head, separated above top (and thus
        -- above everything in rest, since each is separated below top).
        let topHiLtIvLo : (ltNat (snd top) (fst iv) = True)
            topHiLtIvLo = lteFalseToLt (fst iv) (snd top) prf
            sbIv : SepBelow iv (top :: rest)
            sbIv = SBCons top topHiLtIvLo
                          (sepBelowChain top iv rest sbTop wfTop topHiLtIvLo)
        in SSCons iv sbIv (SSCons top sbTop ssRest)
  where
    -- every w in rest has w.snd < top.fst <= top.snd < iv.fst, so w.snd < iv.fst.
    sepBelowChain : (top, iv : Iv) -> (ws : List Iv) -> SepBelow top ws ->
                    WF top -> ltNat (snd top) (fst iv) = True -> SepBelow iv ws
    sepBelowChain top iv []        SBNil               wfTop thLt = SBNil
    sepBelowChain top iv (w :: ws) (SBCons w wLt sbWs) wfTop thLt =
      -- wLt : w.snd < top.fst ; wfTop : top.fst <= top.snd ; thLt : top.snd < iv.fst
      let wLtIvLo : (ltNat (snd w) (fst iv) = True)
          wLtIvLo = ltTrans (snd w) (fst top) (fst iv)
                            wLt
                            (lteLtTrans (fst top) (snd top) (fst iv) wfTop thLt)
      in SBCons w wLtIvLo (sepBelowChain top iv ws sbWs wfTop thLt)

------------------------------------------------------------------------
-- Lift the invariant over foldl.
------------------------------------------------------------------------

total
foldlWF : (l : List Iv) -> (acc : List Iv) -> AllWF acc -> SepSorted acc ->
          AllWF l -> AllWF (foldl Main.step acc l)
foldlWF []        acc awAcc ssAcc AWNil = awAcc
foldlWF (x :: xs) acc awAcc ssAcc (AWCons x wfX awXs) =
  foldlWF xs (step acc x)
          (stepAllWF acc x awAcc wfX)
          (stepSepSorted acc x awAcc ssAcc wfX)
          awXs

total
foldlSS : (l : List Iv) -> (acc : List Iv) -> AllWF acc -> SepSorted acc ->
          AllWF l -> SepSorted (foldl Main.step acc l)
foldlSS []        acc awAcc ssAcc AWNil = ssAcc
foldlSS (x :: xs) acc awAcc ssAcc (AWCons x wfX awXs) =
  foldlSS xs (step acc x)
          (stepAllWF acc x awAcc wfX)
          (stepSepSorted acc x awAcc ssAcc wfX)
          awXs

------------------------------------------------------------------------
-- From the (reversed-accumulator) invariant to the final no-overlap relation.
-- We define the OUTPUT relation as an inductive Pairwise covering ALL pairs.
------------------------------------------------------------------------

-- NoOvBelow b xs : `b` ends strictly before every interval in xs starts.
data NoOvBelow : Iv -> List Iv -> Type where
  NOBNil  : NoOvBelow b []
  NOBCons : (c : Iv) -> ltNat (snd b) (fst c) = True -> NoOvBelow b cs -> NoOvBelow b (c :: cs)

-- Pairwise NoOverlap: for every earlier `a` and later `b`, a.snd < b.fst.
data NoOverlap : List Iv -> Type where
  NONil  : NoOverlap []
  NOCons : (a : Iv) -> NoOvBelow a rest -> NoOverlap rest -> NoOverlap (a :: rest)

------------------------------------------------------------------------
-- Reversing the accumulator turns SepSorted (b ends before earlier a starts,
-- reading head-to-tail) into NoOverlap (a ends before later b starts).
-- We reverse with an accumulator that is already NoOverlap and whose every
-- element starts strictly after everything we have yet to prepend.
------------------------------------------------------------------------

-- "acc-front" invariant: `done` is already NoOverlap, and every interval still
-- in `todo` ends strictly before every interval in `done` starts.  This is the
-- bridge: a SepSorted todo (head separated above the tail) plus this lets us
-- move todo's head onto done preserving NoOverlap.

-- For every w in todo and every c in done: w.snd < c.fst.
data BeforeAll : List Iv -> List Iv -> Type where
  BANil  : BeforeAll [] done
  BACons : (w : Iv) -> NoOvBelow w done -> BeforeAll ws done -> BeforeAll (w :: ws) done

-- If todo is SepSorted and BeforeAll todo done and done is NoOverlap, then
-- reversing todo onto done yields NoOverlap.
total
revOnto : (todo : List Iv) -> (done : List Iv) ->
          SepSorted todo -> NoOverlap done -> BeforeAll todo done ->
          NoOverlap (reverseOnto done todo)
revOnto []            done SSNil                noDone baDone = noDone
revOnto (top :: rest) done (SSCons top sbTop ssRest) noDone (BACons top nobTopDone baRest) =
  -- prepend top to done: top must be NoOvBelow done (we have nobTopDone),
  -- so (top :: done) is NoOverlap.  Then recurse on rest, whose every element
  -- ends before top starts (from sbTop) AND before everything in done
  -- (from baRest), giving BeforeAll rest (top :: done).
  revOnto rest (top :: done) ssRest
          (NOCons top nobTopDone noDone)
          (mkBA rest top done sbTop baRest)
  where
    -- combine: w before top (from SepBelow top) and w before ds (from BeforeAll)
    mkBA : (rest : List Iv) -> (t : Iv) -> (ds : List Iv) ->
           SepBelow t rest -> BeforeAll rest ds -> BeforeAll rest (t :: ds)
    mkBA []            t ds SBNil               BANil                  = BANil
    mkBA (w :: ws)     t ds (SBCons w wLt sbWs) (BACons w nobWDone baWs) =
      BACons w (NOBCons t wLt nobWDone) (mkBA ws t ds sbWs baWs)

------------------------------------------------------------------------
-- The merge and the runnable tests.
--
-- The sort below is an UNVERIFIED insertion sort by start.  As in the Lean
-- proof, sortedness is NOT part of the correctness argument: `step` preserves
-- the invariant for any well-formed incoming interval regardless of order.
-- The sort is here ONLY so the example outputs coalesce in the conventional
-- way (it does not affect WF or no-overlap of the result).
------------------------------------------------------------------------

insertIv : Iv -> List Iv -> List Iv
insertIv x [] = [x]
insertIv x (y :: ys) = if lteNat (fst x) (fst y) then x :: y :: ys else y :: insertIv x ys

isort : List Iv -> List Iv
isort []        = []
isort (x :: xs) = insertIv x (isort xs)

merge : List Iv -> List Iv
merge xs = reverse (foldl step [] (isort xs))

------------------------------------------------------------------------
-- isort preserves AllWF (needed to feed the WELL-FORMED hypothesis through the
-- sort into the fold).  Proven structurally so the WF theorems remain honest.
------------------------------------------------------------------------

total
insertWF : (x : Iv) -> (l : List Iv) -> WF x -> AllWF l -> AllWF (insertIv x l)
insertWF x []        wfX AWNil = AWCons x wfX AWNil
insertWF x (y :: ys) wfX (AWCons y wfY awYs) with (lteNat (fst x) (fst y))
  _ | True  = AWCons x wfX (AWCons y wfY awYs)
  _ | False = AWCons y wfY (insertWF x ys wfX awYs)

total
isortWF : (l : List Iv) -> AllWF l -> AllWF (isort l)
isortWF []        AWNil = AWNil
isortWF (x :: xs) (AWCons x wfX awXs) = insertWF x (isort xs) wfX (isortWF xs awXs)

------------------------------------------------------------------------
-- Correctness theorems.
------------------------------------------------------------------------

-- The fold's accumulator is well-formed and pairwise-separated, starting from
-- a well-formed sorted input.  `Main.step`/`Main.isort` qualified to dodge the
-- lowercase-auto-bind shadowing gotcha.
total
mergeAccWF : (xs : List Iv) -> AllWF xs ->
             AllWF (foldl Main.step [] (Main.isort xs))
mergeAccWF xs awXs = foldlWF (isort xs) [] AWNil SSNil (isortWF xs awXs)

total
mergeAccSS : (xs : List Iv) -> AllWF xs ->
             SepSorted (foldl Main.step [] (Main.isort xs))
mergeAccSS xs awXs = foldlSS (isort xs) [] AWNil SSNil (isortWF xs awXs)

-- PROPERTY 1 -- well-formed inputs are correctly HANDLED: assuming every input
-- is well-formed, the sort keeps them all well-formed, so the coalescing fold
-- receives a well-formed list (which the next two theorems then use).
total
mergeHandlesWF : (xs : List Iv) -> AllWF xs -> AllWF (Main.isort xs)
mergeHandlesWF xs awXs = isortWF xs awXs

-- helper: AllWF on a reversed list (reverseOnto with a WF accumulator).
total
revOntoWF : (todo, done : List Iv) -> AllWF todo -> AllWF done ->
            AllWF (reverseOnto done todo)
revOntoWF []            done AWNil               awDone = awDone
revOntoWF (x :: xs)     done (AWCons x wfX awXs) awDone =
  revOntoWF xs (x :: done) awXs (AWCons x wfX awDone)

-- PROPERTY 2 -- every output interval is well-formed (lo <= hi).
total
mergeOutputsWF : (xs : List Iv) -> AllWF xs -> AllWF (Main.merge xs)
mergeOutputsWF xs awXs =
  revOntoWF (foldl step [] (isort xs)) [] (mergeAccWF xs awXs) AWNil

-- every list is BeforeAll the empty `done` list (NoOvBelow w [] is trivial).
total
beforeAllNil : (todo : List Iv) -> BeforeAll todo []
beforeAllNil []        = BANil
beforeAllNil (w :: ws) = BACons w NOBNil (beforeAllNil ws)

-- PROPERTY 3 -- outputs are pairwise non-overlapping: for any earlier `a` and
-- later `b` in the result, a.snd < b.fst.
total
mergeOutputsNoOverlap : (xs : List Iv) -> AllWF xs -> NoOverlap (Main.merge xs)
mergeOutputsNoOverlap xs awXs =
  revOnto (foldl step [] (isort xs)) [] (mergeAccSS xs awXs) NONil
          (beforeAllNil (foldl step [] (isort xs)))

------------------------------------------------------------------------
-- Runnable examples (Nat pairs).
------------------------------------------------------------------------

main : IO ()
main = do
  printLn (merge [(1,3),(2,6),(8,10),(15,18)])  -- [(1, 6), (8, 10), (15, 18)]
  printLn (merge [(1,4),(4,5)])                 -- [(1, 5)]
