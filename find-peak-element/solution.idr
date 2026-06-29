module Main

import Data.Nat
import Control.WellFounded

------------------------------------------------------------------------
-- LeetCode 162: Find Peak Element.
--
-- Binary search for a peak: model the array as a function `a : Nat -> Nat`,
-- search the window [lo, hi]; compare `a mid` with `a (S mid)` and recurse
-- RIGHT ([S mid, hi]) when ascending, else LEFT ([lo, mid], keeping mid).
--
-- The search is TOTAL by WELL-FOUNDED recursion on the window size `hi - lo`
-- (an explicit `Accessible LT` argument) — NO `assert_smaller`, no `partial`,
-- no fuel.  The two decreasing facts (`leftDecr`, `rightDecr`) are proved total
-- by induction; `mid = lo + half (hi - lo)` is chosen so the proofs need only
-- `half (S d) <= d` and `n - m <= n`, never a `div` lemma.
--
-- CORRECTNESS (`searchPeak`): on a nonempty array whose adjacent entries differ
-- the returned index is a genuine `PeakAt`.  This is the port of the Lean
-- `search_peak` theorem (see solution.lean): the same window invariant carried
-- through the same search recursion, re-established at each step from the
-- comparison.  Idris has no `omega`, so every `<=`/`<`/`half`/`minus` step is by
-- hand; adjacent-distinctness is used in EXACTLY one spot — the `not < & /= => >`
-- step in the LEFT branch (`notLtNeqGt`, a Nat trichotomy).
--
-- The element type is `Nat` (not `Int`): unlike Idris primitive `Int`, `Nat`
-- has a genuinely provable total order (`trichotomy`), so the `not < & /= => >`
-- step is closed honestly with no trusted primitive.
------------------------------------------------------------------------

%default total

-- small self-contained LTE helpers (avoid relying on base lemma names).
lteReflN : (n : Nat) -> LTE n n
lteReflN Z     = LTEZero
lteReflN (S k) = LTESucc (lteReflN k)

lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero     _           = LTEZero
lteTrans (LTESucc p) (LTESucc q) = LTESucc (lteTrans p q)

-- floor(n/2), structural.
half : Nat -> Nat
half Z         = Z
half (S Z)     = Z
half (S (S n)) = S (half n)

-- half (S d) <= d  (so half of a positive number is strictly smaller).
halfLteSelf : (d : Nat) -> half (S d) `LTE` d
halfLteSelf Z         = LTEZero
halfLteSelf (S Z)     = LTESucc LTEZero
halfLteSelf (S (S k)) = LTESucc (lteSuccRight (halfLteSelf k))

halfLt : (d : Nat) -> half (S d) `LT` S d
halfLt d = LTESucc (halfLteSelf d)

-- n - m <= n.
minusLte : (n, m : Nat) -> (n `minus` m) `LTE` n
minusLte Z     m     = LTEZero
minusLte (S k) Z     = lteReflN (S k)
minusLte (S k) (S j) = lteSuccRight (minusLte k j)

------------------------------------------------------------------------
-- The two window-shrinking facts, by induction on lo (everything cancels the
-- shared `lo`, so no division reasoning is needed).
------------------------------------------------------------------------

-- LEFT branch: [lo, mid] is smaller than [lo, hi].
leftDecr : (lo, hi : Nat) -> S lo `LTE` hi ->
           ((lo + half (hi `minus` lo)) `minus` lo) `LT` (hi `minus` lo)
leftDecr Z     (S h) _           = rewrite minusZeroRight (half (S h)) in halfLt h
leftDecr (S l) (S h) (LTESucc p) = leftDecr l h p

-- RIGHT branch: [S mid, hi] is smaller than [lo, hi].
rightDecr : (lo, hi : Nat) -> S lo `LTE` hi ->
            (hi `minus` S (lo + half (hi `minus` lo))) `LT` (hi `minus` lo)
rightDecr Z     (S h) _           = LTESucc (minusLte h (half (S h)))
rightDecr (S l) (S h) (LTESucc p) = rightDecr l h p

------------------------------------------------------------------------
-- Well-foundedness of `LT` on Nat (own proof, core only).
------------------------------------------------------------------------

accLT : (n : Nat) -> Accessible LT n
accLT n = Access (go n)
  where
    go : (n : Nat) -> (y : Nat) -> y `LT` n -> Accessible LT y
    go (S k) y (LTESucc yLEk) =
      Access (\z, zLTy => go k z (lteTrans zLTy yLEk))

------------------------------------------------------------------------
-- The verified-total binary search.  The comparison `a mid` vs `a (S mid)`
-- is performed with a *decidable* `isLTE`, so the correctness proof gets a
-- real `LT`/`Not LT` witness in each branch (no Bool->Prop reconstruction).
------------------------------------------------------------------------

searchAcc : (a : Nat -> Nat) -> (lo, hi : Nat) ->
            Accessible LT (hi `minus` lo) -> Nat
searchAcc a lo hi (Access rec) =
  case isLTE (S lo) hi of
    No  _   => lo
    Yes prf =>
      case isLTE (S (a (lo + half (hi `minus` lo)))) (a (S (lo + half (hi `minus` lo)))) of
        Yes _ => searchAcc a (S (lo + half (hi `minus` lo))) hi
                   (rec (hi `minus` S (lo + half (hi `minus` lo))) (rightDecr lo hi prf))
        No  _ => searchAcc a lo (lo + half (hi `minus` lo))
                   (rec ((lo + half (hi `minus` lo)) `minus` lo) (leftDecr lo hi prf))

------------------------------------------------------------------------
-- PeakAt and the window-invariant boundary predicates (no -inf sentinel;
-- boundaries are disjunctions, mirroring the Lean `PeakAt`).
------------------------------------------------------------------------

PeakAt : (a : Nat -> Nat) -> (n, i : Nat) -> Type
PeakAt a n i =
  ( Either (i = 0)     (LT (a (i `minus` 1)) (a i))
  , Either (i + 1 = n) (LT (a (i + 1))       (a i)) )

LeftBd : (a : Nat -> Nat) -> (lo : Nat) -> Type
LeftBd a lo = Either (lo = 0) (LT (a (lo `minus` 1)) (a lo))

RightBd : (a : Nat -> Nat) -> (n, hi : Nat) -> Type
RightBd a n hi = Either (hi + 1 = n) (LT (a (hi + 1)) (a hi))

------------------------------------------------------------------------
-- Arithmetic lemmas the correctness proof needs (all by hand).
------------------------------------------------------------------------

-- `LTE (S _) Z` is uninhabited.
notLTEsuccZ : LTE (S n) Z -> Void
notLTEsuccZ LTEZero     impossible
notLTEsuccZ (LTESucc _) impossible

-- lo <= lo + m.
lteAddR : (n, m : Nat) -> LTE n (n + m)
lteAddR Z     m = LTEZero
lteAddR (S k) m = LTESucc (lteAddR k m)

-- m + 1 = S m.
plusOne : (m : Nat) -> m + 1 = S m
plusOne Z     = Refl
plusOne (S k) = cong S (plusOne k)

-- (S m) - 1 = m  (the `- 0` tail is stuck for a neutral m, so prove it).
minusSucc1 : (m : Nat) -> (S m) `minus` 1 = m
minusSucc1 m = minusZeroRight m

-- mid = lo + half (hi - lo) is strictly below hi whenever the window is live.
midLtHi : (lo, hi : Nat) -> LTE (S lo) hi -> LT (lo + half (hi `minus` lo)) hi
midLtHi Z     Z     prf           = void (notLTEsuccZ prf)
midLtHi Z     (S h) _             = halfLt h
midLtHi (S l) Z     prf           = void (notLTEsuccZ prf)
midLtHi (S l) (S h) (LTESucc p)   = LTESucc (midLtHi l h p)

-- Nat is totally ordered (the one place adjacency is consumed).
trichotomy : (x, y : Nat) -> Either (LT x y) (Either (x = y) (LT y x))
trichotomy Z     Z     = Right (Left Refl)
trichotomy Z     (S k) = Left (LTESucc LTEZero)
trichotomy (S k) Z     = Right (Right (LTESucc LTEZero))
trichotomy (S k) (S j) = case trichotomy k j of
  Left lt           => Left (LTESucc lt)
  Right (Left eq)   => Right (Left (cong S eq))
  Right (Right gt)  => Right (Right (LTESucc gt))

-- not (x < y) and x /= y imply y < x.
notLtNeqGt : (x, y : Nat) -> Not (LT x y) -> Not (x = y) -> LT y x
notLtNeqGt x y nlt neq = case trichotomy x y of
  Left lt          => void (nlt lt)
  Right (Left eq)  => void (neq eq)
  Right (Right gt) => gt

-- lo <= hi and not (lo < hi) imply lo = hi.
eqFromLteNotLt : (lo, hi : Nat) -> LTE lo hi -> Not (LTE (S lo) hi) -> lo = hi
eqFromLteNotLt Z     Z     _           _    = Refl
eqFromLteNotLt Z     (S h) _           nprf = void (nprf (LTESucc LTEZero))
eqFromLteNotLt (S l) Z     lohi        _    = void (notLTEsuccZ lohi)
eqFromLteNotLt (S l) (S h) (LTESucc p) nprf =
  cong S (eqFromLteNotLt l h p (\q => nprf (LTESucc q)))

------------------------------------------------------------------------
-- CORRECTNESS: the port of Lean `search_peak`.  Induction along the search
-- recursion (on the accessibility witness); the window invariant
--   lo <= hi,  hi < n,  LeftBd a lo,  RightBd a n hi
-- collapses to `PeakAt a n lo` when `not (lo < hi)`, and is re-established on
-- the moved boundary in each recursive branch.
------------------------------------------------------------------------

searchPeak : (a : Nat -> Nat) -> (adj : (k : Nat) -> Not (a k = a (S k))) ->
             (n, lo, hi : Nat) -> (acc : Accessible LT (hi `minus` lo)) ->
             LTE lo hi -> LT hi n ->
             LeftBd a lo -> RightBd a n hi ->
             ( LT (searchAcc a lo hi acc) n
             , PeakAt a n (searchAcc a lo hi acc) )
searchPeak a adj n lo hi (Access rec) lohi hin lbd rbd
    with (isLTE (S lo) hi)
  searchPeak a adj n lo hi (Access rec) lohi hin lbd rbd | No nprf =
      ( replace {p = \z => LT z n} (sym (eqFromLteNotLt lo hi lohi nprf)) hin
      , ( lbd
        , replace {p = \z => Either (z + 1 = n) (LT (a (z + 1)) (a z))}
                  (sym (eqFromLteNotLt lo hi lohi nprf)) rbd ) )
  searchPeak a adj n lo hi (Access rec) lohi hin lbd rbd | Yes prf
      with (isLTE (S (a (lo + half (hi `minus` lo)))) (a (S (lo + half (hi `minus` lo)))))
    searchPeak a adj n lo hi (Access rec) lohi hin lbd rbd | Yes prf | Yes lt =
        -- RIGHT branch: a mid < a (S mid); recurse on [S mid, hi].
        searchPeak a adj n (S (lo + half (hi `minus` lo))) hi
          (rec (hi `minus` S (lo + half (hi `minus` lo))) (rightDecr lo hi prf))
          (midLtHi lo hi prf) hin
          (Right (replace
                    {p = \z => LT (a z) (a (S (lo + half (hi `minus` lo))))}
                    (sym (minusSucc1 (lo + half (hi `minus` lo)))) lt))
          rbd
    searchPeak a adj n lo hi (Access rec) lohi hin lbd rbd | Yes prf | No nlt =
        -- LEFT branch: not (a mid < a (S mid)); recurse on [lo, mid].
        searchPeak a adj n lo (lo + half (hi `minus` lo))
          (rec ((lo + half (hi `minus` lo)) `minus` lo) (leftDecr lo hi prf))
          (lteAddR lo (half (hi `minus` lo)))
          (lteTrans (midLtHi lo hi prf) (lteSuccLeft hin))
          lbd
          (Right (replace
                    {p = \z => LT (a z) (a (lo + half (hi `minus` lo)))}
                    (sym (plusOne (lo + half (hi `minus` lo))))
                    (notLtNeqGt (a (lo + half (hi `minus` lo)))
                                (a (S (lo + half (hi `minus` lo))))
                                nlt (adj (lo + half (hi `minus` lo))))))

-- packaging: on a nonempty (length S m), adjacent-distinct array the index
-- returned by the full-window search is a genuine peak.
findPeakCorrect : (a : Nat -> Nat) -> (adj : (k : Nat) -> Not (a k = a (S k))) ->
                  (m : Nat) ->
                  PeakAt a (S m) (searchAcc a 0 m (accLT (m `minus` 0)))
findPeakCorrect a adj m =
  snd (searchPeak a adj (S m) 0 m (accLT (m `minus` 0))
         LTEZero (lteReflN (S m)) (Left Refl) (Left (plusOne m)))

-- free corollary: every nonempty adjacent-distinct array HAS a peak.
existsPeak : (a : Nat -> Nat) -> (adj : (k : Nat) -> Not (a k = a (S k))) ->
             (m : Nat) -> (i : Nat ** PeakAt a (S m) i)
existsPeak a adj m = (_ ** findPeakCorrect a adj m)

------------------------------------------------------------------------
-- Runnable wrappers over List Nat, plus a Bool PeakAt checker.
------------------------------------------------------------------------

nth : List Nat -> Nat -> Nat
nth []        _     = 0
nth (x :: _)  Z     = x
nth (_ :: xs) (S k) = nth xs k

findPeak : List Nat -> Nat
findPeak xs =
  let n = length xs in
  searchAcc (nth xs) 0 (n `minus` 1) (accLT ((n `minus` 1) `minus` 0))

-- peakAt: boundaries handled as disjunctions (no -inf sentinel).
peakAt : List Nat -> Nat -> Bool
peakAt xs i =
  let f = nth xs
      n = length xs in
  (i == 0 || f (i `minus` 1) < f i) && (i + 1 == n || f (i + 1) < f i)

main : IO ()
main = do
  printLn (findPeak [1, 2, 3, 1])                          -- 2
  printLn (peakAt   [1, 2, 3, 1] (findPeak [1, 2, 3, 1]))  -- True
  -- [1,2,1,3,5,6,4] has two peaks (1 and 5); the search lands on one.
  printLn (findPeak [1, 2, 1, 3, 5, 6, 4])                 -- a peak index
  printLn (peakAt   [1, 2, 1, 3, 5, 6, 4] (findPeak [1, 2, 1, 3, 5, 6, 4]))  -- True
  printLn (peakAt   [1] (findPeak [1]))                    -- True
