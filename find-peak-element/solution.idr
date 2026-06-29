module Main

import Data.Nat
import Control.WellFounded

------------------------------------------------------------------------
-- LeetCode 162: Find Peak Element.
--
-- Binary search for a peak: model the array as a function `a : Nat -> Int`,
-- search the window [lo, hi]; compare `a mid` with `a (S mid)` and recurse
-- RIGHT ([S mid, hi]) when ascending, else LEFT ([lo, mid], keeping mid).
--
-- The search is TOTAL by WELL-FOUNDED recursion on the window size `hi - lo`
-- (an explicit `Accessible LT` argument) — NO `assert_smaller`, no `partial`,
-- no fuel.  The two decreasing facts (`leftDecr`, `rightDecr`) are proved total
-- by induction; `mid = lo + half (hi - lo)` is chosen so the proofs need only
-- `half (S d) <= d` and `n - m <= n`, never a `div` lemma.
--
-- The PeakAt *correctness* proof (the analogue of the Lean `search_peak`) is
-- future work; see solution.lean for the fully verified version.  Here we ship
-- the total search plus a runnable `peakAt` checker and worked examples.
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
-- The verified-total binary search.
------------------------------------------------------------------------

searchAcc : (a : Nat -> Int) -> (lo, hi : Nat) ->
            Accessible LT (hi `minus` lo) -> Nat
searchAcc a lo hi (Access rec) =
  case isLTE (S lo) hi of
    No  _   => lo
    Yes prf =>
      if a (lo + half (hi `minus` lo)) < a (S (lo + half (hi `minus` lo)))
        then searchAcc a (S (lo + half (hi `minus` lo))) hi
               (rec (hi `minus` S (lo + half (hi `minus` lo))) (rightDecr lo hi prf))
        else searchAcc a lo (lo + half (hi `minus` lo))
               (rec ((lo + half (hi `minus` lo)) `minus` lo) (leftDecr lo hi prf))

------------------------------------------------------------------------
-- Runnable wrappers over List Int, plus a PeakAt checker.
------------------------------------------------------------------------

nth : List Int -> Nat -> Int
nth []        _     = 0
nth (x :: _)  Z     = x
nth (_ :: xs) (S k) = nth xs k

findPeak : List Int -> Nat
findPeak xs =
  let n = length xs in
  searchAcc (nth xs) 0 (n `minus` 1) (accLT ((n `minus` 1) `minus` 0))

-- PeakAt: boundaries handled as disjunctions (no -inf sentinel).
peakAt : List Int -> Nat -> Bool
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
