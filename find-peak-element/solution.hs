module Main where

import Data.Array

-- LeetCode 162: Find Peak Element.
-- Binary search for a peak: compare a[mid] with a[mid+1] and recurse into the
-- ascending side (keeping mid on the left branch).  Returns an index i that is a
-- peak (greater than each in-bounds neighbour) in O(log n).  Correctness rests on
-- a search INVARIANT, not on the array being sorted (see solution.lean).
findPeak :: [Int] -> Int
findPeak xs = go 0 (n - 1)
  where
    n   = length xs
    arr = listArray (0, n - 1) xs
    go lo hi
      | lo >= hi               = lo
      | arr ! mid < arr ! succ mid = go (mid + 1) hi
      | otherwise              = go lo mid
      where mid = (lo + hi) `div` 2

-- PeakAt: boundaries handled as disjunctions (no -infinity sentinel).
peakAt :: [Int] -> Int -> Bool
peakAt xs i =
  (i == 0 || xs !! (i - 1) < xs !! i) && (i + 1 == n || xs !! (i + 1) < xs !! i)
  where n = length xs

main :: IO ()
main = do
  print (findPeak [1, 2, 3, 1])                       -- 2
  print (peakAt [1, 2, 3, 1] (findPeak [1, 2, 3, 1])) -- True
  -- [1,2,1,3,5,6,4] has two peaks (1 and 5); the search lands on one of them.
  let xs = [1, 2, 1, 3, 5, 6, 4]
  print (findPeak xs)                                 -- a peak index (1 or 5)
  print (peakAt xs (findPeak xs))                     -- True
  print (peakAt [1] (findPeak [1]))                   -- True
