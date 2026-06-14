module Main

import Data.List

mergeTwo : Ord a => List a -> List a -> List a
mergeTwo [] ys = ys
mergeTwo xs [] = xs
mergeTwo (x :: xs) (y :: ys) =
  if x <= y then x :: mergeTwo xs (y :: ys)
            else y :: mergeTwo (x :: xs) ys

-- Divide and conquer: pairwise-merge the halves.
mergeK : Ord a => List (List a) -> List a
mergeK [] = []
mergeK [xs] = xs
mergeK xss = let half = integerToNat (div (cast (length xss)) 2)
                 (l, r) = splitAt half xss
             in mergeTwo (mergeK l) (mergeK r)

main : IO ()
main = printLn (mergeK [[1,4,5],[1,3,4],[2,6]])  -- [1, 1, 2, 3, 4, 4, 5, 6]
