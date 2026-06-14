module Main where

mergeTwo :: Ord a => [a] -> [a] -> [a]
mergeTwo [] ys = ys
mergeTwo xs [] = xs
mergeTwo (x:xs) (y:ys)
  | x <= y    = x : mergeTwo xs (y:ys)
  | otherwise = y : mergeTwo (x:xs) ys

-- Divide and conquer: pairwise-merge halves.
mergeK :: Ord a => [[a]] -> [a]
mergeK []   = []
mergeK [xs] = xs
mergeK xss  =
  let (l, r) = splitAt (length xss `div` 2) xss
  in mergeTwo (mergeK l) (mergeK r)

main :: IO ()
main = print (mergeK [[1,4,5],[1,3,4],[2,6]])  -- [1,1,2,3,4,4,5,6]
