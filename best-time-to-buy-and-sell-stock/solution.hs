module Main where

-- Single pass: lowest price so far + best profit.
maxProfit :: [Int] -> Int
maxProfit [] = 0
maxProfit (p:ps) = go ps p 0
  where
    go [] _ best = best
    go (x:xs) lowest best = go xs (min lowest x) (max best (x - lowest))

main :: IO ()
main = do
  print (maxProfit [7,1,5,3,6,4])  -- 5
  print (maxProfit [7,6,4,3,1])    -- 0
