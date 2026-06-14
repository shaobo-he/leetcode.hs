module Main where

import Data.List (group, sort)

-- Unique permutations of a multiset: pick each distinct value with count > 0.
permuteUnique :: Ord a => [a] -> [[a]]
permuteUnique xs = go [ (head g, length g) | g <- group (sort xs) ]
  where
    go counts
      | all ((== 0) . snd) counts = [[]]
      | otherwise =
          [ x : rest
          | (x, n) <- counts, n > 0
          , rest <- go (decr x counts) ]
    decr x = map (\(y, n) -> if y == x then (y, n - 1) else (y, n))

main :: IO ()
main = mapM_ print (permuteUnique [1,1,2])  -- [1,1,2], [1,2,1], [2,1,1]
