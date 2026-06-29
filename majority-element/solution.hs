module Main where

import Data.List (foldl')

-- LeetCode 169: Majority Element — Boyer–Moore voting.
-- A left fold carrying (candidate, count).  Returns the candidate; on inputs
-- that have a strict majority (the LeetCode guarantee) it IS that element.
step :: Eq a => (Maybe a, Int) -> a -> (Maybe a, Int)
step (_,      0) x = (Just x, 1)
step (Just c, k) x
  | x == c        = (Just c, k + 1)
  | otherwise     = (Just c, k - 1)
step (Nothing, _) x = (Just x, 1)

majorityElement :: Eq a => [a] -> Maybe a
majorityElement = fst . foldl' step (Nothing, 0)

main :: IO ()
main = do
  print (majorityElement [3, 2, 3])               -- Just 3
  print (majorityElement [2, 2, 1, 1, 1, 2, 2])   -- Just 2
  print (majorityElement ([] :: [Int]))           -- Nothing
