module Main where

-- Merge two sorted lists (cf. the dependently-typed Vect version in solution.idr).
sortedMerge :: Ord a => [a] -> [a] -> [a]
sortedMerge [] ys = ys
sortedMerge xs [] = xs
sortedMerge (x:xs) (y:ys)
  | x <= y    = x : sortedMerge xs (y:ys)
  | otherwise = y : sortedMerge (x:xs) ys

main :: IO ()
main = do
  print (sortedMerge [1,2,3] [2,5,6])  -- [1,2,2,3,5,6]
  print (sortedMerge [1,3,4] [2,5,6])  -- [1,2,3,4,5,6]
