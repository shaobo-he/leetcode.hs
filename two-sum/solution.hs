module Main where

import qualified Data.Map.Strict as Map

-- Hash complement lookup: check for (target - x) among indices seen so far.
twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target = go Map.empty (zip [0 ..] nums)
  where
    go _ [] = error "no pair"
    go seen ((i, x) : rest) =
      case Map.lookup (target - x) seen of
        Just j  -> (j, i)
        Nothing -> go (Map.insert x i seen) rest

main :: IO ()
main = do
  print (twoSum [2,7,11,15] 9)  -- (0,1)
  print (twoSum [3,2,4] 6)      -- (1,2)
