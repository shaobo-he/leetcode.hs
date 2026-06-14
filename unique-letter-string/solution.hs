module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Each character at index i contributes (i - prev) * (next - i), where prev/next
-- are the indices of the same letter on either side (or -1 / n).
uniqueLetterString :: String -> Int
uniqueLetterString s = sum (concatMap contrib (Map.elems positions))
  where
    n = length s
    positions :: Map Char [Int]
    positions = Map.fromListWith (flip (++)) [ (c, [i]) | (i, c) <- zip [0 ..] s ]
    contrib idxs =
      [ (i - l) * (r - i)
      | (l, i, r) <- zip3 (-1 : idxs) idxs (drop 1 idxs ++ [n]) ]

main :: IO ()
main = do
  print (uniqueLetterString "ABC")       -- 10
  print (uniqueLetterString "ABA")       -- 8
  print (uniqueLetterString "LEETCODE")  -- 92
