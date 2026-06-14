module Main where

import qualified Data.Map.Strict as Map

-- Sliding window holding at most k distinct chars; shrink from the left.
lengthOfLongestSubstringKDistinct :: String -> Int -> Int
lengthOfLongestSubstringKDistinct s k = go [] Map.empty 0 s
  where
    go _ _ best [] = best
    go win counts best (c:cs) =
      let (win2, counts2) = shrink (win ++ [c]) (Map.insertWith (+) c 1 counts)
      in go win2 counts2 (max best (length win2)) cs
    shrink win counts
      | Map.size counts > k =
          case win of
            (d:ds) -> shrink ds (decr d counts)
            []     -> (win, counts)
      | otherwise = (win, counts)
    decr d counts = case Map.lookup d counts of
      Just 1 -> Map.delete d counts
      Just n -> Map.insert d (n - 1) counts
      _      -> counts

main :: IO ()
main = do
  print (lengthOfLongestSubstringKDistinct "eceba" 2)  -- 3
  print (lengthOfLongestSubstringKDistinct "aa" 1)     -- 2
  print (lengthOfLongestSubstringKDistinct "abee" 1)   -- 2
