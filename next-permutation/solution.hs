module Main where

-- LeetCode 31: Next Permutation — a purely functional recast over [Int].
--
-- findPivot splits xs as  pre ++ p : suf  where suf is the maximal weakly
-- decreasing suffix and p < head suf (the rightmost ascent).  Nothing means the
-- whole list is weakly decreasing (already the maximal arrangement → wrap to the
-- minimal one by reversing).  swapLast replaces the rightmost element of suf that
-- is > p with p (returning that element g); reversing the modified suffix gives
-- the ascending minimal tail.

findPivot :: [Int] -> Maybe ([Int], Int, [Int])
findPivot []           = Nothing
findPivot [_]          = Nothing
findPivot (x : y : rest) =
  case findPivot (y : rest) of
    Just (pre, p, suf) -> Just (x : pre, p, suf)
    Nothing            -> if x < y then Just ([], x, y : rest) else Nothing

-- replace the rightmost element > p with p; return (that element, new suffix)
swapLast :: Int -> [Int] -> Maybe (Int, [Int])
swapLast _ []        = Nothing
swapLast p (c : rest) =
  case swapLast p rest of
    Just (g, rest') -> Just (g, c : rest')
    Nothing         -> if p < c then Just (c, p : rest) else Nothing

nextPermutation :: [Int] -> [Int]
nextPermutation xs =
  case findPivot xs of
    Nothing -> reverse xs
    Just (pre, p, suf) ->
      case swapLast p suf of
        Just (g, suf') -> pre ++ g : reverse suf'
        Nothing        -> reverse xs

main :: IO ()
main = do
  print (nextPermutation [1, 2, 3])   -- [1,3,2]
  print (nextPermutation [3, 2, 1])   -- [1,2,3] (wrap)
  print (nextPermutation [1, 1, 5])   -- [1,5,1]
  print (nextPermutation [1, 3, 2])   -- [2,1,3]
  print (nextPermutation [1, 5, 1])   -- [5,1,1]
