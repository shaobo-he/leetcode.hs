module Main

import Data.List

-- Sort by start, then fold coalescing overlaps (accumulator kept reversed).
merge : List (Int, Int) -> List (Int, Int)
merge xs = reverse (foldl step [] (sortBy (\a, b => compare (fst a) (fst b)) xs))
  where
    step : List (Int, Int) -> (Int, Int) -> List (Int, Int)
    step [] iv = [iv]
    step (top :: rest) (lo, hi) =
      if lo <= snd top
        then (fst top, max (snd top) hi) :: rest
        else (lo, hi) :: top :: rest

main : IO ()
main = do
  printLn (merge [(1,3),(2,6),(8,10),(15,18)])  -- [(1, 6), (8, 10), (15, 18)]
  printLn (merge [(1,4),(4,5)])                 -- [(1, 5)]
