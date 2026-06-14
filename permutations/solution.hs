module Main where

-- All permutations: pick each element as the head, recurse on the rest.
permute :: [a] -> [[a]]
permute [] = [[]]
permute xs = [ x : p | (x, rest) <- selections xs, p <- permute rest ]
  where
    selections []     = []
    selections (y:ys) = (y, ys) : [ (z, y : zs) | (z, zs) <- selections ys ]

main :: IO ()
main = mapM_ print (permute [1,2,3])
