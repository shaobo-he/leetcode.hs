module Main

import Data.List

-- Peel the first row, rotate the rest counter-clockwise, recurse.
spiralOrder : List (List a) -> List a
spiralOrder [] = []
spiralOrder ([] :: _) = []
spiralOrder (row :: rows) = row ++ spiralOrder (reverse (transpose rows))

main : IO ()
main = do
  printLn (spiralOrder [[1,2,3],[4,5,6],[7,8,9]])           -- [1,2,3,6,9,8,7,4,5]
  printLn (spiralOrder [[1,2,3,4],[5,6,7,8],[9,10,11,12]])  -- [1,2,3,4,8,12,11,10,9,5,6,7]
