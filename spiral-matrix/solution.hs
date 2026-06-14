module Main where

import Data.List (transpose)

-- Peel the first row, rotate the rest counter-clockwise, recurse.
spiralOrder :: [[a]] -> [a]
spiralOrder []         = []
spiralOrder ([] : _)   = []
spiralOrder (row : rows) = row ++ spiralOrder (rotate rows)
  where rotate = reverse . transpose

main :: IO ()
main = do
  print (spiralOrder [[1,2,3],[4,5,6],[7,8,9]])           -- [1,2,3,6,9,8,7,4,5]
  print (spiralOrder [[1,2,3,4],[5,6,7,8],[9,10,11,12]])  -- [1,2,3,4,8,12,11,10,9,5,6,7]
