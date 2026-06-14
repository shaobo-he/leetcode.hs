module Main where

import Data.List (transpose)
import qualified Data.Set as Set

-- Valid when no row, column, or 3x3 box repeats a digit ('.' is empty).
isValidSudoku :: [String] -> Bool
isValidSudoku board = all noDups (rows ++ cols ++ boxes)
  where
    rows  = board
    cols  = transpose board
    boxes = [ [ (board !! r) !! c | r <- [br .. br + 2], c <- [bc .. bc + 2] ]
            | br <- [0, 3, 6], bc <- [0, 3, 6] ]
    noDups xs = let ds = filter (/= '.') xs
                in length ds == Set.size (Set.fromList ds)

main :: IO ()
main = do
  print (isValidSudoku
    ["53..7....","6..195...",".98....6.","8...6...3"
    ,"4..8.3..1","7...2...6",".6....28.","...419..5","....8..79"])  -- True
  print (isValidSudoku
    ["83..7....","6..195...",".98....6.","8...6...3"
    ,"4..8.3..1","7...2...6",".6....28.","...419..5","....8..79"])  -- False
