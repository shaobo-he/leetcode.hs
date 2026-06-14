module Main

import Data.List

noDups : List Char -> Bool
noDups xs = let ds = filter (/= '.') xs in length (nub ds) == length ds

chunks3 : List a -> List (List a)
chunks3 [] = []
chunks3 xs = take 3 xs :: chunks3 (drop 3 xs)

-- the three 3x3 boxes spanning a band of 3 rows
bandBoxes : List (List Char) -> List (List Char)
bandBoxes rows = map concat (transpose (map chunks3 rows))

boxes : List (List Char) -> List (List Char)
boxes board = concatMap bandBoxes (chunks3 board)

isValidSudoku : List (List Char) -> Bool
isValidSudoku board = all noDups (board ++ transpose board ++ boxes board)

main : IO ()
main = do
  let valid = map unpack
        ["53..7....","6..195...",".98....6.","8...6...3","4..8.3..1"
        ,"7...2...6",".6....28.","...419..5","....8..79"]
  printLn (isValidSudoku valid)    -- True
  let invalid = map unpack
        ["83..7....","6..195...",".98....6.","8...6...3","4..8.3..1"
        ,"7...2...6",".6....28.","...419..5","....8..79"]
  printLn (isValidSudoku invalid)  -- False
