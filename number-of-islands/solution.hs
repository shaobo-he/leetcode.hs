module Main where

import Data.Array (Array, listArray, bounds, indices, inRange, (!))
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (foldM)
import Control.Monad.State (State, evalState, get, modify)

-- Number of islands by flood-fill DFS, done purely: the grid is an immutable
-- `Array` (read-only), and the "sink the cell to 0" mutation of the classic
-- IOArray solution becomes a `State (Set (Int,Int))` of visited cells.  We scan
-- every cell; each unvisited land cell starts a new island and floods its whole
-- 4-connected component into the visited set.
numIslands :: [[String]] -> Int
numIslands [] = 0
numIslands grid = evalState (foldM countAt 0 (indices arr)) Set.empty
  where
    rows = length grid
    cols = length (head grid)
    arr :: Array (Int, Int) String
    arr = listArray ((0, 0), (rows - 1, cols - 1)) (concat grid)

    -- a new island iff this cell is unvisited land; flood it, then +1
    countAt :: Int -> (Int, Int) -> State (Set (Int, Int)) Int
    countAt acc p = do
      seen <- get
      if arr ! p == "1" && not (Set.member p seen)
        then flood p >> return (acc + 1)
        else return acc

    -- sink the whole 4-connected component into the visited set
    flood :: (Int, Int) -> State (Set (Int, Int)) ()
    flood p@(r, c)
      | not (inRange (bounds arr) p) = return ()
      | arr ! p /= "1"               = return ()
      | otherwise = do
          seen <- get
          if Set.member p seen
            then return ()
            else do
              modify (Set.insert p)
              mapM_ flood [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

main :: IO ()
main = do
  -- one connected island
  print $ numIslands [ ["1","1","1","1","0"]
                     , ["1","1","0","1","0"]
                     , ["1","1","0","0","0"]
                     , ["0","0","0","0","0"] ]   -- 1
  -- three separate islands (a partial scan that only sees the top row misses these)
  print $ numIslands [ ["1","1","0","0","0"]
                     , ["1","1","0","0","0"]
                     , ["0","0","1","0","0"]
                     , ["0","0","0","1","1"] ]   -- 3
