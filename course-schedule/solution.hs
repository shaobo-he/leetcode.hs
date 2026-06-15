module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Control.Monad.State (StateT, evalStateT, get, modify, lift)

-- Course Schedule II: a valid order to take all courses, or [] on a cycle.
-- Each prerequisite [a, b] means course b must be taken before course a.
--
-- A DFS that threads a per-node status map and short-circuits on a cycle is a
-- textbook fit for `StateT (Map Int Int) Maybe`: the State carries the visited
-- map, the underlying `Maybe` is the cycle short-circuit (`lift Nothing`), and
-- the topological order is the returned value.  Compare the previous hand-rolled
-- version, which threaded `(status, order)` through `foldl` with an explicit
-- `Maybe` wrapper by hand — exactly what this monad automates.
findOrder :: Int -> [[Int]] -> [Int]
findOrder n prereqs =
  fromMaybe [] (evalStateT (foldM step [] [0 .. n - 1]) Map.empty)
  where
    graph :: Map Int [Int]
    graph = Map.fromListWith (++) [ (b, [a]) | [a, b] <- prereqs ]
    -- prepend each finished subtree's order, mirroring the old left-fold
    step acc node = (++ acc) <$> visit node
    -- status: 1 = on the current DFS stack, 2 = finished
    visit :: Int -> StateT (Map Int Int) Maybe [Int]
    visit node = do
      st <- get
      case Map.lookup node st of
        Just 2 -> return []                     -- finished: contributes nothing
        Just _ -> lift Nothing                  -- back-edge => cycle
        Nothing -> do
          modify (Map.insert node 1)            -- mark on-stack
          sub <- foldM step [] (fromMaybe [] (Map.lookup node graph))
          modify (Map.insert node 2)            -- mark finished
          return (node : sub)                   -- post-order => topological

main :: IO ()
main = do
  print (findOrder 4 [[1,0],[2,0],[3,1],[3,2]])  -- a valid order, e.g. [0,1,2,3]
  print (findOrder 2 [[1,0],[0,1]])              -- [] (cycle)
