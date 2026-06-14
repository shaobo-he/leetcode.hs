module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- Course Schedule II: a valid order to take all courses, or [] on a cycle.
-- Each prerequisite [a, b] means course b must be taken before course a.
findOrder :: Int -> [[Int]] -> [Int]
findOrder n prereqs =
  case foldl visit (Just (Map.empty, [])) [0 .. n - 1] of
    Just (_, order) -> order        -- prepended in post-order => already topological
    Nothing -> []
  where
    graph :: Map Int [Int]
    graph = Map.fromListWith (++) [ (b, [a]) | [a, b] <- prereqs ]
    -- status: 1 = on the current DFS stack, 2 = finished; Nothing = cycle
    visit Nothing _ = Nothing
    visit (Just (st, order)) node =
      case Map.lookup node st of
        Just 2 -> Just (st, order)
        Just _ -> Nothing                       -- back-edge => cycle
        Nothing ->
          let st1   = Map.insert node (1 :: Int) st
              succs = fromMaybe [] (Map.lookup node graph)
          in case foldl visit (Just (st1, order)) succs of
               Nothing -> Nothing
               Just (st2, order2) -> Just (Map.insert node 2 st2, node : order2)

main :: IO ()
main = do
  print (findOrder 4 [[1,0],[2,0],[3,1],[3,2]])  -- a valid order, e.g. [0,1,2,3]
  print (findOrder 2 [[1,0],[0,1]])              -- [] (cycle)
