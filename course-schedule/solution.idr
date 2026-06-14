module Main

import Data.List
import Data.Maybe

-- graph as assoc list: prereq -> dependents
addEdge : Int -> Int -> List (Int, List Int) -> List (Int, List Int)
addEdge k v [] = [(k, [v])]
addEdge k v ((k', vs) :: rest) =
  if k == k' then (k', v :: vs) :: rest else (k', vs) :: addEdge k v rest

buildGraph : List (List Int) -> List (Int, List Int)
buildGraph = foldl addOne []
  where
    addOne : List (Int, List Int) -> List Int -> List (Int, List Int)
    addOne g (a :: b :: _) = addEdge b a g
    addOne g _             = g

succsOf : Int -> List (Int, List Int) -> List Int
succsOf node g = fromMaybe [] (lookup node g)

-- DFS with status map (1 = on stack, 2 = finished); Nothing signals a cycle.
visit : List (Int, List Int) -> Maybe (List (Int, Int), List Int) -> Int ->
        Maybe (List (Int, Int), List Int)
visit _ Nothing _ = Nothing
visit g (Just (st, order)) node =
  case lookup node st of
    Just 2  => Just (st, order)
    Just _  => Nothing
    Nothing =>
      case foldl (visit g) (Just ((node, 1) :: st, order)) (succsOf node g) of
        Nothing            => Nothing
        Just (st2, order2) => Just ((node, 2) :: st2, node :: order2)

findOrder : Nat -> List (List Int) -> List Int
findOrder n prereqs =
  let g     = buildGraph prereqs
      nodes = if n == 0 then [] else map cast (the (List Nat) [0 .. minus n 1])
  in case foldl (visit g) (Just ([], [])) nodes of
       Just (_, order) => order
       Nothing         => []

main : IO ()
main = do
  printLn (findOrder 4 [[1,0],[2,0],[3,1],[3,2]])  -- a valid order
  printLn (findOrder 2 [[1,0],[0,1]])              -- []
