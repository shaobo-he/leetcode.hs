module Main

import Data.List
import Data.Maybe

nth : Nat -> List a -> Maybe a
nth _ [] = Nothing
nth Z (x :: _) = Just x
nth (S k) (_ :: xs) = nth k xs

-- grid lookup with Integer coords; out of bounds reads as water
cellAt : List (List Char) -> (Integer, Integer) -> Char
cellAt grid (r, c) =
  if r < 0 || c < 0 then '0'
  else case nth (integerToNat r) grid of
         Nothing  => '0'
         Just row => fromMaybe '0' (nth (integerToNat c) row)

-- flood fill: add every connected land cell to the visited set
flood : List (List Char) -> (Integer, Integer) -> List (Integer, Integer) ->
        List (Integer, Integer)
flood grid pos visited =
  if cellAt grid pos /= '1' || elem pos visited
    then visited
    else let (r, c) = pos
             v1 = pos :: visited
             v2 = flood grid (r + 1, c) v1
             v3 = flood grid (r - 1, c) v2
             v4 = flood grid (r, c + 1) v3
         in flood grid (r, c - 1) v4

numIslands : List (List Char) -> Nat
numIslands grid =
  let rows   = the Integer (cast (length grid))
      cols   = the Integer (cast (fromMaybe 0 (map length (head' grid))))
      coords = [ (r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1] ]
  in snd (foldl step ([], 0) coords)
  where
    step : (List (Integer, Integer), Nat) -> (Integer, Integer) ->
           (List (Integer, Integer), Nat)
    step (visited, count) pos =
      if cellAt grid pos == '1' && not (elem pos visited)
        then (flood grid pos visited, S count)
        else (visited, count)

main : IO ()
main = do
  printLn (numIslands (map unpack ["11110","11010","11000","00000"]))  -- 1
  printLn (numIslands (map unpack ["11000","11000","00100","00011"]))  -- 3
