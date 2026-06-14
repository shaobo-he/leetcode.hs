module Main

import Data.List

-- collect the indices (ascending) at which each char occurs
appendIdx : Char -> Integer -> List (Char, List Integer) -> List (Char, List Integer)
appendIdx c i [] = [(c, [i])]
appendIdx c i ((d, is) :: rest) =
  if c == d then (d, is ++ [i]) :: rest else (d, is) :: appendIdx c i rest

positionsOf : List Char -> List (Char, List Integer)
positionsOf cs = go 0 [] cs
  where
    go : Integer -> List (Char, List Integer) -> List Char -> List (Char, List Integer)
    go _ acc [] = acc
    go i acc (c :: rest) = go (i + 1) (appendIdx c i acc) rest

-- each occurrence contributes (i - prev) * (next - i), with prev/next = -1 / n
contrib : Integer -> List Integer -> Integer
contrib n idxs =
  sum (zipWith3 (\l, i, r => (i - l) * (r - i)) (-1 :: idxs) idxs (drop 1 idxs ++ [n]))

uniqueLetterString : String -> Integer
uniqueLetterString s =
  let cs = unpack s
      n  = the Integer (cast (length cs))
  in sum (map (\p => contrib n (snd p)) (positionsOf cs))

main : IO ()
main = do
  printLn (uniqueLetterString "ABC")       -- 10
  printLn (uniqueLetterString "ABA")       -- 8
  printLn (uniqueLetterString "LEETCODE")  -- 92
