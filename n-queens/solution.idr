module Main

import Data.List

selections : List a -> List (a, List a)
selections [] = []
selections (x :: xs) = (x, xs) :: map (\(y, ys) => (y, x :: ys)) (selections xs)

permute : List a -> List (List a)
permute [] = [[]]
permute xs = concatMap (\(x, rest) => map (x ::) (permute rest)) (selections xs)

ok : (Int, Int) -> (Int, Int) -> Bool
ok (r1, c1) (r2, c2) = abs (r1 - r2) /= abs (c1 - c2)

-- A placement is the column chosen in each successive row; columns are already
-- distinct (it's a permutation), so we only check diagonals.
diagSafe : List Int -> Bool
diagSafe cols = check (zip (map cast (the (List Nat) [1 .. length cols])) cols)
  where
    check : List (Int, Int) -> Bool
    check []        = True
    check (p :: ps) = all (ok p) ps && check ps

queens : Nat -> List (List Int)
queens n = filter diagSafe (permute (map cast (the (List Nat) [1 .. n])))

main : IO ()
main = do
  printLn (length (queens 4))  -- 2
  printLn (length (queens 5))  -- 10
  printLn (length (queens 6))  -- 4
