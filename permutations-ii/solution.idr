module Main

-- multiset as an assoc list of (value, count)
bump : Eq a => a -> List (a, Nat) -> List (a, Nat)
bump x [] = [(x, 1)]
bump x ((y, n) :: rest) = if x == y then (y, S n) :: rest else (y, n) :: bump x rest

countsOf : Eq a => List a -> List (a, Nat)
countsOf = foldl (flip bump) []

decrCount : Eq a => a -> List (a, Nat) -> List (a, Nat)
decrCount x = map (\(y, n) => if y == x then (y, minus n 1) else (y, n))

permsFromCounts : Eq a => List (a, Nat) -> List (List a)
permsFromCounts cs =
  if all (\p => snd p == Z) cs
    then [[]]
    else concatMap
           (\p => if snd p == Z then []
                  else map (fst p ::) (permsFromCounts (decrCount (fst p) cs)))
           cs

permuteUnique : Eq a => List a -> List (List a)
permuteUnique xs = permsFromCounts (countsOf xs)

main : IO ()
main = printLn (permuteUnique [1,1,2])  -- [[1,1,2],[1,2,1],[2,1,1]]
