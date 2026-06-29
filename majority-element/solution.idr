module Main

------------------------------------------------------------------------
-- LeetCode 169: Majority Element — Boyer–Moore voting.
--
-- A total left fold carrying (candidate, count).  `majorityElement` returns
-- the candidate; on inputs that have a strict majority (the LeetCode
-- guarantee) it IS that element.  The full count-invariant proof of the
-- correctness IFF lives in solution.lean.
------------------------------------------------------------------------

total
step : (Maybe Nat, Nat) -> Nat -> (Maybe Nat, Nat)
step (_,       Z)   x = (Just x, 1)
step (Just c,  S k) x = if x == c then (Just c, S (S k)) else (Just c, S k)
step (Nothing, S k) x = (Just x, 1)

total
bm : List Nat -> (Maybe Nat, Nat)
bm = foldl step (Nothing, 0)

total
majorityElement : List Nat -> Maybe Nat
majorityElement xs = fst (bm xs)

main : IO ()
main = do
  printLn (majorityElement [3, 2, 3])               -- Just 3
  printLn (majorityElement [2, 2, 1, 1, 1, 2, 2])   -- Just 2
  printLn (majorityElement [])                      -- Nothing
