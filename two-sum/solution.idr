module Main

import Data.List

-- Hash complement lookup via an assoc list of value -> index seen so far.
twoSum : List Int -> Int -> Maybe (Nat, Nat)
twoSum nums target = go 0 [] nums
  where
    go : Nat -> List (Int, Nat) -> List Int -> Maybe (Nat, Nat)
    go _ _ [] = Nothing
    go i seen (x :: xs) =
      case lookup (target - x) seen of
        Just j  => Just (j, i)
        Nothing => go (S i) ((x, i) :: seen) xs

main : IO ()
main = do
  printLn (twoSum [2,7,11,15] 9)  -- Just (0, 1)
  printLn (twoSum [3,2,4] 6)      -- Just (1, 2)
