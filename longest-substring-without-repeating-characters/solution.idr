module Main

import Data.List

-- last-seen index per char as an assoc list
setIdx : Char -> Nat -> List (Char, Nat) -> List (Char, Nat)
setIdx c i [] = [(c, i)]
setIdx c i ((d, j) :: rest) = if c == d then (c, i) :: rest else (d, j) :: setIdx c i rest

-- Sliding window: jump start past any repeat of the current char.
lengthOfLongest : String -> Nat
lengthOfLongest s = go 0 0 [] 0 (unpack s)
  where
    go : Nat -> Nat -> List (Char, Nat) -> Nat -> List Char -> Nat
    go _ _ _ best [] = best
    go i start seen best (c :: cs) =
      let start' = case lookup c seen of
                     Just j  => max start (S j)
                     Nothing => start
      in go (S i) start' (setIdx c i seen) (max best (minus (S i) start')) cs

main : IO ()
main = do
  printLn (lengthOfLongest "abcabcbb")  -- 3
  printLn (lengthOfLongest "bbbbb")     -- 1
  printLn (lengthOfLongest "pwwkew")    -- 3
