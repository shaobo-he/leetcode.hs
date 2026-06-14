module Main

incr : Eq a => a -> List (a, Nat) -> List (a, Nat)
incr x [] = [(x, 1)]
incr x ((y, n) :: rest) = if x == y then (y, S n) :: rest else (y, n) :: incr x rest

decr : Eq a => a -> List (a, Nat) -> List (a, Nat)
decr x [] = []
decr x ((y, n) :: rest) =
  if x == y
    then (case n of
            S (S k) => (y, S k) :: rest
            _       => rest)
    else (y, n) :: decr x rest

-- Sliding window holding at most k distinct chars; shrink from the left.
lenKDistinct : String -> Nat -> Nat
lenKDistinct s k = go [] [] 0 (unpack s)
  where
    shrink : List Char -> List (Char, Nat) -> (List Char, List (Char, Nat))
    shrink win counts =
      if length counts > k
        then (case win of
                (d :: ds) => shrink ds (decr d counts)
                []        => (win, counts))
        else (win, counts)
    go : List Char -> List (Char, Nat) -> Nat -> List Char -> Nat
    go _ _ best [] = best
    go win counts best (c :: cs) =
      let (win2, counts2) = shrink (win ++ [c]) (incr c counts)
      in go win2 counts2 (max best (length win2)) cs

main : IO ()
main = do
  printLn (lenKDistinct "eceba" 2)  -- 3
  printLn (lenKDistinct "aa" 1)     -- 2
  printLn (lenKDistinct "abee" 1)   -- 2
