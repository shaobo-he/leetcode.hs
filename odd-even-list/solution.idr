module Main

odds : List a -> List a
odds (x :: _ :: rest) = x :: odds rest
odds [x] = [x]
odds [] = []

evens : List a -> List a
evens (_ :: y :: rest) = y :: evens rest
evens _ = []

-- Group elements at odd positions ahead of those at even positions.
oddEven : List a -> List a
oddEven xs = odds xs ++ evens xs

main : IO ()
main = do
  printLn (oddEven [1,2,3,4,5])      -- [1, 3, 5, 2, 4]
  printLn (oddEven [2,1,3,5,6,4,7])  -- [2, 3, 6, 7, 1, 5, 4]
