module Main

-- Kadane's algorithm.
maxSubArray : List Int -> Int
maxSubArray [] = 0
maxSubArray (x :: xs) = snd (foldl step (x, x) xs)
  where
    step : (Int, Int) -> Int -> (Int, Int)
    step (cur, best) n = let cur' = max n (cur + n) in (cur', max best cur')

main : IO ()
main = do
  printLn (maxSubArray [-2,1,-3,4,-1,2,1,-5,4])  -- 6
  printLn (maxSubArray [5,4,-1,7,8])             -- 23
