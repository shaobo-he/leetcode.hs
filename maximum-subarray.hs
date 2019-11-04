module Main where

maxSubArray :: [Int] -> Int
maxSubArray arr = snd $ foldl updateMax (head arr, head arr) (tail arr) where
  updateMax (currMax, globalMax) num =
    let newCurrMax = max num (currMax+num) in
      let newGlobalMax = max globalMax newCurrMax in
        (newCurrMax, newGlobalMax)

main :: IO ()
main = do
  putStrLn $ show $ maxSubArray [-2,1,-3,4,-1,2,1,-5,4]
