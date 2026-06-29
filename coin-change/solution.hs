module Main where

import Data.Array (Array, listArray, (!))

-- LeetCode 322: Coin Change — fewest coins summing to `amount`, or Nothing if
-- impossible.  Bottom-up DP backed by a lazy immutable array: dp ! a = fewest
-- coins to make `a`, computed from the strictly smaller subproblems dp ! (a-c).
coinChange :: [Int] -> Int -> Maybe Int
coinChange coins amount = dp ! amount
  where
    dp :: Array Int (Maybe Int)
    dp = listArray (0, amount) [best a | a <- [0 .. amount]]
    best :: Int -> Maybe Int
    best 0 = Just 0
    best a =
      case [1 + k | c <- coins, c > 0, c <= a, Just k <- [dp ! (a - c)]] of
        [] -> Nothing
        xs -> Just (minimum xs)

main :: IO ()
main = do
  print (coinChange [1, 2, 5] 11)   -- Just 3
  print (coinChange [2] 3)          -- Nothing
  print (coinChange [1, 2, 5] 0)    -- Just 0
  print (coinChange [1] 0)          -- Just 0
  print (coinChange [1] 2)          -- Just 2
  print (coinChange [186, 419, 83, 408] 6249)  -- Just 20
