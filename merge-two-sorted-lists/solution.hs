module Main where

mergeTwoLists :: Ord a => [a] -> [a] -> [a]
mergeTwoLists [] ys = ys
mergeTwoLists xs [] = xs
mergeTwoLists (x:xs) (y:ys)
  | x <= y    = x : mergeTwoLists xs (y:ys)
  | otherwise = y : mergeTwoLists (x:xs) ys

main :: IO ()
main = print (mergeTwoLists [1,2,4] [1,3,4])  -- [1,1,2,3,4,4]
