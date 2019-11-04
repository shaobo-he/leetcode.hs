module Main where

import Data.List
import Data.Function

merge :: Ord a => [[a]] -> [[a]]
merge [] = []
merge lst = foldl merge' [(head sortedLst)] (tail sortedLst) where
  merge' res@((clb:cub:[]):r) interval@(lb:ub:[]) =
    if cub >= lb
       then (clb:(max ub cub):[]):r
       else interval:res
  sortedLst = sortBy (compare `on` head) lst

main :: IO ()
main = do
  putStrLn $ show $ merge [[1,3],[2,6],[8,10],[15,18]]
  putStrLn $ show $ merge [[1,4],[4,5]]
