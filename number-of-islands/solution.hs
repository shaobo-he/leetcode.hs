{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Array.IO
import Control.Monad

type MIsland = IOArray (Int, Int) String

numIsland :: [[String]] -> IO Int -- sad
numIsland island = do
  mIsland <- (newListArray ((0,0), (lenY-1,lenX-1)) $ concat island) :: (IO MIsland)
  numIslandByIdx 0 0 0 mIsland where
  numIslandByIdx x y count mIsland
    | y == lenY = return count
    | x == lenX = numIslandByIdx x (y+1) count mIsland
    | otherwise = do
      e <- readArray mIsland (x,y)
      if e == "1" then  (expand x y mIsland) >> (numIslandByIdx x (y+1) (count+1) mIsland)
                  else numIslandByIdx x (y+1) count mIsland
  expand x y mIsland
    | (x < 0) || (y < 0) || (x >= lenX) || (y >= lenY) = return ()
    | otherwise = do
      e <- readArray mIsland (x,y)
      if e == "1" then (writeArray mIsland (x,y) "0") >> (expand x (y+1) mIsland) >> (expand x (y-1) mIsland) >> (expand (x+1) y mIsland) >> (expand (x-1) y mIsland)
                  else return ()
  lenX = length $ head island
  lenY = length island

main :: IO ()
main = do
  es <- numIsland [["1", "1", "1", "1", "0"], ["1", "1", "0", "1", "0"], ["1", "1", "0", "0", "0"], ["0", "0", "0", "0", "0"]]
  print es
