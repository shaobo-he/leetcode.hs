-- Combination of
-- https://github.com/sshastry/queenslogic
-- Algorithm Design in Haskell
{-# LANGUAGE MonadComprehensions #-}

module Main where

import Control.Monad.Logic
import Control.Monad

type Q = [Int]

choices :: [a] -> Logic a
choices = msum . map return

succs :: Int -> Q -> Logic Q
succs n qs = [q:qs | q <- choices [1..n], q `safe` qs] where
  safe q qs = (not $ elem q qs) && diagSafe q qs where
    diagSafe q qs = and [abs (q - c) /= r - 1 | (r,c) <- zip [2..] qs]

queens :: Int -> [Q]
queens n = observeAll $ foldl (>>-) (return mzero) (replicate n (succs n))

main :: IO ()
main = putStrLn $ show $ queens 4
