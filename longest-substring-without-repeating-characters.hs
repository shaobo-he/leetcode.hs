module Main where

import Control.Monad.State.Lazy
import Control.Monad
import qualified Data.Map.Strict as Map

lengthOfLongestSubstring :: String -> Int
lengthOfLongestSubstring s = evalState (foldM findMaxLen 0 (zip s [0..])) (0, Map.empty)

-- the loop
-- the states consist of two parts, the starting index and the index map
-- the value associated with the state monad is the maximum length
findMaxLen :: Int -> (Char,Int) -> State (Int, Map.Map Char Int) Int
findMaxLen currMax (c,idx) = do
    (startIdx, idxMap) <- get
    let startIdxNew = case Map.lookup c idxMap of
                        Just oldIdx -> max oldIdx startIdx
                        Nothing -> startIdx
    put (startIdxNew, Map.insert c (idx+1) idxMap)
    return $ max currMax (idx-startIdxNew+1)

main :: IO ()
main = do
  putStrLn $ show $ lengthOfLongestSubstring "abcabcbb"
  putStrLn $ show $ lengthOfLongestSubstring "bbbbb"
  putStrLn $ show $ lengthOfLongestSubstring "pwwkew"
