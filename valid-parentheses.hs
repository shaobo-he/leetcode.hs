module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

isValid :: [Char] -> Bool
isValid str = isValidAccum str [] where
  isValidAccum [] [] = True
  isValidAccum [] _ = False
  isValidAccum (h:r) stack
    | isOpen h = isValidAccum r (h:stack)
    | isClose h = if (not $ null stack) && (getCounterPart $ head stack) == h
                     then isValidAccum r (tail stack)
                     else False where
      isOpen c = c `Set.member` openPars
      isClose c = c `Set.member` closePars
      getCounterPart p = let (Just v) =
                               Map.lookup
                               p
                               (Map.fromList [('(',')'), ('[',']'), ('{','}')])
                          in v
      openPars = Set.fromList ['(', '[', '{']
      closePars = Set.fromList [')', ']', '}']

main :: IO ()
main = do
  putStrLn $ show $ isValid "()[]{}"
  putStrLn $ show $ isValid "(]"
  putStrLn $ show $ isValid "{[]}"
