module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Cont (Cont, runCont, callCC)

-- Two takes on the same problem:
--  * isValid   -- the explicit-stack fold (push openers, pop on matching closer)
--  * isValidCC -- the *continuation monad*, a faithful mirror of the Racket
--                 `let/cc` version (solution.rkt): recursive-descent parse of a
--                 balanced prefix that ESCAPES to False on any mismatch.
--                 `callCC \k -> ...` gives the escape `k` exactly as Racket's
--                 `(let/cc k ...)` does, and `k False` is Racket's `(k #f)`.

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

-- Continuation-monad version, mirroring solution.rkt's `let/cc`.  `parse`
-- consumes a maximal run of balanced groups and returns the rest; on any
-- mismatched closer it bails straight out through the captured continuation.
isValidCC :: [Char] -> Bool
isValidCC s = runCont (callCC parser) id where
  parser k = fmap null (parse s) where
    parse lst = case lst of
      ('(':r) -> matched ')' r
      ('[':r) -> matched ']' r
      ('{':r) -> matched '}' r
      _       -> return lst                 -- not an opener: hand back the rest
    matched close r = do
      r' <- parse r                         -- parse the inside
      case r' of
        (c:r'') | c == close -> parse r''   -- expected closer: keep going
        _                    -> k False      -- mismatch: escape (Racket's (k #f))

main :: IO ()
main = do
  let examples = ["()[]{}", "(]", "{[]}", "(", ")(", ""]
  mapM_ (\x -> putStrLn $ show x ++ "  stack=" ++ show (isValid x)
                       ++ "  cont=" ++ show (isValidCC x)) examples
