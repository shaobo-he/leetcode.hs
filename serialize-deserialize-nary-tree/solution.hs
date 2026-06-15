module Main where

import Data.Char (isDigit)
import Control.Monad.State (State, evalState, get, put, state)

data Tree = Node Int [Tree] deriving (Eq, Show)

-- Pre-order parenthesised encoding: (val child1 child2 ...).
serialize :: Tree -> String
serialize (Node v cs) = "(" ++ show v ++ concatMap serialize cs ++ ")"

-- The recursive-descent parse threads the unconsumed input by hand as a
-- `(result, rest)` pair through every helper -- i.e. it IS the State monad over
-- the remaining string.  Spelling it as `State String` lets `do`/`>>=` thread
-- `rest`, so each parser just reads and advances the input.
deserialize :: String -> Tree
deserialize = evalState parse
  where
    -- a node is '(' val children ')'  (children consumes the closing ')')
    parse :: State String Tree
    parse = do
      char '('
      v  <- int
      cs <- children
      return (Node v cs)
    children :: State String [Tree]
    children = do
      s <- get
      case s of
        (')' : rest) -> put rest >> return []        -- end of this node's kids
        ('(' : _)    -> (:) <$> parse <*> children   -- another subtree, then more
        _            -> error "bad input"
    -- consume one expected character
    char :: Char -> State String ()
    char c = state $ \s -> case s of
      (x : xs) | x == c -> ((), xs)
      _                 -> error ("expected " ++ [c])
    -- consume a run of digits as an Int
    int :: State String Int
    int = do
      s <- get
      let (ds, rest) = span isDigit s
      put rest
      return (read ds)

main :: IO ()
main = do
  let t = Node 1 [Node 3 [Node 5 [], Node 6 []], Node 2 [], Node 4 []]
  putStrLn (serialize t)                   -- (1(3(5)(6))(2)(4))
  print (deserialize (serialize t) == t)   -- True
