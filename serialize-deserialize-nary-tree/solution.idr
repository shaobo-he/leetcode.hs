module Main

import Data.List

data Tree = Node Int (List Tree)

-- Pre-order parenthesised encoding: (val child1 child2 ...).
serialize : Tree -> String
serialize (Node v cs) = "(" ++ show v ++ concatMap serialize cs ++ ")"

isDigit' : Char -> Bool
isDigit' c = c >= '0' && c <= '9'

parseInt : List Char -> (Int, List Char)
parseInt cs = let (ds, rest) = span isDigit' cs in (cast (pack ds), rest)

mutual
  parseTree : List Char -> (Tree, List Char)
  parseTree ('(' :: rest) =
    let (v, rest1)  = parseInt rest
        (cs, rest2) = parseChildren rest1
    in (Node v cs, rest2)
  parseTree cs = (Node 0 [], cs)

  parseChildren : List Char -> (List Tree, List Char)
  parseChildren (')' :: rest) = ([], rest)
  parseChildren cs@('(' :: _) =
    let (t, rest1)  = parseTree cs
        (ts, rest2) = parseChildren rest1
    in (t :: ts, rest2)
  parseChildren cs = ([], cs)

deserialize : String -> Tree
deserialize s = fst (parseTree (unpack s))

main : IO ()
main = do
  let t = Node 1 [Node 3 [Node 5 [], Node 6 []], Node 2 [], Node 4 []]
  putStrLn (serialize t)                                       -- (1(3(5)(6))(2)(4))
  printLn (serialize (deserialize (serialize t)) == serialize t)  -- True
