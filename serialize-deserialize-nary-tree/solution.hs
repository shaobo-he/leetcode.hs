module Main where

data Tree = Node Int [Tree] deriving (Eq, Show)

-- Pre-order parenthesised encoding: (val child1 child2 ...).
serialize :: Tree -> String
serialize (Node v cs) = "(" ++ show v ++ concatMap serialize cs ++ ")"

deserialize :: String -> Tree
deserialize = fst . parse
  where
    parse ('(' : rest) =
      let (v, rest1)  = parseInt rest
          (cs, rest2) = parseChildren rest1
      in (Node v cs, rest2)
    parse _ = error "bad input"
    parseChildren (')' : rest) = ([], rest)
    parseChildren s@('(' : _) =
      let (t, rest1)  = parse s
          (ts, rest2) = parseChildren rest1
      in (t : ts, rest2)
    parseChildren _ = error "bad input"
    parseInt s = let (ds, rest) = span (`elem` ['0' .. '9']) s
                 in (read ds, rest)

main :: IO ()
main = do
  let t = Node 1 [Node 3 [Node 5 [], Node 6 []], Node 2 [], Node 4 []]
  putStrLn (serialize t)                   -- (1(3(5)(6))(2)(4))
  print (deserialize (serialize t) == t)   -- True
