module Main where

-- LeetCode 208: Implement Trie (Prefix Tree).
--
-- Functional model: a trie is `Node isEnd children`, where `children` is an
-- assoc list mapping a character to a sub-trie.  insert/search/startsWith all
-- recurse structurally on the key (a String).  `build = foldr insert empty`
-- builds a trie from a word list — exactly the spec the Lean proof refines.

data Trie = Node Bool [(Char, Trie)]

emptyTrie :: Trie
emptyTrie = Node False []

-- update the child keyed by `c` with `f` applied to the old child (or empty)
childUpdate :: Char -> (Trie -> Trie) -> [(Char, Trie)] -> [(Char, Trie)]
childUpdate c f [] = [(c, f emptyTrie)]
childUpdate c f ((y, t) : rest)
  | c == y    = (y, f t) : rest
  | otherwise = (y, t) : childUpdate c f rest

insert :: String -> Trie -> Trie
insert [] (Node _ cs) = Node True cs
insert (x : xs) (Node e cs) = Node e (childUpdate x (insert xs) cs)

childLookup :: Char -> [(Char, Trie)] -> Maybe Trie
childLookup _ [] = Nothing
childLookup c ((y, t) : rest)
  | c == y    = Just t
  | otherwise = childLookup c rest

search :: String -> Trie -> Bool
search [] (Node e _) = e
search (x : xs) (Node _ cs) = maybe False (search xs) (childLookup x cs)

startsWith :: String -> Trie -> Bool
startsWith [] _ = True
startsWith (x : xs) (Node _ cs) = maybe False (startsWith xs) (childLookup x cs)

build :: [String] -> Trie
build = foldr insert emptyTrie

main :: IO ()
main = do
  let t = build ["apple"]
  print (search "apple" t)        -- True
  print (search "app" t)          -- False
  print (startsWith "app" t)      -- True
  print (startsWith "b" t)        -- False
  let t2 = build ["apple", "app", "application"]
  print (search "app" t2)         -- True
  print (search "appl" t2)        -- False
  print (startsWith "appl" t2)    -- True
  print (startsWith "apz" t2)     -- False
