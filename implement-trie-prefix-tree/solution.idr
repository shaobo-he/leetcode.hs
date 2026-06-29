module Main

------------------------------------------------------------------------
-- LeetCode 208: Implement Trie (Prefix Tree).
--
-- The alphabet is a SMALL inductive type `Sym`, so equality is decidable and
-- every function below is `total` (no `Char` coverage gaps).  A trie is
-- `Node isEnd children` with assoc-list children; insert/search/startsWith all
-- recurse structurally on the key word.  `build = foldr insert empty`.
--
-- future work: port the refinement equations proven in solution.lean
--   search (build ws) w  =  (w `elem` ws)
--   startsWith (build ws) p = (p == []) || any (isPrefix p) ws
-- The shipped functions here are total and match that spec on the examples; the
-- full inductive proof (assoc-list hit/miss lemmas + double induction) is the
-- Lean centerpiece.  A faithful Idris port needs `symEq` soundness lemmas to
-- bridge the boolean `symEq` with propositional `=`.
------------------------------------------------------------------------

data Sym = A | B | C | D | E | L | P

-- decidable / boolean equality on the small alphabet
symEq : Sym -> Sym -> Bool
symEq A A = True
symEq B B = True
symEq C C = True
symEq D D = True
symEq E E = True
symEq L L = True
symEq P P = True
symEq _ _ = False

Word : Type
Word = List Sym

wordEq : Word -> Word -> Bool
wordEq []        []        = True
wordEq (x :: xs) (y :: ys) = symEq x y && wordEq xs ys
wordEq _         _         = False

data Trie = Node Bool (List (Sym, Trie))

emptyTrie : Trie
emptyTrie = Node False []

------------------------------------------------------------------------
-- insert (mutual with the child-list update).  `total`: size-change sees the
-- key word strictly shrink on the insert -> childInsert step of every cycle.
------------------------------------------------------------------------

mutual
  total
  insert : Word -> Trie -> Trie
  insert []        (Node _ cs) = Node True cs
  insert (b :: vs) (Node e cs) = Node e (childInsert b vs cs)

  total
  childInsert : Sym -> Word -> List (Sym, Trie) -> List (Sym, Trie)
  childInsert b vs []             = [(b, insert vs emptyTrie)]
  childInsert b vs ((y, t) :: r) =
    if symEq b y then (y, insert vs t) :: r
                 else (y, t) :: childInsert b vs r

total
childLookup : Sym -> List (Sym, Trie) -> Maybe Trie
childLookup _ []             = Nothing
childLookup x ((y, t) :: r) = if symEq x y then Just t else childLookup x r

total
search : Word -> Trie -> Bool
search []        (Node e _)  = e
search (x :: xs) (Node _ cs) = case childLookup x cs of
  Just t  => search xs t
  Nothing => False

total
startsWith : Word -> Trie -> Bool
startsWith []        _          = True
startsWith (x :: xs) (Node _ cs) = case childLookup x cs of
  Just t  => startsWith xs t
  Nothing => False

total
build : List Word -> Trie
build = foldr insert emptyTrie

------------------------------------------------------------------------
-- worked example: build ["apple"]
------------------------------------------------------------------------

apple : Word
apple = [A, P, P, L, E]

app : Word
app = [A, P, P]

bb : Word
bb = [B]

main : IO ()
main = do
  let t = build [apple]
  printLn (search apple t)        -- True
  printLn (search app t)          -- False
  printLn (startsWith app t)      -- True
  printLn (startsWith bb t)       -- False
