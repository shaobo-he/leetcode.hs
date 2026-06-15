module Main

import Data.List
import Data.Nat

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

------------------------------------------------------------------------
-- ROUND-TRIP PROOF (verified total model).
--
-- `parseT (size t) (ser t) = Just (t, [])`, proved for the GRAMMAR: a total
-- fuel-driven parser over a token stream with atomic `Nat` values -- paren
-- matching, child order, and "the parser returns exactly the remaining input".
-- The shipped parser above is partial and lexes decimal digits; here the parser
-- is `total` (structural on the fuel) and the decimal lexer is abstracted as the
-- atomic token `Val`.  Unique names (RTree/ND/...) avoid clashing with the
-- shipped `Tree`/`Node`/`deserialize`.
------------------------------------------------------------------------

mutual
  data RTree : Type where
    ND : Nat -> RForest -> RTree
  data RForest : Type where
    FNil : RForest
    FCons : RTree -> RForest -> RForest

data Tok = LP | RP | Val Nat

-- serialize: preorder, '(' value children ')'
total
ser : RTree -> List Tok
total
serF : RForest -> List Tok
ser (ND v cs)     = LP :: Val v :: (serF cs ++ [RP])
serF FNil         = []
serF (FCons t cs) = ser t ++ serF cs

-- size measure, used as parse fuel
total
size : RTree -> Nat
total
sizeF : RForest -> Nat
size (ND _ cs)     = S (sizeF cs)
sizeF FNil         = 1
sizeF (FCons t cs) = size t + sizeF cs

-- LTE helpers, proved directly (names differ across base versions)
total
lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero _             = LTEZero
lteTrans (LTESucc p) (LTESucc q) = LTESucc (lteTrans p q)

total
lteReflexive : (n : Nat) -> LTE n n
lteReflexive Z     = LTEZero
lteReflexive (S k) = LTESucc (lteReflexive k)

total
lteAddR : (a : Nat) -> {b : Nat} -> LTE a (a + b)
lteAddR Z     = LTEZero
lteAddR (S k) = LTESucc (lteAddR k)

total
plusMonoL : (a : Nat) -> {m, n : Nat} -> LTE m n -> LTE (a + m) (a + n)
plusMonoL Z     p = p
plusMonoL (S k) p = LTESucc (plusMonoL k p)

total
sizePos : (t : RTree) -> LTE 1 (size t)
sizePos (ND v cs) = LTESucc LTEZero

total
sizeFPos : (f : RForest) -> LTE 1 (sizeF f)
sizeFPos FNil         = LTESucc LTEZero
sizeFPos (FCons t cs) = lteTrans (sizePos t) (lteAddR (size t))

-- arithmetic helpers (no `omega` here)
total
boundL : (a : Nat) -> {b, n : Nat} -> LTE 1 b -> LTE (a + b) (S n) -> LTE a n
boundL a oneB hab =
  fromLteSucc (replace {p = \x => LTE x (S n)} (plusCommutative a 1)
                (lteTrans (plusMonoL a oneB) hab))

total
boundR : (a : Nat) -> {b, n : Nat} -> LTE 1 a -> LTE (a + b) (S n) -> LTE b n
boundR a oneA hab = boundL b oneA (replace {p = \x => LTE x (S n)} (plusCommutative a b) hab)

-- total parser, structural on the fuel
total
parseT : Nat -> List Tok -> Maybe (RTree, List Tok)
total
parseF : Nat -> List Tok -> Maybe (RForest, List Tok)
parseT (S fuel) (LP :: Val v :: rest) = case parseF fuel rest of
  Just (cs, rest') => Just (ND v cs, rest')
  Nothing          => Nothing
parseT _ _ = Nothing
parseF (S fuel) (RP :: rest) = Just (FNil, rest)
parseF (S fuel) (LP :: ts) = case parseT fuel (LP :: ts) of
  Just (t, rest1) => case parseF fuel rest1 of
    Just (f, rest2) => Just (FCons t f, rest2)
    Nothing         => Nothing
  Nothing => Nothing
parseF _ _ = Nothing

-- the parser consumes exactly `ser t`, returning the untouched remainder
total
parseTSer : (t : RTree) -> (fuel : Nat) -> (rest : List Tok) ->
            LTE (size t) fuel -> parseT fuel (ser t ++ rest) = Just (t, rest)
total
parseFSer : (f : RForest) -> (fuel : Nat) -> (rest : List Tok) ->
            LTE (sizeF f) fuel -> parseF fuel (serF f ++ (RP :: rest)) = Just (f, rest)
parseTSer (ND v cs) Z rest h = absurd h
parseTSer (ND v cs) (S fuel) rest (LTESucc h) =
  rewrite sym (appendAssociative (serF cs) [RP] rest) in
  rewrite parseFSer cs fuel rest h in Refl
parseFSer FNil Z rest h = absurd h
parseFSer FNil (S fuel) rest h = Refl
parseFSer (FCons (ND v cs') cs) Z rest h = absurd h
parseFSer (FCons (ND v cs') cs) (S fuel) rest h =
  rewrite sym (appendAssociative (ser (ND v cs')) (serF cs) (RP :: rest)) in
  rewrite parseTSer (ND v cs') fuel (serF cs ++ (RP :: rest)) (boundL (size (ND v cs')) (sizeFPos cs) h) in
  rewrite parseFSer cs fuel rest (boundR (size (ND v cs')) (sizePos (ND v cs')) h) in Refl

-- payoff: parsing the serialization (with size-many fuel) recovers the tree
total
roundtrip : (t : RTree) -> parseT (size t) (ser t) = Just (t, [])
roundtrip t = rewrite sym (appendNilRightNeutral (ser t)) in parseTSer t (size t) [] (lteReflexive (size t))

-- a self-fuelling deserialize, with a concrete computed round-trip
total
deserialize2 : List Tok -> Maybe RTree
deserialize2 toks = case parseT (length toks) toks of
  Just (t, _) => Just t
  Nothing     => Nothing

sample : RTree
sample = ND 1 (FCons (ND 3 (FCons (ND 5 FNil) (FCons (ND 6 FNil) FNil)))
              (FCons (ND 2 FNil) (FCons (ND 4 FNil) FNil)))

exampleRT : deserialize2 (ser Main.sample) = Just Main.sample
exampleRT = Refl
