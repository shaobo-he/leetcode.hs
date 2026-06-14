-- Serialize and Deserialize N-ary Tree (Lean 4, core library only)

inductive Tree where
  | node : Int → List Tree → Tree
deriving Inhabited

-- Pre-order parenthesised encoding: (val child1 child2 ...).
partial def serialize : Tree → String
  | Tree.node v cs => "(" ++ toString v ++ String.join (cs.map serialize) ++ ")"

def isDigit' (c : Char) : Bool := c ≥ '0' && c ≤ '9'

-- Parse a non-negative integer (the trees in the examples use non-negative values).
def parseInt (cs : List Char) : Int × List Char :=
  let ds := cs.takeWhile isDigit'
  let rest := cs.dropWhile isDigit'
  (String.toInt! (String.ofList ds), rest)

mutual
  partial def parseTree : List Char → Tree × List Char
    | '(' :: rest =>
      let (v, rest1) := parseInt rest
      let (cs, rest2) := parseChildren rest1
      (Tree.node v cs, rest2)
    | cs => (Tree.node 0 [], cs)

  partial def parseChildren : List Char → List Tree × List Char
    | ')' :: rest => ([], rest)
    | cs@('(' :: _) =>
      let (t, rest1) := parseTree cs
      let (ts, rest2) := parseChildren rest1
      (t :: ts, rest2)
    | cs => ([], cs)
end

partial def deserialize (s : String) : Tree :=
  (parseTree s.toList).fst

def t : Tree :=
  Tree.node 1 [Tree.node 3 [Tree.node 5 [], Tree.node 6 []], Tree.node 2 [], Tree.node 4 []]

#guard serialize t == "(1(3(5)(6))(2)(4))"
#guard serialize (deserialize (serialize t)) == serialize t
