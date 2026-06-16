-- Serialize and Deserialize N-ary Tree (Lean 4, core library only)

inductive Tree where
  | node : Int → List Tree → Tree
deriving Inhabited

-- Pre-order parenthesised encoding: (val child1 child2 ...).
-- Total via a structural helper over the child list (Lean's termination checker
-- can't see `cs.map serialize` as decreasing).
mutual
  def serialize : Tree → String
    | Tree.node v cs => "(" ++ toString v ++ serializeForest cs ++ ")"
  def serializeForest : List Tree → String
    | []      => ""
    | c :: cs => serialize c ++ serializeForest cs
end

def isDigit' (c : Char) : Bool := c ≥ '0' && c ≤ '9'

-- Parse a non-negative integer (the trees in the examples use non-negative values).
def parseInt (cs : List Char) : Int × List Char :=
  let ds := cs.takeWhile isDigit'
  let rest := cs.dropWhile isDigit'
  (String.toInt! (String.ofList ds), rest)

-- parseInt's remainder (a `dropWhile`) is never longer than its input
theorem parseInt_len (cs : List Char) : (parseInt cs).2.length ≤ cs.length := by
  show (cs.dropWhile isDigit').length ≤ cs.length
  induction cs with
  | nil => simp
  | cons c cs ih =>
    simp only [List.dropWhile_cons]
    split
    · simp only [List.length_cons]; omega
    · simp

-- Total recursive-descent parse: a single forest parser (a node is its own
-- `( value children )`).  Its return type carries `remainder.length ≤ input`, so
-- both recursive calls provably shrink the input — the first consumes a `(`, the
-- second is bounded by the first call's carried proof.  No fuel, no `partial`.
def parseForest : (input : List Char) →
    List Tree × { r : List Char // r.length ≤ input.length }
  | ')' :: rest => ([], ⟨rest, Nat.le_succ _⟩)
  | '(' :: rest =>
      let (children, rest2) := parseForest (parseInt rest).2
      let (sibs, rest3)     := parseForest rest2.val
      (Tree.node (parseInt rest).1 children :: sibs,
        ⟨rest3.val,
          Nat.le_trans rest3.property
            (Nat.le_trans rest2.property
              (Nat.le_trans (parseInt_len rest) (Nat.le_succ _)))⟩)
  | cs => ([], ⟨cs, Nat.le_refl _⟩)
  termination_by input => input.length
  decreasing_by
    · simp_wf; have := parseInt_len rest; omega
    · simp_wf; have := parseInt_len rest; have := rest2.property; omega

def deserialize (s : String) : Tree :=
  (parseForest s.toList).1.headD (Tree.node 0 [])

def t : Tree :=
  Tree.node 1 [Tree.node 3 [Tree.node 5 [], Tree.node 6 []], Tree.node 2 [], Tree.node 4 []]

#guard serialize t == "(1(3(5)(6))(2)(4))"
#guard serialize (deserialize (serialize t)) == serialize t

/- ════════════════════════════════════════════════════════════════════════
   ROUND-TRIP PROOF (verified total model).

   `deserialize (serialize t) = t`, proved for the GRAMMAR: a total fuel-driven
   parser over a token stream with atomic `Nat` values.  This is the substance of
   the round-trip — paren matching, child order, and "the parser returns exactly
   the remaining input" — where serializer bugs live.  The shipped `parseForest`
   above lexes decimal digits and carries a remainder-length bound; here the
   parser is fuel-driven and the decimal lexer (Char ↔ Nat) is abstracted as the
   atomic token `Tok.val`.  Mutual `Tree`/`Forest` mirrors the shipped `node`.
-/
namespace Roundtrip

mutual
  inductive Tree where
    | node : Nat → Forest → Tree
  inductive Forest where
    | nil  : Forest
    | cons : Tree → Forest → Forest
end

inductive Tok where
  | lp | rp | val : Nat → Tok
deriving DecidableEq, Repr

-- serialize: preorder, '(' value children ')'
mutual
  def ser : Tree → List Tok
    | .node v cs => Tok.lp :: Tok.val v :: (serF cs ++ [Tok.rp])
  def serF : Forest → List Tok
    | .nil       => []
    | .cons t cs => ser t ++ serF cs
end

-- a size measure, used as parse fuel
mutual
  def size : Tree → Nat
    | .node _ cs => 1 + sizeF cs
  def sizeF : Forest → Nat
    | .nil       => 1
    | .cons t cs => size t + sizeF cs
end

theorem size_pos (t : Tree) : 1 ≤ size t := by
  cases t with | node v cs => simp only [size]; omega

theorem sizeF_pos (f : Forest) : 1 ≤ sizeF f := by
  cases f with
  | nil => simp [sizeF]
  | cons t cs => simp only [sizeF]; have := size_pos t; omega

-- total parser, structural on the fuel
mutual
  def parseT : Nat → List Tok → Option (Tree × List Tok)
    | fuel + 1, Tok.lp :: Tok.val v :: rest =>
        match parseF fuel rest with
        | some (cs, rest') => some (.node v cs, rest')
        | none             => none
    | _, _ => none
  def parseF : Nat → List Tok → Option (Forest × List Tok)
    | _ + 1, Tok.rp :: rest => some (.nil, rest)
    | fuel + 1, toks@(Tok.lp :: _) =>
        match parseT fuel toks with
        | some (t, rest1) =>
            match parseF fuel rest1 with
            | some (f, rest2) => some (.cons t f, rest2)
            | none            => none
        | none => none
    | _, _ => none
end

def deserialize (toks : List Tok) : Option Tree :=
  (parseT toks.length toks).map Prod.fst

-- the parser consumes exactly `serialize t`, returning the untouched remainder.
-- Proved by mutual structural recursion on the tree / forest.
mutual
  theorem parseT_ser (t : Tree) (fuel : Nat) (rest : List Tok)
      (h : size t ≤ fuel) : parseT fuel (ser t ++ rest) = some (t, rest) := by
    cases t with
    | node v cs =>
      cases fuel with
      | zero => simp only [size] at h; omega
      | succ fuel =>
        have hF : parseF fuel (serF cs ++ Tok.rp :: rest) = some (cs, rest) :=
          parseF_ser cs fuel rest (by simp only [size] at h; omega)
        simp only [ser, List.cons_append, List.append_assoc, List.nil_append,
                   parseT, hF]

  theorem parseF_ser (f : Forest) (fuel : Nat) (rest : List Tok)
      (h : sizeF f ≤ fuel) : parseF fuel (serF f ++ Tok.rp :: rest) = some (f, rest) := by
    cases f with
    | nil =>
      cases fuel with
      | zero => simp only [sizeF] at h; omega
      | succ fuel => simp only [serF, List.nil_append, parseF]
    | cons t cs =>
      cases fuel with
      | zero => simp only [sizeF] at h; have := size_pos t; have := sizeF_pos cs; omega
      | succ fuel =>
        obtain ⟨tlt, hser⟩ : ∃ tlt, ser t = Tok.lp :: tlt := by
          cases t with | node v cs' => exact ⟨_, rfl⟩
        have hT : parseT fuel (ser t ++ (serF cs ++ Tok.rp :: rest))
                    = some (t, serF cs ++ Tok.rp :: rest) :=
          parseT_ser t fuel _ (by simp only [sizeF] at h; have := sizeF_pos cs; omega)
        have hF : parseF fuel (serF cs ++ Tok.rp :: rest) = some (cs, rest) :=
          parseF_ser cs fuel rest (by simp only [sizeF] at h; have := size_pos t; omega)
        simp only [hser, List.cons_append] at hT
        simp only [serF, List.append_assoc, hser, List.cons_append, parseF, hT, hF]
end

-- the serialization is long enough to supply its own parse fuel
mutual
  theorem size_le_len (t : Tree) : size t ≤ (ser t).length := by
    cases t with
    | node v cs =>
      simp only [ser, size, List.length_cons, List.length_append]
      have := sizeF_le_len cs; omega
  theorem sizeF_le_len (f : Forest) : sizeF f ≤ (serF f).length + 1 := by
    cases f with
    | nil => simp only [serF, sizeF, List.length_nil]; omega
    | cons t cs =>
      simp only [serF, sizeF, List.length_append]
      have := size_le_len t; have := sizeF_le_len cs; omega
end

-- payoff: deserialize undoes serialize, for EVERY tree
theorem roundtrip (t : Tree) : deserialize (ser t) = some t := by
  have h := parseT_ser t (ser t).length [] (size_le_len t)
  simp only [List.append_nil] at h
  simp only [deserialize, h, Option.map_some]

#print axioms roundtrip

-- concrete sanity check: the functions actually compute the round-trip
def sample : Tree :=
  .node 1 (.cons (.node 3 (.cons (.node 5 .nil) (.cons (.node 6 .nil) .nil)))
          (.cons (.node 2 .nil) (.cons (.node 4 .nil) .nil)))

example : deserialize (ser sample) = some sample := rfl

end Roundtrip
