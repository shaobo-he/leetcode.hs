module solution where

-- Serialize and Deserialize N-ary Tree (port of the Lean/Idris/Haskell solution).
-- Pre-order parenthesised encoding: (val child1 child2 ...).

open import Data.Bool using (Bool; true; false; _∧_)
open import Data.Nat using (ℕ; zero; suc; _∸_; _+_; _*_; _≤_; _<_; z≤n; s≤s)
open import Data.Nat.Properties using (≤-refl; ≤-trans; ≤-pred; n≤1+n; +-monoʳ-≤; +-comm; m≤m+n)
open import Data.Nat.Induction using (<-wellFounded)
open import Induction.WellFounded using (Acc; acc)
open import Data.Nat.Show renaming (show to showNat)
open import Data.Char using (Char; isDigit; toℕ)
open import Data.List using (List; []; _∷_; length) renaming (_++_ to _++ᴸ_)
open import Data.List.Properties using (++-assoc; ++-identityʳ)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.String using (String; toList; fromList; _++_)
open import Data.Product using (_×_; _,_; proj₁; proj₂; Σ; Σ-syntax)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; subst)

data Tree : Set where
  node : ℕ → List Tree → Tree

-- serialize is structurally recursive on the tree (children are smaller),
-- but Agda's termination checker cannot see through the `map` /`foldr`, so we
-- inline a helper `concatChildren` that recurses structurally on the list.
mutual
  serialize : Tree → String
  serialize (node v cs) = "(" ++ showNat v ++ concatChildren cs ++ ")"

  concatChildren : List Tree → String
  concatChildren []       = ""
  concatChildren (c ∷ cs) = serialize c ++ concatChildren cs

-- Parse a non-negative integer (the trees in the examples use non-negative values).
charDigit : Char → ℕ
charDigit c = toℕ c ∸ toℕ '0'

parseDigits : ℕ → List Char → ℕ × List Char
parseDigits accu []       = accu , []
parseDigits accu (c ∷ cs) with isDigit c
... | true  = parseDigits (accu * 10 + charDigit c) cs
... | false = accu , (c ∷ cs)

parseDigits-len : (accu : ℕ) (cs : List Char) →
    length (proj₂ (parseDigits accu cs)) ≤ length cs
parseDigits-len accu []       = ≤-refl
parseDigits-len accu (c ∷ cs) with isDigit c
... | true  = ≤-trans (parseDigits-len (accu * 10 + charDigit c) cs) (n≤1+n (length cs))
... | false = ≤-refl

parseInt : List Char → ℕ × List Char
parseInt cs = parseDigits 0 cs

parseInt-len : (cs : List Char) → length (proj₂ (parseInt cs)) ≤ length cs
parseInt-len cs = parseDigits-len 0 cs

-- TOTAL, no TERMINATING pragma.  parseTree's '('-body is inlined into
-- parseChildren's '(' clause, which removes the only same-input call
-- (the original parseChildren → parseTree on the unchanged list).  Every
-- parseChildren recursion then strictly shortens the input, so plain
-- well-founded recursion on `length` suffices — no rank measure needed.
-- parseChildren carries a proof `length remainder ≤ length input`; parseTree
-- is a non-recursive wrapper that feeds a fresh accessibility witness.

CRes : List Char → Set
CRes cs = List Tree × Σ[ r ∈ List Char ] (length r ≤ length cs)

parseChildren : (cs : List Char) → Acc _<_ (length cs) → CRes cs
parseChildren (')' ∷ rest) _         = [] , (rest , n≤1+n (length rest))
parseChildren ('(' ∷ rest) (acc rec) =
  let v   = proj₁ (parseInt rest)
      r0  = proj₂ (parseInt rest)
      p0  = parseInt-len rest                      -- length r0 ≤ length rest
      icr = parseChildren r0 (rec (s≤s p0))        -- the node's own children
      icB = ≤-trans (proj₂ (proj₂ icr)) p0         -- length (rem icr) ≤ length rest
      sib = parseChildren (proj₁ (proj₂ icr)) (rec (s≤s icB))  -- following siblings
      sibB = ≤-trans (proj₂ (proj₂ sib)) icB       -- length (rem sib) ≤ length rest
  in (node v (proj₁ icr) ∷ proj₁ sib)
   , (proj₁ (proj₂ sib) , ≤-trans sibB (n≤1+n (length rest)))
parseChildren cs _ = [] , (cs , ≤-refl)

parseTree : List Char → Tree × List Char
parseTree ('(' ∷ rest) =
  let v  = proj₁ (parseInt rest)
      r0 = proj₂ (parseInt rest)
      cr = parseChildren r0 (<-wellFounded (length r0))
  in node v (proj₁ cr) , proj₁ (proj₂ cr)
parseTree cs = node 0 [] , cs

deserialize : String → Tree
deserialize s = proj₁ (parseTree (toList s))

t : Tree
t = node 1 (node 3 (node 5 [] ∷ node 6 [] ∷ []) ∷ node 2 [] ∷ node 4 [] ∷ [])

-- compile-time tests (refl fails to typecheck if the computation is wrong)
_ : serialize t ≡ "(1(3(5)(6))(2)(4))"
_ = refl

_ : serialize (deserialize (serialize t)) ≡ serialize t
_ = refl

-- ════════════════════════════════════════════════════════════════════════
-- ROUND-TRIP PROOF (verified total model).
--
-- `parseT (size t) (ser t) ≡ just (t , [])`, proved for the GRAMMAR: a total
-- fuel-driven parser over a token stream with atomic `ℕ` values.  This is the
-- substance of the round-trip — paren matching, child order, and "the parser
-- returns exactly the remaining input".  The shipped parser above is also total
-- (well-founded on `length`) but only its termination is checked, not this
-- correctness equation; here the parser is structural on the fuel and the
-- decimal lexer is abstracted as `val`, which is what makes the equation
-- provable.  (No module wrapper: unique names `RTree`/`RForest`/`nd`/
-- `deserialize₂` avoid clashing with the shipped `Tree`/`node`/`deserialize`.)

mutual
  data RTree : Set where
    nd : ℕ → RForest → RTree
  data RForest : Set where
    fnil  : RForest
    fcons : RTree → RForest → RForest

data Tok : Set where
  lp rp : Tok
  val   : ℕ → Tok

-- serialize: preorder, '(' value children ')'
ser  : RTree → List Tok
serF : RForest → List Tok
ser (nd v cs)     = lp ∷ val v ∷ (serF cs ++ᴸ (rp ∷ []))
serF fnil         = []
serF (fcons t cs) = ser t ++ᴸ serF cs

-- size measure, used as parse fuel
size  : RTree → ℕ
sizeF : RForest → ℕ
size (nd _ cs)     = suc (sizeF cs)
sizeF fnil         = 1
sizeF (fcons t cs) = size t + sizeF cs

size-pos : (t : RTree) → 1 ≤ size t
size-pos (nd v cs) = s≤s z≤n

sizeF-pos : (f : RForest) → 1 ≤ sizeF f
sizeF-pos fnil         = s≤s z≤n
sizeF-pos (fcons t cs) = ≤-trans (size-pos t) (m≤m+n (size t) (sizeF cs))

-- arithmetic helpers (no `omega` here); ≤-pred comes from Data.Nat.Properties
bound-l : ∀ {a b n} → 1 ≤ b → a + b ≤ suc n → a ≤ n
bound-l {a} {b} {n} 1≤b h =
  ≤-pred (subst (λ x → x ≤ suc n) (+-comm a 1) (≤-trans (+-monoʳ-≤ a 1≤b) h))

bound-r : ∀ {a b n} → 1 ≤ a → a + b ≤ suc n → b ≤ n
bound-r {a} {b} {n} 1≤a h = bound-l 1≤a (subst (λ x → x ≤ suc n) (+-comm a b) h)

-- total parser, structural on the fuel
parseT : ℕ → List Tok → Maybe (RTree × List Tok)
parseF : ℕ → List Tok → Maybe (RForest × List Tok)
parseT (suc fuel) (lp ∷ val v ∷ rest) with parseF fuel rest
... | just (cs , rest') = just (nd v cs , rest')
... | nothing           = nothing
parseT _ _ = nothing
parseF (suc fuel) (rp ∷ rest) = just (fnil , rest)
parseF (suc fuel) (lp ∷ ts) with parseT fuel (lp ∷ ts)
... | just (t , rest1) with parseF fuel rest1
...   | just (f , rest2) = just (fcons t f , rest2)
...   | nothing          = nothing
parseF (suc fuel) (lp ∷ ts) | nothing = nothing
parseF _ _ = nothing

-- the parser consumes exactly `ser t`, returning the untouched remainder
parseT-ser : (t : RTree) (fuel : ℕ) (rest : List Tok)
           → size t ≤ fuel → parseT fuel (ser t ++ᴸ rest) ≡ just (t , rest)
parseF-ser : (f : RForest) (fuel : ℕ) (rest : List Tok)
           → sizeF f ≤ fuel → parseF fuel (serF f ++ᴸ (rp ∷ rest)) ≡ just (f , rest)
parseT-ser (nd v cs) zero rest ()
parseT-ser (nd v cs) (suc fuel) rest (s≤s h)
  rewrite ++-assoc (serF cs) (rp ∷ []) rest
        | parseF-ser cs fuel rest h = refl
parseF-ser fnil zero rest ()
parseF-ser fnil (suc fuel) rest h = refl
parseF-ser (fcons (nd v cs') cs) zero rest ()
parseF-ser (fcons (nd v cs') cs) (suc fuel) rest h
  rewrite ++-assoc (ser (nd v cs')) (serF cs) (rp ∷ rest)
        | parseT-ser (nd v cs') fuel (serF cs ++ᴸ (rp ∷ rest)) (bound-l (sizeF-pos cs) h)
        | parseF-ser cs fuel rest (bound-r (size-pos (nd v cs')) h) = refl

-- payoff: parsing the serialization (with size-many fuel) recovers the tree
roundtrip : (t : RTree) → parseT (size t) (ser t) ≡ just (t , [])
roundtrip t = subst (λ z → parseT (size t) z ≡ just (t , []))
                    (++-identityʳ (ser t)) (parseT-ser t (size t) [] ≤-refl)

-- a self-fuelling deserialize, with a concrete computed round-trip
deserialize₂ : List Tok → Maybe RTree
deserialize₂ toks with parseT (length toks) toks
... | just (t , _) = just t
... | nothing      = nothing

sample : RTree
sample = nd 1 (fcons (nd 3 (fcons (nd 5 fnil) (fcons (nd 6 fnil) fnil)))
              (fcons (nd 2 fnil) (fcons (nd 4 fnil) fnil)))

_ : deserialize₂ (ser sample) ≡ just sample
_ = refl
