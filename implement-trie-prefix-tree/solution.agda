module solution where

-- LeetCode 208: Implement Trie (Prefix Tree).
--
-- The alphabet is a SMALL inductive type `Sym` with decidable equality, so all
-- functions are TOTAL by structure (no TERMINATING pragma): insert/search/
-- startsWith recurse structurally on the key word, and the child-list helpers
-- recurse structurally on the assoc list.  `build = foldr insert empty`.
--
-- Compile-time `refl` tests (the repo's `#guard` analogue) check the worked
-- example build ["apple"].
--
-- The refinement equations proven in solution.lean are ported in full below
-- (search-build / startsWith-build), via a soundness bridge for the hand-rolled
-- `symEq` and a double-structural induction over `insert` resting on assoc-list
-- lookup hit/miss lemmas.  No pragmas / postulates.

open import Data.Bool using (Bool; true; false; _∨_; if_then_else_; not)
open import Data.Bool.Properties using (∨-identityʳ; ∨-assoc; ∨-comm)
open import Data.List using (List; []; _∷_; foldr; any)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong)
open import Data.Product using (_×_; _,_)
open import Data.Empty using (⊥-elim)

-- the small alphabet (enough letters for the examples)
data Sym : Set where
  A B C D E L P : Sym

-- decidable equality, by pattern matching on the (finite) constructors
_≟_ : (x y : Sym) → Dec (x ≡ y)
A ≟ A = yes refl
B ≟ B = yes refl
C ≟ C = yes refl
D ≟ D = yes refl
E ≟ E = yes refl
L ≟ L = yes refl
P ≟ P = yes refl
A ≟ B = no (λ ()) ; A ≟ C = no (λ ()) ; A ≟ D = no (λ ()) ; A ≟ E = no (λ ())
A ≟ L = no (λ ()) ; A ≟ P = no (λ ())
B ≟ A = no (λ ()) ; B ≟ C = no (λ ()) ; B ≟ D = no (λ ()) ; B ≟ E = no (λ ())
B ≟ L = no (λ ()) ; B ≟ P = no (λ ())
C ≟ A = no (λ ()) ; C ≟ B = no (λ ()) ; C ≟ D = no (λ ()) ; C ≟ E = no (λ ())
C ≟ L = no (λ ()) ; C ≟ P = no (λ ())
D ≟ A = no (λ ()) ; D ≟ B = no (λ ()) ; D ≟ C = no (λ ()) ; D ≟ E = no (λ ())
D ≟ L = no (λ ()) ; D ≟ P = no (λ ())
E ≟ A = no (λ ()) ; E ≟ B = no (λ ()) ; E ≟ C = no (λ ()) ; E ≟ D = no (λ ())
E ≟ L = no (λ ()) ; E ≟ P = no (λ ())
L ≟ A = no (λ ()) ; L ≟ B = no (λ ()) ; L ≟ C = no (λ ()) ; L ≟ D = no (λ ())
L ≟ E = no (λ ()) ; L ≟ P = no (λ ())
P ≟ A = no (λ ()) ; P ≟ B = no (λ ()) ; P ≟ C = no (λ ()) ; P ≟ D = no (λ ())
P ≟ E = no (λ ()) ; P ≟ L = no (λ ())

-- boolean equality, used by the executable trie
symEq : Sym → Sym → Bool
symEq x y with x ≟ y
... | yes _ = true
... | no  _ = false

-- soundness bridge: connect the boolean test `symEq` with propositional `≡`
symEq-refl : ∀ x → symEq x x ≡ true
symEq-refl x with x ≟ x
... | yes _ = refl
... | no ¬p = ⊥-elim (¬p refl)

symEq-false : ∀ {x y} → ¬ (x ≡ y) → symEq x y ≡ false
symEq-false {x} {y} ¬p with x ≟ y
... | yes p = ⊥-elim (¬p p)
... | no  _ = refl

Word : Set
Word = List Sym

wordEq : Word → Word → Bool
wordEq []       []       = true
wordEq (x ∷ xs) (y ∷ ys) = if symEq x y then wordEq xs ys else false
wordEq _        _        = false

data Trie : Set where
  node : Bool → List (Sym × Trie) → Trie

empty : Trie
empty = node false []

-- insert (mutual with the child-list update); TOTAL by structure — the key word
-- strictly shrinks on the insert → childInsert step of every recursive cycle.
mutual
  insert : Word → Trie → Trie
  insert []       (node _ cs) = node true cs
  insert (b ∷ vs) (node e cs) = node e (childInsert b vs cs)

  childInsert : Sym → Word → List (Sym × Trie) → List (Sym × Trie)
  childInsert b vs []             = (b , insert vs empty) ∷ []
  childInsert b vs ((y , t) ∷ r) =
    if symEq b y then (y , insert vs t) ∷ r
                 else (y , t) ∷ childInsert b vs r

childLookup : Sym → List (Sym × Trie) → Maybe Trie
childLookup x []             = nothing
childLookup x ((y , t) ∷ r) = if symEq x y then just t else childLookup x r

-- the child reached by `x`, defaulting to `empty` when absent (Lean's `lookupD`)
childLookupD : Sym → List (Sym × Trie) → Trie
childLookupD x cs with childLookup x cs
... | just t  = t
... | nothing = empty

search : Word → Trie → Bool
search []       (node e _)  = e
search (x ∷ xs) (node _ cs) with childLookup x cs
... | just t  = search xs t
... | nothing = false

startsWith : Word → Trie → Bool
startsWith []       _          = true
startsWith (x ∷ xs) (node _ cs) with childLookup x cs
... | just t  = startsWith xs t
... | nothing = false

build : List Word → Trie
build = foldr insert empty

-- worked example: build ["apple"]
apple : Word
apple = A ∷ P ∷ P ∷ L ∷ E ∷ []

app : Word
app = A ∷ P ∷ P ∷ []

bb : Word
bb = B ∷ []

_ : search apple (build (apple ∷ [])) ≡ true
_ = refl

_ : search app (build (apple ∷ [])) ≡ false
_ = refl

_ : startsWith app (build (apple ∷ [])) ≡ true
_ = refl

_ : startsWith bb (build (apple ∷ [])) ≡ false
_ = refl

-- ───────────────────────────────────────────────────────────────────────────
-- Refinement: the trie computes exactly the list-of-words spec (port of the
-- Lean theorems search_build / startsWith_build).  Boolean spec functions:
-- ───────────────────────────────────────────────────────────────────────────

isNil : Word → Bool
isNil []      = true
isNil (_ ∷ _) = false

-- our own prefix test on words (mirrors solution.lean's `isPrefix`)
isPrefix : Word → Word → Bool
isPrefix []       _        = true
isPrefix (_ ∷ _)  []       = false
isPrefix (a ∷ as) (b ∷ bs) = if symEq a b then isPrefix as bs else false

-- boolean membership of a word in a word list
member : Word → List Word → Bool
member w []       = false
member w (v ∷ vs) = wordEq w v ∨ member w vs

-- base facts: the empty trie
search-empty : ∀ w → search w empty ≡ false
search-empty []      = refl
search-empty (_ ∷ _) = refl

startsWith-empty : ∀ p → startsWith p empty ≡ isNil p
startsWith-empty []      = refl
startsWith-empty (_ ∷ _) = refl

-- assoc-list lookup HIT: looking up the just-inserted key returns `insert vs`
-- of the old child (or of `empty` when the key was absent).
lookup-self : (b : Sym) (vs : Word) (cs : List (Sym × Trie)) →
    childLookup b (childInsert b vs cs) ≡ just (insert vs (childLookupD b cs))
lookup-self b vs []             rewrite symEq-refl b = refl
lookup-self b vs ((y , t) ∷ r) with b ≟ y
... | yes refl rewrite symEq-refl b      = refl
... | no  b≢y  rewrite symEq-false b≢y   = lookup-self b vs r

-- assoc-list lookup MISS: looking up a different key is unaffected by a
-- `childInsert` on `b`.
lookup-other : (a b : Sym) (vs : Word) (cs : List (Sym × Trie)) → ¬ (a ≡ b) →
    childLookup a (childInsert b vs cs) ≡ childLookup a cs
lookup-other a b vs []             a≢b rewrite symEq-false a≢b = refl
lookup-other a b vs ((y , t) ∷ r) a≢b with b ≟ y
... | yes refl rewrite symEq-refl b | symEq-false a≢b = refl
... | no  b≢y  rewrite symEq-false b≢y with a ≟ y
...   | yes refl rewrite symEq-refl a    = refl
...   | no  a≢y  rewrite symEq-false a≢y  = lookup-other a b vs r a≢b

-- refinement of `search` over a single `insert`: inserting `v` makes `w` an
-- end-word iff it already was, or `w = v`.  (Lean's `search_insert`.)
search-insert : (w v : Word) (t : Trie) →
    search w (insert v t) ≡ (wordEq w v ∨ search w t)
search-insert []       []       (node e cs) = refl
search-insert []       (b ∷ vs) (node e cs) = refl
search-insert (a ∷ ws) []       (node e cs) = refl
search-insert (a ∷ ws) (b ∷ vs) (node e cs) with a ≟ b
... | yes refl rewrite lookup-self a vs cs | symEq-refl a with childLookup a cs
...   | just t0 = search-insert ws vs t0
...   | nothing = trans (search-insert ws vs empty)
                        (cong (wordEq ws vs ∨_) (search-empty ws))
search-insert (a ∷ ws) (b ∷ vs) (node e cs) | no a≢b
  rewrite lookup-other a b vs cs a≢b | symEq-false a≢b = refl

search-build : (w : Word) (ws : List Word) → search w (build ws) ≡ member w ws
search-build w []       = search-empty w
search-build w (v ∷ vs) =
  trans (search-insert w v (build vs))
        (cong (wordEq w v ∨_) (search-build w vs))

-- refinement of `startsWith` over a single `insert`.  (Lean's `startsWith_insert`.)
nil-or-isPrefix : (p vs : Word) → (isNil p ∨ isPrefix p vs) ≡ isPrefix p vs
nil-or-isPrefix []      vs = refl
nil-or-isPrefix (_ ∷ _) vs = refl

startsWith-insert : (p v : Word) (t : Trie) →
    startsWith p (insert v t) ≡ (startsWith p t ∨ isPrefix p v)
startsWith-insert []       v        t           = refl
startsWith-insert (a ∷ ps) []       (node e cs) = sym (∨-identityʳ _)
startsWith-insert (a ∷ ps) (b ∷ vs) (node e cs) with a ≟ b
... | yes refl rewrite lookup-self a vs cs | symEq-refl a with childLookup a cs
...   | just t0 = startsWith-insert ps vs t0
...   | nothing = trans (startsWith-insert ps vs empty)
                        (trans (cong (_∨ isPrefix ps vs) (startsWith-empty ps))
                               (nil-or-isPrefix ps vs))
startsWith-insert (a ∷ ps) (b ∷ vs) (node e cs) | no a≢b
  rewrite lookup-other a b vs cs a≢b | symEq-false a≢b = sym (∨-identityʳ _)

startsWith-build : (p : Word) (ws : List Word) →
    startsWith p (build ws) ≡ (isNil p ∨ any (isPrefix p) ws)
startsWith-build p []       =
  trans (startsWith-empty p) (sym (∨-identityʳ _))
startsWith-build p (v ∷ vs) =
  trans (startsWith-insert p v (build vs))
        (trans (cong (_∨ isPrefix p v) (startsWith-build p vs))
               (rearrange (isNil p) (any (isPrefix p) vs) (isPrefix p v)))
  where
    rearrange : (x y z : Bool) → ((x ∨ y) ∨ z) ≡ (x ∨ (z ∨ y))
    rearrange x y z = trans (∨-assoc x y z) (cong (x ∨_) (∨-comm y z))
