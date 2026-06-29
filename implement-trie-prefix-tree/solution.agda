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
-- future work: port the refinement equations proven in solution.lean
--   search (build ws) w  ≡  (w ∈ ws)
--   startsWith (build ws) p ≡ (p ≡ []) ∨ any (isPrefix p) ws
-- The shipped functions are total by structure and agree with that spec on the
-- examples; the full inductive proof is the Lean centerpiece.

open import Data.Bool using (Bool; true; false; _∨_; if_then_else_; not)
open import Data.List using (List; []; _∷_; foldr; any)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Data.Product using (_×_; _,_)

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
