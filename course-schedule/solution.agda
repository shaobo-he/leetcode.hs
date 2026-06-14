module solution where

-- LeetCode 210: Course Schedule II
-- Topological sort via DFS.  graph as assoc list: prereq -> dependents.
-- Each prerequisite [a, b] means course b must be taken before course a.
-- Returns a valid order, or [] when a cycle is detected.

open import Data.Nat using (ℕ; zero; suc; _≡ᵇ_)
open import Data.Bool using (if_then_else_)
open import Data.List using (List; []; _∷_; foldl; upTo)
open import Data.Product using (_×_; _,_)
open import Data.Maybe using (Maybe; just; nothing)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

Graph : Set
Graph = List (ℕ × List ℕ)

-- assoc-list helpers --------------------------------------------------------

lookupℕ : ℕ → List (ℕ × List ℕ) → Maybe (List ℕ)
lookupℕ _ []             = nothing
lookupℕ k ((k' , vs) ∷ rest) =
  if k ≡ᵇ k' then just vs else lookupℕ k rest

-- status map: ℕ ↦ status (1 = on stack, 2 = finished)
lookupS : ℕ → List (ℕ × ℕ) → Maybe ℕ
lookupS _ []             = nothing
lookupS k ((k' , s) ∷ rest) =
  if k ≡ᵇ k' then just s else lookupS k rest

addEdge : ℕ → ℕ → Graph → Graph
addEdge k v []              = (k , v ∷ []) ∷ []
addEdge k v ((k' , vs) ∷ rest) =
  if k ≡ᵇ k' then (k' , v ∷ vs) ∷ rest
             else (k' , vs) ∷ addEdge k v rest

addOne : Graph → List ℕ → Graph
addOne g (a ∷ b ∷ _) = addEdge b a g
addOne g _           = g

buildGraph : List (List ℕ) → Graph
buildGraph = foldl addOne []

succsOf : ℕ → Graph → List ℕ
succsOf node g with lookupℕ node g
... | just vs = vs
... | nothing = []

-- DFS ----------------------------------------------------------------------
-- State threaded as Maybe (status-map × order).  nothing signals a cycle.
-- order is built in post-order then we prepend, so it ends up topological.
-- Recurses through foldl over successors → not structural → TERMINATING.

State : Set
State = Maybe (List (ℕ × ℕ) × List ℕ)

{-# TERMINATING #-}
visit : Graph → State → ℕ → State
visit _ nothing            _    = nothing
visit g (just (st , order)) node with lookupS node st
... | just (suc (suc zero)) = just (st , order)          -- 2 = finished
... | just _                = nothing                    -- on stack ⇒ cycle
... | nothing with foldl (visit g) (just ((node , 1) ∷ st , order)) (succsOf node g)
...   | nothing             = nothing
...   | just (st2 , order2) = just ((node , 2) ∷ st2 , node ∷ order2)

findOrder : ℕ → List (List ℕ) → List ℕ
findOrder n prereqs with foldl (visit (buildGraph prereqs)) (just ([] , [])) (upTo n)
... | just (_ , order) = order
... | nothing          = []

-- compile-time tests: same examples / expected outputs the other languages
-- assert.  With nodes processed in 0..n-1 order, the DFS yields [0,1,2,3].
_ : findOrder 4 ((1 ∷ 0 ∷ []) ∷ (2 ∷ 0 ∷ []) ∷ (3 ∷ 1 ∷ []) ∷ (3 ∷ 2 ∷ []) ∷ [])
    ≡ (0 ∷ 1 ∷ 2 ∷ 3 ∷ [])
_ = refl

_ : findOrder 2 ((1 ∷ 0 ∷ []) ∷ (0 ∷ 1 ∷ []) ∷ [])
    ≡ []
_ = refl
