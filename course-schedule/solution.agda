module solution where

-- LeetCode 210: Course Schedule II
-- Topological sort via DFS.  graph as assoc list: prereq -> dependents.
-- Each prerequisite [a, b] means course b must be taken before course a.
-- Returns a valid order, or [] when a cycle is detected.
--
-- TOTAL, no TERMINATING pragma.  The DFS explores arbitrary graph successors,
-- not structure, so termination is a finite-measure argument (port of the Lean
-- blueprint):
--   * `graphNodes g`/`univ` is the finite universe of nodes ever visited.
--   * `unvisited univ st` = number of universe nodes not yet in the status map.
--     Visiting a fresh node adds it to `st`, strictly dropping the count.
--   * the recursion runs through a successor loop, so the measure is the
--     lexicographic pair `(unvisited univ st , rank)`: `visit` (rank 0) sits
--     below `visitChildren` (rank = suc (length succs)).
--   * monotonicity of the status map (`Mono`, it only grows) is carried in the
--     return type, so the loop's continuation provably never raises `unvisited`.

open import Data.Nat using (ℕ; zero; suc; _+_; _<_; _≤_; _≡ᵇ_; s≤s; z≤n)
open import Data.Nat.Properties using (≤-refl; +-mono-≤; +-mono-≤-<; m≤n⇒m<n∨m≡n)
open import Induction.WellFounded using (Acc; acc; WellFounded)
open import Data.Product.Relation.Binary.Lex.Strict using (×-Lex; ×-wellFounded)
open import Data.Nat.Induction using (<-wellFounded)
open import Data.Bool using (Bool; true; false; if_then_else_)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.List using (List; []; _∷_; foldl; upTo; concatMap; length; _++_)
open import Data.List.Membership.Propositional using (_∈_; mapWith∈)
open import Data.List.Membership.Propositional.Properties using (∈-++⁺ˡ; ∈-++⁺ʳ)
open import Data.List.Relation.Unary.Any using (here; there)
open import Data.Product using (_×_; _,_; proj₁; proj₂; Σ; Σ-syntax)
open import Data.Maybe using (Maybe; just; nothing; fromMaybe)
open import Data.Empty using (⊥; ⊥-elim)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong; subst)

Graph : Set
Graph = List (ℕ × List ℕ)

-- assoc-list helpers --------------------------------------------------------

lookupℕ : ℕ → Graph → Maybe (List ℕ)
lookupℕ _ []                 = nothing
lookupℕ k ((k' , vs) ∷ rest) = if k ≡ᵇ k' then just vs else lookupℕ k rest

lookupS : ℕ → List (ℕ × ℕ) → Maybe ℕ
lookupS _ []                = nothing
lookupS k ((k' , s) ∷ rest) = if k ≡ᵇ k' then just s else lookupS k rest

addEdge : ℕ → ℕ → Graph → Graph
addEdge k v []                 = (k , v ∷ []) ∷ []
addEdge k v ((k' , vs) ∷ rest) =
  if k ≡ᵇ k' then (k' , v ∷ vs) ∷ rest else (k' , vs) ∷ addEdge k v rest

addOne : Graph → List ℕ → Graph
addOne g (a ∷ b ∷ _) = addEdge b a g
addOne g _           = g

buildGraph : List (List ℕ) → Graph
buildGraph = foldl addOne []

succsOf : ℕ → Graph → List ℕ
succsOf node g = fromMaybe [] (lookupℕ node g)

-- ─── small reflections ───

≡ᵇ-refl : (n : ℕ) → (n ≡ᵇ n) ≡ true
≡ᵇ-refl zero    = refl
≡ᵇ-refl (suc n) = ≡ᵇ-refl n

true≢false : true ≡ false → ⊥
true≢false ()

∉[] : {x : ℕ} → x ∈ [] → ⊥
∉[] ()

-- ─── finite universe of graph nodes (every key and every value) ───

graphNodes : Graph → List ℕ
graphNodes = concatMap (λ p → proj₁ p ∷ proj₂ p)

∈-concatMap⁺ : ∀ {A B : Set} (f : A → List B) {x : A} {y : B} {xs : List A}
             → x ∈ xs → y ∈ f x → y ∈ concatMap f xs
∈-concatMap⁺ f (here refl)  y∈fx = ∈-++⁺ˡ y∈fx
∈-concatMap⁺ f (there x∈xs) y∈fx = ∈-++⁺ʳ _ (∈-concatMap⁺ f x∈xs y∈fx)

-- a successor of a node lies in the universe (the `with` reduces `succsOf` in
-- each branch: matching key ⇒ hs : s ∈ vs; otherwise hs : s ∈ succsOf node g)
succ-mem-graphNodes : (g : Graph) (node s : ℕ) → s ∈ succsOf node g → s ∈ graphNodes g
succ-mem-graphNodes []             node s ()
succ-mem-graphNodes ((k , vs) ∷ g) node s hs with node ≡ᵇ k
... | true  = ∈-++⁺ˡ (there hs)
... | false = ∈-++⁺ʳ (k ∷ vs) (succ-mem-graphNodes g node s hs)

-- ─── status map: "covered" = has any entry; only grows (`Mono`) ───

covered : List (ℕ × ℕ) → ℕ → Bool
covered st nd with lookupS nd st
... | just _  = true
... | nothing = false

covered-head : (k s : ℕ) (st : List (ℕ × ℕ)) → covered ((k , s) ∷ st) k ≡ true
covered-head k s st rewrite ≡ᵇ-refl k = refl

covered-cons : (k s : ℕ) (st : List (ℕ × ℕ)) (nd : ℕ)
             → covered st nd ≡ true → covered ((k , s) ∷ st) nd ≡ true
covered-cons k s st nd h with nd ≡ᵇ k
... | true  = refl
... | false = h

not-covered : (st : List (ℕ × ℕ)) (node : ℕ) → lookupS node st ≡ nothing → covered st node ≡ false
not-covered st node h rewrite h = refl

Mono : List (ℕ × ℕ) → List (ℕ × ℕ) → Set
Mono st st' = (nd : ℕ) → covered st nd ≡ true → covered st' nd ≡ true

-- ─── measure: universe nodes not yet covered ───

unvisited : List ℕ → List (ℕ × ℕ) → ℕ
unvisited []        st = 0
unvisited (nd ∷ ns) st = (if covered st nd then 0 else 1) + unvisited ns st

-- more covered ⇒ no more unvisited
unvisited-le : (univ : List ℕ) (st st' : List (ℕ × ℕ)) → Mono st st'
             → unvisited univ st' ≤ unvisited univ st
unvisited-le []        st st' mono = z≤n
unvisited-le (nd ∷ ns) st st' mono with covered st nd in eqc | covered st' nd in eqc'
... | true  | true  = +-mono-≤ ≤-refl (unvisited-le ns st st' mono)
... | true  | false = ⊥-elim (true≢false (trans (sym (mono nd eqc)) eqc'))
... | false | true  = +-mono-≤ z≤n    (unvisited-le ns st st' mono)
... | false | false = +-mono-≤ ≤-refl (unvisited-le ns st st' mono)

-- covering a fresh universe node strictly drops the count
unvisited-lt : (univ : List ℕ) (st st' : List (ℕ × ℕ)) (node : ℕ)
             → node ∈ univ → covered st node ≡ false → covered st' node ≡ true → Mono st st'
             → unvisited univ st' < unvisited univ st
unvisited-lt (_ ∷ ns) st st' node (here refl) hnc hcov mono rewrite hnc | hcov =
  s≤s (unvisited-le ns st st' mono)
unvisited-lt (u ∷ ns) st st' node (there mem) hnc hcov mono
  with covered st u in eqc | covered st' u in eqc'
... | true  | true  = +-mono-≤-< ≤-refl (unvisited-lt ns st st' node mem hnc hcov mono)
... | true  | false = ⊥-elim (true≢false (trans (sym (mono u eqc)) eqc'))
... | false | true  = +-mono-≤-< z≤n    (unvisited-lt ns st st' node mem hnc hcov mono)
... | false | false = +-mono-≤-< ≤-refl (unvisited-lt ns st st' node mem hnc hcov mono)

-- ─── lexicographic measure (unvisited , rank) ───

_≺_ : (ℕ × ℕ) → (ℕ × ℕ) → Set
_≺_ = ×-Lex _≡_ _<_ _<_

≺-wf : WellFounded _≺_
≺-wf = ×-wellFounded <-wellFounded <-wellFounded

-- the visitChildren-tail decrease: same-or-smaller unvisited, one fewer successor
≺-rest : (univ : List ℕ) (st st' : List (ℕ × ℕ)) (lr : ℕ) → Mono st st'
       → (unvisited univ st' , suc lr) ≺ (unvisited univ st , suc (suc lr))
≺-rest univ st st' lr mono with m≤n⇒m<n∨m≡n (unvisited-le univ st st' mono)
... | inj₁ hlt = inj₁ hlt
... | inj₂ heq = inj₂ (heq , ≤-refl)

-- ─── verified DFS ───

State : Set
State = Maybe (List (ℕ × ℕ) × List ℕ)

-- carried invariant: any successful result has a status map ⊇ the input
MonoR : List (ℕ × ℕ) → State → Set
MonoR st r = (st' : List (ℕ × ℕ)) (o' : List ℕ) → r ≡ just (st' , o') → Mono st st'

Res : List (ℕ × ℕ) → Set
Res st = Σ[ r ∈ State ] MonoR st r

mutual
  visit : (univ : List ℕ) (g : Graph) (hsub : (x : ℕ) → x ∈ graphNodes g → x ∈ univ)
          (st : List (ℕ × ℕ)) (order : List ℕ) (node : ℕ) (hnode : node ∈ univ)
        → Acc _≺_ (unvisited univ st , 0) → Res st
  visit univ g hsub st order node hnode (acc rec) with lookupS node st in eqL
  ... | just (suc (suc zero)) = just (st , order) , λ { _ _ refl nd h → h }     -- 2 = finished
  ... | just _                = nothing , λ _ _ ()                              -- on stack ⇒ cycle
  ... | nothing with visitChildren univ g hsub ((node , 1) ∷ st) order (succsOf node g)
                     (λ x hx → hsub x (succ-mem-graphNodes g node x hx))
                     (rec (inj₁ (unvisited-lt univ st ((node , 1) ∷ st) node hnode
                            (not-covered st node eqL) (covered-head node 1 st)
                            (λ nd h → covered-cons node 1 st nd h))))
  ...   | cr with proj₁ cr in eqcr
  ...     | nothing             = nothing , λ _ _ ()
  ...     | just (st2 , order2) =
              just ((node , 2) ∷ st2 , node ∷ order2) ,
              λ { _ _ refl nd h →
                    covered-cons node 2 st2 nd
                      (proj₂ cr st2 order2 eqcr nd (covered-cons node 1 st nd h)) }

  visitChildren : (univ : List ℕ) (g : Graph) (hsub : (x : ℕ) → x ∈ graphNodes g → x ∈ univ)
                  (st : List (ℕ × ℕ)) (order : List ℕ) (succs : List ℕ)
                  (hsuccs : (s : ℕ) → s ∈ succs → s ∈ univ)
                → Acc _≺_ (unvisited univ st , suc (length succs)) → Res st
  visitChildren univ g hsub st order [] hsuccs _ = just (st , order) , λ { _ _ refl nd h → h }
  visitChildren univ g hsub st order (s ∷ rest) hsuccs (acc rec)
    with visit univ g hsub st order s (hsuccs s (here refl)) (rec (inj₂ (refl , s≤s z≤n)))
  ... | vr with proj₁ vr in eqv
  ...   | nothing             = nothing , λ _ _ ()
  ...   | just (st' , order') =
            let mono = proj₂ vr st' order' eqv
                rr   = visitChildren univ g hsub st' order' rest (λ x hx → hsuccs x (there hx))
                         (rec (≺-rest univ st st' (length rest) mono))
            in proj₁ rr , λ st'' o'' eq nd h → proj₂ rr st'' o'' eq nd (mono nd h)

-- ─── driver ───

attach : ∀ {A : Set} (xs : List A) → List (Σ[ a ∈ A ] (a ∈ xs))
attach xs = mapWith∈ xs (λ {x} m → x , m)

foldStep : (g : Graph) (nodes : List ℕ) → State → Σ[ x ∈ ℕ ] (x ∈ nodes) → State
foldStep g nodes nothing             _        = nothing
foldStep g nodes (just (st , order)) (x , hx) =
  proj₁ (visit (nodes ++ graphNodes g) g (λ y hy → ∈-++⁺ʳ nodes hy) st order x
           (∈-++⁺ˡ hx) (≺-wf (unvisited (nodes ++ graphNodes g) st , 0)))

findOrder : ℕ → List (List ℕ) → List ℕ
findOrder n prereqs = orderOf (foldl (foldStep (buildGraph prereqs) (upTo n)) (just ([] , [])) (attach (upTo n)))
  where
    orderOf : State → List ℕ
    orderOf (just (_ , order)) = order
    orderOf nothing            = []

-- compile-time tests: same examples / expected outputs the other languages
-- assert.  With nodes processed in 0..n-1 order, the DFS yields [0,1,2,3].
_ : findOrder 4 ((1 ∷ 0 ∷ []) ∷ (2 ∷ 0 ∷ []) ∷ (3 ∷ 1 ∷ []) ∷ (3 ∷ 2 ∷ []) ∷ [])
    ≡ (0 ∷ 1 ∷ 2 ∷ 3 ∷ [])
_ = refl

_ : findOrder 2 ((1 ∷ 0 ∷ []) ∷ (0 ∷ 1 ∷ []) ∷ [])
    ≡ []
_ = refl
