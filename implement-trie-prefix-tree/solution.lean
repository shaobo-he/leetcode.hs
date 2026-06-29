/-
LeetCode 208: Implement Trie (Prefix Tree) — with a Lean 4 proof that the trie
operations REFINE the list-of-words specification, via an abstraction function.

We model a trie functionally, with assoc-list children (no no-duplicate-keys
invariant assumed):

    Trie := node (isEnd : Bool) (children : List (Sym × Trie))

`insert` adds a word, `build = foldr insert empty` builds a trie from a word list,
`search` tests membership and `startsWith` tests prefix-existence.  The two
refinement theorems say the data structure computes exactly the list spec:

  • `search_build`     : search w (build ws) = decide (w ∈ ws)
  • `startsWith_build` : startsWith p (build ws)
                          = (decide (p = []) || ws.any (fun w => isPrefix p w))

The `decide (p = []) ||` guard is MANDATORY: the bare "∃ w ∈ ws, p prefix of w"
is FALSE at exactly (ws = [], p = []), where the empty prefix matches nothing yet
`startsWith [] t = true`.  The proofs are two double-structural inductions
(`search_insert`, `startsWith_insert`) over the assoc-list-children trie, resting
on assoc-list lookup hit/miss lemmas.  Words use a SMALL inductive alphabet `Sym`
so equality is decidable.

Lean 4, core library only (no Mathlib / Std / Batteries).
-/

set_option linter.constructorNameAsVariable false

-- A small inductive alphabet (enough letters for the worked examples), with
-- decidable equality — the repo trick for total decidable symbol comparison.
inductive Sym where
  | a | b | c | d | e | l | p
deriving DecidableEq

abbrev Word := List Sym

-- The trie: a terminal flag plus assoc-list children.  Nested inductive through
-- `List`, so functions recurse structurally on the word or on the child list.
inductive Trie where
  | node : Bool → List (Sym × Trie) → Trie

def empty : Trie := .node false []

/- ── assoc-list lookup over the children ─────────────────────────────────── -/

def lookup (x : Sym) : List (Sym × Trie) → Option Trie
  | []          => none
  | (y, t) :: r => if x = y then some t else lookup x r

-- the child reached by `x`, defaulting to the empty trie when absent
def lookupD (x : Sym) (cs : List (Sym × Trie)) : Trie := (lookup x cs).getD empty

/- ── insert (mutual with the child-list update) ──────────────────────────── -/

-- `childInsert b vs cs` updates the child keyed `b`: descend `insert vs` into the
-- existing child, or create a fresh one from `empty`.  First matching key only.
-- Lexicographic termination: word length dominates; the `+ 1` lets the child-list
-- `nil` case still call `insert vs` (which sits at second-component 0).
mutual
  def insert : Word → Trie → Trie
    | [],       .node _ cs => .node true cs
    | b :: vs,  .node e cs => .node e (childInsert b vs cs)
  termination_by w => (w.length, 0)

  def childInsert (b : Sym) (vs : Word) : List (Sym × Trie) → List (Sym × Trie)
    | []          => [(b, insert vs empty)]
    | (y, t) :: r => if b = y then (y, insert vs t) :: r
                              else (y, t) :: childInsert b vs r
  termination_by cs => (vs.length, cs.length + 1)
end

def build (ws : List Word) : Trie := ws.foldr insert empty

/- ── search & startsWith (structural on the word) ────────────────────────── -/

def search : Word → Trie → Bool
  | [],      .node e _  => e
  | a :: ws, .node _ cs =>
      match lookup a cs with
      | some t => search ws t
      | none   => false

def startsWith : Word → Trie → Bool
  | [],      _          => true
  | a :: ps, .node _ cs =>
      match lookup a cs with
      | some t => startsWith ps t
      | none   => false

-- our own prefix test on words (avoids relying on Batteries' `List.isPrefixOf`)
def isPrefix : Word → Word → Bool
  | [],      _       => true
  | _ :: _,  []      => false
  | a :: as, b :: bs => if a = b then isPrefix as bs else false

/- ── runnable self-tests: build ["apple"] ────────────────────────────────── -/

open Sym in def apple : Word := [a, p, p, l, e]
open Sym in def app   : Word := [a, p, p]
open Sym in def bb    : Word := [b]

#guard search apple (build [apple]) == true
#guard search app   (build [apple]) == false
#guard startsWith app (build [apple]) == true
#guard startsWith bb  (build [apple]) == false

/- ── base facts: the empty trie ──────────────────────────────────────────── -/

theorem search_empty : ∀ w : Word, search w empty = false
  | []      => rfl
  | _ :: _  => rfl

theorem startsWith_empty : ∀ p : Word, startsWith p empty = decide (p = [])
  | []      => rfl
  | _ :: _  => rfl

/- ── assoc-list lookup hit / miss under childInsert ──────────────────────── -/

-- HIT: looking up the just-inserted key returns `insert vs` of the old child
-- (or of `empty` when the key was absent).
theorem lookup_childInsert_self (b : Sym) (vs : Word) (cs : List (Sym × Trie)) :
    lookup b (childInsert b vs cs) = some (insert vs (lookupD b cs)) := by
  induction cs with
  | nil => simp [childInsert, lookup, lookupD]
  | cons hd r ih =>
      obtain ⟨y, t⟩ := hd
      simp only [childInsert]
      by_cases h : b = y
      · subst h; simp [lookup, lookupD]
      · simp only [if_neg h, lookup, ih, lookupD]

-- MISS: looking up a different key is unaffected by a `childInsert` on `b`.
theorem lookup_childInsert_other (a b : Sym) (vs : Word) (cs : List (Sym × Trie))
    (hab : a ≠ b) : lookup a (childInsert b vs cs) = lookup a cs := by
  induction cs with
  | nil => simp [childInsert, lookup, hab]
  | cons hd r ih =>
      obtain ⟨y, t⟩ := hd
      simp only [childInsert]
      by_cases h : b = y
      · subst h; simp [lookup, hab]
      · simp only [if_neg h, lookup, ih]

/- ── refinement of `search` over `insert` ────────────────────────────────── -/

-- The core lemma: inserting `v` makes `w` an end-word iff it already was, or w = v.
theorem search_insert (w v : Word) (t : Trie) :
    search w (insert v t) = (decide (w = v) || search w t) := by
  induction w generalizing v t with
  | nil =>
      obtain ⟨e, cs⟩ := t
      cases v with
      | nil      => simp [_root_.insert, search]
      | cons b vs => simp [_root_.insert, search]
  | cons a ws ih =>
      obtain ⟨e, cs⟩ := t
      cases v with
      | nil => simp [_root_.insert, search]
      | cons b vs =>
          by_cases hab : a = b
          · subst hab
            simp only [_root_.insert, search, lookup_childInsert_self]
            rw [ih vs (lookupD a cs)]
            cases hl : lookup a cs with
            | some t0 => simp [lookupD, hl, List.cons.injEq]
            | none    => simp [lookupD, hl, search_empty, List.cons.injEq]
          · simp only [_root_.insert, search]
            rw [lookup_childInsert_other a b vs cs hab]
            have hne : decide (a :: ws = b :: vs) = false := by simp [List.cons.injEq, hab]
            rw [hne, Bool.false_or]

theorem search_build (w : Word) (ws : List Word) :
    search w (build ws) = decide (w ∈ ws) := by
  induction ws with
  | nil => simp [build, search_empty]
  | cons v vs ih =>
      have hb : build (v :: vs) = insert v (build vs) := rfl
      rw [hb, search_insert, ih]
      by_cases h1 : w = v <;> by_cases h2 : w ∈ vs <;> simp [List.mem_cons, h1, h2]

/- ── refinement of `startsWith` over `insert` ────────────────────────────── -/

-- absorbing fact: `decide (p = []) || isPrefix p vs = isPrefix p vs`
theorem nil_or_isPrefix (p vs : Word) :
    (decide (p = []) || isPrefix p vs) = isPrefix p vs := by
  cases p with
  | nil      => simp [isPrefix]
  | cons a ps => simp

-- one prefix step: `(a::ps) prefix (a::vs)` iff `ps prefix vs`
theorem isPrefix_cons_self (a : Sym) (ps vs : Word) :
    isPrefix (a :: ps) (a :: vs) = isPrefix ps vs := by
  simp [isPrefix]

theorem startsWith_insert (p v : Word) (t : Trie) :
    startsWith p (insert v t) = (startsWith p t || isPrefix p v) := by
  induction p generalizing v t with
  | nil => simp [startsWith]
  | cons a ps ih =>
      obtain ⟨e, cs⟩ := t
      cases v with
      | nil => simp [_root_.insert, startsWith, isPrefix]
      | cons b vs =>
          by_cases hab : a = b
          · subst hab
            simp only [_root_.insert, startsWith, lookup_childInsert_self]
            rw [ih vs (lookupD a cs), isPrefix_cons_self]
            cases hl : lookup a cs with
            | some t0 => simp [lookupD, hl]
            | none    => simp [lookupD, hl, startsWith_empty, nil_or_isPrefix]
          · simp only [_root_.insert, startsWith]
            rw [lookup_childInsert_other a b vs cs hab]
            have hpf : isPrefix (a :: ps) (b :: vs) = false := by simp [isPrefix, hab]
            rw [hpf, Bool.or_false]

theorem startsWith_build (p : Word) (ws : List Word) :
    startsWith p (build ws)
      = (decide (p = []) || ws.any (fun w => isPrefix p w)) := by
  induction ws with
  | nil => simp [build, startsWith_empty]
  | cons v vs ih =>
      have hb : build (v :: vs) = insert v (build vs) := rfl
      rw [hb, startsWith_insert, ih]
      simp only [List.any_cons]
      simp [Bool.or_assoc, Bool.or_comm]

#print axioms search_build
#print axioms startsWith_build
