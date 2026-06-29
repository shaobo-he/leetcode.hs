# leetcode.hs

> Selected LeetCode problems, each solved five ways — in **Racket**, **Haskell**, **Idris 2**, **Lean 4**, and **Agda**.
>
> *Functional programming fan gotta get a job!*

[![CI](https://github.com/shaobo-he/leetcode.hs/actions/workflows/ci.yml/badge.svg)](https://github.com/shaobo-he/leetcode.hs/actions/workflows/ci.yml)

Every problem lives in its own folder with all five implementations:

```
two-sum/
  solution.rkt   — Racket (typed where useful), with rackunit tests
  solution.hs    — Haskell (GHC), with a runnable main
  solution.idr   — Idris 2, type-checked (and runnable)
  solution.lean  — Lean 4, with #guard self-tests (and proofs, where ported)
  solution.agda  — Agda, with compile-time refl tests (and proofs, where ported)
```

## Running & testing

| Language | Run one | Check / test all |
| --- | --- | --- |
| Racket  | `racket two-sum/solution.rkt`               | `raco test $(find . -name '*.rkt')` |
| Haskell | `runghc two-sum/solution.hs`                | loop `runghc` over every `*.hs` |
| Idris 2 | `idris2 two-sum/solution.idr --exec main`   | `idris2 --check` over every `*.idr` |
| Lean 4  | `lean two-sum/solution.lean`                | `lean` over every `*.lean` |
| Agda    | `agda -i two-sum two-sum/solution.agda`     | `agda -i <dir>` over every `solution.agda` |

Racket solutions carry their tests in a `(module+ test …)` submodule (91 `rackunit`
checks in all); Haskell and Idris solutions each have a `main` demonstrating worked
examples; Lean solutions self-test with `#guard`, and Agda solutions with compile-time
`refl` assertions (a wrong computation fails to type-check). CI runs all five toolchains
on every push.

The only dependency outside the standard libraries is Haskell's `logict` (the `Logic`
monad in N-Queens); everything else uses each toolchain's bundled libraries.

## Problems

| Problem | Idea & techniques |
| --- | --- |
| [basic-calculator-iii](basic-calculator-iii) | Recursive-descent `+ - * /` with precedence and parens (HS: Parsec `chainl1`; RKT: `parser-tools` lexer; IDR: hand-rolled over `List Char`) |
| [best-time-to-buy-and-sell-stock](best-time-to-buy-and-sell-stock) | Single pass tracking the lowest price so far |
| [coin-change](coin-change) | Fewest coins summing to an amount. Racket/Haskell ship the bottom-up DP table; Lean proves a well-founded top-down recurrence (`termination_by`/`decreasing_by`) optimal against a declarative `Rep` spec — soundness, lower bound, and none-iff-no-representation — via min-over-list argmin/monotone fold lemmas and a head-decomposition lemma. Idris: total structural DP; Agda: total via `Acc`/`<-wellFounded` with refl tests |
| [course-schedule](course-schedule) | DFS topological sort with cycle detection; Course Schedule II (HS: `StateT (Map …) Maybe` — State threads the visited map, the `Maybe` short-circuits on a cycle) |
| [find-peak-element](find-peak-element) | Binary search for a peak justified by a search *invariant*, not sortedness. Lean: total well-founded search on window size `hi-lo`, `search.induct` proves the result is a genuine peak (`PeakAt` with boundary disjunctions) on adjacent-distinct input; subtype-packaged `findPeak` + free `exists_peak` corollary; axiom-clean. Idris/Agda: total binary search by well-founded recursion (`Accessible`/`<-wellFounded`). Racket/Haskell: runnable search + peak checker |
| [generate-parenthesis](generate-parenthesis) | Backtracking over the balanced-parenthesis grammar |
| [implement-trie-prefix-tree](implement-trie-prefix-tree) | Trie (prefix tree) as a functional assoc-list-children `Trie`; `insert`/`search`/`startsWith` recurse structurally on the key, `build = foldr insert empty`. Lean proves the operations *refine* the list-of-words spec via an abstraction function: `search w (build ws) = decide (w ∈ ws)` and `startsWith p (build ws) = (decide (p=[]) ‖ ws.any (isPrefix p))` (empty-prefix guard mandatory) — double-structural inductions with assoc-list lookup hit/miss lemmas, axiom-clean. Small inductive alphabet `Sym` for decidable equality; Idris/Agda total (mutual `insert`/`childInsert`, no pragmas) |
| [length-of-longest-substring-k-distinct](length-of-longest-substring-k-distinct) | Sliding window with per-character counts |
| [letter-combinations-of-a-phone-number](letter-combinations-of-a-phone-number) | Cartesian product of each digit's letter set |
| [longest-substring-without-repeating-characters](longest-substring-without-repeating-characters) | Sliding window over last-seen indices (HS: `State` monad) |
| [majority-element](majority-element) | Boyer–Moore voting fold; Lean proves the count invariant by snoc-induction → candidate-is-majority, the majority *iff*, and majority-uniqueness (axiom-clean); runnable folds + tests in Racket/Haskell/Idris (total)/Agda (total foldl) |
| [maximum-subarray](maximum-subarray) | Kadane's algorithm as a fold |
| [merge-intervals](merge-intervals) | Sort by start, fold-coalescing overlaps |
| [merge-k-sorted-lists](merge-k-sorted-lists) | Divide-and-conquer pairwise merge |
| [merge-sorted-array](merge-sorted-array) | Merge two sorted sequences (IDR: dependently-typed `Vect (m + n)` with a length proof) |
| [merge-two-sorted-lists](merge-two-sorted-lists) | Natural structural-recursion merge |
| [n-queens](n-queens) | Backtracking (HS: `Logic` monad; RKT/IDR: enumerate permutations, filter diagonal-safe) |
| [next-permutation](next-permutation) | Functional recast of in-place next-permutation: `findPivot` splits `pre ++ p :: suf` at the rightmost ascent, `swapLast` swaps in the rightmost element `> p`, then reverse the suffix. Lean proves the result is a `List.Perm` of the input and strictly greater in `List.Lex (· < ·)` when a pivot exists (betweenness left as noted future work); Idris/Agda total with the split-recovery & `p < g` lemmas |
| [number-of-islands](number-of-islands) | Flood-fill DFS (HS: pure `State (Set visited)` over an immutable `Array`; RKT/IDR: functional sink / visited set) |
| [odd-even-list](odd-even-list) | Regroup by position parity (RKT: in-place pointer rewiring) |
| [permutations](permutations) | Enumerate by picking each element as the head |
| [permutations-ii](permutations-ii) | Multiset count-map enumeration to skip duplicates |
| [product-of-array-except-self](product-of-array-except-self) | Prefix- and suffix-product folds |
| [regular-expression-matching](regular-expression-matching) | Brzozowski derivatives of regular expressions |
| [serialize-deserialize-nary-tree](serialize-deserialize-nary-tree) | Pre-order parenthesised encoding + recursive-descent parse (HS: deserialize as a `State String` parser; Lean/Agda/Idris: round-trip proof) |
| [spiral-matrix](spiral-matrix) | Peel the first row, rotate the rest, recurse |
| [two-sum](two-sum) | Hash / assoc complement lookup |
| [unique-letter-string](unique-letter-string) | Per-character contribution counting |
| [valid-parentheses](valid-parentheses) | Stack matching (RKT: escape continuation; HS: explicit stack + a continuation-monad mirror; IDR: explicit stack) |
| [valid-sudoku](valid-sudoku) | Check every row, column, and 3×3 box for duplicates |

## Highlights

A few solutions lean into what each language does best:

- **Brzozowski derivatives** — `regular-expression-matching` matches by differentiating the regex one character at a time, in all four languages.
- **Dependent types** — `merge-sorted-array.idr` returns a `Vect (m + n)`, carrying a proof that merging an `m`-vector and an `n`-vector yields exactly `m + n` elements. It also proves the *order* is right: `mergeSorted` (a `total` Idris proof) shows merging two sorted lists yields a sorted list for any total transitive comparator — `Sorted` as an inductive pairwise type — instantiated for `≤` on `Nat`. So Idris carries both the length (in the type) and the sortedness (as a theorem); the Lean port (below) adds the permutation.
- **Machine-checked proofs** — `generate-parenthesis.idr` proves (totally, in Idris 2) that its output is *exactly* the balanced strings: soundness (`generateSound : All (Bal 0) …`) and completeness (`generateComplete : Bal 0 xs → … Elem …`). `valid-parentheses.idr` proves its recognizer accepts exactly the `Bal 0` strings. Both model a parenthesis as a two-constructor `Paren` type so Idris's coverage checker can verify the proofs are total (a primitive `Char` index can't be case-split).
- **The same proofs in two provers** — the key Idris theorems are also proven in **Lean 4**, idiomatically (tactic mode: `induction`/`simp`/`omega`/functional induction), not transliterated. `valid-parentheses.lean` proves `check_iff_bal : check d cs = true ↔ Bal d cs` (the recognizer accepts exactly the balanced strings); `generate-parenthesis.lean` proves `genB_sound` + `genB_complete` (the output is exactly the balanced strings); `merge-sorted-array.lean` goes *past* the Idris `Vect (m+n)` length guarantee and proves the merge is fully correct: `sortedMerge_perm` (the output is a permutation of the inputs — exactly the right elements) **and** `sortedMerge_sorted` (merging two sorted lists gives a sorted list, for any total transitive comparator), with length and membership as corollaries. Sortedness is core `List.Pairwise`.
- **A serializer's round-trip, certified in three provers** — `serialize-deserialize-nary-tree` proves `deserialize ∘ serialize ≡ id` for the N-ary tree grammar, in **Lean** (axiom-clean), **Agda**, and **Idris** (`total`). The shipped parsers are `partial`/`TERMINATING` (no equations to reason about), so each proof builds a *total* fuel-driven parser over a token stream and proves the generalizing lemma `parse (serialize t ++ rest) = (t, rest)` — the parser consumes exactly the printer's output and returns the rest untouched — by mutual induction on tree/forest; the payoff (`roundtrip`: parsing a serialization recovers the tree) follows. The decimal lexing (`Char ↔ ℕ`) is abstracted as an atomic value token, so the proof targets the paren-matching / child-ordering structure where serializer bugs live. Lean leans on `omega`/`simp`; Idris and Agda crank the `LTE`/`≤` fuel arithmetic by hand.
- **Lean finishes what Idris couldn't** — `length-of-longest-substring-k-distinct.lean` proves *full* optimality of the sliding window — `lenKDistinctV_optimal`: its answer is **exactly** the longest substring with ≤ k distinct chars (the realizability *and* the upper bound). The Idris file only got soundness + the `shrinkVLongest` core and left the lower bound `solve ≤ window` as future work — the blocker was distinct-count monotonicity under append (needs lawful decidable equality). In Lean that's just `nd_snoc`/`nd_mono`, and core `List.IsInfix`/`IsSuffix` replace ~100 lines of Idris enumeration lemmas. `#print axioms` confirms no `sorry` (only the 3 standard axioms).
- **A verified optimisation** — `length-of-longest-substring-k-distinct.idr` carries a brute-force reference with a *full* correctness proof: `optimal` (no substring with ≤ k distinct chars is longer than the answer) and `achievable` (the answer is realised by an actual valid substring), via an inductive "contiguous substring" relation and enumeration sound/completeness lemmas. It also reasons about the **sliding window itself**: `windowRealized` shows the answer is always the length of a genuine contiguous substring (the window stays positioned in the string as it advances), and `windowSound : lenKDistinctV s k ≤ solve k (unpack s)` proves a sliding window never overestimates the true maximum — every window is a *valid* substring by construction (shrink keeps dropping from the left until ≤ k distinct chars). The matching lower bound's core, `shrinkVLongest` (shrink returns the *longest* valid suffix, so no better window is missed), is proven too; threading it to full optimality (`= solve`) is left as future work *in Idris* — but the Lean port (above) completes it. The counts-based window is the fast runnable solution.
- **Certifying the *fast* algorithm, in three provers** — `longest-substring-without-repeating-characters` ships the genuine O(n) last-seen-index sliding window (on each char, jump the window start past that char's previous index) and proves *that* algorithm optimal — not a brute-force stand-in. Reasoning about the index/`seen` bookkeeping head-on is painful, so each proof takes a **detour with equivalence**: prove a simpler structural shrink-window `lengthOfLongestV` is *exactly* the longest all-distinct substring (`lengthOfLongestV_optimal`, reusing the k-distinct optimality machinery with validity = all-distinct), then prove the shipped algorithm computes the same number (`lengthOfLongest_eq` — a `goFast`↔`goV` simulation with a `seen`-correctness invariant and the crux `shrinkV (win ++ [c]) = drop (dropAmt win c) (win ++ [c])` linking `lastIdx` to the window), so optimality transfers to the real code (`lengthOfLongest_optimal`). Done in **Lean** (axiom-clean), **Agda**, and **Idris** (`total`; with no `omega`, the whole simulation — `lastIdx`/`seen`/`shrinkV`-snoc-drop crux and its arithmetic — is by hand).
- **The store: where library maturity shows** — that whole optimality proof touches the last-seen-index store through exactly *one* lemma, the functional-update law (`lookupIdx d (setIdx c i m) = if d == c then just i else lookupIdx d m`). In **Lean** that modularity lets us swap the assoc list for a real `Std.HashMap` (genuine O(n)) almost for free: the ~500-line proof is reused verbatim and only the one lemma is re-proven — straight from the library's own `getElem?_insert` — staying axiom-clean. **Idris and Agda keep the assoc list** (O(n·σ)): neither `Data.SortedMap` (Idris base) nor `Data.Tree.AVL.Map` (Agda stdlib) *proves* the functional-update law for its real tree, and this repo won't `postulate` it. A concrete illustration that the O(n) data structure is gated by the *proof* ecosystem (a library that ships the law as a lemma), not by the language.
- **The same window in State-monad style** — all three provers additionally carry `lengthOfLongestS`, the shipped algorithm rewritten with `get`/`put` to mirror the Haskell `State (Int, Map Char Int)` version, with a proof that it computes *exactly* `lengthOfLongest` (`lengthOfLongestS_eq` / `-eq`). So the monadic plumbing inherits optimality and is shown to buy no power, only syntax. Lean and Idris use their library State monads; Agda uses a minimal `S → A × S` (Haskell's `newtype State s a` unwrapped) so the equivalence stays a clean structural induction.
- **A fold invariant** — the interval merge is proved correct in both **Lean** (`merge-intervals.lean`) and **Idris** (`merge-intervals.idr`): `…NoOverlap` (every pair in the result is separated, `a.snd < b.fst`, as an inductive Pairwise over *all* pairs) and `…WF` (all outputs well-formed `lo ≤ hi`), via a separation invariant threaded through the coalescing `step`/fold — which holds for *any* well-formed input, so it needs no sortedness (the sort only fixes the output shape). Lean is axiom-clean and additionally verifies its insertion sort; the Idris port is `total` throughout, on `Nat` bounds so the comparisons reduce (no `omega` to lean on, so the `≤`/`<`/`max` lemmas are proved by hand).
- **Greedy optimality** — `best-time-to-buy-and-sell-stock` (`.lean` and `.idr`) proves the single-pass max-profit is *exactly* the best trade: `maxProfit_optimal` (no buy-low-then-sell-high pair beats it) **and** `maxProfit_achievable` (the answer is realised by an actual trade), where `Trade prices b s` is the inductive "buy `b`, sell `s` no earlier". Lean uses `Int` + `omega` and is axiom-clean; the Idris port is `total` on `Nat` with monus profit, the `≤`/`min`/`max`/monus lemmas proved by hand.
- **The Logic monad** — `n-queens.hs` does backtracking with `Control.Monad.Logic`; the Racket and Idris ports get the same answers by filtering permutations.
- **Escape continuations / the continuation monad** — `valid-parentheses` bails out the instant it sees a mismatch: `solution.rkt` with `let/cc`, and `solution.hs` mirrors that exact control flow in the **continuation monad** — `callCC \k -> … k False` *is* Racket's `(let/cc k … (k #f))` — sitting next to the plain explicit-stack version for contrast.
- **State monad, beyond the sliding windows** — DFS that threads a visited map and short-circuits on a cycle is a textbook State fit: `course-schedule.hs` is `StateT (Map Int Int) Maybe` (State = the per-node status map, the `Maybe` = the cycle short-circuit via `lift Nothing`), replacing a hand-rolled `foldl` that threaded `(status, order)` through an explicit `Maybe` by hand. `number-of-islands.hs` makes the same move from the other direction — its flood-fill drops the mutable `IOArray` for an immutable `Array` (the read-only grid) plus a `State (Set (Int,Int))` of visited cells, so the "sink the cell" mutation becomes pure state. (`longest-substring-without-repeating-characters` likewise carries a `State`-monad mirror of its window.)
