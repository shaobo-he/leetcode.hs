# leetcode.hs

> Selected LeetCode problems, each solved four ways — in **Racket**, **Haskell**, **Idris 2**, and **Lean 4**.
>
> *Functional programming fan gotta get a job!*

[![CI](https://github.com/shaobo-he/leetcode.hs/actions/workflows/ci.yml/badge.svg)](https://github.com/shaobo-he/leetcode.hs/actions/workflows/ci.yml)

Every problem lives in its own folder with all four implementations:

```
two-sum/
  solution.rkt   — Racket (typed where useful), with rackunit tests
  solution.hs    — Haskell (GHC), with a runnable main
  solution.idr   — Idris 2, type-checked (and runnable)
  solution.lean  — Lean 4, with #guard self-tests (and proofs, where ported)
```

## Running & testing

| Language | Run one | Check / test all |
| --- | --- | --- |
| Racket  | `racket two-sum/solution.rkt`               | `raco test $(find . -name '*.rkt')` |
| Haskell | `runghc two-sum/solution.hs`                | loop `runghc` over every `*.hs` |
| Idris 2 | `idris2 two-sum/solution.idr --exec main`   | `idris2 --check` over every `*.idr` |
| Lean 4  | `lean two-sum/solution.lean`                | `lean` over every `*.lean` |

Racket solutions carry their tests in a `(module+ test …)` submodule (91 `rackunit`
checks in all); Haskell and Idris solutions each have a `main` demonstrating worked
examples; Lean solutions self-test with `#guard` (a failing guard is a compile error).
CI runs all four toolchains on every push.

The only dependency outside the standard libraries is Haskell's `logict` (the `Logic`
monad in N-Queens); everything else uses each toolchain's bundled libraries.

## Problems

| Problem | Idea & techniques |
| --- | --- |
| [basic-calculator-iii](basic-calculator-iii) | Recursive-descent `+ - * /` with precedence and parens (HS: Parsec `chainl1`; RKT: `parser-tools` lexer; IDR: hand-rolled over `List Char`) |
| [best-time-to-buy-and-sell-stock](best-time-to-buy-and-sell-stock) | Single pass tracking the lowest price so far |
| [course-schedule](course-schedule) | DFS topological sort with cycle detection (Course Schedule II) |
| [generate-parenthesis](generate-parenthesis) | Backtracking over the balanced-parenthesis grammar |
| [length-of-longest-substring-k-distinct](length-of-longest-substring-k-distinct) | Sliding window with per-character counts |
| [letter-combinations-of-a-phone-number](letter-combinations-of-a-phone-number) | Cartesian product of each digit's letter set |
| [longest-substring-without-repeating-characters](longest-substring-without-repeating-characters) | Sliding window over last-seen indices (HS: `State` monad) |
| [maximum-subarray](maximum-subarray) | Kadane's algorithm as a fold |
| [merge-intervals](merge-intervals) | Sort by start, fold-coalescing overlaps |
| [merge-k-sorted-lists](merge-k-sorted-lists) | Divide-and-conquer pairwise merge |
| [merge-sorted-array](merge-sorted-array) | Merge two sorted sequences (IDR: dependently-typed `Vect (m + n)` with a length proof) |
| [merge-two-sorted-lists](merge-two-sorted-lists) | Natural structural-recursion merge |
| [n-queens](n-queens) | Backtracking (HS: `Logic` monad; RKT/IDR: enumerate permutations, filter diagonal-safe) |
| [number-of-islands](number-of-islands) | Flood-fill DFS (HS: mutable `IOArray`; RKT/IDR: functional sink / visited set) |
| [odd-even-list](odd-even-list) | Regroup by position parity (RKT: in-place pointer rewiring) |
| [permutations](permutations) | Enumerate by picking each element as the head |
| [permutations-ii](permutations-ii) | Multiset count-map enumeration to skip duplicates |
| [product-of-array-except-self](product-of-array-except-self) | Prefix- and suffix-product folds |
| [regular-expression-matching](regular-expression-matching) | Brzozowski derivatives of regular expressions |
| [serialize-deserialize-nary-tree](serialize-deserialize-nary-tree) | Pre-order parenthesised encoding + recursive-descent parse |
| [spiral-matrix](spiral-matrix) | Peel the first row, rotate the rest, recurse |
| [two-sum](two-sum) | Hash / assoc complement lookup |
| [unique-letter-string](unique-letter-string) | Per-character contribution counting |
| [valid-parentheses](valid-parentheses) | Stack matching (RKT: escape continuation; HS/IDR: explicit stack) |
| [valid-sudoku](valid-sudoku) | Check every row, column, and 3×3 box for duplicates |

## Highlights

A few solutions lean into what each language does best:

- **Brzozowski derivatives** — `regular-expression-matching` matches by differentiating the regex one character at a time, in all four languages.
- **Dependent types** — `merge-sorted-array.idr` returns a `Vect (m + n)`, carrying a proof that merging an `m`-vector and an `n`-vector yields exactly `m + n` elements. It also proves the *order* is right: `mergeSorted` (a `total` Idris proof) shows merging two sorted lists yields a sorted list for any total transitive comparator — `Sorted` as an inductive pairwise type — instantiated for `≤` on `Nat`. So Idris carries both the length (in the type) and the sortedness (as a theorem); the Lean port (below) adds the permutation.
- **Machine-checked proofs** — `generate-parenthesis.idr` proves (totally, in Idris 2) that its output is *exactly* the balanced strings: soundness (`generateSound : All (Bal 0) …`) and completeness (`generateComplete : Bal 0 xs → … Elem …`). `valid-parentheses.idr` proves its recognizer accepts exactly the `Bal 0` strings. Both model a parenthesis as a two-constructor `Paren` type so Idris's coverage checker can verify the proofs are total (a primitive `Char` index can't be case-split).
- **The same proofs in two provers** — the key Idris theorems are also proven in **Lean 4**, idiomatically (tactic mode: `induction`/`simp`/`omega`/functional induction), not transliterated. `valid-parentheses.lean` proves `check_iff_bal : check d cs = true ↔ Bal d cs` (the recognizer accepts exactly the balanced strings); `generate-parenthesis.lean` proves `genB_sound` + `genB_complete` (the output is exactly the balanced strings); `merge-sorted-array.lean` goes *past* the Idris `Vect (m+n)` length guarantee and proves the merge is fully correct: `sortedMerge_perm` (the output is a permutation of the inputs — exactly the right elements) **and** `sortedMerge_sorted` (merging two sorted lists gives a sorted list, for any total transitive comparator), with length and membership as corollaries. Sortedness is core `List.Pairwise`.
- **Lean finishes what Idris couldn't** — `length-of-longest-substring-k-distinct.lean` proves *full* optimality of the sliding window — `lenKDistinctV_optimal`: its answer is **exactly** the longest substring with ≤ k distinct chars (the realizability *and* the upper bound). The Idris file only got soundness + the `shrinkVLongest` core and left the lower bound `solve ≤ window` as future work — the blocker was distinct-count monotonicity under append (needs lawful decidable equality). In Lean that's just `nd_snoc`/`nd_mono`, and core `List.IsInfix`/`IsSuffix` replace ~100 lines of Idris enumeration lemmas. `#print axioms` confirms no `sorry` (only the 3 standard axioms).
- **A verified optimisation** — `length-of-longest-substring-k-distinct.idr` carries a brute-force reference with a *full* correctness proof: `optimal` (no substring with ≤ k distinct chars is longer than the answer) and `achievable` (the answer is realised by an actual valid substring), via an inductive "contiguous substring" relation and enumeration sound/completeness lemmas. It also reasons about the **sliding window itself**: `windowRealized` shows the answer is always the length of a genuine contiguous substring (the window stays positioned in the string as it advances), and `windowSound : lenKDistinctV s k ≤ solve k (unpack s)` proves a sliding window never overestimates the true maximum — every window is a *valid* substring by construction (shrink keeps dropping from the left until ≤ k distinct chars). The matching lower bound's core, `shrinkVLongest` (shrink returns the *longest* valid suffix, so no better window is missed), is proven too; threading it to full optimality (`= solve`) is left as future work *in Idris* — but the Lean port (above) completes it. The counts-based window is the fast runnable solution.
- **A fold invariant** — the interval merge is proved correct in both **Lean** (`merge-intervals.lean`) and **Idris** (`merge-intervals.idr`): `…NoOverlap` (every pair in the result is separated, `a.snd < b.fst`, as an inductive Pairwise over *all* pairs) and `…WF` (all outputs well-formed `lo ≤ hi`), via a separation invariant threaded through the coalescing `step`/fold — which holds for *any* well-formed input, so it needs no sortedness (the sort only fixes the output shape). Lean is axiom-clean and additionally verifies its insertion sort; the Idris port is `total` throughout, on `Nat` bounds so the comparisons reduce (no `omega` to lean on, so the `≤`/`<`/`max` lemmas are proved by hand).
- **The Logic monad** — `n-queens.hs` does backtracking with `Control.Monad.Logic`; the Racket and Idris ports get the same answers by filtering permutations.
- **Escape continuations** — `valid-parentheses.rkt` parses with `let/cc`, bailing out the moment it sees a mismatch.
