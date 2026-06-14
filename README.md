# leetcode.hs

> Selected LeetCode problems, each solved three ways — in **Racket**, **Haskell**, and **Idris 2**.
>
> *Functional programming fan gotta get a job!*

[![CI](https://github.com/shaobo-he/leetcode.hs/actions/workflows/ci.yml/badge.svg)](https://github.com/shaobo-he/leetcode.hs/actions/workflows/ci.yml)

Every problem lives in its own folder with all three implementations:

```
two-sum/
  solution.rkt   — Racket (typed where useful), with rackunit tests
  solution.hs    — Haskell (GHC), with a runnable main
  solution.idr   — Idris 2, type-checked (and runnable)
```

## Running & testing

| Language | Run one | Check / test all |
| --- | --- | --- |
| Racket  | `racket two-sum/solution.rkt`               | `raco test $(find . -name '*.rkt')` |
| Haskell | `runghc two-sum/solution.hs`                | loop `runghc` over every `*.hs` |
| Idris 2 | `idris2 two-sum/solution.idr --exec main`   | `idris2 --check` over every `*.idr` |

Racket solutions carry their tests in a `(module+ test …)` submodule (89 `rackunit`
checks in all); Haskell and Idris solutions each have a `main` demonstrating worked
examples. CI runs all three toolchains on every push.

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

- **Brzozowski derivatives** — `regular-expression-matching` matches by differentiating the regex one character at a time, in all three languages.
- **Dependent types** — `merge-sorted-array.idr` returns a `Vect (m + n)`, carrying a proof that merging an `m`-vector and an `n`-vector yields exactly `m + n` elements.
- **Machine-checked proofs** — `generate-parenthesis.idr` proves (totally, in Idris 2) that its output is *exactly* the balanced strings: soundness (`generateSound : All (Bal 0) …`) and completeness (`generateComplete : Bal 0 xs → … Elem …`). `valid-parentheses.idr` proves its recognizer accepts exactly the `Bal 0` strings. Both model a parenthesis as a two-constructor `Paren` type so Idris's coverage checker can verify the proofs are total (a primitive `Char` index can't be case-split).
- **A verified optimisation** — `length-of-longest-substring-k-distinct.idr` carries a brute-force reference with a *full* correctness proof: `optimal` (no substring with ≤ k distinct chars is longer than the answer) and `achievable` (the answer is realised by an actual valid substring), via an inductive "contiguous substring" relation and enumeration sound/completeness lemmas. The O(n) sliding window is the fast runnable solution.
- **The Logic monad** — `n-queens.hs` does backtracking with `Control.Monad.Logic`; the Racket and Idris ports get the same answers by filtering permutations.
- **Escape continuations** — `valid-parentheses.rkt` parses with `let/cc`, bailing out the moment it sees a mismatch.
