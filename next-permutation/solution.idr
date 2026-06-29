module Main

-- LeetCode 31: Next Permutation — a purely functional recast over `List Int`.
-- All functions are `total` (structural recursion on the list).  We prove the
-- two structural building blocks that the Lean centerpiece relies on:
--   * `findPivotEq`  — `findPivot` recovers the split `xs = pre ++ p :: suf`;
--   * `swapLastGt`   — the swapped-in element strictly exceeds the pivot `p`.
-- (The full `Perm` / `Lex` correctness theorems are carried in solution.lean.)

------------------------------------------------------------------------
-- The algorithm
------------------------------------------------------------------------

-- Rightmost-ascent split: xs = pre ++ p :: suf, suf maximal weakly-decreasing,
-- p < head suf.  Nothing ⇒ xs is weakly decreasing (already maximal).
total
findPivot : List Int -> Maybe (List Int, Int, List Int)
findPivot []             = Nothing
findPivot (_ :: [])      = Nothing
findPivot (x :: y :: rest) =
  case findPivot (y :: rest) of
    Just (pre, p, suf) => Just (x :: pre, p, suf)
    Nothing            => if x < y then Just ([], x, y :: rest) else Nothing

-- replace the rightmost element > p with p; return (that element, new suffix)
total
swapLast : Int -> List Int -> Maybe (Int, List Int)
swapLast _ []        = Nothing
swapLast p (c :: rest) =
  case swapLast p rest of
    Just (g, rest') => Just (g, c :: rest')
    Nothing         => if p < c then Just (c, p :: rest) else Nothing

total
nextPermutation : List Int -> List Int
nextPermutation xs =
  case findPivot xs of
    Nothing => reverse xs
    Just (pre, p, suf) =>
      case swapLast p suf of
        Just (g, suf') => pre ++ g :: reverse suf'
        Nothing        => reverse xs

------------------------------------------------------------------------
-- Proof 1: findPivot recovers the structural split.
------------------------------------------------------------------------

total
findPivotEq : (xs, pre, suf : List Int) -> (p : Int) ->
              findPivot xs = Just (pre, p, suf) -> xs = pre ++ p :: suf
findPivotEq []        pre suf p prf = absurd prf
findPivotEq (_ :: []) pre suf p prf = absurd prf
findPivotEq (x :: y :: rest) pre suf p prf with (findPivot (y :: rest)) proof eq
  _ | Just (pre', p', suf') =
        case prf of
          Refl => cong (x ::) (findPivotEq (y :: rest) pre' suf' p' eq)
  _ | Nothing with (x < y) proof eqxy
    _ | True  = case prf of Refl => Refl
    _ | False = absurd prf

------------------------------------------------------------------------
-- Proof 2: the swapped-in element strictly exceeds the pivot.
------------------------------------------------------------------------

total
swapLastGt : (p : Int) -> (l : List Int) -> (g : Int) -> (l' : List Int) ->
             swapLast p l = Just (g, l') -> (p < g) = True
swapLastGt p []          g l' prf = absurd prf
swapLastGt p (c :: rest) g l' prf with (swapLast p rest) proof eq
  _ | Just (g0, r0) = case prf of Refl => swapLastGt p rest g0 r0 eq
  _ | Nothing with (p < c) proof eqc
    _ | True  = case prf of Refl => eqc
    _ | False = absurd prf

------------------------------------------------------------------------

main : IO ()
main = do
  printLn (nextPermutation [1, 2, 3])   -- [1, 3, 2]
  printLn (nextPermutation [3, 2, 1])   -- [1, 2, 3] (wrap)
  printLn (nextPermutation [1, 1, 5])   -- [1, 5, 1]
  printLn (nextPermutation [1, 3, 2])   -- [2, 1, 3]
  printLn (nextPermutation [1, 5, 1])   -- [5, 1, 1]
