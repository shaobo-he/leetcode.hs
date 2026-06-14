module Main

import Data.List.Quantifiers

------------------------------------------------------------------------
-- Well-formedness, as a depth-indexed inductive relation.
--
--   `Bal d xs` means: reading the characters xs starting at nesting depth d
--   returns to depth 0, and the depth never goes negative along the way.
--   So `Bal 0 xs` is exactly "xs is a balanced parenthesis string".
--
--   * BNil   : at depth 0, the empty string is balanced.
--   * BOpen  : a '(' raises the depth, so '(' :: xs is balanced at depth d
--              whenever xs is balanced at depth (S d).
--   * BClose : a ')' lowers the depth -- only legal when the depth is positive,
--              which is exactly why the conclusion's index is (S d).
------------------------------------------------------------------------

data Bal : Nat -> List Char -> Type where
  BNil   : Bal Z []
  BOpen  : Bal (S d) xs -> Bal d ('(' :: xs)
  BClose : Bal d xs -> Bal (S d) (')' :: xs)

-- Prepend a paren to a certified string, growing its proof accordingly.
consOpen : {d : Nat} -> (xs : List Char ** Bal (S d) xs) -> (ys : List Char ** Bal d ys)
consOpen (xs ** p) = ('(' :: xs ** BOpen p)

consClose : {d : Nat} -> (xs : List Char ** Bal d xs) -> (ys : List Char ** Bal (S d) ys)
consClose (xs ** p) = (')' :: xs ** BClose p)

------------------------------------------------------------------------
-- The generator, correct by construction.  State: o opens still to place,
-- current depth d (which equals the closes still to place minus o).  Every
-- string it emits is paired with a proof that it is `Bal d`.
--   * open  '(' when o > 0      (depth d -> S d, opens -> o-1)
--   * close ')' when d > 0      (depth S d -> d)
------------------------------------------------------------------------

genB : (o : Nat) -> (d : Nat) -> List (xs : List Char ** Bal d xs)
genB Z      Z      = [([] ** BNil)]
genB Z      (S d') = map consClose (genB Z d')
genB (S o') Z      = map consOpen  (genB o' 1)
genB (S o') (S d') = map consOpen  (genB o' (S (S d')))
                  ++ map consClose (genB (S o') d')

------------------------------------------------------------------------
-- The LeetCode answer, now built only from certified character lists.
------------------------------------------------------------------------

dfst : {0 a : Type} -> {0 p : a -> Type} -> (x : a ** p x) -> a
dfst (x ** _) = x

generateParenthesis : Nat -> List String
generateParenthesis n = map (pack . dfst) (genB n 0)

------------------------------------------------------------------------
-- Soundness theorem: every parenthesisation produced for n is balanced.
-- Each element of `genB n 0` already carries a `Bal 0` proof, so we just
-- gather them into an `All`.
------------------------------------------------------------------------

gather : {0 a : Type} -> {0 p : a -> Type} -> (xs : List (x : a ** p x)) -> All p (map Main.dfst xs)
gather []               = []
gather ((x ** px) :: r) = px :: gather r

generateSound : (n : Nat) -> All (Bal 0) (map Main.dfst (genB n 0))
generateSound n = gather (genB n 0)

-- Sanity check that `Bal` really captures well-formedness: a hand proof for
-- "(())".  Malformed strings have *no* such proof -- e.g. `Bal 0 (')' :: _)` is
-- impossible (BClose only ever yields a positive depth), and "(()" gets stuck
-- needing the uninhabited `Bal 1 []`.
exampleBalanced : Bal 0 ('(' :: '(' :: ')' :: ')' :: [])
exampleBalanced = BOpen (BOpen (BClose (BClose BNil)))

main : IO ()
main = do
  printLn (generateParenthesis 3)               -- 5 strings
  printLn (length (generateParenthesis 4))      -- 14 (Catalan number)
  -- `generateSound` type-checks, so every string above is provably balanced.
  putStrLn "every generated string is certified Bal 0"
