module Main

import Data.List.Quantifiers
import Data.List.Elem

------------------------------------------------------------------------
-- A parenthesis is one of exactly two things. Unlike the primitive `Char`,
-- Idris can case-split this type, so every function and proof below is total.
------------------------------------------------------------------------

data Paren = LP | RP   -- '(' and ')'

parenChar : Paren -> Char
parenChar LP = '('
parenChar RP = ')'

------------------------------------------------------------------------
-- Well-formedness, as a depth-indexed inductive relation.
--   `Bal d xs` : reading xs from nesting depth d returns to depth 0 without
--   the depth ever going negative.  `Bal 0` = "balanced parentheses".
------------------------------------------------------------------------

data Bal : Nat -> List Paren -> Type where
  BNil   : Bal Z []
  BOpen  : Bal (S d) xs -> Bal d (LP :: xs)
  BClose : Bal d xs -> Bal (S d) (RP :: xs)

consOpen : {d : Nat} -> (xs : List Paren ** Bal (S d) xs) -> (ys : List Paren ** Bal d ys)
consOpen (xs ** p) = (LP :: xs ** BOpen p)

consClose : {d : Nat} -> (xs : List Paren ** Bal d xs) -> (ys : List Paren ** Bal (S d) ys)
consClose (xs ** p) = (RP :: xs ** BClose p)

------------------------------------------------------------------------
-- The generator, correct by construction: every string it emits is paired
-- with a proof that it is `Bal d`. State: o opens still to place, depth d.
------------------------------------------------------------------------

genB : (o : Nat) -> (d : Nat) -> List (xs : List Paren ** Bal d xs)
genB Z      Z      = [([] ** BNil)]
genB Z      (S d') = map consClose (genB Z d')
genB (S o') Z      = map consOpen  (genB o' 1)
genB (S o') (S d') = map consOpen  (genB o' (S (S d')))
                  ++ map consClose (genB (S o') d')

dfst : {0 a : Type} -> {0 p : a -> Type} -> (x : a ** p x) -> a
dfst (x ** _) = x

render : List Paren -> String
render = pack . map parenChar

generateParenthesis : Nat -> List String
generateParenthesis n = map (render . dfst) (genB n 0)

------------------------------------------------------------------------
-- Soundness: every generated string is balanced (Bal 0).
------------------------------------------------------------------------

gather : {0 a : Type} -> {0 p : a -> Type} ->
         (xs : List (x : a ** p x)) -> All p (map Main.dfst xs)
gather []               = []
gather ((_ ** px) :: r) = px :: gather r

generateSound : (n : Nat) -> All (Bal 0) (map Main.dfst (genB n 0))
generateSound n = gather (genB n 0)

------------------------------------------------------------------------
-- Completeness: every balanced string is actually generated.
-- `opens xs` (number of LP) equals half the length of a Bal 0 string, so this
-- says "every balanced string of length 2n is produced by generateParenthesis n".
------------------------------------------------------------------------

opens : List Paren -> Nat
opens []         = Z
opens (LP :: cs) = S (opens cs)
opens (RP :: cs) = opens cs

mapApp : (f : a -> b) -> (xs, ys : List a) -> map f (xs ++ ys) = map f xs ++ map f ys
mapApp f []        ys = Refl
mapApp f (x :: xs) ys = cong (f x ::) (mapApp f xs ys)

elemAppL : Elem x xs -> Elem x (xs ++ ys)
elemAppL Here      = Here
elemAppL (There p) = There (elemAppL p)

elemAppR : (xs : List a) -> Elem x ys -> Elem x (xs ++ ys)
elemAppR []        p = p
elemAppR (_ :: xs) p = There (elemAppR xs p)

consOpenElem : {0 d : Nat} -> (L : List (z : List Paren ** Bal (S d) z)) ->
               Elem ys (map Main.dfst L) ->
               Elem (LP :: ys) (map Main.dfst (map Main.consOpen L))
consOpenElem [] e = absurd e
consOpenElem ((_ ** _) :: _)    Here      = Here
consOpenElem ((_ ** _) :: rest) (There e) = There (consOpenElem rest e)

consCloseElem : {0 d : Nat} -> (L : List (z : List Paren ** Bal d z)) ->
                Elem ys (map Main.dfst L) ->
                Elem (RP :: ys) (map Main.dfst (map Main.consClose L))
consCloseElem [] e = absurd e
consCloseElem ((_ ** _) :: _)    Here      = Here
consCloseElem ((_ ** _) :: rest) (There e) = There (consCloseElem rest e)

genOpenStep : (o' : Nat) -> (d : Nat) ->
              Elem ys (map Main.dfst (genB o' (S d))) ->
              Elem (LP :: ys) (map Main.dfst (genB (S o') d))
genOpenStep o' Z       e = consOpenElem (genB o' (S Z)) e
genOpenStep o' (S d'') e =
  rewrite mapApp Main.dfst (map consOpen (genB o' (S (S d''))))
                           (map consClose (genB (S o') d'')) in
  elemAppL (consOpenElem (genB o' (S (S d''))) e)

genCloseStep : (o : Nat) -> (d' : Nat) ->
               Elem ys (map Main.dfst (genB o d')) ->
               Elem (RP :: ys) (map Main.dfst (genB o (S d')))
genCloseStep Z       d' e = consCloseElem (genB Z d') e
genCloseStep (S o'') d' e =
  rewrite mapApp Main.dfst (map consOpen (genB o'' (S (S d'))))
                           (map consClose (genB (S o'') d')) in
  elemAppR (map Main.dfst (map consOpen (genB o'' (S (S d')))))
           (consCloseElem (genB (S o'') d') e)

-- a clean, total induction: splitting `List Paren` gives [], LP::ys, RP::ys,
-- and Idris knows those are ALL the cases (Paren has two constructors) -- the
-- coverage problem that `Char` would cause simply does not arise.
complete : (xs : List Paren) -> {d : Nat} -> Bal d xs ->
           Elem xs (map Main.dfst (genB (opens xs) d))
complete []         BNil            = Here
complete (LP :: ys) (BOpen p)       = genOpenStep  (opens ys) d  (complete ys p)
complete (RP :: ys) (BClose {d=d0} p) = genCloseStep (opens ys) d0 (complete ys p)

generateComplete : (n : Nat) -> (xs : List Paren) -> Bal 0 xs -> opens xs = n ->
                   Elem xs (map Main.dfst (genB n 0))
generateComplete n xs bal prf = rewrite sym prf in complete xs bal

-- a hand proof that "(())" is balanced; ")(" and "(()" have no Bal proof.
exampleBalanced : Bal 0 (LP :: LP :: RP :: RP :: [])
exampleBalanced = BOpen (BOpen (BClose (BClose BNil)))

main : IO ()
main = do
  printLn (generateParenthesis 3)           -- 5 strings
  printLn (length (generateParenthesis 4))  -- 14 (Catalan number)
  -- generateSound + generateComplete both type-check (totally), so
  -- generateParenthesis n produces EXACTLY the balanced strings.
  putStrLn "soundness + completeness proven: output = the Bal 0 strings"
