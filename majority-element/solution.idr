module Main

import Data.Nat
import Data.List.Views
import Decidable.Equality

------------------------------------------------------------------------
-- LeetCode 169: Majority Element — Boyer–Moore voting.
--
-- A total left fold carrying (candidate, count).  `majorityElement` returns
-- the candidate; on inputs that have a strict majority (the LeetCode
-- guarantee) it IS that element.
--
-- This file also carries the full count-invariant CORRECTNESS proof, the
-- Idris port of solution.lean: a `Good` invariant established by snoc/reverse
-- induction (`bmGood`), giving `bmFindsMajority` (Boyer–Moore's candidate is
-- the majority whenever one exists) and `majorityUnique` (at most one strict
-- majority).  Idris has no `omega`, so every `<=`/`<`/`+`/`count` step is by
-- hand.  The element type is `Nat` (not the primitive `Int`, which has no
-- provable `DecEq`/order); counts are stated over `Nat`.  Everything `total`.
------------------------------------------------------------------------

%default total

------------------------------------------------------------------------
-- The voting fold.  On a mismatch the count is DECREMENTED (genuine
-- Boyer–Moore); the comparison uses a decidable `decEq` so the proof gets a
-- real `=`/`Not =` witness in each branch.
------------------------------------------------------------------------

step : (Maybe Nat, Nat) -> Nat -> (Maybe Nat, Nat)
step (_,       Z)   x = (Just x, 1)
step (Just c,  S k) x = case decEq x c of
                          Yes _ => (Just c, S (S k))
                          No  _ => (Just c, k)
step (Nothing, S k) x = (Just x, 1)

bm : List Nat -> (Maybe Nat, Nat)
bm = foldl step (Nothing, 0)

majorityElement : List Nat -> Maybe Nat
majorityElement xs = fst (bm xs)

------------------------------------------------------------------------
-- Small self-contained Nat / LTE helpers.
------------------------------------------------------------------------

lteReflN : (n : Nat) -> LTE n n
lteReflN Z     = LTEZero
lteReflN (S k) = LTESucc (lteReflN k)

lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero     _           = LTEZero
lteTrans (LTESucc p) (LTESucc q) = LTESucc (lteTrans p q)

-- LTE (S n) n is impossible.
notSuccLte : (n : Nat) -> LTE (S n) n -> Void
notSuccLte Z      lte          = absurd lte
notSuccLte (S j) (LTESucc lte) = notSuccLte j lte

-- left-cancellation of + under LTE.
ltePlusCancelL : (k : Nat) -> (a, b : Nat) -> LTE (k + a) (k + b) -> LTE a b
ltePlusCancelL Z     a b lte           = lte
ltePlusCancelL (S j) a b (LTESucc lte) = ltePlusCancelL j a b lte

-- n <= k + n.
lteAddL : (k, n : Nat) -> LTE n (k + n)
lteAddL Z     n = lteReflN n
lteAddL (S j) n = lteSuccRight (lteAddL j n)

-- "double", standing in for the Lean `2 *`.
dbl : Nat -> Nat
dbl n = n + n

-- dbl (S n) = S (S (dbl n)).
dblS : (n : Nat) -> dbl (S n) = S (S (dbl n))
dblS n = cong S (sym (plusSuccRightSucc n n))

-- from S (k + m) <= L derive k + S (S m) <= S L.
shiftA : (k, m, l : Nat) -> LTE (S (k + m)) l -> LTE (k + S (S m)) (S l)
shiftA k m l h =
  rewrite sym (plusSuccRightSucc k (S m)) in
  rewrite sym (plusSuccRightSucc k m) in
  LTESucc h

------------------------------------------------------------------------
-- count, with its snoc lemmas (ports of count_snoc_self / count_snoc_of_ne).
------------------------------------------------------------------------

count : Nat -> List Nat -> Nat
count x []        = Z
count x (y :: ys) = case decEq x y of
                      Yes _ => S (count x ys)
                      No  _ => count x ys

countSnocSelf : (x : Nat) -> (xs : List Nat) -> count x (xs ++ [x]) = S (count x xs)
countSnocSelf x []        = rewrite decEqSelfIsYes {x} in Refl
countSnocSelf x (y :: ys) with (decEq x y)
  countSnocSelf x (y :: ys) | Yes _ = cong S (countSnocSelf x ys)
  countSnocSelf x (y :: ys) | No  _ = countSnocSelf x ys

countSnocNe : (x, z : Nat) -> Not (x = z) -> (xs : List Nat) ->
              count x (xs ++ [z]) = count x xs
countSnocNe x z neq [] with (decEq x z)
  countSnocNe x z neq [] | Yes p = void (neq p)
  countSnocNe x z neq [] | No  _ = Refl
countSnocNe x z neq (y :: ys) with (decEq x y)
  countSnocNe x z neq (y :: ys) | Yes _ = cong S (countSnocNe x z neq ys)
  countSnocNe x z neq (y :: ys) | No  _ = countSnocNe x z neq ys

countSingSelf : (y : Nat) -> count y [y] = 1
countSingSelf y = rewrite decEqSelfIsYes {x = y} in Refl

countSingNe : (v, y : Nat) -> Not (v = y) -> count v [y] = 0
countSingNe v y neq with (decEq v y)
  countSingNe v y neq | Yes p = void (neq p)
  countSingNe v y neq | No  _ = Refl

lenSnoc : (xs : List Nat) -> (x : Nat) -> length (xs ++ [x]) = S (length xs)
lenSnoc []        x = Refl
lenSnoc (y :: ys) x = cong S (lenSnoc ys x)

-- foldl over a snoc is one more step (port of bm_snoc / foldl_append).
foldlSnoc : (f : b -> a -> b) -> (acc : b) -> (xs : List a) -> (x : a) ->
            foldl f acc (xs ++ [x]) = f (foldl f acc xs) x
foldlSnoc f acc []        x = Refl
foldlSnoc f acc (y :: ys) x = foldlSnoc f (f acc y) ys x

bmSnoc : (xs : List Nat) -> (x : Nat) -> bm (xs ++ [x]) = step (bm xs) x
bmSnoc xs x = foldlSnoc step (Nothing, 0) xs x

------------------------------------------------------------------------
-- The Good invariant (port of Lean `Good`; the `2 *` of the Lean version is
-- here `dbl`, and the two summands are re-ordered so the `k = 0` instance
-- reduces definitionally).
------------------------------------------------------------------------

Good : List Nat -> (Maybe Nat, Nat) -> Type
Good xs (Nothing, _) = xs = []
Good xs (Just c,  k) =
    ( (v : Nat) -> Not (v = c) -> LTE (k + dbl (count v xs)) (length xs)
    , LTE (dbl (count c xs)) (k + length xs) )

------------------------------------------------------------------------
-- The single-step invariant preservation, split per state shape.  All of the
-- by-hand arithmetic lives here.
------------------------------------------------------------------------

-- for the k = 0 state: every value's doubled count is bounded by the length.
boundZ : (ys : List Nat) -> (c, w : Nat) ->
         ((v : Nat) -> Not (v = c) -> LTE (dbl (count v ys)) (length ys)) ->
         LTE (dbl (count c ys)) (length ys) ->
         LTE (dbl (count w ys)) (length ys)
boundZ ys c w h1 h2 with (decEq w c)
  boundZ ys c w h1 h2 | Yes p  = rewrite p in h2
  boundZ ys c w h1 h2 | No  np = h1 w np

-- Nothing-state (ys = []): the new singleton candidate is `Good`.
nothingCase : (ys : List Nat) -> (y : Nat) -> (ys = []) ->
              Good (ys ++ [y]) (Just y, 1)
nothingCase ys y prf =
  rewrite prf in
    ( \v, vney => rewrite countSingNe v y vney in LTESucc LTEZero
    , rewrite countSingSelf y in LTESucc (LTESucc LTEZero) )

-- (Just c, 0): candidate gets replaced by y, new state (Just y, 1).
c1JustZ : (ys : List Nat) -> (c, y : Nat) ->
          ((v : Nat) -> Not (v = c) -> LTE (dbl (count v ys)) (length ys)) ->
          LTE (dbl (count c ys)) (length ys) ->
          (v : Nat) -> Not (v = y) ->
          LTE (1 + dbl (count v (ys ++ [y]))) (length (ys ++ [y]))
c1JustZ ys c y h1 h2 v vney =
  rewrite countSnocNe v y vney ys in
  rewrite lenSnoc ys y in
  LTESucc (boundZ ys c v h1 h2)

c2JustZ : (ys : List Nat) -> (c, y : Nat) ->
          ((v : Nat) -> Not (v = c) -> LTE (dbl (count v ys)) (length ys)) ->
          LTE (dbl (count c ys)) (length ys) ->
          LTE (dbl (count y (ys ++ [y]))) (1 + length (ys ++ [y]))
c2JustZ ys c y h1 h2 =
  rewrite countSnocSelf y ys in
  rewrite lenSnoc ys y in
  rewrite dblS (count y ys) in
  LTESucc (LTESucc (boundZ ys c y h1 h2))

-- (Just c, S k), y == c: count of c increments, k goes S k -> S (S k).
c1YesBranch : (ys : List Nat) -> (c, y, k : Nat) -> (y = c) ->
              ((v : Nat) -> Not (v = c) -> LTE (S k + dbl (count v ys)) (length ys)) ->
              (v : Nat) -> Not (v = c) ->
              LTE (S (S k) + dbl (count v (ys ++ [y]))) (length (ys ++ [y]))
c1YesBranch ys c y k yc h1 v vnec =
  rewrite countSnocNe v y (\vy => vnec (trans vy yc)) ys in
  rewrite lenSnoc ys y in
  LTESucc (h1 v vnec)

c2YesBranch : (ys : List Nat) -> (c, y, k : Nat) -> (y = c) ->
              LTE (dbl (count c ys)) (S k + length ys) ->
              LTE (dbl (count c (ys ++ [y]))) (S (S k) + length (ys ++ [y]))
c2YesBranch ys c y k yc h2 =
  rewrite yc in
  rewrite countSnocSelf c ys in
  rewrite lenSnoc ys c in
  rewrite dblS (count c ys) in
  LTESucc (LTESucc (rewrite sym (plusSuccRightSucc k (length ys)) in h2))

-- (Just c, S k), y /= c: count of c unchanged, k goes S k -> k.
c1NoBranch : (ys : List Nat) -> (c, y, k : Nat) -> Not (y = c) ->
             ((v : Nat) -> Not (v = c) -> LTE (S k + dbl (count v ys)) (length ys)) ->
             (v : Nat) -> Not (v = c) ->
             LTE (k + dbl (count v (ys ++ [y]))) (length (ys ++ [y]))
c1NoBranch ys c y k np h1 v vnec with (decEq v y)
  c1NoBranch ys c y k np h1 v vnec | Yes veqy =
      rewrite veqy in
      rewrite countSnocSelf y ys in
      rewrite lenSnoc ys y in
      rewrite dblS (count y ys) in
      shiftA k (dbl (count y ys)) (length ys) (h1 y np)
  c1NoBranch ys c y k np h1 v vnec | No vney =
      rewrite countSnocNe v y vney ys in
      rewrite lenSnoc ys y in
      lteSuccRight (lteSuccLeft (h1 v vnec))

c2NoBranch : (ys : List Nat) -> (c, y, k : Nat) -> Not (y = c) ->
             LTE (dbl (count c ys)) (S k + length ys) ->
             LTE (dbl (count c (ys ++ [y]))) (k + length (ys ++ [y]))
c2NoBranch ys c y k np h2 =
  rewrite countSnocNe c y (negEqSym np) ys in
  rewrite lenSnoc ys y in
  rewrite sym (plusSuccRightSucc k (length ys)) in
  h2

goodStep : (ys : List Nat) -> (y : Nat) -> (st : (Maybe Nat, Nat)) ->
           Good ys st -> Good (ys ++ [y]) (step st y)
goodStep ys y (Nothing, Z)   good = nothingCase ys y good
goodStep ys y (Nothing, S k) good = nothingCase ys y good
goodStep ys y (Just c, Z)    good =
    ( c1JustZ ys c y (fst good) (snd good)
    , c2JustZ ys c y (fst good) (snd good) )
goodStep ys y (Just c, S k)  good with (decEq y c)
  goodStep ys y (Just c, S k) good | Yes yc =
      ( c1YesBranch ys c y k yc (fst good)
      , c2YesBranch ys c y k yc (snd good) )
  goodStep ys y (Just c, S k) good | No np =
      ( c1NoBranch ys c y k np (fst good)
      , c2NoBranch ys c y k np (snd good) )

------------------------------------------------------------------------
-- bmGood: the invariant for the full fold, by snoc induction (the `SnocList`
-- view, recursing on its structurally-smaller witness — totally, no fuel).
------------------------------------------------------------------------

bmGoodH : (xs : List Nat) -> SnocList xs -> Good xs (bm xs)
bmGoodH []          Empty          = Refl
bmGoodH (ws ++ [w]) (Snoc w ws rec) =
    rewrite bmSnoc ws w in goodStep ws w (bm ws) (bmGoodH ws rec)

bmGood : (xs : List Nat) -> Good xs (bm xs)
bmGood xs = bmGoodH xs (snocList xs)

------------------------------------------------------------------------
-- Downstream corollaries (ports of bm_finds_majority, majority_iff,
-- count_two_le, majority_unique).
------------------------------------------------------------------------

-- (1) If ANY strict majority exists, Boyer–Moore's candidate IS it.
findsAux : (xs : List Nat) -> (m : Nat) ->
           LTE (S (length xs)) (dbl (count m xs)) ->
           (st : (Maybe Nat, Nat)) -> Good xs st -> fst st = Just m
findsAux xs m hyp (Nothing, k) good =
    void (absurd (replace {p = \zs => LTE (S (length zs)) (dbl (count m zs))} good hyp))
findsAux xs m hyp (Just c, k) good with (decEq m c)
  findsAux xs m hyp (Just c, k) good | Yes mc = cong Just (sym mc)
  findsAux xs m hyp (Just c, k) good | No  mc =
      void (notSuccLte (length xs)
              (lteTrans hyp
                 (lteTrans (lteAddL k (dbl (count m xs))) (fst good m mc))))

bmFindsMajority : (xs : List Nat) -> (m : Nat) ->
                  LTE (S (length xs)) (dbl (count m xs)) ->
                  majorityElement xs = Just m
bmFindsMajority xs m hyp = findsAux xs m hyp (bm xs) (bmGood xs)

-- (2) The proposed iff: the candidate is a strict majority  iff  some value is.
majorityIff : (xs : List Nat) ->
   ( (c : Nat ** (majorityElement xs = Just c, LTE (S (length xs)) (dbl (count c xs)))) ->
       (m : Nat ** LTE (S (length xs)) (dbl (count m xs)))
   , (m : Nat ** LTE (S (length xs)) (dbl (count m xs))) ->
       (c : Nat ** (majorityElement xs = Just c, LTE (S (length xs)) (dbl (count c xs)))) )
majorityIff xs =
   ( \cprf => (fst cprf ** snd (snd cprf))
   , \mprf => (fst mprf ** (bmFindsMajority xs (fst mprf) (snd mprf), snd mprf)) )

-- (3) At most one strict majority exists.
countTwoLe : (a, b : Nat) -> Not (a = b) -> (xs : List Nat) ->
             LTE (count a xs + count b xs) (length xs)
countTwoLe a b neq []        = LTEZero
countTwoLe a b neq (x :: t) with (decEq a x)
  countTwoLe a b neq (x :: t) | Yes ax with (decEq b x)
    countTwoLe a b neq (x :: t) | Yes ax | Yes bx = void (neq (trans ax (sym bx)))
    countTwoLe a b neq (x :: t) | Yes ax | No  _  = LTESucc (countTwoLe a b neq t)
  countTwoLe a b neq (x :: t) | No anx with (decEq b x)
    countTwoLe a b neq (x :: t) | No anx | Yes _  =
        rewrite sym (plusSuccRightSucc (count a t) (count b t)) in
          LTESucc (countTwoLe a b neq t)
    countTwoLe a b neq (x :: t) | No anx | No  _  = lteSuccRight (countTwoLe a b neq t)

majorityUnique : (xs : List Nat) -> (a, b : Nat) ->
                 LTE (S (length xs)) (dbl (count a xs)) ->
                 LTE (S (length xs)) (dbl (count b xs)) ->
                 a = b
majorityUnique xs a b ha hb with (decEq a b)
  majorityUnique xs a b ha hb | Yes ab = ab
  majorityUnique xs a b ha hb | No  ab =
      let ctl    = countTwoLe a b ab xs
          cbLtCa = ltePlusCancelL (count a xs) (S (count b xs)) (count a xs)
                     (replace {p = \z => LTE z (dbl (count a xs))}
                        (plusSuccRightSucc (count a xs) (count b xs))
                        (lteTrans (LTESucc ctl) ha))
          eqStep = trans (cong S (plusCommutative (count a xs) (count b xs)))
                         (plusSuccRightSucc (count b xs) (count a xs))
          caLtCb = ltePlusCancelL (count b xs) (S (count a xs)) (count b xs)
                     (replace {p = \z => LTE z (dbl (count b xs))}
                        eqStep
                        (lteTrans (LTESucc ctl) hb))
      in void (notSuccLte (count a xs) (lteTrans caLtCb (lteSuccLeft cbLtCa)))

------------------------------------------------------------------------

main : IO ()
main = do
  printLn (majorityElement [3, 2, 3])               -- Just 3
  printLn (majorityElement [2, 2, 1, 1, 1, 2, 2])   -- Just 2
  printLn (majorityElement [])                      -- Nothing
