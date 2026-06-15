module Main

import Data.List   -- appendAssociative
import Decidable.Equality

------------------------------------------------------------------------
-- LeetCode 3: Longest Substring Without Repeating Characters.
--
-- This file ports the Lean proof in solution.lean to Idris 2.  It has two
-- halves:
--
--   HALF 1 -- OPTIMALITY of a structurally-shrinking sliding window
--             `lengthOfLongestV`: its answer is EXACTLY the maximum length over
--             all substrings whose characters are pairwise distinct.
--               * windowOptimal    -- no distinct substring is longer;
--               * windowRealizable -- the answer is realised by an actual one;
--               * lengthOfLongestVOptimal -- both, packaged.
--
--   HALF 2 -- EQUIVALENCE: the fast last-seen-index algorithm `lengthOfLongest`
--             (the shipped O(n) solution) computes the same number as the
--             verified shrink window.
--               * goFastEqGoV          -- the simulation, by induction;
--               * lengthOfLongestEq    -- lengthOfLongest s = lengthOfLongestV s;
--               * lengthOfLongestOptimal -- optimality transferred to the shipped
--                                          algorithm.
--
-- Validity = "all characters distinct": `Distinct cs := numDistinct cs = length cs`.
--
-- Everything is structural so it REDUCES inside proofs (Idris has no omega).
-- Every proof function is `total`; the file uses NONE of believe_me,
-- really_believe_me, assert_total, idris_crash, postulate or holes.
-- `assert_smaller` appears only for genuine structural decreases (the
-- as-pattern recursions), exactly as in merge-intervals/best-time.
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Structural Bool comparisons / arithmetic on Nat (these REDUCE in proofs).
------------------------------------------------------------------------

total
lteNat : Nat -> Nat -> Bool
lteNat Z     _     = True
lteNat (S _) Z     = False
lteNat (S m) (S n) = lteNat m n

total
maxN : Nat -> Nat -> Nat
maxN Z     b     = b
maxN a     Z     = a
maxN (S a) (S b) = S (maxN a b)

total
lteRefl : (n : Nat) -> lteNat n n = True
lteRefl Z     = Refl
lteRefl (S m) = lteRefl m

total
lteTrans : (a, b, c : Nat) -> lteNat a b = True -> lteNat b c = True -> lteNat a c = True
lteTrans Z     b     c     _ _ = Refl
lteTrans (S a) (S b) (S c) p q = lteTrans a b c p q
lteTrans (S a) Z     c     p _ = absurd p
lteTrans (S a) (S b) Z     _ q = absurd q

total
lteSuccR : (n : Nat) -> lteNat n (S n) = True
lteSuccR Z     = Refl
lteSuccR (S m) = lteSuccR m

total
leMaxL : (a, b : Nat) -> lteNat a (maxN a b) = True
leMaxL Z     b     = Refl
leMaxL (S a) Z     = lteRefl (S a)
leMaxL (S a) (S b) = leMaxL a b

total
leMaxR : (a, b : Nat) -> lteNat b (maxN a b) = True
leMaxR Z     b     = lteRefl b
leMaxR (S a) Z     = Refl
leMaxR (S a) (S b) = leMaxR a b

total
maxIs : (a, b : Nat) -> Either (maxN a b = a) (maxN a b = b)
maxIs Z     b     = Right Refl
maxIs (S a) Z     = Left Refl
maxIs (S a) (S b) = case maxIs a b of
                      Left  p => Left  (cong S p)
                      Right p => Right (cong S p)

total
maxNLUB : (a, b, n : Nat) -> lteNat a n = True -> lteNat b n = True -> lteNat (maxN a b) n = True
maxNLUB Z     b     n     _  pb = pb
maxNLUB (S a) Z     n     pa _  = pa
maxNLUB (S a) (S b) (S n) pa pb = maxNLUB a b n pa pb
maxNLUB (S a) (S b) Z     pa _  = absurd pa

-- max equals the bigger argument once we know one is below the other
total
maxEqR : (a, b : Nat) -> lteNat a b = True -> maxN a b = b
maxEqR Z     b     _ = Refl
maxEqR (S a) (S b) p = cong S (maxEqR a b p)
maxEqR (S a) Z     p = absurd p

total
maxEqL : (a, b : Nat) -> lteNat b a = True -> maxN a b = a
maxEqL a     Z     _ = case a of { Z => Refl; (S _) => Refl }
maxEqL Z     (S b) p = absurd p
maxEqL (S a) (S b) p = cong S (maxEqL a b p)

------------------------------------------------------------------------
-- Own structural list helpers (right-neutral, associativity, length).
------------------------------------------------------------------------

total
appendNilR : (l : List a) -> l ++ [] = l
appendNilR []        = Refl
appendNilR (x :: xs) = cong (x ::) (appendNilR xs)

total
appendAssoc : (l, m, r : List a) -> (l ++ m) ++ r = l ++ (m ++ r)
appendAssoc []        m r = Refl
appendAssoc (x :: xs) m r = cong (x ::) (appendAssoc xs m r)

total
lengthAppend : (l, r : List a) -> length (l ++ r) = length l + length r
lengthAppend []        r = Refl
lengthAppend (x :: xs) r = cong S (lengthAppend xs r)

total
lengthSnoc : (l : List a) -> (x : a) -> length (l ++ [x]) = S (length l)
lengthSnoc l x = trans (lengthAppend l [x]) (plusOneIsS (length l))
  where
    plusOneIsS : (n : Nat) -> n + 1 = S n
    plusOneIsS Z     = Refl
    plusOneIsS (S m) = cong S (plusOneIsS m)

------------------------------------------------------------------------
-- Distinct-character count via DECIDABLE equality (so we can REASON about
-- it -- unlike `==`, decEq reflects propositional equality).
------------------------------------------------------------------------

total
decElem : Char -> List Char -> Bool
decElem x [] = False
decElem x (y :: ys) = case decEq x y of
                        Yes _ => True
                        No  _ => decElem x ys

-- "+0 if the head already occurs, +1 otherwise", as a STRICT pattern match
-- (no `if`/Lazy) so the count reduces cleanly inside proofs.
total
ndStep : Bool -> Nat -> Nat
ndStep True  m = m
ndStep False m = S m

total
nd : List Char -> Nat
nd []        = 0
nd (x :: xs) = ndStep (decElem x xs) (nd xs)

-- the distinct count never exceeds the length
total
ndLeLength : (cs : List Char) -> lteNat (nd cs) (length cs) = True
ndLeLength []        = Refl
ndLeLength (x :: xs) with (decElem x xs)
  _ | True  = lteTrans (nd xs) (length xs) (S (length xs)) (ndLeLength xs) (lteSuccR (length xs))
  _ | False = ndLeLength xs

-- a decidable single-char equality test as a Bool (so it reduces cleanly)
total
ceq : Char -> Char -> Bool
ceq a b = case decEq a b of { Yes _ => True; No _ => False }

total
ceqSym : (a, b : Char) -> ceq a b = ceq b a
ceqSym a b with (decEq a b)
  _ | Yes p with (decEq b a)
    _ | Yes _ = Refl
    _ | No q  = absurd (q (sym p))
  _ | No p with (decEq b a)
    _ | Yes q = absurd (p (sym q))
    _ | No _  = Refl

-- a STRICT boolean or (no Delay) so everything reduces inside proofs
total
orB : Bool -> Bool -> Bool
orB True  _ = True
orB False b = b

total
orBFalseR : (b : Bool) -> orB b False = b
orBFalseR True  = Refl
orBFalseR False = Refl

total
orBAssoc : (p, q, r : Bool) -> orB p (orB q r) = orB (orB p q) r
orBAssoc True  _ _ = Refl
orBAssoc False _ _ = Refl

-- decElem on a cons, spelled with ceq (strict or)
total
decElemCons : (x, y : Char) -> (ys : List Char) ->
              decElem x (y :: ys) = orB (ceq x y) (decElem x ys)
decElemCons x y ys with (decEq x y)
  _ | Yes _ = Refl
  _ | No  _ = Refl

-- decElem after a snoc: x is in (xs ++ [c]) iff in xs or x == c
total
decElemSnoc : (x, c : Char) -> (xs : List Char) ->
              decElem x (xs ++ [c]) = orB (decElem x xs) (ceq x c)
decElemSnoc x c [] =
  -- decElem x [c] = ceq x c `orB` decElem x []  ==  ceq x c `orB` False  ==  False `orB` ceq x c
  rewrite decElemCons x c [] in orBFalseR (ceq x c)
decElemSnoc x c (y :: ys) =
  rewrite decElemCons x y (ys ++ [c]) in
  rewrite decElemSnoc x c ys in
  rewrite decElemCons x y ys in
  orBAssoc (ceq x y) (decElem x ys) (ceq x c)

total
ceqIsTrue : (a, b : Char) -> a = b -> ceq a b = True
ceqIsTrue a b p with (decEq a b)
  _ | Yes _ = Refl
  _ | No q  = absurd (q p)

total
ceqIsFalse : (a, b : Char) -> (a = b -> Void) -> ceq a b = False
ceqIsFalse a b np with (decEq a b)
  _ | Yes p = absurd (np p)
  _ | No _  = Refl

total
orBTrueR : (b : Bool) -> orB b True = True
orBTrueR True  = Refl
orBTrueR False = Refl

-- the two-branch swap used in nd_snoc when the new char differs from the head:
--   ndStep p (ndStep q m) = ndStep q (ndStep p m)   (ndStep increments commute)
total
ndStepSwap : (p, q : Bool) -> (m : Nat) -> ndStep p (ndStep q m) = ndStep q (ndStep p m)
ndStepSwap True  True  m = Refl
ndStepSwap True  False m = Refl
ndStepSwap False True  m = Refl
ndStepSwap False False m = Refl

-- congruence: ndStep respects equality of its Bool and Nat arguments.
total
ndStepCong : {b, b' : Bool} -> {p, p' : Nat} -> b = b' -> p = p' ->
             ndStep b p = ndStep b' p'
ndStepCong Refl Refl = Refl

-- appending one char changes the distinct count by exactly 0 or 1.
-- We dispatch on the two decidable equalities (mirroring the Lean rcases) via
-- top-level auxiliaries so the recursive call is unobstructed by a `with` block.
total
ndSnocEq : (c, x : Char) -> (xs : List Char) -> x = c ->
           (nd (xs ++ [c]) = ndStep (decElem c xs) (nd xs)) ->
           nd ((x :: xs) ++ [c]) = ndStep (decElem c (x :: xs)) (nd (x :: xs))
ndSnocEq c x xs prf ih =
  -- LHS: nd (x :: (xs++[c])) = ndStep (decElem x (xs++[c])) (nd (xs++[c])).
  rewrite decElemSnoc x c xs in     -- decElem x (xs++[c]) = orB (decElem x xs) (ceq x c)
  rewrite ceqIsTrue x c prf in      -- ceq x c = True
  rewrite orBTrueR (decElem x xs) in-- orB _ True = True, so head step is "True"
  rewrite decElemCons c x xs in     -- decElem c (x::xs) = orB (ceq c x) (decElem c xs)
  rewrite ceqIsTrue c x (sym prf) in-- ceq c x = True, so that head step is "True" too
  -- goal: nd (xs++[c]) = ndStep (decElem x xs) (nd xs).
  -- bridge to `ih` (decElem c xs) via decElem x xs = decElem c xs (from x = c).
  trans ih (ndStepCong (sym (cong (\z => decElem z xs) prf)) Refl)

total
ndSnocNe : (c, x : Char) -> (xs : List Char) -> (x = c -> Void) ->
           (nd (xs ++ [c]) = ndStep (decElem c xs) (nd xs)) ->
           nd ((x :: xs) ++ [c]) = ndStep (decElem c (x :: xs)) (nd (x :: xs))
ndSnocNe c x xs prf ih =
  rewrite decElemSnoc x c xs in     -- decElem x (xs++[c]) = orB (decElem x xs) (ceq x c)
  rewrite ceqIsFalse x c prf in     -- ceq x c = False
  rewrite orBFalseR (decElem x xs) in
  rewrite decElemCons c x xs in     -- decElem c (x::xs) = orB (ceq c x) (decElem c xs)
  rewrite ceqIsFalse c x (\h => prf (sym h)) in -- ceq c x = False
  -- goal: ndStep (decElem x xs) (nd (xs++[c])) = ndStep (decElem c xs) (nd (x::xs))
  --   nd (x::xs) = ndStep (decElem x xs) (nd xs) definitionally.
  -- A: rewrite the inner nd(xs++[c]) by ih; B: commute the two steps.
  trans (ndStepCong {b = decElem x xs} {b' = decElem x xs} Refl ih)
        (ndStepSwap (decElem x xs) (decElem c xs) (nd xs))

total
ndSnoc : (c : Char) -> (xs : List Char) ->
         nd (xs ++ [c]) = ndStep (decElem c xs) (nd xs)
ndSnoc c [] = Refl
ndSnoc c (x :: xs) = case decEq x c of
  Yes prf => ndSnocEq c x xs prf (ndSnoc c xs)
  No  prf => ndSnocNe c x xs prf (ndSnoc c xs)

------------------------------------------------------------------------
-- HALF 1.  Validity = "all characters distinct": the distinct count equals
-- the length.  We use a propositional `Distinct cs := nd cs = length cs`.
------------------------------------------------------------------------

Distinct : List Char -> Type
Distinct cs = (nd cs = length cs)

total
distinctNil : Distinct []
distinctNil = Refl

-- structural Nat equality (decides Distinct without `if`)
total
eqNat : Nat -> Nat -> Bool
eqNat Z     Z     = True
eqNat Z     (S _) = False
eqNat (S _) Z     = False
eqNat (S a) (S b) = eqNat a b

total
eqNatTrue : (a, b : Nat) -> eqNat a b = True -> a = b
eqNatTrue Z     Z     _   = Refl
eqNatTrue Z     (S _) prf = absurd prf
eqNatTrue (S _) Z     prf = absurd prf
eqNatTrue (S a) (S b) prf = cong S (eqNatTrue a b prf)

total
succInj : (a, b : Nat) -> S a = S b -> a = b
succInj b b Refl = Refl

total
eqNatFalse : (a, b : Nat) -> eqNat a b = False -> (a = b -> Void)
eqNatFalse Z     Z     prf _  = absurd prf
eqNatFalse Z     (S _) _   eq = case eq of Refl impossible
eqNatFalse (S _) Z     _   eq = case eq of Refl impossible
eqNatFalse (S a) (S b) prf eq = eqNatFalse a b prf (succInj a b eq)

total
eqNatRefl : (n : Nat) -> eqNat n n = True
eqNatRefl Z     = Refl
eqNatRefl (S m) = eqNatRefl m

-- the distinct count never exceeds the length (Nat-prop form for arithmetic)
total
ndLeLenLTE : (cs : List Char) -> lteNat (nd cs) (length cs) = True
ndLeLenLTE = ndLeLength

-- lteNat (S b) b is impossible
total
ltSelfAbsurd : (b : Nat) -> lteNat (S b) b = True -> Void
ltSelfAbsurd Z     prf = absurd prf
ltSelfAbsurd (S k) prf = ltSelfAbsurd k prf

-- specialised snoc identities (the head-step resolved by a Bool equation)
total
ndSnocT : (c : Char) -> (xs : List Char) -> decElem c xs = True  -> nd (xs ++ [c]) = nd xs
ndSnocT c xs e = trans (ndSnoc c xs) (cong (\b => ndStep b (nd xs)) e)

total
ndSnocF : (c : Char) -> (xs : List Char) -> decElem c xs = False -> nd (xs ++ [c]) = S (nd xs)
ndSnocF c xs e = trans (ndSnoc c xs) (cong (\b => ndStep b (nd xs)) e)

-- a prefix of an all-distinct list is all-distinct (Lean `distinct_init`)
total
distinctInit : (xs : List Char) -> (c : Char) -> Distinct (xs ++ [c]) -> Distinct xs
distinctInit xs c h with (decElem c xs) proof eq
  -- the head occurs already: h forces nd xs = S (length xs), impossible since nd xs <= length xs.
  _ | True  =
      let h1 : (nd xs = S (length xs))
          h1 = trans (sym (ndSnocT c xs eq)) (trans h (lengthSnoc xs c))
      in absurd (ltSelfAbsurd (length xs)
                   (replace {p = \n => lteNat n (length xs) = True} h1 (ndLeLength xs)))
  -- the head is new: nd (xs++[c]) = S (nd xs) = S (length xs), so nd xs = length xs.
  _ | False =
      let h1 : (S (nd xs) = S (length xs))
          h1 = trans (sym (ndSnocF c xs eq)) (trans h (lengthSnoc xs c))
      in succInj (nd xs) (length xs) h1

------------------------------------------------------------------------
-- Suffix relation (`sf` is a suffix of `w`) and the structural shrink window.
------------------------------------------------------------------------

data Suffix : List a -> List a -> Type where
  SuffNil  : Suffix w w
  SuffCons : (x : a) -> Suffix sf w -> Suffix sf (x :: w)

total
suffixNil : (w : List a) -> Suffix [] w
suffixNil []        = SuffNil
suffixNil (x :: xs) = SuffCons x (suffixNil xs)

total
suffixLength : {sf, w : List a} -> Suffix sf w -> lteNat (length sf) (length w) = True
suffixLength {sf} {w = sf} SuffNil = lteRefl (length sf)
suffixLength {sf} {w = (x :: w')} (SuffCons x p) =
  lteTrans (length sf) (length w') (S (length w')) (suffixLength p) (lteSuccR (length w'))

total
suffixTrans : Suffix a b -> Suffix b c -> Suffix a c
suffixTrans p SuffNil          = p
suffixTrans p (SuffCons x q)   = SuffCons x (suffixTrans p q)

-- a suffix relationship survives appending the same element on the right
total
suffixSnoc : (c : a) -> Suffix sf w -> Suffix (sf ++ [c]) (w ++ [c])
suffixSnoc c SuffNil        = SuffNil
suffixSnoc c (SuffCons x p) = SuffCons x (suffixSnoc c p)

-- shrinkV: drop chars from the FRONT until the window is all-distinct
-- (= the longest all-distinct suffix of the window).
total
shrinkV : List Char -> List Char
shrinkV []        = []
shrinkV (x :: xs) =
  if eqNat (nd (x :: xs)) (length (x :: xs))
    then x :: xs
    else shrinkV (assert_smaller (x :: xs) xs)

total
shrinkVSuffix : (w : List Char) -> Suffix (shrinkV w) w
shrinkVSuffix []        = SuffNil
shrinkVSuffix (x :: xs) with (eqNat (nd (x :: xs)) (length (x :: xs)))
  _ | True  = SuffNil
  _ | False = SuffCons x (shrinkVSuffix xs)

total
shrinkVValid : (w : List Char) -> Distinct (shrinkV w)
shrinkVValid []        = Refl
shrinkVValid (x :: xs) with (eqNat (nd (x :: xs)) (length (x :: xs))) proof eq
  _ | True  = eqNatTrue (nd (x :: xs)) (length (x :: xs)) eq
  _ | False = shrinkVValid xs

-- inversion: a suffix of (x :: xs) is either the whole thing or a suffix of xs
total
suffixConsInv : {sf : List Char} -> {x : Char} -> {xs : List Char} ->
                Suffix sf (x :: xs) -> Either (sf = x :: xs) (Suffix sf xs)
suffixConsInv SuffNil          = Left Refl
suffixConsInv (SuffCons x p)   = Right p

-- every all-distinct suffix of w is a suffix of shrinkV w (Lean `shrinkV_longest`)
total
shrinkVLongest : (w : List Char) -> {sf : List Char} -> Distinct sf ->
                 Suffix sf w -> Suffix sf (shrinkV w)
shrinkVLongest []        hv hsf = hsf
shrinkVLongest (x :: xs) hv hsf with (eqNat (nd (x :: xs)) (length (x :: xs))) proof eq
  _ | True  = hsf
  _ | False =
      -- (x::xs) is NOT distinct, but sf IS, so sf /= x::xs; hence sf is a suffix of xs.
      case suffixConsInv hsf of
        Left  eqsf => -- sf = x::xs, contradicting Distinct sf vs not-distinct (x::xs)
          absurd (eqNatFalse (nd (x :: xs)) (length (x :: xs)) eq
                    (replace {p = \l => nd l = length l} eqsf hv))
        Right hsfx => shrinkVLongest xs hv hsfx

------------------------------------------------------------------------
-- Substring (infix) relation and the verified structural sliding window.
------------------------------------------------------------------------

-- `sub` is a contiguous substring of `s`: before ++ (sub ++ after) = s.
Infix : List Char -> List Char -> Type
Infix sub s = (before : List Char ** after : List Char ** before ++ (sub ++ after) = s)

-- a suffix is an infix (take before = the dropped prefix, after = [])
total
suffixToInfix : {sf, w : List Char} -> Suffix sf w -> Infix sf w
suffixToInfix {sf} {w} suf =
  let (pre ** prf) = suffixPrefix suf
  in (pre ** [] ** rewrite appendNilR sf in prf)
  where
    -- a suffix gives the dropped prefix with pre ++ sf = w
    suffixPrefix : {sf, w : List Char} -> Suffix sf w -> (pre : List Char ** pre ++ sf = w)
    suffixPrefix SuffNil        = ([] ** Refl)
    suffixPrefix (SuffCons x p) =
      let (pre ** prf) = suffixPrefix p in (x :: pre ** cong (x ::) prf)

-- if `sf` is a suffix of `b` and `b ++ t = full`, then `sf` is an infix of full.
total
suffixPrefixInfix : {sf, b : List Char} -> Suffix sf b -> (t : List Char) ->
                    (full : List Char) -> b ++ t = full -> Infix sf full
suffixPrefixInfix {sf} {b} suf t full hbt =
  let (pre ** prf) = suffixPrefix' suf       -- pre ++ sf = b
  in (pre ** t ** rewrite sym hbt in
                  rewrite sym (appendAssoc pre sf t) in
                  cong (++ t) prf)
  where
    suffixPrefix' : {sf, w : List Char} -> Suffix sf w -> (pre : List Char ** pre ++ sf = w)
    suffixPrefix' SuffNil        = ([] ** Refl)
    suffixPrefix' (SuffCons x p) =
      let (pre ** prf) = suffixPrefix' p in (x :: pre ** cong (x ::) prf)

------------------------------------------------------------------------
-- More distinct helpers (used by the window invariant and the crux).
------------------------------------------------------------------------

-- Distinct (x :: xs)  <->  decElem x xs = False  AND  Distinct xs
total
distinctConsTrue : (x : Char) -> (xs : List Char) -> Distinct (x :: xs) ->
                   (decElem x xs = False, Distinct xs)
distinctConsTrue x xs hd with (decElem x xs) proof eq
  _ | True  =
      -- nd (x::xs) = nd xs = length (x::xs) = S (length xs); but nd xs <= length xs.
      let h1 : (nd xs = S (length xs))
          h1 = hd
      in absurd (ltSelfAbsurd (length xs)
                   (replace {p = \n => lteNat n (length xs) = True} h1 (ndLeLength xs)))
  _ | False =
      -- nd (x::xs) = S (nd xs) = S (length xs), so nd xs = length xs.
      (Refl, succInj (nd xs) (length xs) hd)

total
distinctConsFalse : (x : Char) -> (xs : List Char) -> decElem x xs = False -> Distinct xs ->
                    Distinct (x :: xs)
distinctConsFalse x xs he hd =
  -- nd (x::xs) = ndStep (decElem x xs) (nd xs) = ndStep False (nd xs) = S (nd xs) = S (length xs).
  rewrite he in cong S hd

-- appending a NEW char to a distinct list keeps it distinct
total
distinctSnoc : (c : Char) -> (xs : List Char) -> decElem c xs = False -> Distinct xs ->
               Distinct (xs ++ [c])
distinctSnoc c xs hc hd =
  -- nd (xs++[c]) = S (nd xs) = S (length xs) = length (xs++[c]).
  trans (ndSnocF c xs hc) (trans (cong S hd) (sym (lengthSnoc xs c)))

-- if (xs ++ [c]) is distinct then c was new in xs (Lean `distinct_snoc_decElem`)
total
distinctSnocDecElem : (c : Char) -> (xs : List Char) -> Distinct (xs ++ [c]) ->
                      decElem c xs = False
distinctSnocDecElem c xs h with (decElem c xs) proof eq
  _ | False = Refl
  _ | True  =
      -- nd (xs++[c]) = nd xs (= length(xs++[c]) = S(length xs)); nd xs <= length xs, contra.
      let h1 : (nd xs = S (length xs))
          h1 = trans (sym (ndSnocT c xs eq)) (trans h (lengthSnoc xs c))
      in absurd (ltSelfAbsurd (length xs)
                   (replace {p = \n => lteNat n (length xs) = True} h1 (ndLeLength xs)))

total
suffixNilEq : {sf : List a} -> Suffix sf [] -> sf = []
suffixNilEq SuffNil = Refl

-- a suffix of (xs ++ [c]) is either empty or sf0 ++ [c] with sf0 a suffix of xs.
total
suffixSnocDecomp : (xs : List Char) -> {c : Char} -> {sf : List Char} ->
                   Suffix sf (xs ++ [c]) ->
                   Either (sf = []) (sf0 : List Char ** (sf = sf0 ++ [c], Suffix sf0 xs))
suffixSnocDecomp [] suf =
  -- suf : Suffix sf [c].  Either sf = [c] = [] ++ [c] or sf = [].
  case suffixConsInv suf of
    Left  eqsf => Right ([] ** (eqsf, SuffNil))
    Right hsfx => Left (suffixNilEq hsfx)
suffixSnocDecomp (y :: ys) suf =
  -- suf : Suffix sf (y :: (ys ++ [c]))
  case suffixConsInv suf of
    Left  eqsf => Right (y :: ys ** (eqsf, SuffNil))
    Right hsfx => case suffixSnocDecomp ys hsfx of
                    Left  e0 => Left e0
                    Right (sf0 ** (eqsf0, suf0)) => Right (sf0 ** (eqsf0, SuffCons y suf0))

------------------------------------------------------------------------
-- The window invariant: `win` is the longest all-distinct suffix of the
-- consumed prefix `P` (Lean `IsLVS`).  Fields are explicit so nothing erases.
------------------------------------------------------------------------

data IsLVS : List Char -> List Char -> Type where
  MkLVS : (win, p : List Char) ->
          Suffix win p ->
          Distinct win ->
          ((sf : List Char) -> Suffix sf p -> Distinct sf -> Suffix sf win) ->
          IsLVS win p

total
lvsInit : IsLVS [] []
lvsInit = MkLVS [] [] SuffNil distinctNil
            (\sf, hsf, _ => rewrite suffixNilEq hsf in SuffNil)

-- the window invariant is preserved by consuming one more char (Lean `lvs_step`)
total
lvsStep : (win, p : List Char) -> (c : Char) -> IsLVS win p ->
          IsLVS (shrinkV (win ++ [c])) (p ++ [c])
lvsStep win p c (MkLVS win p hwP hwv hall) =
  MkLVS (shrinkV (win ++ [c])) (p ++ [c])
        (suffixTrans (shrinkVSuffix (win ++ [c])) (suffixSnoc c hwP))
        (shrinkVValid (win ++ [c]))
        longestStep
  where
    longestStep : (sf : List Char) -> Suffix sf (p ++ [c]) -> Distinct sf ->
                  Suffix sf (shrinkV (win ++ [c]))
    longestStep sf hsf hv =
      case suffixSnocDecomp p hsf of
        Left  isNil => rewrite isNil in suffixNil (shrinkV (win ++ [c]))
        Right (sf0 ** (eqsf, hsf0)) =>
          -- sf = sf0 ++ [c], sf0 <:+ p, Distinct sf0; so sf0 <:+ win, hence sf0++[c] <:+ win++[c].
          let hv0   : Distinct sf0
              hv0   = distinctInit sf0 c (replace {p = \l => Distinct l} eqsf hv)
              hwin0 : Suffix sf0 win
              hwin0 = hall sf0 hsf0 hv0
              hsfwc : Suffix (sf0 ++ [c]) (win ++ [c])
              hsfwc = suffixSnoc c hwin0
              hvsf  : Distinct (sf0 ++ [c])
              hvsf  = replace {p = \l => Distinct l} eqsf hv
          in rewrite eqsf in shrinkVLongest (win ++ [c]) hvsf hsfwc

------------------------------------------------------------------------
-- The verified structural sliding window `goV` and `lengthOfLongestV`.
------------------------------------------------------------------------

total
goV : Nat -> List Char -> List Char -> Nat
goV best win []        = best
goV best win (c :: cs) =
  goV (maxN best (length (shrinkV (win ++ [c])))) (shrinkV (win ++ [c])) cs

total
lengthOfLongestV : String -> Nat
lengthOfLongestV s = goV 0 [] (unpack s)

-- goV only ever grows the running best (Lean `goV_mono`)
total
goVMono : (rem : List Char) -> (best : Nat) -> (win : List Char) ->
          lteNat best (goV best win rem) = True
goVMono []        best win = lteRefl best
goVMono (c :: cs) best win =
  lteTrans best (maxN best (length (shrinkV (win ++ [c]))))
           (goV (maxN best (length (shrinkV (win ++ [c])))) (shrinkV (win ++ [c])) cs)
           (leMaxL best (length (shrinkV (win ++ [c]))))
           (goVMono cs (maxN best (length (shrinkV (win ++ [c])))) (shrinkV (win ++ [c])))

------------------------------------------------------------------------
-- LOWER BOUND: the window's answer >= the length of every distinct substring.
-- Generalised over the running state by induction on `rem` (Lean `goVOpt`).
------------------------------------------------------------------------

-- cons injectivity (for peeling the consumed prefix head)
total
consHeadInj : {a, c : Char} -> {l, r : List Char} -> (a :: l = c :: r) -> a = c
consHeadInj Refl = Refl

total
consTailInj : {a, c : Char} -> {l, r : List Char} -> (a :: l = c :: r) -> l = r
consTailInj Refl = Refl

-- pre ++ suf = [] forces pre = []
total
appendNilLeft : (pre, suf : List Char) -> pre ++ suf = [] -> pre = []
appendNilLeft []        suf _ = Refl
appendNilLeft (_ :: _)  suf e = case e of Refl impossible

total
goVOpt : (rem : List Char) -> (win : List Char) -> (best : Nat) -> (p : List Char) ->
         IsLVS win p -> lteNat (length win) best = True ->
         (sub, pre, suf : List Char) -> rem = pre ++ suf ->
         Suffix sub (p ++ pre) -> Distinct sub ->
         lteNat (length sub) (goV best win rem) = True
goVOpt [] win best p (MkLVS win p _ _ hall) hwb sub pre suf hrem hsub hsv =
  -- rem = [] = pre ++ suf forces pre = []; so sub <:+ p, and length win bounds best.
  let preNil : (pre = [])
      preNil = appendNilLeft pre suf (sym hrem)
      hsubP : Suffix sub p
      hsubP = replace {p = \l => Suffix sub l} (appendNilR p)
                (replace {p = \l => Suffix sub (p ++ l)} preNil hsub)
  in lteTrans (length sub) (length win) best
              (suffixLength (hall sub hsubP hsv)) hwb
goVOpt (c :: cs) win best p hinv hwb sub pre suf hrem hsub hsv =
  case pre of
    [] =>
      -- sub sits entirely within consumed `p`; bound by win, then best, then goVMono.
      let hsubP : Suffix sub p
          hsubP = replace {p = \l => Suffix sub l} (appendNilR p) hsub
          hbound : lteNat (length sub) best = True
          hbound = case hinv of
                     MkLVS win p _ _ hall =>
                       lteTrans (length sub) (length win) best
                                (suffixLength (hall sub hsubP hsv)) hwb
      in lteTrans (length sub) best (goV best win (c :: cs))
                  hbound (goVMono (c :: cs) best win)
    (a :: pre') =>
      -- c :: cs = (a :: pre') ++ suf = a :: (pre' ++ suf); peel the head (a = c).
      let hac  : (a = c)
          hac  = sym (consHeadInj hrem)
          hcs  : (cs = pre' ++ suf)
          hcs  = consTailInj hrem
          hinv' : IsLVS (shrinkV (win ++ [c])) (p ++ [c])
          hinv' = lvsStep win p c hinv
          hwb'  : lteNat (length (shrinkV (win ++ [c]))) (maxN best (length (shrinkV (win ++ [c])))) = True
          hwb'  = leMaxR best (length (shrinkV (win ++ [c])))
          -- sub <:+ p ++ (a :: pre') = p ++ (c :: pre') = (p ++ [c]) ++ pre'.
          hsubC : Suffix sub (p ++ (c :: pre'))
          hsubC = replace {p = \w => Suffix sub (p ++ (w :: pre'))} hac hsub
          hsub' : Suffix sub ((p ++ [c]) ++ pre')
          hsub' = replace {p = \l => Suffix sub l} (sym (appendAssoc p [c] pre')) hsubC
      in goVOpt cs (shrinkV (win ++ [c])) (maxN best (length (shrinkV (win ++ [c])))) (p ++ [c])
                hinv' hwb' sub pre' suf hcs hsub' hsv

------------------------------------------------------------------------
-- OPTIMALITY (upper bound on distinct substrings): every all-distinct
-- substring of `s` is no longer than `lengthOfLongestV s` (Lean
-- `window_optimal`).  Instantiate goVOpt with the initial invariant.
------------------------------------------------------------------------

total
windowOptimal : (s : String) -> (sub : List Char) ->
                Infix sub (unpack s) -> Distinct sub ->
                lteNat (length sub) (lengthOfLongestV s) = True
windowOptimal s sub (before ** after ** hb) hv =
  -- hb : before ++ (sub ++ after) = unpack s.  Use pre = before ++ sub, suf = after.
  -- goVOpt wants Suffix sub ([] ++ (before ++ sub)), i.e. Suffix sub (before ++ sub).
  let hsubBS : Suffix sub (before ++ sub)
      hsubBS = suffixOfApp before sub
      hrem : (unpack s = (before ++ sub) ++ after)
      hrem = trans (sym hb) (sym (appendAssoc before sub after))
  in goVOpt (unpack s) [] 0 [] lvsInit (lteRefl 0) sub (before ++ sub) after hrem
            hsubBS hv
  where
    -- sub is a suffix of (pre ++ sub)
    suffixOfApp : (pre, sub : List Char) -> Suffix sub (pre ++ sub)
    suffixOfApp []        sub = SuffNil
    suffixOfApp (x :: xs) sub = SuffCons x (suffixOfApp xs sub)

------------------------------------------------------------------------
-- REALIZABILITY (lower bound witness): `lengthOfLongestV s` is the length of
-- an ACTUAL all-distinct substring (Lean `window_realizable`/`goVReal`).
------------------------------------------------------------------------

-- a witness that some distinct substring of `full` has length `n`
RealAt : List Char -> Nat -> Type
RealAt full n = (sub : List Char ** (Infix sub full, Distinct sub, length sub = n))

total
goVReal : (full : List Char) -> (rem : List Char) ->
          (win : List Char) -> (best : Nat) -> (p : List Char) ->
          p ++ rem = full -> Suffix win p -> Distinct win ->
          RealAt full best ->
          RealAt full (goV best win rem)
goVReal full []        win best p hPrem hwP _    hreal = hreal
goVReal full (c :: cs) win best p hPrem hwP _    hreal =
  -- (p ++ [c]) ++ cs = full
  let hP'full : ((p ++ [c]) ++ cs = full)
      hP'full = trans (appendAssoc p [c] cs) hPrem
      hwP' : Suffix (shrinkV (win ++ [c])) (p ++ [c])
      hwP' = suffixTrans (shrinkVSuffix (win ++ [c])) (suffixSnoc c hwP)
      hwinf : Infix (shrinkV (win ++ [c])) full
      hwinf = suffixPrefixInfix hwP' cs full hP'full
      hwv : Distinct (shrinkV (win ++ [c]))
      hwv = shrinkVValid (win ++ [c])
      -- the new best = maxN best (length (shrinkV (win++[c]))) is still realized
      hreal' : RealAt full (maxN best (length (shrinkV (win ++ [c]))))
      hreal' = case maxIs best (length (shrinkV (win ++ [c]))) of
                 Left  pEq =>  -- max = best
                   let (sub ** (hi, hd, hl)) = hreal
                   in (sub ** (hi, hd, trans hl (sym pEq)))
                 Right pEq =>  -- max = length (shrinkV ...)
                   (shrinkV (win ++ [c]) ** (hwinf, hwv, sym pEq))
  in goVReal full cs (shrinkV (win ++ [c])) (maxN best (length (shrinkV (win ++ [c]))))
             (p ++ [c]) hP'full hwP' hwv hreal'

total
windowRealizable : (s : String) -> RealAt (unpack s) (lengthOfLongestV s)
windowRealizable s =
  goVReal (unpack s) (unpack s) [] 0 [] Refl SuffNil distinctNil
          ([] ** (([] ** unpack s ** Refl), distinctNil, Refl))

------------------------------------------------------------------------
-- `lengthOfLongestV s` is EXACTLY the longest all-distinct substring length.
------------------------------------------------------------------------

total
lengthOfLongestVOptimal : (s : String) ->
  ( RealAt (unpack s) (lengthOfLongestV s)
  , (sub : List Char) -> Infix sub (unpack s) -> Distinct sub ->
      lteNat (length sub) (lengthOfLongestV s) = True )
lengthOfLongestVOptimal s =
  (windowRealizable s, \sub, hi, hv => windowOptimal s sub hi hv)

------------------------------------------------------------------------
-- HALF 2.  EQUIVALENCE: the fast last-seen-index algorithm `lengthOfLongest`
-- (the shipped O(n) solution) computes the same number as the verified
-- shrink window.  We port the Lean `goFast`<->`goV` simulation.
------------------------------------------------------------------------

-- STRICT if/then/else on Bool (no Lazy) so the assoc-list ops reduce cleanly
-- inside proofs (the builtin `if` wraps both branches in `Delay`, which trips
-- up propositional-equality unification).
total
ite : Bool -> a -> a -> a
ite True  x _ = x
ite False _ y = y

-- last-seen index per char as an assoc list
total
setIdx : Char -> Nat -> List (Char, Nat) -> List (Char, Nat)
setIdx c i []               = [(c, i)]
setIdx c i ((d, j) :: rest) =
  ite (ceq c d) ((c, i) :: rest) ((d, j) :: setIdx c i rest)

total
lookupIdx : Char -> List (Char, Nat) -> Maybe Nat
lookupIdx c []               = Nothing
lookupIdx c ((d, j) :: rest) = ite (ceq c d) (Just j) (lookupIdx c rest)

-- on seeing `c`, jump the window start past its previous index (named so the
-- proof can reason about it; `goFast` below is unchanged operationally).
total
startJump : Nat -> Maybe Nat -> Nat
startJump start (Just j) = maxN start (S j)
startJump start Nothing  = start

-- the fast sliding window: `start'` jumps past a repeat's previous index.
total
goFast : Nat -> Nat -> List (Char, Nat) -> Nat -> List Char -> Nat
goFast i start seen best []        = best
goFast i start seen best (c :: cs) =
  let start' = startJump start (lookupIdx c seen)
  in goFast (S i) start' (setIdx c i seen) (maxN best (minus (S i) start')) cs

total
lengthOfLongest : String -> Nat
lengthOfLongest s = goFast 0 0 [] 0 (unpack s)

------------------------------------------------------------------------
-- The functional-update lemma (Lean `lookupIdx_setIdx`).
------------------------------------------------------------------------

-- ite reductions (strict, so these are just the defining clauses)
total
iteTrue : (b : Bool) -> b = True -> (x, y : a) -> ite b x y = x
iteTrue True  _   x y = Refl
iteTrue False prf x y = absurd prf

total
iteFalse : (b : Bool) -> b = False -> (x, y : a) -> ite b x y = y
iteFalse False _   x y = Refl
iteFalse True  prf x y = absurd prf

-- nested ite collapse on one shared guard
total
collapseIte : (b : Bool) -> (x, y, r : a) -> ite b x r = ite b x (ite b y r)
collapseIte True  x y r = Refl
collapseIte False x y r = Refl

total
ceqTrue : (a, b : Char) -> ceq a b = True -> a = b
ceqTrue a b prf with (decEq a b)
  _ | Yes p = p
  _ | No _  = absurd prf

total
ceqRefl : (a : Char) -> ceq a a = True
ceqRefl a with (decEq a a)
  _ | Yes _ = Refl
  _ | No q  = absurd (q Refl)

-- swap two ites whose guards can't both hold (since ceq c e = False)
total
iteSwap : (d, e, c : Char) -> (j, i : a) -> (r : a) -> ceq c e = False ->
          ite (ceq d e) j (ite (ceq d c) i r) = ite (ceq d c) i (ite (ceq d e) j r)
iteSwap d e c j i r cce with (ceq d e) proof cde
  _ | False with (ceq d c) proof cdc
    _ | False = Refl
    _ | True  = Refl
  _ | True with (ceq d c) proof cdc
    _ | False = Refl
    _ | True  =
        let cEqE : (c = e) = trans (sym (ceqTrue d c cdc)) (ceqTrue d e cde)
        in absurd (trans (sym cce) (rewrite cEqE in ceqRefl e))

total
lookupIdxSetIdx : (c, d : Char) -> (i : Nat) -> (seen : List (Char, Nat)) ->
                  lookupIdx d (setIdx c i seen) = ite (ceq d c) (Just i) (lookupIdx d seen)
lookupIdxSetIdx c d i [] = Refl
lookupIdxSetIdx c d i ((e, j) :: rest) with (ceq c e) proof cce
  _ | True =
      rewrite sym (ceqTrue c e cce) in
      collapseIte (ceq d c) (Just i) (Just j) (lookupIdx d rest)
  _ | False =
      rewrite lookupIdxSetIdx c d i rest in
      iteSwap d e c (Just j) (Just i) (lookupIdx d rest) cce

------------------------------------------------------------------------
-- last-occurrence index, and its lemmas (Lean `lastIdx*`).  Defined via a
-- named `lastIdxStep` (NOT an inline `case`) so it reduces definitionally.
------------------------------------------------------------------------

total
justInj : {a, b : Nat} -> Just a = Just b -> a = b
justInj Refl = Refl

total
lastIdxStep : Char -> Char -> Maybe Nat -> Maybe Nat
lastIdxStep c x (Just j) = Just (S j)
lastIdxStep c x Nothing  = ite (ceq c x) (Just 0) Nothing

total
lastIdx : Char -> List Char -> Maybe Nat
lastIdx c []        = Nothing
lastIdx c (x :: xs) = lastIdxStep c x (lastIdx c xs)

total
lastIdxLt : (c : Char) -> (xs : List Char) -> (j : Nat) ->
            lastIdx c xs = Just j -> lteNat (S j) (length xs) = True
lastIdxLt c []        j prf = absurd prf
lastIdxLt c (x :: xs) j prf with (lastIdx c xs) proof hl
  lastIdxLt c (x :: xs) j prf | Just k = rewrite sym (justInj prf) in lastIdxLt c xs k hl
  lastIdxLt c (x :: xs) j prf | Nothing with (ceq c x)
    lastIdxLt c (x :: xs) j prf | Nothing | True  = rewrite sym (justInj prf) in Refl
    lastIdxLt c (x :: xs) j prf | Nothing | False = absurd prf

total
lastIdxSnocSelf : (c : Char) -> (p : List Char) -> lastIdx c (p ++ [c]) = Just (length p)
lastIdxSnocSelf c []        = rewrite ceqRefl c in Refl
lastIdxSnocSelf c (x :: xs) = rewrite lastIdxSnocSelf c xs in Refl

total
lastIdxSnocOther : (c, d : Char) -> (p : List Char) -> ceq d c = False ->
                   lastIdx d (p ++ [c]) = lastIdx d p
lastIdxSnocOther c d []        dc = rewrite dc in Refl
lastIdxSnocOther c d (x :: xs) dc = rewrite lastIdxSnocOther c d xs dc in Refl

total
maybeS : Maybe Nat -> Nat
maybeS (Just j) = S j
maybeS Nothing  = 0

total
dropAmt : List Char -> Char -> Nat
dropAmt win c = maybeS (lastIdx c win)

------------------------------------------------------------------------
-- shrinkV idempotence (Lean `shrinkV_idem`), via suffix antisymmetry.
------------------------------------------------------------------------

total
lteAntisym : (m, n : Nat) -> lteNat m n = True -> lteNat n m = True -> m = n
lteAntisym Z     Z     _  _  = Refl
lteAntisym Z     (S k) _  p2 = absurd p2
lteAntisym (S k) Z     p1 _  = absurd p1
lteAntisym (S k) (S j) p1 p2 = cong S (lteAntisym k j p1 p2)

-- a suffix of equal length is the whole list
total
suffixEqLen : (a, b : List Char) -> Suffix a b -> length a = length b -> a = b
suffixEqLen a a SuffNil _ = Refl
suffixEqLen a (x :: w) (SuffCons x p) prf =
  void (ltSelfAbsurd (length w)
          (replace {p = \n => lteNat n (length w) = True} prf (suffixLength p)))

total
suffixAntisym : (a, b : List Char) -> Suffix a b -> Suffix b a -> a = b
suffixAntisym a b sab sba =
  suffixEqLen a b sab
    (lteAntisym (length a) (length b) (suffixLength sab) (suffixLength sba))

total
shrinkVIdem : (p : List Char) -> (c : Char) ->
              shrinkV (shrinkV p ++ [c]) = shrinkV (p ++ [c])
shrinkVIdem p c = suffixAntisym _ _ h1 h2
  where
    h1 : Suffix (shrinkV (shrinkV p ++ [c])) (shrinkV (p ++ [c]))
    h1 = shrinkVLongest (p ++ [c]) (shrinkVValid (shrinkV p ++ [c]))
           (suffixTrans (shrinkVSuffix (shrinkV p ++ [c])) (suffixSnoc c (shrinkVSuffix p)))
    inner : Suffix (shrinkV (p ++ [c])) (shrinkV p ++ [c])
    inner = case suffixSnocDecomp p (shrinkVSuffix (p ++ [c])) of
              Left isNil =>
                replace {p = \z => Suffix z (shrinkV p ++ [c])}
                        (sym isNil) (suffixNil (shrinkV p ++ [c]))
              Right (sf0 ** (eqsf, hsf0)) =>
                let hd0 : Distinct sf0
                    hd0 = distinctInit sf0 c
                            (replace {p = \l => Distinct l} eqsf (shrinkVValid (p ++ [c])))
                in rewrite eqsf in suffixSnoc c (shrinkVLongest p hd0 hsf0)
    h2 : Suffix (shrinkV (p ++ [c])) (shrinkV (shrinkV p ++ [c]))
    h2 = shrinkVLongest (shrinkV p ++ [c]) (shrinkVValid (p ++ [c])) inner

------------------------------------------------------------------------
-- Bridge lemmas: `lastIdx c xs = Nothing` iff `decElem c xs = False`.
------------------------------------------------------------------------

total
orBFalse : (a, b : Bool) -> orB a b = False -> (a = False, b = False)
orBFalse False b prf = (Refl, prf)
orBFalse True  b prf = absurd prf

-- lastIdxStep on Nothing with a non-matching head is Nothing
total
lastIdxStepNoneI : (c, x : Char) -> ceq c x = False -> lastIdxStep c x Nothing = Nothing
lastIdxStepNoneI c x hcx = rewrite hcx in Refl

-- c absent from xs  =>  lastIdx finds nothing
total
lastIdxNoneI : (c : Char) -> (xs : List Char) -> decElem c xs = False -> lastIdx c xs = Nothing
lastIdxNoneI c []        prf = Refl
lastIdxNoneI c (x :: xs) prf =
  let (hcx, hrest) = orBFalse (ceq c x) (decElem c xs) (trans (sym (decElemCons c x xs)) prf)
  in trans (cong (lastIdxStep c x) (lastIdxNoneI c xs hrest)) (lastIdxStepNoneI c x hcx)

-- inversion of lastIdxStep yielding Nothing
total
lastIdxStepNone : (c, x : Char) -> (m : Maybe Nat) ->
                  lastIdxStep c x m = Nothing -> (m = Nothing, ceq c x = False)
lastIdxStepNone c x (Just k) prf = absurd prf
lastIdxStepNone c x Nothing  prf with (ceq c x)
  _ | True  = absurd prf
  _ | False = (Refl, Refl)

-- lastIdx finds nothing  =>  c absent from xs
total
decFalseOfLastIdxNone : (c : Char) -> (xs : List Char) ->
                        lastIdx c xs = Nothing -> decElem c xs = False
decFalseOfLastIdxNone c []        prf = Refl
decFalseOfLastIdxNone c (x :: xs) prf =
  let (hm, hcx) = lastIdxStepNone c x (lastIdx c xs) prf
  in rewrite decElemCons c x xs in rewrite hcx in decFalseOfLastIdxNone c xs hm

------------------------------------------------------------------------
-- dropAmt unfoldings + its cons-step (Lean `hstep` inside `shrinkV_snoc_drop`).
------------------------------------------------------------------------

total
dropAmtEq : (c : Char) -> (w : List Char) -> dropAmt w c = maybeS (lastIdx c w)
dropAmtEq c w = Refl

total
dropAmtConsEq : (c, x : Char) -> (w : List Char) ->
                dropAmt (x :: w) c = maybeS (lastIdxStep c x (lastIdx c w))
dropAmtConsEq c x w = Refl

-- with m = lastIdx c w: appending x in front bumps the drop amount by one.
-- The `Nothing & ceq c x = False` case is impossible (it would make x::(w++[c]) distinct).
total
dropAmtConsStepAux : (c, x : Char) -> (w : List Char) -> (m : Maybe Nat) ->
                     lastIdx c w = m -> decElem x w = False -> Distinct w ->
                     eqNat (nd (x :: (w ++ [c]))) (length (x :: (w ++ [c]))) = False ->
                     maybeS (lastIdxStep c x m) = S (maybeS m)
dropAmtConsStepAux c x w (Just k) hlm hxw hw hcond = Refl
dropAmtConsStepAux c x w Nothing  hlm hxw hw hcond with (ceq c x) proof hcx
  _ | True  = Refl
  _ | False =
      let hcw     : (decElem c w = False)
          hcw     = decFalseOfLastIdxNone c w hlm
          hxc     : (ceq x c = False)
          hxc     = trans (ceqSym x c) hcx
          decElemX : (decElem x (w ++ [c]) = False)
          decElemX = trans (decElemSnoc x c w)
                       (trans (cong (\b => orB b (ceq x c)) hxw) hxc)
          hDL     : (Distinct (x :: (w ++ [c])))
          hDL     = distinctConsFalse x (w ++ [c]) decElemX (distinctSnoc c w hcw hw)
          hTrue   : (eqNat (nd (x :: (w ++ [c]))) (length (x :: (w ++ [c]))) = True)
          hTrue   = replace {p = \n => eqNat n (length (x :: (w ++ [c]))) = True}
                      (sym hDL) (eqNatRefl (length (x :: (w ++ [c]))))
      in absurd (trans (sym hTrue) hcond)

total
dropAmtConsStep : (c, x : Char) -> (w : List Char) ->
                  decElem x w = False -> Distinct w ->
                  eqNat (nd (x :: (w ++ [c]))) (length (x :: (w ++ [c]))) = False ->
                  dropAmt (x :: w) c = S (dropAmt w c)
dropAmtConsStep c x w hxw hw hcond =
  trans (dropAmtConsEq c x w)
        (trans (dropAmtConsStepAux c x w (lastIdx c w) Refl hxw hw hcond)
               (cong S (sym (dropAmtEq c w))))

------------------------------------------------------------------------
-- THE CRUX (Lean `shrinkV_snoc_drop`): for a distinct window, appending c
-- and re-shrinking = dropping past c's last occurrence (0 if c is new).
-- The nested `with` on the guard reduces shrinkV in place (cf. shrinkVSuffix).
------------------------------------------------------------------------

total
shrinkVSnocDrop : (c : Char) -> (win : List Char) -> Distinct win ->
                  shrinkV (win ++ [c]) = drop (dropAmt win c) (win ++ [c])
shrinkVSnocDrop c []        hwin = Refl
shrinkVSnocDrop c (x :: w) hwin with (distinctConsTrue x w hwin)
  _ | (hxw, hw) with (eqNat (nd (x :: (w ++ [c]))) (length (x :: (w ++ [c])))) proof hcond
    _ | True =
        let hDL  : (Distinct (x :: (w ++ [c])))
            hDL  = eqNatTrue (nd (x :: (w ++ [c]))) (length (x :: (w ++ [c]))) hcond
            split : (decElem x (w ++ [c]) = False, Distinct (w ++ [c]))
            split = distinctConsTrue x (w ++ [c]) hDL
            hcwn : (lastIdx c w = Nothing)
            hcwn = lastIdxNoneI c w (distinctSnocDecElem c w (snd split))
            hxcF : (ceq x c = False)
            hxcF = snd (orBFalse (decElem x w) (ceq x c)
                         (trans (sym (decElemSnoc x c w)) (fst split)))
            hcx  : (ceq c x = False)
            hcx  = trans (ceqSym c x) hxcF
            hlxw : (lastIdx c (x :: w) = Nothing)
            hlxw = trans (cong (lastIdxStep c x) hcwn) (lastIdxStepNoneI c x hcx)
            hda  : (dropAmt (x :: w) c = 0)
            hda  = cong maybeS hlxw
        in rewrite hda in Refl
    _ | False = rewrite dropAmtConsStep c x w hxw hw hcond in shrinkVSnocDrop c w hw

------------------------------------------------------------------------
-- Arithmetic / list helpers for the simulation.
------------------------------------------------------------------------

total
plusZeroR : (n : Nat) -> n + 0 = n
plusZeroR Z     = Refl
plusZeroR (S k) = cong S (plusZeroR k)

total
plusSuccR : (a, b : Nat) -> a + S b = S (a + b)
plusSuccR Z     b = Refl
plusSuccR (S a) b = cong S (plusSuccR a b)

total
lteSelfPlus : (a, b : Nat) -> lteNat a (a + b) = True
lteSelfPlus Z     b = Refl
lteSelfPlus (S a) b = lteSelfPlus a b

total
takeDropAppend : (n : Nat) -> (l : List Char) -> take n l ++ drop n l = l
takeDropAppend Z     l        = Refl
takeDropAppend (S k) []       = Refl
takeDropAppend (S k) (x :: xs) = cong (x ::) (takeDropAppend k xs)

total
takeLength : (n : Nat) -> (l : List Char) -> lteNat n (length l) = True -> length (take n l) = n
takeLength Z     l        _   = Refl
takeLength (S k) []       prf = absurd prf
takeLength (S k) (x :: xs) prf = cong S (takeLength k xs prf)

total
dropAppendLe : (l1, l2 : List Char) -> (n : Nat) -> lteNat n (length l1) = True ->
               drop n (l1 ++ l2) = drop n l1 ++ l2
dropAppendLe []        l2 Z     _   = Refl
dropAppendLe []        l2 (S k) prf = absurd prf
dropAppendLe (x :: xs) l2 Z     _   = Refl
dropAppendLe (x :: xs) l2 (S m) prf = dropAppendLe xs l2 m prf

total
dropNil : (m : Nat) -> drop m (the (List Char) []) = []
dropNil Z     = Refl
dropNil (S k) = Refl

total
dropDrop : (m, n : Nat) -> (l : List Char) -> drop m (drop n l) = drop (n + m) l
dropDrop m Z     l         = Refl
dropDrop m (S k) []        = dropNil m
dropDrop m (S k) (x :: xs) = dropDrop m k xs

------------------------------------------------------------------------
-- `seen` records each char's last index in the consumed prefix (Lean SeenOk).
------------------------------------------------------------------------

SeenOk : List (Char, Nat) -> List Char -> Type
SeenOk seen p = (d : Char) -> lookupIdx d seen = lastIdx d p

total
seenOkStep : (seen : List (Char, Nat)) -> (p : List Char) -> (c : Char) -> (i : Nat) ->
             i = length p -> SeenOk seen p -> SeenOk (setIdx c i seen) (p ++ [c])
seenOkStep seen p c i hi h d with (ceq d c) proof hdc
  _ | True  = rewrite lookupIdxSetIdx c d i seen in
              rewrite hdc in
              rewrite ceqTrue d c hdc in
              rewrite lastIdxSnocSelf c p in
              rewrite hi in Refl
  _ | False = rewrite lookupIdxSetIdx c d i seen in
              rewrite hdc in
              rewrite lastIdxSnocOther c d p hdc in h d

------------------------------------------------------------------------
-- last-occurrence index in an append (Lean `lastIdx_append`).
------------------------------------------------------------------------

total
maybeShift : Nat -> Maybe Nat -> Maybe Nat -> Maybe Nat
maybeShift n (Just j) _ = Just (n + j)
maybeShift n Nothing  r = r

total
maybeShiftStep : (c, x : Char) -> (n : Nat) -> (my, mxs : Maybe Nat) ->
                 lastIdxStep c x (maybeShift n my mxs) = maybeShift (S n) my (lastIdxStep c x mxs)
maybeShiftStep c x n (Just j) mxs = Refl
maybeShiftStep c x n Nothing  mxs = Refl

total
lastIdxAppend : (c : Char) -> (xs, ys : List Char) ->
                lastIdx c (xs ++ ys) = maybeShift (length xs) (lastIdx c ys) (lastIdx c xs)
lastIdxAppend c [] ys with (lastIdx c ys)
  _ | Just j  = Refl
  _ | Nothing = Refl
lastIdxAppend c (x :: xs) ys =
  trans (cong (lastIdxStep c x) (lastIdxAppend c xs ys))
        (maybeShiftStep c x (length xs) (lastIdx c ys) (lastIdx c xs))

------------------------------------------------------------------------
-- the fast `start'` equals `start` plus how far the shrink window drops
-- (Lean `start_conn`).
------------------------------------------------------------------------

total
startConnAux : (start : Nat) -> (c : Char) -> (dl, tl : List Char) ->
               (m1 : Maybe Nat) -> lastIdx c dl = m1 ->
               (m2 : Maybe Nat) -> lastIdx c tl = m2 ->
               length tl = start ->
               startJump start (maybeShift start m1 m2) = start + maybeS m1
startConnAux start c dl tl (Just k) hm1 m2 hm2 htl =
  rewrite maxEqR start (S (start + k))
            (lteTrans start (start + k) (S (start + k))
              (lteSelfPlus start k) (lteSuccR (start + k))) in
  rewrite plusSuccR start k in Refl
startConnAux start c dl tl Nothing hm1 (Just j) hm2 htl =
  rewrite plusZeroR start in
  rewrite maxEqL start (S j)
            (replace {p = \n => lteNat (S j) n = True} htl (lastIdxLt c tl j hm2)) in Refl
startConnAux start c dl tl Nothing hm1 Nothing hm2 htl =
  rewrite plusZeroR start in Refl

total
startConn : (p : List Char) -> (start : Nat) -> (c : Char) ->
            lteNat start (length p) = True ->
            startJump start (lastIdx c p) = start + dropAmt (drop start p) c
startConn p start c hstart =
  let htl : (length (take start p) = start)
      htl = takeLength start p hstart
      keyP : (lastIdx c p = maybeShift start (lastIdx c (drop start p)) (lastIdx c (take start p)))
      keyP = trans (cong (lastIdx c) (sym (takeDropAppend start p)))
                   (trans (lastIdxAppend c (take start p) (drop start p))
                          (cong (\n => maybeShift n (lastIdx c (drop start p)) (lastIdx c (take start p)))
                                htl))
  in rewrite keyP in
     startConnAux start c (drop start p) (take start p)
       (lastIdx c (drop start p)) Refl (lastIdx c (take start p)) Refl htl

------------------------------------------------------------------------
-- the new window stays the longest distinct suffix (Lean `window_step`).
------------------------------------------------------------------------

total
windowStep : (p : List Char) -> (start : Nat) -> (c : Char) ->
             lteNat start (length p) = True -> shrinkV p = drop start p ->
             shrinkV (p ++ [c]) = drop (start + dropAmt (drop start p) c) (p ++ [c])
windowStep p start c hstart hwin =
  let hdist : (Distinct (drop start p))
      hdist = replace {p = \w => Distinct w} hwin (shrinkVValid p)
  in trans (sym (shrinkVIdem p c))
       (trans (cong (\w => shrinkV (w ++ [c])) hwin)
         (trans (shrinkVSnocDrop c (drop start p) hdist)
           (trans (cong (drop (dropAmt (drop start p) c)) (sym (dropAppendLe p [c] start hstart)))
                  (dropDrop (dropAmt (drop start p) c) start (p ++ [c])))))

------------------------------------------------------------------------
-- length-of-drop and the `start + dropAmt <= length` bound (Lean `hstart'`).
------------------------------------------------------------------------

total
minusZeroR : (n : Nat) -> minus n 0 = n
minusZeroR Z     = Refl
minusZeroR (S k) = Refl

total
lengthDrop : (n : Nat) -> (l : List Char) -> length (drop n l) = minus (length l) n
lengthDrop Z     l         = sym (minusZeroR (length l))
lengthDrop (S k) []        = Refl
lengthDrop (S k) (x :: xs) = lengthDrop k xs

-- s <= n  and  m <= n - s   =>   s + m <= n
total
addLeOfLeMinus : (s, m, n : Nat) -> lteNat s n = True -> lteNat m (minus n s) = True ->
                 lteNat (s + m) n = True
addLeOfLeMinus Z     m Z     hs hm = hm
addLeOfLeMinus Z     m (S n) hs hm = hm
addLeOfLeMinus (S k) m Z     hs hm = absurd hs
addLeOfLeMinus (S k) m (S n) hs hm = addLeOfLeMinus k m n hs hm

total
startBound : (p : List Char) -> (start : Nat) -> (c : Char) ->
             lteNat start (length p) = True ->
             (m : Maybe Nat) -> lastIdx c (drop start p) = m ->
             lteNat (start + maybeS m) (length p) = True
startBound p start c hstart (Just k) hm =
  addLeOfLeMinus start (S k) (length p) hstart
    (replace {p = \n => lteNat (S k) n = True} (lengthDrop start p)
       (lastIdxLt c (drop start p) k hm))
startBound p start c hstart Nothing hm = rewrite plusZeroR start in hstart

------------------------------------------------------------------------
-- THE SIMULATION (Lean `goFast_eq_goV`): goFast tracks the same window as goV.
------------------------------------------------------------------------

total
goFastEqGoV : (rem : List Char) -> (p : List Char) -> (i, start : Nat) ->
              (seen : List (Char, Nat)) -> (best : Nat) ->
              i = length p -> lteNat start (length p) = True ->
              shrinkV p = drop start p -> SeenOk seen p ->
              goFast i start seen best rem = goV best (shrinkV p) rem
goFastEqGoV []        p i start seen best hi hstart hwin hseen = Refl
goFastEqGoV (c :: cs) p i start seen best hi hstart hwin hseen =
  let start'' : Nat
      start'' = start + dropAmt (drop start p) c
      hsj : (startJump start (lookupIdx c seen) = start'')
      hsj = trans (cong (startJump start) (hseen c)) (startConn p start c hstart)
      hwin' : (shrinkV (p ++ [c]) = drop start'' (p ++ [c]))
      hwin' = windowStep p start c hstart hwin
      hlenR : (length (shrinkV (p ++ [c])) = minus (S i) start'')
      hlenR = trans (cong length hwin')
                (trans (lengthDrop start'' (p ++ [c]))
                  (cong (\n => minus n start'') (trans (lengthSnoc p c) (cong S (sym hi)))))
      hstart'' : (lteNat start'' (length (p ++ [c])) = True)
      hstart'' = lteTrans start'' (length p) (length (p ++ [c]))
                   (startBound p start c hstart (lastIdx c (drop start p)) Refl)
                   (replace {p = \n => lteNat (length p) n = True}
                      (sym (lengthSnoc p c)) (lteSuccR (length p)))
      ih : (goFast (S i) start'' (setIdx c i seen) (maxN best (minus (S i) start'')) cs
              = goV (maxN best (minus (S i) start'')) (shrinkV (p ++ [c])) cs)
      ih = goFastEqGoV cs (p ++ [c]) (S i) start'' (setIdx c i seen)
             (maxN best (minus (S i) start''))
             (trans (cong S hi) (sym (lengthSnoc p c)))
             hstart'' hwin' (seenOkStep seen p c i hi hseen)
  in rewrite hsj in rewrite shrinkVIdem p c in rewrite hlenR in ih

------------------------------------------------------------------------
-- EQUIVALENCE + PAYOFF: the shipped `lengthOfLongest` is exactly the longest
-- all-distinct substring length (optimality transferred via the equivalence).
------------------------------------------------------------------------

total
lengthOfLongestEq : (s : String) -> lengthOfLongest s = lengthOfLongestV s
lengthOfLongestEq s = goFastEqGoV (unpack s) [] 0 0 [] 0 Refl Refl Refl (\d => Refl)

total
lengthOfLongestOptimal : (s : String) ->
  ( RealAt (unpack s) (lengthOfLongest s)
  , (sub : List Char) -> Infix sub (unpack s) -> Distinct sub ->
      lteNat (length sub) (lengthOfLongest s) = True )
lengthOfLongestOptimal s = rewrite lengthOfLongestEq s in lengthOfLongestVOptimal s
