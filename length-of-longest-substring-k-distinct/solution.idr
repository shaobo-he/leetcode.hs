module Main

import Data.List       -- nub
import Data.List.Elem
import Data.Nat        -- LTE

------------------------------------------------------------------------
-- Fast solution: sliding window holding at most k distinct chars.
-- Helpers are top-level (not in a `where`) so we can reason about them below.
------------------------------------------------------------------------

-- structural max with clean reduction (the stdlib max is comparison-based)
maxN : Nat -> Nat -> Nat
maxN Z     b     = b
maxN a     Z     = a
maxN (S a) (S b) = S (maxN a b)

incr : Eq a => a -> List (a, Nat) -> List (a, Nat)
incr x [] = [(x, 1)]
incr x ((y, n) :: rest) = if x == y then (y, S n) :: rest else (y, n) :: incr x rest

decr : Eq a => a -> List (a, Nat) -> List (a, Nat)
decr x [] = []
decr x ((y, n) :: rest) =
  if x == y
    then (case n of
            S (S k) => (y, S k) :: rest
            _       => rest)
    else (y, n) :: decr x rest

shrinkW : Nat -> List Char -> List (Char, Nat) -> (List Char, List (Char, Nat))
shrinkW k win counts =
  if length counts > k
    then (case win of
            (d :: ds) => shrinkW k ds (decr d counts)
            []        => (win, counts))
    else (win, counts)

goW : Nat -> List Char -> List (Char, Nat) -> Nat -> List Char -> Nat
goW _ _ _ best [] = best
goW k win counts best (c :: cs) =
  let (win2, counts2) = shrinkW k (win ++ [c]) (incr c counts)
  in goW k win2 counts2 (maxN best (length win2)) cs

lenKDistinct : String -> Nat -> Nat
lenKDistinct s k = goW k [] [] 0 (unpack s)

------------------------------------------------------------------------
-- Verified brute-force reference with a full correctness proof:
--   optimal     : no substring with <= k distinct chars is longer than the answer
--   achievable  : the answer is realised by an actual valid substring
-- (The fast sliding window above is tested to agree with it.)
------------------------------------------------------------------------

lteRefl : {n : Nat} -> LTE n n
lteRefl {n = Z}   = LTEZero
lteRefl {n = S _} = LTESucc lteRefl

lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero     _           = LTEZero
lteTrans (LTESucc p) (LTESucc q) = LTESucc (lteTrans p q)

lteMaxL : (a, b : Nat) -> LTE a (maxN a b)
lteMaxL Z     b     = LTEZero
lteMaxL (S a) Z     = lteRefl
lteMaxL (S a) (S b) = LTESucc (lteMaxL a b)

lteMaxR : (a, b : Nat) -> LTE b (maxN a b)
lteMaxR Z     b     = lteRefl
lteMaxR (S a) Z     = LTEZero
lteMaxR (S a) (S b) = LTESucc (lteMaxR a b)

-- `sub` is a contiguous substring of `s`: a prefix of some suffix of `s`.
data Pre : List a -> List a -> Type where
  PreNil  : Pre [] ys
  PreCons : Pre xs ys -> Pre (x :: xs) (x :: ys)

data Substr : List a -> List a -> Type where
  IsPre : Pre sub s -> Substr sub s
  Skip  : Substr sub s -> Substr sub (x :: s)

-- all prefixes, and all contiguous substrings
prefixes : List a -> List (List a)
prefixes []        = [[]]
prefixes (x :: xs) = [] :: map (x ::) (prefixes xs)

substrs : List a -> List (List a)
substrs []        = [[]]
substrs (x :: xs) = prefixes (x :: xs) ++ substrs xs

-- generic Elem lemmas
mapElem : (f : a -> b) -> Elem x xs -> Elem (f x) (map f xs)
mapElem f Here      = Here
mapElem f (There e) = There (mapElem f e)

elemAppL : Elem x xs -> Elem x (xs ++ ys)
elemAppL Here      = Here
elemAppL (There e) = There (elemAppL e)

elemAppR : (xs : List a) -> Elem x ys -> Elem x (xs ++ ys)
elemAppR []        e = e
elemAppR (_ :: xs) e = There (elemAppR xs e)

nilInPrefixes : (s : List a) -> Elem [] (prefixes s)
nilInPrefixes []       = Here
nilInPrefixes (_ :: _) = Here

prefixElem : {s : List a} -> Pre sub s -> Elem sub (prefixes s)
prefixElem PreNil          = nilInPrefixes s
prefixElem (PreCons {x} p) = There (mapElem (x ::) (prefixElem p))

-- every contiguous substring appears in the enumeration
substrComplete : (s : List a) -> Substr sub s -> Elem sub (substrs s)
substrComplete []        (IsPre PreNil) = Here
substrComplete (x :: xs) (IsPre p)      = elemAppL (prefixElem p)
substrComplete (x :: xs) (Skip inf)     = elemAppR (prefixes (x :: xs)) (substrComplete xs inf)

filterElem : (p : a -> Bool) -> {xs : List a} -> Elem x xs -> p x = True -> Elem x (filter p xs)
filterElem p {xs = x :: rest} Here      prf = rewrite prf in Here
filterElem p {xs = y :: rest} (There e) prf with (p y)
  filterElem p {xs = y :: rest} (There e) prf | True  = There (filterElem p e prf)
  filterElem p {xs = y :: rest} (There e) prf | False = filterElem p e prf

listMax : List Nat -> Nat
listMax []        = 0
listMax (x :: xs) = maxN x (listMax xs)

listMaxUB : {n : Nat} -> (ns : List Nat) -> Elem n ns -> LTE n (listMax ns)
listMaxUB (m :: ms) Here      = lteMaxL m (listMax ms)
listMaxUB (m :: ms) (There e) = lteTrans (listMaxUB ms e) (lteMaxR m (listMax ms))

distinct : List Char -> Nat
distinct = length . nub

-- structural <= so that `validK k []` reduces definitionally to True
leNat : Nat -> Nat -> Bool
leNat Z     _     = True
leNat (S _) Z     = False
leNat (S a) (S b) = leNat a b

validK : Nat -> List Char -> Bool
validK k sub = leNat (distinct sub) k

solve : Nat -> List Char -> Nat
solve k s = listMax (map length (filter (validK k) (substrs s)))

-- OPTIMALITY: any substring with at most k distinct chars is no longer than
-- `solve k s`.  (So the answer never under-counts a valid window.)
optimal : (k : Nat) -> (s : List Char) -> (sub : List Char) ->
          Substr sub s -> validK k sub = True -> LTE (length sub) (solve k s)
optimal k s sub inf vprf =
  listMaxUB (map length (filter (validK k) (substrs s)))
            (mapElem length (filterElem (validK k) (substrComplete s inf) vprf))

-- ACHIEVABILITY: `solve k s` is actually realised by some valid substring.
-- Needs the reverse enumeration lemmas plus "the maximum is attained".

maxNElim : (a, b : Nat) -> Either (maxN a b = a) (maxN a b = b)
maxNElim Z     b     = Right Refl
maxNElim (S a) Z     = Left Refl
maxNElim (S a) (S b) = case maxNElim a b of
                         Left  p => Left  (cong S p)
                         Right p => Right (cong S p)

listMaxElem : (xs : List Nat) -> Either (listMax xs = 0) (Elem (listMax xs) xs)
listMaxElem []        = Left Refl
listMaxElem (x :: xs) = case maxNElim x (listMax xs) of
  Left  p => Right (rewrite p in Here)
  Right p => case listMaxElem xs of
               Left  q => Left  (trans p q)
               Right e => Right (rewrite p in There e)

mapElemInv : (f : a -> b) -> (xs : List a) -> Elem y (map f xs) ->
             (x : a ** (Elem x xs, f x = y))
mapElemInv f []        el        = absurd el
mapElemInv f (x :: xs) Here      = (x ** (Here, Refl))
mapElemInv f (x :: xs) (There e) = let (x' ** (e', q)) = mapElemInv f xs e in (x' ** (There e', q))

elemAppInv : (xs : List a) -> Elem z (xs ++ ys) -> Either (Elem z xs) (Elem z ys)
elemAppInv []        e         = Right e
elemAppInv (x :: xs) Here      = Left Here
elemAppInv (x :: xs) (There e) = case elemAppInv xs e of
                                   Left l  => Left (There l)
                                   Right r => Right r

filterElemInv : (p : a -> Bool) -> (xs : List a) -> Elem x (filter p xs) ->
                (Elem x xs, p x = True)
filterElemInv p []        el = absurd el
filterElemInv p (y :: ys) el with (p y) proof eq
  _ | True  = case el of
                Here        => (Here, eq)
                (There el') => let (i, px) = filterElemInv p ys el' in (There i, px)
  _ | False = let (i, px) = filterElemInv p ys el in (There i, px)

prefixSound : (s : List a) -> Elem sub (prefixes s) -> Pre sub s
prefixSound []        Here      = PreNil
prefixSound []        (There e) = absurd e
prefixSound (x :: xs) Here      = PreNil
prefixSound (x :: xs) (There e) =
  let (sub' ** (e', Refl)) = mapElemInv (x ::) (prefixes xs) e
  in PreCons (prefixSound xs e')

substrSound : (s : List a) -> Elem sub (substrs s) -> Substr sub s
substrSound []        Here      = IsPre PreNil
substrSound []        (There e) = absurd e
substrSound (x :: xs) e = case elemAppInv (prefixes (x :: xs)) e of
                            Left l  => IsPre (prefixSound (x :: xs) l)
                            Right r => Skip (substrSound xs r)

nilValid : (k : Nat) -> validK k [] = True
nilValid k = Refl

achievable : (k : Nat) -> (s : List Char) ->
             (sub : List Char ** (Substr sub s, validK k sub = True, length sub = solve k s))
achievable k s = case listMaxElem (map length (filter (validK k) (substrs s))) of
  Left  eq0 => ([] ** (IsPre PreNil, nilValid k, sym eq0))
  Right e   =>
    let (sub ** (eInFilter, lenEq)) = mapElemInv length (filter (validK k) (substrs s)) e
        (eInSubstrs, vprf)          = filterElemInv (validK k) (substrs s) eInFilter
    in (sub ** (substrSound s eInSubstrs, vprf, lenEq))

------------------------------------------------------------------------
-- A property of the ACTUAL sliding window: its answer is always the length
-- of a genuine contiguous substring of the input (never a fabricated number).
-- (Full correctness `= solve` additionally needs the counts-invariant, which
-- runs into primitive Char equality, and the left-pointer optimality argument.)
------------------------------------------------------------------------

prefixOfApp : (xs : List a) -> (ys : List a) -> Pre xs (xs ++ ys)
prefixOfApp []        ys = PreNil
prefixOfApp (x :: xs) ys = PreCons (prefixOfApp xs ys)

substrFromSplit : (before, mid, after, full : List Char) ->
                  before ++ mid ++ after = full -> Substr mid full
substrFromSplit []        mid after full eq = rewrite sym eq in IsPre (prefixOfApp mid after)
substrFromSplit (b :: bs) mid after full eq =
  rewrite sym eq in Skip (substrFromSplit bs mid after (bs ++ mid ++ after) Refl)

-- shrink only drops characters from the LEFT, so the window stays a suffix
shrinkSuffix : (k : Nat) -> (win : List Char) -> (counts : List (Char, Nat)) ->
               (win2 : List Char) -> (counts2 : List (Char, Nat)) ->
               shrinkW k win counts = (win2, counts2) ->
               (pre : List Char ** win = pre ++ win2)
shrinkSuffix k win counts win2 counts2 eq with (length counts > k)
  shrinkSuffix k []        counts win2 counts2 eq | True  = ([] ** cong fst eq)
  shrinkSuffix k (d :: ds) counts win2 counts2 eq | True  =
    let (pre' ** ih) = shrinkSuffix k ds (decr d counts) win2 counts2 eq
    in (d :: pre' ** cong (d ::) ih)
  shrinkSuffix k win       counts win2 counts2 eq | False = ([] ** cong fst eq)

-- the window stays positioned inside the input as it advances
splitStep : (before, win, pre, win2, cs : List Char) -> (c : Char) -> (full : List Char) ->
            before ++ win ++ (c :: cs) = full ->
            win ++ [c] = pre ++ win2 ->
            (before ++ pre) ++ win2 ++ cs = full
splitStep before win pre win2 cs c full split winEq =
  rewrite sym (appendAssociative before pre (win2 ++ cs)) in
  rewrite appendAssociative pre win2 cs in
  rewrite sym winEq in
  rewrite sym (appendAssociative win [c] cs) in
  split

goRealized : (k : Nat) -> (full : List Char) ->
             (win : List Char) -> (counts : List (Char, Nat)) -> (best : Nat) ->
             (remaining : List Char) ->
             (before : List Char ** before ++ win ++ remaining = full) ->
             (bsub : List Char ** (Substr bsub full, length bsub = best)) ->
             (sub : List Char ** (Substr sub full, length sub = goW k win counts best remaining))
goRealized k full win counts best []        inv1 inv2 = inv2
goRealized k full win counts best (c :: cs) inv1 inv2
   with (shrinkW k (win ++ [c]) (incr c counts)) proof eqs
  _ | (win2, counts2) =
    let (before ** split) = inv1
        (pre ** winEq)     = shrinkSuffix k (win ++ [c]) (incr c counts) win2 counts2 eqs
        split2             = splitStep before win pre win2 cs c full split winEq
        win2sub            = substrFromSplit (before ++ pre) win2 cs full split2
        inv2' = case maxNElim best (length win2) of
                  Left  p => let (bs ** (ss, le)) = inv2 in (bs ** (ss, trans le (sym p)))
                  Right p => (win2 ** (win2sub, sym p))
    in goRealized k full win2 counts2 (maxN best (length win2)) cs
                  (before ++ pre ** split2) inv2'

windowRealized : (s : String) -> (k : Nat) ->
                 (sub : List Char ** (Substr sub (unpack s), length sub = lenKDistinct s k))
windowRealized s k =
  goRealized k (unpack s) [] [] 0 (unpack s) ([] ** Refl) ([] ** (IsPre PreNil, Refl))

main : IO ()
main = do
  printLn (lenKDistinct "eceba" 2)    -- 3  (fast sliding window)
  printLn (lenKDistinct "aa" 1)       -- 2
  printLn (lenKDistinct "abee" 1)     -- 2
  -- verified brute-force agrees:
  printLn (solve 2 (unpack "eceba"))  -- 3
  printLn (solve 1 (unpack "aa"))     -- 2
  printLn (solve 1 (unpack "abee"))   -- 2
