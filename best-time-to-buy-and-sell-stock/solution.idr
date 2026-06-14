module Main

------------------------------------------------------------------------
-- LeetCode 121: Best Time to Buy and Sell Stock -- single pass tracking the
-- lowest price so far and the best profit, with an Idris 2 proof of
-- CORRECTNESS: `maxProfit prices` is *exactly* the best profit over all valid
-- trades (buy on some day, sell on a later-or-equal day).
--
--   * maxProfit_optimal     -- no trade beats the answer (s - b <= maxProfit);
--   * maxProfit_achievable  -- the answer is realised by an actual trade.
--
-- Port of the Lean proof in solution.lean.  We use Nat prices with monus
-- (`minus` truncates at 0): this is faithful, since a losing or same-day trade
-- has profit 0 and `maxProfit` is max(0, best real gain) -- exactly what monus
-- gives.  Structural `lteNat`/`minNat`/`maxNat` are used (NOT Ord's) so they
-- REDUCE inside proofs (Idris has no `omega`).
--
-- Every lemma/proof function is marked `total` and contains none of:
-- believe_me, really_believe_me, assert_total, assert_smaller, idris_crash,
-- postulate, or holes.  The whole file passes `idris2 --check --total`.
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Single pass: `lowest` = min price so far, `best` = best profit so far.
------------------------------------------------------------------------

total
lteNat : Nat -> Nat -> Bool
lteNat Z     _     = True
lteNat (S _) Z     = False
lteNat (S m) (S n) = lteNat m n

total
minNat : Nat -> Nat -> Nat
minNat Z     _     = Z
minNat (S _) Z     = Z
minNat (S a) (S b) = S (minNat a b)

total
maxNat : Nat -> Nat -> Nat
maxNat Z     b     = b
maxNat a     Z     = a
maxNat (S a) (S b) = S (maxNat a b)

total
go : (lowest, best : Nat) -> List Nat -> Nat
go lowest best []        = best
go lowest best (x :: xs) = go (minNat lowest x) (maxNat best (x `minus` lowest)) xs

total
maxProfit : List Nat -> Nat
maxProfit []        = 0
maxProfit (p :: ps) = go p 0 ps

------------------------------------------------------------------------
-- A tiny structural membership predicate (own `Elem`, so its eliminators are
-- exactly what the proofs need).  Carries the element explicitly.
------------------------------------------------------------------------

data Mem : Nat -> List Nat -> Type where
  MHere  : (x : Nat) -> Mem x (x :: xs)
  MThere : (y : Nat) -> Mem x ys -> Mem x (y :: ys)

total
memEmptyAbsurd : Mem x [] -> Void
memEmptyAbsurd m impossible

-- membership in a singleton forces equality
total
memSingleton : Mem p [x] -> p = x
memSingleton (MHere x)         = Refl
memSingleton (MThere y m)      = absurd (memEmptyAbsurd m)

-- membership distributes over ++ (the "or" direction we need)
total
memAppend : (pre : List Nat) -> Mem p (pre ++ ys) -> Either (Mem p pre) (Mem p ys)
memAppend []        m            = Right m
memAppend (q :: qs) (MHere q)    = Left (MHere q)
memAppend (q :: qs) (MThere q m) = case memAppend qs m of
                                     Left mpre => Left (MThere q mpre)
                                     Right mys => Right mys

total
memAppendLeft : Mem p pre -> Mem p (pre ++ ys)
memAppendLeft (MHere p)    = MHere p
memAppendLeft (MThere y m) = MThere y (memAppendLeft m)

total
memAppendRight : (pre : List Nat) -> Mem p ys -> Mem p (pre ++ ys)
memAppendRight []        m = m
memAppendRight (q :: qs) m = MThere q (memAppendRight qs m)

------------------------------------------------------------------------
-- A valid trade in `prices`: buy at `b`, sell at `s` on a later-or-equal day.
--   Here  : buy at the head, sell at any element from the head onward;
--   There : the whole trade happens later in the list.
-- The buy/sell/head values are EXPLICIT fields (not just indices) so matching
-- gives them NON-erased -- we use them at runtime in `lteNat`/`minNat`.
------------------------------------------------------------------------

data Trade : List Nat -> Nat -> Nat -> Type where
  Here  : (b : Nat) -> (s : Nat) -> Mem s (b :: rest) -> Trade (b :: rest) b s
  There : (x : Nat) -> Trade rest b s -> Trade (x :: rest) b s

total
tradeEmptyAbsurd : Trade [] b s -> Void
tradeEmptyAbsurd t impossible

------------------------------------------------------------------------
-- Arithmetic lemmas (short structural inductions on Nat; all total).
------------------------------------------------------------------------

total
lteRefl : (n : Nat) -> lteNat n n = True
lteRefl Z     = Refl
lteRefl (S m) = lteRefl m

total
lteTrans : (a, b, c : Nat) -> lteNat a b = True -> lteNat b c = True -> lteNat a c = True
lteTrans Z     b     c     _   _   = Refl
lteTrans (S a) (S b) (S c) p   q   = lteTrans a b c p q
lteTrans (S a) Z     c     p   _   = absurd p
lteTrans (S a) (S b) Z     _   q   = absurd q

total
zeroLte : (n : Nat) -> lteNat 0 n = True
zeroLte n = Refl

-- min is below each argument
total
minLe1 : (a, b : Nat) -> lteNat (minNat a b) a = True
minLe1 Z     b     = Refl
minLe1 (S a) Z     = Refl
minLe1 (S a) (S b) = minLe1 a b

total
minLe2 : (a, b : Nat) -> lteNat (minNat a b) b = True
minLe2 Z     b     = Refl
minLe2 (S a) Z     = Refl
minLe2 (S a) (S b) = minLe2 a b

-- minNat is one of its arguments
total
minIs : (a, b : Nat) -> Either (minNat a b = a) (minNat a b = b)
minIs Z     b     = Left Refl
minIs (S a) Z     = Right Refl
minIs (S a) (S b) = case minIs a b of
                      Left p  => Left (cong S p)
                      Right p => Right (cong S p)

-- max is above each argument
total
leMax1 : (a, b : Nat) -> lteNat a (maxNat a b) = True
leMax1 Z     b     = Refl
leMax1 (S a) Z     = lteRefl (S a)
leMax1 (S a) (S b) = leMax1 a b

total
leMax2 : (a, b : Nat) -> lteNat b (maxNat a b) = True
leMax2 Z     b     = lteRefl b
leMax2 (S a) Z     = Refl
leMax2 (S a) (S b) = leMax2 a b

-- maxNat is one of its arguments
total
maxIs : (a, b : Nat) -> Either (maxNat a b = a) (maxNat a b = b)
maxIs Z     b     = Right Refl
maxIs (S a) Z     = Left Refl
maxIs (S a) (S b) = case maxIs a b of
                      Left p  => Left (cong S p)
                      Right p => Right (cong S p)

total
minusSelf : (n : Nat) -> n `minus` n = 0
minusSelf Z     = Refl
minusSelf (S m) = minusSelf m

total
lteSuccR : (n : Nat) -> lteNat n (S n) = True
lteSuccR Z     = Refl
lteSuccR (S m) = lteSuccR m

-- monus never grows: n - k <= n
total
minusLe : (n, k : Nat) -> lteNat (n `minus` k) n = True
minusLe Z     k     = Refl
minusLe (S n) Z     = lteRefl (S n)
minusLe (S n) (S k) = lteTrans (n `minus` k) n (S n) (minusLe n k) (lteSuccR n)

-- monus monotone in the subtrahend (smaller subtrahend => bigger result):
-- lo <= b  =>  s - b <= s - lo
total
minusMonoR : (s, lo, b : Nat) -> lteNat lo b = True -> lteNat (s `minus` b) (s `minus` lo) = True
minusMonoR Z     lo     b     _   = Refl
minusMonoR (S s) Z      b     _   = minusLe (S s) b
minusMonoR (S s) (S lo) Z     p   = absurd p
minusMonoR (S s) (S lo) (S b) p   = minusMonoR s lo b p

------------------------------------------------------------------------
-- `lowest` is the minimum of the (nonempty) consumed prefix.
------------------------------------------------------------------------

-- own right-neutral and associativity for ++ (kept total and self-contained).
total
appendNilRight : (l : List Nat) -> l ++ [] = l
appendNilRight []        = Refl
appendNilRight (x :: xs) = cong (x ::) (appendNilRight xs)

total
appendAssoc : (l, m, r : List Nat) -> (l ++ m) ++ r = l ++ (m ++ r)
appendAssoc []        m r = Refl
appendAssoc (x :: xs) m r = cong (x ::) (appendAssoc xs m r)

IsMin : (lowest : Nat) -> (pre : List Nat) -> Type
IsMin lowest pre = (Mem lowest pre, (p : Nat) -> Mem p pre -> lteNat lowest p = True)

------------------------------------------------------------------------
-- A trade in `pre ++ [x]` either lies inside `pre`, or sells at `x` (buying at
-- some earlier element of `pre`, or at `x` itself).  Mirrors Lean `trade_snoc`.
------------------------------------------------------------------------

total
tradeSnoc : (pre : List Nat) -> Trade (pre ++ [x]) b s ->
            Either (Trade pre b s) (s = x, Either (Mem b pre) (b = x))
tradeSnoc [] (Here b s mem) =
  -- list is [x]; Here forces b = x, and mem : Mem s [x] forces s = x.
  rewrite memSingleton mem in Right (Refl, Right Refl)
tradeSnoc [] (There x t) = absurd (tradeEmptyAbsurd t)
tradeSnoc (p :: pre') (Here p s mem) =
  -- b = p (the head).  mem : Mem s (p :: (pre' ++ [x])).
  case mem of
    MHere p          => Left (Here p p (MHere p))            -- s = p, inside pre
    MThere p mem'    =>
      case memAppend pre' mem' of
        Left  msPre  => Left (Here p s (MThere p msPre))
        Right msX    => rewrite memSingleton msX in
                          Right (Refl, Left (MHere p))       -- s = x, buy at head p
tradeSnoc (p :: pre') (There p t') =
  case tradeSnoc pre' t' of
    Left tpre'              => Left (There p tpre')
    Right (eqsx, Left mb)   => Right (eqsx, Left (MThere p mb))
    Right (eqsx, Right eqbx)=> Right (eqsx, Right eqbx)

------------------------------------------------------------------------
-- OPTIMALITY: no trade beats `go`'s answer.  Generalised over the running
-- state, by induction on the remaining list `xs`.  Mirrors Lean `go_opt`.
------------------------------------------------------------------------

total
goOpt : (xs : List Nat) -> (pre : List Nat) -> (lowest, best : Nat) ->
        IsMin lowest pre ->
        ((b, s : Nat) -> Trade pre b s -> lteNat (s `minus` b) best = True) ->
        (b, s : Nat) -> Trade (pre ++ xs) b s ->
        lteNat (s `minus` b) (Main.go lowest best xs) = True
goOpt [] pre lowest best _ hub b s ht =
  -- pre ++ [] = pre, so the trade is in `pre`; the bound `hub` finishes it.
  hub b s (replace {p = \l => Trade l b s} (appendNilRight pre) ht)
goOpt (x :: xs) pre lowest best hmin hub b s ht =
  -- step: lowest' = min lowest x, best' = max best (x - lowest).
  goOpt xs (pre ++ [x]) (minNat lowest x) (maxNat best (x `minus` lowest))
        hmin' hub' b s ht'
  where
    hmem  : Mem lowest pre
    hmem  = fst hmin
    hle   : (p : Nat) -> Mem p pre -> lteNat lowest p = True
    hle   = snd hmin

    -- the new lowest is in pre ++ [x] and is <= every element of it.
    hmin' : IsMin (minNat lowest x) (pre ++ [x])
    hmin' = (memNew, leNew)
      where
        memNew : Mem (minNat lowest x) (pre ++ [x])
        memNew = case minIs lowest x of
                   Left  p => rewrite p in memAppendLeft hmem
                   Right p => rewrite p in memAppendRight pre (MHere x)
        leNew  : (p : Nat) -> Mem p (pre ++ [x]) -> lteNat (minNat lowest x) p = True
        leNew p mp = case memAppend pre mp of
                       Left  mpre => lteTrans (minNat lowest x) lowest p
                                             (minLe1 lowest x) (hle p mpre)
                       Right mx   => rewrite memSingleton mx in minLe2 lowest x

    -- any trade in pre ++ [x] has profit <= the new best (max best (x - lowest)).
    hub' : (b', s' : Nat) -> Trade (pre ++ [x]) b' s' ->
           lteNat (s' `minus` b') (maxNat best (x `minus` lowest)) = True
    hub' b' s' ht'' =
      case tradeSnoc pre ht'' of
        Left tpre =>
          -- profit <= best <= new best.
          lteTrans (s' `minus` b') best (maxNat best (x `minus` lowest))
                   (hub b' s' tpre) (leMax1 best (x `minus` lowest))
        Right (eqsx, Left mbpre) =>
          -- s' = x, b' in pre so lowest <= b', hence x - b' <= x - lowest <= new.
          rewrite eqsx in
            lteTrans (x `minus` b') (x `minus` lowest) (maxNat best (x `minus` lowest))
                     (minusMonoR x lowest b' (hle b' mbpre))
                     (leMax2 best (x `minus` lowest))
        Right (eqsx, Right eqbx) =>
          -- s' = x = b', so profit x - x = 0 <= anything.
          rewrite eqsx in rewrite eqbx in
            rewrite minusSelf x in zeroLte (maxNat best (x `minus` lowest))

    -- re-associate (pre ++ [x]) ++ xs = pre ++ (x :: xs).
    ht' : Trade ((pre ++ [x]) ++ xs) b s
    ht' = rewrite appendAssoc pre [x] xs in ht

total
maxProfit_optimal : (prices : List Nat) -> (b, s : Nat) -> Trade prices b s ->
                    lteNat (s `minus` b) (Main.maxProfit prices) = True
maxProfit_optimal [] b s ht = absurd (tradeEmptyAbsurd ht)
maxProfit_optimal (p :: ps) b s ht =
  goOpt ps [p] p 0 hmin hub b s ht
  where
    hmin : IsMin p [p]
    hmin = (MHere p, \q, mq => rewrite memSingleton mq in lteRefl p)
    hub  : (b', s' : Nat) -> Trade [p] b' s' -> lteNat (s' `minus` b') 0 = True
    hub b' s' ht' =
      case ht' of
        Here p s' mem => rewrite memSingleton mem in     -- s' = p, b' = p
                           rewrite minusSelf p in Refl
        There p t     => absurd (tradeEmptyAbsurd t)

------------------------------------------------------------------------
-- ACHIEVABILITY: the answer is realised by an actual trade.
------------------------------------------------------------------------

-- a trade survives appending more days on the right.  Mirrors Lean `trade_append`.
total
tradeAppend : (ys : List Nat) -> Trade pre b s -> Trade (pre ++ ys) b s
tradeAppend ys (Here b s mem) =
  Here b s (case mem of
              MHere b      => MHere b
              MThere b m   => MThere b (memAppendLeft m))
tradeAppend ys (There x t) = There x (tradeAppend ys t)

-- buying at any earlier day `b` and selling at the appended day `x` is a trade.
-- Mirrors Lean `trade_mem_snoc`.
total
tradeMemSnoc : (pre : List Nat) -> (x : Nat) -> Mem b pre -> Trade (pre ++ [x]) b x
tradeMemSnoc (b :: rest) x (MHere b) =
  -- buy at head b, sell at the appended x (which is in b :: (rest ++ [x])).
  Here b x (MThere b (memAppendRight rest (MHere x)))
tradeMemSnoc (p :: rest) x (MThere p mb) = There p (tradeMemSnoc rest x mb)

-- ACHIEVABILITY generalised over the running state.  Mirrors Lean `go_ach`.
total
goAch : (xs : List Nat) -> (pre : List Nat) -> (lowest, best : Nat) ->
        IsMin lowest pre ->
        (b ** (s ** (Trade pre b s, s `minus` b = best))) ->
        (b ** (s ** (Trade (pre ++ xs) b s, s `minus` b = Main.go lowest best xs)))
goAch [] pre lowest best _ (b ** (s ** (ht, eq))) =
  (b ** (s ** (rewrite appendNilRight pre in ht, eq)))
goAch (x :: xs) pre lowest best hmin real =
  let key = goAch xs (pre ++ [x]) (minNat lowest x) (maxNat best (x `minus` lowest))
                  hmin' real'
  in rewrite sym (appendAssoc pre [x] xs) in key
  where
    hmem  : Mem lowest pre
    hmem  = fst hmin
    hle   : (p : Nat) -> Mem p pre -> lteNat lowest p = True
    hle   = snd hmin

    hmin' : IsMin (minNat lowest x) (pre ++ [x])
    hmin' = (memNew, leNew)
      where
        memNew : Mem (minNat lowest x) (pre ++ [x])
        memNew = case minIs lowest x of
                   Left  p => rewrite p in memAppendLeft hmem
                   Right p => rewrite p in memAppendRight pre (MHere x)
        leNew  : (p : Nat) -> Mem p (pre ++ [x]) -> lteNat (minNat lowest x) p = True
        leNew p mp = case memAppend pre mp of
                       Left  mpre => lteTrans (minNat lowest x) lowest p
                                             (minLe1 lowest x) (hle p mpre)
                       Right mx   => rewrite memSingleton mx in minLe2 lowest x

    -- a trade in pre ++ [x] realising the new best (max best (x - lowest)).
    real' : (b ** (s ** (Trade (pre ++ [x]) b s, s `minus` b = maxNat best (x `minus` lowest))))
    real' = case maxIs best (x `minus` lowest) of
              Left  pmax =>
                -- new best is the old best; reuse the existing trade, appended.
                let (b ** (s ** (ht, eq))) = real
                in (b ** (s ** (tradeAppend [x] ht, rewrite pmax in eq)))
              Right pmax =>
                -- new best is x - lowest; buy at lowest (in pre), sell at x.
                (lowest ** (x ** (tradeMemSnoc pre x hmem, rewrite pmax in Refl)))

total
maxProfit_achievable : (p : Nat) -> (ps : List Nat) ->
                       (b ** (s ** (Trade (p :: ps) b s, s `minus` b = Main.maxProfit (p :: ps))))
maxProfit_achievable p ps =
  goAch ps [p] p 0 hmin real
  where
    hmin : IsMin p [p]
    hmin = (MHere p, \q, mq => rewrite memSingleton mq in lteRefl p)
    real : (b ** (s ** (Trade [p] b s, s `minus` b = 0)))
    real = (p ** (p ** (Here p p (MHere p), minusSelf p)))

------------------------------------------------------------------------
-- `maxProfit` is EXACTLY the best achievable trade profit.
------------------------------------------------------------------------

total
maxProfit_correct : (p : Nat) -> (ps : List Nat) ->
  ( (b ** (s ** (Trade (p :: ps) b s, s `minus` b = Main.maxProfit (p :: ps))))
  , ((b, s : Nat) -> Trade (p :: ps) b s -> lteNat (s `minus` b) (Main.maxProfit (p :: ps)) = True))
maxProfit_correct p ps =
  (maxProfit_achievable p ps, \b, s => maxProfit_optimal (p :: ps) b s)

------------------------------------------------------------------------
-- Runnable examples (Nat prices, monus profit).
------------------------------------------------------------------------

main : IO ()
main = do
  printLn (maxProfit [7,1,5,3,6,4])   -- 5
  printLn (maxProfit [7,6,4,3,1])     -- 0
