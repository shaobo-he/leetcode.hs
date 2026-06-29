module Main

%default total

-- LeetCode 322: Coin Change — fewest coins summing to `amount`, or Nothing if
-- impossible.  This Idris take is the bottom-up DP, made TOTAL by *structure*:
-- `build` recurses on the Nat `amount`, growing a (reversed) DP table whose head
-- is dp[current]; every helper (`nth`, `omin`, `listMin`, `step`) is total, so
-- the whole pipeline type-checks with `%default total`.

-- min over a list of `Maybe Nat`, with `Nothing` standing for "unreachable".
omin : Maybe Nat -> Maybe Nat -> Maybe Nat
omin Nothing  y        = y
omin x        Nothing  = x
omin (Just a) (Just b) = Just (min a b)

listMin : List (Maybe Nat) -> Maybe Nat
listMin = foldr omin Nothing

-- total list indexing: `nth i xs = Just (xs !! i)` when in range, else Nothing.
nth : Nat -> List a -> Maybe a
nth _     []        = Nothing
nth Z     (x :: _)  = Just x
nth (S k) (_ :: xs) = nth k xs

-- `table` is the DP reversed: head = dp[a-1], …, last = dp[0] (length = a).
-- For a coin c (1 ≤ c ≤ a) the subproblem dp[a-c] sits at index c-1; a coin
-- with c-1 out of range is exactly c > a, so it is skipped.
step : List Nat -> List (Maybe Nat) -> Maybe Nat
step coins table = listMin (map cand coins)
  where
    cand : Nat -> Maybe Nat
    cand Z     = Nothing                       -- value-0 coins excluded
    cand (S k) = case nth k table of
                   Nothing    => Nothing        -- c > a: not yet available
                   Just dpval => map (+ 1) dpval

-- reversed DP table for amounts a, a-1, …, 0 (head = dp[a]).
build : List Nat -> Nat -> List (Maybe Nat)
build _     Z     = [Just 0]
build coins (S k) = let prev = build coins k in step coins prev :: prev

coinChange : List Nat -> Nat -> Maybe Nat
coinChange coins amount = case build coins amount of
                            (x :: _) => x
                            []       => Nothing   -- unreachable: build is nonempty

-- small worked examples double as compile-time checks (each a `total` proof);
-- larger amounts are exercised at runtime in `main` (unary-Nat normalisation of
-- big tables at type-check time is too slow to bake in as `Refl`).
ex1 : coinChange [1,2,5] 5 = Just 1
ex1 = Refl

ex2 : coinChange [2] 3 = Nothing
ex2 = Refl

ex3 : coinChange [1,2,5] 0 = Just 0
ex3 = Refl

ex4 : coinChange [1] 0 = Just 0
ex4 = Refl

ex5 : coinChange [1,2,5] 4 = Just 2
ex5 = Refl

main : IO ()
main = do
  printLn (coinChange [1,2,5] 11)        -- Just 3
  printLn (coinChange [2] 3)             -- Nothing
  printLn (coinChange [1,2,5] 0)         -- Just 0
  printLn (coinChange [1] 0)             -- Just 0
  printLn (coinChange [1] 2)             -- Just 2
  printLn (coinChange [2,5,10,1] 27)     -- Just 4
