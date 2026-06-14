module Main

import Data.Vect
import Data.Nat

-- LeetCode 88: Merge Sorted Array — two Idris takes on a merge.
--
--  (1) `sortedMerge` keeps the DEPENDENT LENGTH in the type: `Vect (m + n)`
--      certifies that merging an m-vector and an n-vector yields exactly
--      m + n elements, with no separate proof.
--
--  (2) `mergeL` / `mergeSorted` prove the ORDER is correct: merging two
--      SORTED lists yields a SORTED list, for any total + transitive
--      comparator.  All proof functions are `total`.  Mirrors the Lean
--      `merge-sorted-array/solution.lean`.

------------------------------------------------------------------------
-- (1) Dependent-length merge: the type already certifies the length.
------------------------------------------------------------------------

sortedMerge : Ord a => Vect m a -> Vect n a -> Vect (m + n) a
sortedMerge [] ys = ys
sortedMerge {m} xs [] = rewrite plusZeroRightNeutral m in xs
sortedMerge {m=(S i)} {n=(S j)} fst@(x :: xs) snd@(y :: ys) = if x < y
  then x :: (sortedMerge xs snd)
  else let res = y :: (sortedMerge fst ys) in
      rewrite sym (plusSuccRightSucc i j) in res

------------------------------------------------------------------------
-- (2) Verified-sorted merge: an explicit comparator `le` (so we can state
-- its laws), with a (total) proof that merging two SORTED lists is SORTED.
------------------------------------------------------------------------

total
mergeL : (le : a -> a -> Bool) -> List a -> List a -> List a
mergeL le [] ys = ys
mergeL le (x :: xs) [] = x :: xs
mergeL le (x :: xs) (y :: ys) =
  if le x y then x :: mergeL le xs (y :: ys)
            else y :: mergeL le (x :: xs) ys

-- "z ≤ every element", and "sorted" (head ≤ all of tail, recursively).
-- The head element is an explicit field so it can be recovered when matching.
data AllLE : (le : a -> a -> Bool) -> a -> List a -> Type where
  ALNil  : AllLE le z []
  ALCons : (w : a) -> le z w = True -> AllLE le z ws -> AllLE le z (w :: ws)

data Sorted : (a -> a -> Bool) -> List a -> Type where
  SNil  : Sorted le []
  SCons : (x : a) -> AllLE le x xs -> Sorted le xs -> Sorted le (x :: xs)

-- a lower bound for both inputs is a lower bound for their merge
total
allMergeLE : (le : a -> a -> Bool) -> (z : a) ->
             AllLE le z xs -> AllLE le z ys -> AllLE le z (mergeL le xs ys)
allMergeLE le z ALNil                       pys = pys
allMergeLE le z (ALCons x px pxs)           ALNil = ALCons x px pxs
allMergeLE le z pxAll@(ALCons x px pxs) pyAll@(ALCons y py pys) with (le x y)
  _ | True  = ALCons x px (allMergeLE le z (assert_smaller pxAll pxs) pyAll)
  _ | False = ALCons y py (allMergeLE le z pxAll (assert_smaller pyAll pys))

-- transitivity lowers a bound: x ≤ y and y ≤ all ys  ⇒  x ≤ all ys
total
allTransLE : (le : a -> a -> Bool) -> (x, y : a) ->
             (tr : (u, v, w : a) -> le u v = True -> le v w = True -> le u w = True) ->
             le x y = True -> AllLE le y ys -> AllLE le x ys
allTransLE le x y tr lexy ALNil             = ALNil
allTransLE le x y tr lexy (ALCons w pyw ps) =
  ALCons w (tr x y w lexy pyw) (allTransLE le x y tr lexy ps)

-- THE SPEC: merging two sorted lists yields a sorted list.
total
mergeSorted : (le : a -> a -> Bool) ->
              (tot : (u, v : a) -> Either (le u v = True) (le v u = True)) ->
              (tr  : (u, v, w : a) -> le u v = True -> le v w = True -> le u w = True) ->
              Sorted le xs -> Sorted le ys -> Sorted le (mergeL le xs ys)
mergeSorted le tot tr SNil              sy = sy
mergeSorted le tot tr (SCons x axs sxs) SNil = SCons x axs sxs
mergeSorted le tot tr sxAll@(SCons x axs sxs) syAll@(SCons y ays sys) with (le x y) proof prf
  _ | True  = SCons x (allMergeLE le x axs (ALCons y prf (allTransLE le x y tr prf ays)))
                      (mergeSorted le tot tr (assert_smaller sxAll sxs) syAll)
  _ | False =
      let leyx : (le y x = True)
          leyx = case tot x y of
                   Left lxy => absurd (trans (sym prf) lxy)
                   Right r  => r
      in SCons y (allMergeLE le y (ALCons x leyx (allTransLE le y x tr leyx axs)) ays)
                 (mergeSorted le tot tr sxAll (assert_smaller syAll sys))

------------------------------------------------------------------------
-- (3) Instantiation: a structural ≤ on Nat is total and transitive, so
-- the spec applies (mirrors the Lean `sortedMerge_sorted_nat`).
------------------------------------------------------------------------

total
lteNat : Nat -> Nat -> Bool
lteNat Z     _     = True
lteNat (S _) Z     = False
lteNat (S m) (S n) = lteNat m n

total
lteNatTotal : (u, v : Nat) -> Either (lteNat u v = True) (lteNat v u = True)
lteNatTotal Z     _     = Left Refl
lteNatTotal (S u) Z     = Right Refl
lteNatTotal (S u) (S v) = lteNatTotal u v

total
lteNatTrans : (u, v, w : Nat) -> lteNat u v = True -> lteNat v w = True -> lteNat u w = True
lteNatTrans Z     _     _     _ _ = Refl
lteNatTrans (S u) Z     _     p _ = absurd p
lteNatTrans (S u) (S v) Z     _ q = absurd q
lteNatTrans (S u) (S v) (S w) p q = lteNatTrans u v w p q

total
mergeSortedNat : Sorted Main.lteNat xs -> Sorted Main.lteNat ys ->
                 Sorted Main.lteNat (mergeL Main.lteNat xs ys)
mergeSortedNat sx sy = mergeSorted lteNat lteNatTotal lteNatTrans sx sy

main : IO ()
main = do
  -- dependent-length (Vect) version:
  putStrLn $ show $ sortedMerge [1,2,3] [2,5,6]
  putStrLn $ show $ sortedMerge [1,3,4] [2,5,6]
  -- verified-sorted (List) version, same answers:
  putStrLn $ show $ mergeL lteNat [1,2,3] [2,5,6]
  putStrLn $ show $ mergeL lteNat [1,3,4] [2,5,6]
