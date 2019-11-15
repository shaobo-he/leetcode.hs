module Main

import Data.Vect

sortedMerge : Ord elem => Vect m elem -> Vect n elem -> Vect (m+n) elem
sortedMerge [] ys = ys
sortedMerge {m} xs [] = rewrite plusZeroRightNeutral m in xs
sortedMerge {m=(S i)} {n=(S j)} fst@(x :: xs) snd@(y :: ys) = if x < y
  then x :: (sortedMerge xs snd)
  else let res = y :: (sortedMerge fst ys) in
      rewrite sym (plusSuccRightSucc i j) in res

main : IO ()
main = do
  putStrLn $ show $ sortedMerge [1,2,3] [2,5,6]
  putStrLn $ show $ sortedMerge [1,3,4] [2,5,6]
