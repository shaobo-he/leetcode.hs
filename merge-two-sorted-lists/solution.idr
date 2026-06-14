module Main

mergeTwo : Ord a => List a -> List a -> List a
mergeTwo [] ys = ys
mergeTwo xs [] = xs
mergeTwo (x :: xs) (y :: ys) =
  if x <= y then x :: mergeTwo xs (y :: ys)
            else y :: mergeTwo (x :: xs) ys

main : IO ()
main = printLn (mergeTwo [1,2,4] [1,3,4])  -- [1, 1, 2, 3, 4, 4]
