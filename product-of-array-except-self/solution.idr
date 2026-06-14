module Main

import Data.List

-- running products: prefixProds acc xs = [acc, acc*x0, acc*x0*x1, ...]
prefixProds : Int -> List Int -> List Int
prefixProds _ [] = []
prefixProds acc (y :: ys) = acc :: prefixProds (acc * y) ys

productExceptSelf : List Int -> List Int
productExceptSelf xs =
  let pre = prefixProds 1 xs
      suf = reverse (prefixProds 1 (reverse xs))
  in zipWith (*) pre suf

main : IO ()
main = do
  printLn (productExceptSelf [1,2,3,4])  -- [24, 12, 8, 6]
  printLn (productExceptSelf [5,2,3])    -- [6, 15, 10]
