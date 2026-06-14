module Main

-- each element picked as head, paired with the remaining list
selections : List a -> List (a, List a)
selections [] = []
selections (x :: xs) = (x, xs) :: map (\(y, ys) => (y, x :: ys)) (selections xs)

permute : List a -> List (List a)
permute [] = [[]]
permute xs = concatMap (\(x, rest) => map (x ::) (permute rest)) (selections xs)

main : IO ()
main = printLn (permute [1,2,3])
