module Main where

-- Group elements at odd positions (1st, 3rd, ...) ahead of the even ones.
oddEvenList :: [a] -> [a]
oddEvenList xs = odds xs ++ evens xs
  where
    odds (x:_:rest) = x : odds rest
    odds [x]        = [x]
    odds []         = []
    evens (_:y:rest) = y : evens rest
    evens _          = []

main :: IO ()
main = do
  print (oddEvenList [1,2,3,4,5])      -- [1,3,5,2,4]
  print (oddEvenList [2,1,3,5,6,4,7])  -- [2,3,6,7,1,5,4]
