module Main where

data Re = Empty | Eps | Dot | C Char | Re :• Re | Re :∪ Re | Star Re
  deriving Show

dc :: Char -> Re -> Re
dc _ Empty = Empty
dc _ Eps = Empty
dc _ Dot = Eps
dc c (C c') = if c == c' then Eps else Empty
dc c (l1 :∪ l2) = (dc c l1) :∪ (dc c l2)
dc c (l1 :• l2) = ((dc c l1) :• l2) :∪ (if (null_eh l1) then (dc c l2) else Empty)
dc c (Star l) = (dc c l) :• (Star l)

null_eh :: Re -> Bool
null_eh Empty = False
null_eh Eps = True
null_eh Dot = False
null_eh (C _) = False
null_eh (Star _) = True
null_eh (l1 :∪ l2) = (null_eh l1) || (null_eh l2)
null_eh (l1 :• l2) = (null_eh l1) && (null_eh l2)

matches :: [Char] -> Re -> Bool
matches [] l = null_eh l
matches (h:r) l = matches r (dc h l)

parse_char :: Char -> Re
parse_char '.' = Dot
parse_char c = C c

parse :: [Char] -> Re
parse [] = Eps
parse (f:s:r) =
  if s == '*'
    then Star (parse_char f) :• (parse r)
    else (parse_char f) :• (parse (s:r))
parse (f:r) = parse_char f :• parse r

main :: IO ()
main = do
  putStrLn $ show $ matches "aa" $ parse "a"
  putStrLn $ show $ matches "aa" $ parse "a*"
  putStrLn $ show $ matches "ab" $ parse ".*"
  putStrLn $ show $ matches "aab" $ parse "c*a*b"
  putStrLn $ show $ matches "mississippi" $ parse "mis*is*p*."
