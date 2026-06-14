module Main

-- Brzozowski derivatives (port of the Haskell solution).
data RE = Empty | Eps | Dot | Chr Char | Seq RE RE | Alt RE RE | Star RE

nullable : RE -> Bool
nullable Empty     = False
nullable Eps       = True
nullable Dot       = False
nullable (Chr _)   = False
nullable (Seq a b) = nullable a && nullable b
nullable (Alt a b) = nullable a || nullable b
nullable (Star _)  = True

deriv : Char -> RE -> RE
deriv _ Empty     = Empty
deriv _ Eps       = Empty
deriv _ Dot       = Eps
deriv c (Chr c')  = if c == c' then Eps else Empty
deriv c (Alt a b) = Alt (deriv c a) (deriv c b)
deriv c (Seq a b) = if nullable a
                      then Alt (Seq (deriv c a) b) (deriv c b)
                      else Seq (deriv c a) b
deriv c (Star a)  = Seq (deriv c a) (Star a)

matches : String -> RE -> Bool
matches s r = nullable (foldl (flip deriv) r (unpack s))

atom : Char -> RE
atom '.' = Dot
atom c   = Chr c

parse : List Char -> RE
parse []                = Eps
parse (c :: '*' :: rest) = Seq (Star (atom c)) (parse rest)
parse (c :: rest)        = Seq (atom c) (parse rest)

isMatch : String -> String -> Bool
isMatch s p = matches s (parse (unpack p))

main : IO ()
main = do
  printLn (isMatch "aa" "a")                    -- False
  printLn (isMatch "aa" "a*")                   -- True
  printLn (isMatch "ab" ".*")                   -- True
  printLn (isMatch "aab" "c*a*b")               -- True
  printLn (isMatch "mississippi" "mis*is*p*.")  -- False
