module Main

closeOf : Char -> Char
closeOf '(' = ')'
closeOf '[' = ']'
closeOf '{' = '}'
closeOf c   = c

isOpen : Char -> Bool
isOpen c = c == '(' || c == '[' || c == '{'

-- Push the expected closer for each opener; match closers against the stack top.
isValid : String -> Bool
isValid s = go (unpack s) []
  where
    go : List Char -> List Char -> Bool
    go [] []        = True
    go [] (_ :: _)  = False
    go (c :: cs) stack =
      if isOpen c
        then go cs (closeOf c :: stack)
        else case stack of
               (top :: rest) => if c == top then go cs rest else False
               []            => False

main : IO ()
main = do
  printLn (isValid "()[]{}")  -- True
  printLn (isValid "(]")      -- False
  printLn (isValid "{[]}")    -- True
