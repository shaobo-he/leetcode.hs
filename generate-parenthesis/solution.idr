module Main

-- opensLeft '(' still to place; closesLeft ')' still to place; acc reversed.
-- A ')' is only legal while more opens remain unclosed (closesLeft > opensLeft).
build : Nat -> Nat -> List Char -> List (List Char)
build Z Z acc = [reverse acc]
build opensLeft closesLeft acc =
  let openOpt  = if opensLeft > 0
                   then build (minus opensLeft 1) closesLeft ('(' :: acc)
                   else []
      closeOpt = if closesLeft > opensLeft
                   then build opensLeft (minus closesLeft 1) (')' :: acc)
                   else []
  in openOpt ++ closeOpt

generateParenthesis : Nat -> List String
generateParenthesis n = map pack (build n n [])

main : IO ()
main = printLn (generateParenthesis 3)
