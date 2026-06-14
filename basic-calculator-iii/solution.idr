module Main

import Data.List

isDigit' : Char -> Bool
isDigit' c = c >= '0' && c <= '9'

dropSpaces : List Char -> List Char
dropSpaces (' ' :: cs) = dropSpaces cs
dropSpaces cs = cs

-- integer division truncated toward zero (LeetCode semantics)
tdiv : Int -> Int -> Int
tdiv a b = let q = div (abs a) (abs b) in
           if (a < 0) /= (b < 0) then negate q else q

parseNumber : List Char -> (Int, List Char)
parseNumber cs = let (ds, rest) = span isDigit' cs in (cast (pack ds), rest)

-- recursive descent: expr = term (('+'|'-') term)* ; term = factor (('*'|'/') factor)*
mutual
  parseExpr : List Char -> (Int, List Char)
  parseExpr cs = let (t, rest) = parseTerm cs in exprTail t (dropSpaces rest)

  exprTail : Int -> List Char -> (Int, List Char)
  exprTail acc ('+' :: cs) = let (t, rest) = parseTerm cs in exprTail (acc + t) (dropSpaces rest)
  exprTail acc ('-' :: cs) = let (t, rest) = parseTerm cs in exprTail (acc - t) (dropSpaces rest)
  exprTail acc cs = (acc, cs)

  parseTerm : List Char -> (Int, List Char)
  parseTerm cs = let (f, rest) = parseFactor cs in termTail f (dropSpaces rest)

  termTail : Int -> List Char -> (Int, List Char)
  termTail acc ('*' :: cs) = let (f, rest) = parseFactor cs in termTail (acc * f) (dropSpaces rest)
  termTail acc ('/' :: cs) = let (f, rest) = parseFactor cs in termTail (tdiv acc f) (dropSpaces rest)
  termTail acc cs = (acc, cs)

  parseFactor : List Char -> (Int, List Char)
  parseFactor cs0 =
    case dropSpaces cs0 of
      ('(' :: rest) =>
        let (v, rest1) = parseExpr rest in
        (case dropSpaces rest1 of
           (')' :: rest2) => (v, rest2)
           other          => (v, other))
      cs => parseNumber cs

calculate : String -> Int
calculate s = fst (parseExpr (unpack s))

main : IO ()
main = do
  printLn (calculate "1+1")            -- 2
  printLn (calculate "6-4/2")          -- 4
  printLn (calculate "2*(5+5*2)+3*5")  -- 45
  printLn (calculate "14-3/2")         -- 13
  printLn (calculate "(0-3)/2")        -- -1
