module Main

letters : Char -> List Char
letters '2' = ['a','b','c']
letters '3' = ['d','e','f']
letters '4' = ['g','h','i']
letters '5' = ['j','k','l']
letters '6' = ['m','n','o']
letters '7' = ['p','q','r','s']
letters '8' = ['t','u','v']
letters '9' = ['w','x','y','z']
letters _   = []

step : Char -> List (List Char) -> List (List Char)
step d acc = concatMap (\l => map (l ::) acc) (letters d)

letterCombinations : String -> List String
letterCombinations s = case unpack s of
  []  => []
  cs  => map pack (foldr step [[]] cs)

main : IO ()
main = printLn (letterCombinations "23")
