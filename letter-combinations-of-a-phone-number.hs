letterCombinations :: [Char] -> [[Char]]
letterCombinations [] = []
letterCombinations [d] = [[l] | l <- getLetter d]
letterCombinations (f:r) = (:) <$> getLetter f <*> letterCombinations r

getLetter '2' = ['a', 'b', 'c']
getLetter '3' = ['d', 'e', 'f']
getLetter '4' = ['g', 'h', 'j']
getLetter '5' = ['j', 'k', 'l']
getLetter '6' = ['m', 'n', 'o']
getLetter '7' = ['p', 'q', 'r', 's']
getLetter '8' = ['t', 'u', 'v']
getLetter '9' = ['w', 'x', 'y', 'z']

main = putStrLn $ show $ letterCombinations "23"
