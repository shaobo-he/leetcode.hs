import Control.Applicative

-- Grammar of valid parenthesis
-- VP := "(" VP ")" VP | ε

generateParenthesis :: Int -> [String]
generateParenthesis 0 = [""]
generateParenthesis n = foldl collect [] [0..(n-1)] where
  collect r m = r ++ (pure combine <*> generateParenthesis m <*> generateParenthesis (n - m - 1)) where
    combine l r = "(" ++ l ++ ")" ++ r

main :: IO ()
main = putStrLn $ show $ generateParenthesis 3
