module Main where

import Text.Parsec.Prim (Parsec, parseTest, (<|>))
import Text.Parsec.Char (spaces, char, digit)
import Text.Parsec.Combinator (many1, chainl1)
import Control.Monad ((>>))

type Parser a = Parsec String () a

token :: Parser a -> Parser a
token p = do spaces
             x <- p
             spaces
             return x

decimal :: Parser Integer
decimal = token $ read <$> (many1 digit)

symbol :: Char -> Parser Char
symbol c = token $ char c

parens :: Parser a -> Parser a
parens p = do symbol '('
              x <- p
              symbol ')'
              return x

-- references: https://stackoverflow.com/questions/50261752/parsing-expressions-inside-arithmetic-expressions
--             https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html
expr :: Parser Integer
expr = term `chainl1` linop

term :: Parser Integer
term = factor `chainl1` nonlinop

factor :: Parser Integer
factor = parens expr <|> decimal

linop :: Parser (Integer -> Integer -> Integer)
linop = (symbol '+' >> return (+)) <|> (symbol '-' >> return (-))

nonlinop :: Parser (Integer -> Integer -> Integer)
nonlinop = (symbol '*' >> return (*)) <|> (symbol '/' >> return div)


main :: IO ()
main = parseTest expr "1 - 2 * 3" >>
       parseTest expr "(1 + 2) * 3"
