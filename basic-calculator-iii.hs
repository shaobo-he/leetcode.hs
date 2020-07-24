module Main where

import Text.Parsec.Prim (Parsec, parseTest)
import Text.Parsec.Char (spaces, digit, char)
import Text.Parsec.Combinator (many1, choice)
import Control.Monad ((>>))

type Parser a = Parsec String () a

decimal :: Parser Integer
decimal = token $ read <$> (many1 digit)

token :: Parser a -> Parser a
token p = do spaces
             x <- p
             spaces
             return x

symbol :: Char -> Parser Char
symbol c = token $ char c

linop :: Parser (Integer -> Integer -> Integer)
linop = choice [
          symbol '+' >> return (+),
          symbol '-' >> return (-)
       ]

nonlinop :: Parser (Integer -> Integer -> Integer)
nonlinop = choice [
          symbol '*' >> return (*),
          symbol '/' >> return div 
       ]

{-
  grammar:

  expr ::= expr + term |
           expr - term |
           term

  term ::= term * factor |
           term / factor |
           factor

  factor ::= ( expr ) | decimal
-}

expr :: Parser Integer
expr = choice [
         do lhs <- expr
            op <- linop
            rhs <- term
            return $ op lhs rhs,
         term
       ]

term :: Parser Integer
term = choice [
         do lhs <- term
            op <- nonlinop
            rhs <- factor 
            return $ op lhs rhs,
         factor
       ]

factor :: Parser Integer
factor = choice [
           do symbol '('
              e <- expr
              symbol ')'
              return e,
           decimal
         ]

main :: IO ()
main = parseTest expr "1*2"
