module Espresso.Parser(
    Espresso.Parser.parse,
    identifier) where

import Text.Parser as P
import Text.Parser.Combinators
import Text.Parser.Char
import Espresso.AST

parse :: String -> Either String MJExpression
parse s = P.parse identifier s

identifier :: Parser MJExpression
identifier =  do x  <- (char '_' <|> letter)
                 xs <- many (alphaNum <|> char '_')
                 return (Identifier (x:xs))
