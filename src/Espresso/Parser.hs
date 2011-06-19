-- | This module contains the parser for the MiniJava language
module Espresso.Parser(
    Espresso.Parser.parse,
    identifier) where

import Text.Parser as P
import Text.Parser.Combinators
import Text.Parser.Char
import Espresso.AST

-- | 'parse' parses a string representing a MiniJava program into an abstract 
-- syntax tree representing the program or returns an error.
parse :: String -> Either String MJExpression
parse s = P.parse identifier s

-- | 'identifier' parses an identifier in the MiniJava language.
identifier :: Parser MJExpression
identifier =  do x  <- (char '_' <|> letter)
                 xs <- many (alphaNum <|> char '_')
                 return (Identifier (x:xs))
