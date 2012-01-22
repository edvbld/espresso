-- | This module contains the parser for the MiniJava language
module Espresso.Parser(
    identifier,
    mainClass) where

import Text.Parser as P
import Text.Parser.Combinators
import Text.Parser.Char
import Espresso.AST

-- | 'token' represents a token in the MiniJava language
token :: Parser a -> Parser a
token p = do res <- p
             many spaces
             return res

-- | 'identifier' parses an identifier in the MiniJava language.
identifier :: Parser MJIdentifier
identifier = token p
    where
        p =  do x  <- (char '_' <|> letter)
                xs <- many (alphaNum <|> char '_')
                return $ MJIdentifier $ x:xs

-- | 'keyword' parses a keyword in MiniJava, which is string potentially
-- followed by white space.
keyword :: String -> Parser Char
keyword = token . string

-- | 'mainClass' parses the main class in the MiniJava language
mainClass :: Parser MJExpression
mainClass = do keyword "class"
               classId <- identifier
               keyword "{"
               keyword "public"
               keyword "static"
               keyword "void"
               keyword "main"
               keyword "("
               keyword "String"
               argsId <- identifier
               keyword ")"
               keyword "{"
               keyword "}"
               keyword "}"
               return (MJMainClass classId argsId [])
