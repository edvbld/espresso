module Espresso.Lexer (
       lex,
       Token(..)) where
import Prelude hiding (lex)
import Data.Char(isDigit, isAlpha, isAlphaNum)

data Token = Number Integer
           | Identifier String
           deriving (Show, Eq)

-- | 'lex' splits a string into  a list of tokens
lex :: String -> Either String [Token]
lex str@(c:cs)
    | isDigit c      = number str
    | isIdentifier c = identifier str
    | isWhitespace c = lex cs
    | otherwise      = Left $ "Lexical error on " ++ str
lex [] = Right []

-- | 'number' lexes a number
number :: String -> Either String [Token]
number str = tokens
    where
        num  = takeWhile isDigit str
        rest = dropWhile isDigit str
        int = read num 
        tokens = case lex rest of
                  Left err  -> Left err
                  Right val -> Right $ (Number int):val

-- | 'isIdentifier' checks if a char is the start of an identifier
isIdentifier :: Char -> Bool
isIdentifier c = c == '_' || isAlpha c

-- | 'identifier' lexes an indentfier in MiniJava
identifier :: String -> Either String [Token]
identifier str = tokens
    where
        isIdChar c = c == '_' || isAlphaNum c
        name = takeWhile isIdChar str
        rest = dropWhile isIdChar str
        tokens = case lex rest of 
                   Left err  -> Left err
                   Right val -> Right $ (Identifier name):val

-- | 'isWhitespace' checks if character is a whitespace character
isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\t' || c == '\n'
