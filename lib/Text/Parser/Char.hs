module Text.Parser.Char(
    char,
    alphaNum,
    letter,
    oneOf,
    satisfy) where

import Text.Parser

upperChars :: [Char]
upperChars = ['A'..'Z']

lowerChars :: [Char]
lowerChars = ['a'..'z']

chars :: [Char]
chars = upperChars ++ lowerChars

digits :: [Char]
digits = ['0'..'9']

char :: Char -> Parser Char
char y = satisfy (== y)

alphaNum :: Parser Char
alphaNum = oneOf (chars ++ digits)

letter :: Parser Char
letter = oneOf chars

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (flip elem cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of 
                            (x:xs) -> if p x 
                                        then Right (xs, x)
                                        else Left $ "Parse error on " ++ (x:xs)
                            []     -> Left "No input string"
