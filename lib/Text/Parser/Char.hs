-- | This module contains several common parsers for characters
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

-- | 'char' matches a single characther
char :: Char -> Parser Char
char y = satisfy (== y)

-- | 'alphaNum' matches any alpha-numeric charachter (both lower and upper case)
alphaNum :: Parser Char
alphaNum = oneOf (chars ++ digits)

-- | 'letter' matches a single alphabetic charachter (both lower and upper case)
letter :: Parser Char
letter = oneOf chars

-- | 'oneOf' matches any of the charachters in the given list of characthers
oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (flip elem cs)

-- | 'satisfy' matches the first character in the string against the given 
-- predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of 
                            (x:xs) -> if p x 
                                        then Right (xs, x)
                                        else Left $ "Parse error on " ++ (x:xs)
                            []     -> Left "No input string"
