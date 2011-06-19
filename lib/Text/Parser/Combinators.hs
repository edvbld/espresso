-- | This module contains different parser combinators. A combinator is a way 
-- to construct a new parser from several smaller ones
module Text.Parser.Combinators where

import Control.Monad
import Text.Parser

-- | '<|>' is the choice combinator. The created parser will succeed if any 
-- of the two given parses succeeds.
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

-- | 'many' creates a parser that will suceed if the given parser succeeds 
-- /zero/ or more times.
many :: Parser a -> Parser [a]
many pa = many1 pa <|> return []

-- | 'many1' creates a parser that will succeed if the given parser succeeds 
-- /one/ or more times.
many1 :: Parser a -> Parser [a]
many1 pa = do a <- pa
              as <- (many pa <|> return [])
              return (a:as)
