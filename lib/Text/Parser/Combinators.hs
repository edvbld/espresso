module Text.Parser.Combinators(
    (<|>),
    many) where

import Control.Monad
import Text.Parser

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

many :: Parser a -> Parser [a]
many pa = many1 pa <|> return []

many1 :: Parser a -> Parser [a]
many1 pa = do a <- pa
              as <- (many pa <|> return [])
              return (a:as)
