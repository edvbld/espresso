-- | Contains the Parser type and the parse function
module Text.Parser(Parser(..), ParseError, parse) where

import Control.Monad

-- | Represents a parse error
type ParseError = String

-- | The parser type, a parser is a function from String to either a parse 
-- error or a pair of the remaining input string and the parsed value.
newtype Parser a = 
    -- | The low-level constructor for a Parser. You shouldn't have to use
    -- this constructor, instead, use the combinators.
    Parser { 
        -- | 'run' is the most low-level way to run a parser. 
        -- 'parse' is probably a better alternative in almost all cases.
        run :: String -> Either ParseError (String, a) }

instance Monad Parser where
    -- return :: a -> Parser a
    return a = Parser (\s -> Right (s, a))
    
    -- (>>=) :: (Parser a) -> (a -> Parser b) -> Parser b
    (>>=) pa f = 
        Parser $ \s -> case run pa s of 
                        Left err      -> Left err
                        Right (s', a) -> run (f a) s' 
    
    -- fail :: String -> Parser a
    fail s = Parser (\_ -> Left s)

instance MonadPlus Parser where
    --mzero :: Parser a
    mzero = Parser (\_ -> Left "")

    --mplus :: Parser a -> Parser a -> Parser a
    mplus a b = 
        Parser $ \s -> case run a s of 
                        Left _  -> run b s
                        Right res -> Right res

-- | 'parse' runs a parser on a given string and returns the result
parse :: Parser a -> String -> Either ParseError a
parse p s = case run p s of
             Left err     -> Left err
             Right (_, v) -> Right v 
