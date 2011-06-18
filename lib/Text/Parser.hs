module Text.Parser(Parser(..), ParseError, parse) where

import Control.Monad

type ParseError = String
newtype Parser a = 
    Parser { run :: String -> Either ParseError (String, a) }

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

parse :: Parser a -> String -> Either ParseError a
parse p s = case run p s of
             Left err     -> Left err
             Right (_, v) -> Right v 
