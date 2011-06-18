module Text.Parser(Parser(..), ParseError, parse) where

import Control.Monad

type ParseError = String
newtype Parser a = 
    Parser { run :: String -> Either ParseError (String, a) }

instance Monad (Either a) where
    -- return :: a -> (Either ParserError) a
    return a = Right a

    -- (>>=) :: (Either ParserError) a -> (a -> (Either ParserError) b) ->
    --          (Either ParserError) b
    (>>=) (Right a)  f = f a
    (>>=) (Left err) f = Left err 

instance Monad Parser where
    -- return :: a -> Parser a
    return a = Parser (\s -> return (s, a))
    
    -- (>>=) :: (Parser a) -> (a -> Parser b) -> Parser b
    (>>=) pa f = 
        Parser $ \s -> case run pa s of 
                        Left err      -> Left err
                        Right (s', a) -> run (f a) s' 
    
    -- fail :: String -> Parser a
    fail s = Parser (\s' -> Left s)

instance MonadPlus Parser where
    --mzero :: Parser a
    mzero = Parser (\s -> Left "")

    --mplus :: Parser a -> Parser a -> Parser a
    mplus a b = 
        Parser $ \s -> case run a s of 
                        Left err  -> run b s
                        Right res -> Right res

parse :: Parser a -> String -> Either ParseError a
parse p s = case run p s of
             Left err     -> fail err
             Right (_, v) -> return v 
