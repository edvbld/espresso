module ParserProperties where

import Test.QuickCheck
import Espresso.Parser(identifier)
import Espresso.AST
import Text.Parser(parse)

data Id = Id String
          deriving (Show)

instance Arbitrary Id where
    arbitrary = do let start = ('_':(['a'..'z'] ++ ['A'..'Z']))
                   let rest = start ++ ['0'..'9']
                   x <-  elements start
                   xs <- listOf $ elements rest
                   return (Id (x:xs))


prop_parse_identifier :: Id -> Bool
prop_parse_identifier (Id s) = parse identifier s == Right (Identifier s)
