module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import ParserProperties(prop_parse_identifier)

tests :: [Test]
tests = [ testGroup "Parser tests" [
            testProperty "prop_parse_identifier" prop_parse_identifier]
            ]

main :: IO()
main = defaultMain tests
