module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import ParserProperties(prop_parse_identifier)
import ParserTests(test_parse_empty_main_class)

tests :: [Test]
tests = [testGroup "Parser tests" 
            [testProperty "prop_parse_identifier" prop_parse_identifier,
             testCase "test_parse_empty_main_class" test_parse_empty_main_class
            ]
        ]

main :: IO()
main = defaultMain tests
