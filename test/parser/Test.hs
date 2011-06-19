module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import CombinatorProperties

tests :: [Test]
tests = [ testGroup "Combinator properties" [
            testProperty "prop_choice_succeeds_when_first_fail" 
                         prop_choice_succeeds_when_first_fail,
            testProperty "prop_choice_succeeds_when_first_succeeds" 
                         prop_choice_succeeds_when_first_succeeds
            ]
        ]

main :: IO()
main = defaultMain tests
