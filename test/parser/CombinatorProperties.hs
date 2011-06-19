module CombinatorProperties(
    prop_choice_succeeds_when_first_fail,
    prop_choice_succeeds_when_first_succeeds) where

import Test.QuickCheck
import Text.Parser
import Text.Parser.Combinators

prop_choice_succeeds_when_first_fail :: String -> Property
prop_choice_succeeds_when_first_fail s =
    not (null s) ==> run ((fail "error") <|> (return $ head s)) s == 
                     Right (s, head s)

prop_choice_succeeds_when_first_succeeds :: String -> Property
prop_choice_succeeds_when_first_succeeds s =
    not (null s) ==> run ((return $ head s) <|> (fail "error")) s == 
                     Right (s, head s)
