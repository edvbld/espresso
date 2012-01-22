module ParserTests where

import Test.HUnit
import Text.Parser(parse)
import Espresso.Parser(mainClass)
import Espresso.AST

test_parse_empty_main_class = assertEqual 
    "Should parse empty main class"
    (Right $ MJMainClass (MJIdentifier className) (MJIdentifier argsName) [])
    (parse mainClass (unlines program))
    where
        className = "Main"
        argsName = "main"
        program = ["class " ++ className ++ " {",
                   "public static void main (String " ++ argsName ++ "){",
                   "}", "}"]

