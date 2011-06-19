-- | This module contains the abstract syntax tree for the MiniJava language 
-- used in the compiler
module Espresso.AST where

-- | The datatype for an expression in MiniJava
data MJExpression = Identifier String
                    deriving (Show, Eq)
