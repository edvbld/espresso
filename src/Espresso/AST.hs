-- | This module contains the abstract syntax tree for the MiniJava language 
-- used in the compiler
module Espresso.AST where

newtype MJIdentifier = MJIdentifier String
                       deriving (Show, Eq)

data MJStatement = MJStatement
                   deriving (Show, Eq)

-- | The datatype for an expression in MiniJava
data MJExpression = MJMainClass MJIdentifier MJIdentifier [MJStatement]
                    deriving (Show, Eq)
