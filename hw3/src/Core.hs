{-# LANGUAGE OverloadedStrings #-}

module Core (
    Variable,
    Constant,
    Expression(..),
    Statement(..),
    LocalCtx,
    EvaluateError(..)
) where

import           Data.ByteString      (ByteString, pack)
import           Data.ByteString.UTF8 (fromString, toString)
import           Data.Map             (Map)
import           Text.Megaparsec      (SourcePos)

type Variable = ByteString
type Constant = Integer
data Expression = Lit Constant
        | Var Variable
        | Add Expression Expression
        | Sub Expression Expression
        | Mul Expression Expression
        | Div Expression Expression
        | Let Variable Expression Expression
    deriving (Show, Eq)

data Statement = Declaration Variable Expression | Assignment Variable Expression |
    Input Variable| Output Expression| For Variable Expression Expression [Statement] deriving (Show, Eq)

type LocalCtx = Map Variable Integer
data EvaluateError = ExecutionError Integer EvaluateError| DivideByZero | MultipliedDefinition Variable |
                        UndefinedReference Variable| ParserError SourcePos deriving (Eq)

instance Show EvaluateError where
    show (ExecutionError stId inner) = toString "An exception occured at statement: " ++ show stId ++
        toString ", with inner exception:\n" ++ show inner
    show DivideByZero = toString "Divide by zero"
    show (UndefinedReference name) =  toString "Try to reference to undefined variable with name: " ++ toString name ++ toString ". Declare a variable above"
    show (MultipliedDefinition name) = toString "Variable with name: " ++ toString name ++ toString " has been already declared. Remove declaration or rename"
    show (ParserError t) = toString "Parser error has been occured at " ++ show t

