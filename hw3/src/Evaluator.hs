module Evaluator (
    evaluate
) where

import           Control.Monad        (liftM2, void)
import           Control.Monad.Reader (Reader (..), asks, reader, runReader)
import           Core                 (Constant,
                                       EvaluateError (DivideByZero, UndefinedReference),
                                       Expression (..), LocalCtx, Variable)

import           Data.Map             (insert, lookup)
import           Prelude              hiding (lookup)

tryGetValue :: Variable -> LocalCtx -> Either EvaluateError Integer
tryGetValue variable map = case lookup variable map of
    Nothing -> Left (UndefinedReference variable)
    Just r  -> Right r

addValue :: LocalCtx -> Variable -> Integer -> LocalCtx
addValue map variable value = insert variable value map

checkedDiv :: Either EvaluateError Integer -> Either EvaluateError Integer -> Either EvaluateError Integer
checkedDiv _ (Right 0) = Left DivideByZero
checkedDiv a b         = liftM2 div a b

evaluate :: Expression -> Reader LocalCtx (Either EvaluateError Integer)
evaluate (Lit x)   = return $ Right x
evaluate (Add x y) = liftM2 (liftM2 (+)) (evaluate x) (evaluate y)
evaluate (Sub x y) = liftM2 (liftM2 (-)) (evaluate x) (evaluate y)
evaluate (Mul x y) = liftM2 (liftM2 (*)) (evaluate x) (evaluate y)
evaluate (Div x y) = reader $ \mp -> checkedDiv (runReader (evaluate x) mp) (runReader (evaluate y) mp)
evaluate (Var x) = asks (tryGetValue x)
evaluate (Let var val ex) = reader $ \ mp ->
    case runReader (evaluate val) mp of
        Left err     -> Left err
        Right result -> runReader (evaluate ex) (insert var result mp)
