{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Block1 where

import           Control.Monad (join, liftM2)
import           Data.String   (String)

data Expr
  = Const Int
  | Sum Expr Expr
  | Diff Expr Expr
  | Prod Expr Expr
  | Quot Expr Expr
  | Pow Expr Expr deriving (Show, Eq)

data ArithmeticError = DivisionByZero | InvalidPower String deriving (Show, Eq)

eval :: Expr -> Either ArithmeticError Int
eval (Const x)  = Right x
eval (Sum a b)  = liftM2 (+) (eval a) (eval b)
eval (Diff a b) = liftM2 (-) (eval a) (eval b)
eval (Prod a b) = liftM2 (*) (eval a) (eval b)
eval (Quot a b) = liftM2 quot (eval a) ((eval b) >>= checkDBZ)
  where
    checkDBZ :: Int -> Either ArithmeticError Int
    checkDBZ x
      | x == 0 = Left DivisionByZero
      | otherwise = Right x
eval (Pow a b) = join $ liftM2 checkedPow (eval a) (eval b)
  where
    checkedPow :: Int -> Int -> Either ArithmeticError Int
    checkedPow a' b'
      | (a' == 0 && b' == 0) || b' < 0 = Left $ InvalidPower $ "Can't calculate " ++ (show a') ++ " ^ " ++ (show b')
      | otherwise = Right $ a' ^ b'

bin :: Int -> [[Int]]
bin x
 | x < 0 = error "Argument is less than 0"
 | x == 0 = [[]]
 | otherwise = bin (x - 1) >>= \li -> [0 : li, 1 : li]