module Main where

import           Block1
import           Block2
import           Test.Hspec
import Test.Tasty.Hspec
import Test.Tasty

main :: IO ()
main = (testSpec "Eval Test" testEval) >>= \test -> defaultMain $ testGroup "123" [test]

testEval :: Spec
testEval = do
  describe "expr tests..." $ do
    it "big expr" $
      (eval (Sum (Prod (Const 5) (Const 100)) (Diff (Const 45) (Pow (Const 3) (Const 2))))) `shouldBe` Right 536
    it "dbz expr" $
      (eval (Quot (Const 5) (Sum (Const 3) (Const (-3))))) `shouldBe` Left DivisionByZero
    it "power error expr" $
      (eval (Pow (Const 0) (Const 0))) `shouldBe` Left (InvalidPower "Can't calculate 0 ^ 0")
  describe "bin tests..." $ do
    it "bin" $ do
      bin 1 `shouldMatchList` [[1], [0]]
      bin 2 `shouldMatchList` [[1, 0], [0, 1], [1, 1], [0, 0]]
  describe "stringSum tests..." $ do
    it "stringSum" $ do
      stringSum "12 34 56 78 90" `shouldBe` Just 270
      stringSum "12 a3 45" `shouldBe` Nothing