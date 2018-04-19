module Main where

import           Block1
import           Block2
import           Block3
import           Data.Char        (isUpper)
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
  describe "Block3 tests..." $ do
    it "satisfy" $ do
      runParser (satisfy isUpper) "HASKELL" `shouldBe` Just ('H',"ASKELL")
      runParser (satisfy isUpper) "haskell" `shouldBe` Nothing
    it "ok" $
      runParser ok "Haskell" `shouldBe` Just ((),"Haskell")
    it "eof" $ do
      runParser eof "Haskell" `shouldBe` Nothing
      runParser eof "" `shouldBe` Just ((),"")
    it "element" $ do
      runParser (element 'c') "Haskell" `shouldBe` Nothing
      runParser (element 'H') "Haskell" `shouldBe` Just ('H',"askell")
    it "stream" $ do
      runParser (stream "c") "Haskell" `shouldBe` Nothing
      runParser (stream "H") "Haskell" `shouldBe` Just ("H","askell")
      runParser (stream "Haskel") "Haskell" `shouldBe` Just ("Haskel","l")
    it "spaces" $ do
      runParser spaces "Haskell " `shouldBe` Just ("","Haskell ")
      runParser spaces " Haskell" `shouldBe` Just (" ", "Haskell")
    it "integer" $ do
      runParser integer "Haskell" `shouldBe` Nothing
      runParser integer "4913" `shouldBe` Just (4913,"")
    it "brackets" $ do
      runParser brackets ")((" `shouldBe` Just((), ")((")
      runParser brackets "()()()()((()()))" `shouldBe` Just((), "")
    it "onlySignedInteger" $ do
      runParser onlySignedInteger "+Haskell" `shouldBe` Nothing
      runParser onlySignedInteger "4913" `shouldBe` Nothing
      runParser onlySignedInteger "+4913" `shouldBe` Just (4913,"")
      runParser onlySignedInteger "-4913" `shouldBe` Just (-4913,"")
    it "notOnlySignedInteger" $ do
      runParser notOnlySignedInteger "Haskell" `shouldBe` Nothing
      runParser notOnlySignedInteger "+4913" `shouldBe` Just (4913,"")
      runParser notOnlySignedInteger "4913" `shouldBe` Just (4913,"")
      runParser notOnlySignedInteger "-4913" `shouldBe` Just (-4913,"")
    it "parseListOfLists" $
        runParser parseListOfLists "1, +2,  3, 4, 5, 6,  1, -4913" `shouldBe` Just ([[2], [4, 5, 6], [-4913]], "")