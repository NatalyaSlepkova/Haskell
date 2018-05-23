{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Reader (runReader)
import           Control.Monad.State  (runStateT)
import           Control.Monad.Trans  (liftIO)
import           Core                 (EvaluateError (..), Expression (..),
                                       Statement (..))
import           Data.ByteString      (ByteString, pack)
import           Data.ByteString.UTF8 (fromString)
import           Data.Map             (fromList)
import           Evaluator            (evaluate)
import           Language             (executeStatements)
import           Parser               (ParseError, parseExpression, runParser,
                                       statementsParser)
import           Prelude              hiding (fromList)

main :: IO ()
main = hspec $ do
  describe "Evaluator test" $ do
    it "Primitive -> 2 + 2 = 4" $
        runReader (evaluate (Add (Lit 2) (Lit 2))) (fromList []) `shouldBe` Right 4
    it "Expected : UndefinedReference x,  2 + x = 4" $
        runReader (evaluate (Add (Lit 2) (Var "x"))) (fromList []) `shouldBe` Left (UndefinedReference "x")
    it "Expected: DivideByZero, 2 / x = 4 where x = 0" $
        runReader (evaluate (Div (Lit 2) (Var "x"))) (fromList [("x", 0)]) `shouldBe` Left DivideByZero
    it "Expected: DivideByZero, 2 / 0 = 4" $
        runReader (evaluate (Div (Lit 2) (Lit 0))) (fromList [("x", 0)]) `shouldBe` Left DivideByZero
    it "Primitive with var 2 / x = 4 where x = 1" $
        runReader (evaluate (Div (Lit 2) (Var "x"))) (fromList [("x", 1)]) `shouldBe` Right 2
  describe "Parser test" $ do
    it "2 + 2" $
        transformError (parseExpression "2+2") `shouldBe` Just (Add (Lit 2) (Lit 2))
    it "2 + (let x in x)" $
        transformError (parseExpression "2 + (let x = 2 in x)") `shouldBe` Just (Add (Lit 2) (Let "x" (Lit 2) (Var "x")))
    it "2 + (let x in x)" $
        transformError (parseExpression "2 + (let x in x)") `shouldBe` Nothing
  describe "Parse and eval test" $ do
    it "10" $
        case parseExpression "10" of
            (Right r) -> runReader (evaluate r) (fromList []) `shouldBe` Right 10
    it "2 + 2" $
        case parseExpression "2+2" of
            (Right r) -> runReader (evaluate r) (fromList []) `shouldBe` Right 4
    it "2 * 10 + x * 4" $
        case parseExpression "2 * 10 + x * 4" of
            (Right r) -> runReader (evaluate r) (fromList [("x", 0)]) `shouldBe` Right 20
    it "(((2 * 10))) + (let x = 10 in (x * 4))" $
        case parseExpression "(((2 * 10))) + (let x = 10 in (x * 4))" of
            (Right r) -> runReader (evaluate r) (fromList [("x", 0)]) `shouldBe` Right 60
    it "(((2 * 10))) + (let x = 10 in (x * 4)) / 0" $
        case parseExpression "(((2 * 10))) + (let x = 10 in (x * 4)) / 0" of
            (Right r) -> runReader (evaluate r) (fromList [("x", 0)]) `shouldBe` Left DivideByZero
    it "2 -   2 -   2 - 2 + (let x = 0 in (x * 4)) / 1" $
        case parseExpression  "2 -  2 -   2 -2 + (let x = 0 in (x * 4)) / 1" of
            (Right r) -> runReader (evaluate r) (fromList [("x", 0)]) `shouldBe` Right (-4)
  describe "Statements Parser" $ do
    it "mut x = 5\n" $
        runParser statementsParser "" "mut x = 5" `shouldBe` Right [Declaration "x" (Lit 5)]
    it "mut x = 5\n  x = 10\n" $
        runParser statementsParser "" "mut x = 5\n x = 10" `shouldBe` Right [Declaration "x" (Lit 5), Assignment "x" (Lit 10)]
    it "< x + 10\n" $
        runParser statementsParser "" "< x + 10"  `shouldBe` Right [Output (Add (Var "x") (Lit 10))]
    it "> x" $
        runParser statementsParser "" "> x" `shouldBe` Right [Input "x"]
    it "for x = 0 to 10 \n    { < x}\n" $
        runParser statementsParser "" "for x = 0 to 10 \n    { < x}" `shouldBe` Right[For "x" (Lit 0) (Lit 10) [Output (Var "x")]]
    it "mutx x=10" $
        transformError (runParser statementsParser "" "mutx x=10")  `shouldBe` Nothing
    it "forz x = 0 to 10 \n    { < x}\n" $
        transformError (runParser statementsParser "" "forz x = 0 to 10 \n    { < x}") `shouldBe` Nothing
  describe "Interpretator test" $ do
    it "mut x = 5\n  mut x = 10" $ do
        let result = runParser statementsParser "" "mut x = 5\n  mut x = 10"
        case result of
            (Right st) -> do
                r <- runStateT (executeStatements st) (fromList [])
                r`shouldBe` (Just (ExecutionError 2 (MultipliedDefinition "x")), fromList [("x", 5)])
    it "x = 10" $ do
        let result = runParser statementsParser "" "x = 10"
        case result of
            (Right st) -> do
                r <- runStateT (executeStatements st) (fromList [])
                r`shouldBe` (Just (ExecutionError 1 (UndefinedReference "x")), fromList [])
    it "factorial 5" $ do
        let result = runParser statementsParser "" (fromString getFactorialSource)
        case result of
            (Right st) -> do
                r <- runStateT (executeStatements st) (fromList [])
                r`shouldBe` (Nothing, fromList [("x", 5), ("y", 6), ("fact", 120)])
    it "modify test" $ do
        let result = runParser statementsParser "" "mut x = 5\n x = 10"
        case result of
            (Right st) -> do
                r <- runStateT (executeStatements st) (fromList [])
                r`shouldBe` (Nothing, fromList [("x", 10)])
    it "mut x = 10 x = x / 10" $ do
        let result = runParser statementsParser "" "mut x = 10 x = x / 10"
        case result of
            (Right st) -> do
                r <- runStateT (executeStatements st) (fromList [])
                r`shouldBe` (Nothing, fromList [("x", 1)])
    it "mut x = 10 x = x / 0" $ do
        let result = runParser statementsParser "" "mut x = 10 x = x / 0"
        case result of
            (Right st) -> do
                r <- runStateT (executeStatements st) (fromList [])
                r`shouldBe` (Just (ExecutionError 2 DivideByZero), fromList [("x", 10)])
  where
    transformError t = case t of
                        (Left _)  -> Nothing
                        (Right r) -> Just r
    getFactorialSource =    "mut x = 5\n" ++
                            "mut fact = 1\n" ++
                            "mut y = 0\n" ++
                            "for y = 1 to x {\n" ++
                            "    mut z = fact * y\n" ++
                            "    fact = z\n" ++
                            "}\n"
