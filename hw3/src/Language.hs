{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language (
    executeFromFile,
    executeSourceCode,
    executeStatements
  ) where

import           Control.Applicative       (liftA2)
import           Control.Monad             (liftM2, void)
import           Control.Monad.Reader      (MonadReader, Reader (..), asks, reader,
                                            runReader)
import           Control.Monad.State       (MonadIO, MonadState, MonadTrans (..),
                                            State (..), StateT (..), get, guard, lift,
                                            modify, runState, runStateT, state)
import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Core                      (Constant, EvaluateError (..), Expression (..),
                                            LocalCtx, Statement (..), Variable)
import           Data.ByteString           (ByteString, pack)
import           Data.ByteString.UTF8      (fromString)
import           Data.List.NonEmpty        (head)
import           Data.Map                  (fromList, insert, intersection, lookup,
                                            member, (!))
import           Data.Void                 (Void)
import           Evaluator                 (evaluate)
import           Parser                    (ParseError, Token, errorPos, integer,
                                            parseExpression, runParser, statementsParser)
import           Prelude                   hiding (fromList, head, lookup)



newtype LocalContext m a = LocalContext { runContext :: StateT LocalCtx m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState LocalCtx)

instance MonadTrans LocalContext where
    lift = LocalContext . lift

expr = Var "x" `Add` (Lit 3 `Mul` ("x" `Let` Lit 2 $ Var "x"))

tryInsertValue :: Variable -> Integer -> LocalCtx -> (Maybe EvaluateError, LocalCtx)
tryInsertValue variable value map = case lookup variable map of
    Nothing -> (Nothing, insert variable value map)
    Just _  -> (Just (MultipliedDefinition variable), map)

declareVariable :: (MonadState LocalCtx m) => Variable -> Integer -> m (Maybe EvaluateError)
declareVariable variable value = state $ \env -> tryInsertValue variable value env

tryModifyValue :: Variable -> Integer -> LocalCtx -> (Maybe EvaluateError, LocalCtx)
tryModifyValue variable value map = case lookup variable map of
    Just _  -> (Nothing, insert variable value map)
    Nothing -> (Just (UndefinedReference variable), map)

modifyVariable ::  (MonadState LocalCtx m) => Variable -> Integer -> m (Maybe EvaluateError)
modifyVariable name value = state $ \ctx -> tryModifyValue name value ctx

evalWithoutException :: (MonadState LocalCtx m, MonadIO m) => (Integer -> m (Maybe EvaluateError)) -> Expression -> m (Maybe EvaluateError)
evalWithoutException f expr = do
    ctx <- get
    let result = runReader (evaluate expr) ctx
    case result of
        (Left error) -> return (Just error)
        (Right r)    -> f r

printValue ::  (MonadState LocalCtx m, MonadIO m) => Integer -> m (Maybe EvaluateError)
printValue value =  do
    liftIO $ print value
    return Nothing

getInputValue :: (MonadIO m) => m (Either (ParseError (Token ByteString) Void) Integer)
getInputValue = do
    t <- liftIO getLine
    return $ runParser integer "stdin" (fromString t)

makeForStep :: (MonadState LocalCtx m, MonadIO m) => Variable -> Expression -> [Statement] -> m (Maybe EvaluateError)
makeForStep name end statements = do
    ctx <- get
    let curValue = ctx ! name
    case runReader (evaluate end) ctx of
        (Left error) -> return $ Just error
        (Right result) -> if curValue <= result
            then do
                t <- executeStatements statements
                case t of
                    Nothing -> do
                        modify (\aCtx -> intersection (insert name (curValue + 1) aCtx) ctx)
                        makeForStep name end statements
                    (Just error) -> return $ Just error
            else
                return Nothing

executeStatement :: (MonadState LocalCtx m, MonadIO m) => Statement -> m (Maybe EvaluateError)
executeStatement (Declaration var expr) = evalWithoutException (declareVariable var) expr
executeStatement (Assignment var expr) = evalWithoutException (modifyVariable var) expr
executeStatement (Output expr) = evalWithoutException printValue expr
executeStatement (Input var) = do
    ctx <- get
    if member var ctx
    then do
        input <- getInputValue
        case input of
            (Left err)     -> return $ Just (ParserError $ head $ errorPos err)
            (Right result) -> modifyVariable var result
    else return $ Just (UndefinedReference var)
executeStatement (For var start end statements) =  do
    ctx <- get
    startResult <- evalWithoutException (modifyVariable var) start
    case startResult of
        (Just error) -> return (Just error)
        Nothing      ->  makeForStep var end statements


executeStatementsInner :: (MonadState LocalCtx m, MonadIO m) => Integer -> [Statement] -> m (Maybe EvaluateError)
executeStatementsInner i [] = state $ \env -> (Nothing, env)
executeStatementsInner i (x : xs) =
    do
        ctx <- get
        res <- executeStatement x
        case res of
            Nothing    -> executeStatementsInner (i + 1) xs
            (Just err) -> return $ Just (ExecutionError i err)

executeStatements :: (MonadState LocalCtx m, MonadIO m) => [Statement] -> m (Maybe EvaluateError)
executeStatements = executeStatementsInner 1


executeSourceCode :: ByteString -> String -> IO()
executeSourceCode source label = do
    let result = runParser statementsParser label source
    case result of
        (Left error) -> do
                            putStrLn "Some error has occured while parins"
                            print (ParserError $ head $ errorPos error)
        (Right st) -> do
            (exitCode, ctx) <- runStateT (executeStatements st) (fromList [])
            case exitCode of
                Nothing    -> putStrLn "Program finished without errors"
                (Just err) -> print err

executeFromFile :: String -> IO ()
executeFromFile path = do
    source <- readFile path
    executeSourceCode (fromString source) path


