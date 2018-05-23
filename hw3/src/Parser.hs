{-# LANGUAGE OverloadedStrings #-}

module Parser (
    integer,
    parseExpression,
    statementsParser,
    ParseError,
    runParser,
    errorPos,
    Token
) where

import           Control.Monad              (void)
import           Core                       (Constant, EvaluateError (..),
                                             Expression (..), LocalCtx, Statement (..),
                                             Variable)
import           Data.ByteString            (ByteString, pack)
import qualified Data.ByteString.UTF8       as S8
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParseError, Parsec, between, many,
                                             notFollowedBy, runParser, try, (<|>))
import           Text.Megaparsec.Byte       (alphaNumChar, letterChar, space1, string)
import           Text.Megaparsec.Byte.Lexer as L (decimal, lexeme, skipBlockComment,
                                                  skipLineComment, space, symbol)
import           Text.Megaparsec.Error      (errorPos)
import           Text.Megaparsec.Expr       (Operator (InfixL), makeExprParser)
import           Text.Megaparsec.Stream     (Token)

type Parser = Parsec Void ByteString

sc :: Parser ()
sc = L.space (space1 <|> void (mySymbol "\n"))  (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

myLexeme :: Parser a -> Parser a
myLexeme = L.lexeme sc

mySymbol :: ByteString -> Parser ByteString
mySymbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (mySymbol "(") (mySymbol ")")

integer :: Parser Integer
integer = myLexeme L.decimal

rword :: ByteString -> Parser ()
rword w = myLexeme (string w *> notFollowedBy alphaNumChar)

rws :: [ByteString] -- list of reserved words
rws = ["let", "mut", "for", "in", "break"]

identifier :: Parser ByteString
identifier = (myLexeme . try) (p >>= check)
  where
    p       = pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


operators :: [[Operator Parser Expression]]
operators =
  [ [ InfixL (Mul <$ mySymbol "*")
    , InfixL (Div  <$ mySymbol "/") ]
  , [ InfixL (Add    <$ mySymbol "+")
    , InfixL (Sub<$ mySymbol "-") ]
  ]

exprParser :: Parser Expression
exprParser = makeExprParser termParser operators

termParser :: Parser Expression
termParser = parens (exprParser <|> letParser)
  <|> Var <$> identifier
  <|> Lit <$> integer

letParser :: Parser Expression
letParser = Let <$> (rword "let" *> identifier <* mySymbol "=") <*> exprParser <* rword "in" <*> exprParser

parseExpression :: ByteString -> Either (ParseError (Text.Megaparsec.Stream.Token ByteString) Void) Expression
parseExpression = runParser exprParser ""

statementParser :: Parser Statement
statementParser = many (mySymbol "\n") *> assignmentParser <|> declarationParser <|> outputParser <|> inputParser <|> forParser

assignmentParser :: Parser Statement
assignmentParser = Assignment <$> (identifier <* mySymbol "=") <*> exprParser

declarationParser :: Parser Statement
declarationParser = Declaration <$> (rword "mut" *> identifier <* mySymbol "=") <*> exprParser

outputParser :: Parser Statement
outputParser = Output <$> (mySymbol "<" *> exprParser)

inputParser :: Parser Statement
inputParser = Input <$> (mySymbol ">" *> identifier)

statementsParser :: Parser [Statement]
statementsParser =  many (mySymbol "\n") *> many statementParser

forParser :: Parser Statement
forParser = For <$> (rword "for" *> identifier <* mySymbol "=") <*> (exprParser <* mySymbol "to") <*>
    exprParser <*> (mySymbol "{" *> statementsParser <* mySymbol "}")
