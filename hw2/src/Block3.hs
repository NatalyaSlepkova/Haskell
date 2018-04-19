{-# LANGUAGE InstanceSigs #-}
module Block3 where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Maybe

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f r = Parser (fmap (first f) . runParser r)

instance Applicative (Parser s) where
  pure :: x -> Parser s x
  pure x = Parser $ \s -> Just (x, s)
  (<*>) :: Parser s (x -> y) -> Parser s x -> Parser s y
  a1 <*> a2 = Parser (runParser a1 >=> (\ (f, s1) -> runParser a2 s1 >>= 
      \ (x, s2) -> Just (f x, s2)))

instance Monad (Parser s) where
  return = pure
  (>>=) :: Parser s x -> (x -> Parser s y) -> Parser s y
  p >>= f = Parser (runParser p >=> (\(x, sx) -> runParser (f x) sx))

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (const Nothing)
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser a1) (Parser a2) = Parser (\s -> a1 s <|> a2 s)

ok :: Parser s ()
ok = Parser (\s -> Just ((), s))

eof :: Parser s ()
eof = Parser $ \s -> case s of
    [] -> Just ((), s)
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser f
  where
  f [] = Nothing
  f (x:xs)
    | p x = Just (x, xs)
    | otherwise = Nothing

stream :: Eq s => [s] -> Parser s [s]
stream x = Parser $ \s -> case x of
    (a:b) -> runParser (element a) s >>= 
      \(a, t) -> runParser (stream b) t >>= 
        \(n, f) -> Just(a : n, f)
    [] -> Just([], s)

char :: Char -> Parser Char Char
char c = satisfy (== c)

element :: Eq s => s -> Parser s s
element x = satisfy (== x)


spaces :: Parser Char String
spaces = many (satisfy isSpace)

integer :: Parser Char Integer
integer = Parser (fmap (first read) . runParser (some (satisfy isDigit)))

sign :: Num p => Char -> p -> p
sign s v = if s == '-' then -v else v

onlySignedInteger :: Parser Char Integer
onlySignedInteger =  fmap sign (element '+' <|> element '-') <*> integer

notOnlySignedInteger :: Parser Char Integer
notOnlySignedInteger =  fmap sign (element '+' <|> element '-' <|> pure ' ') <*> integer

  
brackets = (element '(' *> brackets *> element ')') *> brackets <|> ok

spacedInteger :: Parser Char Integer
spacedInteger = spaces *> notOnlySignedInteger <* spaces

commaSpacedInteger :: Parser Char Integer
commaSpacedInteger = element ',' *> spacedInteger

num 0 _    = Parser (\s -> Just ([], s))
num n elem = (:) <$> elem <*> num (n - 1) elem

parseListOfLists :: Parser Char [[Integer]]
parseListOfLists = let l = spacedInteger >>= \n -> num n commaSpacedInteger in
    fmap (:) l <*> many (element ',' *> l)