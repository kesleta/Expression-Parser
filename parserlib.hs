{-# LANGUAGE LambdaCase #-}

module Parserlib
  ( module Parserlib
  , module Control.Applicative
  , module Control.Monad
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Graphics.Win32                 ( Menu )

newtype Parser a b =
  Parser
    { runParser :: [a] -> Maybe ([a], b)
    }

instance Monad (Parser a) where
  return x = Parser $ \input -> Just (input, x)
  (Parser p) >>= f = Parser $ \input -> case p input of
    Nothing          -> Nothing
    Just (input', x) -> runParser (f x) input'

instance Applicative (Parser a) where
  pure  = return
  (<*>) = ap

instance Functor (Parser a) where
  fmap = liftM

instance Alternative (Parser a) where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \input -> case runParser p1 input of
    Nothing -> runParser p2 input
    res     -> res

instance MonadPlus (Parser a) where
  mzero = empty
  mplus = (<|>)

{----------------------------------}
parseIf :: (a -> Bool) -> Parser a a
parseIf f = Parser $ \case
  x : xs | f x       -> Just (xs, x)
         | otherwise -> Nothing
  [] -> Nothing

spanP :: (a -> Bool) -> Parser a [a]
spanP = many . parseIf

item :: Parser a a
item = parseIf $ const True

itemP :: (Eq a) => a -> Parser a a
itemP = parseIf . (==)

itemsP :: (Eq a) => [a] -> Parser a [a]
itemsP = traverse itemP

notNull :: Parser a [b] -> Parser a [b]
notNull (Parser p) = Parser $ \input -> case p input of
  Just (_, []) -> Nothing
  other        -> other

sepBy :: Parser a b -> Parser a c -> Parser a [c]
sepBy sep el = (:) <$> el <*> many (sep *> el)

fieldP :: (a -> Maybe b) -> Parser a b
fieldP sel = Parser $ \case
  cont : rest -> (,) rest <$> sel cont
  []          -> Nothing

chainl :: Parser a b -> Parser a (b -> b -> b) -> b -> Parser a b
chainl p op d = chainl1 p op <|> return d

chainl1 :: Parser a b -> Parser a (b -> b -> b) -> Parser a b
chainl1 p op = p >>= rest
 where
  rest x =
    do
      o <- op
      y <- p
      rest (x `o` y)
    <|> return x

chainr :: Parser a b -> Parser a (b -> b -> b) -> b -> Parser a b
chainr p op d = chainr1 p op <|> return d

chainr1 :: Parser a b -> Parser a (b -> b -> b) -> Parser a b
chainr1 p op = scan
 where
  scan = p >>= rest
  rest x =
    do
      f <- op
      f x <$> scan
    <|> return x
