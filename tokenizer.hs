{-# LANGUAGE LambdaCase #-}

module Tokenizer where

import           Data.Char

data Operator
  = Add
  | Sub
  | Mult
  | Div
  deriving (Eq, Show)

data Token
  = TokInt Int
  | TokIdent String
  | TokOp Operator
  | TokAssign
  | TokEnd
  | TokLParen
  | TokRParen
  deriving (Eq, Show)

getTokOp :: Token -> Maybe Operator
getTokOp = \case
  TokOp x -> Just x
  _       -> Nothing

getTokInt :: Token -> Maybe Int
getTokInt = \case
  TokInt x -> Just x
  _        -> Nothing

getTokIdent :: Token -> Maybe String
getTokIdent = \case
  TokIdent x -> Just x
  _          -> Nothing

{----------------------------------}

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs) | isSpace x       = tokenize xs
                  | isDigit x       = integer (x : xs)
                  | isAlpha x       = idenitfier (x : xs)
                  | x == '#'        = comment (x : xs)
                  | x `elem` "+-*/" = TokOp (operator x) : tokenize xs
                  | x == '='        = TokAssign : tokenize xs
                  | x == ';'        = TokEnd : tokenize xs
                  | x == '('        = TokLParen : tokenize xs
                  | x == ')'        = TokRParen : tokenize xs
 where
  integer :: String -> [Token]
  integer xs = TokInt (read digs) : tokenize rest
    where (digs, rest) = span isDigit xs

  idenitfier :: String -> [Token]
  idenitfier xs = TokIdent ident : tokenize rest
    where (ident, rest) = span isAlphaNum xs

  comment :: String -> [Token]
  comment = tokenize . dropWhile (/= '\n')

  operator :: Char -> Operator
  operator x | x == '+' = Add
             | x == '-' = Sub
             | x == '*' = Mult
             | x == '/' = Div
