module Parser
  ( parse
  , ParseTree(..)
  ) where

import           Data.Char
import           Parserlib
import           Tokenizer

{----------------------------------}

data ParseTree = AddNode  ParseTree ParseTree
          | PrgrmNode   [ParseTree]
          | AssignNode String ParseTree
          | IdentNode  String
          | SubNode    ParseTree ParseTree
          | MultNode   ParseTree ParseTree
          | DivNode    ParseTree ParseTree
          | NegNode    ParseTree
          | NumNode    Int
          | EndNode
    deriving (Show, Eq)

type TokParser = Parser Token ParseTree

{-
Prgrm <- Expr ';' | Expr ';' Prgrm
Expr <- Term (('+' | '-') term)* | Ident '=' Expr
Term <- Factor (('*' | '/') Factor)*
Factor <- Int | '(' Expr ')' | '(' '-' Factor ')' | Ident
-}

parse :: [Token] -> ParseTree
parse = (\(Just (_, x)) -> x) . runParser prgrm

--

prgrm :: TokParser
prgrm = PrgrmNode <$> many (expr <* itemP TokEnd)

expr :: TokParser
expr = chainl1 term (addP <|> subP) <|> assignP
 where
  addP = AddNode <$ itemP (TokOp Add)
  subP = SubNode <$ itemP (TokOp Sub)
  assignP =
    (\x _ y -> AssignNode x y)
      <$> fieldP getTokIdent
      <*> itemP TokAssign
      <*> expr

term :: TokParser
term = chainl1 factor (multP <|> divP)
 where
  multP = MultNode <$ itemP (TokOp Mult)
  divP  = DivNode <$ itemP (TokOp Div)

factor :: TokParser
factor = intP <|> parExprP <|> negP <|> identP
 where
  intP     = NumNode <$> fieldP getTokInt
  parExprP = itemP TokLParen *> expr <* itemP TokRParen
  identP   = IdentNode <$> fieldP getTokIdent
  negP =
    NegNode
      <$> (itemP TokLParen *> itemP (TokOp Sub) *> factor <* itemP TokRParen)
