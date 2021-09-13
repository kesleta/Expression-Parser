module Evaluator
  ( blankState
  , runState
  , evalState
  , execState
  , eval
  ) where

import           Control.Monad.State.Lazy       ( MonadState(state)
                                                , State
                                                , evalState
                                                , execState
                                                , runState
                                                )
import qualified Data.Map                      as M
import           Data.Maybe
import           Parser
import           Tokenizer

type SymTab = M.Map String Int

blankState :: M.Map String Int
blankState = M.empty

{----------------------------------}
eval :: ParseTree -> State SymTab Int
eval (NumNode n         ) = return n
eval (NegNode x         ) = (0 -) <$> eval x
eval (AddNode  x y      ) = (+) <$> eval x <*> eval y
eval (SubNode  x y      ) = (-) <$> eval x <*> eval y
eval (MultNode x y      ) = (*) <$> eval x <*> eval y
eval (DivNode  x y      ) = div <$> eval x <*> eval y
eval (IdentNode str     ) = getValue str
eval (AssignNode str v  ) = putValue str v
eval (PrgrmNode [t     ]) = eval t
eval (PrgrmNode (t : ts)) = eval t >> eval (PrgrmNode ts)

getValue :: String -> State SymTab Int
getValue str = state $ \s -> (catch $ M.lookup str s, s)
  where catch = fromMaybe (error $ concat ["\"", str, "\"", " does not exist"])

putValue :: String -> ParseTree -> State SymTab Int
putValue str v = state $ \s -> (vResult s, M.insert str (vResult s) (vState s))
 where
  vResult = evalState $ eval v
  vState  = execState $ eval v
