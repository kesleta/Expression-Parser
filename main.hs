module Main where
import           Evaluator
import           Parser
import           Tokenizer

main = do
  file <- readFile "program.txt"
  print $ evalState ((eval . parse . tokenize) file) blankState



debug = do
  file <- readFile "program.txt"
  let toks = tokenize file
  print toks
  let tree = parse toks
  print tree
  let (v, s') = runState (eval tree) blankState
  print v
  putStrLn $ dropWhile (/= '[') $ show s'
