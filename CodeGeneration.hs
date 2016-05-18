module CodeGeneration (source) where

  import Data.Maybe
  import AST
  import Control.Monad.State

  type Context = Int

  getLabels :: Int -> State Context Int
  getLabels n = state $ \ctx -> (ctx, ctx + n)

  execute :: Node -> State Context String
  execute (Program nodes) = sourceBody nodes >>= \code -> return $ code ++ "halt\n"
  execute (Initialization _ offset) = return ("pusha " ++ show offset ++ "\npushi 0\nstorei\n")
  execute (Succ left right) = return (address left ++ value right ++ "pushi 1\naddi\nstorei\n")
  execute (Pred left right) = return (address left ++ value right ++ "pushi 1\nsubi\nstorei\n")
  execute (Print var) = return (value var ++ "outi\n")
  execute (While var body) = do
    labels <- getLabels 2
    body' <- sourceBody body
    return $ "label" ++ show labels ++ ":\n" ++ value var ++ "jz label" ++ (show $ labels + 1) ++ "\n" ++ body' ++ "jmp label" ++ show labels ++ "\nlabel" ++ (show $ labels + 1) ++ ":\n"

  sourceBody :: [Node] -> State Context String
  sourceBody = foldM appendCode ""

  appendCode :: String -> Node -> State Context String
  appendCode code node = execute node >>= \toAppend -> return $ code ++ toAppend

  address :: Node -> String
  address (Variable _ definition) = let (Initialization _ varAddress) = fromJust definition in "pusha " ++ show varAddress ++ "\n"

  value :: Node -> String
  value (Variable name definition) = address (Variable name definition) ++ "loadi\n"

  source :: Node -> String
  source node = let (code, _) = runState (execute node) 1 in code
