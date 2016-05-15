module Identification(identifyTree) where

  import AST
  import Control.Monad.State
  import Data.List

  type SymbolTable = [Node]

  insertDef :: Node -> SymbolTable -> SymbolTable
  insertDef definition st = definition : st

  findDef :: String -> SymbolTable -> Maybe Node
  findDef name = find (\def -> let (Initialization defName _) = def in defName == name)

  identify :: Node -> State SymbolTable Node

  identify (Variable name _) = state $ \st -> ((Variable name $ findDef name st), st)

  identify (Initialization name offset) = state $ \st -> (Initialization name offset, insertDef (Initialization name offset) st)

  identify (Succ left right) = do
    left' <- identify left
    right' <- identify right
    return $ Succ left' right'

  identify (Pred left right) = do
    left' <- identify left
    right' <- identify right
    return $ Pred left' right'

  identify (While var body) = do
    var' <- identify var
    body' <- mapM identify body
    return $ While var' body'

  identify (Print var) = identify var >>= \var' -> return $ Print var'

  identify (Program nodes) = mapM identify nodes >>= (\nodes' -> return $ Program nodes')

  identifyTree :: Node -> Node
  identifyTree node = let (node', _) = runState (identify node) [] in node'
