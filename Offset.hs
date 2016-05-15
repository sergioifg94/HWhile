module Offset(setOffset) where

  import AST
  import Control.Monad.State

  type CurrentOffset = Int

  offset :: Node -> State CurrentOffset Node

  offset (Initialization name _) = state $ \current -> (Initialization name current, current + 2)

  offset (Variable name _) = state $ \current -> (Variable name Nothing, current)

  offset (Succ left right) = do
    left' <- offset left
    right' <- offset right
    return $ Succ left' right'

  offset (Pred left right) = do
    left' <- offset left
    right' <- offset right
    return $ Pred left' right'

  offset (While var body) = do
    var' <- offset var
    body' <- mapM offset body
    return $ While var' body'

  offset (Program body) = mapM offset body >>= \nodes -> return $ Program nodes

  offset (Print var) = offset var >>= \var' -> return $ Print var'

  setOffset :: Node -> Node
  setOffset node = let (node', _) = runState (offset node) 0 in node'
