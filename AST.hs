module AST where

data Node =
  Initialization String Int |
  Variable String (Maybe Node) |
  Succ Node Node |
  Pred Node Node |
  While Node [Node] |
  Print Node |
  Program [Node]
  deriving (Show)
