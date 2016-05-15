module Parser(parseSource) where

import Text.ParserCombinators.Parsec
import AST

program :: Parser Node
program = do
  nodes <- many1 node
  eof
  return $ Program nodes

sentence :: Parser Node
sentence = do
  node <- try initVar <|> try predVar <|> try succVar <|> try printVar
  skipMany space
  char ';'
  skipMany space
  return node

while :: Parser Node
while = do
  string "while"
  skipMany space
  varName <- between (char '(' >> skipMany space) (char ')' >> skipMany space) parseId
  nodes <- between (char '{' >> skipMany space) (char '}' >> skipMany space) (many node)
  return $ While (Variable varName Nothing) nodes

node :: Parser Node
node = try sentence <|> while

initVar :: Parser Node
initVar = do
  varName <- parseId
  skipMany space
  string ":="
  skipMany space
  char '0'
  skipMany space
  return $ Initialization varName 0

predVar :: Parser Node
predVar = do
  left <- parseId
  skipMany space
  string ":="
  skipMany space
  string "pred"
  skipMany space
  right <- between (char '(' >> skipMany space) (char ')' >> skipMany space) parseId
  return $ Pred (Variable left Nothing) (Variable right Nothing)

succVar :: Parser Node
succVar = do
  left <- parseId
  skipMany space
  string ":="
  skipMany space
  string "succ"
  skipMany space
  right <- between (char '(' >> skipMany space) (char ')' >> skipMany space) parseId
  return $ Succ (Variable left Nothing) (Variable right Nothing)

printVar :: Parser Node
printVar = do
  string "print"
  skipMany space
  varName <- parseId
  return $ Print (Variable varName Nothing)

parseId :: Parser String
parseId = many1 $ noneOf ":={}() "

parseSource :: String -> Node
parseSource input = case parse program "Program" input of
  Right node -> node
  Left err -> error $ show err
