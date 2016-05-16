module Parser (parseSource) where

  import Text.ParserCombinators.Parsec
  import Lexing
  import AST

  program :: Parser Node
  program = do
    nodes <- many1 node
    eof
    return $ Program nodes

  sentence :: Parser Node
  sentence = do
    node <- try initVar <|> try predVar <|> try succVar <|> try printVar
    parseSemiColon
    return node

  while :: Parser Node
  while = do
    parseWhile
    (Id varName) <- between parseOpenParenth parseCloseParenth parseId
    nodes <- between parseOpenBracket parseCloseBracket (many node)
    return $ While (Variable varName Nothing) nodes

  node :: Parser Node
  node = try sentence <|> while

  initVar :: Parser Node
  initVar = do
    (Id varName) <- parseId
    parseAssignment
    char '0'
    return $ Initialization varName 0

  predVar :: Parser Node
  predVar = do
    (Id left) <- parseId
    parseAssignment
    parsePred
    (Id right) <- between parseOpenParenth parseCloseParenth parseId
    return $ Pred (Variable left Nothing) (Variable right Nothing)


  succVar :: Parser Node
  succVar = do
    (Id left) <- parseId
    parseAssignment
    parseSucc
    (Id right) <- between parseOpenParenth parseCloseParenth parseId
    return $ Pred (Variable left Nothing) (Variable right Nothing)

  printVar :: Parser Node
  printVar = do
    parsePrint
    (Id varName) <- parseId
    return $ Print (Variable varName Nothing)

  parseSource :: String -> Node
  parseSource input = case parse program "Program" input of
    Right node -> node
    Left err -> error $ show err
