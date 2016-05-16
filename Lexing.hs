module Lexing where

  import Text.ParserCombinators.Parsec

  data Token =
    Id String |
    Assignment |
    SuccToken |
    PredToken |
    PrintToken |
    OpenParenth |
    CloseParenth |
    SemiColon |
    WhileToken |
    OpenBracket |
    CloseBracket
    deriving (Show)

  parseId :: Parser Token
  parseId = keyword (many1 $ noneOf ":={}(); ") >>= \parsed -> return $ Id parsed

  parseAssignment :: Parser Token
  parseAssignment = reserved (string ":=") >> return Assignment

  parseSucc :: Parser Token
  parseSucc = keyword (string "succ") >> return SuccToken

  parsePred :: Parser Token
  parsePred = keyword (string "pred") >> return PredToken

  parsePrint :: Parser Token
  parsePrint = keyword (string "print") >> return PrintToken

  parseOpenParenth :: Parser Token
  parseOpenParenth = reserved (char '(') >> return OpenParenth

  parseCloseParenth :: Parser Token
  parseCloseParenth = reserved (char ')') >> return CloseParenth

  parseSemiColon :: Parser Token
  parseSemiColon = reserved (char ';') >> return SemiColon

  parseWhile :: Parser Token
  parseWhile = keyword (string "while") >> return WhileToken

  parseOpenBracket :: Parser Token
  parseOpenBracket = reserved (char '{') >> return OpenBracket

  parseCloseBracket :: Parser Token
  parseCloseBracket = reserved (char '}') >> return CloseBracket

  optSpaces :: Parser ()
  optSpaces = skipMany space

  keyword :: Parser a -> Parser a
  keyword = between spaces spaces

  reserved :: Parser a -> Parser a
  reserved = between optSpaces optSpaces
