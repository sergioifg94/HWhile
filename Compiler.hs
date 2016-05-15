module Compiler (compileOut) where

  import AST
  import Parser
  import Identification
  import Offset
  import CodeGeneration

  import System.Environment

  compile :: String -> String
  compile input = source $ identifyTree $ setOffset $ parseSource input

  compileOut :: FilePath -> FilePath -> IO ()
  compileOut input output = readFile input >>= \inputSrc -> writeFile output (compile inputSrc)
