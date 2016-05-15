module Main where

import Compiler
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  compileOut (head args) (head $ tail args)
