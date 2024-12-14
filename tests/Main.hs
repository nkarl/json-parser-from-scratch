module Main where

import Prelude

import Control.Monad (void, (>=>))

-- import ExampleTest

import Lexer.Lexing
import Lexer.LexingString
import System.Exit as Exit
import Test.HUnit (failures)

main :: IO ()
main = do
  testLexingString <- Lexer.LexingString.runTest
  testLexing <- Lexer.Lexing.runTest
  let result = foldr ((+) . failures) 0 [testLexingString, testLexing]
  if result > 0 then Exit.exitFailure else Exit.exitSuccess
