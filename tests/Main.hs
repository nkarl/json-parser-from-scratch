module Main where

import Prelude

import Control.Monad (void, (>=>))

-- import ExampleTest
import Lexer.LexingString
import System.Exit as Exit
import Test.HUnit (failures)

main :: IO ()
main = do
  result <- Lexer.LexingString.runTest
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
