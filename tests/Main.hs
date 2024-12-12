module Main where

import Prelude

import Control.Monad (void, (>=>))

-- import ExampleTest
import Lexer.Simple
import System.Exit as Exit
import Test.HUnit (failures)

main :: IO ()
main = do
  result <- Lexer.Simple.runTest
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
