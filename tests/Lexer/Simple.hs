module Lexer.Simple where

import Control.Lens
import Control.Lens.Traversal
import Control.Monad (void)
import Test.HUnit (Counts (failures), Test (..), assertBool, assertEqual, runTestTT)

import Lexer as LX

mockState :: [Char] -> LX.State
mockState text = (Source text, [])

testAlwaysFail =
  let output = LX.poopString (mockState "\"") (Just LX.DBL_QUOTE) mempty
      result = output ^. _Right . _2
      expect = LX.MONO LX.RIGHT_BRACE
   in assertEqual "failing test: should tokenize `BRACE` from the input `\"`" expect (result !! 1)

tests =
  TestList
    [ TestCase testAlwaysFail
    ]

runTest :: IO Counts
runTest = runTestTT tests
