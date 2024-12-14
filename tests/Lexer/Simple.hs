module Lexer.Simple where

import Control.Lens
import Control.Lens.Traversal
import Control.Monad (void)
import Test.HUnit (Counts (failures), Test (..), assertBool, assertEqual, runTestTT)

import Lexer as LX

mockState :: LX.SourceInput -> LX.State
mockState source = (source, [])

testAlwaysFail =
  let output = LX.lexString (mockState "\"") (Just LX.DBLQUOTE) mempty
      result = output ^. _Right . _2
      expect = LX.Wrapper LX.BRACE
   in assertEqual "failing test: should tokenize `BRACE` from '\"'" expect (result !! 1)

tests =
  TestList
    [ TestCase testAlwaysFail
    ]

runTest :: IO Counts
runTest = runTestTT tests
