module Lexer.Simple where

import Control.Lens
import Control.Lens.Traversal
import Control.Monad (void)
import Test.HUnit (Counts (failures), Test (..), assertBool, assertEqual, runTestTT)

import LexerSimple (WRAP (..), lexString)
import LexerSimple as Lexer

mockState :: Lexer.SourceInput -> Lexer.State
mockState source = (source, [])

testAlwaysFail =
  let output = lexString (mockState "\"") (Just DBLQUOTE) mempty
      result = output ^. _Right . _2
      expect = Wrapper BRACE
   in assertEqual "failing test: should tokenize `BRACE` from '\"'" expect (result!!1)

tests =
  TestList
    [ TestCase testAlwaysFail
    ]

runTest :: IO Counts
runTest = runTestTT tests
