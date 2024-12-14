module Lexer.Simple where

import Control.Lens
import Control.Lens.Traversal
import Control.Monad (void)
import Test.HUnit (Counts (failures), Test (..), assertBool, assertEqual, runTestTT)

import Lexer as LX

mockState :: [Char] -> LX.State
mockState text = (Source text, [])

testAlwaysFail =
  let
    input = "\""
    output = LX.poopString (mockState input) (Just LX.DBL_QUOTE) mempty
    result = output ^. _Right . _2
    expect = [LX.MONO LX.RIGHT_BRACE]
   in
    assertEqual "should fail to tokenize `RIGHT_BRACE` from the input `\"`" expect result

test0 =
  let
    input = "abc\""
    output = LX.poopString (mockState input) (Just LX.DBL_QUOTE) mempty
    result = output ^. _Right . _2
    expect = [LX.POLY $ LX.STRING "abc", LX.MONO LX.DBL_QUOTE]
   in
    assertEqual "should tokenize `abc` from the input `abc\"`" expect result
tests =
  TestList
    [ TestCase testAlwaysFail
    , TestCase test0
    ]

runTest :: IO Counts
runTest = runTestTT tests
