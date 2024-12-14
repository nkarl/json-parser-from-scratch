module Lexer.LexingString where

import Control.Lens
import Control.Lens.Traversal
import Control.Monad (void)
import Test.HUnit (Counts (failures), Test (..), assertBool, assertEqual, runTestTT)
import Test.HUnit as HUnit

import Lexer as LX

mockState :: [Char] -> LX.State
mockState text = (Source text, mempty)

testAlwaysFail0 =
  let
    input = "\""
    output = lexString (mockState input) (Just DBL_QUOTE) mempty
    result = output ^. _Right . _2
    expect = [META RIGHT_BRACE]
   in
    assertEqual "this test ALWAYS fails. Should not tokenize `RIGHT_BRACE` from the input `\"`" expect result

test0 =
  let
    input = mempty
    output = lexString (mockState input) (Just DBL_QUOTE) mempty
    result = output ^? _Left
    expect = Just $ Unterminated (META DBL_QUOTE)
   in
    assertEqual "should branch Left with ErrorMsg when input string is not terminated" expect result

test1 =
  let
    input = "abc\""
    output = lexString (mockState input) (Just DBL_QUOTE) mempty
    result = output ^. _Right . _2
    expect = [STRING "abc", META DBL_QUOTE]
   in
    assertEqual "should tokenize `abc` from the input `abc\"`" expect result

tests =
  TestList
    [ TestCase testAlwaysFail0
    , TestCase test0
    , TestCase test1
    ]

runTest :: IO Counts
runTest = runTestTT tests
