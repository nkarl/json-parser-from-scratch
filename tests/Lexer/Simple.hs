module Lexer.Simple where

import Control.Lens
import Control.Monad (void)
import Test.HUnit (Counts (failures), Test (..), assertBool, assertEqual, runTestTT)

import LexerSimple (WRAP (..), lexString)

testFail =
  let result = lexString "\"" DBLQUOTE mempty
      expect = "abc"
   in assertEqual "failing test: should not parse `abc` from '\"'" expect (result ^. _2)

test0 =
  let result = lexString "\"" DBLQUOTE mempty
      expect = "abc"
   in assertBool "should not parse `abc` from '\"'" (expect /= (result ^. _2))

test1 =
  let result = lexString "abc\"" DBLQUOTE mempty
      expect = "abc"
   in assertEqual "should parse `abc` from 'abc\"'" expect (result ^. _2)

tests =
  TestList
    [ TestCase testFail
    , TestCase test0
    , TestCase test1
    ]

runTest :: IO Counts
runTest = runTestTT tests
