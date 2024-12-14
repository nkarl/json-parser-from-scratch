module Lexer.Lexing where

import Control.Lens
import Control.Lens.Traversal
import Control.Monad (void)
import Test.HUnit (Counts (failures), Test (..), assertBool, assertEqual, runTestTT)
import Test.HUnit as HUnit

import Lexer as LX

mockState :: [Char] -> LX.State Token
mockState text = (Source text, [])

testAlwaysFail0 =
  let
    input = mempty
    output = runLexer (mockState input)
    result = output ^. _Right . _2
    expect = [META RIGHT_BRACE]
   in
    assertEqual "ALWAYS fails. Must not lex a token list from the empty input" expect result

test0 =
  let
    input = mempty
    output = runLexer (mockState input)
    result = output ^? _Left
    expect = Just EOF
   in
    assertEqual "should branch Left with ErrorMsg when given an empty input (EOF)" expect result

tests =
  TestList
    [ TestCase testAlwaysFail0
    , TestCase test0
    ]

runTest :: IO Counts
runTest = runTestTT tests
