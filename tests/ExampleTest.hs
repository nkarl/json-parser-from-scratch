module ExampleTest where

import Control.Monad (void)
import Test.HUnit (Counts (failures), Test (..), assertEqual, runTestTT)

tests =
  TestList
    [ TestCase
        ( let expected = 42
              actual = -1
           in assertEqual "should be equal" expected actual
        )
    , TestCase
        ( let expected = Just 42
              actual = Nothing
           in assertEqual "should be equal" expected actual
        )
    ]

runTest :: IO Counts
runTest = runTestTT tests
