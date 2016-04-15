module Main (
    main
  ) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Type (typeTests)
import Test.Term (termTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" 
  [ typeTests
  , termTests
  ]

