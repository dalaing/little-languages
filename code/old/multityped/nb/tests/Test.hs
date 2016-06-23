module Main where

import Test.Tasty

import Test.Term (termTests)
import Test.Type (typeTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
        termTests
      , typeTests
      ]

