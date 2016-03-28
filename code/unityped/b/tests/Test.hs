module Main where

import Test.Tasty

import Test.Term (termTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
    termTests
  ]
