module Test.Term where

import Test.Tasty

import Test.Term.Text (textTests)
import Test.Term.Eval (evalTests)

termTests :: TestTree
termTests = testGroup "term" [
    textTests
  , evalTests
  ]
