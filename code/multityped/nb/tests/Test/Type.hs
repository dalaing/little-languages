module Test.Type (
    typeTests
  ) where

import Test.Tasty

import Test.Type.Text (textTests)
import Test.Type.Infer (inferTests)
import Test.Type.Safety (safetyTests)

typeTests :: TestTree
typeTests = testGroup "type" [
    textTests
  , inferTests
  , safetyTests
  ]
