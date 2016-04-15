{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Type (
    typeTests
  ) where

import Test.Tasty (TestTree, testGroup)

import Test.Type.Text (textTests)

typeTests :: TestTree
typeTests =
  testGroup "type"
    [textTests]
