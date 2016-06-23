{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Common (
    commonTests
  ) where

-- from 'tasty'
import           Test.Tasty     (TestTree, testGroup)

-- local
import           Test.Common.Text (textTests)

commonTests :: TestTree
commonTests =
  testGroup "common"
    [textTests]
