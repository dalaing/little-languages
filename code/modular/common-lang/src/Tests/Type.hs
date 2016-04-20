{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Type (
    mkTypeTests
  ) where

import           Test.Tasty       (TestTree, testGroup)

import Component (ComponentOutput)
import           Tests.Type.Text  (mkTextTests)

mkTypeTests :: ( Eq (ty nTy)
               , Show (ty nTy)
               )
            => ComponentOutput r e ty nTy tm nTm a
            -> TestTree
mkTypeTests c =
  testGroup "type"
    [mkTextTests c]
