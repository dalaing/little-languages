{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests (
    mkTests
  ) where

import           Test.Tasty                          (TestTree, testGroup)

import           Component.Type.Error.UnknownType.Class (AsUnknownType)
import           Component                           (ComponentOutput)
import           Tests.Term                          (mkTermTests)
import           Tests.Type                          (mkTypeTests)

mkTests :: ( Eq e
           , Show e
           , Eq (ty nTy)
           , Show (ty nTy)
           , Eq (tm nTm a)
           , Show (tm nTm a)
           , AsUnknownType e
           , Monoid r
           )
        => ComponentOutput r e ty nTy tm nTm a
        -> TestTree
mkTests c =
  testGroup "tests"
    [ mkTypeTests c
    , mkTermTests c
    ]
