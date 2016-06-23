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

import Data.Proxy (Proxy)

import           Test.Tasty                             (TestTree, testGroup)
import           Text.Trifecta.Rendering                (Span)
import Data.Constraint.Forall (ForallT)

import           Component                              (ComponentOutput)
import           Component.Type.Error.UnknownType.Class (AsUnknownType)
import           Tests.Term                             (mkTermTests)
import           Tests.Type                             (mkTypeTests)
import Extras (Eq1, Eq2, Eq3, Show1, Show2, Show3, Monoid2)

mkTests :: ( Eq3 tm
           , Eq1 ty
           , Eq2 e
           , Eq nTy
           , Eq nTm
           , Show3 tm
           , Show1 ty
           , Show2 e
           , Show nTy
           , Show nTm
           , AsUnknownType e
           , Monoid2 r
           )
        => Proxy nTy
        -> Proxy nTm
        -> ComponentOutput r e ty tm
        -> TestTree
mkTests pTy pTm c =
  testGroup "tests"
    [ mkTypeTests c pTy
    , mkTermTests c pTy pTm
    ]
