module Common.Test where

import Test.Tasty

import Common.Term
import Common.Type
import Common.Type.Error
import Common.Test.Term
import Common.Test.Type

mkTests :: ( Eq ty
           , Show ty
           , Eq tm
           , Show tm
           , AsUnknownType e n
           )
        => TypeOutput e ty
        -> TermOutput e ty tm
        -> TestTree
mkTests ty tm = testGroup "tests"
  [ termTests tm
  , typeTests ty
  ]
