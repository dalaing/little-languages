module Common.Test.Type where

import Test.Tasty

import Common.Type

import Common.Test.Type.Text

typeTests :: ( Eq ty
             , Show ty
             )
          => TypeOutput e ty
          -> TestTree
typeTests t = testGroup "type"
  [textTests t]
