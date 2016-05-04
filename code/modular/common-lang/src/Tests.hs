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

import           Test.Tasty                             (TestTree, testGroup)
import           Text.Trifecta.Rendering                (Span)

import           Component                              (ComponentOutput)
import           Component.Type.Error.UnknownType.Class (AsUnknownType)
import           Tests.Term                             (mkTermTests)
import           Tests.Type                             (mkTypeTests)

mkTests :: ( Eq e
           , Show e
           , Eq (ty Span)
           , Show (ty Span)
           , Eq (tm Span Span String)
           , Show (tm Span Span String)
           , AsUnknownType e
           , Monoid r
           )
        => ComponentOutput r e ty Span tm Span String
        -> TestTree
mkTests c =
  testGroup "tests"
    [ mkTypeTests c
    , mkTermTests c
    ]
