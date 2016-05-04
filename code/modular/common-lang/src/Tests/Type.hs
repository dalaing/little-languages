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

import           Test.Tasty              (TestTree, testGroup)
import           Text.Trifecta.Rendering (Span)

import           Component               (ComponentOutput)
import           Tests.Type.Text         (mkTextTests)

mkTypeTests :: ( Eq (ty Span)
               , Show (ty Span)
               )
            => ComponentOutput r e ty Span tm nTm a
            -> TestTree
mkTypeTests c =
  testGroup "type"
    [mkTextTests c]
