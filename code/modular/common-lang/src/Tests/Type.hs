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

import Data.Proxy (Proxy)

import           Test.Tasty              (TestTree, testGroup)
import           Text.Trifecta.Rendering (Span)

import           Component               (ComponentOutput)
import           Tests.Type.Text         (mkTextTests)
import Extras (Eq1, Show1)

mkTypeTests :: ( Eq1 ty
               , Show1 ty
               )
            => ComponentOutput r e ty tm
            -> Proxy nTy
            -> TestTree
mkTypeTests c _ =
  testGroup "type"
    [mkTextTests c]
