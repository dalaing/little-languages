{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Term (
    mkTermTests
  ) where

import Data.Proxy (Proxy)

import           Test.Tasty                             (TestTree, testGroup)
import           Text.Trifecta.Rendering                (Span)
import Data.Constraint.Forall (ForallT)

import           Component                              (ComponentOutput)
import           Component.Type.Error.UnknownType.Class (AsUnknownType (..))
import           Tests.Term.Eval                        (mkEvalTests)
import           Tests.Term.Infer                       (mkInferTests)
import           Tests.Term.Text                        (mkTextTests)
import Extras (Eq1, Show1, Eq2, Show2, Eq3, Show3, Monoid2)

mkTermTests :: ( Eq3 tm
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
            => ComponentOutput r e ty tm
            -> Proxy nTy
            -> Proxy nTm
            -> TestTree
mkTermTests c pTy pTm =
  testGroup "term"
    [ mkTextTests c
    , mkEvalTests c pTy pTm
    , mkInferTests c pTy pTy
    ]
