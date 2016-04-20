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

import           Test.Tasty                          (TestTree, testGroup)

import           Component.Type.Error.UnknownType.Class (AsUnknownType (..))
import           Component                           (ComponentOutput)
import           Tests.Term.Eval                     (mkEvalTests)
import           Tests.Term.Infer                    (mkInferTests)
import           Tests.Term.Text                     (mkTextTests)

mkTermTests :: ( Eq e
               , Show e
               , Eq (tm nTm a)
               , Show (tm nTm a)
               , Eq (ty nTy)
               , Show (ty nTy)
               , AsUnknownType e
               , Monoid r
               )
            => ComponentOutput r e ty nTy tm nTm a
            -> TestTree
mkTermTests c =
  testGroup "term"
    [ mkTextTests c
    , mkEvalTests c
    , mkInferTests c
    ]
