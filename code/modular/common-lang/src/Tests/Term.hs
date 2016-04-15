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

import           Common.Type.Error.UnknownType.Class (AsUnknownType (..))
import           Component                           (ComponentOutput)
import           Tests.Term.Eval                     (mkEvalTests)
import           Tests.Term.Infer                    (mkInferTests)
import           Tests.Term.Text                     (mkTextTests)

mkTermTests :: ( Eq e
               , Show e
               , Eq (tm a)
               , Show (tm a)
               , Eq ty
               , Show ty
               , AsUnknownType e
               )
            => ComponentOutput e ty tm a
            -> TestTree
mkTermTests c =
  testGroup "term"
    [ mkTextTests c
    , mkEvalTests c
    , mkInferTests c
    ]
