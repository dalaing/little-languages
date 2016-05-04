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

import           Test.Tasty                             (TestTree, testGroup)
import           Text.Trifecta.Rendering                (Span)

import           Component                              (ComponentOutput)
import           Component.Type.Error.UnknownType.Class (AsUnknownType (..))
import           Tests.Term.Eval                        (mkEvalTests)
import           Tests.Term.Infer                       (mkInferTests)
import           Tests.Term.Text                        (mkTextTests)

mkTermTests :: ( Eq e
               , Show e
               , Eq (tm Span Span String)
               , Show (tm Span Span String)
               , Eq (ty Span)
               , Show (ty Span)
               , AsUnknownType e
               , Monoid r
               )
            => ComponentOutput r e ty Span tm Span String
            -> TestTree
mkTermTests c =
  testGroup "term"
    [ mkTextTests c
    , mkEvalTests c
    , mkInferTests c
    ]
