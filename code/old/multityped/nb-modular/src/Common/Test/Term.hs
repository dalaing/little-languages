module Common.Test.Term where

import Test.Tasty

import Common.Type.Error
import Common.Term

import Common.Test.Term.Infer
import Common.Test.Term.Eval
import Common.Test.Term.Text

termTests :: ( Eq tm
             , Show tm
             , AsUnknownType e n
             )
          => TermOutput e ty tm
          -> TestTree
termTests t = testGroup "term"
  [ inferTests t
  , evalTests t
  , textTests t
  ]
