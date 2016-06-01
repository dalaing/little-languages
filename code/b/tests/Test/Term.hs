{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Term (
    termTests
  ) where

-- from 'tasty'
import           Test.Tasty      (TestTree, testGroup)

-- local
import           Test.Term.Eval  (evalTests)
import           Test.Term.Infer (inferTests)
import           Test.Term.Text  (textTests)

termTests :: TestTree
termTests =
  testGroup "term"
    [ evalTests
    , inferTests
    , textTests
    ]
