{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Term.Eval (
    evalTests
  ) where

-- from 'tasty'
import           Test.Tasty            (TestTree, testGroup)

-- local
import Test.Term.Eval.Strict (strictTests)
import Test.Term.Eval.Lazy (lazyTests)

evalTests :: TestTree
evalTests = testGroup "eval"
  [
    strictTests
  , lazyTests
  ]
