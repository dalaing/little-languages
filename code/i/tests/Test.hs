{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Main (
    main
  ) where

-- from 'tasty'
import           Test.Tasty  (TestTree, defaultMain, testGroup)

-- local
import           Test.Common (commonTests)
import           Test.Term   (termTests)
import           Test.Type   (typeTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ commonTests
  , typeTests
  , termTests
  ]

