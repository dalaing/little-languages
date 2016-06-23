module Test.Type.Infer (
    inferTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Monad.Except (runExcept)

import Term.Gen
import Type.Infer

inferTests :: TestTree
inferTests = testGroup "infer" [
    testProperty "wellTypedInfer" propWellTypedInfer
  , testProperty "illTypedInfer" propIllTypedInfer
  ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

propWellTypedInfer :: WellTypedTerm -> Bool
propWellTypedInfer (WellTypedTerm t) =
    isRight . runExcept . infer $ t

propIllTypedInfer :: IllTypedTerm -> Bool
propIllTypedInfer (IllTypedTerm t) =
    isLeft . runExcept . infer $ t
