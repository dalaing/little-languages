module Test.Term.Infer (
    inferTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Maybe (mapMaybe)
import Control.Monad.Except (runExcept)

import Term.Gen
import Term.Infer
import Type.Error

inferTests :: TestTree
inferTests = testGroup "infer" [
    testProperty "wellTypedInfer" propWellTypedInfer
  , testProperty "illTypedInfer" propIllTypedInfer
  , testProperty "inferUnique" propInferUnique
  , testProperty "neverUnknown" propNeverUnknown
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

propInferUnique :: AnyTerm -> Property
propInferUnique (AnyTerm t) =
    matches === 1
  where
    matches =
      length .
      fmap runExcept .
      mapMaybe ($ t) $
      inferRules

propNeverUnknown :: AnyTerm -> Bool
propNeverUnknown (AnyTerm t) =
  case (runExcept . infer) t of
    Left (TeUnknownType _) -> False
    _ -> True
