module Test.Term.Infer (
    inferTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Maybe (mapMaybe)
import Control.Monad.Except (runExcept)

import Term
import Term.Gen
import Term.Infer
import Type.Error

inferTests :: TestTree
inferTests = testGroup "infer" [
    testProperty "wellTypedInfer" propWellTypedInfer
  , testProperty "wellTypedShrinks" propWellTypedShrinks
  , testProperty "illTypedInfer" propIllTypedInfer
  , testProperty "illTypedShrinks" propIllTypedShrinks
  , testProperty "inferUnique" propInferUnique
  , testProperty "neverUnknown" propNeverUnknown
  ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

wellTyped :: Term -> Bool
wellTyped = 
  isRight . 
  runExcept . 
  infer

illTyped :: Term -> Bool
illTyped = 
  isLeft . 
  runExcept . 
  infer

propWellTypedInfer :: WellTypedTerm -> Bool
propWellTypedInfer (WellTypedTerm t) =
  wellTyped t

propWellTypedShrinks :: WellTypedTerm -> Bool
propWellTypedShrinks =
  all propWellTypedInfer . 
  shrink

propIllTypedInfer :: IllTypedTerm -> Bool
propIllTypedInfer (IllTypedTerm t) =
  illTyped t

propIllTypedShrinks :: IllTypedTerm -> Bool
propIllTypedShrinks =
  all propIllTypedInfer . 
  shrink

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
    Left TeUnknownType -> False
    _ -> True
