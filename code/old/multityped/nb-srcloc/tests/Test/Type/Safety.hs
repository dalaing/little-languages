module Test.Type.Safety (
    safetyTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Maybe (isNothing, isJust)

import Control.Monad.Except (runExcept)

import Term.Gen
import Term.Eval.Value
import Term.Eval.SmallStep
import Type.Infer

safetyTests :: TestTree
safetyTests = testGroup "safety" [
    testProperty "wellTypedProgress" propWellTypedProgress
  , testProperty "wellTypedPreservation" propWellTypedPreservation
  , testProperty "progress" propProgress
  , testProperty "preservation" propPreservation
  ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- this does not hold, since if has lazy semantics
-- useful to point out
propIllTypedStuck :: IllTypedTerm -> Bool
propIllTypedStuck (IllTypedTerm t) =
    isNothing . value . sEval $ t

propWellTypedProgress :: WellTypedTerm -> Property
propWellTypedProgress (WellTypedTerm t) =
    (isJust v    .&&. isNothing s) .||.
    (isNothing v .&&. isJust s)
  where
    v = value t
    s = smallStep t

propWellTypedPreservation :: WellTypedTerm -> Property
propWellTypedPreservation (WellTypedTerm tm) =
    isJust v .||. (isNothing v .&&. check)
  where
    v = value tm
    inferEither = runExcept . infer
    ty = inferEither tm
    s = smallStep tm
    check = maybe False (\tm2 -> inferEither tm2 == ty) s

propProgress :: AnyTerm -> Property
propProgress (AnyTerm t) =
    not i .||.
    (i .&&. v .&&. not s) .||.
    (i .&&. not v .&&. s)
  where
    i = isRight . runExcept . infer $ t
    v = isJust . value $ t
    s = isJust . smallStep $ t

propPreservation :: AnyTerm -> Property
propPreservation (AnyTerm tm) =
    not i .||.
    (i .&&. v .&&. not s) .||.
    (i .&&. not v && check)
  where
    inferEither = runExcept . infer
    ty = inferEither tm
    i = isRight ty
    v = isJust . value $ tm
    st = smallStep tm
    s = isJust st
    check = maybe False ((== ty) . inferEither) st
