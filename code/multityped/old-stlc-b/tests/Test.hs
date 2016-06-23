module Main where

import Data.Maybe (isJust, isNothing, catMaybes)

import Control.Monad.Except (runExcept)

import Text.Trifecta.Rendering (Span)

import Test.Tasty
import Test.Tasty.QuickCheck

import Term
import Term.Gen
import Term.Parse
import Term.Pretty
import Term.Eval.Value
import Term.Eval.SmallStep
import Term.Eval.BigStep

import Type.Infer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
        testProperty "roundTrip" propRoundTrip
      , testProperty "smallUnique" propSmallUnique
      , testProperty "smallShrinks" propSmallShrinks
      , testProperty "bigUnique" propBigUnique
      , testProperty "smallBig" propSmallBig
      , testProperty "wellTypedInfer" propWellTypedInfer
      , testProperty "illTypedInfer" propIllTypedInfer
      , testProperty "wellTypedProgress" propWellTypedProgress
      , testProperty "wellTypedPreservation" propWellTypedPreservation
      , testProperty "progress" propProgress
      , testProperty "preservation" propPreservation
      ]

propRoundTrip :: AnyTerm Span -> Property
propRoundTrip (AnyTerm t) =
  case (fmap (stripLoc . locToTerm) . parseTermString . prettyTermString) t of
    Left _ -> property False
    Right u -> u === t

propSmallUnique :: WellTypedTerm Span -> Property
propSmallUnique (WellTypedTerm t) =
  (isJust . value $ t) .||.
  ((=== 1) . length . catMaybes . fmap ($ t) $ smallSteps)

propSmallShrinks :: WellTypedTerm Span -> Bool
propSmallShrinks (WellTypedTerm t) = case smallStep t of
  Nothing -> True
  Just u -> size u < size t

propBigUnique :: WellTypedTerm Span -> Property
propBigUnique (WellTypedTerm t) =
  (isJust . value $ t) .||.
  ((=== 1) . length . catMaybes . fmap ($ t) $ bigSteps)

propSmallBig :: WellTypedTerm Span -> Property
propSmallBig (WellTypedTerm t) =
  sEval t === bEval t

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

propWellTypedInfer :: WellTypedTerm Span -> Bool
propWellTypedInfer (WellTypedTerm t) =
    isRight . runExcept . infer . termToMaybeLoc $ t

propIllTypedInfer :: IllTypedTerm Span -> Bool
propIllTypedInfer (IllTypedTerm t) =
    isLeft . runExcept . infer . termToMaybeLoc $ t

propWellTypedProgress :: WellTypedTerm Span -> Property
propWellTypedProgress (WellTypedTerm t) =
    (isJust v    .&&. isNothing s) .||.
    (isNothing v .&&. isJust s)
  where
    v = value t
    s = smallStep t

propWellTypedPreservation :: WellTypedTerm Span -> Property
propWellTypedPreservation (WellTypedTerm tm) =
    isJust v .||. (isNothing v .&&. check)
  where
    v = value tm
    inferEither = runExcept . infer . termToMaybeLoc
    ty = inferEither tm
    s = smallStep tm
    check = maybe False (\tm2 -> inferEither tm2 == ty) s

propProgress :: AnyTerm Span -> Property
propProgress (AnyTerm t) =
    not i .||.
    (i .&&. v .&&. not s) .||.
    (i .&&. not v .&&. s)
  where
    i = isRight . runExcept . infer . termToMaybeLoc $ t
    v = isJust . value $ t
    s = isJust . smallStep $ t

propPreservation :: AnyTerm Span -> Property
propPreservation (AnyTerm tm) =
    not i .||.
    (i .&&. v .&&. not s) .||.
    (i .&&. not v && check)
  where
    inferEither = runExcept . infer . termToMaybeLoc
    ty = inferEither tm
    i = isRight ty
    v = isJust . value $ tm
    st = smallStep tm
    s = isJust st
    check = maybe False ((== ty) . inferEither) st
