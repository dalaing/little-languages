{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Term.Infer (
    inferTests
  ) where

-- from 'base'
import           Data.Maybe            (mapMaybe)

-- from 'tasty'
import           Test.Tasty            (TestTree, testGroup)

-- from 'tasty-quickcheck'
import           Test.Tasty.QuickCheck (testProperty)

-- from 'QuickCheck'
import           Test.QuickCheck       (Arbitrary (..), Property, forAll,
                                        forAllShrink, property, (.||.), (===))

-- local
import           Term                  (Term)
import           Term.Eval.SmallStep   (canStep, smallStep)
import           Term.Eval.Value       (isValue)
import           Term.Gen              (AnyTerm (..), IllTypedTerm (..),
                                        WellTypedTerm (..), genContainingTerm,
                                        genIllTypedTerm, genWellTypedTerm,
                                        shrinkIllTypedTerm)
import           Term.Infer            (inferTerm, inferTermRules, runInfer)
import           Type                  (Type)
import           Type.Error            (TypeError (..))
import           Type.Error.Gen        (AnyTypeError (..))
import           Type.Gen              (genType)

inferTests :: TestTree
inferTests = testGroup "infer"
  [ testProperty "patterns unique" propPatternUnique
  , testProperty "never NoMatchingTypeRule" propNeverNoMatchingTypeRule
  , testProperty "well-typed infer" propWellTypedInfer
  , testProperty "well-typed containing infer" propWellTypedContainingInfer
  , testProperty "ill-typed infer" propIllTypedInfer
  , testProperty "ill-typed error" propIllTypedError
  , testProperty "progress" propProgress
  , testProperty "preservation" propPreservation
  ]

isRight :: Either a b
        -> Bool
isRight (Right _) =
  True
isRight _ =
  False

isLeft :: Either a b
       -> Bool
isLeft (Left _) =
  True
isLeft _ =
  False

infer :: Term
      -> Either TypeError Type
infer =
  runInfer .
  inferTerm

-- Test out the generators

propWellTypedInfer :: WellTypedTerm
                   -> Bool
propWellTypedInfer wttm =
  all
    (isRight . infer . getWellTypedTerm)
    (wttm : shrink wttm)

propWellTypedContainingInfer :: Property
propWellTypedContainingInfer =
  forAll genWellTypedContaining $ \ (ty, tm) ->
    Right ty == infer tm
  where
    genWellTypedContaining = do
      ty <- genType
      tm <- genWellTypedTerm ty
      ty2 <- genType
      tm2 <- genContainingTerm ty tm ty2
      return (ty2, tm2)

propIllTypedInfer :: IllTypedTerm
                  -> Bool
propIllTypedInfer ittm =
  all
    (isLeft . infer . getIllTypedTerm)
    (ittm : shrink ittm)

propIllTypedError :: AnyTypeError
                  -> Property
propIllTypedError (AnyTypeError te) =
  forAllShrink (genType >>= genIllTypedTerm te) shrinkIllTypedTerm $
    (== Left te) .
    infer

-- Canary : check the typing rules are disjoint and exhaustive

propPatternUnique :: AnyTerm
                  -> Property
propPatternUnique (AnyTerm tm) =
  let
    matches =
      length .
      mapMaybe (\i -> fmap runInfer . i $ tm) $
      inferTermRules
  in
    matches === 1

propNeverNoMatchingTypeRule :: AnyTerm
                            -> Bool
propNeverNoMatchingTypeRule (AnyTerm tm) =
  infer tm /= Left NoMatchingTypeRule

-- Type safety properties

propProgress :: WellTypedTerm
             -> Property
propProgress (WellTypedTerm tm) =
  case infer tm of
    Left _ -> property True
    Right _ -> isValue tm .||. canStep tm

propPreservation :: WellTypedTerm
                 -> Property
propPreservation (WellTypedTerm tm) =
  case smallStep tm of
    Nothing -> property True
    Just tm' -> infer tm === infer tm'

