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
import           Test.QuickCheck       (Arbitrary (..), Property, property,
                                        (.||.), (===), forAllShrink)

-- local
import           Term                  (Term)
import           Term.Eval.SmallStep   (canStep, smallStep)
import           Term.Eval.Value       (isValue)
import           Term.Gen              (AnyTerm (..), IllTypedTerm (..),
                                        WellTypedTerm (..), genIllTypedTerm,
                                        shrinkIllTypedTerm)
import           Term.Infer            (inferTerm, inferTermRules, runInfer)
import           Type                  (Type)
import           Type.Gen              (genType)
import           Type.Error            (TypeError)
import           Type.Error.Gen        (AnyTypeError (..))

inferTests :: TestTree
inferTests = testGroup "infer"
  [ testProperty "patterns unique" propPatternUnique
  , testProperty "well-typed infer" propWellTypedInfer
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

