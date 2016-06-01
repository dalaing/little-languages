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
import           Test.QuickCheck       (Property, forAllShrink, property,
                                        (.||.), (===))

-- local
import           Term                  (Term)
import           Term.Eval.SmallStep   (canStep, smallStep)
import           Term.Eval.Value       (isValue)
import           Term.Gen              (genTerm, shrinkTerm)
import           Term.Infer            (inferTerm, inferTermRules, runInfer)
import           Type                  (Type)
import           Type.Error            (TypeError)

inferTests :: TestTree
inferTests = testGroup "infer"
  [ testProperty "patterns unique" propPatternUnique
  , testProperty "well-typed infer" propWellTypedInfer
  , testProperty "progress" propProgress
  , testProperty "preservation" propPreservation
  ]

isRight :: Either a b
        -> Bool
isRight (Right _) =
  True
isRight _ =
  False

infer :: Term
      -> Either TypeError Type
infer =
  runInfer .
  inferTerm

propPatternUnique :: Property
propPatternUnique =
  forAllShrink genTerm shrinkTerm $ \tm ->
    let
      matches =
        length .
        mapMaybe (\i -> fmap runInfer . i $ tm) $
        inferTermRules
    in
      matches === 1

-- For now, all of our terms are well typed.
propWellTypedInfer :: Property
propWellTypedInfer =
  forAllShrink genTerm shrinkTerm $
    isRight .
    infer

-- Assumes we are dealing with a well-typed term.
propProgress :: Property
propProgress =
  forAllShrink genTerm shrinkTerm $ \tm ->
    case infer tm of
      Left _ -> property True
      Right _ -> isValue tm .||. canStep tm

-- Assumes we are dealing with a well-typed term.
propPreservation :: Property
propPreservation =
  forAllShrink genTerm shrinkTerm $ \tm ->
    case smallStep tm of
      Nothing -> property True
      Just tm' -> infer tm === infer tm'

