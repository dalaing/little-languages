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
import           Test.QuickCheck       (Property, property,
                                        (.||.), (===))

-- local
import           Term                  (Term)
import           Term.Eval.SmallStep   (canStep, smallStep)
import           Term.Eval.Value       (isValue)
import           Term.Gen              (AnyTerm(..))
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

-- For now, all of our terms are well typed.
propWellTypedInfer :: AnyTerm
                   -> Bool
propWellTypedInfer (AnyTerm tm) =
  isRight .
  infer $
  tm

-- Assumes we are dealing with a well-typed term.
propProgress :: AnyTerm
             -> Property
propProgress (AnyTerm tm) =
  case infer tm of
    Left _ -> property True
    Right _ -> isValue tm .||. canStep tm

-- Assumes we are dealing with a well-typed term.
propPreservation :: AnyTerm
                 -> Property
propPreservation (AnyTerm tm) =
  case smallStep tm of
    Nothing -> property True
    Just tm' -> infer tm === infer tm'
