module Test.Term.Eval where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Maybe (isJust, mapMaybe)

import Term.Gen
import Term
import Term.Eval.Value
import Term.Eval.SmallStep
import Term.Eval.BigStep

evalTests :: TestTree
evalTests =
  testGroup "eval" [
      testProperty "smallUnique" propSmallUnique
    , testProperty "smallShrinks" propSmallShrinks
    , testProperty "bigUnique" propBigUnique
    , testProperty "smallBig" propSmallBig
    ]

propSmallUnique :: WellTypedTerm
                -> Property
propSmallUnique (WellTypedTerm t) =
    (isValue .&&. matches === 0) .||.
    (not isValue .&&. matches === 1)
  where
    isValue = isJust . value $ t
    matches = length . mapMaybe ($ t) $ smallSteps

propSmallShrinks :: WellTypedTerm
                 -> Bool
propSmallShrinks (WellTypedTerm t) =
  case smallStep t of
    Nothing -> True
    Just u -> size u < size t

propBigUnique :: WellTypedTerm
              -> Property
propBigUnique (WellTypedTerm t) =
    matches === 1
  where
    matches = length . mapMaybe ($ t) $ bigSteps

propSmallBig :: WellTypedTerm
             -> Property
propSmallBig (WellTypedTerm t) =
  sEval t === bEval t
