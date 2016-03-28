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

propSmallUnique :: AnyTerm
                -> Property
propSmallUnique (AnyTerm t) =
    isValue .||. uniqueMatch
  where
    isValue = isJust . value $ t
    uniqueMatch = (=== 1) . length . mapMaybe ($ t) $ smallSteps

propSmallShrinks :: AnyTerm
                 -> Bool
propSmallShrinks (AnyTerm t) =
  case smallStep t of
    Nothing -> True
    Just u -> size u < size t

propBigUnique :: AnyTerm
              -> Property
propBigUnique (AnyTerm t) =
  (=== 1) . length . mapMaybe ($ t) $ bigSteps

propSmallBig :: AnyTerm
             -> Property
propSmallBig (AnyTerm t) =
  sEval t === bEval t
