module Test.Term.Eval where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Maybe (mapMaybe)

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
    matches === 1
  where
    matches = 
      length . 
      mapMaybe ($ t) $ 
        valueRules ++ smallStepRules

propSmallShrinks :: AnyTerm
                 -> Bool
propSmallShrinks (AnyTerm t) =
  case smallStep t of
    Nothing -> True
    Just u -> size u < size t

propBigUnique :: AnyTerm
              -> Property
propBigUnique (AnyTerm t) =
    matches === 1
  where
    matches = length . mapMaybe ($ t) $ bigSteps

propSmallBig :: AnyTerm
             -> Property
propSmallBig (AnyTerm t) =
  sEval t === bEval t
