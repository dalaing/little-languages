{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Term.Eval (
    evalTests
  ) where

import           Data.List             (group)
import           Data.Maybe            (mapMaybe)
import           Test.QuickCheck       (Property, forAllShrink, property, (===),
                                        (==>))
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Term                  (size)
import           Term.Eval.BigStep     (bigStepRules, eval)
import           Term.Eval.SmallStep   (canStep, eval, isNormalForm, smallStep,
                                        smallStepRules)
import           Term.Eval.Value       (isValue, valueRules)
import           Term.Gen              (genTerm, shrinkTerm)

evalTests :: TestTree
evalTests = testGroup "eval"
  [
    testProperty "every value is a normal form" propValueNormal
  , testProperty "every normal form is a value" propNormalValue
  , testProperty "small step is determinate" propSmallDeterminate
  , testProperty "small steps decrease term sizes" propSmallShrinks
  , testProperty "small step rules are unique" propSmallUnique
  , testProperty "big step rules are unique" propBigUnique
  , testProperty "small step and big step agree" propSmallBig
  ]

propValueNormal :: Property
propValueNormal =
  forAllShrink genTerm shrinkTerm $ \tm ->
    isValue tm ==> isNormalForm tm

propNormalValue :: Property
propNormalValue =
  forAllShrink genTerm shrinkTerm $ \tm ->
    isNormalForm tm ==> isValue tm

    -- - either isValue, or there are 1 or more steps we can take that have the same result
propSmallDeterminate :: Property
propSmallDeterminate =
  forAllShrink genTerm shrinkTerm $ \tm ->
    canStep tm ==>
      let
        distinctResults =
          length .
          group .
          mapMaybe ($ tm) $
          smallStepRules
      in
        distinctResults === 1

propSmallShrinks :: Property
propSmallShrinks =
  forAllShrink genTerm shrinkTerm $ \tm -> property $
    case smallStep tm of
      Nothing -> True
      Just tm' -> size tm' < size tm

propSmallUnique :: Property
propSmallUnique =
  forAllShrink genTerm shrinkTerm $ \tm ->
    let
      matches =
        length .
        mapMaybe ($ tm) $
        valueRules ++ smallStepRules
    in
      matches === 1

propBigUnique :: Property
propBigUnique =
  forAllShrink genTerm shrinkTerm $ \tm ->
    let
      matches =
        length .
        mapMaybe ($ tm) $
        bigStepRules
    in
      matches === 1

propSmallBig :: Property
propSmallBig =
  forAllShrink genTerm shrinkTerm $ \tm ->
    Term.Eval.SmallStep.eval tm === Term.Eval.BigStep.eval tm
