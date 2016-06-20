{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Term.Eval.Strict (
    strictTests
  ) where

-- from 'base'
import           Data.List                  (group)
import           Data.Maybe                 (mapMaybe)

-- from 'tasty'
import           Test.Tasty                 (TestTree, testGroup)

-- from 'tasty-quickcheck'
import           Test.Tasty.QuickCheck      (testProperty)

-- from 'QuickCheck'
import           Test.QuickCheck            (Property,
                                             (===), (==>))

-- local
import           Term                       (size)
import           Term.Eval.BigStep.Strict   (bigStepRules, eval)
import           Term.Eval.SmallStep.Strict (canStep, eval, isNormalForm,
                                             smallStep, smallStepRules)
import           Term.Eval.Value.Strict     (isValue, valueRules)
import           Term.Gen                   (AnyTerm(..), WellTypedTerm(..))

strictTests :: TestTree
strictTests = testGroup "strict"
  [
    testProperty "every value is a normal form" propValueNormal
  , testProperty "every normal form is a value" propNormalValue
  , testProperty "small step is determinate" propSmallDeterminate
  , testProperty "small steps decrease term sizes" propSmallShrinks
  , testProperty "small step rules are unique" propSmallUnique
  , testProperty "big step rules are unique" propBigUnique
  , testProperty "small step and big step agree" propSmallBig
  ]

propValueNormal :: AnyTerm
                -> Property
propValueNormal (AnyTerm tm) =
  isValue tm ==> isNormalForm tm

propNormalValue :: WellTypedTerm
                -> Property
propNormalValue (WellTypedTerm tm) =
  isNormalForm tm ==> isValue tm

    -- - either isValue, or there are 1 or more steps we can take that have the same result
propSmallDeterminate :: WellTypedTerm
                     -> Property
propSmallDeterminate (WellTypedTerm tm) =
  canStep tm ==>
    let
      distinctResults =
        length .
        group .
        mapMaybe ($ tm) $
        smallStepRules
    in
      distinctResults === 1

propSmallShrinks :: AnyTerm
                 -> Bool
propSmallShrinks (AnyTerm tm) =
  case smallStep tm of
    Nothing -> True
    Just tm' -> size tm' < size tm

propSmallUnique :: WellTypedTerm
                -> Property
propSmallUnique (WellTypedTerm tm) =
  let
    matches =
      length .
      mapMaybe ($ tm) $
      valueRules ++ smallStepRules
  in
    matches === 1

propBigUnique :: WellTypedTerm
              -> Property
propBigUnique (WellTypedTerm tm) =
  let
    matches =
      length .
      mapMaybe ($ tm) $
      bigStepRules
  in
    matches === 1

propSmallBig :: WellTypedTerm
             -> Property
propSmallBig (WellTypedTerm tm) =
  Term.Eval.SmallStep.Strict.eval tm === Term.Eval.BigStep.Strict.eval tm
