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

-- from 'base'
import           Data.List             (group)
import           Data.Maybe            (mapMaybe)

-- from 'tasty'
import           Test.Tasty            (TestTree, testGroup)

-- from 'tasty-quickcheck'
import           Test.Tasty.QuickCheck (testProperty)

-- from 'QuickCheck'
import           Test.QuickCheck       (Property, (===),
                                        (==>))

-- local
import           Term                  (size)
import           Term.Eval.BigStep     (bigStepRules, eval)
import           Term.Eval.SmallStep   (canStep, eval, isNormalForm, smallStep,
                                        smallStepRules)
import           Term.Eval.Value       (isValue, valueRules)
import           Term.Gen              (AnyTerm(..))

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

propValueNormal :: AnyTerm
                -> Property
propValueNormal (AnyTerm tm )=
  isValue tm ==> isNormalForm tm

propNormalValue :: AnyTerm
                -> Property
propNormalValue (AnyTerm tm) =
  isNormalForm tm ==> isValue tm

    -- - either isValue, or there are 1 or more steps we can take that have the same result
propSmallDeterminate :: AnyTerm
                     -> Property
propSmallDeterminate (AnyTerm tm )=
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

propSmallUnique :: AnyTerm
                -> Property
propSmallUnique (AnyTerm tm) =
  let
    matches =
      length .
      mapMaybe ($ tm) $
      valueRules ++ smallStepRules
  in
    matches === 1

propBigUnique :: AnyTerm
              -> Property
propBigUnique (AnyTerm tm) =
  let
    matches =
      length .
      mapMaybe ($ tm) $
      bigStepRules
  in
    matches === 1

propSmallBig :: AnyTerm
             -> Property
propSmallBig (AnyTerm tm) =
  Term.Eval.SmallStep.eval tm === Term.Eval.BigStep.eval tm
