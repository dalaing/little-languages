module Common.Test.Term.Eval where

import Data.Maybe (isJust, mapMaybe)
import Control.Lens (view)

import Test.Tasty
import Test.Tasty.QuickCheck

import Common.Term
import Common.Term.Size
import Common.Term.Gen
import Common.Term.Eval.Value
import Common.Term.Eval.SmallStep
import Common.Term.Eval.BigStep

evalTests :: ( Eq tm
             , Show tm
             )
           => TermOutput e ty tm
           -> TestTree
evalTests t =
  testGroup "eval" [
      testProperty "smallUnique" (propSmallUnique t)
    , testProperty "smallShrinks" (propSmallShrinks t)
    , testProperty "bigUnique" (propBigUnique t)
    , testProperty "smallBig" (propSmallBig t)
    ]

propSmallUnique :: Show tm
                => TermOutput e ty tm
                -> Property
propSmallUnique t =
  forAllShrink (view genWellTypedTerm t Nothing) (view shrinkTerm t) $ \tm ->
    let
      matches = 
        length . 
        mapMaybe ($ tm) $ 
          view valueRules t ++ view smallStepRules t
    in
      matches === 1

propSmallShrinks :: Show tm
                 => TermOutput e ty tm
                 -> Property
propSmallShrinks t =
  let
    termSize = view size t
    step = view smallStep t
  in
    forAllShrink (view genWellTypedTerm t Nothing) (view shrinkTerm t) $ \tm ->
      case step tm of
        Nothing -> True
        Just u -> termSize u < termSize tm

propBigUnique :: Show tm
              => TermOutput e ty tm
              -> Property
propBigUnique t =
  forAllShrink (view genWellTypedTerm t Nothing) (view shrinkTerm t) $ \tm ->
    let
      matches = 
        length . 
        mapMaybe ($ tm) $ 
        view bigStepRules t
    in
      matches === 1

propSmallBig :: ( Eq tm
                , Show tm
                )
             => TermOutput e ty tm
             -> Property
propSmallBig t =
  let
    sEval = view smallStepEval t
    bEval = view bigStepEval t
  in
    forAllShrink (view genWellTypedTerm t Nothing) (view shrinkTerm t) $ \tm ->
      sEval tm === bEval tm
