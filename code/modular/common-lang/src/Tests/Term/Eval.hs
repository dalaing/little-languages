{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Term.Eval (
    mkEvalTests
  ) where

import           Control.Lens                  (view)
import           Data.List                     (group)
import           Data.Maybe                    (mapMaybe)
import           Test.QuickCheck               (Property, forAllShrink,
                                                property, (===), (==>))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.QuickCheck         (testProperty)

import           Component                     (ComponentOutput)
import           Component.Term.Eval.BigStep   (HasBigStepOutput (..))
import           Component.Term.Eval.SmallStep (HasSmallStepOutput (..))
import           Component.Term.Eval.Value     (HasValueOutput (..))
import           Component.Term.Gen            (HasGenTermOutput (..))
import           Component.Term.Size           (HasTermSizeOutput (..))

mkEvalTests :: ( Eq (tm a)
               , Show (tm a)
               )
            => ComponentOutput e ty tm a
            -> TestTree
mkEvalTests c =
  testGroup "eval"
    [ testProperty "every value is a normal form" $ propValueNormal c
    , testProperty "every normal form is a value" $ propNormalValue c
    , testProperty "small step is determinate" $ propSmallDeterminate c
    , testProperty "small steps decrease term sizes" $ propSmallShrinks c
    , testProperty "small step rules are unique" $ propSmallUnique c
    , testProperty "big step rules are unique" $ propBigUnique c
    , testProperty "small step and big step agree" $ propSmallBig c
    ]

propValueNormal :: Show (tm a)
                => ComponentOutput e ty tm a
                -> Property
propValueNormal c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    isValue' = view isValue c
    isNormalForm' = view isNormalForm c
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm ->
      isValue' tm ==> isNormalForm' tm

propNormalValue :: Show (tm a)
                => ComponentOutput e ty tm a
                -> Property
propNormalValue c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    isValue' = view isValue c
    isNormalForm' = view isNormalForm c
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm ->
      isNormalForm' tm ==> isValue' tm

    -- - either isValue, or there are 1 or more steps we can take that have the same result
propSmallDeterminate :: ( Eq (tm a)
                        , Show (tm a)
                        )
                     => ComponentOutput e ty tm a
                     -> Property
propSmallDeterminate c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    canStep' = view canStep c
    smallStepRules' = view smallStepRules c
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm ->
      canStep' tm ==>
        let
          distinctResults =
            length .
            group .
            mapMaybe ($ tm) $
            smallStepRules'
        in
          distinctResults === 1

propSmallShrinks :: Show (tm a)
                 => ComponentOutput e ty tm a
                 -> Property
propSmallShrinks c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    smallStep' = view smallStep c
    termSize' = view termSize c
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm -> property $
      case smallStep' tm of
        Nothing -> True
        Just tm' -> termSize' tm' < termSize' tm

propSmallUnique :: Show (tm a)
                => ComponentOutput e ty tm a
                -> Property
propSmallUnique c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    valueRules' = view valueRules c
    smallStepRules' = view smallStepRules c
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm ->
      let
        matches =
          length .
          mapMaybe ($ tm) $
          valueRules' ++ smallStepRules'
      in
        matches === 1

propBigUnique :: Show (tm a)
              => ComponentOutput e ty tm a
              -> Property
propBigUnique c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    bigStepRules' = view bigStepRules c
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm ->
      let
        matches =
          length .
          mapMaybe ($ tm) $
          bigStepRules'
      in
        matches === 1

propSmallBig :: ( Eq (tm a)
                , Show (tm a)
                )
             => ComponentOutput e ty tm a
             -> Property
propSmallBig c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    smallStepEval' = view smallStepEval c
    bigStepEval' = view bigStepEval c
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm ->
      smallStepEval' tm === bigStepEval' tm
