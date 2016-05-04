{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module Tests.Term.Eval (
    mkEvalTests
  ) where

import           Control.Lens                  (view)
import           Data.List                     (group)
import           Data.Maybe                    (mapMaybe)
import           Test.QuickCheck               (Property,
                                                property, (===), (==>))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.QuickCheck         (testProperty)

import           Component                     (ComponentOutput)
import           Component.Term.Eval.BigStep   (HasBigStepOutput (..))
import           Component.Term.Eval.SmallStep (HasSmallStepOutput (..))
import           Component.Term.Eval.Value     (HasValueOutput (..))
import           Component.Term.Gen            (forAllWellTypedTerm)
import           Component.Term.SubTerm        (HasSubTermOutput (..))

mkEvalTests :: ( Eq (tm nTy nTm String)
               , Show (tm nTy nTm String)
               )
            => ComponentOutput r e ty nTy tm nTm String
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

propValueNormal :: Show (tm nTy nTm String)
                => ComponentOutput r e ty nTy tm nTm String
                -> Property
propValueNormal c =
  let
    isValue' = view isValue c
    isNormalForm' = view isNormalForm c
  in
    forAllWellTypedTerm c $ \tm ->
      isValue' tm ==> isNormalForm' tm

propNormalValue :: Show (tm nTy nTm String)
                => ComponentOutput r e ty nTy tm nTm String
                -> Property
propNormalValue c =
  let
    isValue' = view isValue c
    isNormalForm' = view isNormalForm c
  in
    forAllWellTypedTerm c $ \tm ->
      isNormalForm' tm ==> isValue' tm

    -- - either isValue, or there are 1 or more steps we can take that have the same result
propSmallDeterminate :: ( Eq (tm nTy nTm String)
                        , Show (tm nTy nTm String)
                        )
                     => ComponentOutput r e ty nTy tm nTm String
                     -> Property
propSmallDeterminate c =
  let
    canStep' = view canStep c
    smallStepRules' = view smallStepRules c
  in
    forAllWellTypedTerm c $ \tm ->
      canStep' tm ==>
        let
          distinctResults =
            length .
            group .
            mapMaybe ($ tm) $
            smallStepRules'
        in
          distinctResults === 1

propSmallShrinks :: Show (tm nTy nTm String)
                 => ComponentOutput r e ty nTy tm nTm String
                 -> Property
propSmallShrinks c =
  let
    smallStep' = view smallStep c
    termSize' = view termSize c
  in
    forAllWellTypedTerm c $ \tm -> property $
      case smallStep' tm of
        Nothing -> True
        Just tm' -> termSize' tm' < termSize' tm

propSmallUnique :: Show (tm nTy nTm String)
                => ComponentOutput r e ty nTy tm nTm String
                -> Property
propSmallUnique c =
  let
    valueRules' = view valueRules c
    smallStepRules' = view smallStepRules c
  in
    forAllWellTypedTerm c $ \tm ->
      let
        matches =
          length .
          mapMaybe ($ tm) $
          valueRules' ++ smallStepRules'
      in
        matches === 1

propBigUnique :: Show (tm nTy nTm String)
              => ComponentOutput r e ty nTy tm nTm String
              -> Property
propBigUnique c =
  let
    bigStepRules' = view bigStepRules c
  in
    forAllWellTypedTerm c $ \tm ->
      let
        matches =
          length .
          mapMaybe ($ tm) $
          bigStepRules'
      in
        matches === 1

propSmallBig :: ( Eq (tm nTy nTm String)
                , Show (tm nTy nTm String)
                )
             => ComponentOutput r e ty nTy tm nTm String
             -> Property
propSmallBig c =
  let
    smallStepEval' = view smallStepEval c
    bigStepEval' = view bigStepEval c
  in
    forAllWellTypedTerm c $ \tm ->
      smallStepEval' tm === bigStepEval' tm
