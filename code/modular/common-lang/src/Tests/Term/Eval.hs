{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Tests.Term.Eval (
    mkEvalTests
  ) where

import           Data.List                     (group)
import           Data.Maybe                    (mapMaybe)
import Data.Proxy (Proxy)

import           Control.Lens                  (view)
import           Test.QuickCheck               (Property,
                                                property, (===), (==>))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.QuickCheck         (testProperty)
import           Data.Constraint

import           Component                     (ComponentOutput)
import           Component.Term.Eval.BigStep   (HasBigStepOutput (..))
import           Component.Term.Eval.SmallStep (HasSmallStepOutput (..))
import           Component.Term.Eval.Value     (HasValueOutput (..))
import           Component.Term.Gen            (forAllWellTypedTerm, termEq)
import           Component.Term.SubTerm        (HasSubTermOutput (..))
import Extras (Eq3(..), Show3(..))

mkEvalTests :: ( Eq3 tm
               , Eq nTy
               , Eq nTm
               , Show3 tm
               , Show nTy
               , Show nTm
               )
            => ComponentOutput r e ty tm
            -> Proxy nTy
            -> Proxy nTm
            -> TestTree
mkEvalTests c pTy pTm =
  testGroup "eval"
    [ testProperty "every value is a normal form" $ propValueNormal c pTy pTm
    , testProperty "every normal form is a value" $ propNormalValue c pTy pTm
    , testProperty "small step is determinate" $ propSmallDeterminate c pTy pTm
    , testProperty "small steps decrease term sizes" $ propSmallShrinks c pTy pTm
    , testProperty "small step rules are unique" $ propSmallUnique c pTy pTm
    , testProperty "big step rules are unique" $ propBigUnique c pTy pTm
    , testProperty "small step and big step agree" $ propSmallBig c pTy pTm
    ]

propValueNormal :: forall r e ty tm nTy nTm. (
                     Show3 tm
                   , Show nTy
                   , Show nTm
                   )
                => ComponentOutput r e ty tm
                -> Proxy nTy
                -> Proxy nTm
                -> Property
propValueNormal c pTy pTm =
  let
    isValue' = view isValue c
    isNormalForm' = view isNormalForm c
    forAllWellTypedTerm' = view forAllWellTypedTerm c pTy pTm

    test :: tm nTy nTm String -> Property
    test tm = isValue' tm ==> isNormalForm' tm
  in
    forAllWellTypedTerm' test

propNormalValue :: forall r e ty tm nTy nTm. (
                     Show3 tm
                   , Show nTy
                   , Show nTm
                   )
                => ComponentOutput r e ty tm
                -> Proxy nTy
                -> Proxy nTm
                -> Property
propNormalValue c pTy pTm =
  let
    isValue' = view isValue c
    isNormalForm' = view isNormalForm c
    forAllWellTypedTerm' = view forAllWellTypedTerm c pTy pTm
    test :: tm nTy nTm String -> Property
    test tm = isNormalForm' tm ==> isValue' tm
  in
    forAllWellTypedTerm' test

    -- - either isValue, or there are 1 or more steps we can take that have the same result
propSmallDeterminate :: forall r e ty tm nTy nTm. (
                          Show3 tm
                        , Show nTy
                        , Show nTm
                        , Eq3 tm
                        , Eq nTy
                        , Eq nTm
                        )
                     => ComponentOutput r e ty tm
                     -> Proxy nTy
                     -> Proxy nTm
                     -> Property
propSmallDeterminate c pTy pTm =
  let
    canStep' = view canStep c
    smallStepRules' = view smallStepRules c
    forAllWellTypedTerm' = view forAllWellTypedTerm c pTy pTm
    test :: Eq (tm nTy nTm String) => tm nTy nTm String -> Property
    test tm =
      canStep' tm ==>
        let
          distinctResults =
            length .
            group .
            mapMaybe ($ tm) $
            smallStepRules'
        in
          distinctResults === 1
  in
    forAllWellTypedTerm'
      (test \\ (spanEq3 :: (Eq nTy, Eq nTm, Eq String) :- Eq (tm nTy nTm String)))

propSmallShrinks :: ( Show3 tm
                    , Show nTy
                    , Show nTm
                    )
                 => ComponentOutput r e ty tm
                 -> Proxy nTy
                 -> Proxy nTm
                 -> Property
propSmallShrinks c pTy pTm =
  let
    smallStep' = view smallStep c
    termSize' = view termSize c
    forAllWellTypedTerm' = view forAllWellTypedTerm c pTy pTm
  in
    forAllWellTypedTerm' $ \tm -> property $
      case smallStep' tm of
        Nothing -> True
        Just tm' -> termSize' tm' < termSize' tm

propSmallUnique :: ( Show3 tm
                   , Show nTy
                   , Show nTm
                   )
                => ComponentOutput r e ty tm
                -> Proxy nTy
                -> Proxy nTm
                -> Property
propSmallUnique c pTy pTm =
  let
    valueRules' = view valueRules c
    smallStepRules' = view smallStepRules c
    forAllWellTypedTerm' = view forAllWellTypedTerm c pTy pTm
  in
    forAllWellTypedTerm' $ \tm ->
      let
        matches =
          length .
          mapMaybe ($ tm) $
          valueRules' ++ smallStepRules'
      in
        matches === 1

propBigUnique :: ( Show3 tm
                 , Show nTy
                 , Show nTm
                 )
              => ComponentOutput r e ty tm
              -> Proxy nTy
              -> Proxy nTm
              -> Property
propBigUnique c pTy pTm =
  let
    bigStepRules' = view bigStepRules c
    forAllWellTypedTerm' = view forAllWellTypedTerm c pTy pTm
  in
    forAllWellTypedTerm' $ \tm ->
      let
        matches =
          length .
          mapMaybe ($ tm) $
          bigStepRules'
      in
        matches === 1

propSmallBig :: forall r e ty tm nTy nTm. ( Eq3 tm
                , Eq nTy
                , Eq nTm
                , Show3 tm
                , Show nTy
                , Show nTm
                )
             => ComponentOutput r e ty tm
             -> Proxy nTy
             -> Proxy nTm
             -> Property
propSmallBig c pTy pTm =
  let
    smallStepEval' = view smallStepEval c
    bigStepEval' = view bigStepEval c
    forAllWellTypedTerm' = view forAllWellTypedTerm c pTy pTm
    termEq' = view termEq c pTy pTm
    test :: tm nTy nTm String -> Property
    test tm =
      termEq' (smallStepEval' tm) (bigStepEval' tm)
  in
    forAllWellTypedTerm' test
