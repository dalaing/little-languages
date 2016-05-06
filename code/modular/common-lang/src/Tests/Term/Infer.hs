{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Tests.Term.Infer (
    mkInferTests
  ) where

import           Data.Maybe            (mapMaybe)
import Data.Proxy (Proxy)

import           Control.Lens          (view)
import           Control.Lens.Prism    (isn't)
import           Test.QuickCheck       (Property, forAllShrink, property,
                                        (.||.), (===))
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import Data.Constraint
import Data.Constraint.Forall (ForallT, instT)

import Component.Type.Error.UnknownType.Class (AsUnknownType(..))
import           Component             (ComponentOutput)
import           Component.Type.Gen    (HasGenTypeOutput (..))
import           Component.Term.Gen    (HasGenTermOutput (..), forAllWellTypedTerm)
import           Component.Term.Infer  (HasInferOutput (..), runInfer)
import           Component.Term.Eval.Value (HasValueOutput (..))
import           Component.Term.Eval.SmallStep (HasSmallStepOutput (..))
import Extras (Eq1(..), Eq2(..), Eq3(..), Show1(..), Show2(..), Show3(..), Monoid2(..))

mkInferTests :: ( Eq3 tm
                , Eq1 ty
                , Eq2 e
                , Eq n
                , Show3 tm
                , Show1 ty
                , Show2 e
                , Show n
                , AsUnknownType e
                , Monoid2 r
                )
             => ComponentOutput r e ty tm
             -> Proxy n
             -> Proxy n
             -> TestTree
mkInferTests c _ p =
  testGroup "infer"
    [ testProperty "patterns unique" $ propPatternUnique c p
    , testProperty "unknown never occurs" $ propUnknownNever c p
    , testProperty "well-typed infer" $ propWellTypedInfer c p
    , testProperty "ill-typed infer" $ propIllTypedInfer c p
    , testProperty "progress" $ propProgress c p
    , testProperty "preservation" $ propPreservation c p
    ]

isRight :: Either a b
        -> Bool
isRight (Right _) =
  True
isRight _ =
  False

isLeft :: Either a b
       -> Bool
isLeft (Left _) =
  True
isLeft _ =
  False

propPatternUnique :: forall r e ty tm n. (
                       Show3 tm
                     , Show n
                     , Eq3 tm
                     , Eq n
                     , Monoid2 r
                     )
                  => ComponentOutput r e ty tm
                  -> Proxy n
                  -> Property
propPatternUnique c _ =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    inferRules' = view inferRules c
    ctx :: r n String
    ctx = mempty \\ (spanMonoid2 :: Ord String :- Monoid (r n String))
    test :: tm n n String -> Property
    test tm =
      let
        matches =
          length .
          mapMaybe (\i -> fmap (runInfer ctx) . i $ tm) $
          inferRules'
      in
        matches === 1
          \\ (spanEq3 :: (Eq n, Eq n, Eq String) :- Eq (tm n n String))
  in
    forAllShrink genAnyTerm' shrAnyTerm' test
      \\ (spanShow3 :: (Show n, Show n, Show String) :- Show (tm n n String))

propUnknownNever :: forall r e ty tm n. (
                      Show3 tm
                    , Show n
                    , Eq3 tm
                    , Eq n
                    , AsUnknownType e
                    , Monoid2 r
                    )
                 => ComponentOutput r e ty tm
                 -> Proxy n
                 -> Property
propUnknownNever c _ =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    ctx :: r n String
    ctx = mempty \\ (spanMonoid2 :: Ord String :- Monoid (r n String))
    infer' :: tm n n String -> Either (e n String) (ty n)
    infer' = runInfer ctx . view infer c
      \\ (spanEq3 :: (Eq n, Eq n, Eq String) :- Eq (tm n n String))
    test tm = property $
      case infer' tm of
        Left e -> isn't _UnknownType e
        Right _ -> True
  in
    forAllShrink genAnyTerm' shrAnyTerm' test
      \\ (spanShow3 :: (Show n, Show n, Show String) :- Show (tm n n String))

propWellTypedInfer :: forall r e ty tm n. (
                        Show3 tm
                      , Show n
                      , Eq n
                      , Monoid2 r
                      )
                   => ComponentOutput r e ty tm
                   -> Proxy n
                   -> Property
propWellTypedInfer c p =
  let
    shrWellTypedTerm' = view shrWellTypedTerm c
    ctx :: r n String
    ctx = mempty \\ (spanMonoid2 :: Ord String :- Monoid (r n String))
    infer' = runInfer ctx . view infer c
    forAllWellTypedTerm' = view forAllWellTypedTerm c p p
    test :: tm n n String -> Bool
    test tm =
      all (isRight . infer') $ tm : shrWellTypedTerm' tm
  in
    forAllWellTypedTerm' test

propIllTypedInfer :: forall r e ty tm n. (
                       Show3 tm
                     , Show n
                     , Eq3 tm
                     , Eq n
                     , Monoid2 r
                     )
                  => ComponentOutput r e ty tm
                  -> Proxy n
                  -> Property
propIllTypedInfer c _ =
  let
    genIllTypedTerm' = view genIllTypedTerm c
    genContainingTerm' = view genContainingTerm c
    shrContainingTerm' = view shrContainingTerm c
    genAnyType' = view genAnyType c
    gen = do
      ty <- genAnyType'
      tm <- genIllTypedTerm' ty
      genContainingTerm' tm ty
    ctx :: r n String
    ctx = mempty \\ (spanMonoid2 :: Ord String :- Monoid (r n String))
    infer' :: tm n n String -> Either (e n String) (ty n)
    infer' = runInfer ctx . view infer c
      \\ (spanEq3 :: (Eq n, Eq n, Eq String) :- Eq (tm n n String))
    test tm =
      -- all (isLeft . infer') $ tm : shrIllTypedTerm' tm
      isLeft . infer' $ tm
  in
    forAllShrink gen shrContainingTerm' test
      \\ (spanShow3 :: (Show n, Show n, Show String) :- Show (tm n n String))

propProgress :: forall r e ty tm n. (
                  Show3 tm
                , Show n
                , Eq n
                , Monoid2 r
                )
             => ComponentOutput r e ty tm
             -> Proxy n
             -> Property
propProgress c p =
  let
    ctx :: r n String
    ctx = mempty \\ (spanMonoid2 :: Ord String :- Monoid (r n String))
    infer' = runInfer ctx . view infer c
    isValue' = view isValue c
    canStep' = view canStep c
    forAllWellTypedTerm' = view forAllWellTypedTerm c p p
    test :: tm n n String -> Property
    test tm =
      case infer' tm of
        Left _ -> property True
        Right _ -> isValue' tm .||. canStep' tm
  in
    forAllWellTypedTerm' test

propPreservation :: forall r e ty tm n. (
                      Eq3 tm
                    , Eq1 ty
                    , Eq2 e
                    , Eq n
                    , Show3 tm
                    , Show1 ty
                    , Show2 e
                    , Show n
                    , Monoid2 r
                    )
                 => ComponentOutput r e ty tm
                 -> Proxy n
                 -> Property
propPreservation c p =
  let
    smallStep' = view smallStep c
    ctx :: r n String
    ctx = mempty \\ (spanMonoid2 :: Ord String :- Monoid (r n String))
    infer' = runInfer ctx . view infer c
    forAllWellTypedTerm' = view forAllWellTypedTerm c p p
    test :: (Eq (ty n), Eq (e n String), Show (ty n), Show (e n String)) => tm n n String -> Property
    test tm =
      case smallStep' tm of
        Nothing -> property True
        Just tm' -> infer' tm === infer' tm'
  in
    forAllWellTypedTerm' $
      test
        \\ (spanEq1 :: Eq n :- Eq (ty n))
        \\ (spanEq2 :: (Eq n, Eq String) :- Eq (e n String))
        \\ (spanShow1 :: Show n :- Show (ty n))
        \\ (spanShow2 :: (Show n, Show String) :- Show (e n String))
