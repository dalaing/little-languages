{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Term.Infer (
    mkInferTests
  ) where

import           Control.Lens          (view)
import           Control.Lens.Prism    (isn't)
import           Data.Maybe            (mapMaybe)
import           Test.QuickCheck       (Property, forAllShrink, property,
                                        (.||.), (===))
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import Component.Type.Error.UnknownType.Class (AsUnknownType(..))
import           Component             (ComponentOutput)
import           Component.Type.Gen    (HasGenTypeOutput (..))
import           Component.Term.Gen    (HasGenTermOutput (..), forAllWellTypedTerm)
import           Component.Term.Infer  (HasInferOutput (..), runInfer)
import           Component.Term.Eval.Value (HasValueOutput (..))
import           Component.Term.Eval.SmallStep (HasSmallStepOutput (..))

mkInferTests :: ( Eq e
                , Show e
                , Eq (tm nTy nTm String)
                , Show (tm nTy nTm String)
                , Eq (ty nTy)
                , Show (ty nTy)
                , AsUnknownType e
                , Monoid r
                )
             => ComponentOutput r e ty nTy tm nTm String
             -> TestTree
mkInferTests c =
  testGroup "infer"
    [ testProperty "patterns unique" $ propPatternUnique c
    , testProperty "unknown never occurs" $ propUnknownNever c
    , testProperty "well-typed infer" $ propWellTypedInfer c
    , testProperty "ill-typed infer" $ propIllTypedInfer c
    , testProperty "progress" $ propProgress c
    , testProperty "preservation" $ propPreservation c
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

propPatternUnique :: ( Show (tm nTy nTm String)
                     , Monoid r
                     )
                  => ComponentOutput r e ty nTy tm nTm String
                  -> Property
propPatternUnique c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    inferRules' = view inferRules c
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm ->
    let
      matches =
        length .
        mapMaybe (\i -> fmap (runInfer mempty) . i $ tm) $
        inferRules'
    in
      matches === 1

propUnknownNever :: ( Show (tm nTy nTm a)
                    , AsUnknownType e
                    , Monoid r
                    )
                 => ComponentOutput r e ty nTy tm nTm a
                 -> Property
propUnknownNever c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    infer' = runInfer mempty . view infer c
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm -> property $
      case infer' tm of
        Left e -> isn't _UnknownType e
        Right _ -> True

propWellTypedInfer :: ( Show (tm nTy nTm a)
                      , Monoid r
                      )
                   => ComponentOutput r e ty nTy tm nTm a
                   -> Property
propWellTypedInfer c =
  let
    shrWellTypedTerm' = view shrWellTypedTerm c
    infer' = runInfer mempty . view infer c
  in
    forAllWellTypedTerm c $ \tm ->
      all (isRight . infer') $ tm : shrWellTypedTerm' tm

propIllTypedInfer :: ( Show (tm nTy nTm a)
                     , Monoid r
                     )
                  => ComponentOutput r e ty nTy tm nTm a
                  -> Property
propIllTypedInfer c =
  let
    genIllTypedTerm' = view genIllTypedTerm c
    genContainingTerm' = view genContainingTerm c
    shrContainingTerm' = view shrContainingTerm c
    genAnyType' = view genAnyType c
    gen = do
      ty <- genAnyType'
      tm <- genIllTypedTerm' ty
      genContainingTerm' tm ty
    infer' = runInfer mempty . view infer c
  in
    forAllShrink gen shrContainingTerm' $ \tm ->
      -- all (isLeft . infer') $ tm : shrIllTypedTerm' tm
      isLeft . infer' $ tm

propProgress :: ( Show (tm nTy nTm String)
                , Monoid r
                )
             => ComponentOutput r e ty nTy tm nTm String
             -> Property
propProgress c =
  let
    infer' = runInfer mempty . view infer c
    isValue' = view isValue c
    canStep' = view canStep c
  in
    forAllWellTypedTerm c $ \tm ->
      case infer' tm of
        Left _ -> property True
        Right _ -> isValue' tm .||. canStep' tm

propPreservation :: ( Eq e
                    , Show e
                    , Eq (tm nTy nTm String)
                    , Show (tm nTy nTm String)
                    , Eq (ty nTy)
                    , Show (ty nTy)
                    , Monoid r
                    )
                 => ComponentOutput r e ty nTy tm nTm String
                 -> Property
propPreservation c =
  let
    smallStep' = view smallStep c
    infer' = runInfer mempty . view infer c
  in
    forAllWellTypedTerm c $ \tm ->
      case smallStep' tm of
        Nothing -> property True
        Just tm' -> infer' tm === infer' tm'
