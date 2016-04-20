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

import           Control.Lens          (view, Lens')
import           Control.Lens.Prism    (isn't)
import           Data.Maybe            (mapMaybe)
import           Test.QuickCheck       (Property, forAllShrink, property,
                                        (.||.), (===), Gen)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import Component.Type.Error.UnknownType.Class (AsUnknownType(..))
import           Component             (ComponentOutput)
import           Component.Term.Gen    (HasGenTermOutput (..))
import           Component.Term.Infer  (HasInferOutput (..), runInfer)
import           Component.Term.Eval.Value (HasValueOutput (..))
import           Component.Term.Eval.SmallStep (HasSmallStepOutput (..))

mkInferTests :: ( Eq e
                , Show e
                , Eq (tm nTm a)
                , Show (tm nTm a)
                , Eq (ty nTy)
                , Show (ty nTy)
                , AsUnknownType e
                , Monoid r
                )
             => ComponentOutput r e ty nTy tm nTm a
             -> TestTree
mkInferTests c =
  testGroup "infer"
    [ testProperty "patterns unique" $ propPatternUnique c
    , testProperty "unknown never occurs" $ propUnknownNever c
    , testProperty "well-typed infer" $ propWellTypedInfer c
--    , testProperty "ill-typed infer" $ propIllTypedInfer c
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

-- temporary hack until we revive the more interesting Gens
-- remove Gen from imports when this goes
genWellTypedTerm :: HasGenTermOutput c tm n a => Lens' c (Gen (tm n a))
genWellTypedTerm = genAnyTerm
shrWellTypedTerm :: HasGenTermOutput c tm n a => Lens' c (tm n a -> [tm n a])
shrWellTypedTerm = shrAnyTerm

propPatternUnique :: ( Show (tm nTm a)
                     , Monoid r
                     )
                  => ComponentOutput r e ty nTy tm nTm a
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

propUnknownNever :: ( Show (tm nTm a)
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

propWellTypedInfer :: ( Show (tm nTm a)
                      , Monoid r
                      )
                   => ComponentOutput r e ty nTy tm nTm a
                   -> Property
propWellTypedInfer c =
  let
    genWellTypedTerm' = view genWellTypedTerm c
    shrWellTypedTerm' = view shrWellTypedTerm c
    infer' = runInfer mempty . view infer c
  in
    forAllShrink genWellTypedTerm' shrWellTypedTerm' $
      isRight .
      infer'

{-
propIllTypedInfer :: ( Show (tm nTm a)
                     , Monoid r
                     )
                  => ComponentOutput r e ty nTy tm nTm a
                  -> Property
propIllTypedInfer c =
  let
    genIllTypedTerm' = view genIllTypedTerm c
    shrIllTypedTerm' = view shrIllTypedTerm c
    infer' = runInfer mempty . view infer c
  in
    forAllShrink genIllTypedTerm' shrIllTypedTerm' $
      isLeft .
      infer'
-}

propProgress :: ( Show (tm nTm a)
                , Monoid r
                )
             => ComponentOutput r e ty nTy tm nTm a
             -> Property
propProgress c =
  let
    genWellTypedTerm' = view genWellTypedTerm c
    shrWellTypedTerm' = view shrWellTypedTerm c
    infer' = runInfer mempty . view infer c
    isValue' = view isValue c
    canStep' = view canStep c
  in
    forAllShrink genWellTypedTerm' shrWellTypedTerm' $ \tm ->
      case infer' tm of
        Left _ -> property True
        Right _ -> isValue' tm .||. canStep' tm

propPreservation :: ( Eq e
                    , Show e
                    , Eq (tm nTm a)
                    , Show (tm nTm a)
                    , Eq (ty nTy)
                    , Show (ty nTy)
                    , Monoid r
                    )
                 => ComponentOutput r e ty nTy tm nTm a
                 -> Property
propPreservation c =
  let
    genWellTypedTerm' = view genWellTypedTerm c
    shrWellTypedTerm' = view shrWellTypedTerm c
    smallStep' = view smallStep c
    infer' = runInfer mempty . view infer c
  in
    forAllShrink genWellTypedTerm' shrWellTypedTerm' $ \tm ->
      case smallStep' tm of
        Nothing -> property True
        Just tm' -> infer' tm === infer' tm'
