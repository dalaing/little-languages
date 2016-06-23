module Common.Test.Term.Infer where

import Data.Maybe (mapMaybe, isNothing)

import Control.Lens (preview, view)
import Control.Monad.Except (runExcept)

import Test.Tasty
import Test.Tasty.QuickCheck

import Common.Type.Error
import Common.Term
import Common.Term.Gen
import Common.Term.Infer

inferTests :: ( Show tm
              , AsUnknownType e n
              )
           => TermOutput e ty tm
           -> TestTree
inferTests t =
  testGroup "infer" [
    testProperty "wellTypedInfer" (propWellTypedInfer t)
  , testProperty "illTypedInfer" (propIllTypedInfer t)
  , testProperty "inferUnique" (propInferUnique t)
  , testProperty "neverUnknown" (propNeverUnknown t)
  ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

propWellTypedInfer :: Show tm
                   => TermOutput e ty tm
                   -> Property
propWellTypedInfer t =
  forAllShrink (view genWellTypedTerm t Nothing) (view shrinkTerm t) $
    isRight .
    runExcept .
    runInfer .
    view infer t

propIllTypedInfer :: Show tm
                  => TermOutput e ty tm
                  -> Property
propIllTypedInfer t =
  forAllShrink (view genIllTypedTerm t Nothing) (view shrinkTerm t) $
    isLeft .
    runExcept .
    runInfer .
    view infer t

propInferUnique :: Show tm
                => TermOutput e ty tm
                -> Property
propInferUnique t =
  forAllShrink (view genAnyTerm t) (view shrinkTerm t) $ \tm ->
    let
      matches =
        length .
        mapMaybe ($ tm) $
        view inferRules t
    in
      matches === 1

propNeverUnknown :: ( Show tm
                    , AsUnknownType e n
                    )
                 => TermOutput e ty tm
                 -> Property
propNeverUnknown t =
  forAllShrink (view genAnyTerm t) (view shrinkTerm t) $ \tm ->
    case (runExcept . runInfer . view infer t) tm of
      Left e -> isNothing . preview _UnknownType $ e
      _ -> True
