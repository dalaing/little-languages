{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.NatBool.Gen (
    genTermInput
  ) where

import           Control.Lens         (preview, review)
import           Test.QuickCheck      (Gen)

import Component.Term.Gen (GenAnyTermRule(..), ShrAnyTermRule(..), GenTermInput(..), GenContainingTermRule(..), GenWellTypedTermRule(..), GenIllTypedTermRule(..))

import           Component.Type.Nat (AsNatType (..), WithNatType)
import           Component.Type.Bool (AsBoolType (..), WithBoolType)
import           Component.Term.NatBool (AsNatBoolTerm (..), WithNatBoolTerm)

-- |
genTmIsZero :: WithNatBoolTerm tm
            => Gen (tm nTy nTm a)
            -> Gen (tm nTy nTm a)
genTmIsZero g =
  review _TmIsZero <$> g

-- |
shrinkTmIsZero :: WithNatBoolTerm tm
             => (tm nTy nTm a -> [tm nTy nTm a])
             -> tm nTy nTm a        -- ^
             -> Maybe [tm nTy nTm a] -- ^
shrinkTmIsZero s =
    fmap shrinkTmIsZero' .
    preview _TmIsZero
  where
    shrinkTmIsZero' tm =
      s tm ++ [tm] ++
      fmap (review _TmIsZero) (s tm)

genContainingTmIsZero :: ( WithNatBoolTerm tm
                         , WithNatType ty
                         , WithBoolType ty
                         )
                      => (ty nTy -> Int -> Gen (tm nTy nTm a))
                      -> (tm nTy nTm a -> ty nTy -> Int -> Gen (tm nTy nTm a))
                      -> tm nTy nTm a
                      -> ty nTy
                      -> Int
                      -> Maybe (Gen (tm nTy nTm a))
genContainingTmIsZero _ genContaining tm ty s =
    fmap genContainingTmIsZero' .
    preview _TyBool $
    ty
  where
    genContainingTmIsZero' _ = do
      let s' = (s - 1) `max` 0
      tm1 <- genContaining tm (review _TyNat ()) s'
      return $ review _TmIsZero tm1

genWellTypedTmIsZero :: ( WithNatBoolTerm tm
                        , WithNatType ty
                        , WithBoolType ty
                        )
                     => (ty nTy -> Int -> Gen (tm nTy nTm a))
                     -> ty nTy
                     -> Int
                     -> Maybe (Gen (tm nTy nTm a))
genWellTypedTmIsZero genWellTyped ty s = do
  _ <- preview _TyBool ty
  let s' = (s - 1) `max` 0
  return $ do
    tm1 <- genWellTyped (review _TyNat ()) s'
    return $ review _TmIsZero tm1

genIllTypedTmIsZero :: ( WithNatBoolTerm tm
                       , WithNatType ty
                       , WithBoolType ty
                       )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTy nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTy nTm a))
genIllTypedTmIsZero genNotType genWellTyped ty s = do
  _ <- preview _TyBool ty
  let s' = (s - 1) `max` 0
  return $ do
    nty <- genNotType (review _TyNat ())
    tm1 <- genWellTyped nty s'
    return $ review _TmIsZero tm1

genTermInput :: ( WithNatBoolTerm tm
                , WithNatType ty
                , WithBoolType ty
                )
             => GenTermInput ty tm
genTermInput =
  GenTermInput
    [GenAnyTermRecurse $ \ g s -> genTmIsZero (g ((s - 1) `max` 0))]
    [ShrAnyTermRecurse shrinkTmIsZero]
    [GenContainingTermRecurse genContainingTmIsZero]
    [GenWellTypedTermRecurse genWellTypedTmIsZero]
    [GenIllTypedTermRecurse genIllTypedTmIsZero]
