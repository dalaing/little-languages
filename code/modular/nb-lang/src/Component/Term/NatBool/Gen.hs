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
genTmIsZero :: WithNatBoolTerm tm n a
            => Gen (tm n a)
            -> Gen (tm n a)
genTmIsZero g =
  review _TmIsZero <$> g

-- |
shrinkTmIsZero :: WithNatBoolTerm tm n a
             => (tm n a -> [tm n a])
             -> tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmIsZero s =
    fmap shrinkTmIsZero' .
    preview _TmIsZero
  where
    shrinkTmIsZero' tm =
      s tm ++ [tm] ++
      fmap (review _TmIsZero) (s tm)

genContainingTmIsZero :: ( WithNatBoolTerm tm nTm a
                         , WithNatType ty nTy
                         , WithBoolType ty nTy
                         )
                      => (ty nTy -> Int -> Gen (tm nTm a))
                      -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                      -> tm nTm a
                      -> ty nTy
                      -> Int
                      -> Maybe (Gen (tm nTm a))
genContainingTmIsZero _ genContaining tm ty s =
    fmap genContainingTmIsZero' .
    preview _TyBool $
    ty
  where
    genContainingTmIsZero' _ = do
      let s' = (s - 1) `max` 0
      tm1 <- genContaining tm (review _TyNat ()) s'
      return $ review _TmIsZero tm1

genWellTypedTmIsZero :: ( WithNatBoolTerm tm nTm a
                        , WithNatType ty nTy
                        , WithBoolType ty nTy
                        )
                     => (ty nTy -> Int -> Gen (tm nTm a))
                     -> ty nTy
                     -> Int
                     -> Maybe (Gen (tm nTm a))
genWellTypedTmIsZero genWellTyped ty s = do
  _ <- preview _TyBool ty
  let s' = (s - 1) `max` 0
  return $ do
    tm1 <- genWellTyped (review _TyNat ()) s'
    return $ review _TmIsZero tm1

genIllTypedTmIsZero :: ( WithNatBoolTerm tm nTm a
                       , WithNatType ty nTy
                       , WithBoolType ty nTy
                       )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmIsZero genNotType genWellTyped ty s = do
  _ <- preview _TyBool ty
  let s' = (s - 1) `max` 0
  return $ do
    nty <- genNotType (review _TyNat ())
    tm1 <- genWellTyped nty s'
    return $ review _TmIsZero tm1

genTermInput :: ( WithNatBoolTerm tm nTm a
                , WithNatType ty nTy
                , WithBoolType ty nTy
                )
             => GenTermInput ty nTy tm nTm a
genTermInput =
  GenTermInput
    [GenAnyTermRecurse $ \ g s -> genTmIsZero (g ((s - 1) `max` 0))]
    [ShrAnyTermRecurse shrinkTmIsZero]
    [GenContainingTermRecurse genContainingTmIsZero]
    [GenWellTypedTermRecurse genWellTypedTmIsZero]
    [GenIllTypedTermRecurse genIllTypedTmIsZero]
