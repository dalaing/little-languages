{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Type.Gen (
    MkAnyType(..)
  , MkNotType(..)
  , MkShrinkType(..)
  , GenTypeInput(..)
  , HasGenTypeInput(..)
  , GenTypeOutput(..)
  , HasGenTypeOutput(..)
  , mkGenType
  ) where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Foldable (asum)

import Control.Lens.TH (makeClassy)

import Test.QuickCheck (Gen, sized, oneof)

data MkAnyType ty =
    TyBase (Gen ty)
  | TyRecurse (Gen ty -> Gen ty)

mtToGen :: Gen ty
        -> Int
        -> MkAnyType ty
        -> Maybe (Gen ty)
mtToGen _ _ (TyBase g)    = Just g
mtToGen _ 0 _             = Nothing
mtToGen g _ (TyRecurse f) = Just $ f g

mkGenAnyType :: [MkAnyType ty]
             -> Gen ty
mkGenAnyType gs =
  sized $ \s ->
    mkGenAnyType' gs s

mkGenAnyType' :: [MkAnyType ty]
              -> Int
              -> Gen ty
mkGenAnyType' gs s =
    oneof .
    catMaybes .
    fmap (mtToGen child s') $
    gs
  where
    s' = s `div` 2
    child = mkGenAnyType' gs s'

data MkNotType ty =
    NTyBase (ty -> Maybe (Gen ty))
  | NTyRecurse (Gen ty -> (ty -> Gen ty) -> ty -> Maybe (Gen ty))

mntToGen :: Gen ty
         -> (ty -> Gen ty)
         -> ty
         -> Int
         -> MkNotType ty
         -> Maybe (Gen ty)
mntToGen _ _ ty _ (NTyBase f)       = f ty
mntToGen _ _ _ 0 _                  = Nothing
mntToGen gt gnt ty _ (NTyRecurse f) = f gt gnt ty

mkGenNotType :: [MkAnyType ty]
             -> [MkNotType ty]
             -> ty
             -> Gen ty
mkGenNotType tgs ntgs ty =
  sized $ \s ->
    mkGenNotType' tgs ntgs s ty

mkGenNotType' :: [MkAnyType ty]
              -> [MkNotType ty]
              -> Int
              -> ty
              -> Gen ty
mkGenNotType' tgs ntgs s ty =
    oneof .
    catMaybes .
    fmap (mntToGen childTy childNotTy ty s') $
    ntgs
  where
    s' = s `div` 2
    childTy = mkGenAnyType' tgs s'
    childNotTy = mkGenNotType' tgs ntgs s'

data MkShrinkType ty =
    ShrTyBase (ty -> Maybe [ty])
  | ShrTyRecurse ((ty -> [ty]) -> ty -> Maybe [ty])

msToShr :: (ty -> [ty])
        -> ty
        -> MkShrinkType ty
        -> Maybe [ty]
msToShr _ ty (ShrTyBase g)    = g ty
msToShr f ty (ShrTyRecurse g) = g f ty

mkShrink :: [MkShrinkType ty]
         -> ty
         -> [ty]
mkShrink ss =
    shr
  where
    shr ty =
      fromMaybe [] .
      asum .
      fmap (msToShr shr ty) $
      ss

data GenTypeInput ty =
  GenTypeInput {
    _typeGens :: [MkAnyType ty]
  , _notTypeGens :: [MkNotType ty]
  , _shrinkTypes :: [MkShrinkType ty]
  }

makeClassy ''GenTypeInput

-- TODO should this be a semigroup? a lack of rules is probably bad...
instance Monoid (GenTypeInput ty) where
  mempty =
    GenTypeInput mempty mempty mempty
  mappend (GenTypeInput t1 nt1 s1) (GenTypeInput t2 nt2 s2) =
    GenTypeInput (mappend t1 t2) (mappend nt1 nt2) (mappend s1 s2)

data GenTypeOutput ty =
  GenTypeOutput {
    _genAnyType :: Gen ty
  , _genNotType :: ty -> Gen ty
  , _shrinkType :: ty -> [ty]
  }

makeClassy ''GenTypeOutput

mkGenType :: GenTypeInput ty
          -> GenTypeOutput ty
mkGenType (GenTypeInput t nt s) =
  GenTypeOutput
    (mkGenAnyType t)
    (mkGenNotType t nt)
    (mkShrink s)
