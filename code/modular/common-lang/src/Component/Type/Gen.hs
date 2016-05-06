{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Component.Type.Gen (
    GenAnyTypeRule(..)
  , ShrAnyTypeRule(..)
  , GenNotTypeRule(..)
  , ShrNotTypeRule(..)
  , GenTypeInput(..)
  , GenTypeOutput(..)
  , HasGenTypeOutput(..)
  , mkGenType
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe, mapMaybe)

import Control.Lens.TH (makeClassy)
import Test.QuickCheck (Gen, oneof, sized)

-- |
data GenAnyTypeRule ty =
    GenAnyTypeBase (forall nTy. Gen (ty nTy)) -- ^
  | GenAnyTypeRecurse (forall nTy. (Int -> Gen (ty nTy)) -> Int -> Gen (ty nTy)) -- ^

-- |
fixGenAnyTypeRule :: (Int -> Gen (ty nTy))
                  -> Int
                  -> GenAnyTypeRule ty
                  -> Maybe (Gen (ty nTy))
fixGenAnyTypeRule _ _ (GenAnyTypeBase f) =
  Just f
fixGenAnyTypeRule _ 0 _ =
  Nothing
fixGenAnyTypeRule step s (GenAnyTypeRecurse f) =
  Just $ f step s

-- |
data ShrAnyTypeRule ty =
    ShrAnyTypeBase (forall nTy. ty nTy -> Maybe [ty nTy]) -- ^
  | ShrAnyTypeRecurse (forall nTy. (ty nTy -> [ty nTy]) -> ty nTy -> Maybe [ty nTy]) -- ^

-- |
fixShrAnyTypeRule :: (ty nTy -> [ty nTy])
                  -> ShrAnyTypeRule ty
                  -> ty nTy
                  -> Maybe [ty nTy]
fixShrAnyTypeRule _ (ShrAnyTypeBase f) x =
  f x
fixShrAnyTypeRule step (ShrAnyTypeRecurse f) x =
  f step x

-- |
data GenNotTypeRule ty =
    GenNotTypeBase (forall nTy. ty nTy -> Maybe (Gen (ty nTy))) -- ^
  | GenNotTypeRecurse (forall nTy. Gen (ty nTy) -> ty nTy -> Maybe (Gen (ty nTy))) -- ^

-- |
fixGenNotTypeRule :: Gen (ty nTy)
                  -> ty nTy
                  -> GenNotTypeRule ty
                  -> Maybe (Gen (ty nTy))
fixGenNotTypeRule _ x (GenNotTypeBase f) =
  f x
fixGenNotTypeRule genType x (GenNotTypeRecurse f) =
  f genType x

-- |
data ShrNotTypeRule ty =
    ShrNotTypeBase (forall nTy. ty nTy -> Maybe [ty nTy]) -- ^
  | ShrNotTypeRecurse (forall nTy. (ty nTy -> [ty nTy]) -> ty nTy -> Maybe [ty nTy]) -- ^

-- we need to tag then gens for notType, so we know what to
-- avoid when we are shrinking

-- we'll reuse this trick a lot in the gens for terms

-- |
fixShrNotTypeRule :: (ty nTy -> [ty nTy])
                  -> ShrNotTypeRule ty
                  -> ty nTy
                  -> Maybe [ty nTy]
fixShrNotTypeRule _ (ShrNotTypeBase f) x =
  f x
fixShrNotTypeRule step (ShrNotTypeRecurse f) x =
  f step x

-- |
data GenTypeInput ty =
  GenTypeInput
    [GenAnyTypeRule ty]
    [ShrAnyTypeRule ty]
    [GenNotTypeRule ty]
    [ShrNotTypeRule ty]

instance Monoid (GenTypeInput ty) where
  mempty =
    GenTypeInput mempty mempty mempty mempty
  mappend (GenTypeInput ga1 sa1 gn1 sn1) (GenTypeInput ga2 sa2 gn2 sn2) =
    GenTypeInput
      (mappend ga1 ga2)
      (mappend sa1 sa2)
      (mappend gn1 gn2)
      (mappend sn1 sn2)

-- |
data GenTypeOutput ty =
  GenTypeOutput {
    _genAnyType      :: forall nTy. Gen (ty nTy)     -- ^
  , _shrAnyType      :: forall nTy. ty nTy -> [ty nTy] -- ^
  , _genNotType      :: forall nTy. ty nTy -> Gen (ty nTy) -- ^
  , _shrNotType      :: forall nTy. ty nTy -> [ty nTy] -- ^
  }

makeClassy ''GenTypeOutput

-- |
mkGenType :: GenTypeInput ty -- ^
          -> GenTypeOutput ty -- ^
mkGenType (GenTypeInput ga sa gn sn) =
  let
    genAnyType' s =
      oneof .
      mapMaybe (fixGenAnyTypeRule genAnyType' s) $
      ga
    shrAnyType' ty =
      fromMaybe [] .
      asum .
      fmap (\r -> fixShrAnyTypeRule shrAnyType' r ty) $
      sa
    genNotType' ty =
      oneof .
      mapMaybe (fixGenNotTypeRule (sized genAnyType') ty) $
      gn
    shrNotType' ty =
      fromMaybe [] .
      asum .
      fmap (\r -> fixShrNotTypeRule shrAnyType' r ty) $
      sn
  in
    GenTypeOutput
      (sized genAnyType')
      shrAnyType'
      genNotType'
      shrNotType'
