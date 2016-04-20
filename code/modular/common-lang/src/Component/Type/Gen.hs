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
data GenAnyTypeRule ty n =
    GenAnyTypeBase (Gen (ty n)) -- ^
  | GenAnyTypeRecurse ((Int -> Gen (ty n)) -> Int -> Gen (ty n)) -- ^

-- |
fixGenAnyTypeRule :: (Int -> Gen (ty n))
                  -> Int
                  -> GenAnyTypeRule ty n
                  -> Maybe (Gen (ty n))
fixGenAnyTypeRule _ _ (GenAnyTypeBase f) =
  Just f
fixGenAnyTypeRule _ 0 _ =
  Nothing
fixGenAnyTypeRule step s (GenAnyTypeRecurse f) =
  Just $ f step s

-- |
data ShrAnyTypeRule ty n =
    ShrAnyTypeBase (ty n -> Maybe [ty n]) -- ^
  | ShrAnyTypeRecurse ((ty n -> [ty n]) -> ty n -> Maybe [ty n]) -- ^

-- |
fixShrAnyTypeRule :: (ty n -> [ty n])
                  -> ShrAnyTypeRule ty n
                  -> ty n
                  -> Maybe [ty n]
fixShrAnyTypeRule _ (ShrAnyTypeBase f) x =
  f x
fixShrAnyTypeRule step (ShrAnyTypeRecurse f) x =
  f step x

-- |
data GenNotTypeRule ty n =
    GenNotTypeBase (ty n -> Maybe (Gen (ty n))) -- ^
  | GenNotTypeRecurse (Gen (ty n) -> ty n -> Maybe (Gen (ty n))) -- ^

-- |
fixGenNotTypeRule :: Gen (ty n)
                  -> ty n
                  -> GenNotTypeRule ty n
                  -> Maybe (Gen (ty n))
fixGenNotTypeRule _ x (GenNotTypeBase f) =
  f x
fixGenNotTypeRule genType x (GenNotTypeRecurse f) =
  f genType x

-- |
data ShrNotTypeRule ty n =
    ShrNotTypeBase (ty n -> Maybe [ty n]) -- ^
  | ShrNotTypeRecurse ((ty n -> [ty n]) -> ty n -> Maybe [ty n]) -- ^

-- we need to tag then gens for notType, so we know what to
-- avoid when we are shrinking

-- we'll reuse this trick a lot in the gens for terms

-- |
fixShrNotTypeRule :: (ty n -> [ty n])
                  -> ShrNotTypeRule ty n
                  -> ty n
                  -> Maybe [ty n]
fixShrNotTypeRule _ (ShrNotTypeBase f) x =
  f x
fixShrNotTypeRule step (ShrNotTypeRecurse f) x =
  f step x

-- |
data GenTypeInput ty n =
  GenTypeInput
    [GenAnyTypeRule ty n]
    [ShrAnyTypeRule ty n]
    [GenNotTypeRule ty n]
    [ShrNotTypeRule ty n]

instance Monoid (GenTypeInput ty n) where
  mempty =
    GenTypeInput mempty mempty mempty mempty
  mappend (GenTypeInput ga1 sa1 gn1 sn1) (GenTypeInput ga2 sa2 gn2 sn2) =
    GenTypeInput
      (mappend ga1 ga2)
      (mappend sa1 sa2)
      (mappend gn1 gn2)
      (mappend sn1 sn2)

-- |
data GenTypeOutput ty n =
  GenTypeOutput {
    _genAnyType      :: Gen (ty n)     -- ^
  , _shrAnyType      :: ty n -> [ty n] -- ^
  , _genNotType      :: ty n -> Gen (ty n) -- ^
  , _shrNotType      :: ty n -> [ty n] -- ^
  }

makeClassy ''GenTypeOutput

-- |
mkGenType :: GenTypeInput ty n -- ^
          -> GenTypeOutput ty n -- ^
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
