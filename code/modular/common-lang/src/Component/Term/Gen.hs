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
module Component.Term.Gen (
    GenAnyTermRule(..)
  , ShrAnyTermRule(..)
  , GenContainingTermRule(..)
  , GenWellTypedTermRule(..)
  , GenIllTypedTermRule(..)
  , GenTermInput(..)
  , GenTermOutput(..)
  , HasGenTermOutput(..)
  , mkGenTerm
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe, mapMaybe)

import Control.Lens.TH (makeClassy)
import Test.QuickCheck (Gen, oneof, sized)

import Component.Type.Gen (GenTypeOutput(..))

-- |
data GenAnyTermRule tm n a =
    GenAnyTermBase (Gen (tm n a)) -- ^
  | GenAnyTermRecurse ((Int -> Gen (tm n a)) -> Int -> Gen (tm n a)) -- ^

-- |
fixGenAnyTermRule :: (Int -> Gen (tm n a))
                  -> Int
                  -> GenAnyTermRule tm n a
                  -> Maybe (Gen (tm n a))
fixGenAnyTermRule _ _ (GenAnyTermBase f) =
  Just f
fixGenAnyTermRule _ 0 _ =
  Nothing
fixGenAnyTermRule step s (GenAnyTermRecurse f) =
  Just $ f step s

-- |
data ShrAnyTermRule tm n a =
    ShrAnyTermBase (tm n a -> Maybe [tm n a]) -- ^
  | ShrAnyTermRecurse ((tm n a -> [tm n a]) -> tm n a -> Maybe [tm n a]) -- ^

-- |
fixShrAnyTermRule :: (tm n a -> [tm n a])
                  -> ShrAnyTermRule tm n a
                  -> tm n a
                  -> Maybe [tm n a]
fixShrAnyTermRule _ (ShrAnyTermBase f) x =
  f x
fixShrAnyTermRule step (ShrAnyTermRecurse f) x =
  f step x

data GenContainingTermRule ty nTy tm nTm a =
    GenContainingTermBase (
         tm nTm a
      -> ty nTy
      -> Maybe (Gen (tm nTm a))
      )
  | GenContainingTermRecurse (
         (ty nTy -> Int -> Gen (tm nTm a))
      -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
      -> tm nTm a
      -> ty nTy
      -> Int
      -> Maybe (Gen (tm nTm a))
      ) -- ^

fixGenContainingTermRule :: (ty nTy -> Int -> Gen (tm nTm a))
                         -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                         -> tm nTm a
                         -> ty nTy
                         -> Int
                         -> GenContainingTermRule ty nTy tm nTm a
                         -> Maybe (Gen (tm nTm a))
fixGenContainingTermRule _ _ tm ty _ (GenContainingTermBase f) =
  f tm ty
fixGenContainingTermRule _ _ tm _ 0 _ =
  Just $ pure tm
fixGenContainingTermRule wellTyped containing tm ty s (GenContainingTermRecurse f) =
  f wellTyped containing tm ty s

data GenWellTypedTermRule ty nTy tm nTm a =
    GenWellTypedTermBase (ty nTy -> Maybe (Gen (tm nTm a))) -- ^
  | GenWellTypedTermRecurse (
         (ty nTy -> Int -> Gen (tm nTm a))
      -> ty nTy
      -> Int
      -> Maybe (Gen (tm nTm a))
    ) -- ^

-- |
fixGenWellTypedTermRule :: (ty nTy -> Int -> Gen (tm nTm a))
                        -> ty nTy
                        -> Int
                        -> GenWellTypedTermRule ty nTy tm nTm a
                        -> Maybe (Gen (tm nTm a))
fixGenWellTypedTermRule _ ty _ (GenWellTypedTermBase f) =
  f ty
fixGenWellTypedTermRule _ _ 0 _ =
  Nothing
fixGenWellTypedTermRule step ty s (GenWellTypedTermRecurse f) =
  f step ty s

data GenIllTypedTermRule ty nTy tm nTm a =
   GenIllTypedTermRecurse (
         (ty nTy -> Gen (ty nTy))
      -> (ty nTy -> Int -> Gen (tm nTm a))
      -> ty nTy
      -> Int
      -> Maybe (Gen (tm nTm a))
      ) -- ^

-- |
fixGenIllTypedTermRule :: (ty nTy -> Gen (ty nTy))
                       -> (ty nTy -> Int -> Gen (tm nTm a))
                       -> ty nTy
                       -> Int
                       -> GenIllTypedTermRule ty nTy tm nTm a
                       -> Maybe (Gen (tm nTm a))
fixGenIllTypedTermRule _ wellTyped ty 0 _ =
  Just $ wellTyped ty 0
fixGenIllTypedTermRule notType wellTyped ty s (GenIllTypedTermRecurse f) =
  f notType wellTyped ty s

-- |
data GenTermInput ty nTy tm nTm a =
  GenTermInput
    [GenAnyTermRule tm nTm a]
    [ShrAnyTermRule tm nTm a]
    [GenContainingTermRule ty nTy tm nTm a]
    [GenWellTypedTermRule ty nTy tm nTm a]
    [GenIllTypedTermRule ty nTy tm nTm a]

instance Monoid (GenTermInput ty nTy tm nTm a) where
  mempty =
    GenTermInput
      mempty
      mempty
      mempty
      mempty
      mempty
  mappend
    (GenTermInput ga1 sa1 gc1 gw1 gi1)
    (GenTermInput ga2 sa2 gc2 gw2 gi2) =
      GenTermInput
        (mappend ga1 ga2)
        (mappend sa1 sa2)
        (mappend gc1 gc2)
        (mappend gw1 gw2)
        (mappend gi1 gi2)


-- |
data GenTermOutput ty nTy tm nTm a =
  GenTermOutput {
    _genAnyTerm        :: Gen (tm nTm a)             -- ^
  , _shrAnyTerm        :: tm nTm a -> [tm nTm a]         -- ^
  , _genContainingTerm :: tm nTm a -> ty nTy -> Gen (tm nTm a)       -- ^
  , _shrContainingTerm :: tm nTm a -> [tm nTm a]         -- ^
  , _genWellTypedTerm  :: ty nTy -> Gen (tm nTm a) -- ^
  , _shrWellTypedTerm  :: tm nTm a -> [tm nTm a]         -- ^
  , _genIllTypedTerm   :: ty nTy -> Gen (tm nTm a) -- ^
  , _shrIllTypedTerm   :: tm nTm a -> [tm nTm a]         -- ^
  }

makeClassy ''GenTermOutput

-- |
mkGenTerm :: GenTypeOutput ty nTy
          -> GenTermInput ty nTy tm nTm a -- ^
          -> GenTermOutput ty nTy tm nTm a -- ^
mkGenTerm (GenTypeOutput _ _ notType _) (GenTermInput ga sa gc gw gi) =
  let
    genAnyTerm' s =
      oneof .
      mapMaybe (fixGenAnyTermRule genAnyTerm' s) $
      ga
    shrAnyTerm' tm =
      fromMaybe [] .
      asum .
      fmap (\r -> fixShrAnyTermRule shrAnyTerm' r tm) $
      sa
    genContainingTerm' tm ty s =
      oneof .
      mapMaybe (fixGenContainingTermRule genWellTypedTerm' genContainingTerm' tm ty s) $
      gc
    shrContainingTerm' _ = []
    genWellTypedTerm' ty s =
      oneof .
      mapMaybe (fixGenWellTypedTermRule genWellTypedTerm' ty s) $
      gw
    shrWellTypedTerm' _ = []
    genIllTypedTerm' ty s =
      oneof .
      mapMaybe (fixGenIllTypedTermRule notType genWellTypedTerm' ty s) $
      gi
    shrIllTypedTerm' _ = []
  in
    GenTermOutput
      (sized genAnyTerm')
      shrAnyTerm'
      (\tm ty -> sized $ genContainingTerm' tm ty)
      shrContainingTerm'
      (sized . genWellTypedTerm')
      shrWellTypedTerm'
      (sized . genIllTypedTerm')
      shrIllTypedTerm'

