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
  , forAllWellTypedTerm
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe, mapMaybe)

import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Test.QuickCheck (Gen, oneof, sized, forAllShrink, Property, Testable)

import Component.Type.Gen (GenTypeOutput(..), HasGenTypeOutput(..))

-- |
data GenAnyTermRule tm nTy nTm a =
    GenAnyTermBase (Gen (tm nTy nTm a)) -- ^
  | GenAnyTermRecurse ((Int -> Gen (tm nTy nTm a)) -> Int -> Gen (tm nTy nTm a)) -- ^

-- |
fixGenAnyTermRule :: (Int -> Gen (tm nTy nTm a))
                  -> Int
                  -> GenAnyTermRule tm nTy nTm a
                  -> Maybe (Gen (tm nTy nTm a))
fixGenAnyTermRule _ _ (GenAnyTermBase f) =
  Just f
fixGenAnyTermRule _ 0 _ =
  Nothing
fixGenAnyTermRule step s (GenAnyTermRecurse f) =
  Just $ f step s

-- |
data ShrAnyTermRule tm nTy nTm a =
    ShrAnyTermBase (tm nTy nTm a -> Maybe [tm nTy nTm a]) -- ^
  | ShrAnyTermRecurse ((tm nTy nTm a -> [tm nTy nTm a]) -> tm nTy nTm a -> Maybe [tm nTy nTm a]) -- ^

-- |
fixShrAnyTermRule :: (tm nTy nTm a -> [tm nTy nTm a])
                  -> ShrAnyTermRule tm nTy nTm a
                  -> tm nTy nTm a
                  -> Maybe [tm nTy nTm a]
fixShrAnyTermRule _ (ShrAnyTermBase f) x =
  f x
fixShrAnyTermRule step (ShrAnyTermRecurse f) x =
  f step x

data GenContainingTermRule ty nTy tm nTm a =
    GenContainingTermBase (
         tm nTy nTm a
      -> ty nTy
      -> Maybe (Gen (tm nTy nTm a))
      )
  | GenContainingTermRecurse (
         (ty nTy -> Int -> Gen (tm nTy nTm a))
      -> (tm nTy nTm a -> ty nTy -> Int -> Gen (tm nTy nTm a))
      -> tm nTy nTm a
      -> ty nTy
      -> Int
      -> Maybe (Gen (tm nTy nTm a))
      ) -- ^

fixGenContainingTermRule :: (ty nTy -> Int -> Gen (tm nTy nTm a))
                         -> (tm nTy nTm a -> ty nTy -> Int -> Gen (tm nTy nTm a))
                         -> tm nTy nTm a
                         -> ty nTy
                         -> Int
                         -> GenContainingTermRule ty nTy tm nTm a
                         -> Maybe (Gen (tm nTy nTm a))
fixGenContainingTermRule _ _ tm ty _ (GenContainingTermBase f) =
  f tm ty
fixGenContainingTermRule _ _ tm _ 0 _ =
  Just $ pure tm
fixGenContainingTermRule wellTyped containing tm ty s (GenContainingTermRecurse f) =
  f wellTyped containing tm ty s

data GenWellTypedTermRule ty nTy tm nTm a =
    GenWellTypedTermBase (ty nTy -> Maybe (Gen (tm nTy nTm a))) -- ^
  | GenWellTypedTermRecurse (
         (ty nTy -> Int -> Gen (tm nTy nTm a))
      -> ty nTy
      -> Int
      -> Maybe (Gen (tm nTy nTm a))
    ) -- ^

-- |
fixGenWellTypedTermRule :: (ty nTy -> Int -> Gen (tm nTy nTm a))
                        -> ty nTy
                        -> Int
                        -> GenWellTypedTermRule ty nTy tm nTm a
                        -> Maybe (Gen (tm nTy nTm a))
fixGenWellTypedTermRule _ ty _ (GenWellTypedTermBase f) =
  f ty
fixGenWellTypedTermRule _ _ 0 _ =
  Nothing
fixGenWellTypedTermRule step ty s (GenWellTypedTermRecurse f) =
  f step ty s

data GenIllTypedTermRule ty nTy tm nTm a =
   GenIllTypedTermRecurse (
         (ty nTy -> Gen (ty nTy))
      -> (ty nTy -> Int -> Gen (tm nTy nTm a))
      -> ty nTy
      -> Int
      -> Maybe (Gen (tm nTy nTm a))
      ) -- ^

-- |
fixGenIllTypedTermRule :: (ty nTy -> Gen (ty nTy))
                       -> (ty nTy -> Int -> Gen (tm nTy nTm a))
                       -> ty nTy
                       -> Int
                       -> GenIllTypedTermRule ty nTy tm nTm a
                       -> Maybe (Gen (tm nTy nTm a))
fixGenIllTypedTermRule notType wellTyped ty s (GenIllTypedTermRecurse f) =
  f notType wellTyped ty s

-- |
data GenTermInput ty nTy tm nTm a =
  GenTermInput
    [GenAnyTermRule tm nTy nTm a]
    [ShrAnyTermRule tm nTy nTm a]
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
    _genAnyTerm        :: Gen (tm nTy nTm a)             -- ^
  , _shrAnyTerm        :: tm nTy nTm a -> [tm nTy nTm a]         -- ^
  , _genContainingTerm :: tm nTy nTm a -> ty nTy -> Gen (tm nTy nTm a)       -- ^
  , _shrContainingTerm :: tm nTy nTm a -> [tm nTy nTm a]         -- ^
  , _genWellTypedTerm  :: ty nTy -> Gen (tm nTy nTm a) -- ^
  , _shrWellTypedTerm  :: tm nTy nTm a -> [tm nTy nTm a]         -- ^
  , _genIllTypedTerm   :: ty nTy -> Gen (tm nTy nTm a) -- ^
  , _shrIllTypedTerm   :: tm nTy nTm a -> [tm nTy nTm a]         -- ^
  }

makeClassy ''GenTermOutput

forAllWellTypedTerm :: ( HasGenTermOutput gto ty nTy tm nTm a
                       , HasGenTypeOutput gto ty nTy
                       , Show (tm nTy nTm a)
                       , Testable prop
                       )
                    => gto
                    -> (tm nTy nTm a -> prop)
                    -> Property
forAllWellTypedTerm gto =
    forAllShrink gen shr
  where
    genWellTypedTerm' = view genWellTypedTerm gto
    genAnyType' = view genAnyType gto
    gen = genAnyType' >>= genWellTypedTerm'
    shr = view shrWellTypedTerm gto

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

