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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Proxy (Proxy)

import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Test.QuickCheck (Gen, oneof, sized, forAllShrink, Property, Testable, (===))
import Data.Constraint

import Component.Type.Gen (GenTypeOutput(..), HasGenTypeOutput(..))
import Extras (Eq3(..), Show3(..))

-- |
data GenAnyTermRule tm =
    GenAnyTermBase (forall nTy nTm. Gen (tm nTy nTm String)) -- ^
  | GenAnyTermRecurse (forall nTy nTm. (Int -> Gen (tm nTy nTm String)) -> Int -> Gen (tm nTy nTm String)) -- ^

-- |
fixGenAnyTermRule :: (Int -> Gen (tm nTy nTm String))
                  -> Int
                  -> GenAnyTermRule tm
                  -> Maybe (Gen (tm nTy nTm String))
fixGenAnyTermRule _ _ (GenAnyTermBase f) =
  Just f
fixGenAnyTermRule _ 0 _ =
  Nothing
fixGenAnyTermRule step s (GenAnyTermRecurse f) =
  Just $ f step s

-- |
data ShrAnyTermRule tm =
    ShrAnyTermBase (forall nTy nTm. tm nTy nTm String -> Maybe [tm nTy nTm String]) -- ^
  | ShrAnyTermRecurse (forall nTy nTm. (tm nTy nTm String -> [tm nTy nTm String]) -> tm nTy nTm String -> Maybe [tm nTy nTm String]) -- ^

-- |
fixShrAnyTermRule :: (tm nTy nTm String -> [tm nTy nTm String])
                  -> ShrAnyTermRule tm
                  -> tm nTy nTm String
                  -> Maybe [tm nTy nTm String]
fixShrAnyTermRule _ (ShrAnyTermBase f) x =
  f x
fixShrAnyTermRule step (ShrAnyTermRecurse f) x =
  f step x

data GenContainingTermRule ty tm =
    GenContainingTermBase ( forall nTy nTm.
         tm nTy nTm String
      -> ty nTy
      -> Maybe (Gen (tm nTy nTm String))
      )
  | GenContainingTermRecurse ( forall nTy nTm.
         (ty nTy -> Int -> Gen (tm nTy nTm String))
      -> (tm nTy nTm String -> ty nTy -> Int -> Gen (tm nTy nTm String))
      -> tm nTy nTm String
      -> ty nTy
      -> Int
      -> Maybe (Gen (tm nTy nTm String))
      ) -- ^

fixGenContainingTermRule :: (ty nTy -> Int -> Gen (tm nTy nTm String))
                         -> (tm nTy nTm String -> ty nTy -> Int -> Gen (tm nTy nTm String))
                         -> tm nTy nTm String
                         -> ty nTy
                         -> Int
                         -> GenContainingTermRule ty tm
                         -> Maybe (Gen (tm nTy nTm String))
fixGenContainingTermRule _ _ tm ty _ (GenContainingTermBase f) =
  f tm ty
fixGenContainingTermRule _ _ tm _ 0 _ =
  Just $ pure tm
fixGenContainingTermRule wellTyped containing tm ty s (GenContainingTermRecurse f) =
  f wellTyped containing tm ty s

data GenWellTypedTermRule ty tm =
    GenWellTypedTermBase ( forall nTy nTm.
         ty nTy
      -> Maybe (Gen (tm nTy nTm String))
    ) -- ^
  | GenWellTypedTermRecurse ( forall nTy nTm.
         (ty nTy -> Int -> Gen (tm nTy nTm String))
      -> ty nTy
      -> Int
      -> Maybe (Gen (tm nTy nTm String))
    ) -- ^

-- |
fixGenWellTypedTermRule :: (ty nTy -> Int -> Gen (tm nTy nTm String))
                        -> ty nTy
                        -> Int
                        -> GenWellTypedTermRule ty tm
                        -> Maybe (Gen (tm nTy nTm String))
fixGenWellTypedTermRule _ ty _ (GenWellTypedTermBase f) =
  f ty
fixGenWellTypedTermRule _ _ 0 _ =
  Nothing
fixGenWellTypedTermRule step ty s (GenWellTypedTermRecurse f) =
  f step ty s

data GenIllTypedTermRule ty tm =
   GenIllTypedTermRecurse ( forall nTy nTm.
         (ty nTy -> Gen (ty nTy))
      -> (ty nTy -> Int -> Gen (tm nTy nTm String))
      -> ty nTy
      -> Int
      -> Maybe (Gen (tm nTy nTm String))
      ) -- ^

-- |
fixGenIllTypedTermRule :: (ty nTy -> Gen (ty nTy))
                       -> (ty nTy -> Int -> Gen (tm nTy nTm String))
                       -> ty nTy
                       -> Int
                       -> GenIllTypedTermRule ty tm
                       -> Maybe (Gen (tm nTy nTm String))
fixGenIllTypedTermRule notType wellTyped ty s (GenIllTypedTermRecurse f) =
  f notType wellTyped ty s

-- |
data GenTermInput ty tm =
  GenTermInput
    [GenAnyTermRule tm]
    [ShrAnyTermRule tm]
    [GenContainingTermRule ty tm]
    [GenWellTypedTermRule ty tm]
    [GenIllTypedTermRule ty tm]

instance Monoid (GenTermInput ty tm) where
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
data GenTermOutput ty tm =
  GenTermOutput {
    _genAnyTerm        :: forall nTy nTm. Gen (tm nTy nTm String)             -- ^
  , _shrAnyTerm        :: forall nTy nTm. tm nTy nTm String -> [tm nTy nTm String]         -- ^
  , _genContainingTerm :: forall nTy nTm. tm nTy nTm String -> ty nTy -> Gen (tm nTy nTm String)       -- ^
  , _shrContainingTerm :: forall nTy nTm. tm nTy nTm String -> [tm nTy nTm String]         -- ^
  , _genWellTypedTerm  :: forall nTy nTm. ty nTy -> Gen (tm nTy nTm String) -- ^
  , _shrWellTypedTerm  :: forall nTy nTm. tm nTy nTm String -> [tm nTy nTm String]         -- ^
  , _genIllTypedTerm   :: forall nTy nTm. ty nTy -> Gen (tm nTy nTm String) -- ^
  , _shrIllTypedTerm   :: forall nTy nTm. tm nTy nTm String -> [tm nTy nTm String]         -- ^
  , _forAllWellTypedTerm :: forall nTy nTm prop. (Show nTy, Show nTm, Testable prop) => Proxy nTy -> Proxy nTm -> (tm nTy nTm String -> prop) -> Property
  , _termEq :: forall nTy nTm a. (Eq nTy, Eq nTm, Eq a, Show nTy, Show nTm, Show a) => Proxy nTy -> Proxy nTm -> tm nTy nTm a -> tm nTy nTm a -> Property
  }

makeClassy ''GenTermOutput

-- |
mkGenTerm :: forall ty tm. (Show3 tm, Eq3 tm)
          => GenTypeOutput ty
          -> GenTermInput ty tm -- ^
          -> GenTermOutput ty tm -- ^
mkGenTerm (GenTypeOutput anyType _ notType _) (GenTermInput ga sa gc gw gi) =
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
    forAllWellTypedTerm' :: forall nTy nTm prop. (Testable prop, Show nTy, Show nTm) => Proxy nTy -> Proxy nTm -> (tm nTy nTm String -> prop) -> Property
    forAllWellTypedTerm' _ _ f =
      let
        gen = anyType >>= (sized . genWellTypedTerm')
        shr = shrWellTypedTerm'
      in
        forAllShrink gen shr f \\ (spanShow3 :: (Show nTy, Show nTm, Show String) :- Show (tm nTy nTm String))
    termEq' :: forall nTy nTm a. (Show nTy, Show nTm, Show a, Eq nTy, Eq nTm, Eq a) => Proxy nTy -> Proxy nTm -> tm nTy nTm a -> tm nTy nTm a -> Property
    termEq' _ _ x y = x === y
      \\ (spanShow3 :: (Show nTy, Show nTm, Show a) :- Show (tm nTy nTm a))
      \\ (spanEq3 :: (Eq nTy, Eq nTm, Eq a) :- Eq (tm nTy nTm a))
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
      forAllWellTypedTerm'
      termEq'

