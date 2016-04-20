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
  , GenValueTermRule(..)
  , ShrValueTermRule(..)
  , GenNonValueTermRule(..)
  , ShrNonValueTermRule(..)
  , GenTermInput(..)
  , GenTermOutput(..)
  , HasGenTermOutput(..)
  , mkGenTerm
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe, mapMaybe)

import Control.Lens.TH (makeClassy)
import Test.QuickCheck (Gen, oneof, sized)

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

-- |
data GenValueTermRule tm n a =
    GenValueTermBase (Gen (tm n a)) -- ^
  | GenValueTermRecurse ((Int -> Gen (tm n a)) -> Int -> Gen (tm n a)) -- ^

-- |
fixGenValueTermRule :: (Int -> Gen (tm n a))
                    -> Int
                    -> GenValueTermRule tm n a
                    -> Maybe (Gen (tm n a))
fixGenValueTermRule _ _ (GenValueTermBase f) =
  Just f
fixGenValueTermRule _ 0 _ =
  Nothing
fixGenValueTermRule step s (GenValueTermRecurse f) =
  Just $ f step s

-- |
data ShrValueTermRule tm n a =
    ShrValueTermBase (tm n a -> Maybe [tm n a]) -- ^
  | ShrValueTermRecurse ((tm n a -> [tm n a]) -> tm n a -> Maybe [tm n a]) -- ^

-- |
fixShrValueTermRule :: (tm n a -> [tm n a])
                    -> ShrValueTermRule tm n a
                    -> tm n a
                    -> Maybe [tm n a]
fixShrValueTermRule _ (ShrValueTermBase f) x =
  f x
fixShrValueTermRule step (ShrValueTermRecurse f) x =
  f step x

-- |
data GenNonValueTermRule tm n a =
  GenNonValueTermRecurse ((Int -> Gen (tm n a)) -> Int -> Gen (tm n a)) -- ^

-- |
fixGenNonValueTermRule :: (Int -> Gen (tm n a))
                    -> Int
                    -> GenNonValueTermRule tm n a
                    -> Maybe (Gen (tm n a))
fixGenNonValueTermRule _ 0 _ =
  Nothing
fixGenNonValueTermRule step s (GenNonValueTermRecurse f) =
  Just $ f step s

-- |
data ShrNonValueTermRule tm n a =
  ShrNonValueTermRecurse ((tm n a -> [tm n a]) -> tm n a -> Maybe [tm n a]) -- ^

-- |
fixShrNonValueTermRule :: (tm n a -> [tm n a])
                    -> ShrNonValueTermRule tm n a
                    -> tm n a
                    -> Maybe [tm n a]
fixShrNonValueTermRule step (ShrNonValueTermRecurse f) x =
  f step x

-- |
data GenTermInput tm n a =
  GenTermInput
    [GenAnyTermRule tm n a]
    [ShrAnyTermRule tm n a]
    [GenValueTermRule tm n a]
    [ShrValueTermRule tm n a]
    [GenNonValueTermRule tm n a]
    [ShrNonValueTermRule tm n a]
    -- [GenContainingTermRule tm]
    -- [ShrContainingTermRule tm]
    -- [GenWellTypedTermRule tm]
    -- [ShrWellTypedTermRule tm]
    -- [GenIllTypedTermRule tm]
    -- [ShrIllTypedTermRule tm]

instance Monoid (GenTermInput tm n a) where
  mempty =
    GenTermInput
      mempty mempty
      mempty mempty
      mempty mempty
  mappend
    (GenTermInput ga1 sa1 gv1 sv1 gnv1 snv1)
    (GenTermInput ga2 sa2 gv2 sv2 gnv2 snv2) =
      GenTermInput
        (mappend ga1 ga2) (mappend sa1 sa2)
        (mappend gv1 gv2) (mappend sv1 sv2)
        (mappend gnv1 gnv2) (mappend snv1 snv2)

-- |
data GenTermOutput tm n a =
  GenTermOutput {
    _genAnyTerm        :: Gen (tm n a)             -- ^
  , _shrAnyTerm        :: tm n a -> [tm n a]         -- ^
  , _genValueTerm      :: Gen (tm n a)             -- ^
  , _shrValueTerm      :: tm n a -> [tm n a]         -- ^
  , _genNonValueTerm   :: Gen (tm n a)             -- ^
  , _shrNonValueTerm   :: tm n a -> [tm n a]         -- ^
  -- , _genContainingTerm :: tm -> Gen tm       -- ^
  -- , _shrContainingTerm :: tm -> [tm]         -- ^
  -- , _genWellTypedTerm  :: Maybe ty -> Gen tm -- ^
  -- , _shrWellTypedTerm  :: tm -> [tm]         -- ^
  -- , _genIllTypedTerm   :: Maybe tm -> Gen tm -- ^
  -- , _shrIllTypedTerm   :: tm -> [tm]         -- ^
  }

makeClassy ''GenTermOutput

-- |
mkGenTerm :: GenTermInput tm n a -- ^
          -> GenTermOutput tm n a -- ^
mkGenTerm (GenTermInput ga sa gv sv gnv snv) =
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
    genValueTerm' s =
      oneof .
      mapMaybe (fixGenValueTermRule genValueTerm' s) $
      gv
    shrValueTerm' tm =
      fromMaybe [] .
      asum .
      fmap (\r -> fixShrValueTermRule shrValueTerm' r tm) $
      sv
    genNonValueTerm' s =
      oneof .
      mapMaybe (fixGenNonValueTermRule genValueTerm' s) $
      gnv
    shrNonValueTerm' tm =
      fromMaybe [] .
      asum .
      fmap (\r -> fixShrNonValueTermRule shrValueTerm' r tm) $
      snv
  in
    GenTermOutput
      (sized genAnyTerm')
      shrAnyTerm'
      (sized genValueTerm')
      shrValueTerm'
      (sized genNonValueTerm')
      shrNonValueTerm'
