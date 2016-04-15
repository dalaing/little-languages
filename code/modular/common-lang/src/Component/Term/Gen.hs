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
data GenAnyTermRule tm =
    GenAnyTermBase (Gen tm) -- ^
  | GenAnyTermRecurse ((Int -> Gen tm) -> Int -> Gen tm) -- ^

-- |
fixGenAnyTermRule :: (Int -> Gen tm)
                  -> Int
                  -> GenAnyTermRule tm
                  -> Maybe (Gen tm)
fixGenAnyTermRule _ _ (GenAnyTermBase f) =
  Just f
fixGenAnyTermRule _ 0 _ =
  Nothing
fixGenAnyTermRule step s (GenAnyTermRecurse f) =
  Just $ f step s

-- |
data ShrAnyTermRule tm =
    ShrAnyTermBase (tm -> Maybe [tm]) -- ^
  | ShrAnyTermRecurse ((tm -> [tm]) -> tm -> Maybe [tm]) -- ^

-- |
fixShrAnyTermRule :: (tm -> [tm])
                  -> ShrAnyTermRule tm
                  -> tm
                  -> Maybe [tm]
fixShrAnyTermRule _ (ShrAnyTermBase f) x =
  f x
fixShrAnyTermRule step (ShrAnyTermRecurse f) x =
  f step x

-- |
data GenTermInput tm =
  GenTermInput
    [GenAnyTermRule tm]
    [ShrAnyTermRule tm]
    -- [GenContainingTermRule tm]
    -- [ShrContainingTermRule tm]
    -- [GenValueTermRule tm]
    -- [ShrValueTermRule tm]
    -- [GenNonValueTermRule tm]
    -- [ShrNonValueTermRule tm]
    -- [GenWellTypedTermRule tm]
    -- [ShrWellTypedTermRule tm]
    -- [GenIllTypedTermRule tm]
    -- [ShrIllTypedTermRule tm]

instance Monoid (GenTermInput tm) where
  mempty =
    GenTermInput mempty mempty
  mappend (GenTermInput ga1 sa1) (GenTermInput ga2 sa2) =
    GenTermInput (mappend ga1 ga2) (mappend sa1 sa2)

-- |
data GenTermOutput tm =
  GenTermOutput {
    _genAnyTerm        :: Gen tm             -- ^
  , _shrAnyTerm        :: tm -> [tm]         -- ^
  -- , _genContainingTerm :: tm -> Gen tm       -- ^
  -- , _shrContainingTerm :: tm -> [tm]         -- ^
  -- , _genValueTerm      :: Gen tm             -- ^
  -- , _shrValueTerm      :: tm -> [tm]         -- ^
  -- , _genNonValueTerm   :: Gen tm             -- ^
  -- , _shrNonValueTerm   :: tm -> [tm]         -- ^
  -- , _genWellTypedTerm  :: Maybe ty -> Gen tm -- ^
  -- , _shrWellTypedTerm  :: tm -> [tm]         -- ^
  -- , _genIllTypedTerm   :: Maybe tm -> Gen tm -- ^
  -- , _shrIllTypedTerm   :: tm -> [tm]         -- ^
  }

makeClassy ''GenTermOutput

-- |
mkGenTerm :: GenTermInput tm  -- ^
          -> GenTermOutput tm -- ^
mkGenTerm (GenTermInput ga sa) =
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
  in
    GenTermOutput
      (sized genAnyTerm')
      shrAnyTerm'
