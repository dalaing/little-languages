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
data GenAnyTypeRule tm =
    GenAnyTypeBase (Gen tm) -- ^
  | GenAnyTypeRecurse ((Int -> Gen tm) -> Int -> Gen tm) -- ^

-- |
fixGenAnyTypeRule :: (Int -> Gen tm)
                  -> Int
                  -> GenAnyTypeRule tm
                  -> Maybe (Gen tm)
fixGenAnyTypeRule _ _ (GenAnyTypeBase f) =
  Just f
fixGenAnyTypeRule _ 0 _ =
  Nothing
fixGenAnyTypeRule step s (GenAnyTypeRecurse f) =
  Just $ f step s

-- |
data ShrAnyTypeRule tm =
    ShrAnyTypeBase (tm -> Maybe [tm]) -- ^
  | ShrAnyTypeRecurse ((tm -> [tm]) -> tm -> Maybe [tm]) -- ^

-- |
fixShrAnyTypeRule :: (tm -> [tm])
                  -> ShrAnyTypeRule tm
                  -> tm
                  -> Maybe [tm]
fixShrAnyTypeRule _ (ShrAnyTypeBase f) x =
  f x
fixShrAnyTypeRule step (ShrAnyTypeRecurse f) x =
  f step x

-- |
data GenTypeInput tm =
  GenTypeInput
    [GenAnyTypeRule tm]
    [ShrAnyTypeRule tm]

instance Monoid (GenTypeInput tm) where
  mempty =
    GenTypeInput mempty mempty
  mappend (GenTypeInput ga1 sa1) (GenTypeInput ga2 sa2) =
    GenTypeInput (mappend ga1 ga2) (mappend sa1 sa2)

-- |
data GenTypeOutput tm =
  GenTypeOutput {
    _genAnyType      :: Gen tm     -- ^
  , _shrAnyType      :: tm -> [tm] -- ^
  }

makeClassy ''GenTypeOutput

-- |
mkGenType :: GenTypeInput tm  -- ^
             -> GenTypeOutput tm -- ^
mkGenType (GenTypeInput ga sa) =
  let
    genAnyType' s =
      oneof .
      mapMaybe (fixGenAnyTypeRule genAnyType' s) $
      ga
    shrAnyType' tm =
      fromMaybe [] .
      asum .
      fmap (\r -> fixShrAnyTypeRule shrAnyType' r tm) $
      sa
  in
    GenTypeOutput
      (sized genAnyType')
      shrAnyType'
