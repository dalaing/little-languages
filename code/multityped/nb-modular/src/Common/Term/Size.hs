{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Term.Size (
    SizeInput(..)
  , HasSizeInput(..)
  , SizeOutput(..)
  , HasSizeOutput(..)
  , mkSize
  ) where

import Control.Lens.TH (makeClassy)

import Common.Recursion

data SizeInput tm =
  SizeInput {
    _sizeSteps :: [MaybeStep tm Int]
  }

makeClassy ''SizeInput

instance Monoid (SizeInput tm) where
  mempty =
    SizeInput mempty
  mappend (SizeInput s1) (SizeInput s2) =
    SizeInput (mappend s1 s2)

data SizeOutput tm =
  SizeOutput {
    _size :: tm -> Int
  }

makeClassy ''SizeOutput

mkSize :: SizeInput tm
       -> SizeOutput tm
mkSize (SizeInput s) =
  SizeOutput (combineMaybeSteps 0 s)
