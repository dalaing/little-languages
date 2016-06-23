{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Term.Note.Data where

import Data.Bifunctor

import Control.Lens (preview)
import Control.Lens.TH (makeClassyPrisms)

import Common.Recursion
import Common.Note
import Common.Term.Size

data NoteTerm t n f =
  TmNoted n f
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NoteTerm

instance Bifunctor (NoteTerm t) where
  bimap g h (TmNoted n f) = TmNoted (g n) (h f)

instance WithoutNote f => WithoutNote (NoteTerm t n f) where
  type Without (NoteTerm t n f) = Without f
  stripNote (TmNoted _ f) = stripNote f

type WithNoteTerm n ty tm = AsNoteTerm tm ty n tm

sizeInput :: WithNoteTerm n ty tm
          => SizeInput tm
sizeInput =
  SizeInput
    [MSRecurse sizeNote]

sizeNote :: WithNoteTerm n ty tm
         => (tm -> Int)
         -> tm
         -> Maybe Int
sizeNote size' =
  fmap (sizeNote' size') .
  preview _NoteTerm

sizeNote' :: WithNoteTerm n ty tm
          => (tm -> Int)
          -> NoteTerm ty n tm
          -> Int
sizeNote' size' (TmNoted _ t) =
  1 + size' t

