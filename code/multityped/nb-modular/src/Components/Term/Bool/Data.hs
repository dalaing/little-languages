{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Term.Bool.Data where

import Control.Lens (preview)
import Control.Lens.TH (makeClassyPrisms)

import Common.Recursion
import Common.Note
import Common.Term.Size

import Components.Type.Bool.Data

data BoolTerm t f =
    TmTrue
  | TmFalse
  | TmIf f f f
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''BoolTerm

instance WithoutNote f => WithoutNote (BoolTerm t f) where
  type Without (BoolTerm t f) = BoolTerm t (Without f)
  stripNote = fmap stripNote

type WithBoolTerm ty tm = (AsBoolTerm tm ty tm, WithBoolType ty)

sizeInput :: WithBoolTerm ty tm
          => SizeInput tm
sizeInput =
  SizeInput
    [MSRecurse sizeBool]

sizeBool :: WithBoolTerm ty tm
         => (tm -> Int)
         -> tm
         -> Maybe Int
sizeBool size' =
  fmap (sizeBool' size') .
  preview _BoolTerm

sizeBool' :: WithBoolTerm ty tm
          => (tm -> Int)
          -> BoolTerm ty tm
          -> Int
sizeBool' _ TmFalse =
  1
sizeBool' _ TmTrue =
  1
sizeBool' size' (TmIf b t f) =
  1 + size' b + size' t + size' f

