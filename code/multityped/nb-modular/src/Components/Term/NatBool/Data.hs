{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Term.NatBool.Data where

import Control.Lens (preview)
import Control.Lens.TH (makeClassyPrisms)

import Common.Recursion
import Common.Note
import Common.Term.Size

import Components.Type.Nat.Data
import Components.Type.Bool.Data

data NatBoolTerm t f =
  TmIsZero f
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NatBoolTerm

instance WithoutNote f => WithoutNote (NatBoolTerm t f) where
  type Without (NatBoolTerm t f) = NatBoolTerm t (Without f)
  stripNote = fmap stripNote

type WithNatBoolTerm ty tm =
  ( AsNatBoolTerm tm ty tm
  , WithNatType ty
  , WithBoolType ty
  )

sizeInput :: WithNatBoolTerm ty tm
          => SizeInput tm
sizeInput =
  SizeInput
    [MSRecurse sizeNatBool]

sizeNatBool :: WithNatBoolTerm ty tm
            => (tm -> Int)
            -> tm
            -> Maybe Int
sizeNatBool size' =
  fmap (sizeNatBool' size') .
  preview _NatBoolTerm

sizeNatBool' :: WithNatBoolTerm ty tm
             => (tm -> Int)
             -> NatBoolTerm ty tm
             -> Int
sizeNatBool' size' (TmIsZero t) =
  1 + size' t

