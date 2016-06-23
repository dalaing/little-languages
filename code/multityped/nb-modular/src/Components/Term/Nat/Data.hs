{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Term.Nat.Data where

import Control.Lens.TH (makeClassyPrisms)
import Control.Lens (preview)

import Common.Recursion
import Common.Note
import Common.Term.Size

import Components.Type.Nat.Data

data NatTerm t f =
    TmZero
  | TmSucc f
  | TmPred f
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NatTerm

instance WithoutNote f => WithoutNote (NatTerm t f) where
  type Without (NatTerm t f) = NatTerm t (Without f)
  stripNote = fmap stripNote

type WithNatTerm ty tm = (AsNatTerm tm ty tm, WithNatType ty)

sizeInput :: WithNatTerm ty tm
          => SizeInput tm
sizeInput =
  SizeInput
    [MSRecurse sizeNat]

sizeNat :: WithNatTerm ty tm
        => (tm -> Int)
        -> tm
        -> Maybe Int
sizeNat size' =
  fmap (sizeNat' size') .
  preview _NatTerm

sizeNat' :: WithNatTerm ty tm
        => (tm -> Int)
        -> NatTerm ty tm
        -> Int
sizeNat' _ TmZero =
  1
sizeNat' size' (TmSucc n) =
  1 + size' n
sizeNat' size' (TmPred n) =
  1 + size' n
