{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Term.Component.Eq where

import Control.Lens
import Bound.Class

import Type.Component.Int
import Type.Component.Bool

import Term.Value

data TmEq t f a =
    TmIsEq (f a) (f a)
  | TmNotEq (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TmEq

type WithTmEq t f a = (AsTyInt t, AsTyBool t, AsTmEq (f a) t f a)

instance Bound (TmEq t) where
  TmIsEq x y >>>= g = TmIsEq (x >>= g) (y >>= g)
  TmNotEq x y >>>= g = TmNotEq (x >>= g) (y >>= g)

instance IsValue (TmEq t f a) where
  isValue = const False

-- Type checking and inference

-- Small step semantics

-- Big step semantics

-- Syntax

infix 4 .==.
(.==.) :: WithTmEq t f a => f a -> f a -> f a
(.==.) x y = review _TmIsEq (x, y)

infix 4 ./=.
(./=.) :: WithTmEq t f a => f a -> f a -> f a
(./=.) x y = review _TmNotEq (x, y)

