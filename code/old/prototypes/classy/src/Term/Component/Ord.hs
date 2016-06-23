{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Term.Component.Ord where

import Control.Lens
import Bound.Class

import Type.Component.Int
import Type.Component.Bool

import Term.Value

import Term.Component.Eq

data TmOrd t f a =
    TmLt (f a) (f a)
  | TmLte (f a) (f a)
  | TmGt (f a) (f a)
  | TmGte (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TmOrd

type WithTmOrd t f a = (AsTyInt t, AsTyBool t, AsTmEq (f a) t f a, AsTmOrd (f a) t f a)

instance Bound (TmOrd t) where
  TmLt x y  >>>= g = TmLt (x >>= g) (y >>= g)
  TmLte x y >>>= g = TmLte (x >>= g) (y >>= g)
  TmGt x y  >>>= g = TmGt (x >>= g) (y >>= g)
  TmGte x y >>>= g = TmGte (x >>= g) (y >>= g)

instance IsValue (TmOrd t f a) where
  isValue = const False

-- Type checking and inference

-- Small step semantics

-- Big step semantics

-- Syntax

infix 4 .<.
(.<.) :: WithTmOrd t f a => f a -> f a -> f a
(.<.) x y = review _TmLt (x, y)

infix 4 .<=.
(.<=.) :: WithTmOrd t f a => f a -> f a -> f a
(.<=.) x y = review _TmLte (x, y)

infix 4 .>.
(.>.) :: WithTmOrd t f a => f a -> f a -> f a
(.>.) x y = review _TmGt (x, y)

infix 4 .>=.
(.>=.) :: WithTmOrd t f a => f a -> f a -> f a
(.>=.) x y = review _TmGte (x, y)

