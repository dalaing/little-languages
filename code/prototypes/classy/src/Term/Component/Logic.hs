{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Term.Component.Logic where

import Control.Lens

import Bound.Class

import Type.Component.Bool

import Term.Value

data TmLogic t f a =
    TmAnd (f a) (f a)
  | TmOr (f a) (f a)
  | TmNot (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TmLogic

type WithTmLogic t f a = (AsTyBool t, AsTmLogic (f a) t f a)

instance Bound (TmLogic t) where
  TmAnd x y >>>= g = TmAnd (x >>= g) (y >>= g)
  TmOr x y  >>>= g = TmOr (x >>= g) (y >>= g)
  TmNot x   >>>= g = TmNot (x >>= g)

instance IsValue (TmLogic t f a) where
  isValue = const False

-- Type checking and inference

-- Small step semantics

-- Big step semantics

-- Syntax

infixr 3 .&&.
(.&&.) :: WithTmLogic t f a => f a -> f a -> f a
(.&&.) x y = review _TmAnd (x, y)

infixr 2 .||.
(.||.) :: WithTmLogic t f a => f a -> f a -> f a
(.||.) x y = review _TmOr (x, y)

not :: WithTmLogic t f a => f a -> f a
not = review _TmNot

