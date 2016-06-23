{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Term.Component.LC where

import Control.Lens
import Control.Lens.Extras (is)

import Bound
import Bound.Name

import Type.Component.Arrow

import Term.Value

data TmLc n t f a =
    TmVar a
  | TmLam t (Scope (Name n ()) f a)
  | TmApp (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TmLc

instance IsValue (TmLc n t f a) where
  isValue = is _TmLam

type WithTmLc n t f a = (AsTyArrow t, AsTmLc (f n t a) n t (f n t) a)

-- ABT helpers

lam :: (WithTmLc n t f n, Monad (f n t), Eq n) => t -> n -> f n t n -> f n t n
lam t x e = review _TmLam (t, abstract1Name x e)

-- Type checking and inference

-- Small step semantics

-- Big step semantics

-- Syntax

v :: WithTmLc n t f a => a -> f n t a
v = review _TmVar

infixr 0 .!.
(.!.) :: (WithTmLc n t f n, Monad (f n t), Eq n) => t -> n -> f n t n -> f n t n
(.!.) = lam 

infixl 9 .@.
(.@.) :: (WithTmLc n t f a) => f n t a -> f n t a -> f n t a
(.@.) f x = review _TmApp (f, x)

