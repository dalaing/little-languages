{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Term.Component.Int where

import Control.Lens

import Type.Component.Int

import Term.Value

data TmInt (t :: k0) (f :: k1 -> *) (a :: k1) = TmIntLit Int
           deriving (Eq, Ord, Show)

makeClassyPrisms ''TmInt

type WithTmInt t f a = (AsTyInt t, AsTmInt (f a) t f a)

instance IsValue (TmInt t f a) where
  isValue = const True

-- Patterns

pIntLit :: WithTmInt t f a => f a -> Maybe Int
pIntLit = preview _TmIntLit

-- Type checking and inference

-- Small step semantics

-- Big step semantics

-- Syntax

int :: WithTmInt t f a => Int -> f a
int = review _TmIntLit

