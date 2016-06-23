{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Term.Component.Bool where

import Control.Lens

import Type.Component.Bool

import Term.Value

data TmBool (t :: k0) (f :: k1 -> *) (a :: k1) = TmBoolLit Bool
           deriving (Eq, Ord, Show)

makeClassyPrisms ''TmBool

type WithTmBool t f a = (AsTyBool t, AsTmBool (f a) t f a)

instance IsValue (TmBool t f a) where
  isValue = const True

-- Type checking and inference

-- Small step semantics

-- Big step semantics

-- Syntax

bool :: WithTmBool t f a => Bool -> f a
bool = review _TmBoolLit

