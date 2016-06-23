{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Common.Term.Infer (
    InferInput(..)
  , HasInferInput(..)
  , InferOutput(..)
  , HasInferOutput(..)
  , mkInfer
  , InferStack(..)
  ) where

import Control.Applicative (Alternative)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError, Except)
import Control.Lens.TH (makeClassy)

import Common.Recursion
import Common.Type.Error

-- TODO replace with non-empty list
-- possibly replace with Validation / AccValidation?
newtype InferStack e a =
  InferStack {
    runInfer :: Except e a
  } deriving (Functor, Applicative, Alternative, Monad, MonadError e)

data InferInput e ty tm =
  InferInput {
    _inferSteps :: [MaybeStep tm (InferStack e ty)]
  }

makeClassy ''InferInput

-- TODO make into a semigroup?
instance Monoid (InferInput e ty tm) where
  mempty =
    InferInput []
  mappend (InferInput i1) (InferInput i2) =
    InferInput (mappend i1 i2)

data InferOutput e ty tm =
  InferOutput {
    _inferRules :: [tm -> Maybe (InferStack e ty)]
  , _infer :: tm -> InferStack e ty
  }

makeClassy ''InferOutput

mkInfer :: ( AsUnknownType e n
           )
        => InferInput e ty tm
        -> InferOutput e ty tm
mkInfer (InferInput is) =
  let
    err = throwing _TeUnknownType Nothing
  in
    InferOutput
      (mkMaybeSteps err is)
      (combineMaybeSteps err is)
