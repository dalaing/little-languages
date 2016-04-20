{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving        #-}
module Component.Term.Infer (
    InferRule(..)
  , InferInput(..)
  , InferOutput
  , HasInferOutput(..)
  , InferStack 
  , runInfer
  , mkInfer
  ) where

import           Data.Foldable                       (asum)
import           Data.Maybe                          (fromMaybe)

import           Control.Lens.TH                     (makeClassy)
import           Control.Monad.Error.Lens            (throwing)
import           Control.Monad.Except                (MonadError, Except, runExcept)
import           Control.Monad.Reader                (MonadReader, ReaderT(..))

import           Component.Type.Error.UnknownType.Class (AsUnknownType (..))
import Component.Type.Note.Strip (StripNoteTypeOutput(..))

newtype InferStack r e a =
    InferStack { getInfer :: ReaderT r (Except e) a}
  deriving (Functor, Applicative, Monad, MonadReader r, MonadError e)

runInfer :: r
         -> InferStack r e a
         -> Either e a
runInfer r =
  runExcept .
  flip runReaderT r .
  getInfer

-- |
data InferRule r e ty nTy tm nTm a =
    InferBase (tm nTm a -> Maybe (InferStack r e (ty nTy)))                           -- ^
  | InferRecurse ((ty nTy -> ty nTy) -> (tm nTm a -> InferStack r e (ty nTy)) -> tm nTm a -> Maybe (InferStack r e (ty nTy))) -- ^

-- |
fixInferRule :: (ty nTy -> ty nTy)
             -> (tm nTm a -> InferStack r e (ty nTy))
             -> InferRule r e ty nTy tm nTm a
             -> tm nTm a
             -> Maybe (InferStack r e (ty nTy))
fixInferRule _ _ (InferBase f) x =
  f x
fixInferRule strip step (InferRecurse f) x =
  f strip step x

-- |
data InferInput r e ty nTy tm nTm a =
  InferInput [InferRule r e ty nTy tm nTm a] -- ^

instance Monoid (InferInput r e ty nTy tm nTm a) where
  mempty =
    InferInput mempty
  mappend (InferInput v1) (InferInput v2) =
    InferInput (mappend v1 v2)

-- |
data InferOutput r e ty nTy tm nTm a =
  InferOutput {
    _infer      :: tm nTm a -> InferStack r e (ty nTy)           -- ^
  , _inferRules :: [tm nTm a -> Maybe (InferStack r e (ty nTy))] -- ^
  }

makeClassy ''InferOutput

-- |
mkInfer :: ( AsUnknownType e
           )
        => StripNoteTypeOutput ty
        -> InferInput r e ty nTy tm nTm a  -- ^
        -> InferOutput r e ty nTy tm nTm a -- ^
mkInfer (StripNoteTypeOutput _ stripNote) (InferInput i) =
  let
    inferRules' =
      fmap (fixInferRule stripNote infer') i
    infer' tm =
      fromMaybe (throwing _UnknownType ()) .
      asum .
      fmap ($ tm) $
      inferRules'
  in
    InferOutput
      infer'
      inferRules'
