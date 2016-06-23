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
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators        #-}
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
import Data.Constraint

import           Component.Type.Error.UnknownType.Class (AsUnknownType (..))
import Component.Type.Note.Strip (StripNoteType(..))
import Extras (Eq1(..))

newtype InferStack r e n a =
    InferStack { getInfer :: ReaderT (r n String) (Except (e n String)) a}
  deriving (Functor, Applicative, Monad, MonadReader (r n String), MonadError (e n String))

runInfer :: r n String
         -> InferStack r e n a
         -> Either (e n String) a
runInfer r =
  runExcept .
  flip runReaderT r .
  getInfer

-- |
data InferRule r e ty tm =
    InferBase (forall n. Eq n => tm n n String -> Maybe (InferStack r e n (ty n)))                           -- ^
  | InferRecurse (forall n. Eq n => (ty n -> ty n) -> (tm n n String -> InferStack r e n (ty n)) -> tm n n String -> Maybe (InferStack r e n (ty n))) -- ^

-- |
fixInferRule :: Eq n
             => (ty n -> ty n)
             -> (tm n n String -> InferStack r e n (ty n))
             -> InferRule r e ty tm
             -> tm n n String
             -> Maybe (InferStack r e n (ty n))
fixInferRule _ _ (InferBase f) x =
  f x
fixInferRule strip step (InferRecurse f) x =
  f strip step x

-- |
data InferInput r e ty tm =
  InferInput [InferRule r e ty tm] -- ^

instance Monoid (InferInput r e ty tm) where
  mempty =
    InferInput mempty
  mappend (InferInput v1) (InferInput v2) =
    InferInput (mappend v1 v2)

-- |
data InferOutput r e ty tm =
  InferOutput {
    _infer      :: forall n. Eq n => tm n n String -> InferStack r e n (ty n)           -- ^
  , _inferRules :: forall n. Eq n => [tm n n String -> Maybe (InferStack r e n (ty n))] -- ^
  }

makeClassy ''InferOutput

-- |
mkInfer :: forall r e ty tm. (
             Eq1 ty
           , AsUnknownType e
           , StripNoteType ty ty
           )
        => InferInput r e ty tm -- ^
        -> InferOutput r e ty tm -- ^
mkInfer (InferInput i) =
  let
    inferRules' :: forall n. Eq n => [tm n n String -> Maybe (InferStack r e n (ty n))]
    inferRules' =
      fmap (fixInferRule stripNoteType infer' \\ (spanEq1 :: Eq n :- Eq (ty n))) i
    infer' :: forall n. Eq n => tm n n String -> InferStack r e n (ty n)
    infer' tm =
      fromMaybe (throwing _UnknownType ()) .
      asum .
      fmap ($ tm) $
      inferRules'
  in
    InferOutput
      infer'
      inferRules'
