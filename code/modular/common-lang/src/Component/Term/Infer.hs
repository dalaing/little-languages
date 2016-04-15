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
module Component.Term.Infer (
    InferRule(..)
  , InferInput(..)
  , InferOutput
  , HasInferOutput(..)
  , runInfer
  , mkInfer
  ) where

import           Data.Foldable                       (asum)
import           Data.Maybe                          (fromMaybe)

import           Control.Lens.TH                     (makeClassy)
import           Control.Monad.Error.Lens            (throwing)
import           Control.Monad.Except                (Except, runExcept)

import           Common.Type.Error.UnknownType.Class (AsUnknownType (..))

-- |
data InferRule e ty tm =
    InferBase (tm -> Maybe (Except e ty))                           -- ^
  | InferRecurse ((tm -> Except e ty) -> tm -> Maybe (Except e ty)) -- ^

-- |
fixInferRule :: (tm -> Except e ty)
             -> InferRule e ty tm
             -> tm
             -> Maybe (Except e ty)
fixInferRule _ (InferBase f) x =
  f x
fixInferRule step (InferRecurse f) x =
  f step x

-- |
data InferInput e ty tm =
  InferInput [InferRule e ty tm] -- ^

instance Monoid (InferInput e ty tm) where
  mempty =
    InferInput mempty
  mappend (InferInput v1) (InferInput v2) =
    InferInput (mappend v1 v2)

-- |
data InferOutput e ty tm =
  InferOutput {
    _infer      :: tm -> Except e ty           -- ^
  , _inferRules :: [tm -> Maybe (Except e ty)] -- ^
  }

makeClassy ''InferOutput

-- |
runInfer :: Except e ty -- ^
         -> Either e ty -- ^
runInfer =
  runExcept

-- |
mkInfer :: ( AsUnknownType e
           )
        => InferInput e ty tm  -- ^
        -> InferOutput e ty tm -- ^
mkInfer (InferInput i) =
  let
    inferRules' =
      fmap (fixInferRule infer') i
    infer' tm =
      fromMaybe (throwing _UnknownType ()) .
      asum .
      fmap ($ tm) $
      inferRules'
  in
    InferOutput
      infer'
      inferRules'
