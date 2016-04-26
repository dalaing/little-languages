{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Term.STLC.Infer (
    inferInput
  ) where

import Control.Lens (preview, review)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Bound (instantiate1)

import Component.Type.Error.Unexpected (AsUnexpected)
import Component.Term.Infer (InferRule(..), InferInput(..))

import Component.Type.STLC (WithSTLCType, HasContext, findInContext, applyArrow, withInContext)
import Component.Term.STLC (AsSTLCTerm(..), AsSTLCVar(..), WithSTLCTerm)
import Component.Type.Error.FreeVar (AsFreeVar)
import Component.Type.Error.NotArrow (AsNotArrow)

inferTmVar :: ( Ord a
              , WithSTLCType ty nTy
              , WithSTLCTerm tm ty nTy nTm a
              , HasContext r ty nTy a
              , MonadReader r m
              , AsFreeVar e a
              , MonadError e m
              )
           => tm nTm a
           -> Maybe (m (ty nTy))
inferTmVar =
  fmap findInContext .
  preview _TmVar

inferTmApp :: ( Eq (ty nTy)
              , WithSTLCType ty nTy
              , WithSTLCTerm tm ty nTy nTm a
              , AsUnexpected e ty nTy
              , AsNotArrow e ty nTy
              , MonadError e m
              )
           => (ty nTy -> ty nTy)
           -> (tm nTm a -> m (ty nTy))
           -> tm nTm a
           -> Maybe (m (ty nTy))
inferTmApp stripNote infer =
    fmap inferTmApp' .
    preview _TmApp
  where
    inferTmApp' (tm1, tm2) = do
      ty1 <- infer tm1
      ty2 <- infer tm2
      applyArrow stripNote ty1 ty2

inferTmLam :: ( WithSTLCType ty nTy
              , WithSTLCTerm tm ty nTy nTm String
              , HasContext r ty nTy String
              , MonadReader r m
              , Monad (tm nTm)
              )
           => (ty nTy -> ty nTy)
           -> (tm nTm String -> m (ty nTy))
           -> tm nTm String
           -> Maybe (m (ty nTy))
inferTmLam _ infer =
    fmap inferTmLam' .
    preview _TmLam
  where
    inferTmLam' (n, ty, s) =
      withInContext n ty $ infer (instantiate1 (review _TmVar n) s)

inferInput :: ( Eq (ty nTy)
              , WithSTLCType ty nTy
              , WithSTLCTerm tm ty nTy nTm String
              , HasContext r ty nTy String
              , AsUnexpected e ty nTy
              , AsFreeVar e String
              , AsNotArrow e ty nTy
              , Monad (tm nTm)
              )
            => InferInput r e ty nTy tm nTm String
inferInput =
  InferInput
    [ InferBase inferTmVar
    , InferRecurse inferTmApp
    , InferRecurse inferTmLam
    ]
