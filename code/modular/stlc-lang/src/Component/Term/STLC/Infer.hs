{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Component.Term.STLC.Infer (
    inferInput
  ) where

import Control.Lens (preview, review)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Data.Constraint.Forall (ForallT)

import Component.Type.Error.Unexpected (AsUnexpected)
import Component.Term.Infer (InferRule(..), InferInput(..))

import Component.Type.STLC (AsSTLCType(..), WithSTLCType, HasContext, findInContext, applyArrow, withInContext)
import Component.Term.STLC (AsSTLCTerm(..), AsSTLCVar(..), WithSTLCTerm, app_)
import Component.Type.Error.FreeVar (AsFreeVar)
import Component.Type.Error.NotArrow (AsNotArrow)

inferTmVar :: ( Ord a
              , WithSTLCType ty
              , WithSTLCTerm tm ty
              , HasContext r ty nTy a
              , MonadReader r m
              , AsFreeVar e a
              , MonadError e m
              )
           => tm nTy nTm a
           -> Maybe (m (ty nTy))
inferTmVar =
  fmap findInContext .
  preview _TmVar

inferTmApp :: ( Eq (ty nTy)
              , WithSTLCType ty
              , WithSTLCTerm tm ty
              , AsUnexpected e ty nTy
              , AsNotArrow e ty nTy
              , MonadError e m
              )
           => (ty nTy -> ty nTy)
           -> (tm nTy nTm a -> m (ty nTy))
           -> tm nTy nTm a
           -> Maybe (m (ty nTy))
inferTmApp stripNote infer =
    fmap inferTmApp' .
    preview _TmApp
  where
    inferTmApp' (tm1, tm2) = do
      ty1 <- infer tm1
      ty2 <- infer tm2
      applyArrow stripNote ty1 ty2

inferTmLam :: ( WithSTLCType ty
              , WithSTLCTerm tm ty
              , HasContext r ty nTy String
              , MonadReader r m
              , ForallT Monad tm
              )
           => (ty nTy -> ty nTy)
           -> (tm nTy nTm String -> m (ty nTy))
           -> tm nTy nTm String
           -> Maybe (m (ty nTy))
inferTmLam _ infer =
    fmap inferTmLam' .
    preview _TmLam
  where
    inferTmLam' (n, ty1, s) = do
      ty2 <- withInContext n ty1 $ infer (app_ (review _TmVar n) s)
      return $ review _TyArr (ty1, ty2)

inferInput :: ( Eq (ty nTy)
              , WithSTLCType ty
              , WithSTLCTerm tm ty
              , HasContext r ty nTy String
              , AsUnexpected e ty nTy
              , AsFreeVar e String
              , AsNotArrow e ty nTy
              , ForallT Monad tm
              )
            => InferInput r e ty nTy tm nTm String
inferInput =
  InferInput
    [ InferBase inferTmVar
    , InferRecurse inferTmApp
    , InferRecurse inferTmLam
    ]
