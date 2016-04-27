{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Int.Infer (
    inferInput
  ) where

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)

import Component.Type.Error.Unexpected (AsUnexpected(..), mkExpect)
import Component.Term.Infer (InferRule(..), InferInput(..))

import Component.Type.Int (AsIntType(..), WithIntType)
import Component.Term.Int (AsIntTerm(..), WithIntTerm)

-- |
inferTmInt :: ( Monad m
               , WithIntTerm tm
               , WithIntType ty
               )
            => tm nTm a          -- ^
            -> Maybe (m (ty nTy)) -- ^
inferTmInt =
  fmap (const . return $ review _TyInt ()) .
  preview _TmIntLit

-- |
inferTmAdd :: ( Eq (ty nTy)
              , AsUnexpected e ty nTy
              , MonadError e m
              , WithIntTerm tm
              , WithIntType ty
              )
           => (ty nTy -> ty nTy)
           -> (tm nTm a -> m (ty nTy))       -- ^
           -> tm nTm a                -- ^
           -> Maybe (m (ty nTy))       -- ^
inferTmAdd stripNote infer =
    fmap inferTmAdd' .
    preview _TmAdd
  where
    expect = mkExpect stripNote
    inferTmAdd' (tm1, tm2) = do
      ty1 <- infer tm1
      expect ty1 (review _TyInt ())
      ty2 <- infer tm2
      expect ty2 (review _TyInt ())
      return $ review _TyInt ()

-- |
inferTmSub :: ( Eq (ty nTy)
              , AsUnexpected e ty nTy
              , MonadError e m
              , WithIntTerm tm
              , WithIntType ty
              )
           => (ty nTy -> ty nTy)
           -> (tm nTm a -> m (ty nTy))       -- ^
           -> tm nTm a                -- ^
           -> Maybe (m (ty nTy))       -- ^
inferTmSub stripNote infer =
    fmap inferTmSub' .
    preview _TmSub
  where
    expect = mkExpect stripNote
    inferTmSub' (tm1, tm2) = do
      ty1 <- infer tm1
      expect ty1 (review _TyInt ())
      ty2 <- infer tm2
      expect ty2 (review _TyInt ())
      return $ review _TyInt ()

-- |
inferTmMul :: ( Eq (ty nTy)
              , AsUnexpected e ty nTy
              , MonadError e m
              , WithIntTerm tm
              , WithIntType ty
              )
           => (ty nTy -> ty nTy)
           -> (tm nTm a -> m (ty nTy))       -- ^
           -> tm nTm a                -- ^
           -> Maybe (m (ty nTy))       -- ^
inferTmMul stripNote infer =
    fmap inferTmMul' .
    preview _TmMul
  where
    expect = mkExpect stripNote
    inferTmMul' (tm1, tm2) = do
      ty1 <- infer tm1
      expect ty1 (review _TyInt ())
      ty2 <- infer tm2
      expect ty2 (review _TyInt ())
      return $ review _TyInt ()

-- |
inferTmExp :: ( Eq (ty nTy)
              , AsUnexpected e ty nTy
              , MonadError e m
              , WithIntTerm tm
              , WithIntType ty
              )
           => (ty nTy -> ty nTy)
           -> (tm nTm a -> m (ty nTy))       -- ^
           -> tm nTm a                -- ^
           -> Maybe (m (ty nTy))       -- ^
inferTmExp stripNote infer =
    fmap inferTmExp' .
    preview _TmExp
  where
    expect = mkExpect stripNote
    inferTmExp' (tm1, tm2) = do
      ty1 <- infer tm1
      expect ty1 (review _TyInt ())
      ty2 <- infer tm2
      expect ty2 (review _TyInt ())
      return $ review _TyInt ()

-- |
inferInput :: ( Eq (ty nTy)
              , AsUnexpected e ty nTy
              , WithIntTerm tm
              , WithIntType ty
              )
           => InferInput r e ty nTy tm nTm a
inferInput =
  InferInput
    [ InferBase inferTmInt
    , InferRecurse inferTmAdd
    , InferRecurse inferTmSub
    , InferRecurse inferTmMul
    , InferRecurse inferTmExp
    ]
