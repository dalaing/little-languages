{-# LANGUAGE FlexibleContexts #-}
module Type.Infer where

import Control.Lens (preview)
import Control.Monad.Except
import Control.Monad.Error.Lens
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)


import Term

import Type
import Type.Error

inferTmZero :: Monad m
             => Term
             -> Maybe (m Type)
inferTmZero = fmap inferTmZero' . preview _TmZero
  where
    inferTmZero' = const $ return TyNat

inferTmSucc :: (AsUnexpected e, MonadError e m)
             => (Term -> m Type)
             -> Term
             -> Maybe (m Type)
inferTmSucc inf = fmap inferTmSucc' . preview _TmSucc
  where
    inferTmSucc' tm = do
      ty <- inf tm
      expect ty TyNat
      return TyNat

inferTmPred :: (AsUnexpected e, MonadError e m)
             => (Term -> m Type)
             -> Term
             -> Maybe (m Type)
inferTmPred inf = fmap inferTmPred' . preview _TmPred
  where
    inferTmPred' tm = do
      ty <- inf tm
      expect ty TyNat
      return TyNat

inferTmFalse :: Monad m
             => Term
             -> Maybe (m Type)
inferTmFalse = fmap inferTmFalse' . preview _TmFalse
  where
    inferTmFalse' = const $ return TyBool

inferTmTrue :: Monad m
             => Term
             -> Maybe (m Type)
inferTmTrue = fmap inferTmTrue' . preview _TmTrue
  where
    inferTmTrue' = const $ return TyBool

inferTmIf :: (AsUnexpected e, AsExpectedEq e, MonadError e m)
             => (Term -> m Type)
             -> Term
             -> Maybe (m Type)
inferTmIf inf = fmap inferTmIf' . preview _TmIf
  where
    inferTmIf' (tm1, tm2, tm3) = do
      ty1 <- inf tm1
      expect ty1 TyBool
      ty2 <- inf tm2
      ty3 <- inf tm3
      expectEq ty2 ty3
      return ty2

inferTmIsZero :: (AsUnexpected e, MonadError e m)
             => (Term -> m Type)
             -> Term
             -> Maybe (m Type)
inferTmIsZero inf = fmap inferTmIsZero' . preview _TmIsZero
  where
    inferTmIsZero' tm = do
      ty <- inf tm
      expect ty TyNat
      return TyBool

-- infer :: (AsUnexpected e, AsExpectedEq e, AsUnknownType e, MonadError e m)
infer :: MonadError TypeError m
      => Term
      -> m Type
infer t =
  fromMaybe (throwing _UnknownType ()) .
  asum .
  map ($ t) $ [
    inferTmZero
  , inferTmSucc infer
  , inferTmPred infer
  , inferTmFalse
  , inferTmTrue
  , inferTmIf infer
  , inferTmIsZero infer
  ]
