{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Type.Infer where

import Control.Lens (view)
import Control.Lens.Prism (Prism')
import Control.Monad.Except
import Control.Monad.Error.Lens

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Loc
import Term

import Type
import Type.Error

toTyLoc :: Functor m
        => (a -> m (Type l))
        -> (Maybe l, a)
        -> m (Type l)
toTyLoc f (Just l, x) =
  fmap (TyLoc l) (f x)
toTyLoc f (Nothing, x) =
  f x

inferPart :: Functor m
           => (a -> m (Type l))
           -> Prism' (Term l) a
           -> Term l
           -> Maybe (m (Type l))
inferPart f p = fmap (toTyLoc f) . withLoc p

inferTmZero :: Monad m
             => Term l
             -> Maybe (m (Type l))
inferTmZero = inferPart inferTmZero' _TmZero
  where
    inferTmZero' = const . return $ TyNat

inferTmSucc :: (AsUnexpected e l, MonadError e m)
             => (Term l -> m (Type l))
             -> Term l
             -> Maybe (m (Type l))
inferTmSucc inf = inferPart inferTmSucc' _TmSucc
  where
    inferTmSucc' tm = do
      ty <- inf tm
      expect ty TyNat
      return TyNat

inferTmPred :: (AsUnexpected e l, MonadError e m)
             => (Term l -> m (Type l))
             -> Term l
             -> Maybe (m (Type l))
inferTmPred inf = inferPart inferTmPred' _TmPred
  where
    inferTmPred' tm = do
      ty <- inf tm
      expect ty TyNat
      return TyNat

inferTmFalse :: Monad m
             => Term l
             -> Maybe (m (Type l))
inferTmFalse = inferPart inferTmFalse' _TmFalse
  where
    inferTmFalse' = const . return $ TyBool

inferTmTrue :: Monad m
            => Term l
            -> Maybe (m (Type l))
inferTmTrue = inferPart inferTmTrue' _TmTrue
  where
    inferTmTrue' = const . return $ TyBool

inferTmIf :: (AsUnexpected e l, AsExpectedEq e l, MonadError e m)
          => (Term l -> m (Type l))
          -> Term l
          -> Maybe (m (Type l))
inferTmIf inf = inferPart inferTmIf' _TmIf
  where
    inferTmIf' (tm1, tm2, tm3) = do
      ty1 <- inf tm1
      expect ty1 TyBool
      ty2 <- inf tm2
      ty3 <- inf tm3
      expectEq ty2 ty3
      -- TODO strip ty2 of info, but in a way that allows rebuilding
      -- only works if we don't use toTyLoc
      return ty2

inferTmIsZero :: (AsUnexpected e l, MonadError e m)
              => (Term l -> m (Type l))
              -> Term l
              -> Maybe (m (Type l))
inferTmIsZero inf = inferPart inferTmIsZero' _TmIsZero
  where
    inferTmIsZero' tm = do
      ty <- inf tm
      expect ty TyNat
      return TyBool

inferTmLoc :: Monad m
             => (Term l -> m (Type l))
             -> Term l
             -> Maybe (m (Type l))
inferTmLoc inf = inferPart inferTmLoc' _TmLoc
  where
    inferTmLoc' (_, tm) = inf tm

-- infer :: (AsUnexpected e l, AsExpectedEq e l, AsUnknownType e l, MonadError e m)
infer :: (MonadError (TypeError l) m)
      => Term l
      -> m (Type l)
infer t =
  let
    (l, _) = view _WithLoc t
  in
    fromMaybe (throwing _UnknownType l) .
    asum .
    map ($ t) $ [
      inferTmZero
    , inferTmSucc infer
    , inferTmPred infer
    , inferTmFalse
    , inferTmTrue
    , inferTmIf infer
    , inferTmIsZero infer
    , inferTmLoc infer
    ]
