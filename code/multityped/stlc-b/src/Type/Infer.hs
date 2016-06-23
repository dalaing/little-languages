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
           => (b -> m (Type l))
           -> Prism' (Term n l a) b
           -> Term n l a
           -> Maybe (m (Type l))
inferPart f p = fmap (toTyLoc f) . withLoc p

inferTmFalse :: Monad m
             => Term n l a
             -> Maybe (m (Type l))
inferTmFalse = inferPart inferTmFalse' _TmFalse
  where
    inferTmFalse' = const . return $ TyBool

inferTmTrue :: Monad m
            => Term n l a
            -> Maybe (m (Type l))
inferTmTrue = inferPart inferTmTrue' _TmTrue
  where
    inferTmTrue' = const . return $ TyBool

inferTmIf :: ( AsUnexpected e l
             , AsExpectedEq e l
             , MonadError e m)
          => (Term n l a -> m (Type l))
          -> Term n l a
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

inferTmLoc :: Monad m
             => (Term n l a -> m (Type l))
             -> Term n l a
             -> Maybe (m (Type l))
inferTmLoc inf = inferPart inferTmLoc' _TmLoc
  where
    inferTmLoc' (_, tm) = inf tm

-- infer :: (AsUnexpected e l, AsExpectedEq e l, AsUnknownType e l, MonadError e m)
infer :: (MonadError (TypeError l) m)
      => Term n l a
      -> m (Type l)
infer t =
  let
    (l, _) = view _WithLoc t
  in
    fromMaybe (throwing _UnknownType l) .
    asum .
    map ($ t) $ [
      inferTmFalse
    , inferTmTrue
    , inferTmIf infer
    , inferTmLoc infer
    ]
