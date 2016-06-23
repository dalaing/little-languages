{-# LANGUAGE FlexibleContexts #-}
module Type.Infer where

import Control.Lens (preview, review)

import Control.Monad.Except
import Control.Monad.Error.Lens
import Control.Monad.Reader

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Bound

import qualified Data.Map as M

import Term

import Type
import Type.Error

data Context l a = Context { _varMap :: M.Map a (TypeInfo l)}
               deriving (Eq, Ord, Show)

ctxLookup :: Ord a
          => a
          -> Context l a
          -> Maybe (TypeInfo l)
ctxLookup k (Context m) = M.lookup k m

ctxInsert :: Ord a
          => a
          -> TypeInfo l
          -> Context l a
          -> Context l a
ctxInsert k v (Context m) = Context (M.insert k v m)

inferTmVar :: (Ord a, MonadReader (Context l a) m, AsUnknownVar e l a, MonadError e m)
            => Loc l n a
            -> Maybe (m (TypeInfo l))
inferTmVar (Loc l t) = fmap inferTmVar' . preview _TmVar $ t
  where
    inferTmVar' x = do
      ty <- asks $ ctxLookup x
      case ty of
        Nothing -> throwing _UnknownVar (l, x)
        Just ti -> return ti

inferTmApp :: (AsUnexpected e l a, AsExpectedArr e l a, MonadError e m)
           => (Loc l n a -> m (TypeInfo l))
           -> Loc l n a
           -> Maybe (m (TypeInfo l))
inferTmApp inf (Loc l t) = fmap inferTmApp' . preview _TmApp $ t
  where
    inferTmApp' (f, x) = do
      tyF <- inf f
      (t1, t2) <- expectArr tyF
      tyX <- inf x
      expect tyX (tiType t1)
      return t2

inferTmLam :: (Monoid l, Ord a, MonadReader (Context l a) m)
           => (Loc l a a -> m (TypeInfo l))
           -> Loc l a a
           -> Maybe (m (TypeInfo l))
inferTmLam inf (Loc l tm) = fmap inferTmLam' . preview _TmLam $ tm
  where
    inferTmLam' (n, t, e) = do
      -- TODO grab the location of the binding, use here
      -- locations are all messed up here
      TypeInfo r _ <- local (ctxInsert n (TypeInfo t l)) (inf (instantiate1 (Loc mempty (review _TmVar n)) e))
      return $ TypeInfo (review _TyArr (t, r)) l

inferTmFalse :: Monad m
             => Loc l n a
             -> Maybe (m (TypeInfo l))
inferTmFalse (Loc l t) = fmap inferTmFalse' . preview _TmFalse $ t
  where
    inferTmFalse' = const . return $ TypeInfo (review _TyBool ()) l

inferTmTrue :: Monad m
            => Loc l n a
            -> Maybe (m (TypeInfo l))
inferTmTrue (Loc l t) = fmap inferTmTrue' . preview _TmTrue $ t
  where
    inferTmTrue' = const . return $ TypeInfo (review _TyBool ()) l

inferTmIf :: (AsUnexpected e l a, AsExpectedEq e l a, MonadError e m)
          => (Loc l n a -> m (TypeInfo l))
          -> Loc l n a
          -> Maybe (m (TypeInfo l))
inferTmIf inf (Loc l t) = fmap inferTmIf' . preview _TmIf $ t
  where
    inferTmIf' (tm1, tm2, tm3) = do
      ty1 <- inf tm1
      expect ty1 (review _TyBool ())
      ty2 <- inf tm2
      ty3 <- inf tm3
      expectEq ty2 ty3
      return $ TypeInfo (tiType ty2) l

inferTmLoc :: Monad m
             => (Loc l n a -> m (TypeInfo l))
             -> Loc l n a
             -> Maybe (m (TypeInfo l))
inferTmLoc inf (Loc _ t) = fmap inferTmLoc' . preview _TmLoc $ t
  where
    inferTmLoc' (_, tm) = inf tm

infer :: (Monoid l, Ord a, MonadReader (Context l a) m, MonadError (TypeError l a) m)
      => Loc l a a
      -> m (TypeInfo l)
infer loc@(Loc l _) =
  fromMaybe (throwing _UnknownType l) .
  asum .
  map ($ loc) $ [
    inferTmVar
  , inferTmApp infer
  , inferTmLam infer
  , inferTmFalse
  , inferTmTrue
  , inferTmIf infer
  , inferTmLoc infer
  ]
