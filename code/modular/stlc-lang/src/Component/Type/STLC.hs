{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ConstraintKinds        #-}
module Component.Type.STLC (
    STLCType(..)
  , AsSTLCType(..)
  , WithSTLCType
  , Context(..)
  , HasContext(..)
  , findInContext
  , applyArrow
  , withInContext
  ) where

import Control.Lens (Lens', over, view, preview, review)
import Control.Lens.Prism (Prism', prism)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)
import qualified Data.Map as M (Map, lookup, insert)
import Data.Constraint (Dict(..), (:-)(..))

import Component.Type.Error.Unexpected (AsUnexpected, mkExpect)
import Component.Type.Note.Strip (StripNoteType(..))
import Extras (Eq1, Monoid2(..))

import Component.Type.Error.FreeVar (AsFreeVar(..))
import Component.Type.Error.NotArrow (AsNotArrow(..))

-- |
data STLCType ty n =
  TyArr (ty n) (ty n)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsSTLCType s ty | s -> ty where
  _STLCType :: Prism' (s n) (STLCType ty n)
  _TyArr :: Prism' (s n) (ty n, ty n)
  _TyArr =
    _STLCType .
    prism
      (uncurry TyArr)
      (\x -> case x of TyArr y z -> Right (y, z))

instance (AsSTLCType ty ty, StripNoteType ty ty) => StripNoteType (STLCType ty) ty where
  mapMaybeNoteType f (TyArr x y) = review _TyArr (mapMaybeNoteType f x, mapMaybeNoteType f y)

type WithSTLCType ty = AsSTLCType ty ty

data Context ty n a =
  Context (M.Map a (ty n))

instance Ord a => Monoid (Context ty n a) where
  mempty = Context mempty
  mappend (Context c1) (Context c2) =
    Context (mappend c1 c2)

instance Monoid2 (Context ty) where
  spanMonoid2 = Sub Dict

find :: Ord a
     => a
     -> Context ty n a
     -> Maybe (ty n)
find a (Context m) =
  M.lookup a m

insert :: Ord a
       => a
       -> ty n
       -> Context ty n a
       -> Context ty n a
insert n ty (Context m) =
  Context (M.insert n ty m)

class HasContext r ty | r -> ty where
  context :: Lens' (r n a) (Context ty n a)

instance HasContext (Context ty) ty where
  context = id

findInContext :: ( Ord a
                 , HasContext r ty
                 , MonadReader (r n a) m
                 , AsFreeVar e
                 , MonadError (e n a) m
                 )
              => a
              -> m (ty n)
findInContext a = do
  mty <- find a <$> view context
  case mty of
    Nothing -> throwing _FreeVar a
    Just ty -> return ty

applyArrow :: ( Eq1 ty
              , Eq n
              , WithSTLCType ty
              , AsUnexpected e ty
              , AsNotArrow e ty
              , MonadError (e n String) m
              )
           => (ty n -> ty n)
           -> ty n
           -> ty n
           -> m (ty n)
applyArrow stripNote ty1 ty2 =
    maybe notArrow applyArrow' .
    preview _TyArr .
    stripNote $
    ty1
  where
    expect = mkExpect stripNote
    notArrow = throwing _NotArrow (ty1, ty2)
    applyArrow' (aFrom, aTo) = do
      expect aFrom ty2
      return aTo

addToContext :: Ord a
             => a
             -> ty n
             -> Context ty n a
             -> Context ty n a
addToContext n ty =
  insert n ty

withInContext :: ( Ord a
                 , HasContext r ty
                 , MonadReader (r n a) m
                 )
              => a
              -> ty n
              -> m (ty n)
              -> m (ty n)
withInContext n ty m =
  local (over context $ addToContext n ty) m
