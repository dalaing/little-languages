{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Component.Type.Note (
    NoteType(..)
  , AsNoteType(..)
  , WithNoteType
  ) where

import Control.Lens (review)
import Control.Lens.Prism (Prism', prism)

import Component.Type.Note.Strip (StripNoteType(..))

data NoteType ty n =
  TyNote n (ty n)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsNoteType s ty | s -> ty where
  _NoteType :: Prism' (s n) (NoteType ty n)
  _TyNote :: Prism' (s n) (n, ty n)
  _TyNote =
    _NoteType .
    prism
      (uncurry TyNote)
      (\x -> case x of TyNote n ty -> Right (n, ty))

instance (AsNoteType ty ty, StripNoteType ty ty) => StripNoteType (NoteType ty) ty where

  mapMaybeNoteType f (TyNote n ty) =
    case f n of
      Nothing -> mapMaybeNoteType f ty
      Just m -> review _TyNote (m, mapMaybeNoteType f ty)

type WithNoteType ty = AsNoteType ty ty

{-
class StripNoteType t p | t -> p where
  mapMaybeNoteType :: (t m -> p m) -> (n -> Maybe m) -> t n -> p m

  stripNoteType :: (t m -> p m) -> t n -> p m
  stripNoteType back = mapMaybeNoteType back (const Nothing)

instance StripNoteType ty ty => StripNoteType (NoteType ty) ty where
  mapMaybeNoteType back f (TyNote n t) =
    case f n of
      Nothing -> mapMaybeNoteType id f t
      Just m -> back $ TyNote m (mapMaybeNoteType id f t)
-}
