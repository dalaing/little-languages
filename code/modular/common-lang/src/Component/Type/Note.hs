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
{-# LANGUAGE TemplateHaskell        #-}
module Component.Type.Note (
    NoteType(..)
  , AsNoteType(..)
  , WithNoteType
  , StripNoteType
  ) where

import Control.Lens (review)
import           Control.Lens.TH (makeClassyPrisms)

data NoteType ty n =
  TyNote n (ty n)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NoteType

type WithNoteType ty n = AsNoteType (ty n) ty n

class StripNoteType t p | t -> p where
  mapMaybeNoteType :: (t m -> p m) -> (n -> Maybe m) -> t n -> p m

  stripNoteType :: (t m -> p m) -> t n -> p m
  stripNoteType back = mapMaybeNoteType back (const Nothing)

instance StripNoteType ty ty => StripNoteType (NoteType ty) ty where
  mapMaybeNoteType back f (TyNote n t) =
    case f n of
      Nothing -> mapMaybeNoteType id f t
      Just m -> back $ TyNote m (mapMaybeNoteType id f t)
