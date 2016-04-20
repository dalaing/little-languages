{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Component.Type.Note.Strip (
    StripNoteTypeRule(..)
  , StripNoteTypeInput(..)
  , StripNoteTypeOutput(..)
  , HasStripNoteTypeOutput(..)
  , mkStripNoteType
  , stripNoteTypeInput
  ) where

import Data.Maybe (fromJust)
import Data.Foldable (asum)

import Control.Lens (preview, review)
import Control.Lens.TH (makeClassy)

import Component.Type.Parent (AsParentType(..))
import Component.Type.Note (NoteType(..))

data StripNoteTypeRule p =
    StripNoteTypeBase (forall n m. (n -> Maybe m) -> (p n -> p m) -> p n -> Maybe (p m))
  | StripNoteTypeRecurse (forall n m. (p n -> p m) -> p n -> Maybe (p m))

fixStripNoteType :: (n -> Maybe m)
                 -> (p n -> p m)
                 -> p n
                 -> StripNoteTypeRule p
                 -> Maybe (p m)
fixStripNoteType base recurse tm (StripNoteTypeBase f) =
  f base recurse tm
fixStripNoteType _ recurse tm (StripNoteTypeRecurse f) =
  f recurse tm

data StripNoteTypeInput p =
  StripNoteTypeInput
    [StripNoteTypeRule p]

instance Monoid (StripNoteTypeInput p) where
  mempty =
    StripNoteTypeInput mempty
  mappend (StripNoteTypeInput i1) (StripNoteTypeInput i2) =
    StripNoteTypeInput (mappend i1 i2)

data StripNoteTypeOutput p =
  StripNoteTypeOutput {
    _mapMaybeNoteType :: forall n m. (n -> Maybe m) -> p n -> p m
  , _stripNoteType :: forall n m. p n -> p m
  }

makeClassy ''StripNoteTypeOutput

mkStripNoteType :: forall p. StripNoteTypeInput p
                -> StripNoteTypeOutput p
mkStripNoteType (StripNoteTypeInput i) =
  let
    mapMaybeNoteType' :: (n -> Maybe m) -> p n -> p m
    mapMaybeNoteType' f t =
      fromJust .
      asum .
      fmap (fixStripNoteType f (mapMaybeNoteType' f) t) $
      i
    stripNoteType' :: p n -> p m
    stripNoteType' = mapMaybeNoteType' (const Nothing)
  in
    StripNoteTypeOutput
      mapMaybeNoteType'
      stripNoteType'

stripNoteTyNote :: AsParentType (NoteType tm) tm
                => (n -> Maybe m)
                -> (tm n -> tm m)
                -> tm n
                -> Maybe (tm m)
stripNoteTyNote f r =
    fmap stripNoteTyNote' .
    preview _ParentType
  where
    stripNoteTyNote' (TyNote n ty) =
      case f n of
        Nothing -> r ty
        Just m -> review _ParentType (TyNote m (r ty))

stripNoteTypeInput :: AsParentType (NoteType ty) ty => StripNoteTypeInput ty
stripNoteTypeInput =
  StripNoteTypeInput
    [StripNoteTypeBase stripNoteTyNote]
