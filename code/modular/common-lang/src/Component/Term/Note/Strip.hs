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
module Component.Term.Note.Strip (
    StripNoteTermRule(..)
  , StripNoteTermInput(..)
  , StripNoteTermOutput(..)
  , HasStripNoteTermOutput(..)
  , mkStripNoteTerm
  , stripNoteTermInput
  ) where

import Data.Maybe (fromJust)
import Data.Foldable (asum)

import Control.Lens (preview, review)
import Control.Lens.TH (makeClassy)

import Component.Term.Parent (AsParentTerm(..))
import Component.Term.Note (NoteTerm(..))

data StripNoteTermRule p =
    StripNoteTermBase (forall n m a. (n -> Maybe m) -> (p n a -> p m a) -> p n a -> Maybe (p m a))
  | StripNoteTermRecurse (forall n m a. (p n a -> p m a) -> p n a -> Maybe (p m a))

fixStripNoteTerm :: (n -> Maybe m)
                 -> (p n a -> p m a)
                 -> p n a
                 -> StripNoteTermRule p
                 -> Maybe (p m a)
fixStripNoteTerm base recurse tm (StripNoteTermBase f) =
  f base recurse tm
fixStripNoteTerm _ recurse tm (StripNoteTermRecurse f) =
  f recurse tm

data StripNoteTermInput p =
  StripNoteTermInput
    [StripNoteTermRule p]

instance Monoid (StripNoteTermInput p) where
  mempty =
    StripNoteTermInput mempty
  mappend (StripNoteTermInput i1) (StripNoteTermInput i2) =
    StripNoteTermInput (mappend i1 i2)

data StripNoteTermOutput p =
  StripNoteTermOutput {
    _mapMaybeNoteTerm :: forall n m a. (n -> Maybe m) -> p n a -> p m a
  , _stripNoteTerm :: forall n m a. p n a -> p m a
  }

makeClassy ''StripNoteTermOutput

mkStripNoteTerm :: forall p. StripNoteTermInput p
                -> StripNoteTermOutput p
mkStripNoteTerm (StripNoteTermInput i) =
  let
    mapMaybeNoteTerm' :: (n -> Maybe m) -> p n b -> p m b
    mapMaybeNoteTerm' f t =
      fromJust .
      asum .
      fmap (fixStripNoteTerm f (mapMaybeNoteTerm' f) t) $
      i
    stripNoteTerm' :: p n a -> p m a
    stripNoteTerm' = mapMaybeNoteTerm' (const Nothing)
  in
    StripNoteTermOutput
      mapMaybeNoteTerm'
      stripNoteTerm'

stripNoteTmNote :: AsParentTerm (NoteTerm tm) tm
                => (n -> Maybe m)
                -> (tm n a -> tm m a)
                -> tm n a
                -> Maybe (tm m a)
stripNoteTmNote f r =
    fmap stripNoteTmNote' .
    preview _ParentTerm
  where
    stripNoteTmNote' (TmNote n tm) =
      case f n of
        Nothing -> r tm
        Just m -> review _ParentTerm (TmNote m (r tm))

stripNoteTermInput :: AsParentTerm (NoteTerm tm) tm => StripNoteTermInput tm
stripNoteTermInput =
  StripNoteTermInput
    [StripNoteTermBase stripNoteTmNote]
