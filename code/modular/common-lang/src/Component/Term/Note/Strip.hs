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
{-# LANGUAGE KindSignatures        #-}
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

data StripNoteTermRule (ty :: * -> *) nTy tm =
    StripNoteTermBase (forall nTm mTm a. (nTm -> Maybe mTm) -> (tm nTm a -> tm mTm a) -> tm nTm a -> Maybe (tm mTm a))
  | StripNoteTermRecurse (forall nTm mTm a. (tm nTm a -> tm mTm a) -> tm nTm a -> Maybe (tm mTm a))

fixStripNoteTerm :: (nTm -> Maybe mTm)
                 -> (tm nTm a -> tm mTm a)
                 -> tm nTm a
                 -> StripNoteTermRule ty nTy tm
                 -> Maybe (tm mTm a)
fixStripNoteTerm base recurse tm (StripNoteTermBase f) =
  f base recurse tm
fixStripNoteTerm _ recurse tm (StripNoteTermRecurse f) =
  f recurse tm

data StripNoteTermInput (ty :: * -> *) nTy tm =
  StripNoteTermInput
    [StripNoteTermRule ty nTy tm]

instance Monoid (StripNoteTermInput ty nTy tm) where
  mempty =
    StripNoteTermInput mempty
  mappend (StripNoteTermInput i1) (StripNoteTermInput i2) =
    StripNoteTermInput (mappend i1 i2)

data StripNoteTermOutput tm =
  StripNoteTermOutput {
    _mapMaybeNoteTerm :: forall n m a. (n -> Maybe m) -> tm n a -> tm m a
  , _stripNoteTerm :: forall n m a. tm n a -> tm m a
  }

makeClassy ''StripNoteTermOutput

mkStripNoteTerm :: forall ty nTy tm. StripNoteTermInput ty nTy tm
                -> StripNoteTermOutput tm
mkStripNoteTerm (StripNoteTermInput i) =
  let
    mapMaybeNoteTerm' :: (nTm -> Maybe mTm) -> tm nTm a -> tm mTm a
    mapMaybeNoteTerm' f t =
      fromJust .
      asum .
      fmap (fixStripNoteTerm f (mapMaybeNoteTerm' f) t) $
      i
    stripNoteTerm' :: tm nTm a -> tm mTm a
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

stripNoteTermInput :: AsParentTerm (NoteTerm tm) tm => StripNoteTermInput ty nTy tm
stripNoteTermInput =
  StripNoteTermInput
    [StripNoteTermBase stripNoteTmNote]
