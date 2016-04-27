{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Component.Term.Note.Strip (
  StripNoteTerm(..)
{-
    StripNoteTermRule(..)
  , StripNoteTermInput(..)
  , StripNoteTermOutput(..)
  , HasStripNoteTermOutput(..)
  , mkStripNoteTerm
  , stripNoteTermInput
-}
  ) where

class StripNoteTerm x p | x -> p where
  mapMaybeNoteTerm :: (n -> Maybe m) -> x n a -> p m a

  stripNoteTerm :: x n a -> p m a
  stripNoteTerm = mapMaybeNoteTerm (const Nothing)

{-
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
-}
