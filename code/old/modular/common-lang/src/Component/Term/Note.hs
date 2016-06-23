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
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Component.Term.Note (
    NoteTerm(..)
  , AsNoteTerm(..)
  , WithNoteTerm
  ) where

import Data.Monoid ((<>))

import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable
import Control.Lens (review)
import           Control.Lens.Prism (Prism', prism)

import           Bound2           (Bound3 (..))

import Component.Term.Note.Strip (StripNoteTerm(..))

data NoteTerm tm nTy nTm a =
  TmNote nTm (tm nTy nTm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsNoteTerm s tm | s -> tm where
  _NoteTerm :: Prism' (s nTy nTm a) (NoteTerm tm nTy nTm a)
  _TmNote :: Prism' (s nTy nTm a) (nTm, tm nTy nTm a)
  _TmNote =
    _NoteTerm .
    prism
      (uncurry TmNote)
      (\x -> case x of TmNote n tm -> Right (n, tm))

type WithNoteTerm tm = AsNoteTerm tm tm

instance Bifunctor (tm nTy) => Bifunctor (NoteTerm tm nTy) where
  bimap l r (TmNote n tm) = TmNote (l n) (bimap l r tm)

instance Bifoldable (tm nTy) => Bifoldable (NoteTerm tm nTy) where
  bifoldMap l r (TmNote n tm) = l n <> bifoldMap l r tm

instance Bitraversable (tm nTy) => Bitraversable (NoteTerm tm nTy) where
  bitraverse l r (TmNote n tm) = TmNote <$> l n <*> bitraverse l r tm

instance Bound3 NoteTerm where
  TmNote n tm >>>>>= f = TmNote n (tm >>= f)

instance (AsNoteTerm tm tm, StripNoteTerm tm tm) => StripNoteTerm (NoteTerm tm) tm where

  mapMaybeNoteTerm f (TmNote n tm) =
    case f n of
      Nothing -> mapMaybeNoteTerm f tm
      Just m -> review _TmNote (m, mapMaybeNoteTerm f tm)
