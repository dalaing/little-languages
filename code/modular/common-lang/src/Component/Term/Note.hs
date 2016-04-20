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
{-# LANGUAGE TemplateHaskell        #-}
module Component.Term.Note (
    NoteTerm(..)
  , AsNoteTerm(..)
  , WithNoteTerm
  ) where

import Data.Monoid ((<>))

import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable
import           Control.Lens.TH (makeClassyPrisms)

import           Bound2           (Bound2 (..))

data NoteTerm tm n a =
  TmNote n (tm n a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NoteTerm

instance Bifunctor tm => Bifunctor (NoteTerm tm) where
  bimap l r (TmNote n tm) = TmNote (l n) (bimap l r tm)

instance Bifoldable tm => Bifoldable (NoteTerm tm) where
  bifoldMap l r (TmNote n tm) = l n <> bifoldMap l r tm

instance Bitraversable tm => Bitraversable (NoteTerm tm) where
  bitraverse l r (TmNote n tm) = TmNote <$> l n <*> bitraverse l r tm

instance Bound2 NoteTerm where
  TmNote n tm >>>>= f = TmNote n (tm >>= f)

type WithNoteTerm tm n a = AsNoteTerm (tm n a) tm n a
