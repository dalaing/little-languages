{-# LANGUAGE FlexibleContexts #-}
module Cofree1 where

import Data.Foldable
import Data.Traversable

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

data Cofree1 f a b = a :< f (Cofree1 f a b) b

extract :: Cofree1 f a b -> a
extract (a :< _) = a

unwrap :: Cofree1 f a b -> f (Cofree1 f a b) b
unwrap (_ :< x) = x

instance Bifunctor f => Bifunctor (Cofree1 f) where
  bimap f g (a :< x) = f a :< bimap (bimap f g) g x

instance Bifunctor f => Functor (Cofree1 f a) where
  fmap = second

instance Bifoldable f => Bifoldable (Cofree1 f) where
  bifoldMap f g (a :< x) = f a `mappend` bifoldMap (bifoldMap f g) g x

instance Bifoldable f => Foldable (Cofree1 f a) where
  foldMap = bifoldMap (const mempty)

instance Bitraversable f => Bitraversable (Cofree1 f) where
  bitraverse f g (a :< x) = (:<) <$> f a <*> bitraverse (bitraverse f g) g x

instance Bitraversable f => Traversable (Cofree1 f a) where
  traverse = bitraverse pure


