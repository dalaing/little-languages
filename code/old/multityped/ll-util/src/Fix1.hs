module Fix1 where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

data Fix1 f a = Fix1 { unFix1 :: f (Fix1 f a) a }

instance Bifunctor f => Functor (Fix1 f) where
  fmap f = Fix1 . bimap (fmap f) f . unFix1

instance Bifoldable f => Foldable (Fix1 f) where
  foldMap f = bifoldMap (foldMap f) f . unFix1

instance Bitraversable f => Traversable (Fix1 f) where
  traverse f = fmap Fix1 . bitraverse (traverse f) f . unFix1
