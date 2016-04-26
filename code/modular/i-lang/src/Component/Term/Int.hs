{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Component.Term.Int (
    IntTerm(..)
  , AsIntTerm(..)
  , WithIntTerm
  ) where

import Data.Monoid ((<>))

import           Control.Lens.TH      (makeClassyPrisms)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))

import Bound2 (Bound2(..))

-- |
data IntTerm tm n a =
    TmIntLit Int         -- ^
  | TmAdd (tm n a) (tm n a)  -- ^
  | TmSub (tm n a) (tm n a)  -- ^
  | TmMul (tm n a) (tm n a)  -- ^
  | TmExp (tm n a) (tm n a)  -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''IntTerm

instance Bifunctor tm => Bifunctor (IntTerm tm) where
  bimap _ _ (TmIntLit i) = TmIntLit i
  bimap l r (TmAdd tm1 tm2) = TmAdd (bimap l r tm1) (bimap l r tm2)
  bimap l r (TmSub tm1 tm2) = TmSub (bimap l r tm1) (bimap l r tm2)
  bimap l r (TmMul tm1 tm2) = TmMul (bimap l r tm1) (bimap l r tm2)
  bimap l r (TmExp tm1 tm2) = TmExp (bimap l r tm1) (bimap l r tm2)

instance Bifoldable tm => Bifoldable (IntTerm tm) where
  bifoldMap _ _ (TmIntLit _) = mempty
  bifoldMap l r (TmAdd tm1 tm2) = bifoldMap l r tm1 <> bifoldMap l r tm2
  bifoldMap l r (TmSub tm1 tm2) = bifoldMap l r tm1 <> bifoldMap l r tm2
  bifoldMap l r (TmMul tm1 tm2) = bifoldMap l r tm1 <> bifoldMap l r tm2
  bifoldMap l r (TmExp tm1 tm2) = bifoldMap l r tm1 <> bifoldMap l r tm2

instance Bitraversable tm => Bitraversable (IntTerm tm) where
  bitraverse _ _ (TmIntLit i) = pure (TmIntLit i)
  bitraverse l r (TmAdd tm1 tm2) = TmAdd <$> bitraverse l r tm1 <*> bitraverse l r tm2
  bitraverse l r (TmSub tm1 tm2) = TmSub <$> bitraverse l r tm1 <*> bitraverse l r tm2
  bitraverse l r (TmMul tm1 tm2) = TmMul <$> bitraverse l r tm1 <*> bitraverse l r tm2
  bitraverse l r (TmExp tm1 tm2) = TmExp <$> bitraverse l r tm1 <*> bitraverse l r tm2

instance Bound2 IntTerm where
  TmIntLit i    >>>>= _ = TmIntLit i
  TmAdd tm1 tm2 >>>>= f = TmAdd (tm1 >>= f) (tm2 >>= f)
  TmSub tm1 tm2 >>>>= f = TmSub (tm1 >>= f) (tm2 >>= f)
  TmMul tm1 tm2 >>>>= f = TmMul (tm1 >>= f) (tm2 >>= f)
  TmExp tm1 tm2 >>>>= f = TmExp (tm1 >>= f) (tm2 >>= f)

type WithIntTerm tm n a = AsIntTerm (tm n a) tm n a
