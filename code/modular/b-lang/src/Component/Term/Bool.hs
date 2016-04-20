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
module Component.Term.Bool (
    BoolTerm(..)
  , AsBoolTerm(..)
  , WithBoolTerm
  ) where

import Data.Monoid ((<>))

import           Control.Lens.TH (makeClassyPrisms)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))

import Bound2 (Bound2(..))

-- |
data BoolTerm tm n a =
    TmFalse       -- ^
  | TmTrue        -- ^
  | TmIf (tm n a) (tm n a) (tm n a) -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''BoolTerm

instance Bifunctor tm => Bifunctor (BoolTerm tm) where
  bimap _ _ TmFalse = TmFalse
  bimap _ _ TmTrue = TmTrue
  bimap l r (TmIf tm1 tm2 tm3) = TmIf (bimap l r tm1) (bimap l r tm2) (bimap l r tm3)

instance Bifoldable tm => Bifoldable (BoolTerm tm) where
  bifoldMap _ _ TmFalse = mempty
  bifoldMap _ _ TmTrue = mempty
  bifoldMap l r (TmIf tm1 tm2 tm3) = bifoldMap l r tm1 <> bifoldMap l r tm2 <> bifoldMap l r tm3

instance Bitraversable tm => Bitraversable (BoolTerm tm) where
  bitraverse _ _ TmFalse = pure TmFalse
  bitraverse _ _ TmTrue = pure TmTrue
  bitraverse l r (TmIf tm1 tm2 tm3) = TmIf <$> bitraverse l r tm1 <*> bitraverse l r tm2 <*> bitraverse l r tm3

instance Bound2 BoolTerm where
  TmFalse          >>>>= _ = TmFalse
  TmTrue           >>>>= _ = TmTrue
  TmIf tm1 tm2 tm3 >>>>= f = TmIf (tm1 >>= f) (tm2 >>= f) (tm3 >>= f)

type WithBoolTerm tm n a = AsBoolTerm (tm n a) tm n a
