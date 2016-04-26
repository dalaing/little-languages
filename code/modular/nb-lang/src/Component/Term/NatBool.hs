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
module Component.Term.NatBool (
    NatBoolTerm(..)
  , AsNatBoolTerm(..)
  , WithNatBoolTerm
  ) where

import           Control.Lens.TH      (makeClassyPrisms)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))

import Bound2 (Bound2(..))

import Component.Term.Nat (WithNatTerm)
import Component.Term.Bool (WithBoolTerm)

-- |
data NatBoolTerm tm n a =
  TmIsZero (tm n a)  -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NatBoolTerm

instance Bifunctor tm => Bifunctor (NatBoolTerm tm) where
  bimap l r (TmIsZero tm) = TmIsZero (bimap l r tm)

instance Bifoldable tm => Bifoldable (NatBoolTerm tm) where
  bifoldMap l r (TmIsZero tm) = bifoldMap l r tm

instance Bitraversable tm => Bitraversable (NatBoolTerm tm) where
  bitraverse l r (TmIsZero tm) = TmIsZero <$> bitraverse l r tm

instance Bound2 NatBoolTerm where
  TmIsZero tm >>>>= f = TmIsZero (tm >>= f)

type WithNatBoolTerm tm n a =
  ( AsNatBoolTerm (tm n a) tm n a
  , WithNatTerm tm n a
  , WithBoolTerm tm n a
  )
