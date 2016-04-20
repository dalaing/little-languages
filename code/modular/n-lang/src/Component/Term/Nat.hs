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
module Component.Term.Nat (
    NatTerm(..)
  , AsNatTerm(..)
  , WithNatTerm
  ) where

import           Control.Lens.TH      (makeClassyPrisms)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))

import Bound2 (Bound2(..))

-- |
data NatTerm tm n a =
    TmZero         -- ^
  | TmSucc (tm n a)  -- ^
  | TmPred (tm n a)  -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NatTerm

instance Bifunctor tm => Bifunctor (NatTerm tm) where
  bimap _ _ TmZero = TmZero
  bimap l r (TmSucc tm) = TmSucc (bimap l r tm)
  bimap l r (TmPred tm) = TmPred (bimap l r tm)

instance Bifoldable tm => Bifoldable (NatTerm tm) where
  bifoldMap _ _ TmZero = mempty
  bifoldMap l r (TmSucc tm) = bifoldMap l r tm
  bifoldMap l r (TmPred tm) = bifoldMap l r tm

instance Bitraversable tm => Bitraversable (NatTerm tm) where
  bitraverse _ _ TmZero = pure TmZero
  bitraverse l r (TmSucc tm) = TmSucc <$> bitraverse l r tm
  bitraverse l r (TmPred tm) = TmPred <$> bitraverse l r tm

instance Bound2 NatTerm where
  TmZero    >>>>= _ = TmZero
  TmSucc tm >>>>= f = TmSucc (tm >>= f)
  TmPred tm >>>>= f = TmPred (tm >>= f)

type WithNatTerm tm n a = AsNatTerm (tm n a) tm n a
