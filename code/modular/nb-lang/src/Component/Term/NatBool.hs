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

import Control.Lens (review)
import Control.Lens.Prism (Prism', prism)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))

import Bound2 (Bound2(..))
import Component.Term.Note.Strip (StripNoteTerm(..))

import Component.Term.Nat (WithNatTerm)
import Component.Term.Bool (WithBoolTerm)

-- |
data NatBoolTerm tm n a =
  TmIsZero (tm n a)  -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsNatBoolTerm s tm | s -> tm where
  _NatBoolTerm :: Prism' (s n a) (NatBoolTerm tm n a)

  _TmIsZero :: Prism' (s n a) (tm n a)
  _TmIsZero = _NatBoolTerm . _TmIsZero

instance AsNatBoolTerm (NatBoolTerm tm) tm where
  _NatBoolTerm = id

  _TmIsZero = prism TmIsZero (\x -> case x of TmIsZero y -> Right y)

instance Bifunctor tm => Bifunctor (NatBoolTerm tm) where
  bimap l r (TmIsZero tm) = TmIsZero (bimap l r tm)

instance Bifoldable tm => Bifoldable (NatBoolTerm tm) where
  bifoldMap l r (TmIsZero tm) = bifoldMap l r tm

instance Bitraversable tm => Bitraversable (NatBoolTerm tm) where
  bitraverse l r (TmIsZero tm) = TmIsZero <$> bitraverse l r tm

instance Bound2 NatBoolTerm where
  TmIsZero tm >>>>= f = TmIsZero (tm >>= f)

instance (AsNatBoolTerm tm tm, StripNoteTerm tm tm) => StripNoteTerm (NatBoolTerm tm) tm where
  mapMaybeNoteTerm f (TmIsZero tm1) =
    review _TmIsZero (mapMaybeNoteTerm f tm1)

type WithNatBoolTerm tm =
  ( AsNatBoolTerm tm tm
  , WithNatTerm tm
  , WithBoolTerm tm
  )
