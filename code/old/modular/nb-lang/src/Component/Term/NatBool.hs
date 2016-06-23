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
{-# LANGUAGE FlexibleContexts       #-}
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

import Bound2 (Bound3(..))
import Component.Term.Note.Strip (StripNoteTerm(..))

import Component.Term.Nat (WithNatTerm)
import Component.Term.Bool (WithBoolTerm)

-- |
data NatBoolTerm tm nTy nTm a =
  TmIsZero (tm nTy nTm a)  -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsNatBoolTerm s tm | s -> tm where
  _NatBoolTerm :: Prism' (s nTy nTm a) (NatBoolTerm tm nTy nTm a)

  _TmIsZero :: Prism' (s nTy nTm a) (tm nTy nTm a)
  _TmIsZero = _NatBoolTerm . _TmIsZero

instance AsNatBoolTerm (NatBoolTerm tm) tm where
  _NatBoolTerm = id

  _TmIsZero = prism TmIsZero (\x -> case x of TmIsZero y -> Right y)

instance Bifunctor (tm nTy) => Bifunctor (NatBoolTerm tm nTy) where
  bimap l r (TmIsZero tm) = TmIsZero (bimap l r tm)

instance Bifoldable (tm nTy) => Bifoldable (NatBoolTerm tm nTy) where
  bifoldMap l r (TmIsZero tm) = bifoldMap l r tm

instance Bitraversable (tm nTy) => Bitraversable (NatBoolTerm tm nTy) where
  bitraverse l r (TmIsZero tm) = TmIsZero <$> bitraverse l r tm

instance Bound3 NatBoolTerm where
  TmIsZero tm >>>>>= f = TmIsZero (tm >>= f)

instance (AsNatBoolTerm tm tm, StripNoteTerm tm tm) => StripNoteTerm (NatBoolTerm tm) tm where
  mapMaybeNoteTerm f (TmIsZero tm1) =
    review _TmIsZero (mapMaybeNoteTerm f tm1)

type WithNatBoolTerm tm =
  ( AsNatBoolTerm tm tm
  , WithNatTerm tm
  , WithBoolTerm tm
  )
