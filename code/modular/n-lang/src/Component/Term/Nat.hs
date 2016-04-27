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
module Component.Term.Nat (
    NatTerm(..)
  , AsNatTerm(..)
  , WithNatTerm
  ) where

import Control.Lens (review)
import Control.Lens.Prism (Prism', prism)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))

import Bound2 (Bound2(..))
import Component.Term.Note.Strip (StripNoteTerm(..))

-- |
data NatTerm tm n a =
    TmZero         -- ^
  | TmSucc (tm n a)  -- ^
  | TmPred (tm n a)  -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsNatTerm s tm | s -> tm where
  _NatTerm :: Prism' (s n a) (NatTerm tm n a)

  _TmZero :: Prism' (s n a) ()
  _TmZero = _NatTerm . _TmZero

  _TmSucc :: Prism' (s n a) (tm n a)
  _TmSucc = _NatTerm . _TmSucc

  _TmPred :: Prism' (s n a) (tm n a)
  _TmPred = _NatTerm . _TmPred

instance AsNatTerm (NatTerm tm) tm where
  _NatTerm = id

  _TmZero = prism (const TmZero) (\x -> case x of TmZero -> Right (); _ -> Left x)
  _TmSucc = prism TmSucc (\x -> case x of TmSucc y -> Right y; _ -> Left x)
  _TmPred = prism TmPred (\x -> case x of TmPred y -> Right y; _ -> Left x)

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

instance (AsNatTerm tm tm, StripNoteTerm tm tm) => StripNoteTerm (NatTerm tm) tm where
  mapMaybeNoteTerm _ TmZero =
    review _TmZero ()
  mapMaybeNoteTerm f (TmSucc tm1) =
    review _TmSucc (mapMaybeNoteTerm f tm1)
  mapMaybeNoteTerm f (TmPred tm1) =
    review _TmPred (mapMaybeNoteTerm f tm1)

type WithNatTerm tm = AsNatTerm tm tm
