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
module Component.Term.Bool (
    BoolTerm(..)
  , AsBoolTerm(..)
  , WithBoolTerm
  ) where

import Data.Monoid ((<>))

import Control.Lens (review)
import Control.Lens.Prism (Prism', prism)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))

import Bound2 (Bound2(..))
import Component.Term.Note.Strip (StripNoteTerm(..))

-- |
data BoolTerm tm n a =
    TmFalse       -- ^
  | TmTrue        -- ^
  | TmIf (tm n a) (tm n a) (tm n a) -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsBoolTerm s tm | s -> tm where
  _BoolTerm :: Prism' (s n a) (BoolTerm tm n a)

  _TmFalse :: Prism' (s n a) ()
  _TmFalse = _BoolTerm . _TmFalse

  _TmTrue :: Prism' (s n a) ()
  _TmTrue = _BoolTerm . _TmTrue

  _TmIf :: Prism' (s n a) (tm n a, tm n a, tm n a)
  _TmIf = _BoolTerm . _TmIf

instance AsBoolTerm (BoolTerm tm) tm where
  _BoolTerm = id

  _TmFalse = prism (const TmFalse) (\x -> case x of TmFalse -> Right (); _ -> Left x)
  _TmTrue = prism (const TmTrue) (\x -> case x of TmTrue -> Right (); _ -> Left x)
  _TmIf = prism (\(x, y, z) -> TmIf x y z) (\x -> case x of TmIf a b c -> Right (a, b, c); _ -> Left x)

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

instance (AsBoolTerm tm tm, StripNoteTerm tm tm) => StripNoteTerm (BoolTerm tm) tm where
  mapMaybeNoteTerm _ TmFalse =
    review _TmFalse ()
  mapMaybeNoteTerm _ TmTrue =
    review _TmTrue ()
  mapMaybeNoteTerm f (TmIf tm1 tm2 tm3) =
    review _TmIf (mapMaybeNoteTerm f tm1, mapMaybeNoteTerm f tm2, mapMaybeNoteTerm f tm3)

type WithBoolTerm tm = AsBoolTerm tm tm 
