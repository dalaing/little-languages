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
{-# LANGUAGE FlexibleContexts       #-}
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

import Bound2 (Bound3(..))
import Component.Term.Note.Strip (StripNoteTerm(..))

-- |
data BoolTerm tm nTy nTm a =
    TmFalse       -- ^
  | TmTrue        -- ^
  | TmIf (tm nTy nTm a) (tm nTy nTm a) (tm nTy nTm a) -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsBoolTerm s tm | s -> tm where
  _BoolTerm :: Prism' (s nTy nTm a) (BoolTerm tm nTy nTm a)

  _TmFalse :: Prism' (s nTy nTm a) ()
  _TmFalse = _BoolTerm . _TmFalse

  _TmTrue :: Prism' (s nTy nTm a) ()
  _TmTrue = _BoolTerm . _TmTrue

  _TmIf :: Prism' (s nTy nTm a) (tm nTy nTm a, tm nTy nTm a, tm nTy nTm a)
  _TmIf = _BoolTerm . _TmIf

instance AsBoolTerm (BoolTerm tm) tm where
  _BoolTerm = id

  _TmFalse = prism (const TmFalse) (\x -> case x of TmFalse -> Right (); _ -> Left x)
  _TmTrue = prism (const TmTrue) (\x -> case x of TmTrue -> Right (); _ -> Left x)
  _TmIf = prism (\(x, y, z) -> TmIf x y z) (\x -> case x of TmIf a b c -> Right (a, b, c); _ -> Left x)

instance Bifunctor (tm nTy) => Bifunctor (BoolTerm tm nTy) where
  bimap _ _ TmFalse = TmFalse
  bimap _ _ TmTrue = TmTrue
  bimap l r (TmIf tm1 tm2 tm3) = TmIf (bimap l r tm1) (bimap l r tm2) (bimap l r tm3)

instance Bifoldable (tm nTy) => Bifoldable (BoolTerm tm nTy) where
  bifoldMap _ _ TmFalse = mempty
  bifoldMap _ _ TmTrue = mempty
  bifoldMap l r (TmIf tm1 tm2 tm3) = bifoldMap l r tm1 <> bifoldMap l r tm2 <> bifoldMap l r tm3

instance Bitraversable (tm nTy) => Bitraversable (BoolTerm tm nTy) where
  bitraverse _ _ TmFalse = pure TmFalse
  bitraverse _ _ TmTrue = pure TmTrue
  bitraverse l r (TmIf tm1 tm2 tm3) = TmIf <$> bitraverse l r tm1 <*> bitraverse l r tm2 <*> bitraverse l r tm3

instance Bound3 BoolTerm where
  TmFalse          >>>>>= _ = TmFalse
  TmTrue           >>>>>= _ = TmTrue
  TmIf tm1 tm2 tm3 >>>>>= f = TmIf (tm1 >>= f) (tm2 >>= f) (tm3 >>= f)

instance (AsBoolTerm tm tm, StripNoteTerm tm tm) => StripNoteTerm (BoolTerm tm) tm where
  mapMaybeNoteTerm _ TmFalse =
    review _TmFalse ()
  mapMaybeNoteTerm _ TmTrue =
    review _TmTrue ()
  mapMaybeNoteTerm f (TmIf tm1 tm2 tm3) =
    review _TmIf (mapMaybeNoteTerm f tm1, mapMaybeNoteTerm f tm2, mapMaybeNoteTerm f tm3)

type WithBoolTerm tm = AsBoolTerm tm tm 
