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
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Component.Term.Int (
    IntTerm(..)
  , AsIntTerm(..)
  , WithIntTerm
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
data IntTerm tm nTy nTm a =
    TmIntLit Int         -- ^
  | TmAdd (tm nTy nTm a) (tm nTy nTm a)  -- ^
  | TmSub (tm nTy nTm a) (tm nTy nTm a)  -- ^
  | TmMul (tm nTy nTm a) (tm nTy nTm a)  -- ^
  | TmExp (tm nTy nTm a) (tm nTy nTm a)  -- ^
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsIntTerm s tm | s -> tm where
  _IntTerm :: Prism' (s nTy nTm a) (IntTerm tm nTy nTm a)

  _TmIntLit :: Prism' (s nTy nTm a) Int
  _TmIntLit =
    _IntTerm . prism TmIntLit
      (\x -> case x of TmIntLit i -> Right i; _ -> Left x)

  _TmAdd :: Prism' (s nTy nTm a) (tm nTy nTm a, tm nTy nTm a)
  _TmAdd =
    _IntTerm . prism (uncurry TmAdd)
      (\x -> case x of TmAdd y z -> Right (y, z); _ -> Left x)

  _TmSub :: Prism' (s nTy nTm a) (tm nTy nTm a, tm nTy nTm a)
  _TmSub =
    _IntTerm . prism (uncurry TmSub)
      (\x -> case x of TmSub y z -> Right (y, z); _ -> Left x)

  _TmMul :: Prism' (s nTy nTm a) (tm nTy nTm a, tm nTy nTm a)
  _TmMul =
    _IntTerm . prism (uncurry TmMul)
      (\x -> case x of TmMul y z -> Right (y, z); _ -> Left x)

  _TmExp :: Prism' (s nTy nTm a) (tm nTy nTm a, tm nTy nTm a)
  _TmExp =
    _IntTerm . prism (uncurry TmExp)
      (\x -> case x of TmExp y z -> Right (y, z); _ -> Left x)

instance Bifunctor (tm nTy) => Bifunctor (IntTerm tm nTy) where
  bimap _ _ (TmIntLit i) = TmIntLit i
  bimap l r (TmAdd tm1 tm2) = TmAdd (bimap l r tm1) (bimap l r tm2)
  bimap l r (TmSub tm1 tm2) = TmSub (bimap l r tm1) (bimap l r tm2)
  bimap l r (TmMul tm1 tm2) = TmMul (bimap l r tm1) (bimap l r tm2)
  bimap l r (TmExp tm1 tm2) = TmExp (bimap l r tm1) (bimap l r tm2)

instance Bifoldable (tm nTy) => Bifoldable (IntTerm tm nTy) where
  bifoldMap _ _ (TmIntLit _) = mempty
  bifoldMap l r (TmAdd tm1 tm2) = bifoldMap l r tm1 <> bifoldMap l r tm2
  bifoldMap l r (TmSub tm1 tm2) = bifoldMap l r tm1 <> bifoldMap l r tm2
  bifoldMap l r (TmMul tm1 tm2) = bifoldMap l r tm1 <> bifoldMap l r tm2
  bifoldMap l r (TmExp tm1 tm2) = bifoldMap l r tm1 <> bifoldMap l r tm2

instance Bitraversable (tm nTy) => Bitraversable (IntTerm tm nTy) where
  bitraverse _ _ (TmIntLit i) = pure (TmIntLit i)
  bitraverse l r (TmAdd tm1 tm2) = TmAdd <$> bitraverse l r tm1 <*> bitraverse l r tm2
  bitraverse l r (TmSub tm1 tm2) = TmSub <$> bitraverse l r tm1 <*> bitraverse l r tm2
  bitraverse l r (TmMul tm1 tm2) = TmMul <$> bitraverse l r tm1 <*> bitraverse l r tm2
  bitraverse l r (TmExp tm1 tm2) = TmExp <$> bitraverse l r tm1 <*> bitraverse l r tm2

instance Bound3 IntTerm where
  TmIntLit i    >>>>>= _ = TmIntLit i
  TmAdd tm1 tm2 >>>>>= f = TmAdd (tm1 >>= f) (tm2 >>= f)
  TmSub tm1 tm2 >>>>>= f = TmSub (tm1 >>= f) (tm2 >>= f)
  TmMul tm1 tm2 >>>>>= f = TmMul (tm1 >>= f) (tm2 >>= f)
  TmExp tm1 tm2 >>>>>= f = TmExp (tm1 >>= f) (tm2 >>= f)

instance (AsIntTerm tm tm, StripNoteTerm tm tm) => StripNoteTerm (IntTerm tm) tm where
  mapMaybeNoteTerm _ (TmIntLit i) =
    review _TmIntLit i
  mapMaybeNoteTerm f (TmAdd tm1 tm2) =
    review _TmAdd (mapMaybeNoteTerm f tm1, mapMaybeNoteTerm f tm2)
  mapMaybeNoteTerm f (TmSub tm1 tm2) =
    review _TmSub (mapMaybeNoteTerm f tm1, mapMaybeNoteTerm f tm2)
  mapMaybeNoteTerm f (TmMul tm1 tm2) =
    review _TmMul (mapMaybeNoteTerm f tm1, mapMaybeNoteTerm f tm2)
  mapMaybeNoteTerm f (TmExp tm1 tm2) =
    review _TmExp (mapMaybeNoteTerm f tm1, mapMaybeNoteTerm f tm2)

type WithIntTerm tm = AsIntTerm tm tm
