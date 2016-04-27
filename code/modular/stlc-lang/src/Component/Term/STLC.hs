{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Component.Term.STLC (
    STLCVar(..)
  , AsSTLCVar(..)
  , STLCTerm(..)
  , AsSTLCTerm(..)
  , WithSTLCTerm
  , lam_
  ) where


import           Control.Lens       (review)
import Control.Lens.Prism (Prism', prism)
import           Bound              (Bound (..), Scope (..), abstract1)
import           Bound.Scope        (bitraverseScope)
import           Data.Bifoldable    (Bifoldable (..))
import           Data.Bifunctor     (Bifunctor (..))
import           Data.Bitraversable (Bitraversable (..), bifoldMapDefault,
                                     bimapDefault)

import           Bound2             (Bound2 (..))
import Component.Term.Note.Strip (StripNoteTerm(..))

-- |
data STLCVar (tm :: * -> * -> *) n a =
    TmVar a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsSTLCVar s tm | s -> tm where
  _STLCVar :: Prism' (s n a) (STLCVar tm n a)

  _TmVar :: Prism' (s n a) a
  _TmVar = _STLCVar . _TmVar

instance AsSTLCVar (STLCVar tm) tm where
  _STLCVar = id
  _TmVar = prism TmVar (\x -> case x of TmVar a -> Right a)

instance Bifunctor (STLCVar tm) where
  bimap _ r (TmVar x) = TmVar (r x)

instance Bifoldable (STLCVar tm) where
  bifoldMap _ r (TmVar x) = r x

instance Bitraversable (STLCVar tm) where
  bitraverse _ r (TmVar x) = TmVar <$> r x

instance (AsSTLCVar tm tm, StripNoteTerm tm tm) => StripNoteTerm (STLCVar tm) tm where
  mapMaybeNoteTerm _ (TmVar v) =
    review _TmVar v

-- |
data STLCTerm ty nTy tm nTm a =
    TmLam String (ty nTy) (Scope () (tm nTm) a)
  | TmApp (tm nTm a) (tm nTm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsSTLCTerm s ty nTy tm | s -> ty, s -> nTy, s -> tm where
  _STLCTerm :: Prism' (s nTm a) (STLCTerm ty nTy tm nTm a)

  _TmLam :: Prism' (s nTm a) (String, ty nTy, Scope () (tm nTm) a)
  _TmLam = _STLCTerm . _TmLam

  _TmApp :: Prism' (s n a) (tm n a, tm n a)
  _TmApp = _STLCTerm . _TmApp

instance AsSTLCTerm (STLCTerm ty nTy tm) ty nTy tm where
  _STLCTerm = id

  _TmLam = prism (\(x, y, z) -> TmLam x y z) (\x -> case x of TmLam a b c -> Right (a, b, c); _ -> Left x)
  _TmApp = prism (uncurry TmApp) (\x -> case x of TmApp y z -> Right (y, z); _ -> Left x)

instance Bitraversable tm => Bifunctor (STLCTerm ty nTy tm) where
  bimap = bimapDefault

instance Bitraversable tm => Bifoldable (STLCTerm ty nTy tm) where
  bifoldMap = bifoldMapDefault

instance Bitraversable tm => Bitraversable (STLCTerm ty nTy tm) where
  bitraverse l r (TmLam n t s) = TmLam n t <$> bitraverseScope l r s
  bitraverse l r (TmApp tm1 tm2) = TmApp <$> bitraverse l r tm1 <*> bitraverse l r tm2

instance Bound2 (STLCTerm ty nTy) where
  TmLam n ty s  >>>>= f = TmLam n ty (s >>>= f)
  TmApp tm1 tm2 >>>>= f = TmApp (tm1 >>= f) (tm2 >>= f)

{-
data ReifiedFunctor f = ReifiedFunctor { reifiedFmap :: forall a b. (a -> b) -> f a -> f b }
newtype ReflectedFunctor f s a = ReflectedFunctor (f a)

instance Reifies s (ReifiedFunctor f) => Functor (ReflectedFunctor f s) where
  fmap f (ReflectedFunctor g) = reflectResult1 (\m -> ReflectedFunctor (reifiedFmap m f g))

reflectResult1 :: forall f s a b. Reifies s a => (a -> f s b) -> f s b
reflectResult1 f = f (reflect (Proxy :: Proxy s))

unreflectedFunctor :: ReflectedFunctor f s a -> proxy s -> f a
unreflectedFunctor (ReflectedFunctor a) _ = a

reifyFunctor :: (forall x y. (x -> y) -> f x -> f y) -> (forall (s :: *). Reifies s (ReifiedFunctor f) => t -> ReflectedFunctor f s a) -> t -> f a
reifyFunctor f m xs = reify (ReifiedFunctor f) (unreflectedFunctor (m xs))
-}

hoistScope2 :: Bifunctor tm => (forall x. tm n x -> tm m x) -> Scope b (tm n) a -> Scope b (tm m) a
hoistScope2 t (Scope b) = Scope $ t (fmap t `second` b)

instance (Bifunctor tm, AsSTLCTerm tm ty nTy tm, StripNoteTerm tm tm) => StripNoteTerm (STLCTerm ty nTy tm) tm where
  mapMaybeNoteTerm f (TmLam v ty s) =
    review _TmLam (v, ty, hoistScope2 (mapMaybeNoteTerm f) s)
  mapMaybeNoteTerm f (TmApp tm1 tm2) =
    review _TmApp (mapMaybeNoteTerm f tm1, mapMaybeNoteTerm f tm2)

type WithSTLCTerm tm ty nTy =
  ( AsSTLCTerm tm ty nTy tm
  , AsSTLCVar tm tm
  )

-- TODO turn into an iso, where we instantiate with TmVar v ?
lam_ :: ( WithSTLCTerm tm ty nTy
        , Monad (tm nTm)
        )
     => String
     -> ty nTy
     -> tm nTm String
     -> tm nTm String
lam_ v ty tm =
  review _TmLam (v, ty, abstract1 v tm)
