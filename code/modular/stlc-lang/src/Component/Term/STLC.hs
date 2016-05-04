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
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Component.Term.STLC (
    STLCVar(..)
  , AsSTLCVar(..)
  , STLCTerm(..)
  , AsSTLCTerm(..)
  , WithSTLCTerm
  , lam_
  , app_
  ) where


import           Control.Lens       (review)
import Control.Lens.Prism (Prism', prism)
import           Bound              (Bound (..), Scope (..), abstract1, instantiate1)
import           Bound.Scope        (bitraverseScope)
import           Data.Bifoldable    (Bifoldable (..))
import           Data.Bifunctor     (Bifunctor (..))
import           Data.Bitraversable (Bitraversable (..), bifoldMapDefault,
                                     bimapDefault)
import Data.Proxy
import Data.Constraint (Dict(..), (\\), (:-))
import Data.Constraint.Forall (instT, ForallT)

import           Bound2             (Bound3 (..))
import Bifunctor2 (Bifunctor2(..))
import Component.Term.Note.Strip (StripNoteTerm(..))

-- |
data STLCVar (tm :: * -> * -> * -> *) nTy nTm a =
    TmVar a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsSTLCVar s tm | s -> tm where
  _STLCVar :: Prism' (s nTy nTm a) (STLCVar tm nTy nTm a)

  _TmVar :: Prism' (s nTy nTm a) a
  _TmVar = _STLCVar . _TmVar

instance AsSTLCVar (STLCVar tm) tm where
  _STLCVar = id
  _TmVar = prism TmVar (\x -> case x of TmVar a -> Right a)

instance Bifunctor (STLCVar tm nTy) where
  bimap _ r (TmVar x) = TmVar (r x)

instance Bifoldable (STLCVar tm nTy) where
  bifoldMap _ r (TmVar x) = r x

instance Bitraversable (STLCVar tm nTy) where
  bitraverse _ r (TmVar x) = TmVar <$> r x

instance (AsSTLCVar tm tm, StripNoteTerm tm tm) => StripNoteTerm (STLCVar tm) tm where
  mapMaybeNoteTerm _ (TmVar v) =
    review _TmVar v

-- |
data STLCTerm ty tm nTy nTm a =
    TmLam String (ty nTy) (Scope () (tm nTy nTm) a)
  | TmApp (tm nTy nTm a) (tm nTy nTm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class AsSTLCTerm s ty tm | s -> ty, s -> tm where
  _STLCTerm :: Prism' (s nTy nTm a) (STLCTerm ty tm nTy nTm a)

  _TmLam :: Prism' (s nTy nTm a) (String, ty nTy, Scope () (tm nTy nTm) a)
  _TmLam = _STLCTerm . _TmLam

  _TmApp :: Prism' (s nTy nTm a) (tm nTy nTm a, tm nTy nTm a)
  _TmApp = _STLCTerm . _TmApp

instance AsSTLCTerm (STLCTerm ty tm) ty tm where
  _STLCTerm = id

  _TmLam = prism (\(x, y, z) -> TmLam x y z) (\x -> case x of TmLam a b c -> Right (a, b, c); _ -> Left x)
  _TmApp = prism (uncurry TmApp) (\x -> case x of TmApp y z -> Right (y, z); _ -> Left x)

instance Bitraversable (tm nTy) => Bifunctor (STLCTerm ty tm nTy) where
  bimap = bimapDefault

instance Bitraversable (tm nTy) => Bifoldable (STLCTerm ty tm nTy) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (tm nTy) => Bitraversable (STLCTerm ty tm nTy) where
  bitraverse l r (TmLam n t s) = TmLam n t <$> bitraverseScope l r s
  bitraverse l r (TmApp tm1 tm2) = TmApp <$> bitraverse l r tm1 <*> bitraverse l r tm2

instance Bound3 (STLCTerm ty) where
  TmLam n ty s  >>>>>= f = TmLam n ty (s >>>= f)
  TmApp tm1 tm2 >>>>>= f = TmApp (tm1 >>= f) (tm2 >>= f)

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

hoistScope3 :: forall tm nTy nTm mTm a b. Bifunctor2 tm => (forall x. tm nTy nTm x -> tm nTy mTm x) -> Scope b (tm nTy nTm) a -> Scope b (tm nTy mTm) a
hoistScope3 t (Scope b) =
  case bifunctor2 (Proxy :: Proxy nTy) :: Dict (Bifunctor (tm nTy)) of
    Dict -> Scope $ t (fmap t `second` b)

instance (AsSTLCTerm tm ty tm, StripNoteTerm tm tm, Bifunctor2 tm) => StripNoteTerm (STLCTerm ty tm) tm where
  mapMaybeNoteTerm f (TmLam v ty s) =
    review _TmLam (v, ty, hoistScope3 (mapMaybeNoteTerm f) s)
  mapMaybeNoteTerm f (TmApp tm1 tm2) =
    review _TmApp (mapMaybeNoteTerm f tm1, mapMaybeNoteTerm f tm2)

type WithSTLCTerm tm ty =
  ( AsSTLCTerm tm ty tm
  , AsSTLCVar tm tm
  )

-- TODO turn into an iso, where we instantiate with TmVar v ?
{-
lam_ :: forall tm nTm ty nTy. ( WithSTLCTerm tm ty 
        , Monad2 tm
        )
     => String
     -> ty nTy
     -> tm nTy nTm String
     -> tm nTy nTm String
lam_ v ty tm =
  case monad2 (Proxy :: Proxy nTy) (Proxy :: Proxy nTm) :: Dict (Monad (tm nTy nTm)) of
    Dict -> review _TmLam (v, ty, abstract1 v tm)
-}

lam_ :: forall tm ty nTy nTm. (WithSTLCTerm tm ty, ForallT Monad tm)
     => String
     -> ty nTy
     -> tm nTy nTm String
     -> tm nTy nTm String
lam_ v ty tm =
  review _TmLam (v, ty, abstract1 v tm \\ (instT :: ForallT Monad tm :- Monad (tm nTy nTm))) 

app_ :: forall tm nTm ty nTy. ( WithSTLCTerm tm ty 
        , ForallT Monad tm
        )
     => tm nTy nTm String
     -> Scope () (tm nTy nTm) String
     -> tm nTy nTm String
app_ sub scope =
  instantiate1 sub scope \\ (instT :: ForallT Monad tm :- Monad (tm nTy nTm))
