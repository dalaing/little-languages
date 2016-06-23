{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Term where

import Control.Monad (ap)

import Control.Lens.TH
import Control.Lens.Prism (Prism', prism)

import Bound
import Bound.Scope
import Prelude.Extras

import Loc
import Type

-- do we want to have Eq / Ord / Show ignore the TmLoc annotations?
-- we could add a newtype that uses stripLoc before those operations
data Term n l a =
    TmVar a
  | TmLam n (Type l) (Scope () (Term n l) a)
  | TmApp (Term n l a) (Term n l a)
  | TmFalse
  | TmTrue
  | TmIf (Term n l a) (Term n l a) (Term n l a)
  | TmLoc l (Term n l a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq n, Eq l) => Eq1 (Term n l) where
  (==#) = (==)

instance (Ord n, Ord l) => Ord1 (Term n l) where
  compare1 = compare

instance (Show n, Show l) => Show1 (Term n l) where
  showsPrec1 = showsPrec

instance Applicative (Term n l) where
  pure = return
  (<*>) = ap

instance Monad (Term n l) where
  return = TmVar

  TmVar x     >>= g = g x
  TmLam n t e >>= g = TmLam n t (e >>>= g)
  TmApp f x   >>= g = TmApp (f >>= g) (x >>= g)
  TmFalse     >>= _ = TmFalse
  TmTrue      >>= _ = TmTrue
  TmIf b t f  >>= g = TmIf (b >>= g) (t >>= g) (f >>= g)
  TmLoc l t   >>= g = TmLoc l (t >>= g)

makeClassyPrisms ''Term

instance WithLoc (Term n l a) where
  type Loc (Term n l a) = l
  type Without (Term n l a) = Term n () a

  _Loc = _TmLoc

  stripLoc (TmVar x) = TmVar x
  stripLoc (TmLam n t e) = TmLam n (stripLoc t) (hoistScope stripLoc e)
  stripLoc (TmApp f x) = TmApp (stripLoc f) (stripLoc x)
  stripLoc TmFalse = TmFalse
  stripLoc TmTrue = TmTrue
  stripLoc (TmIf t1 t2 t3) = TmIf (stripLoc t1) (stripLoc t2) (stripLoc t3)
  stripLoc (TmLoc _ t) = stripLoc t

-- cata was nice for this, maybe bring back TermF if there are enough
-- opportunities to use cata?

_lam :: Eq a
     => Prism' (Term a l a) (a, Type l, Term a l a)
_lam = prism mk match
  where
    mk (x, t, e) = TmLam x t $ abstract1 x e
    match (TmLam n t e) = Right (n, t, instantiate1 (TmVar n) e)
    match x             = Left x

size :: Term n l a -> Int
size (TmVar _) = 1
size (TmLam _ _ e) = 1 + size (fromScope e)
size (TmApp f x) = 1 + size f + size x
size TmFalse = 1
size TmTrue = 1
size (TmIf t1 t2 t3) = 1 + size t1 + size t2 + size t3
size (TmLoc _ t) = 1 + size t

