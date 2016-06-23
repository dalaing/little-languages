{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Bits where

import Control.Monad (ap)

import Bound
import Bound.Name
import Prelude.Extras

data LC n t a =
    TmVar a
  | TmApp (LC n t a) (LC n t a)
  | TmLam t (Scope (Name n ()) (LC n t) a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq t => Eq1 (LC n t) where
  (==#) = (==)

instance Ord t => Ord1 (LC n t) where
  compare1 = compare

instance (Show n, Show t) => Show1 (LC n t) where
  showsPrec1 = showsPrec

instance Applicative (LC n t) where
  pure = return
  (<*>) = ap

instance Monad (LC n t) where
  return = TmVar

  TmVar x   >>= g = g x
  TmApp f x >>= g = TmApp (f >>= g) (x >>= g) 
  TmLam t e >>= g = TmLam t (e >>>= g)

data TmNum f a =
    TmAdd (f a) (f a)
  | TmSub (f a) (f a)
  | TmMul (f a) (f a)
  | TmNeg (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bound TmNum where
  TmAdd x y >>>= f = TmAdd (x >>= f) (y >>= f)
  TmSub x y >>>= f = TmSub (x >>= f) (y >>= f)
  TmMul x y >>>= f = TmMul (x >>= f) (y >>= f)
  TmNeg x   >>>= f = TmNeg (x >>= f)

data TmLogic f a =
    TmAnd (f a) (f a)
  | TmOr (f a) (f a)
  | TmNot (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bound TmLogic where
  TmAnd x y >>>= f = TmAnd (x >>= f) (y >>= f)
  TmOr  x y >>>= f = TmOr  (x >>= f) (y >>= f)
  TmNot x   >>>= f = TmNot (x >>= f)

data TmEq f a =
    TmEq (f a) (f a)
  | TmNeq (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bound TmEq where
  TmEq  x y >>>= f = TmEq  (x >>= f) (y >>= f)
  TmNeq x y >>>= f = TmNeq (x >>= f) (y >>= f)

data TmOrd f a =
    TmLt (f a) (f a)
  | TmLte (f a) (f a)
  | TmGt (f a) (f a)
  | TmGte (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bound TmOrd where
  TmLt x y  >>>= f = TmLt  (x >>= f) (y >>= f)
  TmLte x y >>>= f = TmLte (x >>= f) (y >>= f)
  TmGt x y  >>>= f = TmGt  (x >>= f) (y >>= f)
  TmGte x y >>>= f = TmGte (x >>= f) (y >>= f)

