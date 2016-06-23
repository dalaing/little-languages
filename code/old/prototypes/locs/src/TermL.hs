{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module TermL where

import Control.Monad (ap)

import Data.Bifunctor

import Prelude.Extras
import Bound
import Bound.Name
import Bound.Scope

data Term l a =
    Var (Maybe l) a
  | Lam l (Scope (Name String ()) (Term l) a)
  | App l (Term l a) (Term l a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor Term where
  bimap f g (Var l x) = Var (fmap f l) (g x)
  bimap f g (Lam l e) = Lam (f l) (hoistScope (first f) . fmap g $ e)
  bimap f g (App l x y) = App (f l) (bimap f g x) (bimap f g y)

-- TODO bifoldable and bitraverse, possibly biapplicative

instance Eq l => Eq1 (Term l) where
  (==#) = (==)

instance Ord l => Ord1 (Term l) where
  compare1 = compare

instance Show l => Show1 (Term l) where
  showsPrec1 = showsPrec

instance Applicative (Term l) where
  pure = return
  (<*>) = ap

instance Monad (Term l) where
  return = Var Nothing

  Var _ x   >>= g = g x
  Lam l e   >>= g = Lam l (e >>>= g)
  App l x y >>= g = App l (x >>= g) (y >>= g)


