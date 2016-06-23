{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
module TermL where

import Control.Monad (ap)

import Bound
import Bound.Name
import Bound.Scope

import Prelude.Extras

data Term l n a =
  -- TODO split Var into a Var with a location and a Var without?
  -- TODO might need to track location in a Name-like structure along with the name
    Var (Maybe l) a
  | Lam l (Scope (Name n ()) (Term l n) a)
  | App l (Term l n a) (Term l n a)
  | TmInt l Int
  | TmBool l Bool
  | Add l (Term l n a) (Term l n a)
  | Equ l (Term l n a) (Term l n a)
  | And l (Term l n a) (Term l n a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq l, Eq n) => Eq1 (Term l n) where
  (==#) = (==)

instance (Ord l, Ord n) => Ord1 (Term l n) where
  compare1 = compare

instance (Show l, Show n) => Show1 (Term l n) where 
  showsPrec1 = showsPrec

instance Applicative (Term l n) where
  pure = return
  (<*>) = ap

instance Monad (Term l n) where
  return = Var Nothing

  Var _ x >>= g   = g x
  Lam l e >>= g = Lam l (e >>>= g)
  App l f x >>= g = App l (f >>= g) (x >>= g)

  TmInt l i >>= _ = TmInt l i
  TmBool l b >>= _ = TmBool l b

  Add l x y >>= g = Add l (x >>= g) (y >>= g)
  Equ l x y >>= g = Equ l (x >>= g) (y >>= g)
  And l x y >>= g = And l (x >>= g) (y >>= g)
