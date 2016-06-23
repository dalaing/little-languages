{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Term where

import Control.Lens.Prism (Prism', prism)
import Control.Lens.TH
import Control.Monad (ap)

import Bound
import Prelude.Extras

data Term n a =
    TmVar a
  -- | TmLam (Scope (Name n ()) (Term n) a)
  | TmLam n (Scope () (Term n) a)
  | TmApp (Term n a) (Term n a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq n => Eq1 (Term n) where
  (==#) = (==)

instance Ord n => Ord1 (Term n) where
  compare1 = compare

instance Show n => Show1 (Term n) where
  showsPrec1 = showsPrec

instance Applicative (Term n) where
  pure = return
  (<*>) = ap

instance Monad (Term n) where
  return = TmVar

  TmVar x >>= g = g x
  TmLam n e >>= g = TmLam n (e >>>= g)
  TmApp f x >>= g = TmApp (f >>= g) (x >>= g)

makeClassyPrisms ''Term

_lam :: Eq a => Prism' (Term a a) (a, Term a a)
_lam = prism mk match
  where
    mk (x, e) = TmLam x $ abstract1 x e
    match (TmLam n e) = Right (n, instantiate1 (TmVar n) e)
    match x           = Left x

size :: Term n a -> Int
size (TmVar _) = 1
size (TmLam _ t) = 1 + (size . fromScope $ t)
size (TmApp t1 t2) = 1 + size t1 + size t2
