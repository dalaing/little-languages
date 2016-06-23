{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TermF where

import Control.Monad (ap)

import Bound
import Bound.Name
import Bound.Scope

import Prelude.Extras

data TermF n f a =
    Var a
  | Lam (Scope (Name n ()) f a)
  | App (f a) (f a)
  | TmInt Int
  | TmBool Bool
  | Add (f a) (f a)
  | Equ (f a) (f a)
  | And (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{-
instance Functor f => Applicative (TermF n f) where
  pure = return
  (<*>) = ap

instance Functor f => Monad (TermF n f) where
  return = Var

  Var x >>= g = g x
  -- Lam e >>= g = Lam (e >>>= g)
  App f x >>= g = App (f >>= g) (x >>= g)
  TmInt i >>= _ = TmInt i
  TmBool b >>= _ = TmBool b
  Add x y >>= g = Add (x >>= g) (y >>= g)
  Equ x y >>= g = Equ (x >>= g) (y >>= g)
  And x y >>= g = And (x >>= g) (y >>= g)
-}

-- newtype Mu f a = In { out :: f (Mu f) a }
-- type Term n = Mu (TermF n)

data Term n a = T (TermF n (Term n) a)
              deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq1 (Term n) where
  (==#) = (==)

instance Ord1 (Term n) where
  compare1 = compare

instance Show n => Show1 (Term n) where
  showsPrec1 = showsPrec

instance Applicative (Term n) where
  pure = return
  (<*>) = ap

instance Monad (Term n) where
  return = T . Var

  T (Var x) >>= g = g x
  T (Lam e) >>= g = T $ Lam (e >>>= g)
  T (App f x) >>= g = T $ App (f >>= g) (x >>= g)
  T (TmInt i) >>= _ = T $ TmInt i
  T (TmBool b) >>= _ = T $ TmBool b
  T (Add x y) >>= g = T $ Add (x >>= g) (y >>= g)
  T (Equ x y) >>= g = T $ Equ (x >>= g) (y >>= g)
  T (And x y) >>= g = T $ And (x >>= g) (y >>= g)

-- data Ann l f a = Ann { ann :: l, rest :: f (Ann l f) a}
-- type LocTerm l n = Ann l (TermF n)

data AnnTerm n l f a = TA { ann :: l, rest :: f a}
                     deriving (Functor, Foldable, Traversable)

instance Bound (AnnTerm n l) where
  TA l x >>>= f = TA l (x >>= f)

data TermLoc n l a = TL (TermF n (AnnTerm n l (TermLoc n l)) a)
              deriving (Functor, Foldable, Traversable)

instance Applicative (TermLoc n l) where
  pure = return
  (<*>) = ap

instance Monad (TermLoc n l) where
  return = TL . Var

  TL (Var x) >>= g = g x
  TL (Lam e) >>= g =
    let
      l = ann _
      e' = hoistScope rest e
      e'' = e' >>>= g
    in
      TL $ Lam (hoistScope (TA l) e'')
  TL (App f x) >>= g = TL $ App (f >>>= g) (x >>>= g)
  TL (TmInt i) >>= _ = TL $ TmInt i
  TL (TmBool b) >>= _ = TL $ TmBool b
  TL (Add x y) >>= g = TL $ Add (x >>>= g) (y >>>= g)
  TL (Equ x y) >>= g = TL $ Equ (x >>>= g) (y >>>= g)
  TL (And x y) >>= g = TL $ And (x >>>= g) (y >>>= g)
