{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Term where

import Control.Monad (ap)
import Control.Monad.Trans (lift)

import Prelude.Extras
import Bound
import Bound.Name

data TermF f a =
    Var a
  | Lam (Scope (Name String ()) f a)
  | App (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Term a = Term { unTerm :: TermF Term a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq1 Term where
  (==#) = (==)

instance Ord1 Term where
  compare1 = compare

instance Show1 Term where
  showsPrec1 = showsPrec

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = Term . Var

  Term (Var x)   >>= g = g x
  Term (Lam e)   >>= g = Term $ Lam (e >>>= g)
  Term (App f x) >>= g = Term $ App (f >>= g) (x >>= g)

{-
data Ann l f a = Ann { ann :: l, core :: f a}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype LocTerm l a = LocTerm { unLocTerm :: TermF (Ann l (LocTerm l)) a}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq1 (LocTerm l) where
  (==#) = (==)

instance Ord1 (LocTerm l) where
  compare1 = compare

instance Show1 (LocTerm l) where
  showsPrec1 = showsPrec

instance Applicative (LocTerm l) where
  pure = return
  (<*>) = ap

instance Monad (LocTerm l) where
  return = Var

  Var x >>= g   = g x
  -- Lam e >>= g   = Lam . toScope . (\(l :< x) -> (l :< _)) . fromScope $ e
  App (Ann l1 f) (Ann l2 x) >>= g = App (Ann l1 (f >>= g)) (Ann l2 (x >>= g))
-}

