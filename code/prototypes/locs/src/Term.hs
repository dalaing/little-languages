{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Term where

import Control.Monad (ap)
import Control.Monad.Trans (lift)

import Data.Bifunctor

import Prelude.Extras
import Bound
import Bound.Name

-- going to need some form of fix for parsing

data Ann f l a = Ann { note :: l, body :: f a}
               deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type TermA l a = TermF (Ann (TermA l) l) a

-- typeclass for use in monad should provide a lens to jump through the Ann
-- (or id in the usual case)

data TermF f a = 
    Var a
  | Lam (Scope (Name String ()) f a)
  | App (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Monad f, Eq1 f) => Eq1 (TermF f) where
  Var x1    ==# Var x2 = x1 == x2
  Lam e1    ==# Lam e2 = e1 ==# e2
  App f1 x1 ==# App f2 x2 = f1 ==# f2 && x1 ==# x2
  _ ==# _ = False

-- instance (Monad f, Ord1 f) => Ord1 (TermF f) where
--   compare1 = compare

-- instance (Functor f, Show1 f) => Show1 (TermF f) where
--  showsPrec1 = showsPrec

instance (Monad f, LowerTerm f) => Applicative (TermF f) where
  pure = return
  (<*>) = ap

instance (Monad f, LowerTerm f) => Monad (TermF f) where
  return = Var

  Var x >>= g = g x
  -- Lam e >>= g = Lam (e >>= lift . lowerTerm _ . g)
  App x y >>= g = App (x >>= lowerTerm x . g) (y >>= lowerTerm y . g)

class LowerTerm f where
  lowerTerm :: f a -> TermF f b -> f b

newtype Mu f a = In { out :: f (Mu f) a }

type Term = Mu TermF

instance Functor Term where
  fmap f = In . fmap f . out

instance Foldable Term where
  foldMap f = foldMap f . out

instance Traversable Term where
  traverse f = fmap In . traverse f . out

instance Applicative Term where
  pure = return
  (<*>) = ap

-- prisms to reach var / lam / app ?
-- iso between Scope (Name n ()) (f n) a and (n, f n n)

instance Monad Term where
  return = In . Var

  In (Var x) >>= g = g x
  In (Lam e) >>= g = In (Lam (e >>>= g))
  In (App x y) >>= g = In (App (x >>= g) (y >>= g))

data Cofree l f a = Maybe l :< f (Cofree l f) a

type LocTerm l = Cofree l TermF

instance Functor (LocTerm l) where
  fmap f (l :< r) = l :< fmap f r

instance Foldable (LocTerm l) where
  foldMap f (_ :< r) = foldMap f r

instance Traversable (LocTerm l) where
  traverse f (l :< r) = fmap (l :<) . traverse f $ r

instance Applicative (LocTerm l) where
  pure = return
  (<*>) = ap


-- this would mean have to use Maybe l instead of l
-- then we're back into the realm of what Ermine does
instance Monad (LocTerm l) where
  return x = Nothing :< Var x

  (_ :< Var x) >>= g = g x
  (l :< Lam e) >>= g = l :< Lam (e >>>= g)
  (l :< App x y) >>= g = l :< App (x >>= g) (y >>= g)

-- https://www.reddit.com/r/haskell/comments/1kzwlg/ideas_for_annotating_an_ast_with_custom_data/

-- bifunctor over 
-- data Term l a =
--     Var l a
--   | Lam l (Scope () (Term l) a)
--   | App l (Term l a) (Term l a)

{-
Hi all,

I am currently looking at `bound`[0] and considering the AST typing problem[1]

I have datatype for Lambda Calculus terms, parametised on the variable type:

data Term a =
    Var a
  | Lam (Scope () Term a) -- Scope is a Monad Transformer
  | App (Term a) (Term a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

Thanks to `bound`, the subsitution is taken care of by the monad instance:

instance Monad Term where
    return = Var

    Var x >>= g   = g x
    Lam e >>= g   = Lam (e >>= lift . g)
    App x y >>= g = App (x >>= g) (y >>= g)

Now I'd like to TODO

data TermF f a =
    Var a
  | Lam (Scope () f a)
  | App (f a) (f a)

-- fix for no source location
-- fix with source location

-- we can also do TermF (K Int) a

The thing I'm currently wondering about is

Cheers,

Dave
-}
