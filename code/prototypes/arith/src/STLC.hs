{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module STLC where

import Control.Applicative
import Control.Monad (ap)
import Data.Foldable (toList)
import Data.List (elemIndex)
import Data.Bifunctor

import Bound
import Bound.Name
import Bound.Scope

import Prelude.Extras

import Test.QuickCheck

import Control.Lens

import STLC.Type

-- TODO classy prisms here 
-- we want to be able to make some things available when ints / bools
-- / both are available

data TermF l n f a =
    TmVar a
  | TmLam Type (Scope (Name n ()) f a)
  | TmApp (f a) (f a)
  | TmLoc l (f a)
  | TmInt Int
  | TmBool Bool
  | TmAdd (f a) (f a)
  | TmEq (f a) (f a)
  | TmAnd (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TermF

newtype Term l n a = Term { unTerm :: TermF l n (Term l n ) a}
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
  return = Term . TmVar

  Term (TmVar x) >>= g   = g x
  Term tm >>= g = Term $ case tm of
    TmLam t e -> TmLam t (e >>>= g)
    TmApp f x -> TmApp (f >>= g) (x >>= g)
    TmLoc l x -> TmLoc l (x >>= g)

    TmInt i -> TmInt i
    TmBool b -> TmBool b

    TmAdd x y -> TmAdd (x >>= g) (y >>= g)
    TmEq x y -> TmEq (x >>= g) (y >>= g)
    TmAnd x y -> TmAnd (x >>= g) (y >>= g)

makeWrapped ''Term

instance AsTermF (Term l n a) l n (Term l n) a where
  _TmVar = _Wrapped . _TmVar
  _TmLam = _Wrapped . _TmLam
  _TmApp = _Wrapped . _TmApp
  _TmLoc = _Wrapped . _TmLoc
  _TmInt = _Wrapped . _TmInt
  _TmBool = _Wrapped . _TmBool
  _TmAdd = _Wrapped . _TmAdd
  _TmEq = _Wrapped . _TmEq
  _TmAnd = _Wrapped . _TmAnd

liftL2 :: Prism' s1 a1 -> Prism' s2 a2 -> Prism' (s1, s2) (a1, a2)
liftL2 p1 p2 = prism 
  (\(b1, b2) -> (review p1 b1, review p2 b2))
  (\s@(s1, s2) -> let
      x = (,) <$> preview p1 s1 <*> preview p2 s2
    in
      case x of
          Nothing -> Left s
          Just y -> Right y
   )

liftL3 :: Prism' s1 a1 -> Prism' s2 a2 -> Prism' s3 a3 -> Prism' (s1, s2, s3) (a1, a2, a3)
liftL3 p1 p2 p3 = prism
  (\(b1, b2, b3) -> (review p1 b1, review p2 b2, review p3 b3))
  (\s@(s1, s2, s3) -> let
      x = (,,) <$> preview p1 s1 <*> preview p2 s2 <*> preview p3 s3
    in
      case x of
          Nothing -> Left s
          Just y -> Right y
   )

-- lam is a value
-- need types / prisms for values here as well

