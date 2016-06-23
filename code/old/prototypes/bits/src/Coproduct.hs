{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Coproduct (
    Product(..)
  , PAppend(..)
  , Sum(..)
  , SAppend(..)
  , Contains(..)
  , All
  ) where

import Data.Type.Equality (type (==))
import GHC.Exts (Constraint)

class NotIn (x :: k) (l :: [k])
instance NotIn x '[]
instance (NotIn x t, (x == h) ~ 'False) => NotIn x (h ': t)

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

type family Reduce (f :: (k -> [k] -> Constraint)) (l :: [k]) :: Constraint where
  Reduce f '[] = ()
  Reduce f (h ': t) = (f h t, Reduce f t)

type Unique (l :: [k]) = Reduce NotIn l

data Product (l :: [(* -> *)]) (a :: *) where
  POne  :: h a -> Product (h ': '[]) a
  PMult  :: h a -> Product t a -> Product (h ': t) a

instance All Functor l => Functor (Product l) where
  fmap f (POne h) = POne (fmap f h)
  fmap f (PMult h t) = PMult (fmap f h) (fmap f t)

class PAppend (t :: [(* -> *)]) where
  type PAppendTail t :: * -> *
  (*:*) :: (a -> h a) -> (a -> (PAppendTail t) a) -> a -> Product (h ': t) a

instance Functor h => PAppend (h ': '[]) where
  type PAppendTail (h ': '[]) = h
  (*:*) f g a = PMult (f a) (POne (g a))

instance PAppend (h ': (i ': j)) where
  type PAppendTail (h ': (i ': j)) = Product (h ': (i ': j))
  (*:*) f g a = PMult (f a) (g a)

data Sum (l :: [(* -> *)]) (a :: *) where
  SNext :: Sum t a -> Sum (h ': t) a
  SAdd  :: h a -> Sum (h ': t) a

instance All Functor l => Functor (Sum l) where
  fmap f (SNext t) = SNext (fmap f t)
  fmap f (SAdd h) = SAdd (fmap f h)

class SAppend (t :: [(* -> *)]) where
  type SAppendTail t :: * -> *
  (+:+) :: (a -> h a) -> (a -> (SAppendTail t) a) -> a -> Sum (h ': t) a

instance Functor h => SAppend (h ': '[]) where
  type SAppendTail (h ': '[]) = h
  (+:+) f g a = SAdd (f a)

instance SAppend (h ': (i ': j)) where
  type SAppendTail (h ': (i ': j)) = Sum (h ': (i ': j))
  (+:+) f g a = SAdd (f a)

class Contains (x :: (* -> *)) (xs :: [(* -> *)]) where
  inj :: x a -> Sum xs a

instance Contains x (x ': t) where
  inj = SAdd

instance Contains x t => Contains x (h ': t) where
  inj = SNext . inj

{-
instance (Pairing h1 h2) => Pairing (Product (h1 ': '[])) (Sum (h2 ': '[])) where
  pair f (POne ph) (SAdd sh) = pair f ph sh

instance (Unique (h1 ': t1 ': u1), Unique (h2 ': t2 ': u2), Pairing h1 h2, Pairing (Product (t1 ': u1)) (Sum (t2 ': u2))) => Pairing (Product (h1 ': (t1 ': u1))) (Sum (h2 ': (t2 ': u2))) where
  pair f (PMult ph _) (SAdd sh) = pair f ph sh
  pair f (PMult _ pt) (SNext st) = pair f pt st
-}
