{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PolyKinds #-}
module Forall (
    Forall3, inst3
  , Forall2, inst2
  ) where

import Data.Constraint
import Unsafe.Coerce (unsafeCoerce)

type family SkolemT1 (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4) :: k1
type family SkolemT2 (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4) :: k2
type family SkolemT3 (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4) :: k3

type family Forall3 (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4) :: Constraint where
    Forall3 p t = Forall3_ p t
class p (t (SkolemT1 p t) (SkolemT2 p t) (SkolemT3 p t)) => Forall3_ (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4)
instance p (t (SkolemT1 p t) (SkolemT2 p t) (SkolemT3 p t)) => Forall3_ (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4)

inst3 :: forall (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4) (f :: k1) (a :: k2) (b :: k3). Forall3 p t :- p (t f a b)
inst3 = unsafeCoerce (Sub Dict :: Forall3 p t :- p (t (SkolemT1 p t) (SkolemT2 p t) (SkolemT3 p t)))


type family Forall2 (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4) (b :: k3) :: Constraint where
    Forall2 p t b = Forall2_ p t b
class p (t (SkolemT1 p t) (SkolemT2 p t) b) => Forall2_ (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4) (b :: k3)
instance p (t (SkolemT1 p t) (SkolemT2 p t) b) => Forall2_ (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4) (b :: k3)

inst2 :: forall (p :: k4 -> Constraint) (t :: k1 -> k2 -> k3 -> k4) (f :: k1) (a :: k2) (b :: k3). Forall2 p t b :- p (t f a b)
inst2 = unsafeCoerce (Sub Dict :: Forall2 p t b :- p (t (SkolemT1 p t) (SkolemT2 p t) b))
