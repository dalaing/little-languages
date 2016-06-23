{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
module Decl where

import Control.Monad

import Bound
import Prelude.Extras

data Decl l n t d = 
    Decl d
  | DclLoc l (Decl l n t d)
  | Prod [t]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq l, Eq t) => Eq1 (Decl l n t) where
  (==#) = (==)

instance (Ord l, Ord t) => Ord1 (Decl l n t) where
  compare1 = compare

instance (Show l, Show t) => Show1 (Decl l n t) where
  showsPrec1 = showsPrec

instance Applicative (Decl l n t) where
  pure = return
  (<*>) = ap

instance Monad (Decl l n t) where
  return = Decl

  Decl d >>= f = f d
  DclLoc l d >>= f = DclLoc l (d >>= f)
  Prod ts >>= _ = Prod ts

