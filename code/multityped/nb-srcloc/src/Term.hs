{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Term where

import Control.Lens.TH

import Loc

-- do we want to have Eq / Ord / Show ignore the TmLoc annotations?
-- we could add a newtype that uses stripLoc before those operations
data Term l =
    TmZero
  | TmSucc (Term l)
  | TmPred (Term l)
  | TmFalse
  | TmTrue
  | TmIf (Term l) (Term l) (Term l)
  | TmIsZero (Term l)
  | TmLoc l (Term l)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''Term

instance WithLoc Term where
  _Loc = _TmLoc

  stripLoc TmFalse = TmFalse
  stripLoc TmTrue = TmTrue
  stripLoc (TmIf t1 t2 t3) = TmIf (stripLoc t1) (stripLoc t2) (stripLoc t3)
  stripLoc TmZero = TmZero
  stripLoc (TmSucc t) = TmSucc (stripLoc t)
  stripLoc (TmPred t) = TmPred (stripLoc t)
  stripLoc (TmIsZero t) = TmIsZero (stripLoc t)
  stripLoc (TmLoc _ t) = stripLoc t

-- cata was nice for this, maybe bring back TermF if there are enough
-- opportunities to use cata?

size :: Term l -> Int
size TmFalse = 1
size TmTrue = 1
size (TmIf t1 t2 t3) = 1 + size t1 + size t2 + size t3
size TmZero = 1
size (TmSucc t) = 1 + size t
size (TmPred t) = 1 + size t
size (TmIsZero t) = 1 + size t
size (TmLoc _ t) = 1 + size t

