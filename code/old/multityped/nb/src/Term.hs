{-# LANGUAGE TemplateHaskell #-}
module Term where

import Control.Lens.TH

data Term =
    TmZero
  | TmSucc Term
  | TmPred Term
  | TmFalse
  | TmTrue
  | TmIf Term Term Term
  | TmIsZero Term
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Term

size :: Term -> Int
size TmZero = 1
size (TmSucc t) = 1 + size t
size (TmPred t) = 1 + size t
size TmFalse = 1
size TmTrue = 1
size (TmIf t1 t2 t3) = 1 + size t1 + size t2 + size t3
size (TmIsZero t) = 1 + size t
